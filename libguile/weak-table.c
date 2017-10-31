/* Copyright (C) 2011, 2012, 2013, 2014, 2017 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>

#include "libguile/bdw-gc.h"
#include <gc/gc_typed.h>

#include "libguile/_scm.h"
#include "libguile/hash.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/validate.h"
#include "libguile/weak-list.h"
#include "libguile/weak-table.h"


/* Weak Tables

   This file implements weak hash tables.  Weak hash tables are
   generally used when you want to augment some object with additional
   data, but when you don't have space to store the data in the object.
   For example, procedure properties are implemented with weak tables.

   This is a normal bucket-and-chain hash table, except that the chain
   entries are allocated in such a way that the GC doesn't trace the
   weak values.  For doubly-weak tables, this means that the entries are
   allocated as an "atomic" piece of memory.  Key-weak and value-weak
   tables use a special GC kind with a custom mark procedure.  When
   items are added weakly into table, a disappearing link is registered
   to their locations.  If the referent is collected, then that link
   will be zeroed out.

   An entry in the table consists of the key and the value, together
   with the hash code of the key.

   Note that since the weak references are stored in an atomic region
   with disappearing links, they need to be accessed with the GC alloc
   lock.  `read_weak_entry' will do that for you.  The hash code itself
   can be read outside the lock, though.
  */


typedef struct scm_weak_entry scm_t_weak_entry;

struct scm_weak_entry {
  unsigned long hash;
  scm_t_weak_entry *next;
  scm_t_bits key;
  scm_t_bits value;
};


struct weak_entry_data {
  scm_t_weak_entry *entry;
  scm_t_bits key;
  scm_t_bits value;
};
  
static void*
do_read_weak_entry (void *data)
{
  struct weak_entry_data *e = data;

  e->key = e->entry->key;
  e->value = e->entry->value;

  return NULL;
}

static void
read_weak_entry (scm_t_weak_entry *entry, scm_t_bits *key, scm_t_bits *value)
{
  struct weak_entry_data data;

  data.entry = entry;
  GC_call_with_alloc_lock (do_read_weak_entry, &data);

  *key = data.key;
  *value = data.value;
}
  
static void
register_disappearing_links (scm_t_weak_entry *entry,
                             SCM k, SCM v,
                             scm_t_weak_table_kind kind)
{
  if (SCM_UNPACK (k) && SCM_HEAP_OBJECT_P (k)
      && (kind == SCM_WEAK_TABLE_KIND_KEY
          || kind == SCM_WEAK_TABLE_KIND_BOTH))
    SCM_I_REGISTER_DISAPPEARING_LINK ((void **) &entry->key,
                                      SCM2PTR (k));

  if (SCM_UNPACK (v) && SCM_HEAP_OBJECT_P (v)
      && (kind == SCM_WEAK_TABLE_KIND_VALUE
          || kind == SCM_WEAK_TABLE_KIND_BOTH))
    SCM_I_REGISTER_DISAPPEARING_LINK ((void **) &entry->value,
                                      SCM2PTR (v));
}

static void
unregister_disappearing_links (scm_t_weak_entry *entry,
                               scm_t_weak_table_kind kind)
{
  if (kind == SCM_WEAK_TABLE_KIND_KEY || kind == SCM_WEAK_TABLE_KIND_BOTH)
    GC_unregister_disappearing_link ((void **) &entry->key);

  if (kind == SCM_WEAK_TABLE_KIND_VALUE || kind == SCM_WEAK_TABLE_KIND_BOTH)
    GC_unregister_disappearing_link ((void **) &entry->value);
}

typedef struct {
  scm_t_weak_entry **buckets;   /* the data */
  scm_i_pthread_mutex_t lock;   /* the lock */
  scm_t_weak_table_kind kind;   /* what kind of table it is */
  unsigned long n_buckets;    	/* total number of buckets. */
  unsigned long n_items;	/* number of items in table */
  unsigned long lower;		/* when to shrink */
  unsigned long upper;		/* when to grow */
  int size_index;		/* index into hashtable_size */
  int min_size_index;		/* minimum size_index */
  GC_word last_gc_no;
} scm_t_weak_table;


#define SCM_WEAK_TABLE_P(x) (SCM_HAS_TYP7 (x, scm_tc7_weak_table))
#define SCM_VALIDATE_WEAK_TABLE(pos, arg) \
  SCM_MAKE_VALIDATE_MSG (pos, arg, WEAK_TABLE_P, "weak-table")
#define SCM_WEAK_TABLE(x) ((scm_t_weak_table *) SCM_CELL_WORD_1 (x))




/* GC descriptors for the various kinds of scm_t_weak_entry.  */
static GC_descr weak_key_descr;
static GC_descr weak_value_descr;
static GC_descr doubly_weak_descr;

static scm_t_weak_entry *
allocate_entry (scm_t_weak_table_kind kind)
{
  scm_t_weak_entry *ret;

  switch (kind)
    {
    case SCM_WEAK_TABLE_KIND_KEY:
      ret = GC_malloc_explicitly_typed (sizeof (*ret), weak_key_descr);
      break;
    case SCM_WEAK_TABLE_KIND_VALUE:
      ret = GC_malloc_explicitly_typed (sizeof (*ret), weak_value_descr);
      break;
    case SCM_WEAK_TABLE_KIND_BOTH:
      ret = GC_malloc_explicitly_typed (sizeof (*ret), doubly_weak_descr);
      break;
    default:
      abort ();
    }

  return ret;
}

static void
add_entry (scm_t_weak_table *table, scm_t_weak_entry *entry)
{
  unsigned long bucket = entry->hash % table->n_buckets;
  entry->next = table->buckets[bucket];
  table->buckets[bucket] = entry;
  table->n_items++;
}



/* Growing or shrinking is triggered when the load factor
 *
 *   L = N / S    (N: number of items in table, S: bucket vector length)
 *
 * passes an upper limit of 0.9 or a lower limit of 0.25.
 *
 * The implementation stores the upper and lower number of items which
 * trigger a resize in the hashtable object.
 *
 * Possible hash table sizes (primes) are stored in the array
 * hashtable_size.
 */

static unsigned long hashtable_size[] = {
  31, 61, 113, 223, 443, 883, 1759, 3517, 7027, 14051, 28099, 56197, 112363,
  224717, 449419, 898823, 1797641, 3595271, 7190537, 14381041, 28762081,
  57524111, 115048217, 230096423
};

#define HASHTABLE_SIZE_N (sizeof(hashtable_size)/sizeof(unsigned long))

static void
resize_table (scm_t_weak_table *table)
{
  scm_t_weak_entry **old_buckets, **new_buckets;
  int new_size_index;
  unsigned long old_n_buckets, new_n_buckets, old_k;

  new_size_index = table->size_index;
  if (table->n_items < table->lower)
    {
      /* Rehashing is not triggered when i <= min_size.  */
      do
	new_size_index -= 1;
      while (new_size_index > table->min_size_index
	     && table->n_items < hashtable_size[new_size_index] / 4);
    }
  else if (table->n_items > table->upper)
    {
      new_size_index += 1;
      if (new_size_index >= HASHTABLE_SIZE_N)
        /* Limit max bucket count.  */
        return;
    }
  else
    /* Nothing to do.  */
    return;

  new_n_buckets = hashtable_size[new_size_index];
  new_buckets = scm_gc_malloc (sizeof (*new_buckets) * new_n_buckets,
                               "weak table buckets");

  old_buckets = table->buckets;
  old_n_buckets = table->n_buckets;
  
  table->size_index = new_size_index;
  table->n_buckets = new_n_buckets;
  if (new_size_index <= table->min_size_index)
    table->lower = 0;
  else
    table->lower = new_n_buckets / 4;
  table->upper = 9 * new_n_buckets / 10;
  table->n_items = 0;
  table->buckets = new_buckets;

  for (old_k = 0; old_k < old_n_buckets; old_k++)
    {
      scm_t_weak_entry *entry = old_buckets[old_k];
      while (entry)
        {
          scm_t_weak_entry *next = entry->next;
          entry->next = NULL;
          add_entry (table, entry);
          entry = next;
        }
    }
}

/* Run after GC via do_vacuum_weak_table, this function runs over the
   whole table, removing lost weak references, reshuffling the table as it
   goes.  It might resize the table if it reaps enough buckets.  */
static void
vacuum_weak_table (scm_t_weak_table *table)
{
  GC_word gc_no = GC_get_gc_no ();
  unsigned long k;

  if (gc_no == table->last_gc_no)
    return;

  table->last_gc_no = gc_no;

  for (k = 0; k < table->n_buckets; k++)
    {
      scm_t_weak_entry **loc = table->buckets + k;
      scm_t_weak_entry *entry;

      for (entry = *loc; entry; entry = *loc)
        {
          scm_t_bits key, value;

          read_weak_entry (entry, &key, &value);
          if (!key || !value)
            /* Lost weak reference; prune entry.  */
            {
              *loc = entry->next;
              table->n_items--;
              entry->next = NULL;
              unregister_disappearing_links (entry, table->kind);
            }
          else
            loc = &entry->next;
        }
    }

  if (table->n_items < table->lower)
    resize_table (table);
}




static SCM
weak_table_ref (scm_t_weak_table *table, unsigned long hash,
                scm_t_table_predicate_fn pred, void *closure,
                SCM dflt)
{
  unsigned long bucket = hash % table->n_buckets;
  scm_t_weak_entry *entry;

  for (entry = table->buckets[bucket]; entry; entry = entry->next)
    {
      if (entry->hash == hash)
        {
          scm_t_bits key, value;

          read_weak_entry (entry, &key, &value);
          if (key && value && pred (SCM_PACK (key), SCM_PACK (value), closure))
            /* Found. */
            return SCM_PACK (value);
        }
    }

  return dflt;
}


static void
weak_table_put_x (scm_t_weak_table *table, unsigned long hash,
                  scm_t_table_predicate_fn pred, void *closure,
                  SCM key, SCM value)
{
  unsigned long bucket = hash % table->n_buckets;
  scm_t_weak_entry *entry;

  for (entry = table->buckets[bucket]; entry; entry = entry->next)
    {
      if (entry->hash == hash)
        {
          scm_t_bits k, v;

          read_weak_entry (entry, &k, &v);
          if (k && v && pred (SCM_PACK (k), SCM_PACK (v), closure))
            {
              unregister_disappearing_links (entry, table->kind);
              key = SCM_PACK (k);
              entry->value = SCM_UNPACK (value);
              register_disappearing_links (entry, key, value, table->kind);
              return;
            }
        }
    }

  if (table->n_items > table->upper)
    /* Full table, time to resize.  */
    resize_table (table);

  entry = allocate_entry (table->kind);
  entry->hash = hash;
  entry->key = SCM_UNPACK (key);
  entry->value = SCM_UNPACK (value);
  register_disappearing_links (entry, key, value, table->kind);
  add_entry (table, entry);
}


static void
weak_table_remove_x (scm_t_weak_table *table, unsigned long hash,
                   scm_t_table_predicate_fn pred, void *closure)
{
  unsigned long bucket = hash % table->n_buckets;
  scm_t_weak_entry **loc = table->buckets + bucket;
  scm_t_weak_entry *entry;

  for (entry = *loc; entry; entry = *loc)
    {
      if (entry->hash == hash)
        {
          scm_t_bits k, v;

          read_weak_entry (entry, &k, &v);
          if (k && v && pred (SCM_PACK (k), SCM_PACK (v), closure))
            {
              *loc = entry->next;
              table->n_items--;
              entry->next = NULL;
              unregister_disappearing_links (entry, table->kind);

              if (table->n_items < table->lower)
                resize_table (table);

              return;
            }
        }
      loc = &entry->next;
    }

  return;
}



static SCM
make_weak_table (unsigned long k, scm_t_weak_table_kind kind)
{
  scm_t_weak_table *table;

  int i = 0, n = k ? k : 31;
  while (i + 1 < HASHTABLE_SIZE_N && n > hashtable_size[i])
    ++i;
  n = hashtable_size[i];

  table = scm_gc_malloc (sizeof (*table), "weak-table");
  table->buckets = scm_gc_malloc (sizeof (*table->buckets) * n,
                                  "weak table buckets");
  table->kind = kind;
  table->n_items = 0;
  table->n_buckets = n;
  table->lower = 0;
  table->upper = 9 * n / 10;
  table->size_index = i;
  table->min_size_index = i;
  table->last_gc_no = GC_get_gc_no ();
  scm_i_pthread_mutex_init (&table->lock, NULL);

  return scm_cell (scm_tc7_weak_table, (scm_t_bits)table);
}

void
scm_i_weak_table_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<", port);
  scm_puts ("weak-table ", port);
  scm_uintprint (SCM_WEAK_TABLE (exp)->n_items, 10, port);
  scm_putc ('/', port);
  scm_uintprint (SCM_WEAK_TABLE (exp)->n_buckets, 10, port);
  scm_puts (">", port);
}

static void
do_vacuum_weak_table (SCM table)
{
  scm_t_weak_table *t;

  t = SCM_WEAK_TABLE (table);

  /* Unlike weak sets, the weak table interface allows custom predicates
     to call out to arbitrary Scheme.  There are two ways that this code
     can be re-entrant, then: calling weak hash procedures while in a
     custom predicate, or via finalizers run explicitly by (gc) or in an
     async (for non-threaded Guile).  We add a restriction that
     prohibits the first case, by convention.  But since we can't
     prohibit the second case, here we trylock instead of lock.  In any
     case, if the mutex is held by another thread, then the table is in
     active use, so the next user of the table will handle the vacuum
     for us.  */
  if (scm_i_pthread_mutex_trylock (&t->lock) == 0)
    {
      vacuum_weak_table (t);
      scm_i_pthread_mutex_unlock (&t->lock);
    }

  return;
}

static scm_i_pthread_mutex_t all_weak_tables_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;
static SCM all_weak_tables = SCM_EOL;

static void
vacuum_all_weak_tables (void)
{
  scm_i_pthread_mutex_lock (&all_weak_tables_lock);
  scm_i_visit_weak_list (&all_weak_tables, do_vacuum_weak_table);
  scm_i_pthread_mutex_unlock (&all_weak_tables_lock);
}

SCM
scm_c_make_weak_table (unsigned long k, scm_t_weak_table_kind kind)
{
  SCM ret;

  ret = make_weak_table (k, kind);

  scm_i_pthread_mutex_lock (&all_weak_tables_lock);
  all_weak_tables = scm_i_weak_cons (ret, all_weak_tables);
  scm_i_pthread_mutex_unlock (&all_weak_tables_lock);

  return ret;
}

SCM
scm_weak_table_p (SCM obj)
{
  return scm_from_bool (SCM_WEAK_TABLE_P (obj));
}

SCM
scm_c_weak_table_ref (SCM table, unsigned long raw_hash,
                      scm_t_table_predicate_fn pred,
                      void *closure, SCM dflt)
#define FUNC_NAME "weak-table-ref"
{
  SCM ret;
  scm_t_weak_table *t;

  SCM_VALIDATE_WEAK_TABLE (1, table);

  t = SCM_WEAK_TABLE (table);

  scm_i_pthread_mutex_lock (&t->lock);

  vacuum_weak_table (t);

  ret = weak_table_ref (t, raw_hash, pred, closure, dflt);

  scm_i_pthread_mutex_unlock (&t->lock);

  return ret;
}
#undef FUNC_NAME

void
scm_c_weak_table_put_x (SCM table, unsigned long raw_hash,
                        scm_t_table_predicate_fn pred,
                        void *closure, SCM key, SCM value)
#define FUNC_NAME "weak-table-put!"
{
  scm_t_weak_table *t;

  SCM_VALIDATE_WEAK_TABLE (1, table);

  t = SCM_WEAK_TABLE (table);

  scm_i_pthread_mutex_lock (&t->lock);

  vacuum_weak_table (t);

  weak_table_put_x (t, raw_hash, pred, closure, key, value);

  scm_i_pthread_mutex_unlock (&t->lock);
}
#undef FUNC_NAME

void
scm_c_weak_table_remove_x (SCM table, unsigned long raw_hash,
                           scm_t_table_predicate_fn pred,
                           void *closure)
#define FUNC_NAME "weak-table-remove!"
{
  scm_t_weak_table *t;

  SCM_VALIDATE_WEAK_TABLE (1, table);

  t = SCM_WEAK_TABLE (table);

  scm_i_pthread_mutex_lock (&t->lock);

  vacuum_weak_table (t);

  weak_table_remove_x (t, raw_hash, pred, closure);

  scm_i_pthread_mutex_unlock (&t->lock);
}
#undef FUNC_NAME

static int
assq_predicate (SCM x, SCM y, void *closure)
{
  return scm_is_eq (x, SCM_PACK_POINTER (closure));
}

SCM
scm_weak_table_refq (SCM table, SCM key, SCM dflt)
{
  return scm_c_weak_table_ref (table, scm_ihashq (key, -1),
                               assq_predicate, SCM_UNPACK_POINTER (key),
                               dflt);
}

void
scm_weak_table_putq_x (SCM table, SCM key, SCM value)
{
  scm_c_weak_table_put_x (table, scm_ihashq (key, -1),
                          assq_predicate, SCM_UNPACK_POINTER (key),
                          key, value);
}

void
scm_weak_table_remq_x (SCM table, SCM key)
{
  scm_c_weak_table_remove_x (table, scm_ihashq (key, -1),
                             assq_predicate, SCM_UNPACK_POINTER (key));
}

void
scm_weak_table_clear_x (SCM table)
#define FUNC_NAME "weak-table-clear!"
{
  scm_t_weak_table *t;
  unsigned long k;
  scm_t_weak_entry *entry;

  SCM_VALIDATE_WEAK_TABLE (1, table);

  t = SCM_WEAK_TABLE (table);

  scm_i_pthread_mutex_lock (&t->lock);

  t->last_gc_no = GC_get_gc_no ();

  for (k = 0; k < t->n_buckets; k++)
    {
      for (entry = t->buckets[k]; entry; entry = entry->next)
        unregister_disappearing_links (entry, t->kind);
      t->buckets[k] = NULL;
    }
  t->n_items = 0;

  scm_i_pthread_mutex_unlock (&t->lock);
}
#undef FUNC_NAME

SCM
scm_c_weak_table_fold (scm_t_table_fold_fn proc, void *closure,
                       SCM init, SCM table)
{
  scm_t_weak_table *t;
  unsigned long k;
  SCM alist = SCM_EOL;

  t = SCM_WEAK_TABLE (table);

  scm_i_pthread_mutex_lock (&t->lock);

  vacuum_weak_table (t);

  for (k = 0; k < t->n_buckets; k++)
    {
      scm_t_weak_entry *entry;
      for (entry = t->buckets[k]; entry; entry = entry->next)
        {
          scm_t_bits key, value;
          read_weak_entry (entry, &key, &value);
      
          if (key && value)
            alist = scm_acons (SCM_PACK (key), SCM_PACK (value), alist);
        }
    }
  
  scm_i_pthread_mutex_unlock (&t->lock);
  
  /* Call the proc outside the lock.  */
  for (; !scm_is_null (alist); alist = scm_cdr (alist))
    init = proc (closure, scm_caar (alist), scm_cdar (alist), init);

  return init;
}

static SCM
fold_trampoline (void *closure, SCM k, SCM v, SCM init)
{
  return scm_call_3 (SCM_PACK_POINTER (closure), k, v, init);
}

SCM
scm_weak_table_fold (SCM proc, SCM init, SCM table)
#define FUNC_NAME "weak-table-fold"
{
  SCM_VALIDATE_WEAK_TABLE (3, table);
  SCM_VALIDATE_PROC (1, proc);

  return scm_c_weak_table_fold (fold_trampoline, SCM_UNPACK_POINTER (proc), init, table);
}
#undef FUNC_NAME

static SCM
for_each_trampoline (void *closure, SCM k, SCM v, SCM seed)
{
  scm_call_2 (SCM_PACK_POINTER (closure), k, v);
  return seed;
}

void
scm_weak_table_for_each (SCM proc, SCM table)
#define FUNC_NAME "weak-table-for-each"
{
  SCM_VALIDATE_WEAK_TABLE (2, table);
  SCM_VALIDATE_PROC (1, proc);

  scm_c_weak_table_fold (for_each_trampoline, SCM_UNPACK_POINTER (proc), SCM_BOOL_F, table);
}
#undef FUNC_NAME

static SCM
map_trampoline (void *closure, SCM k, SCM v, SCM seed)
{
  return scm_cons (scm_call_2 (SCM_PACK_POINTER (closure), k, v), seed);
}

SCM
scm_weak_table_map_to_list (SCM proc, SCM table)
#define FUNC_NAME "weak-table-map->list"
{
  SCM_VALIDATE_WEAK_TABLE (2, table);
  SCM_VALIDATE_PROC (1, proc);

  return scm_c_weak_table_fold (map_trampoline, SCM_UNPACK_POINTER (proc), SCM_EOL, table);
}
#undef FUNC_NAME




/* Legacy interface.  */

SCM_DEFINE (scm_make_weak_key_hash_table, "make-weak-key-hash-table", 0, 1, 0, 
	    (SCM n),
	    "@deffnx {Scheme Procedure} make-weak-value-hash-table size\n"
	    "@deffnx {Scheme Procedure} make-doubly-weak-hash-table size\n"
	    "Return a weak hash table with @var{size} buckets.\n"
	    "\n"
	    "You can modify weak hash tables in exactly the same way you\n"
	    "would modify regular hash tables. (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_key_hash_table
{
  return scm_c_make_weak_table (SCM_UNBNDP (n) ? 0 : scm_to_ulong (n),
                                SCM_WEAK_TABLE_KIND_KEY);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_hash_table, "make-weak-value-hash-table", 0, 1, 0, 
            (SCM n),
	    "Return a hash table with weak values with @var{size} buckets.\n"
	    "(@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_value_hash_table
{
  return scm_c_make_weak_table (SCM_UNBNDP (n) ? 0 : scm_to_ulong (n),
                                SCM_WEAK_TABLE_KIND_VALUE);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_doubly_weak_hash_table, "make-doubly-weak-hash-table", 0, 1, 0, 
            (SCM n),
	    "Return a hash table with weak keys and values with @var{size}\n"
	    "buckets.  (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_doubly_weak_hash_table
{
  return scm_c_make_weak_table (SCM_UNBNDP (n) ? 0 : scm_to_ulong (n),
                                SCM_WEAK_TABLE_KIND_BOTH);
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_key_hash_table_p, "weak-key-hash-table?", 1, 0, 0, 
           (SCM obj),
	    "@deffnx {Scheme Procedure} weak-value-hash-table? obj\n"
	    "@deffnx {Scheme Procedure} doubly-weak-hash-table? obj\n"
	    "Return @code{#t} if @var{obj} is the specified weak hash\n"
	    "table. Note that a doubly weak hash table is neither a weak key\n"
	    "nor a weak value hash table.")
#define FUNC_NAME s_scm_weak_key_hash_table_p
{
  return scm_from_bool (SCM_WEAK_TABLE_P (obj) &&
                        SCM_WEAK_TABLE (obj)->kind == SCM_WEAK_TABLE_KIND_KEY);
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_value_hash_table_p, "weak-value-hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak value hash table.")
#define FUNC_NAME s_scm_weak_value_hash_table_p
{
  return scm_from_bool (SCM_WEAK_TABLE_P (obj) &&
                        SCM_WEAK_TABLE (obj)->kind == SCM_WEAK_TABLE_KIND_VALUE);
}
#undef FUNC_NAME


SCM_DEFINE (scm_doubly_weak_hash_table_p, "doubly-weak-hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a doubly weak hash table.")
#define FUNC_NAME s_scm_doubly_weak_hash_table_p
{
  return scm_from_bool (SCM_WEAK_TABLE_P (obj) &&
                        SCM_WEAK_TABLE (obj)->kind == SCM_WEAK_TABLE_KIND_BOTH);
}
#undef FUNC_NAME





void
scm_weak_table_prehistory (void)
{
  GC_word weak_key_bitmap[GC_BITMAP_SIZE (scm_t_weak_entry)] = { 0 };
  GC_word weak_value_bitmap[GC_BITMAP_SIZE (scm_t_weak_entry)] = { 0 };
  GC_word doubly_weak_bitmap[GC_BITMAP_SIZE (scm_t_weak_entry)] = { 0 };

  GC_set_bit (weak_key_bitmap, GC_WORD_OFFSET (scm_t_weak_entry, next));
  GC_set_bit (weak_value_bitmap, GC_WORD_OFFSET (scm_t_weak_entry, next));
  GC_set_bit (doubly_weak_bitmap, GC_WORD_OFFSET (scm_t_weak_entry, next));

  GC_set_bit (weak_key_bitmap, GC_WORD_OFFSET (scm_t_weak_entry, value));
  GC_set_bit (weak_value_bitmap, GC_WORD_OFFSET (scm_t_weak_entry, key));

  weak_key_descr = GC_make_descriptor (weak_key_bitmap,
                                       GC_WORD_LEN (scm_t_weak_entry));
  weak_value_descr = GC_make_descriptor (weak_value_bitmap,
                                         GC_WORD_LEN (scm_t_weak_entry));
  doubly_weak_descr = GC_make_descriptor (doubly_weak_bitmap,
                                          GC_WORD_LEN (scm_t_weak_entry));
}

void
scm_init_weak_table ()
{
#include "libguile/weak-table.x"

  scm_i_register_async_gc_callback (vacuum_all_weak_tables);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
