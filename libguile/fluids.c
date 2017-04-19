/* Copyright (C) 1996,1997,2000,2001, 2004, 2006, 2007, 2008, 2009, 2010,
 *    2011, 2012, 2013, 2017 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <string.h>

#include "libguile/_scm.h"
#include "libguile/atomics-internal.h"
#include "libguile/cache-internal.h"
#include "libguile/print.h"
#include "libguile/dynwind.h"
#include "libguile/fluids.h"
#include "libguile/alist.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/deprecation.h"
#include "libguile/validate.h"
#include "libguile/bdw-gc.h"

/* A dynamic state associates fluids with values.  There are two
   representations of a dynamic state in Guile: the active
   representation that is part of each thread, and a frozen
   representation that can live in Scheme land as a value.

   The active dynamic state has two parts: a locals cache, and a values
   table.  The locals cache stores fluid values that have been recently
   referenced or set.  If a value isn't in the locals cache, Guile then
   looks for it in the values table, which is a weak-key hash table.
   Otherwise Guile falls back to the default value of the fluid.  In any
   case, the value is recorded in the locals cache.  Likewise setting a
   fluid's value simply inserts that association into the locals cache.

   The locals cache is not large, so adding an entry to it might evict
   some other entry.  In that case the entry gets flushed to the values
   table.

   The values table begins as being inherited from the parent dynamic
   state, and represents a capture of the fluid values at a point in
   time.  A dynamic state records when its values table might be
   referenced by other dynamic states.  If it is aliased, then any
   update to that table has to start by making a fresh local copy to
   work on.

   There are two interesting constraints on dynamic states, besides
   speed.  One is that they should hold onto their fluid-value
   associations weakly: they shouldn't keep fluids alive indefinitely,
   and if a fluid goes away, its value should become collectible too.
   This is why the values table is a weak table; it makes access
   somewhat slower, but this is mitigated by the cache.  The cache
   itself holds onto fluids and values strongly, but if there are more
   than 8 fluids in use by a dynamic state, this won't be a problem.

   The other interesting constraint is memory usage: you don't want a
   program with M fluids and N dynamic states to consume N*M memory.
   Guile associates each thread with a dynamic state, which itself isn't
   that bad as there are relatively few threads in a program.  The
   problem comes in with "fibers", lightweight user-space threads that
   can be allocated in the millions.  Here you want new fibers to
   inherit the dynamic state from the fiber that created them, but you
   really need to limit memory overheads.  For reference, in late 2016,
   non-dynamic-state memory overhead per fiber in one user-space library
   is around 500 bytes, in a simple "all fibers try to send a message on
   one channel" test case.

   For this reason the frozen representation of dynamic states is the
   probably-shared values table at the end of a list of fluid-value
   pairs, representing entries from the locals cache that differ from
   the values table.  This keeps per-dynamic-state memory usage in
   check.  A family of fibers that uses the same 3 or 4 fluids probably
   won't ever have to allocate a new values table.  Ideally the values
   table could share more state, as in an immutable weak array-mapped
   hash trie or something, but we don't have such a data structure.  */

#define FLUID_F_THREAD_LOCAL 0x100
#define SCM_I_FLUID_THREAD_LOCAL_P(x) \
  (SCM_CELL_WORD_0 (x) & FLUID_F_THREAD_LOCAL)

static inline int
is_dynamic_state (SCM x)
{
  return SCM_HAS_TYP7 (x, scm_tc7_dynamic_state);
}

static inline SCM
get_dynamic_state (SCM dynamic_state)
{
  return SCM_CELL_OBJECT_1 (dynamic_state);
}

/* Precondition: It's OK to throw away any unflushed data in the current
   cache.  */
static inline void
restore_dynamic_state (SCM saved, scm_t_dynamic_state *state)
{
  int slot;
  for (slot = SCM_CACHE_SIZE - 1; slot >= 0; slot--)
    {
      struct scm_cache_entry *entry = &state->cache.entries[slot];
      if (scm_is_pair (saved))
        {
          entry->key = SCM_UNPACK (SCM_CAAR (saved));
          entry->value = SCM_UNPACK (SCM_CDAR (saved));
          saved = scm_cdr (saved);
        }
      else
        entry->key = entry->value = 0;
    }
  state->values = saved;
  state->has_aliased_values = 1;
}

static inline SCM
save_dynamic_state (scm_t_dynamic_state *state)
{
  int slot;
  SCM saved = state->values;
  for (slot = 0; slot < SCM_CACHE_SIZE; slot++)
    {
      struct scm_cache_entry *entry = &state->cache.entries[slot];
      SCM key = SCM_PACK (entry->key);
      SCM value = SCM_PACK (entry->value);

      if (!entry->key)
        continue;
      if (SCM_I_FLUID_THREAD_LOCAL_P (key))
        {
          /* Because we don't include unflushed thread-local fluids in
             the result, we need to flush them to the table so that
             restore_dynamic_state can just throw away the current
             cache.  */
          scm_hashq_set_x (state->thread_local_values, key, value);
        }
      else if (!scm_is_eq (scm_weak_table_refq (state->values, key,
                                                SCM_UNDEFINED),
                           value))
        {
          if (state->has_aliased_values)
            saved = scm_acons (key, value, saved);
          else
            scm_weak_table_putq_x (state->values, key, value);
        }
    }
  state->has_aliased_values = 1;
  return saved;
}

static SCM
saved_dynamic_state_ref (SCM saved, SCM fluid, SCM dflt)
{
  for (; scm_is_pair (saved); saved = SCM_CDR (saved))
    if (scm_is_eq (SCM_CAAR (saved), fluid))
      return SCM_CDAR (saved);

  return scm_weak_table_refq (saved, fluid, dflt);
}

static SCM
add_entry (void *data, SCM k, SCM v, SCM result)
{
  scm_weak_table_putq_x (result, k, v);
  return result;
}

static SCM
copy_value_table (SCM tab)
{
  SCM ret = scm_c_make_weak_table (0, SCM_WEAK_TABLE_KIND_KEY);
  return scm_c_weak_table_fold (add_entry, NULL, ret, tab);
}




void
scm_i_fluid_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  if (SCM_I_FLUID_THREAD_LOCAL_P (exp))
    scm_puts ("#<thread-local-fluid ", port);
  else
    scm_puts ("#<fluid ", port);
  scm_intprint (SCM_UNPACK (exp), 16, port);
  scm_putc ('>', port);
}

void
scm_i_dynamic_state_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  scm_puts ("#<dynamic-state ", port);
  scm_intprint (SCM_UNPACK (exp), 16, port);
  scm_putc ('>', port);
}




#define SCM_I_FLUID_DEFAULT(x)   (SCM_CELL_OBJECT_1 (x))

static SCM
new_fluid (SCM init, scm_t_bits flags)
{
  return scm_cell (scm_tc7_fluid | flags, SCM_UNPACK (init));
}

SCM
scm_make_fluid (void)
{
  return new_fluid (SCM_BOOL_F, 0);
}

SCM_DEFINE (scm_make_fluid_with_default, "make-fluid", 0, 1, 0, 
	    (SCM dflt),
	    "Return a newly created fluid, whose initial value is @var{dflt},\n"
            "or @code{#f} if @var{dflt} is not given.\n"
	    "Fluids are objects that can hold one\n"
	    "value per dynamic state.  That is, modifications to this value are\n"
	    "only visible to code that executes with the same dynamic state as\n"
	    "the modifying code.  When a new dynamic state is constructed, it\n"
	    "inherits the values from its parent.  Because each thread normally executes\n"
	    "with its own dynamic state, you can use fluids for thread local storage.")
#define FUNC_NAME s_scm_make_fluid_with_default
{
  return new_fluid (SCM_UNBNDP (dflt) ? SCM_BOOL_F : dflt, 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_unbound_fluid, "make-unbound-fluid", 0, 0, 0,
            (),
            "Make a fluid that is initially unbound.")
#define FUNC_NAME s_scm_make_unbound_fluid
{
  return new_fluid (SCM_UNDEFINED, 0);
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_thread_local_fluid, "make-thread-local-fluid", 0, 1, 0, 
	    (SCM dflt),
	    "Return a newly created fluid, whose initial value is @var{dflt},\n"
            "or @code{#f} if @var{dflt} is not given.  Unlike fluids made\n"
	    "with @code{make-fluid}, thread local fluids are not captured\n"
            "by @code{make-dynamic-state}.  Similarly, a newly spawned\n"
            "child thread does not inherit thread-local fluid values from\n"
            "the parent thread.")
#define FUNC_NAME s_scm_make_thread_local_fluid
{
  return new_fluid (SCM_UNBNDP (dflt) ? SCM_BOOL_F : dflt,
                    FLUID_F_THREAD_LOCAL);
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_p, "fluid?", 1, 0, 0, 
	    (SCM obj),
	    "Return @code{#t} iff @var{obj} is a fluid; otherwise, return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_fluid_p
{
  return scm_from_bool (SCM_FLUID_P (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_thread_local_p, "fluid-thread-local?", 1, 0, 0, 
	    (SCM fluid),
	    "Return @code{#t} if the fluid @var{fluid} is is thread local,\n"
            "or @code{#f} otherwise.")
#define FUNC_NAME s_scm_fluid_thread_local_p
{
  SCM_VALIDATE_FLUID (1, fluid);
  return scm_from_bool (SCM_I_FLUID_THREAD_LOCAL_P (fluid));
}
#undef FUNC_NAME

int
scm_is_fluid (SCM obj)
{
  return SCM_FLUID_P (obj);
}

static void
fluid_set_x (scm_t_dynamic_state *dynamic_state, SCM fluid, SCM value)
{
  struct scm_cache_entry *entry;
  struct scm_cache_entry evicted = { 0, 0 };

  entry = scm_cache_lookup (&dynamic_state->cache, fluid);
  if (scm_is_eq (SCM_PACK (entry->key), fluid))
    {
      entry->value = SCM_UNPACK (value);
      return;
    }

  scm_cache_insert (&dynamic_state->cache, fluid, value, &evicted);

  if (evicted.key != 0)
    {
      fluid = SCM_PACK (evicted.key);
      value = SCM_PACK (evicted.value);

      if (SCM_I_FLUID_THREAD_LOCAL_P (fluid))
        {
          scm_hashq_set_x (dynamic_state->thread_local_values, fluid, value);
          return;
        }

      if (dynamic_state->has_aliased_values)
        {
          if (scm_is_eq (scm_weak_table_refq (dynamic_state->values,
                                              fluid, SCM_UNDEFINED),
                         value))
            return;
          dynamic_state->values = copy_value_table (dynamic_state->values);
          dynamic_state->has_aliased_values = 0;
        }

      scm_weak_table_putq_x (dynamic_state->values, fluid, value);
    }
}

/* Return value can be SCM_UNDEFINED; caller checks.  */
static SCM
fluid_ref (scm_t_dynamic_state *dynamic_state, SCM fluid)
{
  SCM val;
  struct scm_cache_entry *entry;

  entry = scm_cache_lookup (&dynamic_state->cache, fluid);
  if (scm_is_eq (SCM_PACK (entry->key), fluid))
    val = SCM_PACK (entry->value);
  else
    {
      if (SCM_I_FLUID_THREAD_LOCAL_P (fluid))
        val = scm_hashq_ref (dynamic_state->thread_local_values, fluid,
                             SCM_UNDEFINED);
      else
        val = scm_weak_table_refq (dynamic_state->values, fluid,
                                   SCM_UNDEFINED);

      if (SCM_UNBNDP (val))
        val = SCM_I_FLUID_DEFAULT (fluid);

      /* Cache this lookup.  */
      fluid_set_x (dynamic_state, fluid, val);
    }

  return val;
}

SCM_DEFINE (scm_fluid_ref, "fluid-ref", 1, 0, 0, 
	    (SCM fluid),
	    "Return the value associated with @var{fluid} in the current\n"
	    "dynamic root.  If @var{fluid} has not been set, then return\n"
	    "its default value.")
#define FUNC_NAME s_scm_fluid_ref
{
  SCM ret;
  SCM_VALIDATE_FLUID (1, fluid);
  ret = fluid_ref (SCM_I_CURRENT_THREAD->dynamic_state, fluid);
  if (SCM_UNBNDP (ret))
    scm_misc_error ("fluid-ref", "unbound fluid: ~S", scm_list_1 (fluid));
  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_ref_star, "fluid-ref*", 2, 0, 0,
	    (SCM fluid, SCM depth),
	    "Return the @var{depth}th oldest value associated with\n"
            "@var{fluid} in the current thread.  If @var{depth} equals\n"
            "or exceeds the number of values that have been assigned to\n"
            "@var{fluid}, return the default value of the fluid.")
#define FUNC_NAME s_scm_fluid_ref_star
{
  SCM ret;
  size_t c_depth;

  SCM_VALIDATE_FLUID (1, fluid);
  c_depth = SCM_NUM2SIZE (2, depth);

  /* Because this function is called to look up the current exception
     handler and this can happen in an out-of-memory situation, we avoid
     cache flushes to the weak table which might cause allocation of a
     disappearing link.  */
  if (c_depth == 0)
    {
      scm_t_dynamic_state *dynamic_state = SCM_I_CURRENT_THREAD->dynamic_state;
      struct scm_cache_entry *entry;

      entry = scm_cache_lookup (&dynamic_state->cache, fluid);
      if (scm_is_eq (SCM_PACK (entry->key), fluid))
        ret = SCM_PACK (entry->value);
      else
        {
          if (SCM_I_FLUID_THREAD_LOCAL_P (fluid))
            ret = scm_hashq_ref (dynamic_state->thread_local_values, fluid,
                                 SCM_UNDEFINED);
          else
            ret = scm_weak_table_refq (dynamic_state->values, fluid,
                                       SCM_UNDEFINED);

          if (SCM_UNBNDP (ret))
            ret = SCM_I_FLUID_DEFAULT (fluid);

          /* Don't cache the lookup.  */
        }
      }
  else
    ret = scm_dynstack_find_old_fluid_value (&SCM_I_CURRENT_THREAD->dynstack,
                                             fluid, c_depth - 1,
                                             SCM_I_FLUID_DEFAULT (fluid));

  if (SCM_UNBNDP (ret))
    scm_misc_error ("fluid-ref*", "unbound fluid: ~S", scm_list_1 (fluid));
  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_set_x, "fluid-set!", 2, 0, 0,
	    (SCM fluid, SCM value),
	    "Set the value associated with @var{fluid} in the current dynamic root.")
#define FUNC_NAME s_scm_fluid_set_x
{
  SCM_VALIDATE_FLUID (1, fluid);
  fluid_set_x (SCM_I_CURRENT_THREAD->dynamic_state, fluid, value);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_unset_x, "fluid-unset!", 1, 0, 0,
            (SCM fluid),
            "Unset the value associated with @var{fluid}.")
#define FUNC_NAME s_scm_fluid_unset_x
{
  /* FIXME: really unset the default value, too?  The current test
     suite demands it, but I would prefer not to.  */
  SCM_VALIDATE_FLUID (1, fluid);
  SCM_SET_CELL_OBJECT_1 (fluid, SCM_UNDEFINED);
  fluid_set_x (SCM_I_CURRENT_THREAD->dynamic_state, fluid, SCM_UNDEFINED);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_fluid_bound_p, "fluid-bound?", 1, 0, 0,
	    (SCM fluid),
	    "Return @code{#t} iff @var{fluid} is bound to a value.\n"
	    "Throw an error if @var{fluid} is not a fluid.")
#define FUNC_NAME s_scm_fluid_bound_p
{
  SCM val;
  SCM_VALIDATE_FLUID (1, fluid);
  val = fluid_ref (SCM_I_CURRENT_THREAD->dynamic_state, fluid);
  return scm_from_bool (! (SCM_UNBNDP (val)));
}
#undef FUNC_NAME

static SCM
apply_thunk (void *thunk)
{
  return scm_call_0 (SCM_PACK (thunk));
}

void
scm_swap_fluid (SCM fluid, SCM value_box, scm_t_dynamic_state *dynstate)
{
  SCM val = fluid_ref (dynstate, fluid);
  fluid_set_x (dynstate, fluid, SCM_VARIABLE_REF (value_box));
  SCM_VARIABLE_SET (value_box, val);
}
  
SCM_DEFINE (scm_with_fluids, "with-fluids*", 3, 0, 0, 
	    (SCM fluids, SCM values, SCM thunk),
	    "Set @var{fluids} to @var{values} temporary, and call @var{thunk}.\n"
	    "@var{fluids} must be a list of fluids and @var{values} must be the same\n"
	    "number of their values to be applied.  Each substitution is done\n"
	    "one after another.  @var{thunk} must be a procedure with no argument.")
#define FUNC_NAME s_scm_with_fluids
{
  return scm_c_with_fluids (fluids, values,
			    apply_thunk, (void *) SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM
scm_c_with_fluids (SCM fluids, SCM values, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluids"
{
  SCM ans;
  long flen, vlen, i;
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;

  SCM_VALIDATE_LIST_COPYLEN (1, fluids, flen);
  SCM_VALIDATE_LIST_COPYLEN (2, values, vlen);
  if (flen != vlen)
    scm_out_of_range (s_scm_with_fluids, values);

  for (i = 0; i < flen; i++)
    {
      scm_dynstack_push_fluid (&thread->dynstack,
                               SCM_CAR (fluids), SCM_CAR (values),
                               thread->dynamic_state);
      fluids = SCM_CDR (fluids);
      values = SCM_CDR (values);
    }

  ans = cproc (cdata);

  for (i = 0; i < flen; i++)
    scm_dynstack_unwind_fluid (&thread->dynstack, thread->dynamic_state);

  return ans;
}
#undef FUNC_NAME

SCM
scm_with_fluid (SCM fluid, SCM value, SCM thunk)
{
  return scm_c_with_fluid (fluid, value,
			   apply_thunk, (void *) SCM_UNPACK (thunk));
}

SCM
scm_c_with_fluid (SCM fluid, SCM value, SCM (*cproc) (), void *cdata)
#define FUNC_NAME "scm_c_with_fluid"
{
  SCM ans;
  scm_i_thread *thread = SCM_I_CURRENT_THREAD;

  scm_dynstack_push_fluid (&thread->dynstack, fluid, value,
                           thread->dynamic_state);
  ans = cproc (cdata);
  scm_dynstack_unwind_fluid (&thread->dynstack, thread->dynamic_state);

  return ans;
}
#undef FUNC_NAME

static void
swap_fluid (SCM data)
{
  scm_t_dynamic_state *dynstate = SCM_I_CURRENT_THREAD->dynamic_state;
  SCM f = SCM_CAR (data);
  SCM t = fluid_ref (dynstate, f);
  fluid_set_x (dynstate, f, SCM_CDR (data));
  SCM_SETCDR (data, t);
}

void
scm_dynwind_fluid (SCM fluid, SCM value)
{
  SCM data = scm_cons (fluid, value);
  scm_dynwind_rewind_handler_with_scm (swap_fluid, data, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler_with_scm (swap_fluid, data, SCM_F_WIND_EXPLICITLY);
}

SCM
scm_i_make_initial_dynamic_state (void)
{
  return scm_cell (scm_tc7_dynamic_state,
                   SCM_UNPACK (scm_c_make_weak_table
                               (0, SCM_WEAK_TABLE_KIND_KEY)));
}

SCM_DEFINE (scm_dynamic_state_p, "dynamic-state?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a dynamic state object;\n"
	    "return @code{#f} otherwise")
#define FUNC_NAME s_scm_dynamic_state_p
{
  return scm_from_bool (is_dynamic_state (obj));
}
#undef FUNC_NAME

int
scm_is_dynamic_state (SCM obj)
{
  return is_dynamic_state (obj);
}

SCM_DEFINE (scm_current_dynamic_state, "current-dynamic-state", 0, 0, 0,
	    (),
	    "Return a snapshot of the current fluid-value associations\n"
            "as a fresh dynamic state object.")
#define FUNC_NAME s_scm_current_dynamic_state
{
  struct scm_dynamic_state *state = SCM_I_CURRENT_THREAD->dynamic_state;
  return scm_cell (scm_tc7_dynamic_state,
                   SCM_UNPACK (save_dynamic_state (state)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_current_dynamic_state, "set-current-dynamic-state", 1,0,0,
	    (SCM state),
	    "Set the current dynamic state object to @var{state}\n"
	    "and return the previous current dynamic state object.")
#define FUNC_NAME s_scm_set_current_dynamic_state
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  SCM old = scm_current_dynamic_state ();
  SCM_ASSERT (is_dynamic_state (state), state, SCM_ARG1, FUNC_NAME);
  restore_dynamic_state (get_dynamic_state (state), t->dynamic_state);
  return old;
}
#undef FUNC_NAME

SCM
scm_dynamic_state_ref (SCM state, SCM fluid, SCM dflt)
{
  SCM_ASSERT (is_dynamic_state (state), state, SCM_ARG1,
              "dynamic-state-ref");
  return saved_dynamic_state_ref (get_dynamic_state (state), fluid, dflt);
}

static void
swap_dynamic_state (SCM loc)
{
  SCM_SETCAR (loc, scm_set_current_dynamic_state (SCM_CAR (loc)));
}

void
scm_dynwind_current_dynamic_state (SCM state)
{
  SCM loc = scm_cons (state, SCM_EOL);
  SCM_ASSERT (is_dynamic_state (state), state, SCM_ARG1, NULL);
  scm_dynwind_rewind_handler_with_scm (swap_dynamic_state, loc,
				     SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler_with_scm (swap_dynamic_state, loc,
				     SCM_F_WIND_EXPLICITLY);
}

void *
scm_c_with_dynamic_state (SCM state, void *(*func)(void *), void *data)
{
  void *result;
  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_current_dynamic_state (state);
  result = func (data);
  scm_dynwind_end ();
  return result;
}

SCM_DEFINE (scm_with_dynamic_state, "with-dynamic-state", 2, 0, 0,
	    (SCM state, SCM proc),
	    "Call @var{proc} while @var{state} is the current dynamic\n"
	    "state object.")
#define FUNC_NAME s_scm_with_dynamic_state
{
  SCM result;
  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_current_dynamic_state (state);
  result = scm_call_0 (proc);
  scm_dynwind_end ();
  return result;
}
#undef FUNC_NAME


void
scm_init_fluids ()
{
#include "libguile/fluids.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
