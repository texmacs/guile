/* This file contains definitions for deprecated features.  When you
   deprecate something, move it here when that is feasible.
*/

/* Copyright (C) 2003, 2004, 2006, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015 Free Software Foundation, Inc.
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

#define SCM_BUILDING_DEPRECATED_CODE

#include "libguile/_scm.h"
#include "libguile/deprecation.h"

#if (SCM_ENABLE_DEPRECATED == 1)



SCM
scm_internal_dynamic_wind (scm_t_guard before,
			   scm_t_inner inner,
			   scm_t_guard after,
			   void *inner_data,
			   void *guard_data)
{
  SCM ans;

  scm_c_issue_deprecation_warning
    ("`scm_internal_dynamic_wind' is deprecated.  "
     "Use the `scm_dynwind_begin' / `scm_dynwind_end' API instead.");

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_rewind_handler (before, guard_data, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (after, guard_data, SCM_F_WIND_EXPLICITLY);
  ans = inner (inner_data);
  scm_dynwind_end ();
  return ans;
}



SCM
scm_immutable_cell (scm_t_bits car, scm_t_bits cdr)
{
  scm_c_issue_deprecation_warning
    ("scm_immutable_cell is deprecated.  Use scm_cell instead.");

  return scm_cell (car, cdr);
}

SCM
scm_immutable_double_cell (scm_t_bits car, scm_t_bits cbr,
			   scm_t_bits ccr, scm_t_bits cdr)
{
  scm_c_issue_deprecation_warning
    ("scm_immutable_double_cell is deprecated.  Use scm_double_cell instead.");

  return scm_double_cell (car, cbr, ccr, cdr);
}




SCM_GLOBAL_SYMBOL (scm_memory_alloc_key, "memory-allocation-error");
void
scm_memory_error (const char *subr)
{
  scm_c_issue_deprecation_warning
    ("scm_memory_error is deprecated.  Use scm_report_out_of_memory to raise "
     "an exception, or abort() to cause the program to exit.");

  fprintf (stderr, "FATAL: memory error in %s\n", subr);
  abort ();
}




static SCM var_slot_ref_using_class = SCM_BOOL_F;
static SCM var_slot_set_using_class_x = SCM_BOOL_F;
static SCM var_slot_bound_using_class_p = SCM_BOOL_F;
static SCM var_slot_exists_using_class_p = SCM_BOOL_F;

SCM scm_no_applicable_method = SCM_BOOL_F;

SCM var_get_keyword = SCM_BOOL_F;

SCM scm_class_boolean, scm_class_char, scm_class_pair;
SCM scm_class_procedure, scm_class_string, scm_class_symbol;
SCM scm_class_primitive_generic;
SCM scm_class_vector, scm_class_null;
SCM scm_class_integer, scm_class_real, scm_class_complex, scm_class_fraction;
SCM scm_class_unknown;
SCM scm_class_top, scm_class_object, scm_class_class;
SCM scm_class_applicable;
SCM scm_class_applicable_struct, scm_class_applicable_struct_with_setter;
SCM scm_class_generic, scm_class_generic_with_setter;
SCM scm_class_accessor;
SCM scm_class_extended_generic, scm_class_extended_generic_with_setter;
SCM scm_class_extended_accessor;
SCM scm_class_method;
SCM scm_class_accessor_method;
SCM scm_class_procedure_class;
SCM scm_class_applicable_struct_class;
SCM scm_class_number, scm_class_list;
SCM scm_class_keyword;
SCM scm_class_port, scm_class_input_output_port;
SCM scm_class_input_port, scm_class_output_port;
SCM scm_class_foreign_slot;
SCM scm_class_self, scm_class_protected;
SCM scm_class_hidden, scm_class_opaque, scm_class_read_only;
SCM scm_class_protected_hidden, scm_class_protected_opaque, scm_class_protected_read_only;
SCM scm_class_scm;
SCM scm_class_int, scm_class_float, scm_class_double;

SCM *scm_port_class, *scm_smob_class;

void
scm_init_deprecated_goops (void)
{
  var_slot_ref_using_class = scm_c_lookup ("slot-ref-using-class");
  var_slot_set_using_class_x = scm_c_lookup ("slot-set-using-class!");
  var_slot_bound_using_class_p = scm_c_lookup ("slot-bound-using-class?");
  var_slot_exists_using_class_p = scm_c_lookup ("slot-exists-using-class?");

  scm_no_applicable_method =
    scm_variable_ref (scm_c_lookup ("no-applicable-method"));

  var_get_keyword = scm_c_lookup ("get-keyword");

  scm_class_class = scm_variable_ref (scm_c_lookup ("<class>"));
  scm_class_top = scm_variable_ref (scm_c_lookup ("<top>"));
  scm_class_object = scm_variable_ref (scm_c_lookup ("<object>"));

  scm_class_foreign_slot = scm_variable_ref (scm_c_lookup ("<foreign-slot>"));
  scm_class_protected = scm_variable_ref (scm_c_lookup ("<protected-slot>"));
  scm_class_hidden = scm_variable_ref (scm_c_lookup ("<hidden-slot>"));
  scm_class_opaque = scm_variable_ref (scm_c_lookup ("<opaque-slot>"));
  scm_class_read_only = scm_variable_ref (scm_c_lookup ("<read-only-slot>"));
  scm_class_self = scm_variable_ref (scm_c_lookup ("<self-slot>"));
  scm_class_protected_opaque = scm_variable_ref (scm_c_lookup ("<protected-opaque-slot>"));
  scm_class_protected_hidden = scm_variable_ref (scm_c_lookup ("<protected-hidden-slot>"));
  scm_class_protected_read_only = scm_variable_ref (scm_c_lookup ("<protected-read-only-slot>"));
  scm_class_scm = scm_variable_ref (scm_c_lookup ("<scm-slot>"));
  scm_class_int = scm_variable_ref (scm_c_lookup ("<int-slot>"));
  scm_class_float = scm_variable_ref (scm_c_lookup ("<float-slot>"));
  scm_class_double = scm_variable_ref (scm_c_lookup ("<double-slot>"));

  /* scm_class_generic functions classes */
  scm_class_procedure_class = scm_variable_ref (scm_c_lookup ("<procedure-class>"));
  scm_class_applicable_struct_class = scm_variable_ref (scm_c_lookup ("<applicable-struct-class>"));

  scm_class_method = scm_variable_ref (scm_c_lookup ("<method>"));
  scm_class_accessor_method = scm_variable_ref (scm_c_lookup ("<accessor-method>"));
  scm_class_applicable = scm_variable_ref (scm_c_lookup ("<applicable>"));
  scm_class_applicable_struct = scm_variable_ref (scm_c_lookup ("<applicable-struct>"));
  scm_class_applicable_struct_with_setter = scm_variable_ref (scm_c_lookup ("<applicable-struct-with-setter>"));
  scm_class_generic = scm_variable_ref (scm_c_lookup ("<generic>"));
  scm_class_extended_generic = scm_variable_ref (scm_c_lookup ("<extended-generic>"));
  scm_class_generic_with_setter = scm_variable_ref (scm_c_lookup ("<generic-with-setter>"));
  scm_class_accessor = scm_variable_ref (scm_c_lookup ("<accessor>"));
  scm_class_extended_generic_with_setter = scm_variable_ref (scm_c_lookup ("<extended-generic-with-setter>"));
  scm_class_extended_accessor = scm_variable_ref (scm_c_lookup ("<extended-accessor>"));

  /* Primitive types classes */
  scm_class_boolean = scm_variable_ref (scm_c_lookup ("<boolean>"));
  scm_class_char = scm_variable_ref (scm_c_lookup ("<char>"));
  scm_class_list = scm_variable_ref (scm_c_lookup ("<list>"));
  scm_class_pair = scm_variable_ref (scm_c_lookup ("<pair>"));
  scm_class_null = scm_variable_ref (scm_c_lookup ("<null>"));
  scm_class_string = scm_variable_ref (scm_c_lookup ("<string>"));
  scm_class_symbol = scm_variable_ref (scm_c_lookup ("<symbol>"));
  scm_class_vector = scm_variable_ref (scm_c_lookup ("<vector>"));
  scm_class_number = scm_variable_ref (scm_c_lookup ("<number>"));
  scm_class_complex = scm_variable_ref (scm_c_lookup ("<complex>"));
  scm_class_real = scm_variable_ref (scm_c_lookup ("<real>"));
  scm_class_integer = scm_variable_ref (scm_c_lookup ("<integer>"));
  scm_class_fraction = scm_variable_ref (scm_c_lookup ("<fraction>"));
  scm_class_keyword = scm_variable_ref (scm_c_lookup ("<keyword>"));
  scm_class_unknown = scm_variable_ref (scm_c_lookup ("<unknown>"));
  scm_class_procedure = scm_variable_ref (scm_c_lookup ("<procedure>"));
  scm_class_primitive_generic = scm_variable_ref (scm_c_lookup ("<primitive-generic>"));
  scm_class_port = scm_variable_ref (scm_c_lookup ("<port>"));
  scm_class_input_port = scm_variable_ref (scm_c_lookup ("<input-port>"));
  scm_class_output_port = scm_variable_ref (scm_c_lookup ("<output-port>"));
  scm_class_input_output_port = scm_variable_ref (scm_c_lookup ("<input-output-port>"));

  scm_smob_class = scm_i_smob_class;
}

SCM
scm_get_keyword (SCM kw, SCM initargs, SCM default_value)
{
  scm_c_issue_deprecation_warning
    ("scm_get_keyword is deprecated.  Use `kw-arg-ref' from Scheme instead.");

  return scm_call_3 (scm_variable_ref (var_get_keyword),
                     kw, initargs, default_value);
}

#define BUFFSIZE 32		/* big enough for most uses */
#define SPEC_OF(x) \
  (scm_slot_ref (x, scm_slot_ref (x, scm_from_latin1_symbol ("specializers"))))
#define CPL_OF(x) \
  (scm_slot_ref (x, scm_slot_ref (x, scm_from_latin1_symbol ("cpl"))))

static SCM
scm_i_vector2list (SCM l, long len)
{
  long j;
  SCM z = scm_c_make_vector (len, SCM_UNDEFINED);

  for (j = 0; j < len; j++, l = SCM_CDR (l)) {
    SCM_SIMPLE_VECTOR_SET (z, j, SCM_CAR (l));
  }
  return z;
}

static int
applicablep (SCM actual, SCM formal)
{
  /* We already know that the cpl is well formed. */
  return scm_is_true (scm_c_memq (formal, CPL_OF (actual)));
}

static int
more_specificp (SCM m1, SCM m2, SCM const *targs)
{
  register SCM s1, s2;
  register long i;
  /*
   * Note:
   *   m1 and m2 can have != length (i.e. one can be one element longer than the
   * other when we have a dotted parameter list). For instance, with the call
   *   (M 1)
   * with
   *   (define-method M (a . l) ....)
   *   (define-method M (a) ....)
   *
   * we consider that the second method is more specific.
   *
   * BTW, targs is an array of types. We don't need it's size since
   * we already know that m1 and m2 are applicable (no risk to go past
   * the end of this array).
   *
   */
  for (i=0, s1=SPEC_OF(m1), s2=SPEC_OF(m2); ; i++, s1=SCM_CDR(s1), s2=SCM_CDR(s2)) {
    if (scm_is_null(s1)) return 1;
    if (scm_is_null(s2)) return 0;
    if (!scm_is_eq (SCM_CAR(s1), SCM_CAR(s2))) {
      register SCM l, cs1 = SCM_CAR(s1), cs2 = SCM_CAR(s2);

      for (l = CPL_OF (targs[i]);   ; l = SCM_CDR(l)) {
	if (scm_is_eq (cs1, SCM_CAR (l)))
	  return 1;
	if (scm_is_eq (cs2, SCM_CAR (l)))
	  return 0;
      }
      return 0;/* should not occur! */
    }
  }
  return 0; /* should not occur! */
}

static SCM
sort_applicable_methods (SCM method_list, long size, SCM const *targs)
{
  long i, j, incr;
  SCM *v, vector = SCM_EOL;
  SCM buffer[BUFFSIZE];
  SCM save = method_list;
  scm_t_array_handle handle;

  /* For reasonably sized method_lists we can try to avoid all the
   * consing and reorder the list in place...
   * This idea is due to David McClain <Dave_McClain@msn.com>
   */
  if (size <= BUFFSIZE)
    {
      for (i = 0;  i < size; i++)
	{
	  buffer[i]   = SCM_CAR (method_list);
	  method_list = SCM_CDR (method_list);
	}
      v = buffer;
    }
  else
    {
      /* Too many elements in method_list to keep everything locally */
      vector = scm_i_vector2list (save, size);
      v = scm_vector_writable_elements (vector, &handle, NULL, NULL);
    }

  /* Use a simple shell sort since it is generally faster than qsort on
   * small vectors (which is probably mostly the case when we have to
   * sort a list of applicable methods).
   */
  for (incr = size / 2; incr; incr /= 2)
    {
      for (i = incr; i < size; i++)
	{
	  for (j = i - incr; j >= 0; j -= incr)
	    {
	      if (more_specificp (v[j], v[j+incr], targs))
		break;
	      else
		{
		  SCM tmp = v[j + incr];
		  v[j + incr] = v[j];
		  v[j] = tmp;
		}
	    }
	}
    }

  if (size <= BUFFSIZE)
    {
      /* We did it in locally, so restore the original list (reordered) in-place */
      for (i = 0, method_list = save; i < size; i++, v++)
	{
	  SCM_SETCAR (method_list, *v);
	  method_list = SCM_CDR (method_list);
	}
      return save;
    }

  /* If we are here, that's that we did it the hard way... */
  scm_array_handle_release (&handle);
  return scm_vector_to_list (vector);
}

SCM
scm_compute_applicable_methods (SCM gf, SCM args, long len, int find_method_p)
{
  register long i;
  long count = 0;
  SCM l, fl, applicable = SCM_EOL;
  SCM save = args;
  SCM buffer[BUFFSIZE];
  SCM const *types;
  SCM *p;
  SCM tmp = SCM_EOL;
  scm_t_array_handle handle;

  scm_c_issue_deprecation_warning
    ("scm_compute_applicable_methods is deprecated.  Use "
     "`compute-applicable-methods' from Scheme instead.");

  /* Build the list of arguments types */
  if (len >= BUFFSIZE) 
    {
      tmp = scm_c_make_vector (len, SCM_UNDEFINED);
      types = p = scm_vector_writable_elements (tmp, &handle, NULL, NULL);

    /*
      note that we don't have to work to reset the generation
      count. TMP is a new vector anyway, and it is found
      conservatively.
    */
    }
  else
    types = p = buffer;

  for (  ; !scm_is_null (args); args = SCM_CDR (args))
    *p++ = scm_class_of (SCM_CAR (args));
  
  /* Build a list of all applicable methods */
  for (l = scm_generic_function_methods (gf); !scm_is_null (l); l = SCM_CDR (l))
    {
      fl = SPEC_OF (SCM_CAR (l));
      for (i = 0; ; i++, fl = SCM_CDR (fl))
	{
	  if (SCM_INSTANCEP (fl)
	      /* We have a dotted argument list */
	      || (i >= len && scm_is_null (fl)))
	    {	/* both list exhausted */
	      applicable = scm_cons (SCM_CAR (l), applicable);
	      count     += 1;
	      break;
	    }
	  if (i >= len
	      || scm_is_null (fl)
	      || !applicablep (types[i], SCM_CAR (fl)))
	    break;
	}
    }

  if (len >= BUFFSIZE)
      scm_array_handle_release (&handle);

  if (count == 0)
    {
      if (find_method_p)
	return SCM_BOOL_F;
      scm_call_2 (scm_no_applicable_method, gf, save);
      /* if we are here, it's because no-applicable-method hasn't signaled an error */
      return SCM_BOOL_F;
    }

  return (count == 1
	  ? applicable
	  : sort_applicable_methods (applicable, count, types));
}

SCM_SYMBOL (sym_compute_applicable_methods, "compute-applicable-methods");

SCM
scm_find_method (SCM l)
#define FUNC_NAME "find-method"
{
  SCM gf;
  long len = scm_ilength (l);

  if (len == 0)
    SCM_WRONG_NUM_ARGS ();

  scm_c_issue_deprecation_warning
    ("scm_find_method is deprecated.  Use `compute-applicable-methods' "
     "from Scheme instead.");

  gf = SCM_CAR(l); l = SCM_CDR(l);
  SCM_VALIDATE_GENERIC (1, gf);
  if (scm_is_null (scm_slot_ref (gf, scm_from_latin1_symbol ("methods"))))
    SCM_MISC_ERROR ("no methods for generic ~S", scm_list_1 (gf));

  return scm_compute_applicable_methods (gf, l, len - 1, 1);
}
#undef FUNC_NAME

SCM
scm_basic_make_class (SCM meta, SCM name, SCM dsupers, SCM dslots)
{
  scm_c_issue_deprecation_warning
    ("scm_basic_make_class is deprecated.  Use `define-class' in Scheme,"
     "or use `(make META #:name NAME #:dsupers DSUPERS #:slots DSLOTS)' "
     "in Scheme.");

  return scm_make_standard_class (meta, name, dsupers, dslots);
}

/* Scheme will issue the deprecation warning for these.  */
SCM
scm_slot_ref_using_class (SCM class, SCM obj, SCM slot_name)
{
  return scm_call_3 (scm_variable_ref (var_slot_ref_using_class),
                     class, obj, slot_name);
}

SCM
scm_slot_set_using_class_x (SCM class, SCM obj, SCM slot_name, SCM value)
{
  return scm_call_4 (scm_variable_ref (var_slot_set_using_class_x),
                     class, obj, slot_name, value);
}

SCM
scm_slot_bound_using_class_p (SCM class, SCM obj, SCM slot_name)
{
  return scm_call_3 (scm_variable_ref (var_slot_bound_using_class_p),
                     class, obj, slot_name);
}

SCM
scm_slot_exists_using_class_p (SCM class, SCM obj, SCM slot_name)
{
  return scm_call_3 (scm_variable_ref (var_slot_exists_using_class_p),
                     class, obj, slot_name);
}



#define FETCH_STORE(fet,mem,sto)                        \
  do {                                                  \
    scm_i_scm_pthread_mutex_lock (&scm_i_misc_mutex);   \
    (fet) = (mem);                                      \
    (mem) = (sto);                                      \
    scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);     \
  } while (0)

static scm_t_bits scm_tc16_arbiter;


#define SCM_LOCK_VAL         (scm_tc16_arbiter | (1L << 16))
#define SCM_UNLOCK_VAL       scm_tc16_arbiter
#define SCM_ARB_LOCKED(arb)  ((SCM_CELL_WORD_0 (arb)) & (1L << 16))


static int 
arbiter_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<arbiter ", port);
  if (SCM_ARB_LOCKED (exp))
    scm_puts ("locked ", port);
  scm_iprin1 (SCM_PACK (SCM_SMOB_DATA (exp)), port, pstate);
  scm_putc ('>', port);
  return !0;
}

SCM_DEFINE (scm_make_arbiter, "make-arbiter", 1, 0, 0, 
	    (SCM name),
	    "Return an arbiter object, initially unlocked.  Currently\n"
	    "@var{name} is only used for diagnostic output.")
#define FUNC_NAME s_scm_make_arbiter
{
  scm_c_issue_deprecation_warning
    ("Arbiters are deprecated.  "
     "Use mutexes or atomic variables instead.");

  SCM_RETURN_NEWSMOB (scm_tc16_arbiter, SCM_UNPACK (name));
}
#undef FUNC_NAME


/* The atomic FETCH_STORE here is so two threads can't both see the arbiter
   unlocked and return #t.  The arbiter itself wouldn't be corrupted by
   this, but two threads both getting #t would be contrary to the intended
   semantics.  */

SCM_DEFINE (scm_try_arbiter, "try-arbiter", 1, 0, 0, 
	    (SCM arb),
	    "If @var{arb} is unlocked, then lock it and return @code{#t}.\n"
	    "If @var{arb} is already locked, then do nothing and return\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_try_arbiter
{
  scm_t_bits old;
  scm_t_bits *loc;
  SCM_VALIDATE_SMOB (1, arb, arbiter);
  loc = (scm_t_bits*)SCM_SMOB_OBJECT_N_LOC (arb, 0);
  FETCH_STORE (old, *loc, SCM_LOCK_VAL);
  return scm_from_bool (old == SCM_UNLOCK_VAL);
}
#undef FUNC_NAME


/* The atomic FETCH_STORE here is so two threads can't both see the arbiter
   locked and return #t.  The arbiter itself wouldn't be corrupted by this,
   but we don't want two threads both thinking they were the unlocker.  The
   intended usage is for the code which locked to be responsible for
   unlocking, but we guarantee the return value even if multiple threads
   compete.  */

SCM_DEFINE (scm_release_arbiter, "release-arbiter", 1, 0, 0,
	    (SCM arb),
	    "If @var{arb} is locked, then unlock it and return @code{#t}.\n"
	    "If @var{arb} is already unlocked, then do nothing and return\n"
	    "@code{#f}.\n"
	    "\n"
	    "Typical usage is for the thread which locked an arbiter to\n"
	    "later release it, but that's not required, any thread can\n"
	    "release it.")
#define FUNC_NAME s_scm_release_arbiter
{
  scm_t_bits old;
  scm_t_bits *loc;
  SCM_VALIDATE_SMOB (1, arb, arbiter);
  loc = (scm_t_bits*)SCM_SMOB_OBJECT_N_LOC (arb, 0);
  FETCH_STORE (old, *loc, SCM_UNLOCK_VAL);
  return scm_from_bool (old == SCM_LOCK_VAL);
}
#undef FUNC_NAME




/* User asyncs. */

static scm_t_bits tc16_async;

/* cmm: this has SCM_ prefix because SCM_MAKE_VALIDATE expects it.
   this is ugly.  */
#define SCM_ASYNCP(X)		SCM_TYP16_PREDICATE (tc16_async, X)
#define VALIDATE_ASYNC(pos, a)	SCM_MAKE_VALIDATE_MSG(pos, a, ASYNCP, "user async")

#define ASYNC_GOT_IT(X)        (SCM_SMOB_FLAGS (X))
#define SET_ASYNC_GOT_IT(X, V) (SCM_SET_SMOB_FLAGS ((X), ((V))))
#define ASYNC_THUNK(X)         SCM_SMOB_OBJECT_1 (X)


SCM_DEFINE (scm_async, "async", 1, 0, 0,
	    (SCM thunk),
	    "Create a new async for the procedure @var{thunk}.")
#define FUNC_NAME s_scm_async
{
  scm_c_issue_deprecation_warning
    ("\"User asyncs\" are deprecated.  Use closures instead.");

  SCM_RETURN_NEWSMOB (tc16_async, SCM_UNPACK (thunk));
}
#undef FUNC_NAME

SCM_DEFINE (scm_async_mark, "async-mark", 1, 0, 0,
            (SCM a),
	    "Mark the async @var{a} for future execution.")
#define FUNC_NAME s_scm_async_mark
{
  VALIDATE_ASYNC (1, a);
  SET_ASYNC_GOT_IT (a, 1);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_run_asyncs, "run-asyncs", 1, 0, 0,
	    (SCM list_of_a),
	    "Execute all thunks from the asyncs of the list @var{list_of_a}.")
#define FUNC_NAME s_scm_run_asyncs
{
  while (! SCM_NULL_OR_NIL_P (list_of_a))
    {
      SCM a;
      SCM_VALIDATE_CONS (1, list_of_a);
      a = SCM_CAR (list_of_a);
      VALIDATE_ASYNC (SCM_ARG1, a);
      if (ASYNC_GOT_IT (a))
	{
	  SET_ASYNC_GOT_IT (a, 0);
	  scm_call_0 (ASYNC_THUNK (a));
	}
      list_of_a = SCM_CDR (list_of_a);
    }
  return SCM_BOOL_T;
}
#undef FUNC_NAME


static scm_i_pthread_mutex_t critical_section_mutex;
static SCM dynwind_critical_section_mutex;

void
scm_critical_section_start (void)
{
  scm_c_issue_deprecation_warning
    ("Critical sections are deprecated.  Instead use dynwinds and "
     "\"scm_dynwind_pthread_mutex_lock\" together with "
     "\"scm_dynwind_block_asyncs\" if appropriate.");

  scm_i_pthread_mutex_lock (&critical_section_mutex);
  SCM_I_CURRENT_THREAD->block_asyncs++;
}

void
scm_critical_section_end (void)
{
  SCM_I_CURRENT_THREAD->block_asyncs--;
  scm_i_pthread_mutex_unlock (&critical_section_mutex);
  scm_async_tick ();
}

void
scm_dynwind_critical_section (SCM mutex)
{
  scm_c_issue_deprecation_warning
    ("Critical sections are deprecated.  Instead use dynwinds and "
     "\"scm_dynwind_pthread_mutex_lock\" together with "
     "\"scm_dynwind_block_asyncs\" if appropriate.");

  if (scm_is_false (mutex))
    mutex = dynwind_critical_section_mutex;
  scm_dynwind_lock_mutex (mutex);
  scm_dynwind_block_asyncs ();
}




SCM
scm_make_mutex_with_flags (SCM flags)
{
  SCM kind = SCM_UNDEFINED;

  scm_c_issue_deprecation_warning
    ("'scm_make_mutex_with_flags' is deprecated.  "
     "Use 'scm_make_mutex_with_kind' instead.");

  if (!scm_is_null (flags))
    {
      if (!scm_is_null (scm_cdr (flags)))
	scm_misc_error (NULL, "too many mutex options: ~a", scm_list_1 (flags));
      kind = scm_car (flags);
    }

  return scm_make_mutex_with_kind (kind);
}

SCM
scm_lock_mutex_timed (SCM m, SCM timeout, SCM owner)
{
  scm_c_issue_deprecation_warning
    ("'scm_lock_mutex_timed' is deprecated.  "
     "Use 'scm_timed_lock_mutex' instead.");

  if (!SCM_UNBNDP (owner) && !scm_is_false (owner))
    scm_c_issue_deprecation_warning
      ("The 'owner' argument to 'scm_lock_mutex_timed' is deprecated.  "
       "Use SRFI-18 directly if you need this concept.");

  return scm_timed_lock_mutex (m, timeout);
}

SCM
scm_unlock_mutex_timed (SCM mx, SCM cond, SCM timeout)
{
  scm_c_issue_deprecation_warning
    ("'scm_unlock_mutex_timed' is deprecated.  "
     "Use just plain old 'scm_unlock_mutex' instead, or otherwise "
     "'scm_wait_condition_variable' if you need to.");

  if (!SCM_UNBNDP (cond) &&
      scm_is_false (scm_timed_wait_condition_variable (cond, mx, timeout)))
    return SCM_BOOL_F;

  return scm_unlock_mutex (mx);
}




void
scm_i_init_deprecated ()
{
  scm_tc16_arbiter = scm_make_smob_type ("arbiter", 0);
  scm_set_smob_print (scm_tc16_arbiter, arbiter_print);
  tc16_async = scm_make_smob_type ("async", 0);
  scm_i_pthread_mutex_init (&critical_section_mutex,
			    scm_i_pthread_mutexattr_recursive);
  dynwind_critical_section_mutex = scm_make_recursive_mutex ();
#include "libguile/deprecated.x"
}

#endif
