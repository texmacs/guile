/* Copyright 1995-1998,2000-2001,2003-2004,2006,2008,2009-2014,2017-2019
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <alloca.h>
#include <stdio.h>
#include <unistdio.h>

#include "boolean.h"
#include "control.h"
#include "eq.h"
#include "eval.h"
#include "fluids.h"
#include "gsubr.h"
#include "init.h"
#include "keywords.h"
#include "list.h"
#include "modules.h"
#include "numbers.h"
#include "pairs.h"
#include "ports.h"
#include "smob.h"
#include "stackchk.h"
#include "stacks.h"
#include "strings.h"
#include "symbols.h"
#include "variable.h"

#include "exceptions.h"


/* Pleasantly enough, the guts of exception handling are defined in
   Scheme, in terms of prompt, abort, and the %exception-handler fluid.
   Check boot-9 for the definitions.

   Still, it's useful to be able to raise unwind-only exceptions from C,
   for example so that we can recover from stack overflow.  We also need
   to have implementations of with-exception-handler and raise handy
   before boot time.  For that reason we have a parallel implementation
   of with-exception-handler that uses the same fluids here.  Exceptions
   raised from C still call out to Scheme though, so that pre-unwind
   handlers can be run.  */




/* First, some support for C bodies and exception handlers.  */

static scm_t_bits tc16_thunk;
static scm_t_bits tc16_exception_handler;

SCM
scm_c_make_thunk (scm_t_thunk thunk, void *data)
{
  SCM_RETURN_NEWSMOB2 (tc16_thunk, thunk, data);
}

SCM
scm_c_make_exception_handler (scm_t_exception_handler handler, void *data)
{
  SCM_RETURN_NEWSMOB2 (tc16_exception_handler, handler, data);
}

static SCM
call_thunk (SCM clo)
{
  scm_t_thunk thunk = (void*)SCM_SMOB_DATA (clo);
  void *data = (void*)SCM_SMOB_DATA_2 (clo);

  return thunk (data);
}

static SCM
call_exception_handler (SCM clo, SCM exn)
{
  scm_t_exception_handler handler = (void*)SCM_SMOB_DATA (clo);
  void *data = (void*)SCM_SMOB_DATA_2 (clo);

  return handler (data, exn);
}




/* Now, the implementation of with-exception-handler used internally to
   Guile at boot-time.  */

SCM_KEYWORD (kw_unwind_p, "unwind?");
SCM_KEYWORD (kw_unwind_for_type, "unwind-for-type");
static SCM exception_handler_fluid;
static SCM active_exception_handlers_fluid;
static SCM with_exception_handler_var;
static SCM raise_exception_var;

SCM
scm_c_with_exception_handler (SCM type, scm_t_exception_handler handler,
                              void *handler_data,
                              scm_t_thunk thunk, void *thunk_data)
{
  if (!scm_is_eq (type, SCM_BOOL_T) && !scm_is_symbol (type))
    scm_wrong_type_arg ("%with-exception-handler", 1, type);

  SCM prompt_tag = scm_cons (SCM_INUM0, SCM_EOL);
  scm_thread *t = SCM_I_CURRENT_THREAD;
  scm_t_dynstack *dynstack = &t->dynstack;
  scm_t_dynamic_state *dynamic_state = t->dynamic_state;
  jmp_buf registers;
  jmp_buf *prev_registers;
  ptrdiff_t saved_stack_depth;
  uint8_t *mra = NULL;

  prev_registers = t->vm.registers;
  saved_stack_depth = t->vm.stack_top - t->vm.sp;

  /* Push the prompt and exception handler onto the dynamic stack. */
  scm_dynstack_push_prompt (dynstack,
                            SCM_F_DYNSTACK_PROMPT_ESCAPE_ONLY,
                            prompt_tag,
                            t->vm.stack_top - t->vm.fp,
                            saved_stack_depth,
                            t->vm.ip,
                            mra,
                            &registers);
  scm_dynstack_push_fluid (dynstack, exception_handler_fluid,
                           scm_cons (prompt_tag, type),
                           dynamic_state);

  if (setjmp (registers))
    {
      /* A non-local return.  */
      SCM args;

      t->vm.registers = prev_registers;
      scm_gc_after_nonlocal_exit ();

      /* FIXME: We know where the args will be on the stack; we could
         avoid consing them.  */
      args = scm_i_prompt_pop_abort_args_x (&t->vm, saved_stack_depth);

      /* The first abort arg is the continuation, which is #f.  The
         second and final arg is the exception. */
      args = scm_cdr (args);
      SCM exn = scm_car (args);
      if (!scm_is_null (scm_cdr (args)))
        abort ();
      return handler (handler_data, exn);
    }

  SCM res = thunk (thunk_data);

  scm_dynstack_unwind_fluid (dynstack, dynamic_state);
  scm_dynstack_pop (dynstack);

  return res;
}

SCM
scm_with_exception_handler (SCM type, SCM handler, SCM thunk)
{
  return scm_call_6 (scm_variable_ref (with_exception_handler_var),
                     handler, thunk, kw_unwind_p, SCM_BOOL_T,
                     kw_unwind_for_type, type);
}

SCM
scm_with_pre_unwind_exception_handler (SCM handler, SCM thunk)
{
  return scm_call_2 (scm_variable_ref (with_exception_handler_var),
                     handler, thunk);
}




SCM_SYMBOL (sys_exception_sym, "%exception");
/* Note that these record types are marked as non-extensible, so their
   type predicate is a simple vtable comparison.  */
static SCM compound_exception;
static SCM exception_with_kind_and_args;
static SCM quit_exception;

static SCM
extract_exception (SCM obj, SCM non_extensible_vtable)
{
  if (!SCM_STRUCTP (obj)) {
    return SCM_BOOL_F;
  }
  if (scm_is_eq (SCM_STRUCT_VTABLE (obj), non_extensible_vtable)) {
    return obj;
  }
  if (!scm_is_eq (SCM_STRUCT_VTABLE (obj), compound_exception)) {
    return SCM_BOOL_F;
  }

  SCM exns = SCM_STRUCT_SLOT_REF (obj, 0);
  while (!scm_is_null (exns)) {
    SCM exn = scm_car (exns);
    if (scm_is_eq (SCM_STRUCT_VTABLE (exn), non_extensible_vtable)) {
      return exn;
    }
    exns = scm_cdr (exns);
  }
  return SCM_BOOL_F;
}

SCM
scm_exception_kind (SCM obj)
{
  SCM exn = extract_exception (obj, exception_with_kind_and_args);
  if (scm_is_false (exn)) {
    return sys_exception_sym;
  }
  return SCM_STRUCT_SLOT_REF (exn, 0);
}

SCM
scm_exception_args (SCM obj)
{
  SCM exn = extract_exception (obj, exception_with_kind_and_args);
  if (scm_is_false (exn)) {
    return scm_list_1 (obj);
  }
  return SCM_STRUCT_SLOT_REF (exn, 1);
}

static int
exception_has_type (SCM exn, SCM type)
{
  return scm_is_eq (type, SCM_BOOL_T) ||
    scm_is_eq (type, scm_exception_kind (exn));
}




void
scm_dynwind_throw_handler (void)
{
  scm_dynwind_fluid (active_exception_handlers_fluid, SCM_BOOL_F);
}




/* Default exception handlers.  */

/* Derive the an exit status from the arguments to (quit ...).  */
int
scm_exit_status (SCM args)
{
  if (scm_is_pair (args))
    {
      SCM cqa = SCM_CAR (args);

      if (scm_is_integer (cqa))
	return scm_to_int (cqa);
      else if (scm_is_false (cqa))
	return EXIT_FAILURE;
      else
        return EXIT_SUCCESS;
    }
  else if (scm_is_null (args))
    return EXIT_SUCCESS;
  else
    /* A type error.  Strictly speaking we shouldn't get here.  */
    return EXIT_FAILURE;
}

static SCM
get_quit_exception (SCM obj)
{
  return extract_exception (obj, quit_exception);
}

static int
quit_exception_code (SCM exn)
{
  return scm_to_int (SCM_STRUCT_SLOT_REF (exn, 0));
}

static void
scm_display_exception (SCM port, SCM exn)
{
  // FIXME: Make a good exception printer.
  scm_puts ("key: ", port);
  scm_write (scm_exception_kind (exn), port);
  scm_puts (", args: ", port);
  scm_write (scm_exception_args (exn), port);
  scm_newline (port);
}

static void
default_exception_handler (SCM exn)
{
  static int error_printing_error = 0;
  static int error_printing_fallback = 0;

  if (error_printing_fallback)
    fprintf (stderr, "\nFailed to print exception.\n");
  else if (error_printing_error)
    {
      fprintf (stderr, "\nError while printing exception:\n");
      error_printing_fallback = 1;
      scm_write (exn, scm_current_error_port ());
      scm_newline (scm_current_error_port ());
    }
  else if (scm_is_true (get_quit_exception (exn)))
    {
      exit (quit_exception_code (get_quit_exception (exn)));
    }
  else
    {
      SCM port = scm_current_error_port ();
      error_printing_error = 1;
      scm_puts ("Uncaught exception:\n", port);
      scm_display_exception (port, exn);
      scm_i_pthread_exit (NULL);
    }

  /* We fall through here for the error-printing-error cases.  */
  fprintf (stderr, "Aborting.\n");
  abort ();
}

static SCM
default_exception_handler_wrapper (void *data, SCM exn)
{
  default_exception_handler (exn);
  return SCM_UNDEFINED;
}

SCM
scm_c_with_default_exception_handler (scm_t_thunk thunk, void *data)
{
  return scm_c_with_exception_handler (SCM_BOOL_T,
                                       default_exception_handler_wrapper, NULL,
                                       thunk, data);
}




/* An implementation of "raise" for use during boot and in
   resource-exhaustion situations.  */



static void
emergency_raise (SCM exn, const char *reason)
{
  size_t depth = 0;

  /* This function is not only the boot implementation of "raise", it is
     also called in response to resource allocation failures such as
     stack-overflow or out-of-memory.  For that reason we need to be
     careful to avoid allocating memory.  */
  while (1)
    {
      SCM eh = scm_fluid_ref_star (exception_handler_fluid,
                                   scm_from_size_t (depth++));
      if (scm_is_false (eh)) {
        default_exception_handler (exn);
        abort ();
      }

      if (!scm_is_pair (eh)) {
        fprintf (stderr, "Warning: Unwind-only %s exception; "
                 "skipping pre-unwind handler.\n", reason);
      } else {
        SCM prompt_tag = scm_car (eh);
        SCM type = scm_cdr (eh);
        if (exception_has_type (exn, type)) {
          SCM tag_and_exn[] = { prompt_tag, exn };
          scm_i_vm_emergency_abort (tag_and_exn, 2);
          /* Unreachable.  */
          abort ();
        }
      }
    }
}

static SCM
pre_boot_raise (SCM exn)
{
  emergency_raise (exn, "pre-boot");
  return SCM_UNDEFINED;
}

SCM
scm_raise_exception (SCM exn)
{
  scm_call_1 (scm_variable_ref (raise_exception_var), exn);
  /* Should not be reached.  */
  abort ();
}




SCM_SYMBOL (scm_stack_overflow_key, "stack-overflow");
SCM_SYMBOL (scm_out_of_memory_key, "out-of-memory");

static SCM stack_overflow_exn = SCM_BOOL_F;
static SCM out_of_memory_exn = SCM_BOOL_F;

/* Since these two functions may be called in response to resource
   exhaustion, we have to avoid allocating memory.  */

void
scm_report_stack_overflow (void)
{
  if (scm_is_false (stack_overflow_exn))
    abort ();
  emergency_raise (stack_overflow_exn, "stack overflow");

  /* Not reached.  */
  abort ();
}

void
scm_report_out_of_memory (void)
{
  if (scm_is_false (out_of_memory_exn))
    abort ();
  emergency_raise (out_of_memory_exn, "out of memory");

  /* Not reached.  */
  abort ();
}

static SCM
make_scm_exception (SCM type, SCM subr, SCM message, SCM args, SCM rest)
{
  return scm_make_struct_simple
    (exception_with_kind_and_args,
     scm_list_2 (type,
                 scm_list_4 (subr, message, args, rest)));
}

static SCM
sys_init_exceptions_x (SCM compound_exception_type,
                       SCM exception_with_kind_and_args_type,
                       SCM quit_exception_type)
{
  compound_exception = compound_exception_type;
  exception_with_kind_and_args = exception_with_kind_and_args_type;
  quit_exception = quit_exception_type;


  /* Arguments as if from:

       scm_error (stack-overflow, NULL, "Stack overflow", #f, #f);

     We build the arguments manually to avoid allocating memory in
     emergency circumstances.  */
  stack_overflow_exn = make_scm_exception
    (scm_stack_overflow_key, SCM_BOOL_F,
     scm_from_latin1_string ("Stack overflow"), SCM_BOOL_F, SCM_BOOL_F);
  out_of_memory_exn = make_scm_exception
    (scm_out_of_memory_key, SCM_BOOL_F,
     scm_from_latin1_string ("Out of memory"), SCM_BOOL_F, SCM_BOOL_F);

  return SCM_UNDEFINED;
}




/* Initialization.  */

void
scm_init_exceptions ()
{
  tc16_thunk = scm_make_smob_type ("thunk", 0);
  scm_set_smob_apply (tc16_thunk, call_thunk, 0, 0, 0);

  tc16_exception_handler = scm_make_smob_type ("exception-handler", 0);
  scm_set_smob_apply (tc16_exception_handler, call_exception_handler, 1, 0, 0);

  exception_handler_fluid = scm_make_thread_local_fluid (SCM_BOOL_F);
  active_exception_handlers_fluid = scm_make_thread_local_fluid (SCM_BOOL_F);
  /* These binding are later removed when the Scheme definitions of
     raise and with-exception-handler are created in boot-9.scm.  */
  scm_c_define ("%exception-handler", exception_handler_fluid);
  scm_c_define ("%active-exception-handlers", active_exception_handlers_fluid);

  with_exception_handler_var =
    scm_c_define ("with-exception-handler", SCM_BOOL_F);
  raise_exception_var =
    scm_c_define ("raise-exception",
                  scm_c_make_gsubr ("raise-exception", 1, 0, 0,
                                    (scm_t_subr) pre_boot_raise));

  scm_c_define ("%init-exceptions!",
                scm_c_make_gsubr ("%init-exceptions!", 3, 0, 0,
                                  (scm_t_subr) sys_init_exceptions_x));

#include "exceptions.x"
}
