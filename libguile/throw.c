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

#include <stdio.h>
#include <unistdio.h>

#include "backtrace.h"
#include "boolean.h"
#include "debug.h"
#include "dynwind.h"
#include "eq.h"
#include "eval.h"
#include "exceptions.h"
#include "fluids.h"
#include "gsubr.h"
#include "init.h"
#include "list.h"
#include "modules.h"
#include "numbers.h"
#include "pairs.h"
#include "ports.h"
#include "private-options.h"
#include "smob.h"
#include "stackchk.h"
#include "stacks.h"
#include "strings.h"
#include "symbols.h"
#include "variable.h"
#include "vm.h"

#include "throw.h"




static SCM throw_var;




/* TAG is the catch tag.  Typically, this is a symbol, but this
   function doesn't actually care about that.

   BODY is a pointer to a C function which runs the body of the catch;
   this is the code you can throw from.  We call it like this:
      BODY (BODY_DATA)
   where:
      BODY_DATA is just the BODY_DATA argument we received; we pass it
	 through to BODY as its first argument.  The caller can make
	 BODY_DATA point to anything useful that BODY might need.

   HANDLER is a pointer to a C function to deal with a throw to TAG,
   should one occur.  We call it like this:
      HANDLER (HANDLER_DATA, THROWN_TAG, THROW_ARGS)
   where
      HANDLER_DATA is the HANDLER_DATA argument we recevied; it's the
         same idea as BODY_DATA above.
      THROWN_TAG is the tag that the user threw to; usually this is
         TAG, but it could be something else if TAG was #t (i.e., a
         catch-all), or the user threw to a jmpbuf.
      THROW_ARGS is the list of arguments the user passed to the THROW
         function, after the tag.

   BODY_DATA is just a pointer we pass through to BODY.  HANDLER_DATA
   is just a pointer we pass through to HANDLER.  We don't actually
   use either of those pointers otherwise ourselves.  The idea is
   that, if our caller wants to communicate something to BODY or
   HANDLER, it can pass a pointer to it as MUMBLE_DATA, which BODY and
   HANDLER can then use.  Think of it as a way to make BODY and
   HANDLER closures, not just functions; MUMBLE_DATA points to the
   enclosed variables.

   Of course, it's up to the caller to make sure that any data a
   MUMBLE_DATA needs is protected from GC.  A common way to do this is
   to make MUMBLE_DATA a pointer to data stored in an automatic
   structure variable; since the collector must scan the stack for
   references anyway, this assures that any references in MUMBLE_DATA
   will be found.  */

struct scm_catch_data
{
  SCM tag;
  scm_t_thunk body;
  void *body_data;
  scm_t_catch_handler handler;
  void *handler_data;
  scm_t_catch_handler pre_unwind_handler;
  void *pre_unwind_handler_data;
  SCM pre_unwind_running;
};

static SCM
catch_post_unwind_handler (void *data, SCM exn)
{
  struct scm_catch_data *catch_data = data;
  return catch_data->handler (catch_data->handler_data,
                              scm_exception_kind (exn),
                              scm_exception_args (exn));
}

static SCM
catch_pre_unwind_handler (void *data, SCM exn)
{
  struct scm_catch_data *catch_data = data;
  SCM kind = scm_exception_kind (exn);
  SCM args = scm_exception_args (exn);
  if ((scm_is_eq (catch_data->tag, SCM_BOOL_T)
       || scm_is_eq (kind, catch_data->tag))
      && scm_is_false (scm_fluid_ref (catch_data->pre_unwind_running))) {
    scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
    scm_dynwind_throw_handler ();
    scm_dynwind_fluid (catch_data->pre_unwind_running, SCM_BOOL_T);
    catch_data->pre_unwind_handler (catch_data->pre_unwind_handler_data,
                                    kind, args);
    scm_dynwind_end ();
  }
  return scm_raise_exception (exn);
}

static SCM
catch_body (void *data)
{
  struct scm_catch_data *catch_data = data;

  if (catch_data->pre_unwind_handler) {
    SCM thunk = scm_c_make_thunk (catch_data->body, catch_data->body_data);
    SCM handler = scm_c_make_exception_handler (catch_pre_unwind_handler, data);
    SCM fluid = scm_make_thread_local_fluid (SCM_BOOL_F);
    catch_data->pre_unwind_running = fluid;
    return scm_with_pre_unwind_exception_handler (handler, thunk);
  }

  return catch_data->body (catch_data->body_data);
}

SCM
scm_c_catch (SCM tag,
	     scm_t_thunk body, void *body_data,
	     scm_t_catch_handler handler, void *handler_data,
	     scm_t_catch_handler pre_unwind_handler, void *pre_unwind_handler_data)
{
  struct scm_catch_data data =
    { tag, body, body_data, handler, handler_data, pre_unwind_handler,
      pre_unwind_handler_data, SCM_BOOL_F };

  return scm_c_with_exception_handler (tag, catch_post_unwind_handler, &data,
                                       catch_body, &data);
}

SCM
scm_internal_catch (SCM tag,
		    scm_t_thunk body, void *body_data,
		    scm_t_catch_handler handler, void *handler_data)
{
  return scm_c_catch (tag,
                      body, body_data,
                      handler, handler_data,
                      NULL, NULL);
}


SCM
scm_c_with_throw_handler (SCM tag,
			  scm_t_thunk body,
			  void *body_data,
			  scm_t_catch_handler handler,
			  void *handler_data,
			  int lazy_catch_p)
{
  struct scm_catch_data data =
    { tag, body, body_data, NULL, NULL, handler, handler_data, SCM_BOOL_F };

  if (lazy_catch_p)
    /* Non-zero lazy_catch_p arguments have been deprecated since
       2010.  */
    abort ();

  return catch_body (&data);
}

static SCM
call_thunk (void* data)
{
  return scm_call_0 (PTR2SCM (data));
}

static SCM
call_handler (void* data, SCM a, SCM b)
{
  return scm_call_2 (PTR2SCM (data), a, b);
}

SCM
scm_catch (SCM key, SCM thunk, SCM handler)
{
  return scm_c_catch (key, call_thunk, SCM2PTR (thunk),
                      call_handler, SCM2PTR (handler), NULL, NULL);
}

SCM
scm_catch_with_pre_unwind_handler (SCM key, SCM thunk, SCM handler,
                                   SCM pre_unwind_handler)
{
  if (SCM_UNBNDP (pre_unwind_handler))
    return scm_catch (key, thunk, handler);

  return scm_c_catch (key, call_thunk, SCM2PTR (thunk),
                      call_handler, SCM2PTR (handler),
                      call_handler, SCM2PTR (pre_unwind_handler));
}

SCM
scm_with_throw_handler (SCM key, SCM thunk, SCM handler)
{
  return scm_c_with_throw_handler (key, call_thunk, SCM2PTR (thunk),
                                   call_handler, SCM2PTR (handler), 0);
}

SCM
scm_throw (SCM key, SCM args)
{
  SCM throw = scm_variable_ref (throw_var);
  if (scm_is_false (throw)) {
    static int error_printing_error = 0;
    if (error_printing_error++)
      {
        fprintf (stderr, "Error while printing pre-boot error: %s\n",
                 scm_i_symbol_chars (key));
      }
    else
      {
        SCM port = scm_current_error_port ();
        scm_puts ("Pre-boot error; key: ", port);
        scm_write (key, port);
        scm_puts (", args: ", port);
        scm_write (args, port);
      }
    abort ();
  }
  scm_apply_1 (throw, key, args);
  /* Should not be reached.  */
  abort ();
}



/* Now some support for C bodies and catch handlers */

static scm_t_bits tc16_catch_handler;

SCM
scm_i_make_catch_handler (scm_t_catch_handler handler, void *data)
{
  SCM_RETURN_NEWSMOB2 (tc16_catch_handler, handler, data);
}

static SCM
apply_catch_handler (SCM clo, SCM args)
{
  scm_t_catch_handler handler = (void*)SCM_SMOB_DATA (clo);
  void *data = (void*)SCM_SMOB_DATA_2 (clo);
  return handler (data, scm_car (args), scm_cdr (args));
}


/* body and handler functions for use with any of the above catch variants */

/* This is a body function you can pass to scm_internal_catch if you
   want the body to be like Scheme's `catch' --- a thunk.

   BODY_DATA is a pointer to a scm_body_thunk_data structure, which
   contains the Scheme procedure to invoke as the body, and the tag
   we're catching.  */

SCM
scm_body_thunk (void *body_data)
{
  struct scm_body_thunk_data *c = (struct scm_body_thunk_data *) body_data;

  return scm_call_0 (c->body_proc);
}


/* This is a handler function you can pass to scm_internal_catch if
   you want the handler to act like Scheme's catch: (throw TAG ARGS ...)
   applies a handler procedure to (TAG ARGS ...).

   If the user does a throw to this catch, this function runs a
   handler procedure written in Scheme.  HANDLER_DATA is a pointer to
   an SCM variable holding the Scheme procedure object to invoke.  It
   ought to be a pointer to an automatic variable (i.e., one living on
   the stack), or the procedure object should be otherwise protected
   from GC.  */
SCM
scm_handle_by_proc (void *handler_data, SCM tag, SCM throw_args)
{
  SCM *handler_proc_p = (SCM *) handler_data;

  return scm_apply_1 (*handler_proc_p, tag, throw_args);
}

/* SCM_HANDLE_BY_PROC_CATCHING_ALL is like SCM_HANDLE_BY_PROC but
   catches all throws that the handler might emit itself.  The handler
   used for these `secondary' throws is SCM_HANDLE_BY_MESSAGE_NO_EXIT.  */

struct hbpca_data {
  SCM proc;
  SCM args;
};

static SCM
hbpca_body (void *body_data)
{
  struct hbpca_data *data = (struct hbpca_data *)body_data;
  return scm_apply_0 (data->proc, data->args);
}

SCM
scm_handle_by_proc_catching_all (void *handler_data, SCM tag, SCM throw_args)
{
  SCM *handler_proc_p = (SCM *) handler_data;
  struct hbpca_data data;
  data.proc = *handler_proc_p;
  data.args = scm_cons (tag, throw_args);

  return scm_internal_catch (SCM_BOOL_T,
			     hbpca_body, &data,
			     scm_handle_by_message_noexit, NULL);
}


static int
should_print_backtrace (SCM tag, SCM stack)
{
  return SCM_BACKTRACE_P
    && scm_is_true (stack)
    && scm_initialized_p
    /* It's generally not useful to print backtraces for errors reading
       or expanding code in these fallback catch statements. */
    && !scm_is_eq (tag, scm_from_latin1_symbol ("read-error"))
    && !scm_is_eq (tag, scm_from_latin1_symbol ("syntax-error"));
}

static void
handler_message (void *handler_data, SCM tag, SCM args)
{
  SCM p, stack, frame;

  p = scm_current_error_port ();
  /* Usually we get here via a throw to a catch-all.  In that case
     there is the throw frame active, and the catch closure, so narrow by
     two frames.  It is possible for a user to invoke
     scm_handle_by_message directly, though, so it could be this
     narrows too much.  We'll have to see how this works out in
     practice.  */
  stack = scm_make_stack (SCM_BOOL_T, scm_list_1 (scm_from_int (2)));
  frame = scm_is_true (stack) ? scm_stack_ref (stack, SCM_INUM0) : SCM_BOOL_F;

  if (should_print_backtrace (tag, stack))
    {
      scm_puts ("Backtrace:\n", p);
      scm_display_backtrace_with_highlights (stack, p,
                                             SCM_BOOL_F, SCM_BOOL_F,
                                             SCM_EOL);
      scm_newline (p);
    }

  scm_print_exception (p, frame, tag, args);
}


/* This is a handler function to use if you want scheme to print a
   message and die.  Useful for dealing with throws to uncaught keys
   at the top level.

   At boot time, we establish a catch-all that uses this as its handler.
   1) If the user wants something different, they can use (catch #t
   ...) to do what they like.
   2) Outside the context of a read-eval-print loop, there isn't
   anything else good to do; libguile should not assume the existence
   of a read-eval-print loop.
   3) Given that we shouldn't do anything complex, it's much more
   robust to do it in C code.

   HANDLER_DATA, if non-zero, is assumed to be a char * pointing to a
   message header to print; if zero, we use "guile" instead.  That
   text is followed by a colon, then the message described by ARGS.  */

/* Dirk:FIXME:: The name of the function should make clear that the
 * application gets terminated.
 */

SCM
scm_handle_by_message (void *handler_data, SCM tag, SCM args)
{
  if (scm_is_true (scm_eq_p (tag, scm_from_latin1_symbol ("quit"))))
    exit (scm_exit_status (args));

  handler_message (handler_data, tag, args);
  scm_i_pthread_exit (NULL);

  /* this point not reached, but suppress gcc warning about no return value
     in case scm_i_pthread_exit isn't marked as "noreturn" (which seemed not
     to be the case on cygwin for instance) */
  return SCM_BOOL_F;
}


/* This is just like scm_handle_by_message, but it doesn't exit; it
   just returns #f.  It's useful in cases where you don't really know
   enough about the body to handle things in a better way, but don't
   want to let throws fall off the bottom of the wind list.  */
SCM
scm_handle_by_message_noexit (void *handler_data, SCM tag, SCM args)
{
  if (scm_is_true (scm_eq_p (tag, scm_from_latin1_symbol ("quit"))))
    exit (scm_exit_status (args));

  handler_message (handler_data, tag, args);

  return SCM_BOOL_F;
}


SCM
scm_handle_by_throw (void *handler_data SCM_UNUSED, SCM tag, SCM args)
{
  scm_ithrow (tag, args, 1);
  return SCM_UNSPECIFIED;  /* never returns */
}

SCM
scm_ithrow (SCM key, SCM args, int no_return SCM_UNUSED)
{
  scm_throw (key, args);
}

void
scm_init_throw ()
{
  tc16_catch_handler = scm_make_smob_type ("catch-handler", 0);
  scm_set_smob_apply (tc16_catch_handler, apply_catch_handler, 0, 0, 1);

  throw_var = scm_c_define ("throw", SCM_BOOL_F);

#include "throw.x"
}
