/* Copyright (C) 1995,1996,1997,1998,2000,2001, 2002, 2004, 2006, 2008,
 *   2009, 2010, 2011, 2014 Free Software Foundation, Inc.
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
#  include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/atomics-internal.h"
#include "libguile/eval.h"
#include "libguile/throw.h"
#include "libguile/root.h"
#include "libguile/smob.h"
#include "libguile/dynwind.h"
#include "libguile/deprecation.h"

#include "libguile/validate.h"
#include "libguile/async.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <unistd.h>

#include <full-write.h>


/* {Asynchronous Events}
 *
 * Asyncs are used to run arbitrary code at the next safe point in a
 * specified thread.  You can use them to trigger execution of Scheme
 * code from signal handlers or to interrupt a thread, for example.
 *
 * Each thread has a list of 'activated asyncs', which is a normal
 * Scheme list of procedures with zero arguments.  When a thread
 * executes an scm_async_tick (), it will call all procedures on this
 * list.
 */


void
scm_async_tick (void)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  SCM asyncs;

  if (t->block_asyncs)
    return;

  asyncs = scm_atomic_swap_scm (&t->pending_asyncs, SCM_EOL);
  while (!scm_is_null (asyncs))
    {
      SCM next = scm_cdr (asyncs);
      scm_call_0 (scm_car (asyncs));
      scm_set_cdr_x (asyncs, SCM_BOOL_F);
      asyncs = next;
    }
}

int
scm_i_setup_sleep (scm_i_thread *t,
		   SCM sleep_object, scm_i_pthread_mutex_t *sleep_mutex,
		   int sleep_fd)
{
  struct scm_thread_wake_data *wake;

  wake = scm_gc_typed_calloc (struct scm_thread_wake_data);
  wake->object = sleep_object;
  wake->mutex = sleep_mutex;
  wake->fd = sleep_fd;

  scm_atomic_set_pointer ((void **)&t->wake, wake);

  return !scm_is_null (scm_atomic_ref_scm (&t->pending_asyncs));
}

void
scm_i_reset_sleep (scm_i_thread *t)
{
  scm_atomic_set_pointer ((void **)&t->wake, NULL);
}

SCM_DEFINE (scm_system_async_mark_for_thread, "system-async-mark", 1, 1, 0,
           (SCM proc, SCM thread),
	    "Mark @var{proc} (a procedure with zero arguments) for future execution\n"
	    "in @var{thread}.  If @var{proc} has already been marked for\n"
	    "@var{thread} but has not been executed yet, this call has no effect.\n"
	    "If @var{thread} is omitted, the thread that called\n"
	    "@code{system-async-mark} is used.\n\n"
	    "This procedure is not safe to be called from C signal handlers.  Use\n"
	    "@code{scm_sigaction} or @code{scm_sigaction_for_thread} to install\n"
	    "signal handlers.")
#define FUNC_NAME s_scm_system_async_mark_for_thread
{
  scm_i_thread *t;
  SCM asyncs;
  struct scm_thread_wake_data *wake;

  if (SCM_UNBNDP (thread))
    t = SCM_I_CURRENT_THREAD;
  else
    {
      SCM_VALIDATE_THREAD (2, thread);
      if (scm_c_thread_exited_p (thread))
	SCM_MISC_ERROR ("thread has already exited", SCM_EOL);
      t = SCM_I_THREAD_DATA (thread);
    }

  asyncs = scm_atomic_ref_scm (&t->pending_asyncs);
  do
    if (scm_is_true (scm_c_memq (proc, asyncs)))
      return SCM_UNSPECIFIED;
  while (!scm_atomic_compare_and_swap_scm (&t->pending_asyncs, &asyncs,
                                           scm_cons (proc, asyncs)));

  /* At this point the async is enqueued.  However if the thread is
     sleeping, we have to wake it up.  */
  if ((wake = scm_atomic_ref_pointer ((void **) &t->wake)))
    {
      /* By now, the thread T might be out of its sleep already, or
	 might even be in the next, unrelated sleep.  Interrupting it
	 anyway does no harm, however.

	 The important thing to prevent here is to signal sleep_cond
	 before T waits on it.  This can not happen since T has
	 sleep_mutex locked while setting t->sleep_mutex and will only
	 unlock it again while waiting on sleep_cond.
      */
      scm_i_scm_pthread_mutex_lock (wake->mutex);
      scm_i_pthread_cond_signal (&t->sleep_cond);
      scm_i_pthread_mutex_unlock (wake->mutex);

      /* This is needed to protect wake->mutex.
       */
      scm_remember_upto_here_1 (wake->object);

      if (wake->fd >= 0)
        {
          char dummy = 0;

          /* Likewise, T might already been done with sleeping here, but
             interrupting it once too often does no harm.  T might also
             not yet have started sleeping, but this is no problem
             either since the data written to a pipe will not be lost,
             unlike a condition variable signal.  */
          full_write (wake->fd, &dummy, 1);
        }
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_system_async_mark (SCM proc)
#define FUNC_NAME s_scm_system_async_mark_for_thread
{
  return scm_system_async_mark_for_thread (proc, SCM_UNDEFINED);
}
#undef FUNC_NAME




SCM_DEFINE (scm_noop, "noop", 0, 0, 1,
	    (SCM args),
	    "Do nothing.  When called without arguments, return @code{#f},\n"
	    "otherwise return the first argument.")
#define FUNC_NAME s_scm_noop
{
  SCM_VALIDATE_REST_ARGUMENT (args);
  return (SCM_NULL_OR_NIL_P (args) ? SCM_BOOL_F : SCM_CAR (args));
}
#undef FUNC_NAME




static void
increase_block (void *data)
{
  scm_i_thread *t = data;
  t->block_asyncs++;
}

static void
decrease_block (void *data)
{
  scm_i_thread *t = data;
  if (--t->block_asyncs == 0)
    scm_async_tick ();
}

void
scm_dynwind_block_asyncs (void)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  scm_dynwind_rewind_handler (increase_block, t, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (decrease_block, t, SCM_F_WIND_EXPLICITLY);
}

void
scm_dynwind_unblock_asyncs (void)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  if (t->block_asyncs == 0)
    scm_misc_error ("scm_with_unblocked_asyncs", 
		    "asyncs already unblocked", SCM_EOL);
  scm_dynwind_rewind_handler (decrease_block, t, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (increase_block, t, SCM_F_WIND_EXPLICITLY);
}

SCM_DEFINE (scm_call_with_blocked_asyncs, "call-with-blocked-asyncs", 1, 0, 0,
	    (SCM proc),
	    "Call @var{proc} with no arguments and block the execution\n"
	    "of system asyncs by one level for the current thread while\n"
	    "it is running.  Return the value returned by @var{proc}.\n")
#define FUNC_NAME s_scm_call_with_blocked_asyncs
{
  SCM ans;

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_block_asyncs ();
  ans = scm_call_0 (proc);
  scm_dynwind_end ();

  return ans;
}
#undef FUNC_NAME

void *
scm_c_call_with_blocked_asyncs (void *(*proc) (void *data), void *data)
{
  void* ans;

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_block_asyncs ();
  ans = proc (data);
  scm_dynwind_end ();

  return ans;
}


SCM_DEFINE (scm_call_with_unblocked_asyncs, "call-with-unblocked-asyncs", 1, 0, 0,
	    (SCM proc),
	    "Call @var{proc} with no arguments and unblock the execution\n"
	    "of system asyncs by one level for the current thread while\n"
	    "it is running.  Return the value returned by @var{proc}.\n")
#define FUNC_NAME s_scm_call_with_unblocked_asyncs
{
  SCM ans;

  if (SCM_I_CURRENT_THREAD->block_asyncs == 0)
    SCM_MISC_ERROR ("asyncs already unblocked", SCM_EOL);

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_unblock_asyncs ();
  ans = scm_call_0 (proc);
  scm_dynwind_end ();

  return ans;
}
#undef FUNC_NAME

void *
scm_c_call_with_unblocked_asyncs (void *(*proc) (void *data), void *data)
{
  void* ans;

  if (SCM_I_CURRENT_THREAD->block_asyncs == 0)
    scm_misc_error ("scm_c_call_with_unblocked_asyncs",
		    "asyncs already unblocked", SCM_EOL);

  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_unblock_asyncs ();
  ans = proc (data);
  scm_dynwind_end ();

  return ans;
}


static scm_i_pthread_mutex_t critical_section_mutex;

void
scm_critical_section_start (void)
{
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
scm_init_async ()
{
  scm_i_pthread_mutex_init (&critical_section_mutex,
			    scm_i_pthread_mutexattr_recursive);
#include "libguile/async.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
