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
 * list in the order they were added to the list.
 */

void
scm_i_async_push (scm_i_thread *t, SCM proc)
{
  SCM asyncs;

  /* The usual algorithm you'd use for atomics with GC would be
     something like:

     repeat
       l = get(asyncs);
     until swap(l, cons(proc, l))

     But this is a LIFO list of asyncs, and that's not so great.  To
     make it FIFO, you'd do:

     repeat
       l = get(asyncs);
     until swap(l, append(l, list(proc)))

     However, some parts of Guile need to add entries to the async list
     from a context in which allocation is unsafe, for example right
     before GC or from a signal handler.  They do that by pre-allocating
     a pair, then when the interrupt fires the code does a setcdr of
     that pair to the t->pending_asyncs and atomically updates
     t->pending_asyncs.  So the append strategy doesn't work.

     Instead to preserve the FIFO behavior we atomically cut off the
     tail of the asyncs every time we want to run an interrupt, then
     disable that newly-severed tail by setting its cdr to #f.  Not so
     nice, but oh well.  */
  asyncs = scm_atomic_ref_scm (&t->pending_asyncs);
  do
    {
      /* Traverse the asyncs list atomically.  */
      SCM walk;
      for (walk = asyncs;
           scm_is_pair (walk);
           walk = scm_atomic_ref_scm (SCM_CDRLOC (walk)))
        if (scm_is_eq (SCM_CAR (walk), proc))
          return;
    }
  while (!scm_atomic_compare_and_swap_scm (&t->pending_asyncs, &asyncs,
                                           scm_cons (proc, asyncs)));
}

/* Precondition: there are pending asyncs.  */
SCM
scm_i_async_pop (scm_i_thread *t)
{
  while (1)
    {
      SCM asyncs, last_pair, penultimate_pair;

      last_pair = asyncs = scm_atomic_ref_scm (&t->pending_asyncs);
      penultimate_pair = SCM_BOOL_F;

      /* Since we are the only writer to cdrs of pairs in ASYNCS, and these
         pairs were given to us after an atomic update to t->pending_asyncs,
         no need to use atomic ops to traverse the list.  */
      while (scm_is_pair (SCM_CDR (last_pair)))
        {
          penultimate_pair = last_pair;
          last_pair = SCM_CDR (last_pair);
        }

      /* Sever the tail.  */
      if (scm_is_false (penultimate_pair))
        {
          if (!scm_atomic_compare_and_swap_scm (&t->pending_asyncs, &asyncs,
                                                SCM_EOL))
            continue;
        }
      else
        scm_atomic_set_scm (SCM_CDRLOC (penultimate_pair), SCM_EOL);

      /* Disable it.  */
      scm_atomic_set_scm (SCM_CDRLOC (last_pair), SCM_BOOL_F);

      return SCM_CAR (last_pair);
    }
}

void
scm_async_tick (void)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (t->block_asyncs)
    return;

  while (!scm_is_null (scm_atomic_ref_scm (&t->pending_asyncs)))
    scm_call_0 (scm_i_async_pop (t));
}

struct scm_thread_wake_data {
  enum { WAIT_FD, WAIT_COND } kind;
  union {
    struct {
      int fd;
    } wait_fd;
    struct {
      scm_i_pthread_mutex_t *mutex;
      scm_i_pthread_cond_t *cond;
    } wait_cond;
  } data;
};

int
scm_i_prepare_to_wait (scm_i_thread *t,
                       struct scm_thread_wake_data *wake)
{
  if (t->block_asyncs)
    return 0;

  scm_atomic_set_pointer ((void **)&t->wake, wake);

  /* If no interrupt was registered in the meantime, then any future
     wakeup will signal the FD or cond var.  */
  if (scm_is_null (scm_atomic_ref_scm (&t->pending_asyncs)))
    return 0;

  /* Otherwise clear the wake pointer and indicate that the caller
     should handle interrupts directly.  */
  scm_i_wait_finished (t);
  return 1;
}

void
scm_i_wait_finished (scm_i_thread *t)
{
  scm_atomic_set_pointer ((void **)&t->wake, NULL);
}

int
scm_i_prepare_to_wait_on_fd (scm_i_thread *t, int fd)
{
  struct scm_thread_wake_data *wake;
  wake = scm_gc_typed_calloc (struct scm_thread_wake_data);
  wake->kind = WAIT_FD;
  wake->data.wait_fd.fd = fd;
  return scm_i_prepare_to_wait (t, wake);
}

int
scm_c_prepare_to_wait_on_fd (int fd)
{
  return scm_i_prepare_to_wait_on_fd (SCM_I_CURRENT_THREAD, fd);
}

int
scm_i_prepare_to_wait_on_cond (scm_i_thread *t,
                               scm_i_pthread_mutex_t *m,
                               scm_i_pthread_cond_t *c)
{
  struct scm_thread_wake_data *wake;
  wake = scm_gc_typed_calloc (struct scm_thread_wake_data);
  wake->kind = WAIT_COND;
  wake->data.wait_cond.mutex = m;
  wake->data.wait_cond.cond = c;
  return scm_i_prepare_to_wait (t, wake);
}

int
scm_c_prepare_to_wait_on_cond (scm_i_pthread_mutex_t *m,
                               scm_i_pthread_cond_t *c)
{
  return scm_i_prepare_to_wait_on_cond (SCM_I_CURRENT_THREAD, m, c);
}

void
scm_c_wait_finished (void)
{
  scm_i_wait_finished (SCM_I_CURRENT_THREAD);
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
  struct scm_thread_wake_data *wake;

  if (SCM_UNBNDP (thread))
    t = SCM_I_CURRENT_THREAD;
  else
    {
      SCM_VALIDATE_THREAD (2, thread);
      t = SCM_I_THREAD_DATA (thread);
    }

  scm_i_async_push (t, proc);

  /* At this point the async is enqueued.  However if the thread is
     sleeping, we have to wake it up.  */
  if ((wake = scm_atomic_ref_pointer ((void **) &t->wake)))
    {
      /* By now, the thread T might be out of its sleep already, or
	 might even be in the next, unrelated sleep.  Interrupting it
	 anyway does no harm, however.

	 The important thing to prevent here is to signal the cond
	 before T waits on it.  This can not happen since T has its
	 mutex locked while preparing the wait and will only unlock it
	 again while waiting on the cond.
      */
      if (wake->kind == WAIT_COND)
        {
          scm_i_scm_pthread_mutex_lock (wake->data.wait_cond.mutex);
          scm_i_pthread_cond_signal (wake->data.wait_cond.cond);
          scm_i_pthread_mutex_unlock (wake->data.wait_cond.mutex);
        }
      else if (wake->kind == WAIT_FD)
        {
          char dummy = 0;

          /* Likewise, T might already been done with sleeping here, but
             interrupting it once too often does no harm.  T might also
             not yet have started sleeping, but this is no problem
             either since the data written to a pipe will not be lost,
             unlike a condition variable signal.  */
          full_write (wake->data.wait_fd.fd, &dummy, 1);
        }
      else
        abort ();
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



void
scm_init_async ()
{
#include "libguile/async.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
