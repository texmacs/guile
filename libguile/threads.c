/* Copyright (C) 1995, 1996, 1997, 1998, 2000, 2001, 2002, 2003, 2004,
 *   2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013,
 *   2014 Free Software Foundation, Inc.
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

#include "libguile/bdw-gc.h"
#include <gc/gc_mark.h>
#include "libguile/_scm.h"
#include "libguile/deprecation.h"

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>   /* for memset used by FD_ZERO on Solaris 10 */
#endif

#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#if HAVE_PTHREAD_NP_H
# include <pthread_np.h>
#endif

#include <sys/select.h>

#include <assert.h>
#include <fcntl.h>
#include <nproc.h>

#include "libguile/validate.h"
#include "libguile/root.h"
#include "libguile/eval.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/threads.h"
#include "libguile/dynwind.h"
#include "libguile/iselect.h"
#include "libguile/fluids.h"
#include "libguile/continuations.h"
#include "libguile/gc.h"
#include "libguile/gc-inline.h"
#include "libguile/init.h"
#include "libguile/scmsigs.h"
#include "libguile/strings.h"
#include "libguile/vm.h"

#include <full-read.h>




/* The GC "kind" for threads that allow them to mark their VM
   stacks.  */
static int thread_gc_kind;

static struct GC_ms_entry *
thread_mark (GC_word *addr, struct GC_ms_entry *mark_stack_ptr,
             struct GC_ms_entry *mark_stack_limit, GC_word env)
{
  int word;
  const struct scm_i_thread *t = (struct scm_i_thread *) addr;

  if (SCM_UNPACK (t->handle) == 0)
    /* T must be on the free-list; ignore.  (See warning in
       gc_mark.h.)  */
    return mark_stack_ptr;

  /* Mark T.  We could be more precise, but it doesn't matter.  */
  for (word = 0; word * sizeof (*addr) < sizeof (*t); word++)
    mark_stack_ptr = GC_MARK_AND_PUSH ((void *) addr[word],
				       mark_stack_ptr, mark_stack_limit,
				       NULL);

  /* The pointerless freelists are threaded through their first word,
     but GC doesn't know to trace them (as they are pointerless), so we
     need to do that here.  See the comments at the top of libgc's
     gc_inline.h.  */
  if (t->pointerless_freelists)
    {
      size_t n;
      for (n = 0; n < SCM_INLINE_GC_FREELIST_COUNT; n++)
        {
          void *chain = t->pointerless_freelists[n];
          if (chain)
            {
              /* The first link is already marked by the freelist vector,
                 so we just have to mark the tail.  */
              while ((chain = *(void **)chain))
                mark_stack_ptr = GC_mark_and_push (chain, mark_stack_ptr,
                                                   mark_stack_limit, NULL);
            }
        }
    }

  if (t->vp)
    mark_stack_ptr = scm_i_vm_mark_stack (t->vp, mark_stack_ptr,
                                          mark_stack_limit);

  return mark_stack_ptr;
}



static void
to_timespec (SCM t, scm_t_timespec *waittime)
{
  if (scm_is_pair (t))
    {
      waittime->tv_sec = scm_to_ulong (SCM_CAR (t));
      waittime->tv_nsec = scm_to_ulong (SCM_CDR (t)) * 1000;
    }
  else
    {
      double time = scm_to_double (t);
      double sec = scm_c_truncate (time);

      waittime->tv_sec = (long) sec;
      waittime->tv_nsec = (long) ((time - sec) * 1000000000);
    }
}



/*** Queues */

/* Note: We annotate with "GC-robust" assignments whose purpose is to avoid
   the risk of false references leading to unbounded retained space as
   described in "Bounding Space Usage of Conservative Garbage Collectors",
   H.J. Boehm, 2001.  */

/* Make an empty queue data structure.
 */
static SCM
make_queue ()
{
  return scm_cons (SCM_EOL, SCM_EOL);
}

static scm_i_pthread_mutex_t queue_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

/* Put T at the back of Q and return a handle that can be used with
   remqueue to remove T from Q again.
 */
static SCM
enqueue (SCM q, SCM t)
{
  SCM c = scm_cons (t, SCM_EOL);
  scm_i_pthread_mutex_lock (&queue_lock);
  if (scm_is_null (SCM_CDR (q)))
    SCM_SETCDR (q, c);
  else
    SCM_SETCDR (SCM_CAR (q), c);
  SCM_SETCAR (q, c);
  scm_i_pthread_mutex_unlock (&queue_lock);
  return c;
}

/* Remove the element that the handle C refers to from the queue Q.  C
   must have been returned from a call to enqueue.  The return value
   is zero when the element referred to by C has already been removed.
   Otherwise, 1 is returned.
*/
static int
remqueue (SCM q, SCM c)
{
  SCM p, prev = q;
  scm_i_pthread_mutex_lock (&queue_lock);
  for (p = SCM_CDR (q); !scm_is_null (p); p = SCM_CDR (p))
    {
      if (scm_is_eq (p, c))
	{
	  if (scm_is_eq (c, SCM_CAR (q)))
            SCM_SETCAR (q, scm_is_eq (prev, q) ? SCM_EOL : prev);
	  SCM_SETCDR (prev, SCM_CDR (c));

	  /* GC-robust */
	  SCM_SETCDR (c, SCM_EOL);

          scm_i_pthread_mutex_unlock (&queue_lock);
	  return 1;
	}
      prev = p;
    }
  scm_i_pthread_mutex_unlock (&queue_lock);
  return 0;
}

/* Remove the front-most element from the queue Q and return it.
   Return SCM_BOOL_F when Q is empty.
*/
static SCM
dequeue (SCM q)
{
  SCM c;
  scm_i_pthread_mutex_lock (&queue_lock);
  c = SCM_CDR (q);
  if (scm_is_null (c))
    {
      scm_i_pthread_mutex_unlock (&queue_lock);
      return SCM_BOOL_F;
    }
  else
    {
      SCM_SETCDR (q, SCM_CDR (c));
      if (scm_is_null (SCM_CDR (q)))
	SCM_SETCAR (q, SCM_EOL);
      scm_i_pthread_mutex_unlock (&queue_lock);

      /* GC-robust */
      SCM_SETCDR (c, SCM_EOL);

      return SCM_CAR (c);
    }
}

/*** Thread smob routines */


static int
thread_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  /* On a Gnu system pthread_t is an unsigned long, but on mingw it's a
     struct.  A cast like "(unsigned long) t->pthread" is a syntax error in
     the struct case, hence we go via a union, and extract according to the
     size of pthread_t.  */
  union {
    scm_i_pthread_t p;
    unsigned short us;
    unsigned int   ui;
    unsigned long  ul;
    scm_t_uintmax  um;
  } u;
  scm_i_thread *t = SCM_I_THREAD_DATA (exp);
  scm_i_pthread_t p = t->pthread;
  scm_t_uintmax id;
  u.p = p;
  if (sizeof (p) == sizeof (unsigned short))
    id = u.us;
  else if (sizeof (p) == sizeof (unsigned int))
    id = u.ui;
  else if (sizeof (p) == sizeof (unsigned long))
    id = u.ul;
  else
    id = u.um;

  scm_puts ("#<thread ", port);
  scm_uintprint (id, 10, port);
  scm_puts (" (", port);
  scm_uintprint ((scm_t_bits)t, 16, port);
  scm_puts (")>", port);
  return 1;
}


/*** Blocking on queues. */

/* See also scm_system_async_mark_for_thread for how such a block is
   interrputed.
*/

/* Put the current thread on QUEUE and go to sleep, waiting for it to
   be woken up by a call to 'unblock_from_queue', or to be
   interrupted.  Upon return of this function, the current thread is
   no longer on QUEUE, even when the sleep has been interrupted.

   The caller of block_self must hold MUTEX.  It will be atomically
   unlocked while sleeping, just as with scm_i_pthread_cond_wait.

   When WAITTIME is not NULL, the sleep will be aborted at that time.

   The return value of block_self is an errno value.  It will be zero
   when the sleep has been successfully completed by a call to
   unblock_from_queue, EINTR when it has been interrupted by the
   delivery of a system async, and ETIMEDOUT when the timeout has
   expired.

   The system asyncs themselves are not executed by block_self.
*/
static int
block_self (SCM queue, scm_i_pthread_mutex_t *mutex,
	    const scm_t_timespec *waittime)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  SCM q_handle;
  int err;

  if (scm_i_setup_sleep (t, mutex, -1))
    {
      scm_i_reset_sleep (t);
      err = EINTR;
    }
  else
    {
      t->block_asyncs++;
      q_handle = enqueue (queue, t->handle);
      if (waittime == NULL)
	err = scm_i_scm_pthread_cond_wait (&t->sleep_cond, mutex);
      else
	err = scm_i_scm_pthread_cond_timedwait (&t->sleep_cond, mutex, waittime);

      /* When we are still on QUEUE, we have been interrupted.  We
	 report this only when no other error (such as a timeout) has
	 happened above.
      */
      if (remqueue (queue, q_handle) && err == 0)
	err = EINTR;
      t->block_asyncs--;
      scm_i_reset_sleep (t);
    }

  return err;
}

/* Wake up the first thread on QUEUE, if any.  The awoken thread is
   returned, or #f if the queue was empty.
 */
static SCM
unblock_from_queue (SCM queue)
{
  SCM thread = dequeue (queue);
  if (scm_is_true (thread))
    scm_i_pthread_cond_signal (&SCM_I_THREAD_DATA(thread)->sleep_cond);
  return thread;
}


/* Getting into and out of guile mode.
 */

/* Key used to attach a cleanup handler to a given thread.  Also, if
   thread-local storage is unavailable, this key is used to retrieve the
   current thread with `pthread_getspecific ()'.  */
scm_i_pthread_key_t scm_i_thread_key;


#ifdef SCM_HAVE_THREAD_STORAGE_CLASS

/* When thread-local storage (TLS) is available, a pointer to the
   current-thread object is kept in TLS.  Note that storing the thread-object
   itself in TLS (rather than a pointer to some malloc'd memory) is not
   possible since thread objects may live longer than the actual thread they
   represent.  */
SCM_THREAD_LOCAL scm_i_thread *scm_i_current_thread = NULL;

#endif /* SCM_HAVE_THREAD_STORAGE_CLASS */


static scm_i_pthread_mutex_t thread_admin_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;
static scm_i_thread *all_threads = NULL;
static int thread_count;

static SCM scm_i_default_dynamic_state;

/* Run when a fluid is collected.  */
void
scm_i_reset_fluid (size_t n)
{
  scm_i_thread *t;

  scm_i_pthread_mutex_lock (&thread_admin_mutex);
  for (t = all_threads; t; t = t->next_thread)
    if (SCM_I_DYNAMIC_STATE_P (t->dynamic_state))
      {
        SCM v = SCM_I_DYNAMIC_STATE_FLUIDS (t->dynamic_state);
          
        if (n < SCM_SIMPLE_VECTOR_LENGTH (v))
          SCM_SIMPLE_VECTOR_SET (v, n, SCM_UNDEFINED);
      }
  scm_i_pthread_mutex_unlock (&thread_admin_mutex);
}

/* Perform first stage of thread initialisation, in non-guile mode.
 */
static void
guilify_self_1 (struct GC_stack_base *base)
{
  scm_i_thread t;

  /* We must arrange for SCM_I_CURRENT_THREAD to point to a valid value
     before allocating anything in this thread, because allocation could
     cause GC to run, and GC could cause finalizers, which could invoke
     Scheme functions, which need the current thread to be set.  */

  t.pthread = scm_i_pthread_self ();
  t.handle = SCM_BOOL_F;
  t.result = SCM_BOOL_F;
  t.join_queue = SCM_EOL;
  t.freelists = NULL;
  t.pointerless_freelists = NULL;
  t.dynamic_state = SCM_BOOL_F;
  t.dynstack.base = NULL;
  t.dynstack.top = NULL;
  t.dynstack.limit = NULL;
  t.pending_asyncs = SCM_EOL;
  t.block_asyncs = 1;
  t.base = base->mem_base;
#ifdef __ia64__
  t.register_backing_store_base = base->reg_base;
#endif
  t.continuation_root = SCM_EOL;
  t.continuation_base = t.base;
  scm_i_pthread_cond_init (&t.sleep_cond, NULL);
  t.wake = NULL;
  t.vp = NULL;

  if (pipe2 (t.sleep_pipe, O_CLOEXEC) != 0)
    /* FIXME: Error conditions during the initialization phase are handled
       gracelessly since public functions such as `scm_init_guile ()'
       currently have type `void'.  */
    abort ();

  scm_i_pthread_mutex_init (&t.admin_mutex, NULL);
  t.exited = 0;
  t.guile_mode = 0;

  /* The switcheroo.  */
  {
    scm_i_thread *t_ptr = &t;
    
    GC_disable ();
    t_ptr = GC_generic_malloc (sizeof (*t_ptr), thread_gc_kind);
    memcpy (t_ptr, &t, sizeof t);

    scm_i_pthread_setspecific (scm_i_thread_key, t_ptr);

#ifdef SCM_HAVE_THREAD_STORAGE_CLASS
    /* Cache the current thread in TLS for faster lookup.  */
    scm_i_current_thread = t_ptr;
#endif

    scm_i_pthread_mutex_lock (&thread_admin_mutex);
    t_ptr->next_thread = all_threads;
    all_threads = t_ptr;
    thread_count++;
    scm_i_pthread_mutex_unlock (&thread_admin_mutex);

    GC_enable ();
  }
}

/* Perform second stage of thread initialisation, in guile mode.
 */
static void
guilify_self_2 (SCM parent)
{
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  t->guile_mode = 1;

  SCM_NEWSMOB (t->handle, scm_tc16_thread, t);

  t->continuation_root = scm_cons (t->handle, SCM_EOL);
  t->continuation_base = t->base;

  {
    size_t size = SCM_INLINE_GC_FREELIST_COUNT * sizeof (void *);
    t->freelists = scm_gc_malloc (size, "freelists");
    t->pointerless_freelists = scm_gc_malloc (size, "atomic freelists");
  }

  if (scm_is_true (parent))
    t->dynamic_state = scm_make_dynamic_state (parent);
  else
    t->dynamic_state = scm_i_make_initial_dynamic_state ();

  t->dynstack.base = scm_gc_malloc (16 * sizeof (scm_t_bits), "dynstack");
  t->dynstack.limit = t->dynstack.base + 16;
  t->dynstack.top = t->dynstack.base + SCM_DYNSTACK_HEADER_LEN;

  t->join_queue = make_queue ();
  t->block_asyncs = 0;

  /* See note in finalizers.c:queue_finalizer_async().  */
  GC_invoke_finalizers ();
}




/* Perform thread tear-down, in guile mode.
 */
static void *
do_thread_exit (void *v)
{
  scm_i_thread *t = (scm_i_thread *) v;

  scm_i_scm_pthread_mutex_lock (&t->admin_mutex);

  t->exited = 1;
  close (t->sleep_pipe[0]);
  close (t->sleep_pipe[1]);
  while (scm_is_true (unblock_from_queue (t->join_queue)))
    ;

  scm_i_pthread_mutex_unlock (&t->admin_mutex);

  return NULL;
}

static void *
do_thread_exit_trampoline (struct GC_stack_base *sb, void *v)
{
  /* Won't hurt if we are already registered.  */
#if SCM_USE_PTHREAD_THREADS
  GC_register_my_thread (sb);
#endif

  return scm_with_guile (do_thread_exit, v);
}

static void
on_thread_exit (void *v)
{
  /* This handler is executed in non-guile mode.  */
  scm_i_thread *t = (scm_i_thread *) v, **tp;

  /* If we were canceled, we were unable to clear `t->guile_mode', so do
     it here.  */
  t->guile_mode = 0;

  /* Reinstate the current thread for purposes of scm_with_guile
     guile-mode cleanup handlers.  Only really needed in the non-TLS
     case but it doesn't hurt to be consistent.  */
  scm_i_pthread_setspecific (scm_i_thread_key, t);

  /* Scheme-level thread finalizers and other cleanup needs to happen in
     guile mode.  */
  GC_call_with_stack_base (do_thread_exit_trampoline, t);

  /* Removing ourself from the list of all threads needs to happen in
     non-guile mode since all SCM values on our stack become
     unprotected once we are no longer in the list.  */
  scm_i_pthread_mutex_lock (&thread_admin_mutex);
  for (tp = &all_threads; *tp; tp = &(*tp)->next_thread)
    if (*tp == t)
      {
	*tp = t->next_thread;

	/* GC-robust */
	t->next_thread = NULL;

	break;
      }
  thread_count--;

  /* If there's only one other thread, it could be the signal delivery
     thread, so we need to notify it to shut down by closing its read pipe.
     If it's not the signal delivery thread, then closing the read pipe isn't
     going to hurt.  */
  if (thread_count <= 1)
    scm_i_close_signal_pipe ();

  scm_i_pthread_mutex_unlock (&thread_admin_mutex);

  scm_i_pthread_setspecific (scm_i_thread_key, NULL);

  if (t->vp)
    {
      scm_i_vm_free_stack (t->vp);
      t->vp = NULL;
    }

#if SCM_USE_PTHREAD_THREADS
  GC_unregister_my_thread ();
#endif
}

static scm_i_pthread_once_t init_thread_key_once = SCM_I_PTHREAD_ONCE_INIT;

static void
init_thread_key (void)
{
  scm_i_pthread_key_create (&scm_i_thread_key, on_thread_exit);
}

/* Perform any initializations necessary to make the current thread
   known to Guile (via SCM_I_CURRENT_THREAD), initializing Guile itself,
   if necessary.

   BASE is the stack base to use with GC.

   PARENT is the dynamic state to use as the parent, ot SCM_BOOL_F in
   which case the default dynamic state is used.

   Returns zero when the thread was known to guile already; otherwise
   return 1.

   Note that it could be the case that the thread was known
   to Guile, but not in guile mode (because we are within a
   scm_without_guile call).   Check SCM_I_CURRENT_THREAD->guile_mode to
   be sure.  New threads are put into guile mode implicitly.  */

static int
scm_i_init_thread_for_guile (struct GC_stack_base *base, SCM parent)
{
  scm_i_pthread_once (&init_thread_key_once, init_thread_key);

  if (SCM_I_CURRENT_THREAD)
    {
      /* Thread is already known to Guile.
      */
      return 0;
    }
  else
    {
      /* This thread has not been guilified yet.
       */

      scm_i_pthread_mutex_lock (&scm_i_init_mutex);
      if (scm_initialized_p == 0)
	{
	  /* First thread ever to enter Guile.  Run the full
	     initialization.
	  */
	  scm_i_init_guile (base);

#if SCM_USE_PTHREAD_THREADS
          /* Allow other threads to come in later.  */
          GC_allow_register_threads ();
#endif

	  scm_i_pthread_mutex_unlock (&scm_i_init_mutex);
	}
      else
	{
	  /* Guile is already initialized, but this thread enters it for
	     the first time.  Only initialize this thread.
	  */
	  scm_i_pthread_mutex_unlock (&scm_i_init_mutex);

          /* Register this thread with libgc.  */
#if SCM_USE_PTHREAD_THREADS
          GC_register_my_thread (base);
#endif

	  guilify_self_1 (base);
	  guilify_self_2 (parent);
	}
      return 1;
    }
}

void
scm_init_guile ()
{
  struct GC_stack_base stack_base;
  
  if (GC_get_stack_base (&stack_base) == GC_SUCCESS)
    scm_i_init_thread_for_guile (&stack_base,
                                 scm_i_default_dynamic_state);
  else
    {
      fprintf (stderr, "Failed to get stack base for current thread.\n");
      exit (EXIT_FAILURE);
    }
}

struct with_guile_args
{
  GC_fn_type func;
  void *data;
  SCM parent;
};

static void *
with_guile_trampoline (void *data)
{
  struct with_guile_args *args = data;

  return scm_c_with_continuation_barrier (args->func, args->data);
}
  
static void *
with_guile_and_parent (struct GC_stack_base *base, void *data)
{
  void *res;
  int new_thread;
  scm_i_thread *t;
  struct with_guile_args *args = data;

  new_thread = scm_i_init_thread_for_guile (base, args->parent);
  t = SCM_I_CURRENT_THREAD;
  if (new_thread)
    {
      /* We are in Guile mode.  */
      assert (t->guile_mode);

      res = scm_c_with_continuation_barrier (args->func, args->data);

      /* Leave Guile mode.  */
      t->guile_mode = 0;
    }
  else if (t->guile_mode)
    {
      /* Already in Guile mode.  */
      res = scm_c_with_continuation_barrier (args->func, args->data);
    }
  else
    {
      /* We are not in Guile mode, either because we are not within a
         scm_with_guile, or because we are within a scm_without_guile.

         This call to scm_with_guile() could happen from anywhere on the
         stack, and in particular lower on the stack than when it was
         when this thread was first guilified.  Thus, `base' must be
         updated.  */
#if SCM_STACK_GROWS_UP
      if (SCM_STACK_PTR (base->mem_base) < t->base)
        t->base = SCM_STACK_PTR (base->mem_base);
#else
      if (SCM_STACK_PTR (base->mem_base) > t->base)
        t->base = SCM_STACK_PTR (base->mem_base);
#endif

      t->guile_mode = 1;
      res = GC_call_with_gc_active (with_guile_trampoline, args);
      t->guile_mode = 0;
    }
  return res;
}

static void *
scm_i_with_guile_and_parent (void *(*func)(void *), void *data, SCM parent)
{
  struct with_guile_args args;

  args.func = func;
  args.data = data;
  args.parent = parent;
  
  return GC_call_with_stack_base (with_guile_and_parent, &args);
}

void *
scm_with_guile (void *(*func)(void *), void *data)
{
  return scm_i_with_guile_and_parent (func, data,
				      scm_i_default_dynamic_state);
}

void *
scm_without_guile (void *(*func)(void *), void *data)
{
  void *result;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  if (t->guile_mode)
    {
      SCM_I_CURRENT_THREAD->guile_mode = 0;
      result = GC_do_blocking (func, data);
      SCM_I_CURRENT_THREAD->guile_mode = 1;
    }
  else
    /* Otherwise we're not in guile mode, so nothing to do.  */
    result = func (data);

  return result;
}


/*** Thread creation */

/* Because (ice-9 boot-9) loads up (ice-9 threads), we know that this
   variable will get loaded before a call to scm_call_with_new_thread
   and therefore no lock or pthread_once_t is needed. */
static SCM call_with_new_thread_var;

SCM
scm_call_with_new_thread (SCM thunk, SCM handler)
{
  SCM call_with_new_thread = scm_variable_ref (call_with_new_thread_var);
  if (SCM_UNBNDP (handler))
    return scm_call_1 (call_with_new_thread, thunk);
  return scm_call_2 (call_with_new_thread, thunk, handler);
}

typedef struct {
  SCM parent;
  SCM thunk;
} launch_data;

static void *
really_launch (void *d)
{
  SCM_I_CURRENT_THREAD->result = scm_call_0 (((launch_data *)d)->thunk);
  return 0;
}

static void *
launch_thread (void *d)
{
  launch_data *data = (launch_data *)d;
  scm_i_pthread_detach (scm_i_pthread_self ());
  scm_i_with_guile_and_parent (really_launch, d, data->parent);
  return NULL;
}

SCM_INTERNAL SCM scm_sys_call_with_new_thread (SCM);
SCM_DEFINE (scm_sys_call_with_new_thread, "%call-with-new-thread", 1, 0, 0,
	    (SCM thunk), "")
#define FUNC_NAME s_scm_sys_call_with_new_thread
{
  launch_data *data;
  scm_i_pthread_t id;
  int err;

  SCM_ASSERT (scm_is_true (scm_thunk_p (thunk)), thunk, SCM_ARG1, FUNC_NAME);

  GC_collect_a_little ();
  data = scm_gc_typed_calloc (launch_data);
  data->parent = scm_current_dynamic_state ();
  data->thunk = thunk;
  err = scm_i_pthread_create (&id, NULL, launch_thread, data);
  if (err)
    {
      errno = err;
      scm_syserror (NULL);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

typedef struct {
  SCM parent;
  scm_t_catch_body body;
  void *body_data;
  scm_t_catch_handler handler;
  void *handler_data;
  SCM thread;
  scm_i_pthread_mutex_t mutex;
  scm_i_pthread_cond_t cond;
} spawn_data;

static void *
really_spawn (void *d)
{
  spawn_data *data = (spawn_data *)d;
  scm_t_catch_body body = data->body;
  void *body_data = data->body_data;
  scm_t_catch_handler handler = data->handler;
  void *handler_data = data->handler_data;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  scm_i_scm_pthread_mutex_lock (&data->mutex);
  data->thread = scm_current_thread ();
  scm_i_pthread_cond_signal (&data->cond);
  scm_i_pthread_mutex_unlock (&data->mutex);

  if (handler == NULL)
    t->result = body (body_data);
  else
    t->result = scm_internal_catch (SCM_BOOL_T,
				    body, body_data,
				    handler, handler_data);

  return 0;
}

static void *
spawn_thread (void *d)
{
  spawn_data *data = (spawn_data *)d;
  scm_i_pthread_detach (scm_i_pthread_self ());
  scm_i_with_guile_and_parent (really_spawn, d, data->parent);
  return NULL;
}

SCM
scm_spawn_thread (scm_t_catch_body body, void *body_data,
		  scm_t_catch_handler handler, void *handler_data)
{
  spawn_data data;
  scm_i_pthread_t id;
  int err;

  data.parent = scm_current_dynamic_state ();
  data.body = body;
  data.body_data = body_data;
  data.handler = handler;
  data.handler_data = handler_data;
  data.thread = SCM_BOOL_F;
  scm_i_pthread_mutex_init (&data.mutex, NULL);
  scm_i_pthread_cond_init (&data.cond, NULL);

  scm_i_scm_pthread_mutex_lock (&data.mutex);
  err = scm_i_pthread_create (&id, NULL, spawn_thread, &data);
  if (err)
    {
      scm_i_pthread_mutex_unlock (&data.mutex);
      errno = err;
      scm_syserror (NULL);
    }

  while (scm_is_false (data.thread))
    scm_i_scm_pthread_cond_wait (&data.cond, &data.mutex);

  scm_i_pthread_mutex_unlock (&data.mutex);

  assert (SCM_I_IS_THREAD (data.thread));

  return data.thread;
}

SCM_DEFINE (scm_yield, "yield", 0, 0, 0,
	    (),
"Move the calling thread to the end of the scheduling queue.")
#define FUNC_NAME s_scm_yield
{
  return scm_from_bool (scm_i_sched_yield ());
}
#undef FUNC_NAME

/* Some systems, notably Android, lack 'pthread_cancel'.  Don't provide
   'cancel-thread' on these systems.  */

static SCM cancel_thread_var;

SCM
scm_cancel_thread (SCM thread)
{
  scm_call_1 (scm_variable_ref (cancel_thread_var), thread);
  return SCM_UNSPECIFIED;
}

SCM
scm_join_thread (SCM thread)
{
  return scm_join_thread_timed (thread, SCM_UNDEFINED, SCM_UNDEFINED);
}

SCM_DEFINE (scm_join_thread_timed, "join-thread", 1, 2, 0,
	    (SCM thread, SCM timeout, SCM timeoutval),
"Suspend execution of the calling thread until the target @var{thread} "
"terminates, unless the target @var{thread} has already terminated. ")
#define FUNC_NAME s_scm_join_thread_timed
{
  scm_i_thread *t;
  scm_t_timespec ctimeout, *timeout_ptr = NULL;
  SCM res = SCM_BOOL_F;

  if (! (SCM_UNBNDP (timeoutval)))
    res = timeoutval;

  SCM_VALIDATE_THREAD (1, thread);
  if (scm_is_eq (scm_current_thread (), thread))
    SCM_MISC_ERROR ("cannot join the current thread", SCM_EOL);

  t = SCM_I_THREAD_DATA (thread);
  scm_i_scm_pthread_mutex_lock (&t->admin_mutex);

  if (! SCM_UNBNDP (timeout))
    {
      to_timespec (timeout, &ctimeout);
      timeout_ptr = &ctimeout;
    }

  if (t->exited)
    res = t->result;
  else
    {
      while (1)
	{
	  int err = block_self (t->join_queue, &t->admin_mutex,
				timeout_ptr);
          scm_remember_upto_here_1 (thread);
	  if (err == 0)
	    {
	      if (t->exited)
		{
		  res = t->result;
		  break;
		}
	    }
	  else if (err == ETIMEDOUT)
	    break;

	  scm_i_pthread_mutex_unlock (&t->admin_mutex);
	  SCM_TICK;
	  scm_i_scm_pthread_mutex_lock (&t->admin_mutex);

	  /* Check for exit again, since we just released and
	     reacquired the admin mutex, before the next block_self
	     call (which would block forever if t has already
	     exited). */
	  if (t->exited)
	    {
	      res = t->result;
	      break;
	    }
	}
    }

  scm_i_pthread_mutex_unlock (&t->admin_mutex);

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_thread_p, "thread?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a thread.")
#define FUNC_NAME s_scm_thread_p
{
  return SCM_I_IS_THREAD(obj) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME




/* We implement our own mutex type since we want them to be 'fair', we
   want to do fancy things while waiting for them (like running
   asyncs) and we might want to add things that are nice for
   debugging.
*/

enum scm_mutex_kind {
  /* A standard mutex can only be locked once.  If you try to lock it
     again from the thread that locked it to begin with (the "owner"
     thread), it throws an error.  It can only be unlocked from the
     thread that locked it in the first place.  */
  SCM_MUTEX_STANDARD,
  /* A recursive mutex can be locked multiple times by its owner.  It
     then has to be unlocked the corresponding number of times, and like
     standard mutexes can only be unlocked by the owner thread.  */
  SCM_MUTEX_RECURSIVE,
  /* An unowned mutex is like a standard mutex, except that it can be
     unlocked by any thread.  A corrolary of this behavior is that a
     thread's attempt to lock a mutex that it already owns will block
     instead of signalling an error, as it could be that some other
     thread unlocks the mutex, allowing the owner thread to proceed.
     This kind of mutex is a bit strange and is here for use by
     SRFI-18.  */
  SCM_MUTEX_UNOWNED
};

struct scm_mutex {
  scm_i_pthread_mutex_t lock;
  SCM owner;
  int level; /* how much the owner owns us.  <= 1 for non-recursive mutexes */
  SCM waiting;    /* the threads waiting for this mutex. */
};

#define SCM_MUTEXP(x)     SCM_SMOB_PREDICATE (scm_tc16_mutex, x)
#define SCM_MUTEX_DATA(x) ((struct scm_mutex *) SCM_SMOB_DATA (x))
#define SCM_MUTEX_KIND(x) ((enum scm_mutex_kind) (SCM_SMOB_FLAGS (x) & 0x3))

static int
scm_mutex_print (SCM mx, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  struct scm_mutex *m = SCM_MUTEX_DATA (mx);
  scm_puts ("#<mutex ", port);
  scm_uintprint ((scm_t_bits)m, 16, port);
  scm_puts (">", port);
  return 1;
}

SCM_SYMBOL (allow_external_unlock_sym, "allow-external-unlock");
SCM_SYMBOL (recursive_sym, "recursive");

SCM_DEFINE (scm_make_mutex_with_kind, "make-mutex", 0, 1, 0,
	    (SCM kind),
	    "Create a new mutex.  If @var{kind} is not given, the mutex\n"
            "will be a standard non-recursive mutex.  Otherwise pass\n"
            "@code{recursive} to make a recursive mutex, or\n"
            "@code{allow-external-unlock} to make a non-recursive mutex\n"
            "that can be unlocked from any thread.")
#define FUNC_NAME s_scm_make_mutex_with_kind
{
  enum scm_mutex_kind mkind = SCM_MUTEX_STANDARD;
  struct scm_mutex *m;
  scm_i_pthread_mutex_t lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;

  if (!SCM_UNBNDP (kind))
    {
      if (scm_is_eq (kind, allow_external_unlock_sym))
	mkind = SCM_MUTEX_UNOWNED;
      else if (scm_is_eq (kind, recursive_sym))
	mkind = SCM_MUTEX_RECURSIVE;
      else
	SCM_MISC_ERROR ("unsupported mutex kind: ~a", scm_list_1 (kind));
    }

  m = scm_gc_malloc (sizeof (struct scm_mutex), "mutex");
  /* Because PTHREAD_MUTEX_INITIALIZER is static, it's plain old data,
     and so we can just copy it.  */
  memcpy (&m->lock, &lock, sizeof (m->lock));
  m->owner = SCM_BOOL_F;
  m->level = 0;
  m->waiting = make_queue ();

  return scm_new_smob (scm_tc16_mutex | (mkind << 16), (scm_t_bits) m);
}
#undef FUNC_NAME

SCM
scm_make_mutex (void)
{
  return scm_make_mutex_with_kind (SCM_UNDEFINED);
}

SCM_DEFINE (scm_make_recursive_mutex, "make-recursive-mutex", 0, 0, 0,
	    (void),
	    "Create a new recursive mutex. ")
#define FUNC_NAME s_scm_make_recursive_mutex
{
  return scm_make_mutex_with_kind (recursive_sym);
}
#undef FUNC_NAME

SCM
scm_lock_mutex (SCM mx)
{
  return scm_timed_lock_mutex (mx, SCM_UNDEFINED);
}

SCM_DEFINE (scm_timed_lock_mutex, "lock-mutex", 1, 1, 0,
	    (SCM mutex, SCM timeout),
	    "Lock mutex @var{mutex}. If the mutex is already locked, "
            "the calling thread blocks until the mutex becomes available.")
#define FUNC_NAME s_scm_timed_lock_mutex
{
  scm_t_timespec cwaittime, *waittime = NULL;
  struct timeval current_time;
  struct scm_mutex *m;
  SCM new_owner = scm_current_thread();

  SCM_VALIDATE_MUTEX (1, mutex);
  m = SCM_MUTEX_DATA (mutex);

  if (! SCM_UNBNDP (timeout) && ! scm_is_false (timeout))
    {
      to_timespec (timeout, &cwaittime);
      waittime = &cwaittime;
    }

  scm_i_scm_pthread_mutex_lock (&m->lock);

  while (1)
    {
      if (m->level == 0)
	{
	  m->owner = new_owner;
	  m->level++;
          scm_i_pthread_mutex_unlock (&m->lock);
          return SCM_BOOL_T;
	}
      else if (scm_is_eq (m->owner, new_owner) &&
               SCM_MUTEX_KIND (mutex) != SCM_MUTEX_UNOWNED)
	{
	  if (SCM_MUTEX_KIND (mutex) == SCM_MUTEX_RECURSIVE)
	    {
	      m->level++;
              scm_i_pthread_mutex_unlock (&m->lock);
              return SCM_BOOL_T;
	    }
	  else
	    {
              scm_i_pthread_mutex_unlock (&m->lock);
	      SCM_MISC_ERROR ("mutex already locked by thread", SCM_EOL);
	    }
	}
      else
	{
	  if (waittime != NULL)
	    {
	      gettimeofday (&current_time, NULL);
	      if (current_time.tv_sec > waittime->tv_sec ||
		  (current_time.tv_sec == waittime->tv_sec &&
		   current_time.tv_usec * 1000 > waittime->tv_nsec))
		{
                  scm_i_pthread_mutex_unlock (&m->lock);
                  return SCM_BOOL_F;
		}
	    }
          block_self (m->waiting, &m->lock, waittime);
          scm_remember_upto_here_1 (mutex);
	  scm_i_pthread_mutex_unlock (&m->lock);
	  SCM_TICK;
	  scm_i_scm_pthread_mutex_lock (&m->lock);
	}
    }
}
#undef FUNC_NAME

static void
lock_mutex_return_void (SCM mx)
{
  (void) scm_lock_mutex (mx);
}

static void
unlock_mutex_return_void (SCM mx)
{
  (void) scm_unlock_mutex (mx);
}

void
scm_dynwind_lock_mutex (SCM mutex)
{
  scm_dynwind_unwind_handler_with_scm (unlock_mutex_return_void, mutex,
				       SCM_F_WIND_EXPLICITLY);
  scm_dynwind_rewind_handler_with_scm (lock_mutex_return_void, mutex,
				       SCM_F_WIND_EXPLICITLY);
}

SCM
scm_try_mutex (SCM mutex)
{
  return scm_timed_lock_mutex (mutex, SCM_INUM0);
}

SCM_DEFINE (scm_unlock_mutex, "unlock-mutex", 1, 0, 0, (SCM mutex),
            "Unlocks @var{mutex}.  The calling thread must already hold\n"
            "the lock on @var{mutex}, unless the mutex was created with\n"
            "the @code{allow-external-unlock} option; otherwise an error\n"
            "will be signalled.")
#define FUNC_NAME s_scm_unlock_mutex
{
  struct scm_mutex *m;

  SCM_VALIDATE_MUTEX (1, mutex);

  m = SCM_MUTEX_DATA (mutex);

  scm_i_scm_pthread_mutex_lock (&m->lock);

  if (!scm_is_eq (m->owner, SCM_I_CURRENT_THREAD->handle))
    {
      if (m->level == 0)
	{
          scm_i_pthread_mutex_unlock (&m->lock);
          SCM_MISC_ERROR ("mutex not locked", SCM_EOL);
	}
      else if (SCM_MUTEX_KIND (mutex) != SCM_MUTEX_UNOWNED)
	{
	  scm_i_pthread_mutex_unlock (&m->lock);
	  SCM_MISC_ERROR ("mutex not locked by current thread", SCM_EOL);
	}
    }

  if (m->level > 0)
    m->level--;
  if (m->level == 0)
    /* Wake up one waiter.  */
    {
      m->owner = SCM_BOOL_F;
      unblock_from_queue (m->waiting);
    }

  scm_i_pthread_mutex_unlock (&m->lock);

  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_mutex_p, "mutex?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a mutex.")
#define FUNC_NAME s_scm_mutex_p
{
  return SCM_MUTEXP (obj) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_mutex_owner, "mutex-owner", 1, 0, 0,
	    (SCM mx),
	    "Return the thread owning @var{mx}, or @code{#f}.")
#define FUNC_NAME s_scm_mutex_owner
{
  SCM owner;
  struct scm_mutex *m = NULL;

  SCM_VALIDATE_MUTEX (1, mx);
  m = SCM_MUTEX_DATA (mx);
  scm_i_pthread_mutex_lock (&m->lock);
  owner = m->owner;
  scm_i_pthread_mutex_unlock (&m->lock);

  return owner;
}
#undef FUNC_NAME

SCM_DEFINE (scm_mutex_level, "mutex-level", 1, 0, 0,
	    (SCM mx),
	    "Return the lock level of mutex @var{mx}.")
#define FUNC_NAME s_scm_mutex_level
{
  SCM_VALIDATE_MUTEX (1, mx);
  return scm_from_int (SCM_MUTEX_DATA(mx)->level);
}
#undef FUNC_NAME

SCM_DEFINE (scm_mutex_locked_p, "mutex-locked?", 1, 0, 0,
	    (SCM mx),
	    "Returns @code{#t} if the mutex @var{mx} is locked.")
#define FUNC_NAME s_scm_mutex_locked_p
{
  SCM_VALIDATE_MUTEX (1, mx);
  return SCM_MUTEX_DATA (mx)->level > 0 ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME




struct scm_cond {
  scm_i_pthread_mutex_t lock;
  SCM waiting;               /* the threads waiting for this condition. */
};

#define SCM_CONDVARP(x)       SCM_SMOB_PREDICATE (scm_tc16_condvar, x)
#define SCM_CONDVAR_DATA(x)   ((struct scm_cond *) SCM_SMOB_DATA (x))

static int
scm_cond_print (SCM cv, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  struct scm_cond *c = SCM_CONDVAR_DATA (cv);
  scm_puts ("#<condition-variable ", port);
  scm_uintprint ((scm_t_bits)c, 16, port);
  scm_puts (">", port);
  return 1;
}

SCM_DEFINE (scm_make_condition_variable, "make-condition-variable", 0, 0, 0,
	    (void),
	    "Make a new condition variable.")
#define FUNC_NAME s_scm_make_condition_variable
{
  struct scm_cond *c;
  SCM cv;

  c = scm_gc_malloc (sizeof (struct scm_cond), "condition variable");
  c->waiting = SCM_EOL;
  SCM_NEWSMOB (cv, scm_tc16_condvar, (scm_t_bits) c);
  c->waiting = make_queue ();
  return cv;
}
#undef FUNC_NAME

SCM_DEFINE (scm_timed_wait_condition_variable, "wait-condition-variable", 2, 1, 0,
	    (SCM cond, SCM mutex, SCM timeout),
"Wait until condition variable @var{cv} has been signalled.  While waiting, "
"mutex @var{mx} is atomically unlocked (as with @code{unlock-mutex}) and "
"is locked again when this function returns.  When @var{t} is given, "
"it specifies a point in time where the waiting should be aborted.  It "
"can be either a integer as returned by @code{current-time} or a pair "
"as returned by @code{gettimeofday}.  When the waiting is aborted the "
"mutex is locked and @code{#f} is returned.  When the condition "
"variable is in fact signalled, the mutex is also locked and @code{#t} "
"is returned. ")
#define FUNC_NAME s_scm_timed_wait_condition_variable
{
  scm_t_timespec waittime_val, *waittime = NULL;
  struct scm_cond *c;
  struct scm_mutex *m;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;

  SCM_VALIDATE_CONDVAR (1, cond);
  SCM_VALIDATE_MUTEX (2, mutex);

  c = SCM_CONDVAR_DATA (cond);
  m = SCM_MUTEX_DATA (mutex);

  if (!SCM_UNBNDP (timeout))
    {
      to_timespec (timeout, &waittime_val);
      waittime = &waittime_val;
    }

  scm_i_scm_pthread_mutex_lock (&m->lock);

  if (!scm_is_eq (m->owner, t->handle))
    {
      if (m->level == 0)
	{
          scm_i_pthread_mutex_unlock (&m->lock);
          SCM_MISC_ERROR ("mutex not locked", SCM_EOL);
	}
      else if (SCM_MUTEX_KIND (mutex) != SCM_MUTEX_UNOWNED)
	{
	  scm_i_pthread_mutex_unlock (&m->lock);
	  SCM_MISC_ERROR ("mutex not locked by current thread", SCM_EOL);
	}
    }

  while (1)
    {
      int err = 0;

      if (m->level > 0)
        m->level--;
      if (m->level == 0)
        {
          m->owner = SCM_BOOL_F;
          /* Wake up one waiter.  */
          unblock_from_queue (m->waiting);
        }

      t->block_asyncs++;

      err = block_self (c->waiting, &m->lock, waittime);
      scm_i_pthread_mutex_unlock (&m->lock);

      if (err == 0)
        {
          scm_lock_mutex (mutex);
          t->block_asyncs--;
          return SCM_BOOL_T;
        }
      else if (err == ETIMEDOUT)
        {
          scm_lock_mutex (mutex);
          t->block_asyncs--;
          return SCM_BOOL_F;
        }
      else if (err != EINTR)
        {
          errno = err;
          /* FIXME: missing t->block_asyncs--; ??? */
          SCM_SYSERROR;
        }

      t->block_asyncs--;
      scm_async_tick ();

      scm_remember_upto_here_2 (cond, mutex);

      scm_i_scm_pthread_mutex_lock (&m->lock);
    }
}
#undef FUNC_NAME

SCM_DEFINE (scm_signal_condition_variable, "signal-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up one thread that is waiting for @var{cv}")
#define FUNC_NAME s_scm_signal_condition_variable
{
  struct scm_cond *c;
  SCM_VALIDATE_CONDVAR (1, cv);
  c = SCM_CONDVAR_DATA (cv);
  unblock_from_queue (c->waiting);
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_broadcast_condition_variable, "broadcast-condition-variable", 1, 0, 0,
	    (SCM cv),
	    "Wake up all threads that are waiting for @var{cv}. ")
#define FUNC_NAME s_scm_broadcast_condition_variable
{
  struct scm_cond *c;
  SCM_VALIDATE_CONDVAR (1, cv);
  c = SCM_CONDVAR_DATA (cv);
  while (scm_is_true (unblock_from_queue (c->waiting)))
    ;
  return SCM_BOOL_T;
}
#undef FUNC_NAME

SCM_DEFINE (scm_condition_variable_p, "condition-variable?", 1, 0, 0,
	    (SCM obj),
	    "Return @code{#t} if @var{obj} is a condition variable.")
#define FUNC_NAME s_scm_condition_variable_p
{
  return SCM_CONDVARP(obj) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME



/*** Select */

struct select_args
{
  int             nfds;
  fd_set         *read_fds;
  fd_set         *write_fds;
  fd_set         *except_fds;
  struct timeval *timeout;

  int             result;
  int             errno_value;
};

static void *
do_std_select (void *args)
{
  struct select_args *select_args;

  select_args = (struct select_args *) args;

  select_args->result =
    select (select_args->nfds,
	    select_args->read_fds, select_args->write_fds,
	    select_args->except_fds, select_args->timeout);
  select_args->errno_value = errno;

  return NULL;
}

int
scm_std_select (int nfds,
		fd_set *readfds,
		fd_set *writefds,
		fd_set *exceptfds,
		struct timeval *timeout)
{
  fd_set my_readfds;
  int res, eno, wakeup_fd;
  scm_i_thread *t = SCM_I_CURRENT_THREAD;
  struct select_args args;

  if (readfds == NULL)
    {
      FD_ZERO (&my_readfds);
      readfds = &my_readfds;
    }

  while (scm_i_setup_sleep (t, NULL, t->sleep_pipe[1]))
    {
      scm_i_reset_sleep (t);
      SCM_TICK;
    }

  wakeup_fd = t->sleep_pipe[0];
  FD_SET (wakeup_fd, readfds);
  if (wakeup_fd >= nfds)
    nfds = wakeup_fd+1;

  args.nfds = nfds;
  args.read_fds = readfds;
  args.write_fds = writefds;
  args.except_fds = exceptfds;
  args.timeout = timeout;

  /* Explicitly cooperate with the GC.  */
  scm_without_guile (do_std_select, &args);

  res = args.result;
  eno = args.errno_value;

  scm_i_reset_sleep (t);

  if (res > 0 && FD_ISSET (wakeup_fd, readfds))
    {
      char dummy;
      full_read (wakeup_fd, &dummy, 1);

      FD_CLR (wakeup_fd, readfds);
      res -= 1;
      if (res == 0)
	{
	  eno = EINTR;
	  res = -1;
	}
    }
  errno = eno;
  return res;
}

/* Convenience API for blocking while in guile mode. */

#if SCM_USE_PTHREAD_THREADS

/* It seems reasonable to not run procedures related to mutex and condition
   variables within `GC_do_blocking ()' since, (i) the GC can operate even
   without it, and (ii) the only potential gain would be GC latency.  See
   http://thread.gmane.org/gmane.comp.programming.garbage-collection.boehmgc/2245/focus=2251
   for a discussion of the pros and cons.  */

int
scm_pthread_mutex_lock (scm_i_pthread_mutex_t *mutex)
{
  int res = scm_i_pthread_mutex_lock (mutex);
  return res;
}

static void
do_unlock (void *data)
{
  scm_i_pthread_mutex_unlock ((scm_i_pthread_mutex_t *)data);
}

void
scm_dynwind_pthread_mutex_lock (scm_i_pthread_mutex_t *mutex)
{
  scm_i_scm_pthread_mutex_lock (mutex);
  scm_dynwind_unwind_handler (do_unlock, mutex, SCM_F_WIND_EXPLICITLY);
}

int
scm_pthread_cond_wait (scm_i_pthread_cond_t *cond, scm_i_pthread_mutex_t *mutex)
{
  return scm_i_pthread_cond_wait (cond, mutex);
}

int
scm_pthread_cond_timedwait (scm_i_pthread_cond_t *cond,
			    scm_i_pthread_mutex_t *mutex,
			    const scm_t_timespec *wt)
{
  return scm_i_pthread_cond_timedwait (cond, mutex, wt);
}

#endif

static void
do_unlock_with_asyncs (void *data)
{
  scm_i_pthread_mutex_unlock ((scm_i_pthread_mutex_t *)data);
  SCM_I_CURRENT_THREAD->block_asyncs--;
}

void
scm_i_dynwind_pthread_mutex_lock_block_asyncs (scm_i_pthread_mutex_t *mutex)
{
  SCM_I_CURRENT_THREAD->block_asyncs++;
  scm_i_scm_pthread_mutex_lock (mutex);
  scm_dynwind_unwind_handler (do_unlock_with_asyncs, mutex,
                              SCM_F_WIND_EXPLICITLY);
}

unsigned long
scm_std_usleep (unsigned long usecs)
{
  struct timeval tv;
  tv.tv_usec = usecs % 1000000;
  tv.tv_sec = usecs / 1000000;
  scm_std_select (0, NULL, NULL, NULL, &tv);
  return tv.tv_sec * 1000000 + tv.tv_usec;
}

unsigned int
scm_std_sleep (unsigned int secs)
{
  struct timeval tv;
  tv.tv_usec = 0;
  tv.tv_sec = secs;
  scm_std_select (0, NULL, NULL, NULL, &tv);
  return tv.tv_sec;
}

/*** Misc */

SCM_DEFINE (scm_current_thread, "current-thread", 0, 0, 0,
	    (void),
	    "Return the thread that called this function.")
#define FUNC_NAME s_scm_current_thread
{
  return SCM_I_CURRENT_THREAD->handle;
}
#undef FUNC_NAME

static SCM
scm_c_make_list (size_t n, SCM fill)
{
  SCM res = SCM_EOL;
  while (n-- > 0)
    res = scm_cons (fill, res);
  return res;
}

SCM_DEFINE (scm_all_threads, "all-threads", 0, 0, 0,
	    (void),
	    "Return a list of all threads.")
#define FUNC_NAME s_scm_all_threads
{
  /* We can not allocate while holding the thread_admin_mutex because
     of the way GC is done.
  */
  int n = thread_count;
  scm_i_thread *t;
  SCM list = scm_c_make_list (n, SCM_UNSPECIFIED), *l;

  scm_i_pthread_mutex_lock (&thread_admin_mutex);
  l = &list;
  for (t = all_threads; t && n > 0; t = t->next_thread)
    {
      if (t != scm_i_signal_delivery_thread)
	{
	  SCM_SETCAR (*l, t->handle);
	  l = SCM_CDRLOC (*l);
	}
      n--;
    }
  *l = SCM_EOL;
  scm_i_pthread_mutex_unlock (&thread_admin_mutex);
  return list;
}
#undef FUNC_NAME

SCM_DEFINE (scm_thread_exited_p, "thread-exited?", 1, 0, 0,
	    (SCM thread),
	    "Return @code{#t} iff @var{thread} has exited.\n")
#define FUNC_NAME s_scm_thread_exited_p
{
  return scm_from_bool (scm_c_thread_exited_p (thread));
}
#undef FUNC_NAME

int
scm_c_thread_exited_p (SCM thread)
#define FUNC_NAME  s_scm_thread_exited_p
{
  scm_i_thread *t;
  SCM_VALIDATE_THREAD (1, thread);
  t = SCM_I_THREAD_DATA (thread);
  return t->exited;
}
#undef FUNC_NAME

SCM_DEFINE (scm_total_processor_count, "total-processor-count", 0, 0, 0,
	    (void),
	    "Return the total number of processors of the machine, which\n"
	    "is guaranteed to be at least 1.  A ``processor'' here is a\n"
	    "thread execution unit, which can be either:\n\n"
	    "@itemize\n"
	    "@item an execution core in a (possibly multi-core) chip, in a\n"
	    "  (possibly multi- chip) module, in a single computer, or\n"
	    "@item a thread execution unit inside a core in the case of\n"
	    "  @dfn{hyper-threaded} CPUs.\n"
	    "@end itemize\n\n"
	    "Which of the two definitions is used, is unspecified.\n")
#define FUNC_NAME s_scm_total_processor_count
{
  return scm_from_ulong (num_processors (NPROC_ALL));
}
#undef FUNC_NAME

SCM_DEFINE (scm_current_processor_count, "current-processor-count", 0, 0, 0,
	    (void),
	    "Like @code{total-processor-count}, but return the number of\n"
	    "processors available to the current process.  See\n"
	    "@code{setaffinity} and @code{getaffinity} for more\n"
	    "information.\n")
#define FUNC_NAME s_scm_current_processor_count
{
  return scm_from_ulong (num_processors (NPROC_CURRENT));
}
#undef FUNC_NAME




static scm_i_pthread_cond_t wake_up_cond;
static int threads_initialized_p = 0;


/*** Initialization */

scm_i_pthread_mutex_t scm_i_misc_mutex;

#if SCM_USE_PTHREAD_THREADS
pthread_mutexattr_t scm_i_pthread_mutexattr_recursive[1];
#endif

void
scm_threads_prehistory (void *base)
{
#if SCM_USE_PTHREAD_THREADS
  pthread_mutexattr_init (scm_i_pthread_mutexattr_recursive);
  pthread_mutexattr_settype (scm_i_pthread_mutexattr_recursive,
			     PTHREAD_MUTEX_RECURSIVE);
#endif

  scm_i_pthread_mutex_init (&scm_i_misc_mutex, NULL);
  scm_i_pthread_cond_init (&wake_up_cond, NULL);

  thread_gc_kind =
    GC_new_kind (GC_new_free_list (),
		 GC_MAKE_PROC (GC_new_proc (thread_mark), 0),
		 0, 1);

  guilify_self_1 ((struct GC_stack_base *) base);
}

scm_t_bits scm_tc16_thread;
scm_t_bits scm_tc16_mutex;
scm_t_bits scm_tc16_condvar;

static void
scm_init_ice_9_threads (void *unused)
{
#include "libguile/threads.x"

  cancel_thread_var =
    scm_module_variable (scm_current_module (),
                         scm_from_latin1_symbol ("cancel-thread"));
  call_with_new_thread_var =
    scm_module_variable (scm_current_module (),
                         scm_from_latin1_symbol ("call-with-new-thread"));
}

void
scm_init_threads ()
{
  scm_tc16_thread = scm_make_smob_type ("thread", sizeof (scm_i_thread));
  scm_set_smob_print (scm_tc16_thread, thread_print);

  scm_tc16_mutex = scm_make_smob_type ("mutex", sizeof (struct scm_mutex));
  scm_set_smob_print (scm_tc16_mutex, scm_mutex_print);

  scm_tc16_condvar = scm_make_smob_type ("condition-variable",
					 sizeof (struct scm_cond));
  scm_set_smob_print (scm_tc16_condvar, scm_cond_print);

  scm_i_default_dynamic_state = SCM_BOOL_F;
  guilify_self_2 (SCM_BOOL_F);
  threads_initialized_p = 1;

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_ice_9_threads",
                            scm_init_ice_9_threads, NULL);
}

void
scm_init_threads_default_dynamic_state ()
{
  SCM state = scm_make_dynamic_state (scm_current_dynamic_state ());
  scm_i_default_dynamic_state = state;
}


/* IA64-specific things.  */

#ifdef __ia64__
# ifdef __hpux
#  include <sys/param.h>
#  include <sys/pstat.h>
void *
scm_ia64_register_backing_store_base (void)
{
  struct pst_vm_status vm_status;
  int i = 0;
  while (pstat_getprocvm (&vm_status, sizeof (vm_status), 0, i++) == 1)
    if (vm_status.pst_type == PS_RSESTACK)
      return (void *) vm_status.pst_vaddr;
  abort ();
}
void *
scm_ia64_ar_bsp (const void *ctx)
{
  uint64_t bsp;
  __uc_get_ar_bsp (ctx, &bsp);
  return (void *) bsp;
}
# endif /* hpux */
# ifdef linux
#  include <ucontext.h>
void *
scm_ia64_register_backing_store_base (void)
{
  extern void *__libc_ia64_register_backing_store_base;
  return __libc_ia64_register_backing_store_base;
}
void *
scm_ia64_ar_bsp (const void *opaque)
{
  const ucontext_t *ctx = opaque;
  return (void *) ctx->uc_mcontext.sc_ar_bsp;
}
# endif /* linux */
# ifdef __FreeBSD__
#  include <ucontext.h>
void *
scm_ia64_register_backing_store_base (void)
{
  return (void *)0x8000000000000000;
}
void *
scm_ia64_ar_bsp (const void *opaque)
{
  const ucontext_t *ctx = opaque;
  return (void *)(ctx->uc_mcontext.mc_special.bspstore
                  + ctx->uc_mcontext.mc_special.ndirty);
}
# endif /* __FreeBSD__ */
#endif /* __ia64__ */


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
