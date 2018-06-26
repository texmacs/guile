#ifndef SCM_ASYNC_H
#define SCM_ASYNC_H

/* Copyright 1995-1998,2000-2002,2004-2006,2008-2009,2011,2014,2018
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



#include "libguile/threads.h"



#define SCM_TICK scm_async_tick ()



SCM_API void scm_async_tick (void);
SCM_API void scm_switch (void);
SCM_API SCM scm_system_async_mark (SCM a);
SCM_API SCM scm_system_async_mark_for_thread (SCM a, SCM thread);

SCM_API int scm_c_prepare_to_wait_on_fd (int fd);
SCM_API int scm_c_prepare_to_wait_on_cond (scm_i_pthread_mutex_t *m,
                                           scm_i_pthread_cond_t *c);
SCM_API void scm_c_wait_finished (void);

SCM_API SCM scm_noop (SCM args);
SCM_API SCM scm_call_with_blocked_asyncs (SCM proc);
SCM_API SCM scm_call_with_unblocked_asyncs (SCM proc);
SCM_API void *scm_c_call_with_blocked_asyncs (void *(*p) (void *d), void *d);
SCM_API void *scm_c_call_with_unblocked_asyncs (void *(*p) (void *d), void *d);
SCM_API void scm_dynwind_block_asyncs (void);
SCM_API void scm_dynwind_unblock_asyncs (void);

SCM_INTERNAL int scm_i_prepare_to_wait (scm_thread *,
                                        struct scm_thread_wake_data *);
SCM_INTERNAL void scm_i_wait_finished (scm_thread *);
SCM_INTERNAL int scm_i_prepare_to_wait_on_fd (scm_thread *, int);
SCM_INTERNAL int scm_i_prepare_to_wait_on_cond (scm_thread *,
                                                scm_i_pthread_mutex_t *,
                                                scm_i_pthread_cond_t *);

SCM_INTERNAL void scm_i_async_push (scm_thread *t, SCM proc);
SCM_INTERNAL SCM scm_i_async_pop (scm_thread *t);

SCM_INTERNAL void scm_init_async (void);

#endif  /* SCM_ASYNC_H */
