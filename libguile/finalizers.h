#ifndef SCM_FINALIZERS_H
#define SCM_FINALIZERS_H

/* Copyright 2012, 2013, 2014, 2018
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



#include "libguile/scm.h"



typedef void (*scm_t_finalizer_proc) (void *obj, void *data);

SCM_INTERNAL void scm_i_set_finalizer (void *obj, scm_t_finalizer_proc,
                                       void *data);
SCM_INTERNAL void scm_i_add_finalizer (void *obj, scm_t_finalizer_proc,
                                       void *data);
SCM_INTERNAL void scm_i_add_resuscitator (void *obj, scm_t_finalizer_proc,
                                          void *data);

SCM_INTERNAL void scm_i_finalizer_pre_fork (void);

/* CALLBACK will be called after each garbage collection.  It will be
   called from a finalizer, which may be from an async or from another
   thread. */
SCM_INTERNAL void scm_i_register_async_gc_callback (void (*callback) (void));

SCM_API int scm_set_automatic_finalization_enabled (int enabled_p);
SCM_API int scm_run_finalizers (void);

SCM_INTERNAL void scm_init_finalizers (void);
SCM_INTERNAL void scm_init_finalizer_thread (void);

#endif  /* SCM_FINALIZERS_H */
