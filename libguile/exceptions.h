#ifndef SCM_EXCEPTIONS_H
#define SCM_EXCEPTIONS_H

/* Copyright 1995-1996,1998,2000,2006,2008,2010,2014,2017-2019
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



typedef SCM (*scm_t_thunk) (void *data);
typedef SCM (*scm_t_exception_handler) (void *data, SCM exn);

SCM_INTERNAL SCM scm_c_make_thunk (scm_t_thunk body,
                                   void *body_data);
SCM_INTERNAL SCM scm_c_make_exception_handler (scm_t_exception_handler h,
                                               void *handler_data);

SCM_INTERNAL SCM scm_c_with_exception_handler (SCM type,
                                               scm_t_exception_handler handler,
                                               void *handler_data,
                                               scm_t_thunk thunk,
                                               void *thunk_data);

SCM_INTERNAL SCM scm_c_with_default_exception_handler (scm_t_thunk thunk,
                                                       void *data);

SCM_INTERNAL SCM scm_with_exception_handler (SCM type, SCM handler, SCM thunk);
SCM_INTERNAL SCM scm_with_pre_unwind_exception_handler (SCM handler, SCM thunk);
SCM_INTERNAL SCM scm_raise_exception (SCM exn) SCM_NORETURN;

SCM_INTERNAL SCM scm_exception_kind (SCM exn);
SCM_INTERNAL SCM scm_exception_args (SCM exn);

SCM_INTERNAL void scm_dynwind_throw_handler (void);

/* This raises a `stack-overflow' exception, without running pre-unwind
   handlers.  */
SCM_API void scm_report_stack_overflow (void);

/* This raises an `out-of-memory' exception, without running pre-unwind
   handlers.  */
SCM_API void scm_report_out_of_memory (void);

SCM_INTERNAL void scm_init_exceptions (void);

#endif  /* SCM_EXCEPTIONS_H */
