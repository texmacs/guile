#ifndef SCM_PROCS_H
#define SCM_PROCS_H

/* Copyright 1995-1996,1998-2001,2006,2008-2009,2012-2013,2018
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



#include "libguile/boolean.h"
#include <libguile/error.h>



#define SCM_VALIDATE_THUNK(pos, thunk) \
  do { \
    SCM_ASSERT (scm_is_true (scm_thunk_p (thunk)), thunk, pos, FUNC_NAME); \
  } while (0)

#define SCM_VALIDATE_PROC(pos, proc) \
  do { \
    SCM_ASSERT (scm_is_true (scm_procedure_p (proc)), proc, pos, FUNC_NAME); \
  } while (0)

SCM_API SCM scm_procedure_p (SCM obj);
SCM_API SCM scm_thunk_p (SCM obj);
SCM_API SCM scm_procedure_with_setter_p (SCM obj);
SCM_API SCM scm_make_procedure_with_setter (SCM procedure, SCM setter);
SCM_API SCM scm_procedure (SCM proc);
SCM_API SCM scm_setter (SCM proc);
SCM_INTERNAL void scm_init_procs (void);

#endif  /* SCM_PROCS_H */
