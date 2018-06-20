#ifndef SCM_DEBUG_MALLOC_H
#define SCM_DEBUG_MALLOC_H

/* Copyright 2000-2001,2006,2008,2018
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



SCM_API void scm_malloc_register (void *obj, const char *what);
SCM_API void scm_malloc_unregister (void *obj);
SCM_API void scm_malloc_reregister (void *obj, void *new, const char *what);

SCM_API SCM scm_malloc_stats (void);

SCM_INTERNAL void scm_debug_malloc_prehistory (void);
SCM_INTERNAL void scm_init_debug_malloc (void);

#endif  /* SCM_DEBUG_MALLOC_H */
