#ifndef SCM_EXTENSIONS_H
#define SCM_EXTENSIONS_H

/* Copyright 2001,2006,2008,2018
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



typedef void (*scm_t_extension_init_func)(void*);

SCM_API void scm_c_register_extension (const char *lib, const char *init,
				       void (*func) (void *), void *data);

SCM_API void scm_c_load_extension (const char *lib, const char *init);
SCM_API SCM scm_load_extension (SCM lib, SCM init);

SCM_INTERNAL void scm_init_extensions (void);

#endif  /* SCM_EXTENSIONS_H */
