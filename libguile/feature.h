#ifndef SCM_FEATURE_H
#define SCM_FEATURE_H

/* Copyright 1995-1996,1999-2001,2006-2008,2011,2018
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

SCM_API void scm_add_feature (const char* str);
SCM_API SCM scm_program_arguments (void);
SCM_API void scm_set_program_arguments (int argc, char **argv, char *first);
SCM_API SCM scm_set_program_arguments_scm (SCM lst);

SCM_INTERNAL SCM scm_program_arguments_fluid;
SCM_INTERNAL void scm_init_feature (void);

#endif  /* SCM_FEATURE_H */
