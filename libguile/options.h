#ifndef SCM_OPTIONS_H
#define SCM_OPTIONS_H

/* Copyright 1995-1996,2000-2001,2006,2008,2018
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



typedef struct scm_t_option
{
  unsigned int type;
  const char *name;
  scm_t_bits val;
  char *doc;
} scm_t_option;


#define SCM_OPTION_BOOLEAN 0
#define SCM_OPTION_INTEGER 1
#define SCM_OPTION_SCM     2


SCM_API SCM scm_options_try (SCM args, scm_t_option options[], const char *s, int dry_run);
SCM_API SCM scm_options (SCM, scm_t_option [], const char*);
SCM_API void scm_init_opts (SCM (*) (SCM), scm_t_option []);
SCM_INTERNAL void scm_init_options (void);

#endif  /* SCM_OPTIONS_H */
