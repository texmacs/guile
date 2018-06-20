#ifndef SCM_CHOOKS_H
#define SCM_CHOOKS_H

/* Copyright 1995-1996,1999,2000-2001,2006,2008-2009,2018
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

/*
 * C level hooks
 */

/*
 * The interface is designed for and- and or-type hooks which
 * both may want to indicate success/failure and return a result.
 */

typedef enum scm_t_c_hook_type {
  SCM_C_HOOK_NORMAL,
  SCM_C_HOOK_OR,
  SCM_C_HOOK_AND
} scm_t_c_hook_type;

typedef void  *(*scm_t_c_hook_function) (void *hook_data,
					 void *fn_data,
					 void *data);

typedef struct scm_t_c_hook_entry {
  struct scm_t_c_hook_entry *next;
  scm_t_c_hook_function func;
  void *data;
} scm_t_c_hook_entry;

typedef struct scm_t_c_hook {
  scm_t_c_hook_entry *first;
  scm_t_c_hook_type type;
  void *data;
} scm_t_c_hook;

SCM_API void scm_c_hook_init (scm_t_c_hook *hook,
			      void *hook_data,
			      scm_t_c_hook_type type);
SCM_API void scm_c_hook_add (scm_t_c_hook *hook,
			     scm_t_c_hook_function func,
			     void *fn_data, 
			     int appendp);
SCM_API void scm_c_hook_remove (scm_t_c_hook *hook,
				scm_t_c_hook_function func,
				void *fn_data);
SCM_API void *scm_c_hook_run (scm_t_c_hook *hook, void *data);


#endif  /* SCM_CHOOKS_H */
