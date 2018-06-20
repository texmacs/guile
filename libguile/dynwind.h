#ifndef SCM_DYNWIND_H
#define SCM_DYNWIND_H

/* Copyright 1995-1996,1998-2000,2003-2004,2006,2008,2011-2012,2018
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



#include "libguile/dynstack.h"



SCM_API SCM scm_dynamic_wind (SCM thunk1, SCM thunk2, SCM thunk3);

SCM_INTERNAL void scm_init_dynwind (void);

SCM_API void scm_swap_bindings (SCM vars, SCM vals);

typedef enum {
  SCM_F_DYNWIND_REWINDABLE = SCM_F_DYNSTACK_FRAME_REWINDABLE
} scm_t_dynwind_flags;

typedef enum {
  SCM_F_WIND_EXPLICITLY = SCM_F_DYNSTACK_WINDER_EXPLICIT
} scm_t_wind_flags;

SCM_API void scm_dynwind_begin (scm_t_dynwind_flags);
SCM_API void scm_dynwind_end (void);

SCM_API void scm_dynwind_unwind_handler (void (*func) (void *), void *data,
					 scm_t_wind_flags);
SCM_API void scm_dynwind_rewind_handler (void (*func) (void *), void *data,
					 scm_t_wind_flags);

SCM_API void scm_dynwind_unwind_handler_with_scm (void (*func) (SCM), SCM data,
						  scm_t_wind_flags);
SCM_API void scm_dynwind_rewind_handler_with_scm (void (*func) (SCM), SCM data,
						  scm_t_wind_flags);

SCM_API void scm_dynwind_free (void *mem);


#endif  /* SCM_DYNWIND_H */
