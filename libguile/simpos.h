#ifndef SCM_SIMPOS_H
#define SCM_SIMPOS_H

/* Copyright 1995-1998,2000,2006,2008,2013,2018
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



SCM_API SCM scm_system (SCM cmd);
SCM_API SCM scm_getenv (SCM nam);
SCM_API SCM scm_primitive_exit (SCM status);
SCM_API SCM scm_primitive__exit (SCM status);
SCM_INTERNAL int scm_getenv_int (const char *var, int def);
SCM_INTERNAL void scm_init_simpos (void);

#endif  /* SCM_SIMPOS_H */
