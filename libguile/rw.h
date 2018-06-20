#ifndef SCM_RW_H
#define SCM_RW_H

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

SCM_API SCM scm_read_string_x_partial (SCM str, SCM port_or_fdes, SCM start,
				       SCM end);
SCM_API SCM scm_write_string_partial (SCM str, SCM port_or_fdes, SCM start,
				      SCM end);

SCM_INTERNAL SCM scm_init_rw_builtins (void);
SCM_INTERNAL void scm_init_rw (void);

#endif  /* SCM_RW_H */
