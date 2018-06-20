#ifndef SCM_UNICODE_H
#define SCM_UNICODE_H

/* Copyright 2014,2018
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

SCM_INTERNAL SCM scm_formal_name_to_char (SCM);
SCM_INTERNAL SCM scm_char_to_formal_name (SCM);
SCM_INTERNAL void scm_init_unicode (void);

#endif  /* SCM_UNICODE_H */
