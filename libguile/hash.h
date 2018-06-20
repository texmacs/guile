#ifndef SCM_HASH_H
#define SCM_HASH_H

/* Copyright 1995-1996,2000,2006,2008,2011,2015,2018
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



SCM_INTERNAL unsigned long scm_i_locale_string_hash (const char *str,
                                                     size_t len);
SCM_INTERNAL unsigned long scm_i_latin1_string_hash (const  char *str,
                                                     size_t len);
SCM_INTERNAL unsigned long scm_i_utf8_string_hash (const char *str,
                                                   size_t len);

SCM_INTERNAL unsigned long scm_i_string_hash (SCM str);
SCM_API unsigned long scm_ihashq (SCM obj, unsigned long n);
SCM_API SCM scm_hashq (SCM obj, SCM n);
SCM_API unsigned long scm_ihashv (SCM obj, unsigned long n);
SCM_API SCM scm_hashv (SCM obj, SCM n);
SCM_API unsigned long scm_ihash (SCM obj, unsigned long n);
SCM_API SCM scm_hash (SCM obj, SCM n);
SCM_INTERNAL void scm_init_hash (void);

#endif  /* SCM_HASH_H */
