#ifndef SCM_GENERALIZED_VECTORS_H
#define SCM_GENERALIZED_VECTORS_H

/* Copyright 1995-1997,1999-2001,2004,2006,2008-2009,2013,2018
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



#include "libguile/snarf.h"



/* Generalized vectors */

SCM_API SCM scm_make_generalized_vector (SCM type, SCM len, SCM fill);
SCM_INTERNAL void scm_i_register_vector_constructor (SCM type, SCM (*ctor)(SCM, SCM));

#define SCM_VECTOR_IMPLEMENTATION(type, ctor)                   \
  SCM_SNARF_INIT (scm_i_register_vector_constructor             \
                  (scm_i_array_element_types[type], ctor))

SCM_INTERNAL void scm_init_generalized_vectors (void);

#endif  /* SCM_GENERALIZED_VECTORS_H */
