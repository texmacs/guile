#ifndef SCM_WEAK_VECTOR_H
#define SCM_WEAK_VECTOR_H

/* Copyright 1995-1996,2000-2001,2003,2006,2008-2009,2011,2014,2018
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


/* Weak vectors.  */

#define SCM_I_WVECTP(x) (SCM_HAS_TYP7 (x, scm_tc7_wvect))

SCM_API SCM scm_make_weak_vector (SCM len, SCM fill);
SCM_API SCM scm_weak_vector (SCM l);
SCM_API SCM scm_weak_vector_p (SCM x);
SCM_API SCM scm_weak_vector_length (SCM v);
SCM_API SCM scm_weak_vector_ref (SCM v, SCM k);
SCM_API SCM scm_weak_vector_set_x (SCM v, SCM k, SCM x);

SCM_API SCM scm_c_make_weak_vector (size_t len, SCM fill);
SCM_API int scm_is_weak_vector (SCM obj);
SCM_API size_t scm_c_weak_vector_length (SCM vec);
SCM_API SCM scm_c_weak_vector_ref (SCM v, size_t k);
SCM_API void scm_c_weak_vector_set_x (SCM v, size_t k, SCM x);

SCM_INTERNAL void scm_init_weak_vectors (void);


#endif  /* SCM_WEAK_VECTOR_H */
