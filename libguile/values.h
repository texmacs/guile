#ifndef SCM_VALUES_H
#define SCM_VALUES_H

/* Copyright 2000-2001,2006,2008,2012,2018
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



#include "libguile/gc.h"

static inline int
scm_is_values (SCM x)
{
  return SCM_HAS_TYP7 (x, scm_tc7_values);
}

#ifdef BUILDING_LIBGUILE
static inline size_t
scm_i_nvalues (SCM x)
{
  return SCM_CELL_WORD_0 (x) >> 8;
}

static inline SCM
scm_i_value_ref (SCM x, size_t n)
{
  return SCM_CELL_OBJECT (x, n+1);
}
#endif

#define SCM_VALUESP(x) (scm_is_values (x))

SCM_INTERNAL void scm_i_extract_values_2 (SCM obj, SCM *p1, SCM *p2);

SCM_API SCM scm_values (SCM args);
SCM_API SCM scm_c_values (SCM *base, size_t n);
SCM_API SCM scm_values_2 (SCM a, SCM b);
SCM_API SCM scm_values_3 (SCM a, SCM b, SCM c);
SCM_API size_t scm_c_nvalues (SCM obj);
SCM_API SCM scm_c_value_ref (SCM obj, size_t idx);
SCM_INTERNAL void scm_init_values (void);

#endif  /* SCM_VALUES_H */
