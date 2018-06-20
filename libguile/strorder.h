#ifndef SCM_STRORDER_H
#define SCM_STRORDER_H

/* Copyright 1995-1996,2000,2006,2008,2018
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



SCM_API SCM scm_string_equal_p (SCM s1, SCM s2);
SCM_API SCM scm_string_ci_equal_p (SCM s1, SCM s2);
SCM_API SCM scm_string_less_p (SCM s1, SCM s2);
SCM_API SCM scm_string_leq_p (SCM s1, SCM s2);
SCM_API SCM scm_string_gr_p (SCM s1, SCM s2);
SCM_API SCM scm_string_geq_p (SCM s1, SCM s2);
SCM_API SCM scm_string_ci_less_p (SCM s1, SCM s2);
SCM_API SCM scm_string_ci_leq_p (SCM s1, SCM s2);
SCM_API SCM scm_string_ci_gr_p (SCM s1, SCM s2);
SCM_API SCM scm_string_ci_geq_p (SCM s1, SCM s2);
SCM_INTERNAL void scm_init_strorder (void);

#endif  /* SCM_STRORDER_H */
