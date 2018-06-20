#ifndef SCM_SORT_H
#define SCM_SORT_H

/* Copyright 1999-2000,2006,2008,2018
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



SCM_API SCM scm_restricted_vector_sort_x (SCM vec,
					  SCM less,
					  SCM startpos,
					  SCM endpos);
SCM_API SCM scm_sorted_p (SCM ls, SCM less);
SCM_API SCM scm_merge (SCM ls1, SCM ls2, SCM less);
SCM_API SCM scm_merge_x (SCM ls1, SCM ls2, SCM less);
SCM_API SCM scm_sort (SCM ls, SCM less);
SCM_API SCM scm_sort_x (SCM ls, SCM less);
SCM_API SCM scm_stable_sort (SCM ls, SCM less);
SCM_API SCM scm_stable_sort_x (SCM ls, SCM less);
SCM_API SCM scm_sort_list (SCM ls, SCM less);
SCM_API SCM scm_sort_list_x (SCM ls, SCM less);
SCM_INTERNAL void scm_init_sort (void);

#endif  /* SCM_SORT_H */
