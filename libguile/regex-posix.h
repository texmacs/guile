#ifndef SCM_REGEX_POSIX_H
#define SCM_REGEX_POSIX_H

/* Copyright 1997-1998,2000-2001,2006,2008,2018
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



#include <libguile/error.h>

SCM_API scm_t_bits scm_tc16_regex;
#define SCM_RGX(X)	((regex_t *) SCM_SMOB_DATA (X))
#define SCM_RGXP(X)	(SCM_SMOB_PREDICATE (scm_tc16_regex, (X)))

#define SCM_VALIDATE_RGXP(pos, a) SCM_MAKE_VALIDATE_MSG (pos, a, RGXP, "regexp")

SCM_API SCM scm_make_regexp (SCM pat, SCM flags);
SCM_API SCM scm_regexp_p (SCM x);
SCM_API SCM scm_regexp_exec (SCM rx, SCM str, SCM start, SCM flags);
SCM_INTERNAL void scm_init_regex_posix (void);

#endif  /* SCM_REGEX_POSIX_H */
