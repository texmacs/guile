#ifndef SCM_DEPRECATION_H
#define SCM_DEPRECATION_H

/* Copyright 2001,2006,2008-2009,2018
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



/* These functions are a possibly useful part of the API and not only used
   internally, thus they are exported always, not depending on
   SCM_ENABLE_DEPRECATED.  */

SCM_API void scm_c_issue_deprecation_warning (const char *msg);
SCM_API void scm_c_issue_deprecation_warning_fmt (const char *msg, ...);
SCM_API SCM scm_issue_deprecation_warning (SCM msgs);

SCM_API SCM scm_include_deprecated_features (void);
SCM_INTERNAL void scm_init_deprecation (void);

#endif  /* SCM_DEPRECATION_H */
