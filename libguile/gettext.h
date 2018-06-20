#ifndef SCM_GETTEXT_H
#define SCM_GETTEXT_H

/* Copyright 2004,2006,2008,2018
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

SCM_API SCM scm_gettext (SCM msgid, SCM domainname, SCM category);
SCM_API SCM scm_ngettext (SCM msgid, SCM msgid_plural, SCM n, SCM domainname, SCM category);
SCM_API SCM scm_textdomain (SCM domainname);
SCM_API SCM scm_bindtextdomain (SCM domainname, SCM directory);
SCM_API SCM scm_bind_textdomain_codeset (SCM domainname, SCM encoding);

SCM_INTERNAL int scm_i_to_lc_category (SCM category, int allow_lc_all);

SCM_INTERNAL void scm_init_gettext (void);

#endif  /* SCM_GETTEXT_H */
