#ifndef SCM_SRCPROP_H
#define SCM_SRCPROP_H

/* Copyright 1995-1996,2000-2001,2006,2008-2012,2018
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



#include "libguile/boolean.h"



/* {Source properties}
 */
#define SCM_PROCTRACEP(x) (scm_is_true (scm_procedure_property (x, scm_sym_trace)))
#define SCM_SOURCE_PROPERTY_FLAG_BREAK 1

SCM_API scm_t_bits scm_tc16_srcprops;

SCM_API SCM scm_sym_filename;
SCM_API SCM scm_sym_copy;
SCM_API SCM scm_sym_line;
SCM_API SCM scm_sym_column;



SCM_API SCM scm_supports_source_properties_p (SCM obj);
SCM_API SCM scm_make_srcprops (long line, int col, SCM fname, SCM copy, SCM plist);
SCM_API SCM scm_source_property (SCM obj, SCM key);
SCM_API SCM scm_set_source_property_x (SCM obj, SCM key, SCM datum);
SCM_API SCM scm_source_properties (SCM obj);
SCM_API SCM scm_set_source_properties_x (SCM obj, SCM props);
SCM_INTERNAL int scm_i_has_source_properties (SCM obj);
SCM_INTERNAL void scm_i_set_source_properties_x (SCM obj, long line, int col,
                                                 SCM fname);
SCM_API SCM scm_cons_source (SCM xorig, SCM x, SCM y);
SCM_INTERNAL void scm_init_srcprop (void);


#endif  /* SCM_SRCPROP_H */
