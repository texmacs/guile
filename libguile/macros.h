#ifndef SCM_MACROS_H
#define SCM_MACROS_H

/* Copyright 1998,2000-2003,2006,2008-2010,2018
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



typedef SCM (*scm_t_macro_primitive) (SCM, SCM);

SCM_API SCM scm_make_syntax_transformer (SCM name_or_existing_definition,
                                         SCM type, SCM binding);
SCM_API SCM scm_macro_p (SCM obj);
SCM_API SCM scm_macro_type (SCM m);
SCM_API SCM scm_macro_name (SCM m);
SCM_API SCM scm_macro_binding (SCM m);
SCM_API SCM scm_macro_transformer (SCM m);

SCM_INTERNAL SCM scm_i_make_primitive_macro (const char *name,
                                             scm_t_macro_primitive fn);
SCM_INTERNAL scm_t_macro_primitive scm_i_macro_primitive (SCM m);

SCM_INTERNAL void scm_init_macros (void);


#endif  /* SCM_MACROS_H */
