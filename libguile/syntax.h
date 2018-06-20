#ifndef SCM_SYNTAX_H
#define SCM_SYNTAX_H

/* Copyright 2017-2018
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

SCM_INTERNAL SCM scm_syntax_p (SCM obj);
SCM_INTERNAL SCM scm_make_syntax (SCM exp, SCM wrap, SCM module);
SCM_INTERNAL SCM scm_syntax_expression (SCM obj);
SCM_INTERNAL SCM scm_syntax_wrap (SCM obj);
SCM_INTERNAL SCM scm_syntax_module (SCM obj);

SCM_INTERNAL void scm_i_syntax_print (SCM obj, SCM port,
                                      scm_print_state *pstate);
SCM_INTERNAL void scm_init_syntax (void);

#endif  /* SCM_SYNTAX_H */
