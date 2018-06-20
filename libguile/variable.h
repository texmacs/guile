#ifndef SCM_VARIABLE_H
#define SCM_VARIABLE_H

/* Copyright 1995-1996,2000-2001,2006,2008,2011,2018
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
#include <libguile/gc.h>
#include <libguile/snarf.h>



/* Variables 
 */
#define SCM_VARIABLEP(X)      (SCM_HAS_TYP7 (X, scm_tc7_variable))
#define SCM_VARIABLE_REF(V)   SCM_CELL_OBJECT_1 (V)
#define SCM_VARIABLE_SET(V, X) SCM_SET_CELL_OBJECT_1 (V, X)
#define SCM_VARIABLE_LOC(V)   (SCM_CELL_OBJECT_LOC ((V), 1))

#define SCM_VALIDATE_VARIABLE(pos, var) \
  SCM_MAKE_VALIDATE_MSG (pos, var, VARIABLEP, "variable")




#define SCM_VARIABLE(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_c_define (scheme_name, SCM_BOOL_F);)

#define SCM_GLOBAL_VARIABLE(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_c_define (scheme_name, SCM_BOOL_F);)

#define SCM_VARIABLE_INIT(c_name, scheme_name, init_val) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_c_define (scheme_name, init_val);)

#define SCM_GLOBAL_VARIABLE_INIT(c_name, scheme_name, init_val) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_c_define (scheme_name, init_val);)




SCM_API SCM scm_make_variable (SCM init);
SCM_API SCM scm_make_undefined_variable (void);
SCM_API SCM scm_variable_p (SCM obj);
SCM_API SCM scm_variable_ref (SCM var);
SCM_API SCM scm_variable_set_x (SCM var, SCM val);
SCM_API SCM scm_variable_unset_x (SCM var);
SCM_API SCM scm_variable_bound_p (SCM var);

SCM_INTERNAL void scm_i_variable_print (SCM var, SCM port, scm_print_state *pstate);

SCM_INTERNAL void scm_init_variable (void);

#endif  /* SCM_VARIABLE_H */
