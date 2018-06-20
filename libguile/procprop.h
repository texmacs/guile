#ifndef SCM_PROCPROP_H
#define SCM_PROCPROP_H

/* Copyright 1995-1996,1998,2000,2006,2008-2011,2013,2018
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



SCM_API SCM scm_sym_name;
SCM_API SCM scm_sym_system_procedure;
SCM_INTERNAL SCM scm_sym_documentation;



SCM_INTERNAL int scm_i_procedure_arity (SCM proc, int *req, int *opt, int *rest);
SCM_API SCM scm_set_procedure_minimum_arity_x (SCM proc, SCM req, SCM opt,
                                               SCM rest);
SCM_API SCM scm_procedure_minimum_arity (SCM proc);
SCM_API SCM scm_procedure_properties (SCM proc);
SCM_API SCM scm_set_procedure_properties_x (SCM proc, SCM alist);
SCM_API SCM scm_procedure_property (SCM proc, SCM key);
SCM_API SCM scm_set_procedure_property_x (SCM proc, SCM key, SCM val);
SCM_API SCM scm_procedure_source (SCM proc);
SCM_API SCM scm_procedure_name (SCM proc);
SCM_API SCM scm_procedure_documentation (SCM proc);
SCM_INTERNAL void scm_init_procprop (void);

#endif  /* SCM_PROCPROP_H */
