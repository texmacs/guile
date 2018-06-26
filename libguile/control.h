/* Copyright 2010-2013,2018
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

#ifndef SCM_CONTROL_H
#define SCM_CONTROL_H

#include "libguile/scm.h"


SCM_INTERNAL SCM scm_i_prompt_pop_abort_args_x (struct scm_vm *vp,
                                                ptrdiff_t saved_stack_depth);

SCM_INTERNAL SCM scm_i_make_composable_continuation (SCM vmcont);

SCM_INTERNAL SCM scm_abort_to_prompt_star (SCM tag, SCM args) SCM_NORETURN;


SCM_INTERNAL void scm_init_control (void);


#endif /* SCM_CONTROL_H */
