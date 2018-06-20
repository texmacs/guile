#ifndef SCM_HOOKS_H
#define SCM_HOOKS_H

/* Copyright 1995-1996,1999,2000-2001,2006,2008-2009,2018
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
#include <libguile/smob.h>

/*
 * Scheme level hooks
 */

SCM_API scm_t_bits scm_tc16_hook;

#define SCM_HOOKP(x)			SCM_SMOB_PREDICATE (scm_tc16_hook, x)
#define SCM_HOOK_ARITY(hook)		SCM_SMOB_FLAGS (hook)
#define SCM_HOOK_PROCEDURES(hook)	SCM_SMOB_OBJECT (hook)
#define SCM_SET_HOOK_PROCEDURES(hook, procs) SCM_SET_SMOB_OBJECT ((hook), (procs))

#define SCM_VALIDATE_HOOK(pos, a) SCM_MAKE_VALIDATE_MSG (pos, a, HOOKP, "hook")

SCM_API SCM scm_make_hook (SCM n_args);
SCM_API SCM scm_hook_p (SCM x);
SCM_API SCM scm_hook_empty_p (SCM hook);
SCM_API SCM scm_add_hook_x (SCM hook, SCM thunk, SCM appendp);
SCM_API SCM scm_remove_hook_x (SCM hook, SCM thunk);
SCM_API SCM scm_reset_hook_x (SCM hook);
SCM_API SCM scm_run_hook (SCM hook, SCM args);
SCM_API void scm_c_run_hook (SCM hook, SCM args);
SCM_API void scm_c_run_hookn (SCM hook, SCM *argv, size_t nargs);
SCM_API SCM scm_hook_to_list (SCM hook);
SCM_INTERNAL void scm_init_hooks (void);

#endif  /* SCM_HOOKS_H */
