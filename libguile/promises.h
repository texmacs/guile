#ifndef SCM_PROMISES_H
#define SCM_PROMISES_H

/* Copyright 1995-1996,1998-2004,2008-2009,2018
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



#include "libguile/smob.h"



/* {Promises}
 */

#define SCM_F_PROMISE_COMPUTED (1L << 0)
#define SCM_PROMISE_COMPUTED_P(promise) \
  (SCM_F_PROMISE_COMPUTED & SCM_SMOB_FLAGS (promise))
#define SCM_SET_PROMISE_COMPUTED(promise) \
  SCM_SET_SMOB_FLAGS ((promise), SCM_F_PROMISE_COMPUTED)
#define SCM_PROMISE_MUTEX     SCM_SMOB_OBJECT_2
#define SCM_PROMISE_DATA      SCM_SMOB_OBJECT
#define SCM_SET_PROMISE_DATA  SCM_SET_SMOB_OBJECT


SCM_API scm_t_bits scm_tc16_promise;



SCM_API SCM scm_make_promise (SCM thunk);
SCM_API SCM scm_force (SCM x);
SCM_API SCM scm_promise_p (SCM x);

SCM_INTERNAL void scm_init_promises (void);


#endif  /* SCM_PROMISES_H */
