/* Copyright 1995-1997,2000-2001,2006,2008,2010-2011,2014,2018
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




#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "dynwind.h"
#include "gsubr.h"
#include "numbers.h"
#include "ports.h"
#include "threads.h"

#include "stackchk.h"




/* {Stack Checking}
 */

int scm_stack_checking_enabled_p;

long
scm_stack_size (SCM_STACKITEM *start)
{
  SCM_STACKITEM stack;
#if SCM_STACK_GROWS_UP
  return &stack - start;
#else
  return start - &stack;
#endif /* SCM_STACK_GROWS_UP */
}


void 
scm_stack_report ()
{
  SCM port = scm_current_error_port ();
  SCM_STACKITEM stack;
  scm_thread *thread = SCM_I_CURRENT_THREAD;

  scm_uintprint ((scm_stack_size (thread->continuation_base) 
		  * sizeof (SCM_STACKITEM)),
		 16, port);
  scm_puts (" of stack: 0x", port);
  scm_uintprint ((scm_t_bits) thread->continuation_base, 16, port);
  scm_puts (" - 0x", port);
  scm_uintprint ((scm_t_bits) &stack, 16, port);
  scm_puts ("\n", port);
}


SCM_DEFINE (scm_sys_get_stack_size, "%get-stack-size", 0, 0, 0,
	    (),
	    "Return the current thread's C stack size (in Scheme objects).")
#define FUNC_NAME s_scm_sys_get_stack_size
{
  return scm_from_long (scm_stack_size (SCM_I_CURRENT_THREAD->base));
}
#undef FUNC_NAME


void
scm_init_stackchk ()
{
#include "stackchk.x"
}
