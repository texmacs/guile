/* Copyright 1995-2011,2018
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
#  include <config.h>
#endif

#include <alloca.h>

#include "alist.h"
#include "async.h"
#include "continuations.h"
#include "debug.h"
#include "deprecation.h"
#include "dynwind.h"
#include "eq.h"
#include "eval.h"
#include "feature.h"
#include "fluids.h"
#include "goops.h"
#include "gsubr.h"
#include "hash.h"
#include "hashtab.h"
#include "list.h"
#include "macros.h"
#include "memoize.h"
#include "modules.h"
#include "ports.h"
#include "print.h"
#include "procprop.h"
#include "procs.h"
#include "programs.h"
#include "smob.h"
#include "srcprop.h"
#include "stackchk.h"
#include "strings.h"
#include "threads.h"
#include "throw.h"
#include "values.h"

#include "promises.h"




scm_t_bits scm_tc16_promise;

SCM_DEFINE (scm_make_promise, "make-promise", 1, 0, 0, 
	    (SCM thunk),
	    "Create a new promise object.\n\n"
            "@code{make-promise} is a procedural form of @code{delay}.\n"
            "These two expressions are equivalent:\n"
            "@lisp\n"
	    "(delay @var{exp})\n"
	    "(make-promise (lambda () @var{exp}))\n"
            "@end lisp\n")
#define FUNC_NAME s_scm_make_promise
{
  SCM_VALIDATE_THUNK (1, thunk);
  SCM_RETURN_NEWSMOB2 (scm_tc16_promise,
		       SCM_UNPACK (thunk),
		       SCM_UNPACK (scm_make_recursive_mutex ()));
}
#undef FUNC_NAME

static int 
promise_print (SCM exp, SCM port, scm_print_state *pstate)
{
  int writingp = SCM_WRITINGP (pstate);
  scm_puts ("#<promise ", port);
  SCM_SET_WRITINGP (pstate, 1);
  scm_iprin1 (SCM_PROMISE_DATA (exp), port, pstate);
  SCM_SET_WRITINGP (pstate, writingp);
  scm_putc ('>', port);
  return !0;
}

SCM_DEFINE (scm_force, "force", 1, 0, 0, 
	    (SCM promise),
	    "If @var{promise} has not been computed yet, compute and\n"
	    "return @var{promise}, otherwise just return the previously computed\n"
	    "value.")
#define FUNC_NAME s_scm_force
{
  SCM_VALIDATE_SMOB (1, promise, promise);
  scm_lock_mutex (SCM_PROMISE_MUTEX (promise));
  if (!SCM_PROMISE_COMPUTED_P (promise))
    {
      SCM ans = scm_call_0 (SCM_PROMISE_DATA (promise));
      if (!SCM_PROMISE_COMPUTED_P (promise))
	{
	  SCM_SET_PROMISE_DATA (promise, ans);
	  SCM_SET_PROMISE_COMPUTED (promise);
	}
    }
  scm_unlock_mutex (SCM_PROMISE_MUTEX (promise));
  return SCM_PROMISE_DATA (promise);
}
#undef FUNC_NAME


SCM_DEFINE (scm_promise_p, "promise?", 1, 0, 0, 
            (SCM obj),
	    "Return true if @var{obj} is a promise, i.e. a delayed computation\n"
	    "(@pxref{Delayed evaluation,,,r5rs.info,The Revised^5 Report on Scheme}).")
#define FUNC_NAME s_scm_promise_p
{
  return scm_from_bool (SCM_TYP16_PREDICATE (scm_tc16_promise, obj));
}
#undef FUNC_NAME

void 
scm_init_promises ()
{
  scm_tc16_promise = scm_make_smob_type ("promise", 0);
  scm_set_smob_print (scm_tc16_promise, promise_print);

#include "promises.x"

  scm_add_feature ("delay");
}
