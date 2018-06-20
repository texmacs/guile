/* Copyright 1998-2003,2006,2008-2013,2015,2018
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

#include "gsubr.h"
#include "eval.h"
#include "list.h"
#include "fluids.h"
#include "modules.h"
#include "pairs.h"
#include "symbols.h"
#include "variable.h"

#include "evalext.h"

SCM_DEFINE (scm_defined_p, "defined?", 1, 1, 0,
            (SCM sym, SCM module),
	    "Return @code{#t} if @var{sym} is defined in the module "
	    "@var{module} or the current module when @var{module} is not"
	    "specified.")
#define FUNC_NAME s_scm_defined_p
{
  SCM var;

  SCM_VALIDATE_SYMBOL (1, sym);

  if (SCM_UNBNDP (module))
    module = scm_current_module ();
  else
    SCM_VALIDATE_MODULE (2, module);

  var = scm_module_variable (module, sym);

  return (scm_is_false (var) || SCM_UNBNDP (SCM_VARIABLE_REF (var))
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}
#undef FUNC_NAME


SCM_DEFINE (scm_self_evaluating_p, "self-evaluating?", 1, 0, 0,
	    (SCM obj),
	    "Return #t for objects which Guile considers self-evaluating")
#define FUNC_NAME s_scm_self_evaluating_p
{
  switch (SCM_ITAG3 (obj))
    {
    case scm_tc3_int_1:
    case scm_tc3_int_2:
      /* inum */
      return SCM_BOOL_T;
    case scm_tc3_imm24:
	/* characters, booleans, other immediates */
      return scm_from_bool (!scm_is_null_and_not_nil (obj));
    case scm_tc3_cons:
      switch (SCM_TYP7 (obj))
	{
	case scm_tc7_vector:
	case scm_tc7_wvect:
	case scm_tc7_pointer:
	case scm_tc7_hashtable:
	case scm_tc7_weak_set:
	case scm_tc7_weak_table:
	case scm_tc7_fluid:
	case scm_tc7_dynamic_state:
        case scm_tc7_frame:
        case scm_tc7_keyword:
        case scm_tc7_syntax:
        case scm_tc7_vm_cont:
	case scm_tc7_number:
	case scm_tc7_string:
	case scm_tc7_smob:
	case scm_tc7_program:
	case scm_tc7_bytevector:
	case scm_tc7_array:
	case scm_tc7_bitvector:
	case scm_tcs_struct:
	  return SCM_BOOL_T;
	default:
	  return SCM_BOOL_F;
	}
    }
  SCM_MISC_ERROR ("Internal error: Object ~S has unknown type",
		  scm_list_1 (obj));
  return SCM_UNSPECIFIED; /* never reached */
}
#undef FUNC_NAME

void 
scm_init_evalext ()
{
#include "evalext.x"
}
