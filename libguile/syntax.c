/* Copyright (C) 2017 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/keywords.h"
#include "libguile/ports.h"
#include "libguile/syntax.h"
#include "libguile/validate.h"



static int
scm_is_syntax (SCM x)
{
  return SCM_HAS_TYP7 (x, scm_tc7_syntax);
}

#define SCM_VALIDATE_SYNTAX(pos, scm) \
  SCM_I_MAKE_VALIDATE_MSG2 (pos, scm, scm_is_syntax, "syntax object")

SCM_DEFINE (scm_syntax_p, "syntax?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if the argument @var{obj} is a syntax object,\n"
            "else @code{#f}.")
#define FUNC_NAME s_scm_syntax_p
{
  return scm_from_bool (scm_is_syntax (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_syntax, "make-syntax", 3, 0, 0,
	    (SCM exp, SCM wrap, SCM module),
	    "Make a new syntax object.")
#define FUNC_NAME s_scm_make_syntax
{
  return scm_double_cell (scm_tc7_syntax, SCM_UNPACK (exp),
                          SCM_UNPACK (wrap), SCM_UNPACK (module));
}
#undef FUNC_NAME

SCM_DEFINE (scm_syntax_expression, "syntax-expression", 1, 0, 0,
	    (SCM obj),
	    "Return the expression contained in the syntax object @var{obj}.")
#define FUNC_NAME s_scm_syntax_expression
{
  SCM_VALIDATE_SYNTAX (1, obj);
  return SCM_CELL_OBJECT_1 (obj);
}
#undef FUNC_NAME

SCM_DEFINE (scm_syntax_wrap, "syntax-wrap", 1, 0, 0,
	    (SCM obj),
	    "Return the wrap contained in the syntax object @var{obj}.")
#define FUNC_NAME s_scm_syntax_wrap
{
  SCM_VALIDATE_SYNTAX (1, obj);
  return SCM_CELL_OBJECT_2 (obj);
}
#undef FUNC_NAME

SCM_DEFINE (scm_syntax_module, "syntax-module", 1, 0, 0,
	    (SCM obj),
	    "Return the module info contained in the syntax object @var{obj}.")
#define FUNC_NAME s_scm_syntax_module
{
  SCM_VALIDATE_SYNTAX (1, obj);
  return SCM_CELL_OBJECT_3 (obj);
}
#undef FUNC_NAME

static SCM print_syntax_var;

static void
init_print_syntax_var (void)
{
  print_syntax_var =
    scm_c_private_variable ("system syntax", "print-syntax");
}

void
scm_i_syntax_print (SCM obj, SCM port, scm_print_state *pstate)
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_print_syntax_var);
  scm_call_2 (scm_variable_ref (print_syntax_var), obj, port);
}

void
scm_init_syntax ()
{
#include "libguile/syntax.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
