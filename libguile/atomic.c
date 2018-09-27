/* Copyright (C) 2016, 2018 Free Software Foundation, Inc.
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
#include "libguile/ports.h"
#include "libguile/validate.h"
#include "libguile/atomics-internal.h"
#include "libguile/atomic.h"


SCM_DEFINE (scm_make_atomic_box, "make-atomic-box", 1, 0, 0,
            (SCM init),
            "Return an atomic box initialized to value @var{init}.")
#define FUNC_NAME s_scm_make_atomic_box
{
  SCM ret = scm_cell (scm_tc7_atomic_box, SCM_UNPACK (SCM_UNDEFINED));
  scm_atomic_box_set_x (ret, init);
  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_atomic_box_p, "atomic-box?", 1, 0, 0,
            (SCM obj),
            "Return @code{#t} if @var{obj} is an atomic-box object, else\n"
	    "return @code{#f}.")
#define FUNC_NAME s_scm_atomic_box_p
{
  return scm_from_bool (scm_is_atomic_box (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_atomic_box_ref, "atomic-box-ref", 1, 0, 0,
            (SCM box),
            "Fetch the value stored in the atomic box @var{box} and\n"
            "return it.")
#define FUNC_NAME s_scm_atomic_box_ref
{
  SCM_VALIDATE_ATOMIC_BOX (1, box);
  return scm_atomic_ref_scm (scm_atomic_box_loc (box));
}
#undef FUNC_NAME

SCM_DEFINE (scm_atomic_box_set_x, "atomic-box-set!", 2, 0, 0,
            (SCM box, SCM val),
            "Store @var{val} into the atomic box @var{box}.")
#define FUNC_NAME s_scm_atomic_box_set_x
{
  SCM_VALIDATE_ATOMIC_BOX (1, box);
  scm_atomic_set_scm (scm_atomic_box_loc (box), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_atomic_box_swap_x, "atomic-box-swap!", 2, 0, 0,
            (SCM box, SCM val),
            "Store @var{val} into the atomic box @var{box},\n"
            "and return the value that was previously stored in\n"
            "the box.")
#define FUNC_NAME s_scm_atomic_box_swap_x
{
  SCM_VALIDATE_ATOMIC_BOX (1, box);
  return scm_atomic_swap_scm (scm_atomic_box_loc (box), val);
}
#undef FUNC_NAME

SCM_DEFINE (scm_atomic_box_compare_and_swap_x,
            "atomic-box-compare-and-swap!", 3, 0, 0,
            (SCM box, SCM expected, SCM desired),
            "If the value of the atomic box @var{box} is the same as,\n"
            "@var{expected} (in the sense of @code{eq?}), replace the\n"
            "contents of the box with @var{desired}.  Otherwise does not\n"
            "update the box.  Returns the previous value of the box in\n"
            "either case, so you can know if the swap worked by checking\n"
            "if the return value is @code{eq?} to @var{expected}.")
#define FUNC_NAME s_scm_atomic_box_compare_and_swap_x
{
  SCM result = expected;

  SCM_VALIDATE_ATOMIC_BOX (1, box);
  while (!scm_atomic_compare_and_swap_scm (scm_atomic_box_loc (box),
                                           &result, desired)
         && scm_is_eq (result, expected))
    {
      /* 'scm_atomic_compare_and_swap_scm' has spuriously failed,
         i.e. it has returned 0 to indicate failure, although the
         observed value is 'eq?' to EXPECTED.  In this case, we *must*
         try again, because the API of 'atomic-box-compare-and-swap!'
         provides no way to indicate to the caller that the exchange
         failed when the observed value is 'eq?' to EXPECTED.  */
    }
  return result;
}
#undef FUNC_NAME

void
scm_i_atomic_box_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<atomic-box ", port);
  scm_uintprint (SCM_UNPACK (exp), 16, port);
  scm_puts (" value: ", port);
  scm_iprin1 (scm_atomic_box_ref (exp), port, pstate);
  scm_putc ('>', port);
}

static void
scm_init_atomic (void)
{
#include "libguile/atomic.x"
}

void
scm_register_atomic (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_atomic",
			    (scm_t_extension_init_func) scm_init_atomic,
			    NULL);
}
