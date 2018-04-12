/* Copyright (C) 2018 Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "_scm.h"
#include "intrinsics.h"


struct scm_vm_intrinsics scm_vm_intrinsics;

SCM_DEFINE (scm_intrinsic_list, "intrinsic-list", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_intrinsic_list
{
  SCM list = SCM_EOL;

#define ADD_INTRINSIC(type, id, name, ID)                   \
  if (name)                                                 \
    list = scm_acons (scm_from_latin1_symbol (name),        \
                      scm_from_int (SCM_VM_INTRINSIC_##ID), \
                      list);
  SCM_FOR_ALL_VM_INTRINSICS (ADD_INTRINSIC);
#undef ADD_INTRINSIC

  return list;
}
#undef FUNC_NAME

static SCM
add_immediate (SCM a, scm_t_uint8 b)
{
  return scm_sum (a, scm_from_uint8 (b));
}

static SCM
sub_immediate (SCM a, scm_t_uint8 b)
{
  return scm_difference (a, scm_from_uint8 (b));
}

static void
string_set_x (SCM str, scm_t_uint64 idx, scm_t_uint64 ch)
{
  str = scm_i_string_start_writing (str);
  scm_i_string_set_x (str, idx, ch);
  scm_i_string_stop_writing ();
}

static SCM
string_to_number (SCM str)
{
  return scm_string_to_number (str, SCM_UNDEFINED /* radix = 10 */);
}

static scm_t_uint64
scm_to_uint64_truncate (SCM x)
{
  if (SCM_I_INUMP (x))
    return (scm_t_uint64) SCM_I_INUM (x);
  else
    return scm_to_uint64 (scm_logand (x, scm_from_uint64 ((scm_t_uint64) -1)));
}

void
scm_bootstrap_intrinsics (void)
{
  scm_vm_intrinsics.add = scm_sum;
  scm_vm_intrinsics.add_immediate = add_immediate;
  scm_vm_intrinsics.sub = scm_difference;
  scm_vm_intrinsics.sub_immediate = sub_immediate;
  scm_vm_intrinsics.mul = scm_product;
  scm_vm_intrinsics.div = scm_divide;
  scm_vm_intrinsics.quo = scm_quotient;
  scm_vm_intrinsics.rem = scm_remainder;
  scm_vm_intrinsics.mod = scm_modulo;
  scm_vm_intrinsics.logand = scm_logand;
  scm_vm_intrinsics.logior = scm_logior;
  scm_vm_intrinsics.logxor = scm_logxor;
  scm_vm_intrinsics.string_set_x = string_set_x;
  scm_vm_intrinsics.string_to_number = string_to_number;
  scm_vm_intrinsics.string_to_symbol = scm_string_to_symbol;
  scm_vm_intrinsics.symbol_to_keyword = scm_symbol_to_keyword;
  scm_vm_intrinsics.class_of = scm_class_of;
  scm_vm_intrinsics.scm_to_f64 = scm_to_double;
  scm_vm_intrinsics.scm_to_u64 = scm_to_uint64;
  scm_vm_intrinsics.scm_to_u64_truncate = scm_to_uint64_truncate;
  scm_vm_intrinsics.scm_to_s64 = scm_to_int64;
  scm_vm_intrinsics.u64_to_scm = scm_from_uint64;
  scm_vm_intrinsics.s64_to_scm = scm_from_int64;

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_intrinsics",
                            (scm_t_extension_init_func)scm_init_intrinsics,
                            NULL);
}

void
scm_init_intrinsics (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "libguile/intrinsics.x"
#endif
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
