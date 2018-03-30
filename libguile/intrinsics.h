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

#ifndef _SCM_VM_INTRINSICS_H_
#define _SCM_VM_INTRINSICS_H_

#include <libguile.h>

#ifdef BUILDING_LIBGUILE

typedef SCM (*scm_t_binary_scm_intrinsic) (SCM, SCM);
typedef SCM (*scm_t_binary_uimm_intrinsic) (SCM, scm_t_uint8);

#define SCM_FOR_ALL_VM_INTRINSICS(M) \
  M(binary_scm, add, "add", ADD) \
  M(binary_uimm, add_immediate, "add/immediate", ADD_IMMEDIATE) \
  M(binary_scm, sub, "sub", SUB) \
  M(binary_uimm, sub_immediate, "sub/immediate", SUB_IMMEDIATE) \
  M(binary_scm, mul, "mul", MUL) \
  M(binary_scm, div, "div", DIV) \
  M(binary_scm, quo, "quo", QUO) \
  M(binary_scm, rem, "rem", REM) \
  M(binary_scm, mod, "mod", MOD) \
  M(binary_scm, logand, "logand", LOGAND) \
  M(binary_scm, logior, "logior", LOGIOR) \
  M(binary_scm, logxor, "logxor", LOGXOR) \
  /* Add new intrinsics here; also update scm_bootstrap_intrinsics.  */

enum scm_vm_intrinsic
  {
#define DEFINE_ENUM(type, id, name, ID) SCM_VM_INTRINSIC_##ID,
    SCM_FOR_ALL_VM_INTRINSICS(DEFINE_ENUM)
#undef DEFINE_ENUM
    SCM_VM_INTRINSIC_COUNT
  };

SCM_INTERNAL struct scm_vm_intrinsics
{
#define DEFINE_MEMBER(type, id, name, ID) scm_t_##type##_intrinsic id;
    SCM_FOR_ALL_VM_INTRINSICS(DEFINE_MEMBER)
#undef DEFINE_MEMBER
} scm_vm_intrinsics;

#endif /* BUILDING_LIBGUILE  */

SCM_INTERNAL SCM scm_intrinsic_list (void);

SCM_INTERNAL void scm_bootstrap_intrinsics (void);
SCM_INTERNAL void scm_init_intrinsics (void);

#endif /* _SCM_INSTRUCTIONS_H_ */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
