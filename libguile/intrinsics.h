/* Copyright 2018-2019
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

#ifndef _SCM_INTRINSICS_H_
#define _SCM_INTRINSICS_H_

#ifndef BUILDING_LIBGUILE
#error intrinsics.h is private and uninstalled
#endif

#include <setjmp.h>

#include <libguile/scm.h>


typedef SCM (*scm_t_scm_from_scm_scm_intrinsic) (SCM, SCM);
typedef SCM (*scm_t_scm_from_scm_uimm_intrinsic) (SCM, uint8_t);
typedef void (*scm_t_scm_sz_u32_intrinsic) (SCM, size_t, uint32_t);
typedef SCM (*scm_t_scm_from_scm_intrinsic) (SCM);
typedef double (*scm_t_f64_from_scm_intrinsic) (SCM);

/* If we don't have 64-bit registers, the intrinsics will take and
   return 64-bit values by reference.  */
#if SIZEOF_UINTPTR_T >= 8
#define INDIRECT_INT64_INTRINSICS 0
#else
#define INDIRECT_INT64_INTRINSICS 1
#endif

#if INDIRECT_INT64_INTRINSICS
typedef void (*scm_t_u64_from_scm_intrinsic) (uint64_t*, SCM);
typedef void (*scm_t_s64_from_scm_intrinsic) (int64_t*, SCM);
typedef SCM (*scm_t_scm_from_u64_intrinsic) (uint64_t*);
typedef SCM (*scm_t_scm_from_s64_intrinsic) (int64_t*);
typedef SCM (*scm_t_scm_from_scm_u64_intrinsic) (SCM, uint64_t*);
#else
typedef uint64_t (*scm_t_u64_from_scm_intrinsic) (SCM);
typedef int64_t (*scm_t_s64_from_scm_intrinsic) (SCM);
typedef SCM (*scm_t_scm_from_u64_intrinsic) (uint64_t);
typedef SCM (*scm_t_scm_from_s64_intrinsic) (int64_t);
typedef SCM (*scm_t_scm_from_scm_u64_intrinsic) (SCM, uint64_t);
#endif

typedef void (*scm_t_thread_intrinsic) (scm_thread*);
typedef void (*scm_t_thread_scm_intrinsic) (scm_thread*, SCM);
typedef void (*scm_t_thread_scm_scm_intrinsic) (scm_thread*, SCM, SCM);
typedef SCM (*scm_t_scm_from_thread_scm_intrinsic) (scm_thread*, SCM);
typedef int (*scm_t_bool_from_scm_scm_intrinsic) (SCM, SCM);
typedef enum scm_compare (*scm_t_compare_from_scm_scm_intrinsic) (SCM, SCM);
typedef void (*scm_t_thread_sp_intrinsic) (scm_thread*, union scm_vm_stack_element*);
typedef SCM (*scm_t_scm_from_thread_u32_intrinsic) (scm_thread*, uint32_t);
typedef uint32_t (*scm_t_u32_from_thread_u32_u32_intrinsic) (scm_thread*, uint32_t, uint32_t);
typedef void (*scm_t_thread_u32_u32_scm_u8_u8_intrinsic) (scm_thread*, uint32_t,
                                                          uint32_t, SCM, uint8_t,
                                                          uint8_t);
typedef SCM (*scm_t_scm_from_scm_scm_scmp_sp_intrinsic) (SCM, SCM, SCM*,
                                                         const union scm_vm_stack_element*);
typedef void (*scm_t_thread_noreturn_intrinsic) (scm_thread*) SCM_NORETURN;
typedef void (*scm_t_thread_scm_noreturn_intrinsic) (scm_thread*, SCM) SCM_NORETURN;
typedef int (*scm_t_int_from_scm_intrinsic) (SCM);
typedef void (*scm_t_scm_scm_noreturn_intrinsic) (SCM, SCM) SCM_NORETURN;
typedef void (*scm_t_noreturn_intrinsic) (void) SCM_NORETURN;
typedef void (*scm_t_scm_noreturn_intrinsic) (SCM) SCM_NORETURN;
typedef void (*scm_t_u32_noreturn_intrinsic) (uint32_t) SCM_NORETURN;
typedef SCM (*scm_t_scm_from_thread_sz_intrinsic) (scm_thread*, size_t);
typedef SCM (*scm_t_scm_from_thread_intrinsic) (scm_thread*);
typedef void (*scm_t_thread_u8_scm_sp_vra_mra_intrinsic) (scm_thread*,
                                                          uint8_t, SCM,
                                                          const union scm_vm_stack_element*,
                                                          uint32_t*, uint8_t*);
typedef void (*scm_t_thread_mra_intrinsic) (scm_thread*, uint8_t*);
typedef uint32_t* (*scm_t_vra_from_thread_intrinsic) (scm_thread*);
typedef uint8_t* (*scm_t_mra_from_thread_scm_intrinsic) (scm_thread*, SCM);
typedef uint8_t* (*scm_t_mra_from_thread_mra_intrinsic) (scm_thread*, uint8_t*);
typedef SCM (*scm_t_scm_from_ptr_intrinsic) (SCM*);
typedef void (*scm_t_ptr_scm_intrinsic) (SCM*, SCM);
typedef SCM (*scm_t_scm_from_ptr_scm_intrinsic) (SCM*, SCM);
typedef SCM (*scm_t_scm_from_ptr_scm_scm_intrinsic) (SCM*, SCM, SCM);
typedef double (*scm_t_f64_from_f64_intrinsic) (double);
typedef uint32_t* scm_t_vcode_intrinsic;

#define SCM_FOR_ALL_VM_INTRINSICS(M) \
  M(scm_from_scm_scm, add, "add", ADD) \
  M(scm_from_scm_uimm, add_immediate, "add/immediate", ADD_IMMEDIATE) \
  M(scm_from_scm_scm, sub, "sub", SUB) \
  M(scm_from_scm_uimm, sub_immediate, "sub/immediate", SUB_IMMEDIATE) \
  M(scm_from_scm_scm, mul, "mul", MUL) \
  M(scm_from_scm_scm, div, "div", DIV) \
  M(scm_from_scm_scm, quo, "quo", QUO) \
  M(scm_from_scm_scm, rem, "rem", REM) \
  M(scm_from_scm_scm, mod, "mod", MOD) \
  M(scm_from_scm_scm, logand, "logand", LOGAND) \
  M(scm_from_scm_scm, logior, "logior", LOGIOR) \
  M(scm_from_scm_scm, logxor, "logxor", LOGXOR) \
  M(scm_sz_u32, string_set_x, "string-set!", STRING_SET_X) \
  M(scm_from_scm, string_to_number, "string->number", STRING_TO_NUMBER) \
  M(scm_from_scm, string_to_symbol, "string->symbol", STRING_TO_SYMBOL) \
  M(scm_from_scm, symbol_to_keyword, "symbol->keyword", SYMBOL_TO_KEYWORD) \
  M(scm_from_scm, class_of, "class-of", CLASS_OF) \
  M(f64_from_scm, scm_to_f64, "scm->f64", SCM_TO_F64) \
  M(u64_from_scm, scm_to_u64, "scm->u64", SCM_TO_U64) \
  M(u64_from_scm, scm_to_u64_truncate, "scm->u64/truncate", SCM_TO_U64_TRUNCATE) \
  M(s64_from_scm, scm_to_s64, "scm->s64", SCM_TO_S64) \
  M(scm_from_u64, u64_to_scm, "u64->scm", U64_TO_SCM) \
  M(scm_from_s64, s64_to_scm, "s64->scm", S64_TO_SCM) \
  M(scm_from_scm_scm, logsub, "logsub", LOGSUB) \
  M(thread_scm_scm, wind, "wind", WIND) \
  M(thread, unwind, "unwind", UNWIND) \
  M(thread_scm_scm, push_fluid, "push-fluid", PUSH_FLUID) \
  M(thread, pop_fluid, "pop-fluid", POP_FLUID) \
  M(scm_from_thread_scm, fluid_ref, "fluid-ref", FLUID_REF) \
  M(thread_scm_scm, fluid_set_x, "fluid-set!", FLUID_SET_X) \
  M(thread_scm, push_dynamic_state, "push-dynamic-state", PUSH_DYNAMIC_STATE) \
  M(thread, pop_dynamic_state, "pop-dynamic-state", POP_DYNAMIC_STATE) \
  M(scm_from_scm_u64, lsh, "lsh", LSH) \
  M(scm_from_scm_u64, rsh, "rsh", RSH) \
  M(scm_from_scm_uimm, lsh_immediate, "lsh/immediate", LSH_IMMEDIATE) \
  M(scm_from_scm_uimm, rsh_immediate, "rsh/immediate", RSH_IMMEDIATE) \
  M(bool_from_scm_scm, heap_numbers_equal_p, "heap-numbers-equal?", HEAP_NUMBERS_EQUAL_P) \
  M(compare_from_scm_scm, less_p, "<?", LESS_P) \
  M(bool_from_scm_scm, numerically_equal_p, "=?", NUMERICALLY_EQUAL_P) \
  M(scm_from_scm_uimm, resolve_module, "resolve-module", RESOLVE_MODULE) \
  M(scm_from_scm_scm, lookup, "lookup", LOOKUP) \
  M(scm_from_scm_scm, define_x, "define!", DEFINE_X) \
  M(thread_sp, expand_stack, "expand-stack", EXPAND_STACK) \
  M(scm_from_thread_u32, cons_rest, "cons-rest", CONS_REST) \
  M(u32_from_thread_u32_u32, compute_kwargs_npositional, "compute-kwargs-npositional", COMPUTE_KWARGS_NPOSITIONAL) \
  M(thread_u32_u32_scm_u8_u8, bind_kwargs, "bind-kwargs", BIND_KWARGS) \
  M(thread_mra, push_interrupt_frame, "push-interrupt-frame", PUSH_INTERRUPT_FRAME) \
  M(thread_scm_scm, foreign_call, "foreign-call", FOREIGN_CALL) \
  M(thread_scm_noreturn, reinstate_continuation_x, "reinstate-continuation!", REINSTATE_CONTINUATION_X) \
  M(scm_from_thread, capture_continuation, "capture-continuation", CAPTURE_CONTINUATION) \
  M(mra_from_thread_scm, compose_continuation, "compose-continuation", COMPOSE_CONTINUATION) \
  M(thread, expand_apply_argument, "expand-apply-argument", EXPAND_APPLY_ARGUMENT) \
  M(mra_from_thread_mra, abort_to_prompt, "abort-to-prompt", ABORT_TO_PROMPT) \
  M(scm_scm_noreturn, throw_, "throw", THROW) \
  M(scm_scm_noreturn, throw_with_value, "throw/value", THROW_WITH_VALUE) \
  M(scm_scm_noreturn, throw_with_value_and_data, "throw/value+data", THROW_WITH_VALUE_AND_DATA) \
  M(thread_noreturn, error_wrong_num_args, "wrong-num-args", ERROR_WRONG_NUM_ARGS) \
  M(noreturn, error_no_values, "no-values", ERROR_NO_VALUES) \
  M(noreturn, error_not_enough_values, "not-enough-values", ERROR_NOT_ENOUGH_VALUES) \
  M(u32_noreturn, error_wrong_number_of_values, "wrong-number-of-values", ERROR_WRONG_NUMBER_OF_VALUES) \
  M(vra_from_thread, get_callee_vcode, "get-callee-vcode", GET_CALLEE_VCODE) \
  M(scm_from_thread_sz, allocate_words, "allocate-words", ALLOCATE_WORDS) \
  M(scm_from_thread, current_module, "current-module", CURRENT_MODULE) \
  M(thread_u8_scm_sp_vra_mra, push_prompt, "push-prompt", PUSH_PROMPT)     \
  M(thread_scm, unpack_values_object, "unpack-values-object", UNPACK_VALUES_OBJECT) \
  M(vcode, handle_interrupt_code, "%handle-interrupt-code", HANDLE_INTERRUPT_CODE) \
  M(scm_from_thread_sz, allocate_words_with_freelist, "allocate-words/freelist", ALLOCATE_WORDS_WITH_FREELIST) \
  M(scm_from_scm, abs, "abs", ABS) \
  M(scm_from_scm, sqrt, "sqrt", SQRT) \
  M(f64_from_f64, fabs, "fabs", FABS) \
  M(f64_from_f64, fsqrt, "fsqrt", FSQRT) \
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

SCM_INTERNAL SCM scm_intrinsic_list (void);

SCM_INTERNAL void scm_bootstrap_intrinsics (void);
SCM_INTERNAL void scm_init_intrinsics (void);

#endif /* _SCM_INTRINSICS_H_ */
