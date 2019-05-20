/*
 * Copyright (C) 2012-2019  Free Software Foundation, Inc.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GNU lightning is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * Authors:
 *      Paulo Cesar Pereira de Andrade
 */

/*
 * Types
 */
typedef union _jit_thumb_t {
  int32_t             i;
  int16_t             s[2];
} jit_thumb_t;

/* libgcc */
extern void __clear_cache(void *, void *);

#include "arm-cpu.c"
#include "arm-vfp.c"

static const jit_gpr_t abi_gpr_args[] = {
  _R0, _R1, _R2, _R3
};
static const int abi_gpr_arg_count = sizeof(abi_gpr_args) / sizeof(abi_gpr_args[0]);

struct abi_arg_iterator
{
  const jit_operand_t *args;
  size_t argc;

  size_t arg_idx;
  size_t gpr_idx;
  uint32_t vfp_used_registers;
  size_t stack_size;
  size_t stack_padding;
};

static size_t page_size;

jit_bool_t
jit_get_cpu(void)
{
  page_size = sysconf(_SC_PAGE_SIZE);
  // FIXME check version, thumb, hardware fp support
  return 1;
}

jit_bool_t
jit_init(jit_state_t *_jit)
{
  return 1;
}

static size_t
jit_initial_frame_size (void)
{
  return 0;
}

static void
reset_abi_arg_iterator(struct abi_arg_iterator *iter, size_t argc,
                       const jit_operand_t *args)
{
  memset(iter, 0, sizeof *iter);
  iter->argc = argc;
  iter->args = args;
}

static void
next_abi_arg(struct abi_arg_iterator *iter, jit_operand_t *arg)
{
  ASSERT(iter->arg_idx < iter->argc);
  enum jit_operand_abi abi = iter->args[iter->arg_idx].abi;
  iter->arg_idx++;
  if (is_gpr_arg(abi) && iter->gpr_idx < abi_gpr_arg_count) {
    *arg = jit_operand_gpr (abi, abi_gpr_args[iter->gpr_idx++]);
    return;
  }
  if (is_fpr_arg(abi)) {
    // The ARM VFP ABI passes floating-point arguments in d0-d7
    // (s0-s15), and allows for "back-filling".  Say you have a
    // function:
    //
    //  void f(float a, double b, float c);
    //
    // A gets allocated to s0, then b to d1 (which aliases s2+s3), then
    // c to s1.
    uint32_t width = abi == JIT_OPERAND_ABI_FLOAT ? 1 : 2;
    uint32_t mask = (1 << width) - 1;
    for (size_t i = 0; i < 16; i += width) {
      if ((iter->vfp_used_registers & (mask << i)) == 0) {
        iter->vfp_used_registers |= (mask << i);
        *arg = jit_operand_fpr (abi, JIT_FPR(i));
        return;
      }
    }
  }
  *arg = jit_operand_mem (abi, JIT_SP, iter->stack_size);
  iter->stack_size += 4;
}

static void
jit_flush(void *fptr, void *tptr)
{
  jit_word_t f = (jit_word_t)fptr & -page_size;
  jit_word_t t = (((jit_word_t)tptr) + page_size - 1) & -page_size;
  __clear_cache((void *)f, (void *)t);
}

static inline size_t
jit_stack_alignment(void)
{
  return 8;
}

static void
jit_try_shorten(jit_state_t *_jit, jit_reloc_t reloc, jit_pointer_t addr)
{
}

static void*
bless_function_pointer(void *ptr)
{
  // Set low bit to mark as thumb mode.
  return (void*) (((uintptr_t)ptr) | 1);
}
