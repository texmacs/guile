/*
 * Copyright (C) 2013-2019  Free Software Foundation, Inc.
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
 *	Paulo Cesar Pereira de Andrade
 */

/* libgcc */
extern void __clear_cache(void *, void *);


static inline int32_t
read_offset(uint32_t *loc, uint8_t bits, uint8_t base)
{
  return (*((int32_t*)loc)) << (32 - bits - base) >> (32 - bits);
}

static inline int
in_signed_range(ptrdiff_t diff, uint8_t bits)
{
  return (-1 << (bits - 1)) <= diff && diff < (1 << (bits - 1));
}

static inline int32_t
write_offset(uint32_t *loc, uint8_t bits, uint8_t base, ptrdiff_t offset)
{
  ASSERT(read_offset(loc, bits, base) == 0);
  ASSERT(in_signed_range(offset, bits));
  *loc |= (((uint32_t) offset) & ((1 << bits) - 1)) << base;
}

#define DEFINE_PATCHABLE_INSTRUCTION(name, bits, base, RELOC, rsh)      \
  static const uint8_t name##_offset_bits = bits;                       \
  static const uint8_t name##_offset_base = base;                       \
  static int32_t                                                        \
  read_##name##_offset(uint32_t *loc)                                   \
  {                                                                     \
    return read_offset(loc, name##_offset_bits, name##_offset_base);    \
  }                                                                     \
  static int                                                            \
  in_##name##_range(ptrdiff_t diff)                                     \
  {                                                                     \
    return in_signed_range(diff, name##_offset_bits);                   \
  }                                                                     \
  static int32_t                                                        \
  write_##name##_offset(uint32_t *loc, ptrdiff_t diff)                  \
  {                                                                     \
    return write_offset(loc, name##_offset_bits, name##_offset_base, diff); \
  }                                                                     \
  static jit_reloc_t                                                    \
  emit_##name(jit_state_t *_jit, uint32_t inst)                         \
  {                                                                     \
    jit_reloc_t ret = jit_reloc (_jit, JIT_RELOC_##RELOC, 0,            \
                                 _jit->pc.uc, _jit->pc.uc, rsh);        \
    add_pending_literal(_jit, ret, name##_offset_bits);                 \
    emit_u32(_jit, inst);                                               \
    return ret;                                                         \
  }

DEFINE_PATCHABLE_INSTRUCTION(jmp, 26, 0, JCC_WITH_VENEER, 2);
DEFINE_PATCHABLE_INSTRUCTION(jcc, 19, 5, JMP_WITH_VENEER, 2);
DEFINE_PATCHABLE_INSTRUCTION(load_from_pool, 19, 5, LOAD_FROM_POOL, 2);

struct veneer
{
  uint32_t ldr;
  uint32_t br;
  uint64_t addr;
};

static void
patch_veneer(uint32_t *loc, jit_pointer_t addr)
{
  struct veneer *v = (struct veneer*) v;
  v->addr = (uint64_t) addr;
}

#include "aarch64-cpu.c"
#include "aarch64-fpu.c"

static const jit_gpr_t abi_gpr_args[] = {
  _X0, _X1, _X2, _X3, _X4, _X5, _X6, _X7
};

static const jit_fpr_t abi_fpr_args[] = {
  _D0, _D1, _D2, _D3, _D4, _D5, _D6, _D7
};

static const int abi_gpr_arg_count = sizeof(abi_gpr_args) / sizeof(abi_gpr_args[0]);
static const int abi_fpr_arg_count = sizeof(abi_fpr_args) / sizeof(abi_fpr_args[0]);

struct abi_arg_iterator
{
  const jit_operand_t *args;
  size_t argc;

  size_t arg_idx;
  size_t gpr_idx;
  size_t fpr_idx;
  size_t stack_size;
  size_t stack_padding;
};

static size_t page_size;

jit_bool_t
jit_get_cpu(void)
{
  page_size = sysconf(_SC_PAGE_SIZE);
  return 1;
}

jit_bool_t
jit_init(jit_state_t *_jit)
{
  return 1;
}

static size_t
jit_operand_abi_sizeof(enum jit_operand_abi abi)
{
  return 8;
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
  if (is_gpr_arg(abi) && iter->gpr_idx < abi_gpr_arg_count) {
    *arg = jit_operand_gpr (abi, abi_gpr_args[iter->gpr_idx++]);
  } else if (is_fpr_arg(abi) && iter->fpr_idx < abi_fpr_arg_count) {
    *arg = jit_operand_fpr (abi, abi_fpr_args[iter->fpr_idx++]);
  } else {
    *arg = jit_operand_mem (abi, JIT_SP, iter->stack_size);
    iter->stack_size += 8;
  }
  iter->arg_idx++;
}

void
jit_flush(void *fptr, void *tptr)
{
  jit_word_t f = (jit_word_t)fptr & -page_size;
  jit_word_t t = (((jit_word_t)tptr) + page_size - 1) & -page_size;
  __clear_cache((void *)f, (void *)t);
}

static inline size_t
jit_stack_alignment(void)
{
  return 16;
}
