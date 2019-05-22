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
read_signed_bitfield(uint32_t word, uint8_t width, uint8_t shift)
{
  return ((int32_t)word) << (32 - width - shift) >> (32 - width);
}

static inline uint32_t
read_unsigned_bitfield(uint32_t word, uint8_t width, uint8_t shift)
{
  return word << (32 - width - shift) >> (32 - width);
}

static inline int
in_signed_range(ptrdiff_t diff, uint8_t bits)
{
  return (-1 << (bits - 1)) <= diff && diff < (1 << (bits - 1));
}

static inline int
in_unsigned_range(uint32_t val, uint8_t bits)
{
  ASSERT(bits < __WORDSIZE);
  return val < (1 << bits);
}

static inline uint32_t
write_unsigned_bitfield(uint32_t word, uint32_t val, uint8_t width, uint8_t shift)
{
  ASSERT(read_unsigned_bitfield(word, width, shift) == 0);
  ASSERT(in_unsigned_range(val, width));
  return word | (val << shift);
}

static inline int32_t
write_signed_bitfield(uint32_t word, ptrdiff_t val, uint8_t width, uint8_t shift)
{
  ASSERT(read_signed_bitfield(word, width, shift) == 0);
  ASSERT(in_signed_range(val, width));
  return word | ((val & ((1 << width) - 1)) << shift);
}

#define DEFINE_ENCODER(name, width, shift, kind, val_t)                 \
  static const uint8_t name##_width = width;                            \
  static const uint8_t name##_shift = shift;                            \
  static uint32_t                                                       \
  write_##name##_bitfield(uint32_t word, val_t val)                     \
  {                                                                     \
    return write_##kind##_bitfield(word, val, name##_width, name##_shift); \
  }

DEFINE_ENCODER(Rd, 5, 0, unsigned, uint32_t)
DEFINE_ENCODER(Rm, 5, 16, unsigned, uint32_t)
DEFINE_ENCODER(Rn, 5, 5, unsigned, uint32_t)
DEFINE_ENCODER(Rt, 5, 0, unsigned, uint32_t)
DEFINE_ENCODER(Rt2, 5, 10, unsigned, uint32_t)
DEFINE_ENCODER(cond, 4, 12, unsigned, uint32_t)
DEFINE_ENCODER(cond2, 4, 0, unsigned, uint32_t)
DEFINE_ENCODER(simm7, 7, 15, signed, ptrdiff_t)
DEFINE_ENCODER(simm9, 9, 12, signed, ptrdiff_t)
DEFINE_ENCODER(imm12, 12, 10, unsigned, uint32_t)
DEFINE_ENCODER(imm16, 16, 5, unsigned, uint32_t)
DEFINE_ENCODER(simm19, 19, 5, signed, ptrdiff_t)
DEFINE_ENCODER(simm26, 26, 0, signed, ptrdiff_t)
DEFINE_ENCODER(immr, 6, 16, unsigned, uint32_t)
DEFINE_ENCODER(imms, 6, 10, unsigned, uint32_t)
DEFINE_ENCODER(size, 2, 22, unsigned, uint32_t)

#define DEFINE_PATCHABLE_INSTRUCTION(name, kind, RELOC, rsh)            \
  static int32_t                                                        \
  read_##name##_offset(uint32_t *loc)                                   \
  {                                                                     \
    return read_signed_bitfield(*loc, kind##_width, kind##_shift);      \
  }                                                                     \
  static int offset_in_##name##_range(ptrdiff_t diff) maybe_unused;     \
  static int                                                            \
  offset_in_##name##_range(ptrdiff_t diff)                              \
  {                                                                     \
    return in_signed_range(diff, kind##_width);                         \
  }                                                                     \
  static void                                                           \
  patch_##name##_offset(uint32_t *loc, ptrdiff_t diff)                  \
  {                                                                     \
    *loc = write_##kind##_bitfield(*loc, diff);                         \
  }                                                                     \
  static jit_reloc_t                                                    \
  emit_##name(jit_state_t *_jit, uint32_t inst)                         \
  {                                                                     \
    while (1) {                                                         \
      jit_reloc_t ret = jit_reloc (_jit, JIT_RELOC_##RELOC, 0,          \
                                   _jit->pc.uc, _jit->pc.uc, rsh);      \
      if (add_pending_literal(_jit, ret, kind##_width - 1)) {           \
        emit_u32(_jit, inst);                                           \
        return ret;                                                     \
      }                                                                 \
    }                                                                   \
  }

DEFINE_PATCHABLE_INSTRUCTION(jmp, simm26, JMP_WITH_VENEER, 2);
DEFINE_PATCHABLE_INSTRUCTION(jcc, simm19, JCC_WITH_VENEER, 2);
DEFINE_PATCHABLE_INSTRUCTION(load_from_pool, simm19, LOAD_FROM_POOL, 2);

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
  return 16;
}

static void
jit_try_shorten(jit_state_t *_jit, jit_reloc_t reloc, jit_pointer_t addr)
{
}

static void*
bless_function_pointer(void *ptr)
{
  return ptr;
}
