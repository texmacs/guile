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

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <sys/mman.h>

#include "../lightening.h"

#if defined(__GNUC__)
# define maybe_unused           __attribute__ ((unused))
#else
# define maybe_unused           /**/
#endif

#define _NOREG 0xffff

union jit_pc
{
  uint8_t *uc;
  uint16_t *us;
  uint32_t *ui;
  uint64_t *ul;
  intptr_t w;
  uintptr_t uw;
};

struct jit_state
{
  union jit_pc pc;
  uint8_t *start;
  uint8_t *last_instruction_start;
  uint8_t *limit;
  uint8_t temp_gpr_saved;
  uint8_t temp_fpr_saved;
  uint8_t overflow;
  int frame_size; // Used to know when to align stack.
  void* (*alloc)(size_t);
  void (*free)(void*);
};

#define ASSERT(x) do { if (!(x)) abort(); } while (0)
#if defined(__GNUC__)
# define UNLIKELY(exprn) __builtin_expect(exprn, 0)
#else
# define UNLIKELY(exprn) exprn
#endif

static jit_bool_t jit_get_cpu(void);
static jit_bool_t jit_init(jit_state_t *);
static void jit_flush(void *fptr, void *tptr);
static void jit_try_shorten(jit_state_t *_jit, jit_reloc_t reloc,
                            jit_pointer_t addr);

struct abi_arg_iterator;

static void reset_abi_arg_iterator(struct abi_arg_iterator *iter, size_t argc,
                                   const jit_operand_t *args);
static void next_abi_arg(struct abi_arg_iterator *iter,
                         jit_operand_t *arg);

jit_bool_t
init_jit(void)
{
  return jit_get_cpu ();
}

jit_state_t *
jit_new_state(void* (*alloc_fn)(size_t), void (*free_fn)(void*))
{
  if (!alloc_fn) alloc_fn = malloc;
  if (!free_fn) free_fn = free;

  jit_state_t *_jit = alloc_fn (sizeof (*_jit));
  if (!_jit)
    abort ();

  memset(_jit, 0, sizeof (*_jit));
  _jit->alloc = alloc_fn;
  _jit->free = free_fn;

  if (!jit_init (_jit)) {
    free_fn (_jit);
    return NULL;
  }

  return _jit;
}

void
jit_destroy_state(jit_state_t *_jit)
{
  _jit->free (_jit);
}

jit_pointer_t
jit_address(jit_state_t *_jit)
{
  ASSERT (_jit->start);
  return _jit->pc.uc;
}

void
jit_begin(jit_state_t *_jit, uint8_t* buf, size_t length)
{
  ASSERT (!_jit->start);

  _jit->pc.uc = _jit->start = buf;
  _jit->limit = buf + length;
  _jit->overflow = 0;
  _jit->frame_size = 0;
}

jit_bool_t
jit_has_overflow(jit_state_t *_jit)
{
  ASSERT (_jit->start);
  return _jit->overflow;
}

void
jit_reset(jit_state_t *_jit)
{
  ASSERT (_jit->start);
  _jit->pc.uc = _jit->start = _jit->limit = NULL;
  _jit->overflow = 0;
  _jit->frame_size = 0;
}

void*
jit_end(jit_state_t *_jit, size_t *length)
{
  uint8_t *code = _jit->start;
  uint8_t *end = _jit->pc.uc;

  ASSERT (code);
  ASSERT (code <= end);
  ASSERT (end <= _jit->limit);
  ASSERT (!_jit->overflow);

  jit_flush (code, end);

  if (length) {
    *length = end - code;
  }

  _jit->pc.uc = _jit->start = _jit->limit = NULL;
  _jit->overflow = 0;
  _jit->frame_size = 0;

  return code;
}

static int
is_power_of_two (unsigned x)
{
  return x && !(x & (x-1));
}

void
jit_align(jit_state_t *_jit, unsigned align)
{
  ASSERT (is_power_of_two (align));
  uintptr_t here = _jit->pc.w;
  uintptr_t there = (here + align - 1) & ~(align - 1);
  if (there - here)
    jit_nop(_jit, there - here);
}

static inline void emit_u8(jit_state_t *_jit, uint8_t u8) {
  if (UNLIKELY(_jit->pc.uc + 1 > _jit->limit)) {
    _jit->overflow = 1;
  } else {
    *_jit->pc.uc++ = u8;
  }
}

static inline void emit_u16(jit_state_t *_jit, uint16_t u16) {
  if (UNLIKELY(_jit->pc.us + 1 > (uint16_t*)_jit->limit)) {
    _jit->overflow = 1;
  } else {
    *_jit->pc.us++ = u16;
  }
}

static inline void emit_u32(jit_state_t *_jit, uint32_t u32) {
  if (UNLIKELY(_jit->pc.ui + 1 > (uint32_t*)_jit->limit)) {
    _jit->overflow = 1;
  } else {
    *_jit->pc.ui++ = u32;
  }
}

static inline void emit_u64(jit_state_t *_jit, uint64_t u64) {
  if (UNLIKELY(_jit->pc.ul + 1 > (uint64_t*)_jit->limit)) {
    _jit->overflow = 1;
  } else {
    *_jit->pc.ul++ = u64;
  }
}

static inline jit_reloc_t
jit_reloc (jit_state_t *_jit, enum jit_reloc_kind kind,
           uint8_t inst_start_offset)
{
  jit_reloc_t ret;

  ret.kind = kind;
  ret.inst_start_offset = inst_start_offset;
  ret.offset = _jit->pc.uc - _jit->start;
  
  switch (kind)
    {
    case JIT_RELOC_ABSOLUTE:
      if (sizeof(intptr_t) == 4)
        emit_u32 (_jit, 0);
      else
        emit_u64 (_jit, 0);
      break;
    case JIT_RELOC_REL8:
      emit_u8 (_jit, 0);
      break;
    case JIT_RELOC_REL16:
      emit_u16 (_jit, 0);
      break;
    case JIT_RELOC_REL32:
      emit_u32 (_jit, 0);
      break;
    case JIT_RELOC_REL64:
      emit_u64 (_jit, 0);
      break;
    default:
      abort ();
    }

  return ret;
}

void
jit_patch_here(jit_state_t *_jit, jit_reloc_t reloc)
{
  jit_patch_there (_jit, reloc, jit_address (_jit));
}

void
jit_patch_there(jit_state_t* _jit, jit_reloc_t reloc, jit_pointer_t addr)
{
  if (_jit->overflow)
    return;
  union jit_pc loc;
  uint8_t *end;
  loc.uc = _jit->start + reloc.offset;
  ptrdiff_t diff;

  switch (reloc.kind)
    {
    case JIT_RELOC_ABSOLUTE:
      if (sizeof(diff) == 4)
        *loc.ui = (uintptr_t)addr;
      else
        *loc.ul = (uintptr_t)addr;
      end = loc.uc + sizeof(diff);
      break;
    case JIT_RELOC_REL8:
      diff = ((uint8_t*)addr) - (loc.uc + 1);
      ASSERT (INT8_MIN <= diff && diff <= INT8_MAX);
      *loc.uc = diff;
      end = loc.uc + 1;
      break;
    case JIT_RELOC_REL16:
      diff = ((uint8_t*)addr) - (loc.uc + 2);
      ASSERT (INT16_MIN <= diff && diff <= INT16_MAX);
      *loc.us = diff;
      end = loc.uc + 2;
      break;
    case JIT_RELOC_REL32:
      diff = ((uint8_t*)addr) - (loc.uc + 4);
      ASSERT (INT32_MIN <= diff && diff <= INT32_MAX);
      *loc.ui = diff;
      end = loc.uc + 4;
      break;
    case JIT_RELOC_REL64:
      diff = ((uint8_t*)addr) - (loc.uc + 8);
      *loc.ul = diff;
      end = loc.uc + 8;
      break;
    default:
      abort ();
    }

  if (end == _jit->pc.uc)
    jit_try_shorten (_jit, reloc, addr);
}

#if defined(__i386__) || defined(__x86_64__)
# include "x86.c"
#elif defined(__mips__)
# include "mips.c"
#elif defined(__arm__)
# include "arm.c"
#elif defined(__ppc__) || defined(__powerpc__)
# include "ppc.c"
#elif defined(__sparc__)
# include "sparc.c"
#elif defined(__hppa__)
# include "hppa.c"
#elif defined(__aarch64__)
# include "aarch64.c"
#elif defined(__s390__) || defined(__s390x__)
# include "s390.c"
#endif

#define JIT_IMPL_0(stem, ret) \
  ret jit_##stem (jit_state_t* _jit) \
  {                                  \
    return stem(_jit);            \
  }
#define JIT_IMPL_1(stem, ret, ta)                 \
  ret jit_##stem (jit_state_t* _jit, jit_##ta##_t a) \
  {                                               \
    return stem(_jit, unwrap_##ta(a));         \
  }
#define JIT_IMPL_2(stem, ret, ta, tb)                             \
  ret jit_##stem (jit_state_t* _jit, jit_##ta##_t a, jit_##tb##_t b) \
  {                                                               \
    return stem(_jit, unwrap_##ta(a), unwrap_##tb(b));         \
  }
#define JIT_IMPL_3(stem, ret, ta, tb, tc)                               \
  ret jit_##stem (jit_state_t* _jit, jit_##ta##_t a, jit_##tb##_t b, jit_##tc##_t c) \
  {                                                                     \
    return stem(_jit, unwrap_##ta(a), unwrap_##tb(b), unwrap_##tc(c)); \
  }
#define JIT_IMPL_4(stem, ret, ta, tb, tc, td)                           \
  ret jit_##stem (jit_state_t* _jit, jit_##ta##_t a, jit_##tb##_t b, jit_##tc##_t c, jit_##td##_t d) \
  {                                                                     \
    return stem(_jit, unwrap_##ta(a), unwrap_##tb(b), unwrap_##tc(c), unwrap_##td(d)); \
  }

#define JIT_IMPL_RFF__(stem) JIT_IMPL_2(stem, jit_reloc_t, fpr, fpr)
#define JIT_IMPL_RGG__(stem) JIT_IMPL_2(stem, jit_reloc_t, gpr, gpr)
#define JIT_IMPL_RG___(stem) JIT_IMPL_1(stem, jit_reloc_t, gpr)
#define JIT_IMPL_RGi__(stem) JIT_IMPL_2(stem, jit_reloc_t, gpr, imm)
#define JIT_IMPL_RGu__(stem) JIT_IMPL_2(stem, jit_reloc_t, gpr, uimm)
#define JIT_IMPL_R____(stem) JIT_IMPL_0(stem, jit_reloc_t)
#define JIT_IMPL__FFF_(stem) JIT_IMPL_3(stem, void, fpr, fpr, fpr)
#define JIT_IMPL__FF__(stem) JIT_IMPL_2(stem, void, fpr, fpr)
#define JIT_IMPL__FGG_(stem) JIT_IMPL_3(stem, void, fpr, gpr, gpr)
#define JIT_IMPL__FG__(stem) JIT_IMPL_2(stem, void, fpr, gpr)
#define JIT_IMPL__FGo_(stem) JIT_IMPL_3(stem, void, fpr, gpr, off)
#define JIT_IMPL__F___(stem) JIT_IMPL_1(stem, void, fpr)
#define JIT_IMPL__Fd__(stem) JIT_IMPL_2(stem, void, fpr, float64)
#define JIT_IMPL__Ff__(stem) JIT_IMPL_2(stem, void, fpr, float32)
#define JIT_IMPL__Fp__(stem) JIT_IMPL_2(stem, void, fpr, pointer)
#define JIT_IMPL__GF__(stem) JIT_IMPL_2(stem, void, gpr, fpr)
#define JIT_IMPL__GGF_(stem) JIT_IMPL_3(stem, void, gpr, gpr, fpr)
#define JIT_IMPL__GGGG(stem) JIT_IMPL_4(stem, void, gpr, gpr, gpr, gpr)
#define JIT_IMPL__GGG_(stem) JIT_IMPL_3(stem, void, gpr, gpr, gpr)
#define JIT_IMPL__GGGi(stem) JIT_IMPL_4(stem, void, gpr, gpr, gpr, imm)
#define JIT_IMPL__GGGu(stem) JIT_IMPL_4(stem, void, gpr, gpr, gpr, uimm)
#define JIT_IMPL__GG__(stem) JIT_IMPL_2(stem, void, gpr, gpr)
#define JIT_IMPL__GGi_(stem) JIT_IMPL_3(stem, void, gpr, gpr, imm)
#define JIT_IMPL__GGo_(stem) JIT_IMPL_3(stem, void, gpr, gpr, off)
#define JIT_IMPL__GGu_(stem) JIT_IMPL_3(stem, void, gpr, gpr, uimm)
#define JIT_IMPL__G___(stem) JIT_IMPL_1(stem, void, gpr)
#define JIT_IMPL__Gi__(stem) JIT_IMPL_2(stem, void, gpr, imm)
#define JIT_IMPL__Gp__(stem) JIT_IMPL_2(stem, void, gpr, pointer)
#define JIT_IMPL______(stem) JIT_IMPL_0(stem, void)
#define JIT_IMPL__i___(stem) JIT_IMPL_1(stem, void, imm)
#define JIT_IMPL__oGF_(stem) JIT_IMPL_3(stem, void, off, gpr, fpr)
#define JIT_IMPL__oGG_(stem) JIT_IMPL_3(stem, void, off, gpr, gpr)
#define JIT_IMPL__pF__(stem) JIT_IMPL_2(stem, void, pointer, fpr)
#define JIT_IMPL__pG__(stem) JIT_IMPL_2(stem, void, pointer, gpr)
#define JIT_IMPL__p___(stem) JIT_IMPL_1(stem, void, pointer)

#define unwrap_gpr(r) jit_gpr_regno(r)
#define unwrap_fpr(r) jit_fpr_regno(r)
#define unwrap_imm(i) i
#define unwrap_uimm(u) u
#define unwrap_off(o) o
#define unwrap_pointer(p) ((uintptr_t) p)
#define unwrap_float32(f) f
#define unwrap_float64(d) d

#define IMPL_INSTRUCTION(kind, stem) JIT_IMPL_##kind(stem)
FOR_EACH_INSTRUCTION(IMPL_INSTRUCTION)
#undef IMPL_INSTRUCTION

static void
abi_imm_to_gpr(jit_state_t *_jit, enum jit_operand_abi abi, jit_gpr_t dst,
               intptr_t imm)
{
  switch (abi) {
  case JIT_OPERAND_ABI_UINT8:
    ASSERT(0 <= imm);
    ASSERT(imm <= UINT8_MAX);
    break;
  case JIT_OPERAND_ABI_INT8:
    ASSERT(INT8_MIN <= imm);
    ASSERT(imm <= INT8_MAX);
    break;
  case JIT_OPERAND_ABI_UINT16:
    ASSERT(0 <= imm);
    ASSERT(imm <= UINT16_MAX);
    break;
  case JIT_OPERAND_ABI_INT16:
    ASSERT(INT16_MIN <= imm);
    ASSERT(imm <= INT16_MAX);
    break;
  case JIT_OPERAND_ABI_UINT32:
    ASSERT(0 <= imm);
    ASSERT(imm <= UINT32_MAX);
    break;
  case JIT_OPERAND_ABI_INT32:
    ASSERT(INT32_MIN <= imm);
    ASSERT(imm <= INT32_MAX);
    break;
#if __WORDSIZE > 32
  case JIT_OPERAND_ABI_UINT64:
  case JIT_OPERAND_ABI_INT64:
    break;
#endif
  case JIT_OPERAND_ABI_POINTER:
    break;
  default:
    abort();
  }
  jit_movi (_jit, dst, imm);
}

static void
abi_gpr_to_mem(jit_state_t *_jit, enum jit_operand_abi abi,
               jit_gpr_t base, ptrdiff_t offset, jit_gpr_t src)
{
  switch (abi) {
  case JIT_OPERAND_ABI_UINT8:
  case JIT_OPERAND_ABI_INT8:
    jit_stxi_c(_jit, offset, base, src);
    break;
  case JIT_OPERAND_ABI_UINT16:
  case JIT_OPERAND_ABI_INT16:
    jit_stxi_s(_jit, offset, base, src);
    break;
  case JIT_OPERAND_ABI_UINT32:
  case JIT_OPERAND_ABI_INT32:
#if __WORDSIZE == 32
  case JIT_OPERAND_ABI_POINTER:
#endif
    jit_stxi_i(_jit, offset, base, src);
    break;
#if __WORDSIZE == 64
  case JIT_OPERAND_ABI_UINT64:
  case JIT_OPERAND_ABI_INT64:
  case JIT_OPERAND_ABI_POINTER:
    jit_stxi_l(_jit, offset, base, src);
    break;
#endif
  default:
    abort();
  }
}

static void
abi_fpr_to_mem(jit_state_t *_jit, enum jit_operand_abi abi,
               jit_gpr_t base, ptrdiff_t offset, jit_fpr_t src)
{
  switch (abi) {
  case JIT_OPERAND_ABI_FLOAT:
    jit_stxi_f(_jit, offset, base, src);
    break;
  case JIT_OPERAND_ABI_DOUBLE:
    jit_stxi_d(_jit, offset, base, src);
    break;
  default:
    abort();
  }
}

static void
abi_mem_to_gpr(jit_state_t *_jit, enum jit_operand_abi abi,
               jit_gpr_t dst, jit_gpr_t base, ptrdiff_t offset)
{
  switch (abi) {
  case JIT_OPERAND_ABI_UINT8:
    jit_ldxi_uc(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_INT8:
    jit_ldxi_c(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_UINT16:
    jit_ldxi_us(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_INT16:
    jit_ldxi_s(_jit, dst, base, offset);
    break;
#if __WORDSIZE == 32
  case JIT_OPERAND_ABI_UINT32:
  case JIT_OPERAND_ABI_POINTER:
#endif
  case JIT_OPERAND_ABI_INT32:
    jit_ldxi_i(_jit, dst, base, offset);
    break;
#if __WORDSIZE == 64
  case JIT_OPERAND_ABI_UINT32:
    jit_ldxi_ui(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_UINT64:
  case JIT_OPERAND_ABI_POINTER:
  case JIT_OPERAND_ABI_INT64:
    jit_ldxi_l(_jit, dst, base, offset);
    break;
#endif
  default:
    abort();
  }
}

static void
abi_mem_to_fpr(jit_state_t *_jit, enum jit_operand_abi abi,
               jit_fpr_t dst, jit_gpr_t base, ptrdiff_t offset)
{
  switch (abi) {
  case JIT_OPERAND_ABI_FLOAT:
    jit_ldxi_f(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_DOUBLE:
    jit_ldxi_d(_jit, dst, base, offset);
    break;
  default:
    abort();
  }
}

static void
abi_imm_to_mem(jit_state_t *_jit, enum jit_operand_abi abi, jit_gpr_t base,
               ptrdiff_t offset, jit_imm_t imm)
{
  ASSERT(!is_fpr_arg(abi));

  jit_gpr_t tmp = get_temp_gpr(_jit);
  abi_imm_to_gpr(_jit, abi, tmp, imm);
  abi_gpr_to_mem(_jit, abi, base, offset, tmp);
  unget_temp_gpr(_jit);
}

static void
abi_mem_to_mem(jit_state_t *_jit, enum jit_operand_abi abi, jit_gpr_t base,
               ptrdiff_t offset, jit_gpr_t src_base, ptrdiff_t src_offset)
{
  if (is_gpr_arg (abi)) {
    jit_gpr_t tmp = get_temp_gpr(_jit);
    abi_mem_to_gpr(_jit, abi, tmp, src_base, src_offset);
    abi_gpr_to_mem(_jit, abi, base, offset, tmp);
    unget_temp_gpr(_jit);
  } else {
    jit_fpr_t tmp = get_temp_xpr(_jit);
    abi_mem_to_fpr(_jit, abi, tmp, src_base, src_offset);
    abi_fpr_to_mem(_jit, abi, base, offset, tmp);
    unget_temp_xpr(_jit);
  }
}

#define MOVE_KIND(a, b) ((((int) a) << 4) | ((int) b))

#define MOVE_KIND_ENUM(a, b) \
  MOVE_##a##_TO_##b = MOVE_KIND(JIT_OPERAND_KIND_##a, JIT_OPERAND_KIND_##b)
enum move_kind {
  MOVE_KIND_ENUM(IMM, GPR),
  MOVE_KIND_ENUM(GPR, GPR),
  MOVE_KIND_ENUM(MEM, GPR),
  MOVE_KIND_ENUM(FPR, FPR),
  MOVE_KIND_ENUM(MEM, FPR),
  MOVE_KIND_ENUM(IMM, MEM),
  MOVE_KIND_ENUM(GPR, MEM),
  MOVE_KIND_ENUM(FPR, MEM),
  MOVE_KIND_ENUM(MEM, MEM)
};
#undef MOVE_KIND_ENUM

static void
move_operand(jit_state_t *_jit, jit_operand_t dst, jit_operand_t src)
{
  switch (MOVE_KIND (src.kind, dst.kind)) {
  case MOVE_IMM_TO_GPR:
    return abi_imm_to_gpr(_jit, src.abi, dst.loc.gpr.gpr, src.loc.imm);

  case MOVE_GPR_TO_GPR:
    return jit_movr(_jit, dst.loc.gpr.gpr, src.loc.gpr.gpr);

  case MOVE_MEM_TO_GPR:
    return abi_mem_to_gpr(_jit, src.abi, dst.loc.gpr.gpr, src.loc.mem.base,
                          src.loc.mem.offset);

  case MOVE_FPR_TO_FPR:
    return jit_movr_d(_jit, dst.loc.fpr, src.loc.fpr);

  case MOVE_MEM_TO_FPR:
    return abi_mem_to_fpr(_jit, src.abi, dst.loc.fpr, src.loc.mem.base,
                          src.loc.mem.offset);

  case MOVE_IMM_TO_MEM:
    return abi_imm_to_mem(_jit, src.abi, dst.loc.mem.base, dst.loc.mem.offset,
                          src.loc.imm);

  case MOVE_GPR_TO_MEM:
    return abi_gpr_to_mem(_jit, src.abi, dst.loc.mem.base, dst.loc.mem.offset,
                          src.loc.gpr.gpr);

  case MOVE_FPR_TO_MEM:
    return abi_fpr_to_mem(_jit, src.abi, dst.loc.mem.base, dst.loc.mem.offset,
                          src.loc.fpr);

  case MOVE_MEM_TO_MEM:
    return abi_mem_to_mem(_jit, src.abi, dst.loc.mem.base, dst.loc.mem.offset,
                          src.loc.mem.base, src.loc.mem.offset);

  default:
    abort();
  }
}

// A direct transliteration of "Tilting at windmills with Coq: formal
// verification of a compilation algorithm for parallel moves" by
// Laurence Rideau, Bernard Paul Serpette, and Xavier Leroy:
// https://xavierleroy.org/publi/parallel-move.pdf

enum move_status { TO_MOVE, BEING_MOVED, MOVED };

static inline int
already_in_place(jit_operand_t src, jit_operand_t dst)
{
  switch (MOVE_KIND(src.kind, dst.kind)) {
  case MOVE_GPR_TO_GPR:
    return jit_same_gprs (src.loc.gpr.gpr, dst.loc.gpr.gpr);
  case MOVE_FPR_TO_FPR:
    return jit_same_fprs (src.loc.fpr, dst.loc.fpr);
  case MOVE_MEM_TO_MEM:
    return jit_same_gprs (src.loc.mem.base, dst.loc.mem.base) &&
      src.loc.mem.offset == dst.loc.mem.offset;
  default:
    return 0;
  }
}

static inline int
write_would_clobber(jit_operand_t src, jit_operand_t dst)
{
  if (already_in_place (src, dst))
    return 1;

  if (MOVE_KIND(src.kind, dst.kind) == MOVE_MEM_TO_GPR)
    return jit_same_gprs(src.loc.mem.base, dst.loc.gpr.gpr);

  return 0;
}

static inline ptrdiff_t
operand_addend(jit_operand_t op)
{
  switch (op.kind) {
  case JIT_OPERAND_KIND_GPR:
    return op.loc.gpr.addend;
  case JIT_OPERAND_KIND_MEM:
    return op.loc.mem.addend;
  default:
    abort();
  }
}

static void
move_one(jit_state_t *_jit, jit_operand_t *dst, jit_operand_t *src,
         size_t argc, enum move_status *status, size_t i)
{
  int tmp_gpr = 0, tmp_fpr = 0;

  if (already_in_place(src[i], dst[i]))
    return;

  status[i] = BEING_MOVED;
  for (size_t j = 0; j < argc; j++) {
    if (write_would_clobber(src[j], dst[i])) {
      switch (status[j]) {
      case TO_MOVE:
        move_one(_jit, dst, src, argc, status, j);
        break;
      case BEING_MOVED: {
        jit_operand_t tmp;
        if (is_fpr_arg (src[j].kind)) {
          tmp_fpr = 1;
          tmp = jit_operand_fpr(src[j].abi, get_temp_xpr(_jit));
        } else {
          tmp_gpr = 1;
          /* Preserve addend, if any, from source operand, to be applied
             at the end.  */
          tmp = jit_operand_gpr_with_addend(src[j].abi, get_temp_gpr(_jit),
                                            operand_addend(src[j]));
        }
        move_operand (_jit, tmp, src[j]);
        src[j] = tmp;
        break;
      }
      case MOVED:
        break;
      default:
        abort ();
      }
    }
  }

  move_operand (_jit, dst[i], src[i]);
  status[i] = MOVED;
  if (tmp_gpr)
    unget_temp_gpr(_jit);
  else if (tmp_fpr)
    unget_temp_xpr(_jit);
}

static void
apply_addend(jit_state_t *_jit, jit_operand_t dst, jit_operand_t src)
{
  switch (MOVE_KIND(src.kind, dst.kind)) {
  case MOVE_GPR_TO_GPR:
  case MOVE_MEM_TO_GPR:
    if (operand_addend(src))
      jit_addi(_jit, dst.loc.gpr.gpr, dst.loc.gpr.gpr, operand_addend(src));
    break;
  case MOVE_GPR_TO_MEM:
  case MOVE_MEM_TO_MEM:
    if (operand_addend(src)) {
      jit_gpr_t tmp = get_temp_gpr(_jit);
      abi_mem_to_gpr(_jit, dst.abi, tmp, dst.loc.mem.base, dst.loc.mem.offset);
      jit_addi(_jit, tmp, tmp, operand_addend(src));
      abi_gpr_to_mem(_jit, dst.abi, dst.loc.mem.base, dst.loc.mem.offset, tmp);
      unget_temp_gpr(_jit);
    }
    break;
  default:
    break;
  }
}

/* Preconditions: No dest operand is IMM.  No dest operand aliases
   another dest operand.  No dest MEM operand uses a base register which
   is used as a dest GPR.  No dst operand has an addend.  The registers
   returned by get_temp_gpr and get_temp_fpr do not appear in source or
   dest args.  */
void
jit_move_operands(jit_state_t *_jit, jit_operand_t *dst, jit_operand_t *src,
                  size_t argc)
{
  // Check preconditions, except the condition about tmp registers.
  {
    uint64_t src_gprs = 0;
    uint64_t dst_gprs = 0;
    uint64_t dst_fprs = 0;
    uint64_t dst_mem_base_gprs = 0;
    for (size_t i = 0; i < argc; i++) {
      switch (src[i].kind) {
      case JIT_OPERAND_KIND_GPR:
        src_gprs |= 1ULL << jit_gpr_regno(src[i].loc.gpr.gpr);
        break;
      case JIT_OPERAND_KIND_FPR:
      case JIT_OPERAND_KIND_IMM:
      case JIT_OPERAND_KIND_MEM:
        break;
      default:
        abort();
      }
      switch (dst[i].kind) {
      case JIT_OPERAND_KIND_GPR: {
        ASSERT(dst[i].loc.gpr.addend == 0);
        uint64_t bit = 1ULL << jit_gpr_regno(dst[i].loc.gpr.gpr);
        ASSERT((dst_gprs & bit) == 0);
        dst_gprs |= bit;
        break;
      }
      case JIT_OPERAND_KIND_FPR: {
        uint64_t bit = 1ULL << jit_fpr_regno(dst[i].loc.fpr);
        ASSERT((dst_fprs & bit) == 0);
        dst_fprs |= bit;
        break;
      }
      case JIT_OPERAND_KIND_MEM: {
        ASSERT(dst[i].loc.mem.addend == 0);
        uint64_t bit = 1ULL << jit_gpr_regno(dst[i].loc.mem.base);
        dst_mem_base_gprs |= bit;
        break;
      }
      case JIT_OPERAND_KIND_IMM:
      default:
        abort();
        break;
      }
    }
    ASSERT(((src_gprs | dst_gprs) & dst_mem_base_gprs) == 0);
  }

  enum move_status status[argc];
  for (size_t i = 0; i < argc; i++)
    status[i] = TO_MOVE;
  for (size_t i = 0; i < argc; i++)
    if (status[i] == TO_MOVE)
      move_one(_jit, dst, src, argc, status, i);

  // Apply addends at the end.  We could do it earlier in some cases but
  // at least at the end we know that an in-place increment of one
  // operand won't alias another.
  for (size_t i = 0; i < argc; i++)
    apply_addend(_jit, dst[i], src[i]);
}

size_t
jit_align_stack(jit_state_t *_jit, size_t expand)
{
  size_t new_size = _jit->frame_size + expand;
  // Align stack to double-word boundaries.  This isn't really a
  // principle but it does work for Aarch32, AArch64 and x86-64.
  size_t alignment = jit_stack_alignment ();
  size_t aligned_size = (new_size + alignment - 1) & ~(alignment - 1);
  size_t diff = aligned_size - _jit->frame_size;
  if (diff)
    jit_subi (_jit, JIT_SP, JIT_SP, diff);
  _jit->frame_size = aligned_size;
  return diff;
}

void
jit_shrink_stack(jit_state_t *_jit, size_t diff)
{
  if (diff)
    jit_addi (_jit, JIT_SP, JIT_SP, diff);
  _jit->frame_size -= diff;
}

static const jit_gpr_t V[] = {
#ifdef JIT_VTMP
  JIT_VTMP ,
#endif
  JIT_V0, JIT_V1, JIT_V2
#ifdef JIT_V3
  , JIT_V3
#endif
#ifdef JIT_V4
  , JIT_V4
#endif
#ifdef JIT_V5
  , JIT_V5
#endif
#ifdef JIT_V6
  , JIT_V6
#endif
#ifdef JIT_V7
  , JIT_V7
#endif
 };

static const jit_fpr_t VF[] = {
#ifdef JIT_VFTMP
  JIT_VFTMP ,
#endif
#ifdef JIT_VF0
  JIT_VF0
#endif
#ifdef JIT_VF1
  , JIT_VF1
#endif
#ifdef JIT_VF2
  , JIT_VF2
#endif
#ifdef JIT_VF3
  , JIT_VF3
#endif
#ifdef JIT_VF4
  , JIT_VF4
#endif
#ifdef JIT_VF5
  , JIT_VF5
#endif
#ifdef JIT_VF6
  , JIT_VF6
#endif
#ifdef JIT_VF7
  , JIT_VF7
#endif
};

static const size_t v_count = sizeof(V) / sizeof(V[0]);
static const size_t vf_count = sizeof(VF) / sizeof(VF[0]);

size_t
jit_enter_jit_abi(jit_state_t *_jit, size_t v, size_t vf, size_t frame_size)
{
#ifdef JIT_VTMP
  v++;
#endif
#ifdef JIT_VFTMP
  vf++;
#endif

  ASSERT(v <= v_count);
  ASSERT(vf <= vf_count);

  ASSERT(_jit->frame_size == 0);
  _jit->frame_size = jit_initial_frame_size();

  /* Save values of callee-save registers.  */
  for (size_t i = 0; i < v; i++)
    jit_pushr (_jit, V[i]);
  for (size_t i = 0; i < vf; i++)
    jit_pushr_d (_jit, VF[i]);

  return jit_align_stack(_jit, frame_size);
}

void
jit_leave_jit_abi(jit_state_t *_jit, size_t v, size_t vf, size_t frame_size)
{
#ifdef JIT_VTMP
  v++;
#endif
#ifdef JIT_VFTMP
  vf++;
#endif

  jit_shrink_stack(_jit, frame_size);

  /* Restore callee-save registers.  */
  for (size_t i = 0; i < vf; i++)
    jit_popr_d (_jit, VF[vf - i - 1]);

  for (size_t i = 0; i < v; i++)
    jit_popr (_jit, V[v - i - 1]);
}


// Precondition: stack is already aligned.
static size_t
prepare_call_args(jit_state_t *_jit, size_t argc, jit_operand_t args[])
{
  jit_operand_t dst[argc];
  struct abi_arg_iterator iter;
  
  // Compute shuffle destinations and space for spilled arguments.
  reset_abi_arg_iterator(&iter, argc, args);
  for (size_t i = 0; i < argc; i++)
    next_abi_arg(&iter, &dst[i]);

  // Reserve space for spilled arguments and ensure stack alignment.
  size_t stack_size = jit_align_stack(_jit, iter.stack_size);

  // Fix up SP-relative operands.
  for (size_t i = 0; i < argc; i++) {
    switch(args[i].kind) {
    case JIT_OPERAND_KIND_GPR:
      if (jit_same_gprs (args[i].loc.mem.base, JIT_SP))
        args[i].loc.gpr.addend += stack_size;
      break;
    case JIT_OPERAND_KIND_MEM:
      if (jit_same_gprs (args[i].loc.mem.base, JIT_SP))
        args[i].loc.mem.offset += stack_size;
      break;
    default:
      break;
    }
  }

  jit_move_operands(_jit, dst, args, argc);

  return stack_size;
}

void
jit_calli(jit_state_t *_jit, jit_pointer_t f, size_t argc, jit_operand_t args[])
{
  size_t stack_bytes = prepare_call_args(_jit, argc, args);

  calli(_jit, (jit_word_t)f);

  jit_shrink_stack(_jit, stack_bytes);
}

void
jit_callr(jit_state_t *_jit, jit_gpr_t f, size_t argc, jit_operand_t args[])
{
  size_t stack_bytes = prepare_call_args(_jit, argc, args);

  callr(_jit, jit_gpr_regno(f));

  jit_shrink_stack(_jit, stack_bytes);
}

void
jit_locate_args(jit_state_t *_jit, size_t argc, jit_operand_t args[])
{
  struct abi_arg_iterator iter;
    
  reset_abi_arg_iterator(&iter, argc, args);
  iter.stack_size += _jit->frame_size;
  for (size_t i = 0; i < argc; i++)
    next_abi_arg(&iter, &args[i]);
}

/* Precondition: args are distinct locations of type GPR or FPR.  All
   addends of arg operands are zero.  No GPR arg is SP.  */
void
jit_load_args(jit_state_t *_jit, size_t argc, jit_operand_t args[])
{
  jit_operand_t src[argc];

  memcpy(src, args, sizeof(src[0]) * argc);

  jit_locate_args(_jit, argc, src);
  jit_move_operands(_jit, args, src, argc);
}
