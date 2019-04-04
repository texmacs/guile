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

#define rc(value)               jit_class_##value
#define rn(reg)                 (jit_regno(_rvs[jit_regno(reg.bits)].spec))

#if defined(__i386__) || defined(__x86_64__)
# define JIT_SP         JIT_GPR(_RSP)
# define JIT_RET                JIT_GPR(_RAX)
# if __X32
#  define JIT_FRET              JIT_FPR(_ST0)
# else
#  if __CYGWIN__
#   define JIT_RA0              JIT_GPR(_RCX)
#  else
#   define JIT_RA0              JIT_GPR(_RDI)
#  endif
#  define JIT_FA0               JIT_FPR(_XMM0)
#  define JIT_FRET              JIT_FPR(_XMM0)
# endif
#elif defined(__mips__)
# define JIT_RA0                JIT_GPR(_A0)
# define JIT_FA0                JIT_FPR(_F12)
# define JIT_SP         JIT_GPR(_SP)
# define JIT_RET                JIT_GPR(_V0)
# define JIT_FRET               JIT_FPR(_F0)
#elif defined(__arm__)
# define JIT_RA0                JIT_GPR(_R0)
# define JIT_FA0                JIT_FPR(_D0)
# define JIT_SP         JIT_GPR(_R13)
# define JIT_RET                JIT_GPR(_R0)
# if defined(__ARM_PCS_VFP)
#  define JIT_FRET              JIT_FPR(_D0)
# else
#  define JIT_FRET              JIT_FPR(_R0)
# endif
#elif defined(__ppc__) || defined(__powerpc__)
# define JIT_RA0                JIT_GPR(_R3)
# define JIT_FA0                JIT_FPR(_F1)
# define JIT_SP         JIT_GPR(_R1)
# define JIT_RET                JIT_GPR(_R3)
# define JIT_FRET               JIT_FPR(_F1)
#elif defined(__sparc__)
# define JIT_SP         JIT_GPR(_SP)
# define JIT_RET                JIT_GPR(_I0)
# define JIT_FRET               JIT_FPR(_F0)
#elif defined(__ia64__)
# define JIT_SP         JIT_GPR(_R12)
# define JIT_RET                JIT_GPR(_R8)
# define JIT_FRET               JIT_FPR(_F8)
#elif defined(__hppa__)
# define JIT_SP         JIT_GPR(_R30)
# define JIT_RET                JIT_GPR(_R28)
# define JIT_FRET               JIT_FPR(_F4)
#elif defined(__aarch64__)
# define JIT_RA0                JIT_GPR(_R0)
# define JIT_FA0                JIT_FPR(_V0)
# define JIT_SP         JIT_GPR(_SP)
# define JIT_RET                JIT_GPR(_R0)
# define JIT_FRET               JIT_FPR(_V0)
#elif defined(__s390__) || defined(__s390x__)
# define JIT_SP         JIT_GPR(_R15)
# define JIT_RET                JIT_GPR(_R2)
# define JIT_FRET               JIT_FPR(_F0)
#elif defined(__alpha__)
# define JIT_SP         JIT_GPR(_SP)
# define JIT_RET                JIT_GPR(_V0)
# define JIT_FRET               JIT_FPR(_F0)
#endif

/*
 * Private jit_class bitmasks
 */
#define jit_class_named         0x00400000      /* hit must be the named reg */
#define jit_class_nospill       0x00800000      /* hint to fail if need spill */
#define jit_class_sft           0x01000000      /* not a hardware register */
#define jit_class_rg8           0x04000000      /* x86 8 bits */
#define jit_class_xpr           0x80000000      /* float / vector */
/* Used on sparc64 where %f0-%f31 can be encode for single float
 * but %f32 to %f62 only as double precision */
#define jit_class_sng           0x10000000      /* Single precision float */
#define jit_class_dbl           0x20000000      /* Only double precision float */
#define jit_regno_patch         0x00008000      /* this is a register
                                                 * returned by a "user" call
                                                 * to jit_get_reg() */

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
  void* (*alloc)(size_t);
  void (*free)(void*);
};

enum jit_reloc_flags
{
  JIT_RELOC_CAN_SHORTEN = 1<<0
};

struct jit_register
{
  jit_reg_t spec;
  char *name;
};

typedef struct jit_register jit_register_t;

static const jit_register_t _rvs[];

#define jit_regload_reload              0       /* convert to reload */
#define jit_regload_delete              1       /* just remove node */
#define jit_regload_isdead              2       /* delete and unset live bit */

#define ASSERT(x) do { if (!(x)) abort(); } while (0)
#if defined(__GNUC__)
# define UNLIKELY(exprn) __builtin_expect(exprn, 0)
#else
# define UNLIKELY(exprn) exprn
#endif

static jit_bool_t jit_get_cpu(void);
static jit_bool_t jit_init(jit_state_t *);
static void jit_flush(void *fptr, void *tptr);
static void jit_try_shorten(jit_state_t *_jit, jit_reloc_t reloc);

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

  if (!jit_init (_jit));

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
           uint8_t inst_start_offset, uint16_t flags)
{
  jit_reloc_t ret;

  ret.kind = kind;
  ret.inst_start_offset = inst_start_offset;
  ret.flags = 0;
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
  loc.uc = _jit->start + reloc.offset;
  ptrdiff_t diff;

  switch (reloc.kind)
    {
    case JIT_RELOC_ABSOLUTE:
      if (sizeof(diff) == 4)
        *loc.ui = (uintptr_t)addr;
      else
        *loc.ul = (uintptr_t)addr;
      if (loc.uc + sizeof(diff) == _jit->pc.uc &&
          (reloc.flags & JIT_RELOC_CAN_SHORTEN))
        jit_try_shorten (_jit, reloc);
      break;
    case JIT_RELOC_REL8:
      diff = ((uint8_t*)addr) - (loc.uc + 1);
      ASSERT (INT8_MIN <= diff && diff <= INT8_MAX);
      *loc.uc = diff;
      break;
    case JIT_RELOC_REL16:
      diff = ((uint8_t*)addr) - (loc.uc + 2);
      ASSERT (INT16_MIN <= diff && diff <= INT16_MAX);
      *loc.us = diff;
      if ((loc.uc + 1) == _jit->pc.uc && (reloc.flags & JIT_RELOC_CAN_SHORTEN))
        jit_try_shorten (_jit, reloc);
      break;
    case JIT_RELOC_REL32:
      diff = ((uint8_t*)addr) - (loc.uc + 4);
      ASSERT (INT32_MIN <= diff && diff <= INT32_MAX);
      *loc.ui = diff;
      if ((loc.ui + 1) == _jit->pc.ui && (reloc.flags & JIT_RELOC_CAN_SHORTEN))
        jit_try_shorten (_jit, reloc);
      break;
    case JIT_RELOC_REL64:
      diff = ((uint8_t*)addr) - (loc.uc + 8);
      *loc.ul = diff;
      if ((loc.ul + 1) == _jit->pc.ul && (reloc.flags & JIT_RELOC_CAN_SHORTEN))
        jit_try_shorten (_jit, reloc);
      break;
    default:
      abort ();
    }
}

jit_bool_t
jit_gpr_is_callee_save (jit_state_t *_jit, jit_gpr_t reg)
{
  return jit_class(_rvs[jit_regno(reg.bits)].spec) & jit_class_sav;
}

jit_bool_t
jit_fpr_is_callee_save (jit_state_t *_jit, jit_fpr_t reg)
{
  return jit_class(_rvs[jit_regno(reg.bits)].spec) & jit_class_sav;
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
#elif defined(__ia64__)
# include "ia64.c"
#elif defined(__hppa__)
# include "hppa.c"
#elif defined(__aarch64__)
# include "aarch64.c"
#elif defined(__s390__) || defined(__s390x__)
# include "s390.c"
#elif defined(__alpha__)
# include "alpha.c"
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

#define unwrap_gpr(r) rn(r)
#define unwrap_fpr(r) rn(r)
#define unwrap_imm(i) i
#define unwrap_uimm(u) u
#define unwrap_off(o) o
#define unwrap_pointer(p) ((uintptr_t) p)
#define unwrap_float32(f) f
#define unwrap_float64(d) d

#define IMPL_INSTRUCTION(kind, stem) JIT_IMPL_##kind(stem)
FOR_EACH_INSTRUCTION(IMPL_INSTRUCTION)
#undef IMPL_INSTRUCTION
