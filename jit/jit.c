/*
 * Copyright (C) 2012-2018  Free Software Foundation, Inc.
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

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <sys/mman.h>

#include "../jit.h"

#if defined(__GNUC__)
#  define maybe_unused		__attribute__ ((unused))
#else
#  define maybe_unused		/**/
#endif

#define rc(value)		jit_class_##value
#define rn(reg)			(jit_regno(_rvs[jit_regno(reg)].spec))

#if defined(__i386__) || defined(__x86_64__)
#  define JIT_SP		_RSP
#  define JIT_RET		_RAX
#  if __X32
#    define JIT_FRET		_ST0
#  else
#    if __CYGWIN__
#      define JIT_RA0		_RCX
#    else
#      define JIT_RA0		_RDI
#    endif
#    define JIT_FA0		_XMM0
#    define JIT_FRET		_XMM0
#  endif
#elif defined(__mips__)
#  define JIT_RA0		_A0
#  define JIT_FA0		_F12
#  define JIT_SP		_SP
#  define JIT_RET		_V0
#  define JIT_FRET		_F0
#elif defined(__arm__)
#  define JIT_RA0		_R0
#  define JIT_FA0		_D0
#  define JIT_SP		_R13
#  define JIT_RET		_R0
#  if defined(__ARM_PCS_VFP)
#    define JIT_FRET		_D0
#  else
#    define JIT_FRET		_R0
#  endif
#elif defined(__ppc__) || defined(__powerpc__)
#  define JIT_RA0		_R3
#  define JIT_FA0		_F1
#  define JIT_SP		_R1
#  define JIT_RET		_R3
#  define JIT_FRET		_F1
#elif defined(__sparc__)
#  define JIT_SP		_SP
#  define JIT_RET		_I0
#  define JIT_FRET		_F0
#elif defined(__ia64__)
#  define JIT_SP		_R12
#  define JIT_RET		_R8
#  define JIT_FRET		_F8
#elif defined(__hppa__)
#  define JIT_SP		_R30
#  define JIT_RET		_R28
#  define JIT_FRET		_F4
#elif defined(__aarch64__)
#  define JIT_RA0		_R0
#  define JIT_FA0		_V0
#  define JIT_SP		_SP
#  define JIT_RET		_R0
#  define JIT_FRET		_V0
#elif defined(__s390__) || defined(__s390x__)
#  define JIT_SP		_R15
#  define JIT_RET		_R2
#  define JIT_FRET		_F0
#elif defined(__alpha__)
#  define JIT_SP		_SP
#  define JIT_RET		_V0
#  define JIT_FRET		_F0
#endif

/*
 * Private jit_class bitmasks
 */
#define jit_class_named		0x00400000	/* hit must be the named reg */
#define jit_class_nospill	0x00800000	/* hint to fail if need spill */
#define jit_class_sft		0x01000000	/* not a hardware register */
#define jit_class_rg8		0x04000000	/* x86 8 bits */
#define jit_class_xpr		0x80000000	/* float / vector */
/* Used on sparc64 where %f0-%f31 can be encode for single float
 * but %f32 to %f62 only as double precision */
#define jit_class_sng		0x10000000	/* Single precision float */
#define jit_class_dbl		0x20000000	/* Only double precision float */
#define jit_regno_patch		0x00008000	/* this is a register
						 * returned by a "user" call
						 * to jit_get_reg() */

struct jit_state
{
  union {
    uint8_t *uc;
    uint16_t *us;
    uint32_t *ui;
    uint64_t *ul;
    intptr_t w;
    uintptr_t uw;
  } pc;
  uint8_t *start;
  uint8_t *last_instruction_start;
  uint8_t *limit;
};

struct jit_register
{
  jit_reg_t spec;
  char *name;
};

typedef struct jit_register jit_register_t;

static jit_register_t _rvs[];

#define jit_regload_reload		0	/* convert to reload */
#define jit_regload_delete		1	/* just remove node */
#define jit_regload_isdead		2	/* delete and unset live bit */

#define ASSERT(x) do { if (!(x)) abort(); } while (0)

static inline uint8_t*
jit_reloc_instruction (jit_reloc_t reloc)
{
  return (uint8_t*) reloc;
}

static void jit_get_cpu(void);
static void jit_init(jit_state_t *);
static void jit_nop(jit_state_t *, unsigned);
static void jit_patch(jit_state_t *, const uint8_t *loc, const uint8_t *addr);
static void jit_patch_last(jit_state_t *, const uint8_t *loc, const uint8_t *addr);
static void jit_flush(void *fptr, void *tptr);

void
init_jit(void)
{
    jit_get_cpu();
}

jit_state_t *
jit_new_state(void)
{
    jit_state_t		*_jit;

    _jit = malloc (sizeof (*_jit));
    if (!_jit)
      abort ();

    memset(_jit, 0, sizeof (*_jit));

    jit_init (_jit);

    return _jit;
}

void
jit_destroy_state(jit_state_t *_jit)
{
  free (_jit);
}

jit_pointer_t
jit_address(jit_state_t *_jit)
{
  /* TODO: FIXME */
  abort ();
}

void
jit_begin(jit_state_t *_jit, jit_addr_t addr, size_t length)
{
  ASSERT (!_jit->start);

  _jit->start = addr;
  _jit->limit = _jit->start + length;
  jit_reset(_jit);
}

void
jit_reset(jit_state_t *_jit)
{
  ASSERT (_jit->start);
  _jit->pc.uc = _jit->start = _jit->limit = NULL;
}

jit_addr_t
jit_end(jit_state_t *_jit, size_t *length)
{
  uint8_t *code = _jit->start;
  uint8_t *end = _jit->pc.uc;

  ASSERT (code);
  ASSERT (end > code);
  ASSERT (end <= _jit->limit);

  jit_flush (code, end);

  if (length) {
    *length = end - code;
  }

  jit_reset (_jit);

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

void
jit_patch_here(jit_state_t *_jit, jit_reloc_t reloc)
{
  jit_patch_there (_jit, reloc, jit_address (_jit));
}

void
jit_patch_there(jit_state_t* _jit, jit_reloc_t reloc, jit_pointer_t addr)
{
  const uint8_t *loc = jit_reloc_instruction (reloc);

  if (loc == _jit->last_instruction_start)
    jit_patch_last (_jit, loc, addr);
  else
    jit_patch (_jit, loc, addr);
}

#if defined(__i386__) || defined(__x86_64__)
#  include "x86.c"
#elif defined(__mips__)
#  include "mips.c"
#elif defined(__arm__)
#  include "arm.c"
#elif defined(__ppc__) || defined(__powerpc__)
#  include "ppc.c"
#elif defined(__sparc__)
#  include "sparc.c"
#elif defined(__ia64__)
#  include "ia64.c"
#elif defined(__hppa__)
#  include "hppa.c"
#elif defined(__aarch64__)
#  include "aarch64.c"
#elif defined(__s390__) || defined(__s390x__)
#  include "s390.c"
#elif defined(__alpha__)
#  include "alpha.c"
#endif

#define JIT_CALL_0(stem) _jit_##stem (_jit)
#define JIT_CALL_1(stem) _jit_##stem (_jit, a)
#define JIT_CALL_2(stem) _jit_##stem (_jit, a, b)
#define JIT_CALL_3(stem) _jit_##stem (_jit, a, b, c)
#define JIT_CALL_4(stem) _jit_##stem (_jit, a, b, c, d)

#define JIT_TAIL_CALL_RFF__(stem) return JIT_CALL_2(stem)
#define JIT_TAIL_CALL_RGG__(stem) return JIT_CALL_2(stem)
#define JIT_TAIL_CALL_RG___(stem) return JIT_CALL_1(stem)
#define JIT_TAIL_CALL_RGi__(stem) return JIT_CALL_2(stem)
#define JIT_TAIL_CALL_RGu__(stem) return JIT_CALL_2(stem)
#define JIT_TAIL_CALL_R____(stem) return JIT_CALL_0(stem)
#define JIT_TAIL_CALL__FFF_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__FF__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__FGG_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__FG__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__FGo_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__F___(stem) JIT_CALL_1(stem)
#define JIT_TAIL_CALL__Fd__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__Ff__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__Fp__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__GF__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__GGF_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__GGGG(stem) JIT_CALL_4(stem)
#define JIT_TAIL_CALL__GGG_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__GGGi(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__GGGu(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__GG__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__GGi_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__GGo_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__GGu_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__G___(stem) JIT_CALL_1(stem)
#define JIT_TAIL_CALL__Gi__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__Gp__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL______(stem) JIT_CALL_0(stem)
#define JIT_TAIL_CALL__i___(stem) JIT_CALL_1(stem)
#define JIT_TAIL_CALL__oGF_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__oGG_(stem) JIT_CALL_3(stem)
#define JIT_TAIL_CALL__pF__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__pG__(stem) JIT_CALL_2(stem)
#define JIT_TAIL_CALL__p___(stem) JIT_CALL_1(stem)

#define DEFINE_INSTRUCTION(kind, stem) \
  JIT_PROTO_##kind(stem)               \
  {                                    \
    JIT_TAIL_CALL_##kind(stem);        \
  }
FOR_EACH_INSTRUCTION(DEFINE_INSTRUCTION)
#undef DEFINE_INSTRUCTION
