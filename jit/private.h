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

#ifndef _jit_private_h
#define _jit_private_h

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <limits.h>
#include <stdio.h>

#if defined(__GNUC__)
#  define maybe_unused		__attribute__ ((unused))
#  define unlikely(exprn)	__builtin_expect(!!(exprn), 0)
#  define likely(exprn)		__builtin_expect(!!(exprn), 1)
#  if (__GNUC__ >= 4)
#    define PUBLIC		__attribute__ ((visibility("default")))
#    define HIDDEN		__attribute__ ((visibility("hidden")))
#  else
#    define PUBLIC		/**/
#    define HIDDEN		/**/
#  endif
#else
#  define maybe_unused		/**/
#  define unlikely(exprn)	exprn
#  define likely(exprn)		exprn
#  define PUBLIC		/**/
#  define HIDDEN		/**/
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
  uint8_t *end;
};

struct jit_register
{
  jit_reg_t spec;
  char *name;
};

typedef struct jit_register jit_register_t;

static void jit_get_cpu(void);
static void jit_flush(void *fptr, void *tptr);

static jit_register_t _rvs[];

#endif /* _jit_private_h */
