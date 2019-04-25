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

#ifndef _jit_x86_h
#define _jit_x86_h

#define JIT_HASH_CONSTS         1
#define JIT_NUM_OPERANDS        2

/*
 * Types
 */
#define jit_sse2_p()            jit_cpu.sse2
#define jit_x87_reg_p(reg)      ((reg) >= _ST0 && (reg) <= _ST6)
#if __WORDSIZE == 32
# if defined(__x86_64__)
#  define __X64    1
#  define __X64_32 1
#  define __X32    0
# else
#  define __X64    0
#  define __X64_32 0
#  define __X32    1
# endif
#else
#  define __X64    1
#  define __X64_32 0
#  define __X32    0
#endif

#if __X32
#  define _RAX     JIT_GPR(0)
#  define _RCX     JIT_GPR(1)
#  define _RDX     JIT_GPR(2)
#  define _RBX     JIT_GPR(3 | jit_class_sav)
#  define _RSP     JIT_GPR(4 | jit_class_sav)
#  define _RBP     JIT_GPR(5 | jit_class_sav)
#  define _RSI     JIT_GPR(6 | jit_class_sav)
#  define _RDI     JIT_GPR(7 | jit_class_sav)
#  define _XMM0    JIT_FPR(0)
#  define _XMM1    JIT_FPR(1)
#  define _XMM2    JIT_FPR(2)
#  define _XMM3    JIT_FPR(3)
#  define _XMM4    JIT_FPR(4)
#  define _XMM5    JIT_FPR(5)
#  define _XMM6    JIT_FPR(6)
#  define _XMM7    JIT_FPR(7)
#elif __CYGWIN__
#  define _RAX     JIT_GPR(0)
#  define _RCX     JIT_GPR(1)
#  define _RDX     JIT_GPR(2)
#  define _RBX     JIT_GPR(3 | jit_class_sav)
#  define _RSP     JIT_GPR(4 | jit_class_sav)
#  define _RBP     JIT_GPR(5 | jit_class_sav)
#  define _RSI     JIT_GPR(6 | jit_class_sav)
#  define _RDI     JIT_GPR(7 | jit_class_sav)
#  define _R8      JIT_GPR(8)
#  define _R9      JIT_GPR(9)
#  define _R10     JIT_GPR(10)
#  define _R11     JIT_GPR(11)
#  define _R12     JIT_GPR(12 | jit_class_sav)
#  define _R13     JIT_GPR(13 | jit_class_sav)
#  define _R14     JIT_GPR(14 | jit_class_sav)
#  define _R15     JIT_GPR(15 | jit_class_sav)
#  define _XMM0    JIT_FPR(0)
#  define _XMM1    JIT_FPR(1)
#  define _XMM2    JIT_FPR(2)
#  define _XMM3    JIT_FPR(3)
#  define _XMM4    JIT_FPR(4)
#  define _XMM5    JIT_FPR(5)
#  define _XMM6    JIT_FPR(6 | jit_class_sav)
#  define _XMM7    JIT_FPR(7 | jit_class_sav)
#  define _XMM8    JIT_FPR(8 | jit_class_sav)
#  define _XMM9    JIT_FPR(9 | jit_class_sav)
#  define _XMM10   JIT_FPR(10 | jit_class_sav)
#  define _XMM11   JIT_FPR(11 | jit_class_sav)
#  define _XMM12   JIT_FPR(12 | jit_class_sav)
#  define _XMM13   JIT_FPR(13 | jit_class_sav)
#  define _XMM14   JIT_FPR(14 | jit_class_sav)
#  define _XMM15   JIT_FPR(15 | jit_class_sav)
#else
#  define _RAX     JIT_GPR(0)
#  define _RCX     JIT_GPR(1)
#  define _RDX     JIT_GPR(2)
#  define _RBX     JIT_GPR(3 | jit_class_sav)
#  define _RSP     JIT_GPR(4 | jit_class_sav)
#  define _RBP     JIT_GPR(5 | jit_class_sav)
#  define _RSI     JIT_GPR(6)
#  define _RDI     JIT_GPR(7)
#  define _R8      JIT_GPR(8)
#  define _R9      JIT_GPR(9)
#  define _R10     JIT_GPR(10)
#  define _R11     JIT_GPR(11)
#  define _R12     JIT_GPR(12 | jit_class_sav)
#  define _R13     JIT_GPR(13 | jit_class_sav)
#  define _R14     JIT_GPR(14 | jit_class_sav)
#  define _R15     JIT_GPR(15 | jit_class_sav)
#  define _XMM0    JIT_FPR(0)
#  define _XMM1    JIT_FPR(1)
#  define _XMM2    JIT_FPR(2)
#  define _XMM3    JIT_FPR(3)
#  define _XMM4    JIT_FPR(4)
#  define _XMM5    JIT_FPR(5)
#  define _XMM6    JIT_FPR(6)
#  define _XMM7    JIT_FPR(7)
#  define _XMM8    JIT_FPR(8)
#  define _XMM9    JIT_FPR(9)
#  define _XMM10   JIT_FPR(10)
#  define _XMM11   JIT_FPR(11)
#  define _XMM12   JIT_FPR(12)
#  define _XMM13   JIT_FPR(13)
#  define _XMM14   JIT_FPR(14)
#  define _XMM15   JIT_FPR(15)
#endif

#define JIT_SP     _RSP
#define JIT_FP     _RBP
#if __X32
#  define JIT_R0   _RAX
#  define JIT_R1   _RCX
#  define JIT_R2   _RDX
#  define JIT_V0   _RBX
#  define JIT_V1   _RSI
#  define JIT_V2   _RDI
#  define JIT_F0   _XMM0
#  define JIT_F1   _XMM1
#  define JIT_F2   _XMM2
#  define JIT_F3   _XMM3
#  define JIT_F4   _XMM4
#  define JIT_F5   _XMM5
#  define JIT_F6   _XMM6
#  define JIT_F7   _XMM6
#elif __CYGWIN__
#  define JIT_R0   _RAX
#  define JIT_R1   _R10
#  define JIT_R2   _R11
#  define JIT_V0   _RBX
#  define JIT_V1   _RDI
#  define JIT_V2   _RSI
#  define JIT_V3   _R12
#  define JIT_V4   _R13
#  define JIT_V5   _R14
#  define JIT_V6   _R15
#  define JIT_F0   _XMM0
#  define JIT_F1   _XMM1
#  define JIT_F2   _XMM2
#  define JIT_F3   _XMM3
#  define JIT_F4   _XMM4
#  define JIT_F5   _XMM5
#  define JIT_F6   _XMM6
#  define JIT_F7   _XMM7
#  define JIT_F8   _XMM8
#  define JIT_F9   _XMM9
#  define JIT_F10  _XMM10
#  define JIT_F11  _XMM11
#  define JIT_F12  _XMM12
#  define JIT_F13  _XMM13
#  define JIT_F14  _XMM14
#  define JIT_F15  _XMM15
#else
#  define JIT_R0   _RAX
#  define JIT_R1   _R10
#  define JIT_R2   _R11
#  define JIT_R3   _R12
#  define JIT_V0   _RBX
#  define JIT_V1   _R13
#  define JIT_V2   _R14
#  define JIT_V3   _R15
#  define JIT_F0   _XMM0
#  define JIT_F1   _XMM1
#  define JIT_F2   _XMM2
#  define JIT_F3   _XMM3
#  define JIT_F4   _XMM4
#  define JIT_F5   _XMM5
#  define JIT_F6   _XMM6
#  define JIT_F7   _XMM7
#  define JIT_F8   _XMM8
#  define JIT_F9   _XMM9
#  define JIT_F10  _XMM10
#  define JIT_F11  _XMM11
#  define JIT_F12  _XMM12
#  define JIT_F13  _XMM13
#  define JIT_F14  _XMM14
#  define JIT_F15  _XMM15
#endif

typedef struct {
  /* x87 present */
  uint32_t fpu                : 1;
  /* cmpxchg8b instruction */
  uint32_t cmpxchg8b  : 1;
  /* cmov and fcmov branchless conditional mov */
  uint32_t cmov               : 1;
  /* mmx registers/instructions available */
  uint32_t mmx                : 1;
  /* sse registers/instructions available */
  uint32_t sse                : 1;
  /* sse2 registers/instructions available */
  uint32_t sse2               : 1;
  /* sse3 instructions available */
  uint32_t sse3               : 1;
  /* pcmulqdq instruction */
  uint32_t pclmulqdq  : 1;
  /* ssse3 suplemental sse3 instructions available */
  uint32_t ssse3              : 1;
  /* fused multiply/add using ymm state */
  uint32_t fma                : 1;
  /* cmpxchg16b instruction */
  uint32_t cmpxchg16b : 1;
  /* sse4.1 instructions available */
  uint32_t sse4_1             : 1;
  /* sse4.2 instructions available */
  uint32_t sse4_2             : 1;
  /* movbe instruction available */
  uint32_t movbe              : 1;
  /* popcnt instruction available */
  uint32_t popcnt             : 1;
  /* aes instructions available */
  uint32_t aes                : 1;
  /* avx instructions available */
  uint32_t avx                : 1;
  /* lahf/sahf available in 64 bits mode */
  uint32_t lahf               : 1;
} jit_cpu_t;

/*
 * Initialization
 */
JIT_API jit_cpu_t               jit_cpu;

#endif /* _jit_x86_h */
