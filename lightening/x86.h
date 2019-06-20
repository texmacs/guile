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

#define _RAX     JIT_GPR(0)
#define _RCX     JIT_GPR(1)
#define _RDX     JIT_GPR(2)
#define _RBX     JIT_GPR(3)
#define _RSP     JIT_GPR(4)
#define _RBP     JIT_GPR(5)
#define _RSI     JIT_GPR(6)
#define _RDI     JIT_GPR(7)

#define _XMM0    JIT_FPR(0)
#define _XMM1    JIT_FPR(1)
#define _XMM2    JIT_FPR(2)
#define _XMM3    JIT_FPR(3)
#define _XMM4    JIT_FPR(4)
#define _XMM5    JIT_FPR(5)
#define _XMM6    JIT_FPR(6)
#define _XMM7    JIT_FPR(7)

#if __X64
#  define _R8      JIT_GPR(8)
#  define _R9      JIT_GPR(9)
#  define _R10     JIT_GPR(10)
#  define _R11     JIT_GPR(11)
#  define _R12     JIT_GPR(12)
#  define _R13     JIT_GPR(13)
#  define _R14     JIT_GPR(14)
#  define _R15     JIT_GPR(15)
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
#define JIT_LR     JIT_TMP0
#if __X32
#  define JIT_R0   _RAX
#  define JIT_R1   _RCX
#  define JIT_R2   _RDX
#  define JIT_V0   _RBP
#  define JIT_V1   _RSI
#  define JIT_V2   _RDI
#  define JIT_TMP0 _RBX
#  define JIT_F0   _XMM0
#  define JIT_F1   _XMM1
#  define JIT_F2   _XMM2
#  define JIT_F3   _XMM3
#  define JIT_F4   _XMM4
#  define JIT_F5   _XMM5
#  define JIT_F6   _XMM6
#  define JIT_FTMP _XMM7
#  define JIT_PLATFORM_CALLEE_SAVE_GPRS JIT_TMP0
#elif __CYGWIN__
#  define JIT_R0   _RAX
#  define JIT_R1   _RCX
#  define JIT_R2   _RDX
#  define JIT_R3   _R8
#  define JIT_R4   _R9
#  define JIT_R5   _R10
#  define JIT_TMP0 _R11
#  define JIT_V0   _RBX
#  define JIT_V1   _RSI
#  define JIT_V2   _RDI
#  define JIT_V3   _R12
#  define JIT_V4   _R13
#  define JIT_V5   _R14
#  define JIT_V6   _R15
#  define JIT_F0   _XMM0
#  define JIT_F1   _XMM1
#  define JIT_F2   _XMM2
#  define JIT_F3   _XMM3
#  define JIT_F4   _XMM4
#  define JIT_FTMP  _XMM5
#  define JIT_VF0  _XMM6
#  define JIT_VF1  _XMM7
#  define JIT_VF2  _XMM8
#  define JIT_VF3  _XMM9
#  define JIT_VF4  _XMM10
#  define JIT_VF5  _XMM11
#  define JIT_VF6  _XMM12
#  define JIT_VF7  _XMM13
#  define JIT_VF8  _XMM14
#  define JIT_VF9  _XMM15
#  define JIT_PLATFORM_CALLEE_SAVE_GPRS /**/
#else
#  define JIT_R0   _RAX
#  define JIT_R1   _RCX
#  define JIT_R2   _RDX
#  define JIT_R3   _RSI
#  define JIT_R4   _RDI
#  define JIT_R5   _R8
#  define JIT_R6   _R9
#  define JIT_R7   _R10
#  define JIT_TMP0 _R11
#  define JIT_V0   _RBX
#  define JIT_V1   _R12
#  define JIT_V2   _R13
#  define JIT_V3   _R14
#  define JIT_V4   _R15
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
#  define JIT_FTMP _XMM15
#  define JIT_PLATFORM_CALLEE_SAVE_GPRS /**/
#endif

#endif /* _jit_x86_h */
