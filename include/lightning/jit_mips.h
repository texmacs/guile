/*
 * Copyright (C) 2012, 2013  Free Software Foundation, Inc.
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 */

#ifndef _jit_mips_h
#define _jit_mips_h

#define JIT_HASH_CONSTS		1
#define JIT_NUM_OPERANDS	3

/*
 * Types
 */
#define JIT_FP			_FP
typedef enum {
#define jit_arg_reg_p(i)	((i) >= 0 && (i) < 4)
#define jit_r(i)		(_V0 + (i))
#define jit_r_num()		12
#define jit_v(i)		(_S0 + (i))
#define jit_v_num()		8
#define jit_arg_f_reg_p(i)	((i) >= 0 && (i) < 4)
#define jit_f(i)		(_F0 + (i))
#define jit_f_num()		14
    _AT,
#define JIT_R0			_V0
#define JIT_R1			_V1
#define JIT_R2			_T0
#define JIT_R3			_T1
#define JIT_R4			_T2
#define JIT_R5			_T3
#define JIT_R6			_T4
#define JIT_R7			_T5
#define JIT_R8			_T6
#define JIT_R9			_T7
#define JIT_R10			_T8
#define JIT_R11			_T9	/* must point to PIC function */
    _V0, _V1, _T0, _T1, _T2, _T3, _T4, _T5, _T6, _T7, _T8, _T9,
#define JIT_V0			_S0
#define JIT_V1			_S1
#define JIT_V2			_S2
#define JIT_V3			_S3
#define JIT_V4			_S4
#define JIT_V5			_S5
#define JIT_V6			_S6
#define JIT_V7			_S7
    _S0, _S1, _S2, _S3, _S4, _S5, _S6, _S7,
    _ZERO, _K0, _K1, _RA,
    _GP,				/* FIXME use to point to jit data */
    _SP, _FP,
#  define JIT_RA0		_A0
#  define JIT_RA1		_A1
#  define JIT_RA2		_A2
#  define JIT_RA3		_A3
    _A3, _A2, _A1, _A0,

#define JIT_F0			_F0
#define JIT_F1			_F2
#define JIT_F2			_F4
#define JIT_F3			_F6
#define JIT_F4			_F8
#define JIT_F5			_F10
    _F0, _F2, _F4, _F6, _F8, _F10,
    /* callee save float registers */
#define JIT_FS0			_F16
#define JIT_FS1			_F18
#define JIT_FS2			_F20
#define JIT_FS3			_F22
#define JIT_FS4			_F24
#define JIT_FS5			_F26
#define JIT_FS6			_F28
#define JIT_FS7			_F30
    _F16, _F18, _F20, _F22, _F24, _F26, _F28, _F30,
#define JIT_FA0			_F12
#define JIT_FA1			_F14
    _F14, _F12,
#define JIT_NOREG		_NOREG
    _NOREG,
} jit_reg_t;

typedef jit_int64_t		jit_regset_t;

#endif /* _jit_mips_h */
