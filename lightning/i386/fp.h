/******************************** -*- C -*- ****************************
 *
 *	Run-time assembler & support macros for the i386 math coprocessor
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2000, 2001, 2002, 2004 Free Software Foundation, Inc.
 * Written by Paolo Bonzini.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1, or (at your option)
 * any later version.
 * 
 * GNU lightning is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with GNU lightning; see the file COPYING.LESSER; if not, write to the
 * Free Software Foundation, 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 *
 ***********************************************************************/


#ifndef __lightning_asm_fp_h
#define __lightning_asm_fp_h

/* Actually, we should redesign the jitfp interface.  As a first step, I have
   defined the macros for many x87 instructions, and I am using them here.

   In practice, we can provide something sensible and make it work on the x86
   using the stack like a file of eight registers.  Then this awful stuff goes
   away, and everything is "beautiful" as the rest of GNU lightning---and we'll
   document it, promised.

   Well, let's use six or seven registers so as to have some freedom
   for floor, ceil, round, log, tan, atn and exp.

   Not hard at all, basically play with FXCH.  FXCH is mostly free,
   so the generated code is not bad.  Of course we special case when one
   of the operands turns out to be ST0.

   - binary ops:

	add FRR3 to FPR0
		FADD ST0,ST3

	add FPR0 to FPR3
		FADD ST3,ST0

	add FPR3 to FPR7 (I'm using nasm syntax here)
		FXCH ST3
		FADD ST7,ST0
		FXCH ST3

   - stores:

	store FPR3

		FXCH ST3
		FST [FUBAR]
		FXCH ST3

	store FPR0

		FST [FUBAR]

	(and similarly for other unary ops like FCHS or FABS)

   - moves:

	move FPR0 to FPR3
		FST  ST3

	move FPR3 to FPR0
		FXCH ST3
		FST  ST3

	move FPR3 to FPR1
		FSTP ST1   Save old st0 into destination register
		FLD  ST2   Stack is rotated, so FPRn becomes STn-1
		FXCH ST1   Get back old st0

   - loads:

	load into FPR0
		FSTP ST0
		FLD  [FUBAR]

	load into FPR3
		FSTP ST3     Save old st0 into destination register
		FLD  [FUBAR]
		FXCH ST3     Get back old st0

   (and similarly for immediates, using the stack) */

#define jit_add_two(reg0)	FADDPr(1)
#define jit_sub_two(reg0)	FSUBRPr(1)
#define jit_mul_two(reg0)	FMULPr(1)
#define jit_div_two(reg0)	FDIVRPr(1)

#define jit_abs(reg0)		_OO(0xd9e1)			/* fabs */
#define jit_sqr(reg0)		FMULrr(0,0)
#define jit_sqrt(reg0)		_OO(0xd9fa)			/* fsqrt */

#define jit_exti_d(reg0, rs)	(PUSHLr((rs)), FILDLm(0, _ESP, 0, 0), POPLr((rs)))

#define jit_neg(reg0)		_OO(0xd9e0)			/* fchs */

#define jit_ldxr_f(reg0, s1, s2)	FLDSm(0, (s1), (s2), 1)
#define jit_ldxi_f(reg0, rs, is)	FLDSm((is), (rs), 0, 0)
#define jit_ldxr_f(reg0, s1, s2)	FLDSm(0, (s1), (s2), 1)
#define jit_ldxi_d(reg0, rs, is)	FLDLm((is), (rs), 0, 0)
#define jit_ldxr_d(reg0, s1, s2)	FLDLm(0, (s1), (s2), 1)
#define jit_ldi_f(reg0, is)		FLDSm((is), 0,    0, 0)
#define jit_ldr_f(reg0, rs)		FLDSm(0,    (rs), 0, 0)
#define jit_ldi_d(reg0, is)		FLDLm((is), 0,    0, 0)
#define jit_ldr_d(reg0, rs)		FLDLm(0,    (rs), 0, 0)
#define jit_stxi_f(id, rd, reg0)	FSTPSm((id), (rd), 0, 0)
#define jit_stxr_f(d1, d2, reg0)	FSTPSm(0, (d1), (d2), 1)
#define jit_stxi_d(id, rd, reg0)	FSTPLm((id), (rd), 0, 0)
#define jit_stxr_d(d1, d2, reg0)	FSTPLm(0, (d1), (d2), 1)
#define jit_sti_f(id, reg0)		FSTPSm((id), 0,    0, 0)
#define jit_str_f(rd, reg0)		FSTPSm(0,    (rd), 0, 0)
#define jit_sti_d(id, reg0)		FSTPLm((id), 0,    0, 0)
#define jit_str_d(rd, reg0)		FSTPLm(0,    (rd), 0, 0)

#define jit_fpimm(reg0, first, second)	\
	(PUSHLi(second),		\
	PUSHLi(first),			\
	FLDLm(0, _ESP, 0, 0),		\
	ADDLir(8, _ESP))


/* Assume round to near mode */
#define jit_floor(rd, reg0)	\
	jit_floor2((rd), ((rd) == _EDX ? _EAX : _EDX))

#define jit_ceil(rd, reg0)	\
	jit_ceil2((rd), ((rd) == _EDX ? _EAX : _EDX))

#define jit_trunc(rd, reg0)	\
	jit_trunc2((rd), ((rd) == _EDX ? _EAX : _EDX))

#define jit_calc_diff(ofs)		\
	FISTLm(ofs, _ESP, 0, 0),	\
	FILDLm(ofs, _ESP, 0, 0),	\
	FSUBRPr(1),			\
	FSTPSm(4+ofs, _ESP, 0, 0)	\

/* The real meat */
#define jit_floor2(rd, aux)		\
	(PUSHLr(aux),			\
	SUBLir(8, _ESP),		\
	jit_calc_diff(0),		\
	POPLr(rd),			/* floor in rd */ \
	POPLr(aux),			/* x-round(x) in aux */ \
	ADDLir(0x7FFFFFFF, aux),	/* carry if x-round(x) < -0 */ \
	SBBLir(0, rd),			/* subtract 1 if carry */ \
	POPLr(aux))

#define jit_ceil2(rd, aux)		\
	(PUSHLr(aux),			\
	SUBLir(8, _ESP),		\
	jit_calc_diff(0),		\
	POPLr(rd),			/* floor in rd */ \
	POPLr(aux),			/* x-round(x) in aux */ \
	TESTLrr(aux, aux),		\
	SETGr(jit_reg8(aux)),		\
	SHRLir(1, aux),			\
	ADCLir(0, rd),			\
	POPLr(aux))

/* a mingling of the two above */
#define jit_trunc2(rd, aux)			\
	(PUSHLr(aux),				\
	SUBLir(12, _ESP),			\
	FSTSm(0, _ESP, 0, 0),			\
	jit_calc_diff(4),			\
	POPLr(aux),				\
	POPLr(rd),				\
	TESTLrr(aux, aux),			\
	POPLr(aux),				\
	JSSm(_jit.x.pc + 11, 0, 0, 0),		\
	ADDLir(0x7FFFFFFF, aux),	/* 6 */	\
	SBBLir(0, rd),			/* 3 */ \
	JMPSm(_jit.x.pc + 10, 0, 0, 0),	/* 2 */ \
	TESTLrr(aux, aux),		/* 2 */ \
	SETGr(jit_reg8(aux)),		/* 3 */ \
	SHRLir(1, aux),			/* 2 */ \
	ADCLir(0, rd),			/* 3 */ \
	POPLr(aux))

/* the easy one */
#define jit_round(rd, reg0)		\
	(PUSHLr(_EAX),			\
	FISTPLm(0, _ESP, 0, 0),		\
	POPLr((rd)))

#define jit_cmp(le, ge, reg0) (					\
	((le) == _EAX || (ge) == _EAX ? 0 : PUSHLr(_EAX)),	\
	FCOMr(0),						\
	FNSTSWr(_AX),						\
	TESTBir(0x40, _AH),					\
	MOVLir(0, (le)),					\
	MOVLrr((le), (ge)),					\
	JZSm(_jit.x.pc + 11, 0, 0, 0),				\
	_OO(0xd9e4),			/* ftst */	/* 2 */ \
	FNSTSWr(_AX),					/* 2 */	\
	SAHF(),						/* 1 */ \
	SETLEr( ((le) & 15) | 0x10),			/* 3 */ \
	SETGEr( ((ge) & 15) | 0x10),			/* 3 */ \
	((le) == _EAX || (ge) == _EAX ? ANDLir (1, _EAX) : POPLr(_EAX)) )

#define jitfp_getarg_f(ofs)             jitfp_ldxi_f(JIT_FP,(ofs))
#define jitfp_getarg_d(ofs)             jitfp_ldxi_d(JIT_FP,(ofs))
#define jitfp_pusharg_d(op1)            (jit_subi_i(JIT_SP,JIT_SP,sizeof(double)), jitfp_str_d(JIT_SP,(op1)))
#define jitfp_pusharg_f(op1)            (jit_subi_i(JIT_SP,JIT_SP,sizeof(float)), jitfp_str_f(JIT_SP,(op1)))
#define jitfp_retval(op1)               _jit_emit(&_jit, (op1), JIT_NULL, 0, 0, 0)

#define JIT_TRANSCENDENTAL

#define jit_sin(reg0)		_OO(0xd9fe)			/* fsin */
#define jit_cos(reg0)		_OO(0xd9ff)			/* fcos */
#define jit_tan(reg0)		(_OO(0xd9f2), 			/* fptan */ \
				 FSTPr(0))			/* fstp st */
#define jit_atn(reg0)		(_OO(0xd9e8), 			/* fld1 */ \
				 _OO(0xd9f3))			/* fpatan */
#define jit_exp(reg0)		(_OO(0xd9ea), 			/* fldl2e */ \
				 FMULPr(1), 			/* fmulp */ \
				 _OO(0xd9c0),			/* fld st */ \
				 _OO(0xd9fc),		 	/* frndint */ \
				 _OO(0xdce9), 			/* fsubr */ \
				 FXCHr(1), 			/* fxch st(1) */ \
				 _OO(0xd9f0), 			/* f2xm1 */ \
				 _OO(0xd9e8), 			/* fld1 */ \
				 _OO(0xdec1), 			/* faddp */ \
				 _OO(0xd9fd), 			/* fscale */ \
				 FSTPr(1))			/* fstp st(1) */
#define jit_log(reg0)		(_OO(0xd9ed), 			/* fldln2 */ \
				 FXCHr(1), 			/* fxch st(1) */ \
				 _OO(0xd9f1))			/* fyl2x */

#endif /* __lightning_asm_h */
