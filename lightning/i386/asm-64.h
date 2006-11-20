/******************************** -*- C -*- ****************************
 *
 *	Run-time assembler for the x86-64
 *
 ***********************************************************************/


/***********************************************************************
 *
 * Copyright 2006 Matthew Flatt
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




#ifndef __lightning_asm_h
#define __lightning_asm_h

#ifndef LIGHTNING_DEBUG

#include "asm-i386.h"

/*	OPCODE	+ i		= immediate operand
 *		+ r		= register operand
 *		+ m		= memory operand (disp,base,index,scale)
 *		+ sr/sm		= a star preceding a register or memory
 */


#define _rA(R)          _r8(R)

/* Use RIP-addressing in 64-bit mode, if possible */
#if 0
#define _x86_RIP_addressing_possible(D,O)	(X86_RIP_RELATIVE_ADDR && \
						((unsigned long)x86_get_target() + 4 + (O) - (D) <= 0xffffffff))

#define _r_X(   R, D,B,I,S,O)	(_r0P(I) ? (_r0P(B)    ? (!X86_TARGET_64BIT ? _r_D(R,D) : \
					                 (_x86_RIP_addressing_possible(D, O) ? \
				                          _r_D(R, (D) - ((unsigned long)x86_get_target() + 4 + (O))) : \
				                          _r_DSIB(R,D))) : \
					                 _r_DSIB(R,D                ))  : \
				           (_rIP(B)    ? _r_D   (R,D                )   : \
				           (_rsp12P(B) ? _r_DBIS(R,D,_RSP,_RSP,1)   : \
						         _r_DB  (R,D,     B       ))))  : \
				 (_r0P(B)	       ? _r_4IS (R,D,	         I,S)   : \
				 (!_rspP(I)            ? _r_DBIS(R,D,     B,     I,S)   : \
						         JITFAIL("illegal index register: %esp"))))
#else
#define _r_X(   R, D,B,I,S,O)	(_r0P(I) ? (_r0P(B)    ? _r_DSIB(R,D                )   : \
				           (_rIP(B)    ? _r_D   (R,D                )   : \
				           (_rsp12P(B) ? _r_DBIS(R,D,_RSP,_RSP,1)   : \
						         _r_DB  (R,D,     B       ))))  : \
				 (_r0P(B)	       ? _r_4IS (R,D,	         I,S)   : \
				 (!_rspP(I)            ? _r_DBIS(R,D,     B,     I,S)   : \
						         JITFAIL("illegal index register: %esp"))))
#endif


#define _m32only(X)		(JITFAIL("invalid instruction in 64-bit mode"))
#define _m64only(X)		(X)
#define _m64(X)			(X)

#define CALLsr(R)			CALLQsr(R)
#define JMPsr(R)			JMPQsr(R)

#define _SPL		0x14
#define _BPL		0x15
#define _SIL		0x16
#define _DIL		0x17
#define _R8B		0x18
#define _R9B		0x19
#define _R10B		0x1A
#define _R11B		0x1B
#define _R12B		0x1C
#define _R13B		0x1D
#define _R14B		0x1E
#define _R15B		0x1F

#define _R8W		0x38
#define _R9W		0x39
#define _R10W		0x3A
#define _R11W		0x3B
#define _R12W		0x3C
#define _R13W		0x3D
#define _R14W		0x3E
#define _R15W		0x3F
#define _R8D		0x48
#define _R9D		0x49
#define _R10D		0x4A
#define _R11D		0x4B
#define _R12D		0x4C
#define _R13D		0x4D
#define _R14D		0x4E
#define _R15D		0x4F

#define _RAX		0x50
#define _RCX		0x51
#define _RDX		0x52
#define _RBX		0x53
#define _RSP		0x54
#define _RBP		0x55
#define _RSI		0x56
#define _RDI		0x57
#define _R8		0x48
#define _R9		0x49
#define _R10		0x4A
#define _R11		0x4B
#define _R12		0x4C
#define _R13		0x4D
#define _R14		0x4E
#define _R15		0x4F
#define _RIP		-2

#if 0
#define _r8(R)		( (_rC(R) == 0x50)			? _rN(R) : JITFAIL("64-bit register required"))
#else
#define _r8(R)		( (_rC(R) == 0x50)			? _rN(R) : _r4(R))
#endif

#define _r1e8lP(R)	((int)(R) >= _SPL && (int)(R) <= _DIL)

#define DECWr(RD)	(_d16(), _REXLrr(0, RD),	_O_Mrm		(0xff		,_b11,_b001  ,_r2(RD)				))
#define DECLr(RD)	(_REXLrr(0, RD),		_O_Mrm		(0xff		,_b11,_b001  ,_r4(RD)				))
#define INCWr(RD)	(_d16(), _REXLrr(0, RD),	_O_Mrm		(0xff		,_b11,_b000  ,_r2(RD)				))
#define INCLr(RD)	(_REXLrr(0, RD),		_O_Mrm		(0xff		,_b11,_b000  ,_r4(RD)				))

#endif
#endif /* __lightning_asm_h */

