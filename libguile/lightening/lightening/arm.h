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
 *	Paulo Cesar Pereira de Andrade
 */

#ifndef _jit_arm_h
#define _jit_arm_h


#define JIT_NEEDS_LITERAL_POOL 1

#define _R0     JIT_GPR(0)
#define _R1     JIT_GPR(1)
#define _R2     JIT_GPR(2)
#define _R3     JIT_GPR(3)
#define _R4     JIT_GPR(4)
#define _R5     JIT_GPR(5)
#define _R6     JIT_GPR(6)
#define _R7     JIT_GPR(7)
#define _R8     JIT_GPR(8)
#define _R9     JIT_GPR(9)
#define _R10    JIT_GPR(10)
#define _R11    JIT_GPR(11)
#define _R12    JIT_GPR(12)
#define _R13    JIT_GPR(13)
#define _R14    JIT_GPR(14)
#define _R15    JIT_GPR(15)

#define _D0     JIT_FPR(0)
#define _D1     JIT_FPR(2)
#define _D2     JIT_FPR(4)
#define _D3     JIT_FPR(6)
#define _D4     JIT_FPR(8)
#define _D5     JIT_FPR(10)
#define _D6     JIT_FPR(12)
#define _D7     JIT_FPR(14)
#define _D8     JIT_FPR(16)
#define _D9     JIT_FPR(18)
#define _D10    JIT_FPR(20)
#define _D11    JIT_FPR(22)
#define _D12    JIT_FPR(24)
#define _D13    JIT_FPR(26)
#define _D14    JIT_FPR(28)
#define _D15    JIT_FPR(30)

#define _S0     JIT_FPR(0)
#define _S1     JIT_FPR(1)
#define _S2     JIT_FPR(2)
#define _S3     JIT_FPR(3)
#define _S4     JIT_FPR(4)
#define _S5     JIT_FPR(5)
#define _S6     JIT_FPR(6)
#define _S7     JIT_FPR(7)
#define _S8     JIT_FPR(8)
#define _S9     JIT_FPR(9)
#define _S10     JIT_FPR(10)
#define _S11     JIT_FPR(11)
#define _S12     JIT_FPR(12)
#define _S13     JIT_FPR(13)
#define _S14     JIT_FPR(14)
#define _S15     JIT_FPR(15)
#define _S16     JIT_FPR(16)
#define _S17     JIT_FPR(17)
#define _S18     JIT_FPR(18)
#define _S19     JIT_FPR(19)
#define _S20    JIT_FPR(20)
#define _S21    JIT_FPR(21)
#define _S22    JIT_FPR(22)
#define _S23    JIT_FPR(23)
#define _S24    JIT_FPR(24)
#define _S25    JIT_FPR(25)
#define _S26    JIT_FPR(26)
#define _S27    JIT_FPR(27)
#define _S28    JIT_FPR(28)
#define _S29    JIT_FPR(29)
#define _S30    JIT_FPR(30)
#define _S31    JIT_FPR(31)

#define JIT_R0    _R0
#define JIT_R1    _R1
#define JIT_R2    _R2
#define JIT_R3    _R3
#define JIT_TMP0  _R12

#define JIT_V0    _R4
#define JIT_V1    _R5
#define JIT_V2    _R6
#define JIT_TMP1  _R7
#define JIT_V3    _R8
#define JIT_V4    _R9
#define JIT_V5    _R10
#define JIT_V6    _R11

#define JIT_LR _R14
#define JIT_SP _R13
#define _LR _R14
#define _PC _R15

#define JIT_F0 _D0
#define JIT_F1 _D1
#define JIT_F2 _D2
#define JIT_F3 _D3
#define JIT_F4 _D4
#define JIT_F5 _D5
#define JIT_F6 _D6
#define JIT_FTMP _D7

#define JIT_VF0 _D8
#define JIT_VF1 _D9
#define JIT_VF2 _D10
#define JIT_VF3 _D11
#define JIT_VF4 _D12
#define JIT_VF5 _D13
#define JIT_VF6 _D14
#define JIT_VF7 _D15

#define JIT_PLATFORM_CALLEE_SAVE_GPRS _LR, JIT_TMP1


#endif /* _jit_arm_h */
