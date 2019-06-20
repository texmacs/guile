/*
 * Copyright (C) 2013-2017, 2019  Free Software Foundation, Inc.
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

#ifndef _jit_aarch64_h
#define _jit_aarch64_h


#define JIT_NEEDS_LITERAL_POOL 1

#define _X0     JIT_GPR(0)
#define _X1     JIT_GPR(1)
#define _X2     JIT_GPR(2)
#define _X3     JIT_GPR(3)
#define _X4     JIT_GPR(4)
#define _X5     JIT_GPR(5)
#define _X6     JIT_GPR(6)
#define _X7     JIT_GPR(7)
#define _X8     JIT_GPR(8)
#define _X9     JIT_GPR(9)
#define _X10    JIT_GPR(10)
#define _X11    JIT_GPR(11)
#define _X12    JIT_GPR(12)
#define _X13    JIT_GPR(13)
#define _X14    JIT_GPR(14)
#define _X15    JIT_GPR(15)
#define _X16    JIT_GPR(16)
#define _X17    JIT_GPR(17)
#define _X18    JIT_GPR(18)
#define _X19    JIT_GPR(19)
#define _X20    JIT_GPR(20)
#define _X21    JIT_GPR(21)
#define _X22    JIT_GPR(22)
#define _X23    JIT_GPR(23)
#define _X24    JIT_GPR(24)
#define _X25    JIT_GPR(25)
#define _X26    JIT_GPR(26)
#define _X27    JIT_GPR(27)
#define _X28    JIT_GPR(28)
#define _X29    JIT_GPR(29)
#define _X30    JIT_GPR(30)
#define _X31    JIT_GPR(31)

#define _D0     JIT_FPR(0)
#define _D1     JIT_FPR(1)
#define _D2     JIT_FPR(2)
#define _D3     JIT_FPR(3)
#define _D4     JIT_FPR(4)
#define _D5     JIT_FPR(5)
#define _D6     JIT_FPR(6)
#define _D7     JIT_FPR(7)
#define _D8     JIT_FPR(8)
#define _D9     JIT_FPR(9)
#define _D10    JIT_FPR(10)
#define _D11    JIT_FPR(11)
#define _D12    JIT_FPR(12)
#define _D13    JIT_FPR(13)
#define _D14    JIT_FPR(14)
#define _D15    JIT_FPR(15)
#define _D16    JIT_FPR(16)
#define _D17    JIT_FPR(17)
#define _D18    JIT_FPR(18)
#define _D19    JIT_FPR(19)
#define _D20    JIT_FPR(20)
#define _D21    JIT_FPR(21)
#define _D22    JIT_FPR(22)
#define _D23    JIT_FPR(23)
#define _D24    JIT_FPR(24)
#define _D25    JIT_FPR(25)
#define _D26    JIT_FPR(26)
#define _D27    JIT_FPR(27)
#define _D28    JIT_FPR(28)
#define _D29    JIT_FPR(29)
#define _D30    JIT_FPR(30)
#define _D31    JIT_FPR(31)

#define JIT_R0    _X0
#define JIT_R1    _X1
#define JIT_R2    _X2
#define JIT_R3    _X3
#define JIT_R4    _X4
#define JIT_R5    _X5
#define JIT_R6    _X6
#define JIT_R7    _X7
#define JIT_R8    _X8
#define JIT_R9    _X9
#define JIT_R10   _X10
#define JIT_R11   _X11
#define JIT_R12   _X12
#define JIT_R13   _X13
#define JIT_R14   _X14
#define JIT_R15   _X15
#define JIT_TMP0  _X16
#define JIT_TMP1  _X17
// x18 is reserved by the platform.
#define JIT_V0    _X19
#define JIT_V1    _X20
#define JIT_V2    _X21
#define JIT_V3    _X22
#define JIT_V4    _X23
#define JIT_V5    _X24
#define JIT_V6    _X25
#define JIT_V7    _X26
#define JIT_V8    _X27
#define JIT_V9    _X28

// x29 is frame pointer; x30 is link register.
#define JIT_PLATFORM_CALLEE_SAVE_GPRS _X29, _X30

// x31 is stack pointer.
#define JIT_LR    _X30
#define JIT_SP    _X31

#define JIT_F0  _D0
#define JIT_F1  _D1
#define JIT_F2  _D2
#define JIT_F3  _D3
#define JIT_F4  _D4
#define JIT_F5  _D5
#define JIT_F6  _D6
#define JIT_F7  _D7
#define JIT_F8  _D16
#define JIT_F9  _D17
#define JIT_F10 _D18
#define JIT_F11 _D19
#define JIT_F12 _D20
#define JIT_F13 _D21
#define JIT_F14 _D22
#define JIT_F15 _D23
#define JIT_F16 _D24
#define JIT_F17 _D25
#define JIT_F18 _D26
#define JIT_F19 _D27
#define JIT_F20 _D28
#define JIT_F21 _D29
#define JIT_F22 _D30
#define JIT_FTMP _D31

#define JIT_VF0  _D8
#define JIT_VF1  _D9
#define JIT_VF2 _D10
#define JIT_VF3 _D11
#define JIT_VF4 _D12
#define JIT_VF5 _D13
#define JIT_VF6 _D14
#define JIT_VF7 _D15

#define _FP _X29
#define _LR _X30
#define _SP _X31


#endif /* _jit_aarch64_h */
