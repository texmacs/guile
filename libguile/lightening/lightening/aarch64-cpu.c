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
 *      Paulo Cesar Pereira de Andrade
 */

#if __BYTE_ORDER != __LITTLE_ENDIAN
#error AArch64 requires little-endian host
#endif

static int32_t
logical_immediate(jit_word_t imm)
{
  /* There are 5334 possible immediate values, but to avoid the
   * need of either too complex code or large lookup tables,
   * only check for (simply) encodable common/small values */
  switch (imm) {
  case -16:       return 0xf3b;
  case -15:       return 0xf3c;
  case -13:       return 0xf3d;
  case -9:        return 0xf3e;
  case -8:        return 0xf7c;
  case -7:        return 0xf7d;
  case -5:        return 0xf7e;
  case -4:        return 0xfbd;
  case -3:        return 0xfbe;
  case -2:        return 0xffe;
  case 1:         return 0x000;
  case 2:         return 0xfc0;
  case 3:         return 0x001;
  case 4:         return 0xf80;
  case 6:         return 0xfc1;
  case 7:         return 0x002;
  case 8:         return 0xf40;
  case 12:        return 0xf81;
  case 14:        return 0xfc2;
  case 15:        return 0x003;
  case 16:        return 0xf00;
  default:        return -1;
  }
}

static void
oxxx(jit_state_t *_jit, int32_t Op, int32_t Rd, int32_t Rn, int32_t Rm)
{
  uint32_t inst = Op;
  inst = write_Rd_bitfield(inst, Rd);
  inst = write_Rn_bitfield(inst, Rn);
  inst = write_Rm_bitfield(inst, Rm);
  emit_u32_with_pool(_jit, inst);
}

static void
oxxi(jit_state_t *_jit, int32_t Op, int32_t Rd, int32_t Rn, int32_t Imm12)
{
  uint32_t inst = Op;
  inst = write_Rd_bitfield(inst, Rd);
  inst = write_Rn_bitfield(inst, Rn);
  inst = write_imm12_bitfield(inst, Imm12);
  emit_u32_with_pool(_jit, inst);
}

static void
oxx9(jit_state_t *_jit, int32_t Op, int32_t Rd, int32_t Rn, int32_t Simm9)
{
  uint32_t inst = Op;
  inst = write_Rd_bitfield(inst, Rd);
  inst = write_Rn_bitfield(inst, Rn);
  inst = write_simm9_bitfield(inst, Simm9);
  emit_u32_with_pool(_jit, inst);
}

static uint32_t
encode_ox19(int32_t Op, int32_t Rd)
{
  uint32_t inst = Op;
  inst = write_Rd_bitfield(inst, Rd);
  return inst;
}

static uint32_t
encode_oc19(int32_t Op, int32_t Cc)
{
  uint32_t inst = Op;
  inst = write_cond2_bitfield(inst, Cc);
  return inst;
}

static uint32_t
encode_o26(int32_t Op)
{
  uint32_t inst = Op;
  return inst;
}

static void
ox_x(jit_state_t *_jit, int32_t Op, int32_t Rd, int32_t Rm)
{
  uint32_t inst = Op;
  inst = write_Rd_bitfield(inst, Rd);
  inst = write_Rm_bitfield(inst, Rm);
  emit_u32_with_pool(_jit, inst);
}

static void
o_xx(jit_state_t *_jit, int32_t Op, int32_t Rd, int32_t Rn)
{
  uint32_t inst = Op;
  inst = write_Rd_bitfield(inst, Rd);
  inst = write_Rn_bitfield(inst, Rn);
  emit_u32_with_pool(_jit, inst);
}

static void
oxx_(jit_state_t *_jit, int32_t Op, int32_t Rn, int32_t Rm)
{
  uint32_t inst = Op;
  inst = write_Rn_bitfield(inst, Rn);
  inst = write_Rm_bitfield(inst, Rm);
  emit_u32_with_pool(_jit, inst);
}

static void
o_x_(jit_state_t *_jit, int32_t Op, int32_t Rn)
{
  uint32_t inst = Op;
  inst = write_Rn_bitfield(inst, Rn);
  emit_u32_with_pool(_jit, inst);
}

static void
ox_h(jit_state_t *_jit, int32_t Op, int32_t Rd, int32_t Imm16)
{
  uint32_t inst = Op;
  inst = write_Rd_bitfield(inst, Rd);
  inst = write_imm16_bitfield(inst, Imm16);
  emit_u32_with_pool(_jit, inst);
}

static void
oxxrs(jit_state_t *_jit, int32_t Op,
      int32_t Rd, int32_t Rn, int32_t R, int32_t S)
{
  uint32_t inst = Op;
  inst = write_Rd_bitfield(inst, Rd);
  inst = write_Rn_bitfield(inst, Rn);
  inst = write_immr_bitfield(inst, R);
  inst = write_imms_bitfield(inst, S);
  emit_u32_with_pool(_jit, inst);
}

#define XZR_REGNO                     0x1f
#define WZR_REGNO                     XZR_REGNO
#define LSL_12                        0x00400000
#define MOVI_LSL_16                   0x00200000
#define MOVI_LSL_32                   0x00400000
#define MOVI_LSL_48                   0x00600000
#define XS                            0x80000000      /* Wn -> Xn */
#define BCC_EQ                        0x0
#define BCC_NE                        0x1
#define BCC_CS                        0x2
#define BCC_HS                        BCC_CS
#define BCC_CC                        0x3
#define BCC_LO                        BCC_CC
#define BCC_MI                        0x4
#define BCC_PL                        0x5
#define BCC_VS                        0x6
#define BCC_VC                        0x7
#define BCC_HI                        0x8
#define BCC_LS                        0x9
#define BCC_GE                        0xa
#define BCC_LT                        0xb
#define BCC_GT                        0xc
#define BCC_LE                        0xd
#define BCC_AL                        0xe
#define BCC_NV                        0xf
/* adapted and cut down to only tested and required by lightening,
 * from data in binutils/aarch64-tbl.h */
#define A64_ADCS                      0x3a000000
#define A64_SBCS                      0x7a000000
#define A64_ADDI                      0x11000000
#define A64_ADDSI                     0xb1000000
#define A64_SUBI                      0x51000000
#define A64_SUBSI                     0x71000000
#define A64_ADD                       0x0b000000
#define A64_ADDS                      0x2b000000
#define A64_SUB                       0x4b000000
#define A64_NEG                       0x4b0003e0
#define A64_SUBS                      0x6b000000
#define A64_CMP                       0x6b00001f
#define A64_SBFM                      0x93400000
#define A64_UBFM                      0x53400000
#define A64_UBFX                      0x53000000
#define A64_B                         0x14000000
#define A64_BL                        0x94000000
#define A64_BR                        0xd61f0000
#define A64_BLR                       0xd63f0000
#define A64_RET                       0xd65f0000
#define A64_CBZ                       0x34000000
#define A64_CBNZ                      0x35000000
#define A64_B_C                       0x54000000
#define A64_REV                       0xdac00c00
#define A64_UDIV                      0x1ac00800
#define A64_SDIV                      0x1ac00c00
#define A64_LSL                       0x1ac02000
#define A64_LSR                       0x1ac02400
#define A64_ASR                       0x1ac02800
#define A64_MUL                       0x1b007c00
#define A64_SMULH                     0x9b407c00
#define A64_UMULH                     0x9bc07c00
#define A64_LDAR                      0xc8dffc00
#define A64_STLR                      0xc89ffc00
#define A64_LDAXR                     0xc85ffc00
#define A64_STLXR                     0xc800fc00
#define A64_STRBI                     0x39000000
#define A64_LDRBI                     0x39400000
#define A64_LDRSBI                    0x39800000
#define A64_STRI                      0xf9000000
#define A64_LDRI                      0xf9400000
#define A64_LDRI_LITERAL              0x58000000
#define A64_STRHI                     0x79000000
#define A64_LDRHI                     0x79400000
#define A64_LDRSHI                    0x79800000
#define A64_STRWI                     0xb9000000
#define A64_LDRWI                     0xb9400000
#define A64_LDRSWI                    0xb9800000
#define A64_STRB                      0x38206800
#define A64_LDRB                      0x38606800
#define A64_LDRSB                     0x38e06800
#define A64_STR                       0xf8206800
#define A64_LDR                       0xf8606800
#define A64_STRH                      0x78206800
#define A64_LDRH                      0x78606800
#define A64_LDRSH                     0x78a06800
#define A64_STRW                      0xb8206800
#define A64_LDRW                      0xb8606800
#define A64_LDRSW                     0xb8a06800
#define A64_STURB                     0x38000000
#define A64_LDURB                     0x38400000
#define A64_LDURSB                    0x38800000
#define A64_STUR                      0xf8000000
#define A64_LDUR                      0xf8400000
#define A64_STURH                     0x78000000
#define A64_LDURH                     0x78400000
#define A64_LDURSH                    0x78800000
#define A64_STURW                     0xb8000000
#define A64_LDURW                     0xb8400000
#define A64_LDURSW                    0xb8800000
#define A64_ANDI                      0x12400000
#define A64_ORRI                      0x32400000
#define A64_EORI                      0x52400000
#define A64_ANDSI                     0x72000000
#define A64_AND                       0x0a000000
#define A64_ORR                       0x2a000000
#define A64_MOV                       0x2a0003e0      /* AKA orr Rd,xzr,Rm */
#define A64_MVN                       0x2a2003e0
#define A64_UXTW                      0x2a0003e0      /* AKA MOV */
#define A64_EOR                       0x4a000000
#define A64_ANDS                      0x6a000000
#define A64_MOVN                      0x12800000
#define A64_MOVZ                      0x52800000
#define A64_MOVK                      0x72800000
#define A64_BRK                       0xd4200000

static void
SBFM(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t ImmR, int32_t ImmS) 
{
  return oxxrs(_jit, A64_SBFM|XS,Rd,Rn,ImmR,ImmS);
}

static void
UBFM(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t ImmR, int32_t ImmS) 
{
  return oxxrs(_jit, A64_UBFM|XS,Rd,Rn,ImmR,ImmS);
}

static void
UBFX(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t ImmR, int32_t ImmS) 
{
  return oxxrs(_jit, A64_UBFX,Rd,Rn,ImmR,ImmS);
}

static void
CMP(jit_state_t *_jit, int32_t Rn, int32_t Rm) 
{
  return oxx_(_jit, A64_CMP|XS,Rn,Rm);
}

static void
CMPI(jit_state_t *_jit, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_SUBSI|XS,XZR_REGNO,Rn,Imm12);
}

static void
CMPI_12(jit_state_t *_jit, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_SUBSI|XS|LSL_12,XZR_REGNO,Rn,Imm12);
}

static void
CMNI(jit_state_t *_jit, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_ADDSI|XS,XZR_REGNO,Rn,Imm12);
}

static void
CMNI_12(jit_state_t *_jit, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_ADDSI|XS|LSL_12,XZR_REGNO,Rn,Imm12);
}

static void
TST(jit_state_t *_jit, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_ANDS|XS,XZR_REGNO,Rn,Rm);
}

/* actually should use oxxrs but logical_immediate returns proper encoding */
static void
TSTI(jit_state_t *_jit, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_ANDSI,XZR_REGNO,Rn,Imm12);
}

static void
MOV(jit_state_t *_jit, int32_t Rd, int32_t Rm) 
{
  return ox_x(_jit, A64_MOV|XS,Rd,Rm);
}

static void
MVN(jit_state_t *_jit, int32_t Rd, int32_t Rm) 
{
  return ox_x(_jit, A64_MVN|XS,Rd,Rm);
}

static void
NEG(jit_state_t *_jit, int32_t Rd, int32_t Rm) 
{
  return ox_x(_jit, A64_NEG|XS,Rd,Rm);
}

static void
MOVN(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVN|XS,Rd,Imm16);
}

static void
MOVN_16(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVN|XS|MOVI_LSL_16,Rd,Imm16);
}

static void
MOVN_32(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVN|XS|MOVI_LSL_32,Rd,Imm16);
}

static void
MOVN_48(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVN|XS|MOVI_LSL_48,Rd,Imm16);
}

static void
MOVZ(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVZ|XS,Rd,Imm16);
}

static void
MOVZ_16(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVZ|XS|MOVI_LSL_16,Rd,Imm16);
}

static void
MOVZ_32(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVZ|XS|MOVI_LSL_32,Rd,Imm16);
}

static void
MOVZ_48(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVZ|XS|MOVI_LSL_48,Rd,Imm16);
}

static void
MOVK_16(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVK|XS|MOVI_LSL_16,Rd,Imm16);
}

static void
MOVK_32(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVK|XS|MOVI_LSL_32,Rd,Imm16);
}

static void
MOVK_48(jit_state_t *_jit, int32_t Rd, int32_t Imm16) 
{
  return ox_h(_jit, A64_MOVK|XS|MOVI_LSL_48,Rd,Imm16);
}

static void
ADD(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_ADD|XS,Rd,Rn,Rm);
}

static void
ADDI(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_ADDI|XS,Rd,Rn,Imm12);
}

static void
ADDI_12(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_ADDI|XS|LSL_12,Rd,Rn,Imm12);
}

static void
ADDS(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_ADDS|XS,Rd,Rn,Rm);
}

static void
ADDSI(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_ADDSI|XS,Rd,Rn,Imm12);
}

static void
ADDSI_12(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_ADDSI|XS|LSL_12,Rd,Rn,Imm12);
}

static void
ADCS(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_ADCS|XS,Rd,Rn,Rm);
}

static void
SUB(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_SUB|XS,Rd,Rn,Rm);
}

static void
SUBI(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_SUBI|XS,Rd,Rn,Imm12);
}

static void
SUBI_12(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_SUBI|XS|LSL_12,Rd,Rn,Imm12);
}

static void
SUBS(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_SUBS|XS,Rd,Rn,Rm);
}

static void
SUBSI(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_SUBSI|XS,Rd,Rn,Imm12);
}

static void
SUBSI_12(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_SUBSI|XS|LSL_12,Rd,Rn,Imm12);
}

static void
SBCS(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_SBCS|XS,Rd,Rn,Rm);
}

static void
MUL(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_MUL|XS,Rd,Rn,Rm);
}

static void
SMULH(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_SMULH,Rd,Rn,Rm);
}

static void
UMULH(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_UMULH,Rd,Rn,Rm);
}

static void
SDIV(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_SDIV|XS,Rd,Rn,Rm);
}

static void
UDIV(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_UDIV|XS,Rd,Rn,Rm);
}

static void
LSL(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_LSL|XS,Rd,Rn,Rm);
}

static void
LSLI(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0) 
{
  return UBFM(_jit, r0,r1,(64-i0)&63,63-i0);
}

static void
ASR(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_ASR|XS,Rd,Rn,Rm);
}

static void
ASRI(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0) 
{
  return SBFM(_jit, r0,r1,i0,63);
}

static void
LSR(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_LSR|XS,Rd,Rn,Rm);
}

static void
LSRI(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0) 
{
  return UBFM(_jit, r0,r1,i0,63);
}

static void
AND(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_AND|XS,Rd,Rn,Rm);
}

/* actually should use oxxrs but logical_immediate returns proper encoding */;
static void
ANDI(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_ANDI|XS,Rd,Rn,Imm12);
}

static void
ORR(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_ORR|XS,Rd,Rn,Rm);
}

/* actually should use oxxrs but logical_immediate returns proper encoding */
static void
ORRI(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_ORRI|XS,Rd,Rn,Imm12);
}

static void
EOR(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_EOR|XS,Rd,Rn,Rm);
}

/* actually should use oxxrs but logical_immediate returns proper encoding */
static void
EORI(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_EORI|XS,Rd,Rn,Imm12);
}

static void
SXTB(jit_state_t *_jit, int32_t Rd, int32_t Rn) 
{
  return SBFM(_jit, Rd,Rn,0,7);
}

static void
SXTH(jit_state_t *_jit, int32_t Rd, int32_t Rn) 
{
  return SBFM(_jit, Rd,Rn,0,15);
}

static void
SXTW(jit_state_t *_jit, int32_t Rd, int32_t Rn) 
{
  return SBFM(_jit, Rd,Rn,0,31);
}

static void
UXTB(jit_state_t *_jit, int32_t Rd, int32_t Rn) 
{
  return UBFX(_jit, Rd,Rn,0,7);
}

static void
UXTH(jit_state_t *_jit, int32_t Rd, int32_t Rn) 
{
  return UBFX(_jit, Rd,Rn,0,15);
}

static void
UXTW(jit_state_t *_jit, int32_t Rd, int32_t Rm) 
{
  return ox_x(_jit, A64_UXTW,Rd,Rm);
}

static void
REV(jit_state_t *_jit, int32_t Rd, int32_t Rn) 
{
  return o_xx(_jit, A64_REV,Rd,Rn);
}

static void
LDAR(jit_state_t *_jit, int32_t Rt, int32_t Rn) 
{
  return o_xx(_jit, A64_LDAR, Rt, Rn);
}

static void
STLR(jit_state_t *_jit, int32_t Rt, int32_t Rn) 
{
  return o_xx(_jit, A64_STLR, Rt, Rn);
}

static void
LDAXR(jit_state_t *_jit, int32_t Rt, int32_t Rn) 
{
  return o_xx(_jit, A64_LDAXR, Rt, Rn);
}

static void
STLXR(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm)
{
  return oxxx(_jit, A64_STLXR, Rt, Rn, Rm);
}

static void
LDRSB(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_LDRSB,Rt,Rn,Rm);
}

static void
LDRSBI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_LDRSBI,Rt,Rn,Imm12);
}

static void
LDURSB(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_LDURSB,Rt,Rn,Imm9);
}

static void
LDRB(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_LDRB,Rt,Rn,Rm);
}

static void
LDRBI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_LDRBI,Rt,Rn,Imm12);
}

static void
LDURB(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_LDURB,Rt,Rn,Imm9);
}

static void
LDRSH(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_LDRSH,Rt,Rn,Rm);
}

static void
LDRSHI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_LDRSHI,Rt,Rn,Imm12);
}

static void
LDURSH(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_LDURSH,Rt,Rn,Imm9);
}

static void
LDRH(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_LDRH,Rt,Rn,Rm);
}

static void
LDRHI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_LDRHI,Rt,Rn,Imm12);
}

static void
LDURH(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_LDURH,Rt,Rn,Imm9);
}

static void
LDRSW(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_LDRSW,Rt,Rn,Rm);
}

static void
LDRSWI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_LDRSWI,Rt,Rn,Imm12);
}

static void
LDURSW(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_LDURSW,Rt,Rn,Imm9);
}

static void
LDRW(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_LDRW,Rt,Rn,Rm);
}

static void
LDRWI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_LDRWI,Rt,Rn,Imm12);
}

static void
LDURW(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_LDURW,Rt,Rn,Imm9);
}

static void
LDR(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_LDR,Rt,Rn,Rm);
}

static void
LDRI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_LDRI,Rt,Rn,Imm12);
}

static void
LDUR(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_LDUR,Rt,Rn,Imm9);
}

static void
STRB(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_STRB,Rt,Rn,Rm);
}

static void
STRBI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_STRBI,Rt,Rn,Imm12);
}

static void
STURB(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_STURB,Rt,Rn,Imm9);
}

static void
STRH(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_STRH,Rt,Rn,Rm);
}

static void
STRHI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_STRHI,Rt,Rn,Imm12);
}

static void
STURH(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_STURH,Rt,Rn,Imm9);
}

static void
STRW(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_STRW,Rt,Rn,Rm);
}

static void
STRWI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_STRWI,Rt,Rn,Imm12);
}

static void
STURW(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_STURW,Rt,Rn,Imm9);
}

static void
STR(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Rm) 
{
  return oxxx(_jit, A64_STR,Rt,Rn,Rm);
}

static void
STRI(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm12) 
{
  return oxxi(_jit, A64_STRI,Rt,Rn,Imm12);
}

static void
STUR(jit_state_t *_jit, int32_t Rt, int32_t Rn, int32_t Imm9) 
{
  return oxx9(_jit, A64_STUR,Rt,Rn,Imm9);
}

static jit_reloc_t
B(jit_state_t *_jit)
{
  return emit_jmp(_jit, encode_o26(A64_B));
}

static jit_reloc_t
BL(jit_state_t *_jit)
{
  return emit_jmp(_jit, encode_o26(A64_BL));
}

static void
BR(jit_state_t *_jit, int32_t Rn) 
{
  return o_x_(_jit, A64_BR,Rn);
}

static void
BLR(jit_state_t *_jit, int32_t Rn)
{
  return o_x_(_jit, A64_BLR,Rn);
}

static void
RET(jit_state_t *_jit)
{
  return o_x_(_jit, A64_RET,jit_gpr_regno(_LR));
}

static jit_reloc_t
B_C(jit_state_t *_jit, int32_t Cc) 
{
  return emit_jcc(_jit, encode_oc19(A64_B_C, Cc));
}

static jit_reloc_t
CBZ(jit_state_t *_jit, int32_t Rd) 
{
  return emit_jcc(_jit, encode_ox19(A64_CBZ|XS,Rd));
}

static jit_reloc_t
CBNZ(jit_state_t *_jit, int32_t Rd) 
{
  return emit_jcc(_jit, encode_ox19(A64_CBNZ|XS,Rd));
}

static void
NOP(jit_state_t *_jit)
{
  return emit_u32_with_pool(_jit, 0xd503201f);
}

static void
BRK(jit_state_t *_jit)
{
  emit_u32_with_pool(_jit, A64_BRK);
}

static jit_reloc_t
movi_from_pool(jit_state_t *_jit, int32_t Rt)
{
  return emit_load_from_pool(_jit, encode_ox19(A64_LDRI_LITERAL, Rt));
}

static void
emit_veneer(jit_state_t *_jit, jit_pointer_t target)
{
  jit_gpr_t tmp = get_temp_gpr(_jit);
  uint32_t ldr = encode_ox19(A64_LDRI_LITERAL, jit_gpr_regno(tmp));
  uint32_t br = write_Rn_bitfield(A64_BR, jit_gpr_regno(tmp));
  uint32_t *loc = _jit->pc.ui;
  emit_u32(_jit, ldr);
  emit_u32(_jit, br);
  unget_temp_gpr(_jit);
  if (_jit->overflow)
    return;
  // Patch load to here, divided by 4.
  patch_load_from_pool_offset(loc, _jit->pc.ui - loc);
  emit_u64(_jit, (uint64_t) target);
}

static void
movr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    MOV(_jit, r0, r1);
}

static void
addr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return ADD(_jit,r0,r1,r2);
}

static void
addcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return ADDS(_jit,r0,r1,r2);
}

static void
addxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return ADCS(_jit,r0,r1,r2);
}

static void
subr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return SUB(_jit,r0,r1,r2);
}

static void
subcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return SUBS(_jit,r0,r1,r2);
}

static void
subxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return SBCS(_jit,r0,r1,r2);
}

static void
mulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return MUL(_jit,r0,r1,r2);
}

static void
divr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return SDIV(_jit,r0,r1,r2);
}

static void
divr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return UDIV(_jit,r0,r1,r2);
}

static void
iqdivr(jit_state_t *_jit, jit_bool_t sign,
       int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  int32_t rg0, rg1;
  if (r0 == r2 || r0 == r3) {
    rg0 = jit_gpr_regno(get_temp_gpr(_jit));
  } else {
    rg0 = r0;
  }
  if (r1 == r2 || r1 == r3) {
    rg1 = jit_gpr_regno(get_temp_gpr(_jit));
  } else {
    rg1 = r1;
  }
  if (sign)
    divr(_jit, rg0, r2, r3);
  else
    divr_u(_jit, rg0, r2, r3);
  mulr(_jit, rg1, r3, rg0);
  subr(_jit, rg1, r2, rg1);
  if (rg0 != r0) {
    movr(_jit, r0, rg0);
    unget_temp_gpr(_jit);
  }
  if (rg1 != r1) {
    movr(_jit, r1, rg1);
    unget_temp_gpr(_jit);
  }
}

static void
qdivr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqdivr(_jit,1,r0,r1,r2,r3);
}

static void
qdivr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqdivr(_jit,0,r0,r1,r2,r3);
}

static void
lshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return LSL(_jit,r0,r1,r2);
}

static void
rshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return ASR(_jit,r0,r1,r2);
}

static void
rshr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return LSR(_jit,r0,r1,r2);
}

static void
negr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return NEG(_jit,r0,r1);
}

static void
comr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return MVN(_jit,r0,r1);
}

static void
andr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return AND(_jit,r0,r1,r2);
}

static void
orr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return ORR(_jit,r0,r1,r2);
}

static void
xorr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return EOR(_jit,r0,r1,r2);
}

static void
ldr_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return LDRSBI(_jit,r0,r1,0);
}

static void
ldr_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return LDRSHI(_jit,r0,r1,0);
}

static void
ldr_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return LDRSWI(_jit,r0,r1,0);
}

static void
ldxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return LDRSH(_jit,r0,r1,r2);
}

static void
ldxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return LDRSW(_jit,r0,r1,r2);
}

static void
ldxr_l(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return LDR(_jit,r0,r1,r2);
}

static void
str_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return STRBI(_jit,r1,r0,0);
}

static void
str_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return STRHI(_jit,r1,r0,0);
}

static void
str_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return STRWI(_jit,r1,r0,0);
}

static void
str_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return STRI(_jit,r1,r0,0);
}

static void
stxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return STRB(_jit,r2,r1,r0);
}

static void
stxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return STRH(_jit,r2,r1,r0);
}

static void
stxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return STRW(_jit,r2,r1,r0);
}

static void
stxr_l(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return STR(_jit,r2,r1,r0);
}

static void
bswapr_ul(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return REV(_jit,r0,r1);
}

static void
extr_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return SXTB(_jit,r0,r1);
}

static void
extr_uc(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return UXTB(_jit,r0,r1);
}

static void
extr_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return SXTH(_jit,r0,r1);
}

static void
extr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return UXTH(_jit,r0,r1);
}

static void
extr_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return SXTW(_jit,r0,r1);
}

static void
extr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return UXTW(_jit,r0,r1);
}

static void
movi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_word_t n0 = ~i0, ibit = 0, nbit = 0;
  if (i0 & 0x000000000000ffffL)       ibit |= 1;
  if (i0 & 0x00000000ffff0000L)       ibit |= 2;
  if (i0 & 0x0000ffff00000000L)       ibit |= 4;
  if (i0 & 0xffff000000000000L)       ibit |= 8;
  if (n0 & 0x000000000000ffffL)       nbit |= 1;
  if (n0 & 0x00000000ffff0000L)       nbit |= 2;
  if (n0 & 0x0000ffff00000000L)       nbit |= 4;
  if (n0 & 0xffff000000000000L)       nbit |= 8;
  switch (ibit) {
  case 0:
    MOVZ   (_jit, r0,  0);
    break;
  case 1:
    MOVZ   (_jit, r0,  i0        & 0xffff);
    break;
  case 2:
    MOVZ_16(_jit, r0, (i0 >> 16) & 0xffff);
    break;
  case 3:
    MOVZ   (_jit, r0,  i0        & 0xffff);
    MOVK_16(_jit, r0, (i0 >> 16) & 0xffff);
    break;
  case 4:
    MOVZ_32(_jit, r0, (i0 >> 32) & 0xffff);
    break;
  case 5:
    MOVZ   (_jit, r0,  i0        & 0xffff);
    MOVK_32(_jit, r0, (i0 >> 32) & 0xffff);
    break;
  case 6:
    MOVZ_16(_jit, r0, (i0 >> 16) & 0xffff);
    MOVK_32(_jit, r0, (i0 >> 32) & 0xffff);
    break;
  case 7:
    if (nbit == 8) {
      MOVN_48(_jit, r0, (n0 >> 48) & 0xffff);
    } else {
      MOVZ   (_jit, r0,  i0        & 0xffff);
      MOVK_16(_jit, r0, (i0 >> 16) & 0xffff);
      MOVK_32(_jit, r0, (i0 >> 32) & 0xffff);
    }
    break;
  case 8:
    MOVZ_48(_jit, r0, (i0 >> 48) & 0xffff);
    break;
  case 9:
    MOVZ   (_jit, r0,  i0        & 0xffff);
    MOVK_48(_jit, r0, (i0 >> 48) & 0xffff);
    break;
  case 10:
    MOVZ_16(_jit, r0, (i0 >> 16) & 0xffff);
    MOVK_48(_jit, r0, (i0 >> 48) & 0xffff);
    break;
  case 11:
    if (nbit == 4) {
      MOVN_32(_jit, r0, (n0 >> 32) & 0xffff);
    } else {
      MOVZ   (_jit, r0,  i0        & 0xffff);
      MOVK_16(_jit, r0, (i0 >> 16) & 0xffff);
      MOVK_48(_jit, r0, (i0 >> 48) & 0xffff);
    }
    break;
  case 12:
    MOVZ_32(_jit, r0, (i0 >> 32) & 0xffff);
    MOVK_48(_jit, r0, (i0 >> 48) & 0xffff);
    break;
  case 13:
    if (nbit == 2) {
      MOVN_16(_jit, r0, (n0 >> 16) & 0xffff);
    } else {
      MOVZ   (_jit, r0,  i0        & 0xffff);
      MOVK_32(_jit, r0, (i0 >> 32) & 0xffff);
      MOVK_48(_jit, r0, (i0 >> 48) & 0xffff);
    }
    break;
  case 14:
    if (nbit == 1) {
      MOVN   (_jit, r0, (n0)       & 0xffff);
    } else {
      MOVZ_16(_jit, r0, (i0 >> 16) & 0xffff);
      MOVK_32(_jit, r0, (i0 >> 32) & 0xffff);
      MOVK_48(_jit, r0, (i0 >> 48) & 0xffff);
    }
    break;
  case 15:
    if (nbit == 0) {
      MOVN   (_jit, r0,  0);
    } else if (nbit == 1) {
      MOVN   (_jit, r0,  n0        & 0xffff);
    } else if (nbit == 8) {
      MOVN_48(_jit, r0, (n0 >> 48) & 0xffff);
    } else {
      MOVZ   (_jit, r0,  i0        & 0xffff);
      MOVK_16(_jit, r0, (i0 >> 16) & 0xffff);
      MOVK_32(_jit, r0, (i0 >> 32) & 0xffff);
      MOVK_48(_jit, r0, (i0 >> 48) & 0xffff);
    }
    break;
  default:
    abort();
  }
}

static jit_reloc_t
bccr(jit_state_t *_jit, int32_t cc, int32_t r0, int32_t r1)
{
  CMP(_jit, r0, r1);
  return B_C(_jit, cc);
}

static jit_reloc_t
bcci(jit_state_t *_jit, int32_t cc, int32_t r0, jit_word_t i1)
{
  jit_word_t          is =  i1 >> 12;
  jit_word_t          in = -i1;
  jit_word_t          iS =  in >> 12;
  if (      i1 >= 0 && i1 <= 0xfff) {
    CMPI   (_jit, r0, i1);
  } else if ((is << 12) == i1 && is >= 0 && is <= 0xfff) {
    CMPI_12(_jit, r0, is);
  } else if ( in >= 0 && in <= 0xfff) {
    CMNI   (_jit, r0, in);
  } else if ((iS << 12) == is && iS >= 0 && iS <= 0xfff) {
    CMNI_12(_jit, r0, iS);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i1);
    CMP(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
  return B_C(_jit, cc);
}

static jit_reloc_t
bltr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_LT,r0,r1);
}

static jit_reloc_t
blti(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit,BCC_LT,r0,i1);
}

static jit_reloc_t
bltr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_CC,r0,r1);
}

static jit_reloc_t
blti_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit,BCC_CC,r0,i1);
}

static jit_reloc_t
bler(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_LE,r0,r1);
}

static jit_reloc_t
blei(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit,BCC_LE,r0,i1);
}

static jit_reloc_t
bler_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_LS,r0,r1);
}

static jit_reloc_t
blei_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit,BCC_LS,r0,i1);
}

static jit_reloc_t
beqr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_EQ,r0,r1);
}

static jit_reloc_t
bger(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_GE,r0,r1);
}

static jit_reloc_t
bgei(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit,BCC_GE,r0,i1);
}

static jit_reloc_t
bger_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_CS,r0,r1);
}

static jit_reloc_t
bgei_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit,BCC_CS,r0,i1);
}

static jit_reloc_t
bgtr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_GT,r0,r1);
}

static jit_reloc_t
bgti(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit,BCC_GT,r0,i1);
}

static jit_reloc_t
bgtr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_HI,r0,r1);
}

static jit_reloc_t
bgti_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit,BCC_HI,r0,i1);
}

static jit_reloc_t
bner(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit,BCC_NE,r0,r1);
}

static void
addi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_word_t          is =  i0 >> 12;
  jit_word_t          in = -i0;
  jit_word_t          iS =  in >> 12;
  if (      i0 >= 0 && i0 <= 0xfff) {
    ADDI   (_jit, r0, r1, i0);
  } else if ((is << 12) == i0 && is >= 0 && is <= 0xfff) {
    ADDI_12(_jit, r0, r1, is);
  } else if ( in >= 0 && in <= 0xfff) {
    SUBI   (_jit, r0, r1, in);
  } else if ((iS << 12) == is && iS >= 0 && iS <= 0xfff) {
    SUBI_12(_jit, r0, r1, iS);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    movi(_jit, r2, i0);
    addr(_jit, r0, r1, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
}

static void
addci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_word_t          is =  i0 >> 12;
  jit_word_t          in = -i0;
  jit_word_t          iS =  in >> 12;
  if (      i0 >= 0 && i0 <= 0xfff) {
    ADDSI   (_jit, r0, r1, i0);
  } else if ((is << 12) == i0 && is >= 0 && is <= 0xfff) {
    ADDSI_12(_jit, r0, r1, is);
  } else if ( in >= 0 && in <= 0xfff) {
    SUBSI   (_jit, r0, r1, in);
  } else if ((iS << 12) == is && iS >= 0 && iS <= 0xfff) {
    SUBSI_12(_jit, r0, r1, iS);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    movi(_jit, r2, i0);
    addcr(_jit, r0, r1, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
}

static void
addxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
  movi(_jit, r2, i0);
  addxr(_jit, r0, r1, r2);
  if (r0 == r1)
    unget_temp_gpr(_jit);
}

static void
subi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_word_t          is = i0 >> 12;
  if (      i0 >= 0 && i0 <= 0xfff) {
    SUBI   (_jit, r0, r1, i0);
  } else if ((is << 12) == i0 && is >= 0 && is <= 0xfff) {
    SUBI_12(_jit, r0, r1, is);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    movi(_jit, r2, i0);
    subr(_jit, r0, r1, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
}

static void
subci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_word_t          is = i0 >> 12;
  if (      i0 >= 0 && i0 <= 0xfff) {
    SUBSI   (_jit, r0, r1, i0);
  } else if ((is << 12) == i0 && is >= 0 && is <= 0xfff) {
    SUBSI_12(_jit, r0, r1, is);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    movi(_jit, r2, i0);
    subcr(_jit, r0, r1, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
}

static void
subxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
  movi(_jit, r2, i0);
  subxr(_jit, r0, r1, r2);
  if (r0 == r1)
    unget_temp_gpr(_jit);
}

static jit_reloc_t
baddr(jit_state_t *_jit, int32_t cc, int32_t r0, int32_t r1)
{
  addcr(_jit, r0, r0, r1);
  return B_C(_jit, cc);
}

static jit_reloc_t
baddi(jit_state_t *_jit, int32_t cc, int32_t r0, jit_word_t i1)
{
  addci(_jit, r0, r0, i1);
  return B_C(_jit, cc);
}

static jit_reloc_t
boaddr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return baddr(_jit,BCC_VS,r0,r1);
}

static jit_reloc_t
boaddi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return baddi(_jit,BCC_VS,r0,i1);
}

static jit_reloc_t
boaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return baddr(_jit,BCC_HS,r0,r1);
}

static jit_reloc_t
boaddi_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return baddi(_jit,BCC_HS,r0,i1);
}

static jit_reloc_t
bxaddr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return baddr(_jit,BCC_VC,r0,r1);
}

static jit_reloc_t
bxaddi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return baddi(_jit,BCC_VC,r0,i1);
}

static jit_reloc_t
bxaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return baddr(_jit,BCC_LO,r0,r1);
}

static jit_reloc_t
bxaddi_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return baddi(_jit,BCC_LO,r0,i1);
}

static jit_reloc_t
bsubr(jit_state_t *_jit, int32_t cc, int32_t r0, int32_t r1)
{
  subcr(_jit, r0, r0, r1);
  return B_C(_jit, cc);
}

static jit_reloc_t
bsubi(jit_state_t *_jit, int32_t cc, int32_t r0, jit_word_t i1)
{
  subci(_jit, r0, r0, i1);
  return B_C(_jit, cc);
}

static jit_reloc_t
bosubr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bsubr(_jit,BCC_VS,r0,r1);
}

static jit_reloc_t
bosubi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bsubi(_jit,BCC_VS,r0,i1);
}

static jit_reloc_t
bosubr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bsubr(_jit,BCC_LO,r0,r1);
}

static jit_reloc_t
bosubi_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bsubi(_jit,BCC_LO,r0,i1);
}

static jit_reloc_t
bxsubr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bsubr(_jit,BCC_VC,r0,r1);
}

static jit_reloc_t
bxsubi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bsubi(_jit,BCC_VC,r0,i1);
}

static jit_reloc_t
bxsubr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bsubr(_jit,BCC_HS,r0,r1);
}

static jit_reloc_t
bxsubi_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bsubi(_jit,BCC_HS,r0,i1);
}

static jit_reloc_t
bmxr(jit_state_t *_jit, int32_t cc, int32_t r0, int32_t r1)
{
  TST(_jit, r0, r1);
  return B_C(_jit, cc);
}

static jit_reloc_t
bmxi(jit_state_t *_jit, int32_t cc, int32_t r0, jit_word_t i1)
{
  int32_t             imm;
  imm = logical_immediate(i1);
  if (imm != -1) {
    TSTI(_jit, r0, imm);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i1);
    TST(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
  return B_C(_jit, cc);
}

static jit_reloc_t
bmsr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bmxr(_jit,BCC_NE,r0,r1);
}

static jit_reloc_t
bmsi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bmxi(_jit,BCC_NE,r0,i1);
}

static jit_reloc_t
bmcr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bmxr(_jit,BCC_EQ,r0,r1);
}

static jit_reloc_t
bmci(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bmxi(_jit,BCC_EQ,r0,i1);
}

static void
jmpr(jit_state_t *_jit, int32_t r0)
{
  return BR(_jit, r0);
}

static void
callr(jit_state_t *_jit, int32_t r0)
{
  return BLR(_jit,r0);
}

static void
nop(jit_state_t *_jit, int32_t i0)
{
  for (; i0 > 0; i0 -= 4)
    NOP(_jit);
  ASSERT(i0 == 0);
}

static void
muli(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
  movi(_jit, r2, i0);
  mulr(_jit, r0, r1, r2);
  if (r0 == r1)
    unget_temp_gpr(_jit);
}

static void
qmulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  jit_gpr_t reg;
  if (r0 == r2 || r0 == r3) {
    reg = get_temp_gpr(_jit);
    mulr(_jit, jit_gpr_regno(reg), r2, r3);
  } else {
    mulr(_jit, r0, r2, r3);
  }
  SMULH(_jit, r1, r2, r3);
  if (r0 == r2 || r0 == r3) {
    movr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
qmuli(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  qmulr(_jit, r0, r1, r2, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
qmulr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  jit_gpr_t reg;
  if (r0 == r2 || r0 == r3) {
    reg = get_temp_gpr(_jit);
    mulr(_jit, jit_gpr_regno(reg), r2, r3);
  } else {
    mulr(_jit, r0, r2, r3);
  }
  UMULH(_jit, r1, r2, r3);
  if (r0 == r2 || r0 == r3) {
    movr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
qmuli_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  qmulr_u(_jit, r0, r1, r2, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
divi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
  movi(_jit, r2, i0);
  divr(_jit, r0, r1, r2);
  if (r0 == r1)
    unget_temp_gpr(_jit);
}

static void
divi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
  movi(_jit, r2, i0);
  divr_u(_jit, r0, r1, r2);
  if (r0 == r1)
    unget_temp_gpr(_jit);
}

static void
qdivi(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  qdivr(_jit, r0, r1, r2, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
qdivi_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  qdivr_u(_jit, r0, r1, r2, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
remr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1 || r0 == r2) {
    jit_gpr_t reg = get_temp_gpr(_jit);
    divr(_jit, jit_gpr_regno(reg), r1, r2);
    mulr(_jit, jit_gpr_regno(reg), r2, jit_gpr_regno(reg));
    subr(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  } else {
    divr(_jit, r0, r1, r2);
    mulr(_jit, r0, r2, r0);
    subr(_jit, r0, r1, r0);
  }
}

static void
remi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  remr(_jit, r0, r1, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
remr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1 || r0 == r2) {
    jit_gpr_t reg = get_temp_gpr(_jit);
    divr_u(_jit, jit_gpr_regno(reg), r1, r2);
    mulr(_jit, jit_gpr_regno(reg), r2, jit_gpr_regno(reg));
    subr(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  } else {
    divr_u(_jit, r0, r1, r2);
    mulr(_jit, r0, r2, r0);
    subr(_jit, r0, r1, r0);
  }
}

static void
remi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  remr_u(_jit, r0, r1, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
lshi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 == 0) {
    movr(_jit, r0, r1);
  } else {
    ASSERT(i0 > 0 && i0 < 64);
    LSLI(_jit, r0, r1, i0);
  }
}

static void
rshi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 == 0) {
    movr(_jit, r0, r1);
  } else {
    ASSERT(i0 > 0 && i0 < 64);
    ASRI(_jit, r0, r1, i0);
  }
}

static void
rshi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 == 0) {
    movr(_jit, r0, r1);
  } else {
    ASSERT(i0 > 0 && i0 < 64);
    LSRI(_jit, r0, r1, i0);
  }
}

static void
andi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t             imm;
  if (i0 == 0) {
    movi(_jit, r0, 0);
  } else if (i0 == -1){
    movr(_jit, r0, r1);
  } else {
    imm = logical_immediate(i0);
    if (imm != -1) {
      ANDI(_jit, r0, r1, imm);
    } else {
      int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
      movi(_jit, r2, i0);
      andr(_jit, r0, r1, r2);
      if (r0 == r1)
        unget_temp_gpr(_jit);
    }
  }
}

static void
ori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t             imm;
  if (i0 == 0) {
    movr(_jit, r0, r1);
  } else if (i0 == -1) {
    movi(_jit, r0, -1);
  } else {
    imm = logical_immediate(i0);
    if (imm != -1) {
      ORRI(_jit, r0, r1, imm);
    } else {
      int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
      movi(_jit, r2, i0);
      orr(_jit, r0, r1, r2);
      if (r0 == r1)
        unget_temp_gpr(_jit);
    }
  }
}

static void
xori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t             imm;
  if (i0 == 0) {
    movr(_jit, r0, r1);
  } else if (i0 == -1) {
    comr(_jit, r0, r1);
  } else {
    imm = logical_immediate(i0);
    if (imm != -1) {
      EORI(_jit, r0, r1, imm);
    } else {
      int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
      movi(_jit, r2, i0);
      xorr(_jit, r0, r1, r2);
      if (r0 == r1)
        unget_temp_gpr(_jit);
    }
  }
}

static void
bswapr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  bswapr_ul(_jit, r0, r1);
  rshi_u(_jit, r0, r0, 48);
}

static void
bswapr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  bswapr_ul(_jit, r0, r1);
  rshi_u(_jit, r0, r0, 32);
}

static void
ldi_c(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  movi(_jit, r0, i0);
  ldr_c(_jit, r0, r0);
}

static void
ldr_uc(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  LDRBI(_jit, r0, r1, 0);
#if 0
  extr_uc(_jit, r0, r0);
#endif
}

static void
ldi_uc(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  movi(_jit, r0, i0);
  ldr_uc(_jit, r0, r0);
}

static void
ldi_s(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  movi(_jit, r0, i0);
  ldr_s(_jit, r0, r0);
}

static void
ldr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  LDRHI(_jit, r0, r1, 0);
#if 0
  extr_us(_jit, r0, r0);
#endif
}

static void
ldi_us(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  movi(_jit, r0, i0);
  ldr_us(_jit, r0, r0);
}

static void
ldi_i(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  movi(_jit, r0, i0);
  ldr_i(_jit, r0, r0);
}

static void
ldr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  LDRWI(_jit, r0, r1, 0);
#if 0
  extr_ui(_jit, r0, r0);
#endif
}

static void
ldi_ui(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  movi(_jit, r0, i0);
  ldr_ui(_jit, r0, r0);
}

static void
ldr_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  LDRI(_jit, r0, r1, 0);
}

static void
ldi_l(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  movi(_jit, r0, i0);
  ldr_l(_jit, r0, r0);
}

static void
ldxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  LDRSB(_jit, r0, r1, r2);
  extr_c(_jit, r0, r0);
}

static void
ldxi_c(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 >= 0 && i0 <= 4095) {
    LDRSBI(_jit, r0, r1, i0);
  } else if (i0 > -256 && i0 < 0) {
    LDURSB(_jit, r0, r1, i0);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    movi(_jit, r2, i0);
    LDRSB(_jit, r0, r1, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
  extr_c(_jit, r0, r0);
}

static void
ldxr_uc(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  LDRB(_jit, r0, r1, r2);
#if 0
  extr_uc(_jit, r0, r0);
#endif
}

static void
ldxi_uc(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 >= 0 && i0 <= 4095) {
    LDRBI(_jit, r0, r1, i0);
  } else if (i0 > -256 && i0 < 0) {
    LDURB(_jit, r0, r1, i0);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    addi(_jit, r2, r1, i0);
    ldr_uc(_jit, r0, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
#if 0
  extr_uc(_jit, r0, r0);
#endif
}

static void
ldxi_s(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  ASSERT(!(i0 & 1));
  if (i0 >= 0 && i0 <= 8191) {
    LDRSHI(_jit, r0, r1, i0 >> 1);
  } else if (i0 > -256 && i0 < 0) {
    LDURSH(_jit, r0, r1, i0);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    movi(_jit, r2, i0);
    LDRSH(_jit, r0, r1, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
}

static void
ldxr_us(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  LDRH(_jit, r0, r1, r2);
#if 0
  extr_us(_jit, r0, r0);
#endif
}

static void
ldxi_us(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  ASSERT(!(i0 & 1));
  if (i0 >= 0 && i0 <= 8191) {
    LDRHI(_jit, r0, r1, i0 >> 1);
  } else if (i0 > -256 && i0 < 0) {
    LDURH(_jit, r0, r1, i0);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    movi(_jit, r2, i0);
    LDRH(_jit, r0, r1, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
#if 0
  extr_us(_jit, r0, r0);
#endif
}

static void
ldxi_i(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  ASSERT(!(i0 & 3));
  if (i0 >= 0 && i0 <= 16383) {
    LDRSWI(_jit, r0, r1, i0 >> 2);
  } else if (i0 > -256 && i0 < 0) {
    LDURSW(_jit, r0, r1, i0);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    addi(_jit, r2, r1, i0);
    ldr_i(_jit, r0, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
}

static void
ldxr_ui(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  LDRW(_jit, r0, r1, r2);
#if 0
  extr_ui(_jit, r0, r0);
#endif
}

static void
ldxi_ui(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  ASSERT(!(i0 & 3));
  if (i0 >= 0 && i0 <= 16383) {
    LDRWI(_jit, r0, r1, i0 >> 2);
  } else if (i0 > -256 && i0 < 0) {
    LDURW(_jit, r0, r1, i0);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    movi(_jit, r2, i0);
    LDRW(_jit, r0, r1, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
#if 0
  extr_ui(_jit, r0, r0);
#endif
}

static void
ldxi_l(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  ASSERT(!(i0 & 7));
  if (i0 >= 0 && i0 <= 32767) {
    LDRI(_jit, r0, r1, i0 >> 3);
  } else if (i0 > -256 && i0 < 0) {
    LDUR(_jit, r0, r1, i0);
  } else {
    int32_t r2 = (r0 == r1) ? jit_gpr_regno(get_temp_gpr(_jit)) : r0;
    addi(_jit, r2, r1, i0);
    ldr_l(_jit, r0, r2);
    if (r0 == r1)
      unget_temp_gpr(_jit);
  }
}

static void
sti_c(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  str_c(_jit, jit_gpr_regno(reg), r0);
  unget_temp_gpr(_jit);
}

static void
sti_s(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  str_s(_jit, jit_gpr_regno(reg), r0);
  unget_temp_gpr(_jit);
}

static void
sti_i(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  str_i(_jit, jit_gpr_regno(reg), r0);
  unget_temp_gpr(_jit);
}

static void
sti_l(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  str_l(_jit, jit_gpr_regno(reg), r0);
  unget_temp_gpr(_jit);
}

static void
stxi_c(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (i0 >= 0 && i0 <= 4095) {
    STRBI(_jit, r1, r0, i0);
  } else if (i0 > -256 && i0 < 0) {
    STURB(_jit, r1, r0, i0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(reg), r0, i0);
    str_c(_jit, jit_gpr_regno(reg), r1);
    unget_temp_gpr(_jit);
  }
}

static void
stxi_s(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  ASSERT(!(i0 & 1));
  if (i0 >= 0 && i0 <= 8191) {
    STRHI(_jit, r1, r0, i0 >> 1);
  } else if (i0 > -256 && i0 < 0) {
    STURH(_jit, r1, r0, i0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(reg), r0, i0);
    str_s(_jit, jit_gpr_regno(reg), r1);
    unget_temp_gpr(_jit);
  }
}

static void
stxi_i(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  ASSERT(!(i0 & 3));
  if (i0 >= 0 && i0 <= 16383) {
    STRWI(_jit, r1, r0, i0 >> 2);
  } else if (i0 > -256 && i0 < 0) {
    STURW(_jit, r1, r0, i0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(reg), r0, i0);
    str_i(_jit, jit_gpr_regno(reg), r1);
    unget_temp_gpr(_jit);
  }
}

static void
stxi_l(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  ASSERT(!(i0 & 7));
  if (i0 >= 0 && i0 <= 32767) {
    STRI(_jit, r1, r0, i0 >> 3);
  } else if (i0 > -256 && i0 < 0) {
    STUR(_jit, r1, r0, i0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(reg), r0, i0);
    str_l(_jit, jit_gpr_regno(reg), r1);
    unget_temp_gpr(_jit);
  }
}

static jit_reloc_t
mov_addr(jit_state_t *_jit, int32_t r0)
{
  return movi_from_pool(_jit, r0);
}

static jit_reloc_t
beqi(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1 == 0) {
    return CBZ(_jit, r0);
  } else {
    return bcci(_jit, BCC_EQ, r0, i1);
  }
}

static jit_reloc_t
bnei(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1 == 0) {
    return CBNZ(_jit, r0);
  } else {
    return bcci(_jit, BCC_NE, r0, i1);
  }
}

static jit_reloc_t
jmp(jit_state_t *_jit)
{
  return B(_jit);
}

static void
jmpi(jit_state_t *_jit, jit_word_t i0)
{
  return jit_patch_there(_jit, jmp(_jit), (void*)i0);
}

static jit_reloc_t
call(jit_state_t *_jit)
{
  return BL(_jit);
}

static void
calli(jit_state_t *_jit, jit_word_t i0)
{
  return jit_patch_there(_jit, call(_jit), (void*)i0);
}

static void
jmpi_with_link(jit_state_t *_jit, jit_word_t i0)
{
  return calli(_jit, i0);
}

static void
push_link_register(jit_state_t *_jit)
{
}

static void
pop_link_register(jit_state_t *_jit)
{
}

static void
ret(jit_state_t *_jit)
{
  RET(_jit);
}

static void
retr(jit_state_t *_jit, int32_t r)
{
  movr(_jit, jit_gpr_regno(_X0), r);
  ret(_jit);
}

static void
reti(jit_state_t *_jit, int32_t i)
{
  movi(_jit, jit_gpr_regno(_X0), i);
  ret(_jit);
}

static void
retval_c(jit_state_t *_jit, int32_t r0)
{
  extr_c(_jit, r0, jit_gpr_regno(_X0));
}

static void
retval_uc(jit_state_t *_jit, int32_t r0)
{
  extr_uc(_jit, r0, jit_gpr_regno(_X0));
}

static void
retval_s(jit_state_t *_jit, int32_t r0)
{
  extr_s(_jit, r0, jit_gpr_regno(_X0));
}

static void
retval_us(jit_state_t *_jit, int32_t r0)
{
  extr_us(_jit, r0, jit_gpr_regno(_X0));
}

static void
retval_i(jit_state_t *_jit, int32_t r0)
{
  extr_i(_jit, r0, jit_gpr_regno(_X0));
}

static void
retval_ui(jit_state_t *_jit, int32_t r0)
{
  extr_ui(_jit, r0, jit_gpr_regno(_X0));
}

static void
retval_l(jit_state_t *_jit, int32_t r0)
{
  movr(_jit, r0, jit_gpr_regno(_X0));
}

static uint32_t*
jmp_without_veneer(jit_state_t *_jit)
{
  uint32_t *loc = _jit->pc.ui;
  emit_u32(_jit, encode_o26(A64_B));
  return loc;
}

static void
patch_jmp_without_veneer(jit_state_t *_jit, uint32_t *loc)
{
  patch_jmp_offset(loc, _jit->pc.ui - loc);
}

static void
ldr_atomic(jit_state_t *_jit, int32_t dst, int32_t loc)
{
  LDAR(_jit, dst, loc);
}

static void
str_atomic(jit_state_t *_jit, int32_t loc, int32_t val)
{
  STLR(_jit, val, loc);
}

static void
swap_atomic(jit_state_t *_jit, int32_t dst, int32_t loc, int32_t val)
{
  void *retry = jit_address(_jit);
  int32_t result = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t val_or_tmp = dst == val ? jit_gpr_regno(get_temp_gpr(_jit)) : val;
  movr(_jit, val_or_tmp, val);
  LDAXR(_jit, dst, loc);
  STLXR(_jit, val_or_tmp, loc, result);
  jit_patch_there(_jit, bnei(_jit, result, 0), retry);
  if (dst == val) unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
}

static void
cas_atomic(jit_state_t *_jit, int32_t dst, int32_t loc, int32_t expected,
           int32_t desired)
{
  int32_t dst_or_tmp;
  if (dst == loc || dst == expected || dst == expected)
    dst_or_tmp = jit_gpr_regno(get_temp_gpr(_jit));
  else
    dst_or_tmp = dst;
  void *retry = jit_address(_jit);
  LDAXR(_jit, dst_or_tmp, loc);
  jit_reloc_t bad = bner(_jit, dst_or_tmp, expected);
  int result = jit_gpr_regno(get_temp_gpr(_jit));
  STLXR(_jit, desired, loc, result);
  jit_patch_there(_jit, bnei(_jit, result, 0), retry);
  unget_temp_gpr(_jit);
  jit_patch_here(_jit, bad);
  movr(_jit, dst, dst_or_tmp);
  unget_temp_gpr(_jit);
}

static void
breakpoint(jit_state_t *_jit)
{
  BRK(_jit);
}
