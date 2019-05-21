/*
 * Copyright (C) 2012-2017, 2019  Free Software Foundation, Inc.
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

#define _s20P(d)                      ((d) >= -(int)0x80000 && d <= 0x7ffff)
#define _s24P(d)                      ((d) >= -(int)0x800000 && d <= 0x7fffff)
#define _u3(v)                        ((v) & 0x7)
#define _u4(v)                        ((v) & 0xf)
#define _u5(v)                        ((v) & 0x1f)
#define _u8(v)                        ((v) & 0xff)
#define _u12(v)                       ((v) & 0xfff)
#define _u13(v)                       ((v) & 0x1fff)
#define _u16(v)                       ((v) & 0xffff)
#define _u24(v)                       ((v) & 0xffffff)

#define ARM_CC_EQ                     0x00000000      /* Z=1 */
#define ARM_CC_NE                     0x10000000      /* Z=0 */
#define ARM_CC_HS                     0x20000000      /* C=1 */
#define ARM_CC_LO                     0x30000000      /* C=0 */
#define ARM_CC_MI                     0x40000000      /* N=1 */
#define ARM_CC_VS                     0x60000000      /* V=1 */
#define ARM_CC_VC                     0x70000000      /* V=0 */
#define ARM_CC_HI                     0x80000000      /* C=1 && Z=0 */
#define ARM_CC_LS                     0x90000000      /* C=0 || Z=1 */
#define ARM_CC_GE                     0xa0000000      /* N=V */
#define ARM_CC_LT                     0xb0000000      /* N!=V */
#define ARM_CC_GT                     0xc0000000      /* Z=0 && N=V */
#define ARM_CC_LE                     0xd0000000      /* Z=1 || N!=V */
#define ARM_CC_AL                     0xe0000000      /* always */
#define ARM_CC_NV                     0xf0000000      /* reserved */
#define THUMB_MOV                         0x4600
#define THUMB_MOVI                        0x2000
#define THUMB2_MOVI                   0xf0400000
#define THUMB2_MOVWI                  0xf2400000
#define THUMB2_MOVTI                  0xf2c00000
#define THUMB_MVN                         0x43c0
#define THUMB2_MVN                    0xea600000
#define THUMB2_MVNI                   0xf0600000
#define ARM_S                         0x00100000 /* set flags */
#define THUMB_ADD                         0x1800
#define THUMB_ADDX                        0x4400
#define THUMB2_ADD                    0xeb000000
#define THUMB_ADDI3                       0x1c00
#define THUMB_ADDI8                       0x3000
#define THUMB2_ADDI                   0xf1000000
#define THUMB2_ADDWI                  0xf2000000
#define THUMB_ADC                         0x4140
#define THUMB2_ADC                    0xeb400000
#define THUMB2_ADCI                   0xf1400000
#define THUMB_SUB                         0x1a00
#define THUMB2_SUB                    0xeba00000
#define THUMB_SUBI3                       0x1e00
#define THUMB_SUBI8                       0x3800
#define THUMB2_SUBI                   0xf1a00000
#define THUMB2_SUBWI                  0xf2a00000
#define THUMB_SBC                         0x4180
#define THUMB2_SBC                    0xeb600000
#define THUMB2_SBCI                   0xf1600000
#define THUMB_RSBI                        0x4240
#define THUMB2_RSBI                   0xf1c00000
#define THUMB_MUL                         0x4340
#define THUMB2_MUL                    0xfb00f000
#define THUMB2_UMULL                  0xfba00000
#define THUMB2_SMULL                  0xfb800000
#define THUMB_MLS                     0xfb000010
#define THUMB2_SDIV                   0xfb90f0f0
#define THUMB2_UDIV                   0xfbb0f0f0
#define THUMB_AND                         0x4000
#define THUMB2_AND                    0xea000000
#define THUMB2_ANDI                   0xf0000000
#define THUMB2_BIC                    0xea200000
#define THUMB2_BICI                   0xf0200000
#define THUMB_ORR                         0x4300
#define THUMB2_ORR                    0xea400000
#define THUMB2_ORRI                   0xf0400000
#define THUMB_EOR                         0x4040
#define THUMB2_EOR                    0xea800000
#define THUMB2_EORI                   0xf0800000
#define THUMB_REV                         0xba00
#define THUMB2_REV                    0xfa90f080
#define THUMB_SXTB                        0xb240
#define THUMB2_SXTB                   0xfa40f080
#define THUMB_UXTB                        0xb2c0
#define THUMB2_UXTB                   0xfa50f080
#define THUMB_SXTH                        0xb200
#define THUMB2_SXTH                   0xfa00f080
#define THUMB_UXTH                        0xb280
#define THUMB2_UXTH                   0xfa10f080
#define ARM_LSL                       0x00000000
#define THUMB_LSL                         0x4080
#define THUMB2_LSL                    0xfa00f000
#define THUMB_LSLI                        0x0000
#define THUMB2_LSLI                   0xea4f0000
#define ARM_LSR                       0x00000020
#define THUMB_LSR                         0x40c0
#define THUMB2_LSR                    0xfa20f000
#define THUMB_LSRI                        0x0800
#define THUMB2_LSRI                   0xea4f0010
#define ARM_ASR                       0x00000040
#define THUMB_ASR                         0x4100
#define THUMB2_ASR                    0xfa40f000
#define THUMB_ASRI                        0x1000
#define THUMB2_ASRI                   0xea4f0020
#define THUMB_CMP                         0x4280
#define THUMB_CMPX                        0x4500
#define THUMB2_CMP                    0xebb00000
#define THUMB_CMPI                        0x2800
#define THUMB2_CMPI                   0xf1b00000
#define THUMB2_CMN                    0xeb100000
#define THUMB2_CMNI                   0xf1100000
#define THUMB_TST                         0x4200
#define THUMB2_TST                    0xea100000
#define THUMB2_TSTI                   0xf0100000
#define THUMB_BLX                         0x4780
#define THUMB_BX                          0x4700
#define THUMB_CC_B                        0xd000
#define THUMB_B                           0xe000
#define THUMB2_CC_B                   0xf0008000
#define THUMB2_B                      0xf0009000
#define THUMB2_BLI                    0xf000d000
#define THUMB2_BLXI                   0xf000c000
#define THUMB2_P                      0x00000400
#define THUMB2_U                      0x00000200
#define THUMB_LDRSB                       0x5600
#define THUMB2_LDRSB                  0xf9100000
#define THUMB2_LDRSBI                 0xf9100c00
#define THUMB2_LDRSBWI                0xf9900000
#define THUMB_LDRB                        0x5c00
#define THUMB2_LDRB                   0xf8100000
#define THUMB_LDRBI                       0x7800
#define THUMB2_LDRBI                  0xf8100c00
#define THUMB2_LDRBWI                 0xf8900000
#define THUMB_LDRSH                       0x5e00
#define THUMB2_LDRSH                  0xf9300000
#define THUMB2_LDRSHI                 0xf9300c00
#define THUMB2_LDRSHWI                0xf9b00000
#define THUMB_LDRH                        0x5a00
#define THUMB2_LDRH                   0xf8300000
#define THUMB_LDRHI                       0x8800
#define THUMB2_LDRHI                  0xf8300c00
#define THUMB2_LDRHWI                 0xf8b00000
#define THUMB_LDR                         0x5800
#define THUMB2_LDR                    0xf8500000
#define THUMB2_LDRP                   0xf85f0000
#define THUMB_LDRI                        0x6800
#define THUMB_LDRISP                      0x9800
#define THUMB2_LDRI                   0xf8500c00
#define THUMB2_LDRWI                  0xf8d00000
#define THUMB_STRB                        0x5400
#define THUMB2_STRB                   0xf8000000
#define THUMB_STRBI                       0x7000
#define THUMB2_STRBI                  0xf8000c00
#define THUMB2_STRBWI                 0xf8800000
#define THUMB_STRH                        0x5200
#define THUMB2_STRH                   0xf8200000
#define THUMB_STRHI                       0x8000
#define THUMB2_STRHI                  0xf8200c00
#define THUMB2_STRHWI                 0xf8a00000
#define THUMB_STR                         0x5000
#define THUMB2_STR                    0xf8400000
#define THUMB_STRI                        0x6000
#define THUMB2_STRWI                   0xf8c00000
#define THUMB_STRISP                      0x9000
#define THUMB2_STRI                   0xf8400c00
#define THUMB2_LDM_W                  0x00200000
#define THUMB2_PUSH                   0xe92d0000

#define _NOREG (jit_gpr_regno(_PC))

static void
emit_wide_thumb(jit_state_t *_jit, uint32_t inst)
{
  emit_u16(_jit, inst >> 16);
  emit_u16_with_pool(_jit, inst & 0xffff);
}

/* from binutils */
#  define rotate_left(v, n)     (v << n | v >> (32 - n))
static int
encode_arm_immediate(unsigned int v)
{
  unsigned int        a, i;

  for (i = 0; i < 32; i += 2)
    if ((a = rotate_left(v, i)) <= 0xff)
      return (a | (i << 7));

  return (-1);
}

static int
encode_thumb_immediate(unsigned int v)
{
  int                 i;
  unsigned int        m;
  unsigned int        n;
  /* 00000000 00000000 00000000 abcdefgh */
  if ((v & 0xff) == v)
    return (v);
  /* 00000000 abcdefgh 00000000 abcdefgh */
  if ((v & 0xff00ff) == v && ((v & 0xff0000) >> 16) == (v & 0xff))
    return ((v & 0xff) | (1 << 12));
  /* abcdefgh 00000000 abcdefgh 00000000 */
  if (((v & 0xffff0000) >> 16) == (v & 0xffff) && (v & 0xff) == 0)
    return ((v & 0x000000ff) | (2 << 12));
  /* abcdefgh abcdefgh abcdefgh abcdefgh */
  if ( (v &    0xff)        == ((v &     0xff00) >>  8) &&
       ((v &   0xff00) >> 8) == ((v &   0xff0000) >> 16) &&
       ((v & 0xff0000) << 8) ==  (v & 0xff000000))
    return ((v & 0xff) | (3 << 12));
  /* 1bcdefgh << 24 ... 1bcdefgh << 1 */
  for (i = 8, m = 0xff000000, n = 0x80000000;
       i < 23; i++, m >>= 1,  n >>= 1) {
    if ((v & m) == v && (v & n)) {
      v >>= 32 - i;
      if (!(i & 1))
        v &= 0x7f;
      i >>= 1;
      return (((i & 7) << 12) | ((i & 8) << 23) | v);
    }
  }
  return (-1);
}

static int
encode_thumb_word_immediate(unsigned int v)
{
  if ((v & 0xfffff000) == 0)
    return (((v & 0x800) << 15) | ((v & 0x700) << 4) | (v & 0xff));
  return (-1);
}

static uint32_t
read_wide_thumb(uint32_t *loc)
{
  uint16_t *sloc = (uint16_t*)loc;
  return (sloc[0] << 16) | sloc[1];
}

static void
write_wide_thumb(uint32_t *loc, uint32_t v)
{
  uint16_t *sloc = (uint16_t *)loc;
  sloc[0] = v >> 16;
  sloc[1] = v & 0xffff;
}

static int
offset_in_jmp_range(int32_t offset)
{
  return -0x800000 <= offset && offset <= 0x7fffff;
}

static int32_t
decode_thumb_jump(uint32_t v)
{
  uint32_t s  = (v >> 26) & 1;
  uint32_t j1 = (v >> 13) & 1;
  uint32_t j2 = (v >> 11) & 1;
  uint32_t i1 = s ? j1 : !j1;
  uint32_t i2 = s ? j2 : !j2;
  uint32_t hi = (v >> 16) & 0x3ff;
  uint32_t lo = v & 0x7ff;

  int32_t ret = s << 31;
  ret >>= 8;
  ret |= i1 << 22;
  ret |= i2 << 21;
  ret |= hi << 11;
  ret |= lo;
  return ret;
}

static const uint32_t thumb_jump_mask = 0xf800d000;

static uint32_t
encode_thumb_jump(int32_t v)
{
  ASSERT(offset_in_jmp_range(v));
  uint32_t s  = !!(v & 0x800000);
  uint32_t i1 = !!(v & 0x400000);
  uint32_t i2 = !!(v & 0x200000);
  uint32_t j1 = s ? i1 : !i1;
  uint32_t j2 = s ? i2 : !i2;
  uint32_t ret = (s<<26)|((v&0x1ff800)<<5)|(j1<<13)|(j2<<11)|(v&0x7ff);
  ASSERT(decode_thumb_jump(ret) == v);
  ASSERT((ret & thumb_jump_mask) == 0);
  return ret;
}

static uint32_t
patch_thumb_jump(uint32_t inst, int32_t v)
{
  return (inst & thumb_jump_mask) | encode_thumb_jump(v);
}

static int32_t
read_jmp_offset(uint32_t *loc)
{
  return decode_thumb_jump(read_wide_thumb(loc));
}

static void
patch_jmp_offset(uint32_t *loc, int32_t v)
{
  write_wide_thumb(loc, patch_thumb_jump(read_wide_thumb(loc), v));
}

static jit_reloc_t
emit_thumb_jump(jit_state_t *_jit, uint32_t inst)
{
  uint8_t *pc_base = _jit->pc.uc + 4;
  uint8_t rsh = 1;
  int32_t off = (_jit->pc.uc - pc_base) >> rsh;
  jit_reloc_t ret =
    jit_reloc (_jit, JIT_RELOC_JMP_WITH_VENEER, 0, _jit->pc.uc, pc_base, rsh);
  uint8_t thumb_jump_width = 24;
  add_pending_literal(_jit, ret, thumb_jump_width - 1);
  emit_wide_thumb(_jit, patch_thumb_jump(inst, off));
  return ret;
}

static int
offset_in_jcc_range(int32_t v)
{
  return -0x80000 <= v && v <= 0x7ffff;
}

static int32_t
decode_thumb_cc_jump(uint32_t v)
{
  uint32_t s  = (v >> 26) & 1;
  uint32_t j1 = (v >> 13) & 1;
  uint32_t j2 = (v >> 11) & 1;
  uint32_t hi = (v >> 16) & 0x3f;
  uint32_t lo = v & 0x7ff;

  int32_t ret = s << 31;
  ret >>= 12;
  ret |= j2 << 18;
  ret |= j1 << 17;
  ret |= hi << 11;
  ret |= lo;
  return ret;
}

static const uint32_t thumb_cc_jump_mask = 0xfbc0d000;

static uint32_t
encode_thumb_cc_jump(int32_t v)
{
  ASSERT(offset_in_jcc_range(v));
  uint32_t s  = !!(v & 0x80000);
  uint32_t j2 = !!(v & 0x40000);
  uint32_t j1 = !!(v & 0x20000);
  uint32_t hi = (v >> 11) & 0x3f;
  uint32_t lo = v & 0x7ff;
  uint32_t ret = (s<<26)|(hi << 16)|(j1<<13)|(j2<<11)|lo;
  ASSERT(decode_thumb_cc_jump(ret) == v);
  ASSERT((ret & thumb_cc_jump_mask) == 0);
  return ret;
}

static uint32_t
patch_thumb_cc_jump(uint32_t inst, int32_t v)
{
  return (inst & thumb_cc_jump_mask) | encode_thumb_cc_jump(v);
}

static int32_t
read_jcc_offset(uint32_t *loc)
{
  return decode_thumb_cc_jump(read_wide_thumb(loc));
}

static void
patch_jcc_offset(uint32_t *loc, int32_t v)
{
  write_wide_thumb(loc, patch_thumb_cc_jump(read_wide_thumb(loc), v));
}

static jit_reloc_t
emit_thumb_cc_jump(jit_state_t *_jit, uint32_t inst)
{
  uint8_t *pc_base = _jit->pc.uc + 4;
  uint8_t rsh = 1;
  int32_t off = (_jit->pc.uc - pc_base) >> rsh;
  jit_reloc_t ret =
    jit_reloc (_jit, JIT_RELOC_JCC_WITH_VENEER, 0, _jit->pc.uc, pc_base, rsh);
  uint8_t thumb_cc_jump_width = 20;
  add_pending_literal(_jit, ret, thumb_cc_jump_width - 1);
  emit_wide_thumb(_jit, patch_thumb_cc_jump(inst, off));
  return ret;
}

static void
torrr(jit_state_t *_jit, int o, int rn, int rd, int rm)
{
  ASSERT(!(o & 0xf0f0f));
  emit_wide_thumb(_jit, o|(_u4(rn)<<16)|(_u4(rd)<<8)|_u4(rm));
}

static void
torxr(jit_state_t *_jit, int o, int rn, int rt, int rm)
{
  ASSERT(!(o & 0xf0f0f));
  emit_wide_thumb(_jit, o|(_u4(rn)<<16)|(_u4(rt)<<12)|_u4(rm));
}

static void
torrrr(jit_state_t *_jit, int o, int rn, int rl, int rh, int rm)
{
  ASSERT(!(o & 0x000fff0f));
  emit_wide_thumb(_jit, o|(_u4(rn)<<16)|(_u4(rl)<<12)|(_u4(rh)<<8)|_u4(rm));
}

static void
torri(jit_state_t *_jit, int o, int rn, int rd, int im)
{
  ASSERT(!(o  & 0x0c0f7fff));
  ASSERT(!(im & 0xfbff8f00));
  emit_wide_thumb(_jit, o|(_u4(rn)<<16)|(_u4(rd)<<8)|im);
}

static void
torri8(jit_state_t *_jit, int o, int rn, int rt, int im)
{
  ASSERT(!(o  & 0x000ff0ff));
  ASSERT(!(im & 0xffffff00));
  emit_wide_thumb(_jit, o|(_u4(rn)<<16)|(_u4(rt)<<12)|im);
}

static void
torri12(jit_state_t *_jit, int o, int rn, int rt, int im)
{
  ASSERT(!(o  & 0x000fffff));
  ASSERT(!(im & 0xfffff000));
  emit_wide_thumb(_jit, o|(_u4(rn)<<16)|(_u4(rt)<<12)|im);
}

static void
tshift(jit_state_t *_jit, int o, int rd, int rm, int im)
{
  ASSERT(!(o & 0x7fcf));
  ASSERT(im >= 0 && im < 32);
  emit_wide_thumb(_jit, o|((im&0x1c)<<10)|(_u4(rd)<<8)|((im&3)<<6)|_u4(rm));
}

static void
toriw(jit_state_t *_jit, int o, int rd, int im)
{
  ASSERT(!(im & 0xffff0000));
  emit_wide_thumb(_jit, o|((im&0xf000)<<4)|((im&0x800)<<15)|((im&0x700)<<4)|(_u4(rd)<<8)|(im&0xff));
}

static jit_reloc_t
tcb(jit_state_t *_jit, int cc)
{
  ASSERT(!(cc & 0xfffffff));
  ASSERT(cc != ARM_CC_AL && cc != ARM_CC_NV);
  cc = ((uint32_t)cc) >> 6;
  return emit_thumb_cc_jump(_jit, THUMB2_CC_B|cc);
}

static jit_reloc_t
tb(jit_state_t *_jit, int o)
{
  ASSERT(!(o & 0x07ff2fff));
  return emit_thumb_jump(_jit, o);
}

static void
T1_ORR(jit_state_t *_jit, int32_t rdn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_ORR|(_u3(rm)<<3)|_u3(rdn));
}

static void
T2_ORR(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_ORR,rn,rd,rm);
}

static void
T2_ORRI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_ORRI,rn,rd,im);
}

static void
T1_EOR(jit_state_t *_jit, int32_t rdn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_EOR|(_u3(rm)<<3)|_u3(rdn));
}

static void
T2_EOR(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_EOR,rn,rd,rm);
}

static void
T2_EORI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_EORI,rn,rd,im);
}

static void
T1_MOV(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_MOV|((_u4(rd)&8)<<4)|(_u4(rm)<<3)|(rd&7));
}

static void
T1_MOVI(jit_state_t *_jit, int32_t rd, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_MOVI|(_u3(rd)<<8)|_u8(im));
}

static void
T2_MOVI(jit_state_t *_jit, int32_t rd, int32_t im)
{
  return torri(_jit, THUMB2_MOVI,_NOREG,rd,im);
}

static void
T2_MOVWI(jit_state_t *_jit, int32_t rd, int32_t im)
{
  return toriw(_jit, THUMB2_MOVWI,rd,im);
}

static void
T2_MOVTI(jit_state_t *_jit, int32_t rd, int32_t im)
{
  return toriw(_jit, THUMB2_MOVTI,rd,im);
}

static void
T1_MVN(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_MVN|(_u3(rm)<<3)|_u3(rd));
}

static void
T2_MVN(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  return torrr(_jit, THUMB2_MVN,_NOREG,rd,rm);
}

static void
T2_MVNI(jit_state_t *_jit, int32_t rd, int32_t im)
{
  return torri(_jit, THUMB2_MVNI,_NOREG,rd,im);
}

static void
T1_NOT(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  return T1_MVN(_jit, rd,rm);
}

static void
T2_NOT(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  return T2_MVN(_jit, rd,rm);
}

static void
T1_NOP(jit_state_t *_jit)
{
  emit_u16_with_pool(_jit, 0xbf00);
}

static void
T1_ADD(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_ADD|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rd));
}

static void
T1_ADDX(jit_state_t *_jit, int32_t rdn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_ADDX|((_u4(rdn)&8)<<4)|(_u4(rm)<<3)|(rdn&7));
}

static void
T2_ADD(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_ADD,rn,rd,rm);
}

static void
T1_ADDI3(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_ADDI3|(_u3(im)<<6)|(_u3(rn)<<3)|_u3(rd));
}

static void
T1_ADDI8(jit_state_t *_jit, int32_t rdn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_ADDI8|(_u3(rdn)<<8)|_u8(im));
}

static void
T2_ADDI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_ADDI,rn,rd,im);
}

static void
T2_ADDWI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_ADDWI,rn,rd,im);
}

static void
T2_ADDS(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_ADD|ARM_S,rn,rd,rm);
}

static void
T2_ADDSI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_ADDI|ARM_S,rn,rd,im);
}

static void
T1_ADC(jit_state_t *_jit, int32_t rdn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_ADC|(_u3(rm)<<3)|_u3(rdn));
}

static void
T2_ADCS(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_ADC|ARM_S,rn,rd,rm);
}

static void
T2_ADCSI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_ADCI|ARM_S,rn,rd,im);
}

static void
T1_SUB(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_SUB|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rd));
}

static void
T2_SUB(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_SUB,rn,rd,rm);
}

static void
T1_SUBI3(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_SUBI3|(_u3(im)<<6)|(_u3(rn)<<3)|_u3(rd));
}

static void
T1_SUBI8(jit_state_t *_jit, int32_t rdn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_SUBI8|(_u3(rdn)<<8)|_u8(im));
}

static void
T2_SUBI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_SUBI,rn,rd,im);
}

static void
T2_SUBWI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_SUBWI,rn,rd,im);
}

static void
T2_SUBS(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_SUB|ARM_S,rn,rd,rm);
}

static void
T2_SUBSI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_SUBI|ARM_S,rn,rd,im);
}

static void
T1_SBC(jit_state_t *_jit, int32_t rdn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_SBC|(_u3(rm)<<3)|_u3(rdn));
}

static void
T2_SBCS(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_SBC|ARM_S,rn,rd,rm);
}

static void
T2_SBCSI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_SBCI|ARM_S,rn,rd,im);
}

static void
T1_RSBI(jit_state_t *_jit, int32_t rd, int32_t rn)
{
  emit_u16_with_pool(_jit, THUMB_RSBI|(_u3(rn)<<3)|_u3(rd));
}

static void
T2_RSBI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_RSBI,rn,rd,im);
}

static void
T1_MUL(jit_state_t *_jit, int32_t rdm, int32_t rn)
{
  emit_u16_with_pool(_jit, THUMB_MUL|(_u3(rn)<<3)|_u3(rdm));
}

static void
T2_MUL(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_MUL,rn,rd,rm);
}

static void
T2_SMULL(jit_state_t *_jit, int32_t rl, int32_t rh, int32_t rn, int32_t rm)
{
  return torrrr(_jit, THUMB2_SMULL,rn,rl,rh,rm);
}

static void
T2_UMULL(jit_state_t *_jit, int32_t rl, int32_t rh, int32_t rn, int32_t rm)
{
  return torrrr(_jit, THUMB2_UMULL,rn,rl,rh,rm);
}

static void
T2_SDIV(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_SDIV,rn,rd,rm);
}

static void
T2_UDIV(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_UDIV,rn,rd,rm);
}

static void
T1_MLS(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm, int32_t ra)
{
  return torrrr(_jit, THUMB_MLS, rn, ra, rd, rm);
}

static void
T1_AND(jit_state_t *_jit, int32_t rdn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_AND|(_u3(rm)<<3)|_u3(rdn));
}

static void
T2_AND(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_AND,rn,rd,rm);
}

static void
T2_ANDI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_ANDI,rn,rd,im);
}

static void
T2_BICI(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_BICI,rn,rd,im);
}

static void
T1_REV(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_REV|(_u3(rm)<<3)|_u3(rd));
}

static void
T2_REV(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  return torrr(_jit, THUMB2_REV,rm,rd,rm);
}

static void
T1_SXTB(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_SXTB|(_u3(rm)<<3)|_u3(rd));
}

static void
T2_SXTB(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  return torrr(_jit, THUMB2_SXTB,_NOREG,rd,rm);
}

static void
T1_UXTB(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_UXTB|(_u3(rm)<<3)|_u3(rd));
}

static void
T2_UXTB(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  return torrr(_jit, THUMB2_UXTB,_NOREG,rd,rm);
}

static void
T1_SXTH(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_SXTH|(_u3(rm)<<3)|_u3(rd));
}

static void
T2_SXTH(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  return torrr(_jit, THUMB2_SXTH,_NOREG,rd,rm);
}

static void
T1_UXTH(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_UXTH|(_u3(rm)<<3)|_u3(rd));
}

static void
T2_UXTH(jit_state_t *_jit, int32_t rd, int32_t rm)
{
  return torrr(_jit, THUMB2_UXTH,_NOREG,rd,rm);
}

static void
T1_LSL(jit_state_t *_jit, int32_t rdn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_LSL|(_u3(rm)<<3)|_u3(rdn));
}

static void
T2_LSL(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_LSL,rn,rd,rm);
}

static void
T1_LSLI(jit_state_t *_jit, int32_t rd, int32_t rm, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_LSLI|(_u5(im)<<6)|(_u3(rm)<<3)|_u3(rd));
}

static void
T2_LSLI(jit_state_t *_jit, int32_t rd, int32_t rm, int32_t im)
{
  return tshift(_jit, THUMB2_LSLI,rd,rm,im);
}

static void
T1_LSR(jit_state_t *_jit, int32_t rdn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_LSR|(_u3(rm)<<3)|_u3(rdn));
}

static void
T2_LSR(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_LSR,rn,rd,rm);
}

static void
T1_LSRI(jit_state_t *_jit, int32_t rd, int32_t rm, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_LSRI|(_u5(im)<<6)|(_u3(rm)<<3)|_u3(rd));
}

static void
T2_LSRI(jit_state_t *_jit, int32_t rd, int32_t rm, int32_t im)
{
  return tshift(_jit, THUMB2_LSRI,rd,rm,im);
}

static void
T1_ASR(jit_state_t *_jit, int32_t rdn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_ASR|(_u3(rm)<<3)|_u3(rdn));
}

static void
T2_ASR(jit_state_t *_jit, int32_t rd, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_ASR,rn,rd,rm);
}

static void
T1_ASRI(jit_state_t *_jit, int32_t rd, int32_t rm, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_ASRI|(_u5(im)<<6)|(_u3(rm)<<3)|_u3(rd));
}

static void
T2_ASRI(jit_state_t *_jit, int32_t rd, int32_t rm, int32_t im)
{
  return tshift(_jit, THUMB2_ASRI,rd,rm,im);
}

static void
T1_CMP(jit_state_t *_jit, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_CMP|(_u3(rm)<<3)|_u3(rn));
}

static void
T1_CMPX(jit_state_t *_jit, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_CMPX|((_u4(rn)&8)<<4)|(_u4(rm)<<3)|(rn&7));
}

static void
T2_CMP(jit_state_t *_jit, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_CMP,rn,_NOREG,rm);
}

static void
T1_CMPI(jit_state_t *_jit, int32_t rn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_CMPI|(_u3(rn)<<8)|_u8(im));
}

static void
T2_CMPI(jit_state_t *_jit, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_CMPI,rn,_NOREG,im);
}

static void
T2_CMNI(jit_state_t *_jit, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_CMNI,rn,_NOREG,im);
}

static void
T1_TST(jit_state_t *_jit, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_TST|(_u3(rm)<<3)|_u3(rn));
}

static void
T2_TST(jit_state_t *_jit, int32_t rn, int32_t rm)
{
  return torrr(_jit, THUMB2_TST,rn,_NOREG,rm);
}

static void
T2_TSTI(jit_state_t *_jit, int32_t rn, int32_t im)
{
  return torri(_jit, THUMB2_TSTI,rn,_NOREG,im);
}

static void
T1_BLX(jit_state_t *_jit, int32_t r0)
{
  emit_u16_with_pool(_jit, THUMB_BLX|(_u4(r0)<<3));
}

static void
T1_BX(jit_state_t *_jit, int32_t r0)
{
  emit_u16_with_pool(_jit, THUMB_BX|(_u4(r0)<<3));
}

static jit_reloc_t
T2_CC_B(jit_state_t *_jit, uint32_t cc)
{
  return tcb(_jit, cc);
}

static jit_reloc_t
T2_B(jit_state_t *_jit)
{
  return tb(_jit, THUMB2_B);
}

static jit_reloc_t
T2_BLI(jit_state_t *_jit)
{
  return tb(_jit, THUMB2_BLI);
}

static jit_reloc_t
T2_BLXI(jit_state_t *_jit)
{
  return tb(_jit, THUMB2_BLXI);
}

static void
T1_LDRSB(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_LDRSB|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_LDRSB(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  return torxr(_jit, THUMB2_LDRSB,rn,rt,rm);
}

static void
T2_LDRSBI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRSBI|THUMB2_U,rn,rt,im);
}

static void
T2_LDRSBWI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri12(_jit, THUMB2_LDRSBWI,rn,rt,im);
}

static void
T2_LDRSBIN(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRSBI,rn,rt,im);
}

static void
T1_LDRB(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_LDRB|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_LDRB(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  return torxr(_jit, THUMB2_LDRB,rn,rt,rm);
}

static void
T1_LDRBI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_LDRBI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_LDRBI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRBI|THUMB2_U,rn,rt,im);
}

static void
T2_LDRBWI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri12(_jit, THUMB2_LDRBWI,rn,rt,im);
}

static void
T2_LDRBIN(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRBI,rn,rt,im);
}

static void
T1_LDRSH(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_LDRSH|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_LDRSH(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  return torxr(_jit, THUMB2_LDRSH,rn,rt,rm);
}

static void
T2_LDRSHI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRSHI|THUMB2_U,rn,rt,im);
}

static void
T2_LDRSHWI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri12(_jit, THUMB2_LDRSHWI,rn,rt,im);
}

static void
T2_LDRSHIN(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRSHI,rn,rt,im);
}

static void
T1_LDRH(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_LDRH|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_LDRH(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  return torxr(_jit, THUMB2_LDRH,rn,rt,rm);
}

static void
T1_LDRHI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_LDRHI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_LDRHI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRHI|THUMB2_U,rn,rt,im);
}

static void
T2_LDRHWI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri12(_jit, THUMB2_LDRHWI,rn,rt,im);
}

static void
T2_LDRHIN(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRHI,rn,rt,im);
}

static void
T1_LDR(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_LDR|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_LDR(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  return torxr(_jit, THUMB2_LDR,rn,rt,rm);
}

static void
T1_LDRI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_LDRI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T1_LDRISP(jit_state_t *_jit, int32_t rt, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_LDRISP|(_u3(rt)<<8)|_u8(im));
}

static void
T2_LDRI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRI|THUMB2_U,rn,rt,im);
}

static void
T2_LDRWI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri12(_jit, THUMB2_LDRWI,rn,rt,im);
}

static void
T2_LDRIN(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_LDRI,rn,rt,im);
}

static void
T1_STRB(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_STRB|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_STRB(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  return torxr(_jit, THUMB2_STRB,rn,rt,rm);
}

static void
T1_STRBI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_STRBI | (_u5(im) << 6) | (_u3(rn) << 3) | _u3(rt));
}

static void
T2_STRBI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_STRBI|THUMB2_U,rn,rt,im);
}

static void
T2_STRBWI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri12(_jit, THUMB2_STRBWI,rn,rt,im);
}

static void
T2_STRBIN(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_STRBI,rn,rt,im);
}

static void
T1_STRH(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_STRH|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_STRH(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  return torxr(_jit, THUMB2_STRH,rn,rt,rm);
}

static void
T1_STRHI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_STRHI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_STRHI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_STRHI|THUMB2_U,rn,rt,im);
}

static void
T2_STRHWI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri12(_jit, THUMB2_STRHWI,rn,rt,im);
}

static void
T2_STRHIN(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_STRHI,rn,rt,im);
}

static void
T1_STR(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  emit_u16_with_pool(_jit, THUMB_STR|(_u3(rm)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T2_STR(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t rm)
{
  return torxr(_jit, THUMB2_STR,rn,rt,rm);
}

static void
T1_STRI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_STRI|(_u5(im)<<6)|(_u3(rn)<<3)|_u3(rt));
}

static void
T1_STRISP(jit_state_t *_jit, int32_t rt, int32_t im)
{
  emit_u16_with_pool(_jit, THUMB_STRISP|(_u3(rt)<<8)|(_u8(im)));
}

static void
T2_STRI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_STRI|THUMB2_U,rn,rt,im);
}

static void
T2_STRWI(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri12(_jit, THUMB2_STRWI,rn,rt,im);
}

static void
T2_STRIN(jit_state_t *_jit, int32_t rt, int32_t rn, int32_t im)
{
  return torri8(_jit, THUMB2_STRI,rn,rt,im);
}

static void
nop(jit_state_t *_jit, int32_t i0)
{
  for (; i0 > 0; i0 -= 2)
    T1_NOP(_jit);
    
  ASSERT(i0 == 0);
}

static void
movr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1) {
    T1_MOV(_jit, r0, r1);
  }
}

enum preserve_flags { PRESERVE_FLAGS, FLAGS_UNIMPORTANT };

static void
_movi(jit_state_t *_jit, int32_t r0, jit_word_t i0, enum preserve_flags flags)
{
  int                 i;
    
  if (flags == PRESERVE_FLAGS && r0 < 8 && !(i0 & 0xffffff80))
    T1_MOVI(_jit, r0, i0);
  else if (r0 < 8 && !(i0 & 0xffffff80))
    T1_MOVI(_jit, r0, i0);
  else if ((i = encode_thumb_immediate(i0)) != -1)
    T2_MOVI(_jit, r0, i);
  else if ((i = encode_thumb_immediate(~i0)) != -1)
    T2_MVNI(_jit, r0, i);
  else {
    T2_MOVWI(_jit, r0, (uint16_t)i0);
    if (i0 & 0xffff0000)
      T2_MOVTI(_jit, r0, (uint16_t)((unsigned)i0 >> 16));
  }
}

static void
movi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  return _movi(_jit, r0, i0, FLAGS_UNIMPORTANT);
}

static int
offset_in_load_from_pool_range(int32_t offset)
{
  return -0xfff <= offset && offset <= 0xfff;
}

static int32_t
decode_load_from_pool_offset(uint32_t inst)
{
  int32_t ret = inst & 0xfff;
  return ((inst >> 23) & 1) ? ret : -ret;
}

static uint32_t
encode_load_from_pool_offset(int32_t off)
{
  ASSERT(offset_in_load_from_pool_range(off));
  uint32_t u = off >= 0;
  uint32_t ret = ((u ? off : -off) & 0xfff) | (u << 23);
  ASSERT(decode_load_from_pool_offset(ret) == off);
  return ret;
}

static uint32_t
patch_load_from_pool(uint32_t inst, int32_t off)
{
  uint32_t load_from_pool_mask = THUMB2_LDRP | (0xf << 12);
  return (inst & load_from_pool_mask) | encode_load_from_pool_offset(off);
}

static int32_t
read_load_from_pool_offset(uint32_t *loc)
{
  return decode_load_from_pool_offset(read_wide_thumb(loc));
}

static void
patch_load_from_pool_offset(uint32_t *loc, int32_t v)
{
  write_wide_thumb(loc, patch_load_from_pool(read_wide_thumb(loc), v));
}

static jit_reloc_t
emit_load_from_pool(jit_state_t *_jit, uint32_t inst)
{
  uint8_t *pc_base = (uint8_t *)((_jit->pc.w + 4) & ~3);
  uint8_t rsh = 0;
  int32_t off = (_jit->pc.uc - pc_base) >> rsh;
  jit_reloc_t ret =
    jit_reloc (_jit, JIT_RELOC_LOAD_FROM_POOL, 0, _jit->pc.uc, pc_base, rsh);
  uint8_t load_from_pool_width = 12;
  add_pending_literal(_jit, ret, load_from_pool_width);
  emit_wide_thumb(_jit, patch_load_from_pool(inst, off));
  return ret;
}  

static jit_reloc_t
movi_from_pool(jit_state_t *_jit, int32_t Rt)
{
  return emit_load_from_pool(_jit, THUMB2_LDRP | (_u4(Rt) << 12));
}

static jit_reloc_t
mov_addr(jit_state_t *_jit, int32_t r0)
{
  return movi_from_pool(_jit, r0);
}

static void
comr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_NOT(_jit, r0, r1);
  else
    T2_NOT(_jit, r0, r1);
}

static void
negr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_RSBI(_jit, r0, r1);
  else
    T2_RSBI(_jit, r0, r1, 0);
}

static void
addr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8)
    T1_ADD(_jit, r0, r1, r2);
  else if (r0 == r1 || r0 == r2)
    T1_ADDX(_jit, r0, r0 == r1 ? r2 : r1);
  else
    T2_ADD(_jit, r0, r1, r2);
}

static void
addi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int                 i;
    
  if ((r0|r1) < 8 && !(i0 & ~7))
    T1_ADDI3(_jit, r0, r1, i0);
  else if ((r0|r1) < 8 && !(-i0 & ~7))
    T1_SUBI3(_jit, r0, r1, -i0);
  else if (r0 < 8 && r0 == r1 && !(i0 & ~0xff))
    T1_ADDI8(_jit, r0, i0);
  else if (r0 < 8 && r0 == r1 && !(-i0 & ~0xff))
    T1_SUBI8(_jit, r0, -i0);
  else if ((i = encode_thumb_immediate(i0)) != -1)
    T2_ADDI(_jit, r0, r1, i);
  else if ((i = encode_thumb_immediate(-i0)) != -1)
    T2_SUBI(_jit, r0, r1, i);
  else if ((i = encode_thumb_word_immediate(i0)) != -1)
    T2_ADDWI(_jit, r0, r1, i);
  else if ((i = encode_thumb_word_immediate(-i0)) != -1)
    T2_SUBWI(_jit, r0, r1, i);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    T2_ADD(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
addcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  /* thumb auto set carry if not inside IT block */
  if ((r0|r1|r2) < 8)
    T1_ADD(_jit, r0, r1, r2);
  else
    T2_ADDS(_jit, r0, r1, r2);
}

static void
addci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int                 i;
    
  if ((r0|r1) < 8 && !(i0 & ~7))
    T1_ADDI3(_jit, r0, r1, i0);
  else if ((r0|r1) < 8 && !(-i0 & ~7))
    T1_SUBI3(_jit, r0, r1, -i0);
  else if (r0 < 8 && r0 == r1 && !(i0 & ~0xff))
    T1_ADDI8(_jit, r0, i0);
  else if (r0 < 8 && r0 == r1 && !(-i0 & ~0xff))
    T1_SUBI8(_jit, r0, -i0);
  else if ((i = encode_thumb_immediate(i0)) != -1)
    T2_ADDSI(_jit, r0, r1, i);
  else if ((i = encode_thumb_immediate(-i0)) != -1)
    T2_SUBSI(_jit, r0, r1, i);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    T2_ADDS(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
addxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  /* keep setting carry because don't know last ADC */
  
  /* thumb auto set carry if not inside IT block */
  if ((r0|r1|r2) < 8 && (r0 == r1 || r0 == r2))
    T1_ADC(_jit, r0, r0 == r1 ? r2 : r1);
  else
    T2_ADCS(_jit, r0, r1, r2);
}

static void
addxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int                 i;
  if ((i = encode_thumb_immediate(i0)) != -1)
    T2_ADCSI(_jit, r0, r1, i);
  else if ((i = encode_thumb_immediate(-i0)) != -1)
    T2_SBCSI(_jit, r0, r1, i);
  else if (r0 != r1) {
    _movi(_jit, r0, i0, PRESERVE_FLAGS);
    T2_ADCS(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    _movi(_jit, jit_gpr_regno(reg), i0, PRESERVE_FLAGS);
    T2_ADCS(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
subr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8)
    T1_SUB(_jit, r0, r1, r2);
  else
    T2_SUB(_jit, r0, r1, r2);
}

static void
subi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int                 i;
    
  if ((r0|r1) < 8 && !(i0 & ~7))
    T1_SUBI3(_jit, r0, r1, i0);
  else if ((r0|r1) < 8 && !(-i0 & ~7))
    T1_ADDI3(_jit, r0, r1, -i0);
  else if (r0 < 8 && r0 == r1 && !(i0 & ~0xff))
    T1_SUBI8(_jit, r0, i0);
  else if (r0 < 8 && r0 == r1 && !(-i0 & ~0xff))
    T1_ADDI8(_jit, r0, -i0);
  else if ((i = encode_thumb_immediate(i0)) != -1)
    T2_SUBI(_jit, r0, r1, i);
  else if ((i = encode_thumb_immediate(-i0)) != -1)
    T2_ADDI(_jit, r0, r1, i);
  else if ((i = encode_thumb_word_immediate(i0)) != -1)
    T2_SUBWI(_jit, r0, r1, i);
  else if ((i = encode_thumb_word_immediate(-i0)) != -1)
    T2_ADDWI(_jit, r0, r1, i);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    T2_SUB(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
subcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  /* thumb auto set carry if not inside IT block */
  if ((r0|r1|r2) < 8)
    T1_SUB(_jit, r0, r1, r2);
  else
    T2_SUBS(_jit, r0, r1, r2);
}

static void
subci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int                 i;
    
  if ((r0|r1) < 8 && !(i0 & ~7))
    T1_SUBI3(_jit, r0, r1, i0);
  else if ((r0|r1) < 8 && !(-i0 & ~7))
    T1_ADDI3(_jit, r0, r1, -i0);
  else if (r0 < 8 && r0 == r1 && !(i0 & ~0xff))
    T1_SUBI8(_jit, r0, i0);
  else if (r0 < 8 && r0 == r1 && !(-i0 & ~0xff))
    T1_ADDI8(_jit, r0, -i0);
  else if ((i = encode_thumb_immediate(i0)) != -1)
    T2_SUBSI(_jit, r0, r1, i);
  else if ((i = encode_thumb_immediate(-i0)) != -1)
    T2_ADDSI(_jit, r0, r1, i);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    T2_SUBS(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
subxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  /* keep setting carry because don't know last SBC */
  
  /* thumb auto set carry if not inside IT block */
  if ((r0|r1|r2) < 8 && r0 == r1)
    T1_SBC(_jit, r0, r2);
  else
    T2_SBCS(_jit, r0, r1, r2);
}

static void
subxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int                 i;
  if ((i = encode_arm_immediate(i0)) != -1)
    T2_SBCSI(_jit, r0, r1, i);
  else if ((i = encode_arm_immediate(-i0)) != -1)
    T2_ADCSI(_jit, r0, r1, i);
  else if (r0 != r1) {
    _movi(_jit, r0, i0, PRESERVE_FLAGS);
    T2_SBCS(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    _movi(_jit, jit_gpr_regno(reg), i0, PRESERVE_FLAGS);
    T2_SBCS(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
mulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r2 && (r0|r1) < 8)
    T1_MUL(_jit, r0, r1);
  else if (r0 == r1 && (r0|r2) < 8)
    T1_MUL(_jit, r0, r2);
  else
    T2_MUL(_jit, r0, r1, r2);
}

static void
muli(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  mulr(_jit, r0, r1, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
iqmulr(jit_state_t *_jit, int32_t r0, int32_t r1,
        int32_t r2, int32_t r3, jit_bool_t sign)
{
  if (r2 == r3) {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movr(_jit, jit_gpr_regno(reg), r2);
    if (sign)
      T2_SMULL(_jit, r0, r1, jit_gpr_regno(reg), r2);
    else
      T2_UMULL(_jit, r0, r1, jit_gpr_regno(reg), r2);
    unget_temp_gpr(_jit);
  } else if (r0 != r2 && r1 != r2) {
    if (sign)
      T2_SMULL(_jit, r0, r1, r2, r3);
    else
      T2_UMULL(_jit, r0, r1, r2, r3);
  } else {
    if (sign)
      T2_SMULL(_jit, r0, r1, r3, r2);
    else
      T2_UMULL(_jit, r0, r1, r3, r2);
  }
}

static void
iqmuli(jit_state_t *_jit, int32_t r0, int32_t r1,
        int32_t r2, jit_word_t i0, jit_bool_t sign)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  iqmulr(_jit, r0, r1, r2, jit_gpr_regno(reg), sign);
  unget_temp_gpr(_jit);
}

static void
qmulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqmulr(_jit, r0,r1,r2,r3,1);
}

static void
qmulr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqmulr(_jit, r0,r1,r2,r3,0);
}

static void
qmuli(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t i0)
{
  return iqmuli(_jit, r0,r1,r2,i0,1);
}

static void
qmuli_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t i0)
{
  return iqmuli(_jit, r0,r1,r2,i0,0);
}

static void
divr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  T2_SDIV(_jit, r0, r1, r2);
}

static void
divi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  divr(_jit, r0, r1, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
divr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  T2_UDIV(_jit, r0, r1, r2);
}

static void
divi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  divr_u(_jit, r0, r1, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
iqdivr(jit_state_t *_jit, int32_t r0, int32_t r1,
        int32_t r2, int32_t r3, jit_bool_t sign)
{
  int need_tmp = r0 == r2 || r0 == r3;
  if (need_tmp) {
    int32_t tmp = jit_gpr_regno(get_temp_gpr(_jit));
    if (r0 == r2) {
      movr(_jit, tmp, r2);
      r2 = tmp;
    }
    if (r0 == r3) {
      if (r2 != r3)
        movr(_jit, tmp, r3);
      r3 = tmp;
    }
  }
  if (sign)
    divr(_jit, r0, r2, r3);
  else
    divr_u(_jit, r0, r2, r3);
  T1_MLS(_jit, r1, r3, r0, r2);
  if (need_tmp)
    unget_temp_gpr(_jit);
}

static void
iqdivi(jit_state_t *_jit, int32_t r0, int32_t r1,
        int32_t r2, jit_word_t i0, jit_bool_t sign)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  iqdivr(_jit, r0, r1, r2, jit_gpr_regno(reg), sign);
  unget_temp_gpr(_jit);
}

static void
qdivr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqdivr(_jit, r0,r1,r2,r3,1);
}

static void
qdivr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqdivr(_jit, r0,r1,r2,r3,0);
}

static void
qdivi(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t i0)
{
  return iqdivi(_jit, r0,r1,r2,i0,1);
}

static void
qdivi_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t i0)
{
  return iqdivi(_jit, r0,r1,r2,i0,0);
}

static void
iremr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_bool_t sign)
{
  return iqdivr(_jit, r0, r0, r1, r2, sign);
}

static void
remr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return iremr(_jit, r0, r1, r2, 1);
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
  return iremr(_jit, r0, r1, r2, 0);
}

static void
remi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  remr_u(_jit, r0, r1,jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
andr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8 && (r0 == r1 || r0 == r2))
    T1_AND(_jit, r0, r0 == r1 ? r2 : r1);
  else
    T2_AND(_jit, r0, r1, r2);
}

static void
andi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int                 i;
    
  if ((i = encode_thumb_immediate(i0)) != -1)
    T2_ANDI(_jit, r0, r1, i);
  else if ((i = encode_thumb_immediate(~i0)) != -1)
    T2_BICI(_jit, r0, r1, i);
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    T2_AND(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    T2_AND(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
orr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8 && (r0 == r1 || r0 == r2))
    T1_ORR(_jit, r0, r0 == r1 ? r2 : r1);
  else
    T2_ORR(_jit, r0, r1, r2);
}

static void
ori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int                 i;
    
  if ((i = encode_thumb_immediate(i0)) != -1)
    T2_ORRI(_jit, r0, r1, i);
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    T2_ORR(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    T2_ORR(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
xorr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8 && (r0 == r1 || r0 == r2))
    T1_EOR(_jit, r0, r0 == r1 ? r2 : r1);
  else
    T2_EOR(_jit, r0, r1, r2);
}

static void
xori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int                 i;
    
  if ((i = encode_thumb_immediate(i0)) != -1)
    T2_EORI(_jit, r0, r1, i);
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    T2_EOR(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    T2_EOR(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
lshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8 && r0 == r1)
    T1_LSL(_jit, r0, r2);
  else
    T2_LSL(_jit, r0, r1, r2);
}

static void
lshi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  ASSERT(i0 >= 0 && i0 <= 31);
  if (i0 == 0)
    movr(_jit, r0, r1);
  else {
    if ((r0|r1) < 8)
      T1_LSLI(_jit, r0, r1, i0);
    else
      T2_LSLI(_jit, r0, r1, i0);
  }
}

static void
rshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8 && r0 == r1)
    T1_ASR(_jit, r0, r2);
  else
    T2_ASR(_jit, r0, r1, r2);
}

static void
rshi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  ASSERT(i0 >= 0 && i0 <= 31);
  if (i0 == 0)
    movr(_jit, r0, r1);
  else {
    if ((r0|r1) < 8)
      T1_ASRI(_jit, r0, r1, i0);
    else
      T2_ASRI(_jit, r0, r1, i0);
  }
}

static void
rshr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8 && r0 == r1)
    T1_LSR(_jit, r0, r2);
  else
    T2_LSR(_jit, r0, r1, r2);
}

static void
rshi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  ASSERT(i0 >= 0 && i0 <= 31);
  if (i0 == 0)
    movr(_jit, r0, r1);
  else {
    if ((r0|r1) < 8)
      T1_LSRI(_jit, r0, r1, i0);
    else
      T2_LSRI(_jit, r0, r1, i0);
  }
}

static void
jmpr(jit_state_t *_jit, int32_t r0)
{
  T1_MOV(_jit, jit_gpr_regno(_PC), r0);
}

static jit_reloc_t
jmp(jit_state_t *_jit)
{
  return T2_B(_jit);
}

static void
jmpi(jit_state_t *_jit, jit_word_t i0)
{
  /* Strip thumb bit, if any.  */
  i0 &= ~1;
  return jit_patch_there(_jit, jmp(_jit), (void*)i0);
}

static jit_reloc_t
bccr(jit_state_t *_jit, int cc, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_CMP(_jit, r0, r1);
  else if ((r0&r1) & 8)
    T1_CMPX(_jit, r0, r1);
  else
    T2_CMP(_jit, r0, r1);
  return T2_CC_B(_jit, cc);
}

static jit_reloc_t
bcci(jit_state_t *_jit, int cc, int32_t r0, jit_word_t i1)
{
  int i;
  if (r0 < 7 && !(i1 & 0xffffff00))
    T1_CMPI(_jit, r0, i1);
  else if ((i = encode_thumb_immediate(i1)) != -1)
    T2_CMPI(_jit, r0, i);
  else if ((i = encode_thumb_immediate(-i1)) != -1)
    T2_CMNI(_jit, r0, i);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i1);
    T2_CMP(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
  return T2_CC_B(_jit, cc);
}

static jit_reloc_t
bltr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_LT, r0, r1);
}

static jit_reloc_t
blti(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_LT, r0, i1);
}

static jit_reloc_t
bltr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_LO, r0, r1);
}

static jit_reloc_t
blti_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_LO, r0, i1);
}

static jit_reloc_t
bler(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_LE, r0, r1);
}

static jit_reloc_t
blei(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_LE, r0, i1);
}

static jit_reloc_t
bler_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_LS, r0, r1);
}

static jit_reloc_t
blei_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_LS, r0, i1);
}

static jit_reloc_t
beqr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_EQ, r0, r1);
}

static jit_reloc_t
beqi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_EQ, r0, i1);
}

static jit_reloc_t
bger(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_GE, r0, r1);
}

static jit_reloc_t
bgei(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_GE, r0, i1);
}

static jit_reloc_t
bger_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_HS, r0, r1);
}

static jit_reloc_t
bgei_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_HS, r0, i1);
}

static jit_reloc_t
bgtr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_GT, r0, r1);
}

static jit_reloc_t
bgti(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_GT, r0, i1);
}

static jit_reloc_t
bgtr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_HI, r0, r1);
}

static jit_reloc_t
bgti_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_HI, r0, i1);
}

static jit_reloc_t
bner(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bccr(_jit, ARM_CC_NE, r0, r1);
}

static jit_reloc_t
bnei(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bcci(_jit, ARM_CC_NE, r0, i1);
}

static jit_reloc_t
baddr(jit_state_t *_jit, int cc, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_ADD(_jit, r0, r0, r1);
  else
    T2_ADDS(_jit, r0, r0, r1);
  return T2_CC_B(_jit, cc);
}

static jit_reloc_t
baddi(jit_state_t *_jit, int cc, int32_t r0, int i1)
{
  int i;
  if (r0 < 8 && !(i1 & ~7))
    T1_ADDI3(_jit, r0, r0, i1);
  else if (r0 < 8 && !(-i1 & ~7))
    T1_SUBI3(_jit, r0, r0, -i1);
  else if (r0 < 8 && !(i1 & ~0xff))
    T1_ADDI8(_jit, r0, i1);
  else if (r0 < 8 && !(-i1 & ~0xff))
    T1_SUBI8(_jit, r0, -i1);
  else if ((i = encode_thumb_immediate(i1)) != -1)
    T2_ADDSI(_jit, r0, r0, i);
  else if ((i = encode_thumb_immediate(-i1)) != -1)
    T2_SUBSI(_jit, r0, r0, i);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i1);
    T2_ADDS(_jit, r0, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
  return T2_CC_B(_jit, cc);
}

static jit_reloc_t
boaddr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return baddr(_jit, ARM_CC_VS, r0, r1);
}

static jit_reloc_t
boaddi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return baddi(_jit, ARM_CC_VS, r0, i1);
}

static jit_reloc_t
boaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return baddr(_jit, ARM_CC_HS, r0, r1);
}

static jit_reloc_t
boaddi_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return baddi(_jit, ARM_CC_HS, r0, i1);
}

static jit_reloc_t
bxaddr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return baddr(_jit, ARM_CC_VC, r0, r1);
}

static jit_reloc_t
bxaddi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return baddi(_jit, ARM_CC_VC, r0, i1);
}

static jit_reloc_t
bxaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return baddr(_jit, ARM_CC_LO, r0, r1);
}

static jit_reloc_t
bxaddi_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return baddi(_jit, ARM_CC_LO, r0, i1);
}

static jit_reloc_t
bsubr(jit_state_t *_jit, int cc, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_SUB(_jit, r0, r0, r1);
  else
    T2_SUBS(_jit, r0, r0, r1);
  return T2_CC_B(_jit, cc);
}

static jit_reloc_t
bsubi(jit_state_t *_jit, int cc, int32_t r0, int i1)
{
  int i;
  if (r0 < 8 && !(i1 & ~7))
    T1_SUBI3(_jit, r0, r0, i1);
  else if (r0 < 8 && !(-i1 & ~7))
    T1_ADDI3(_jit, r0, r0, -i1);
  else if (r0 < 8 && !(i1 & ~0xff))
    T1_SUBI8(_jit, r0, i1);
  else if (r0 < 8 && !(-i1 & ~0xff))
    T1_ADDI8(_jit, r0, -i1);
  else if ((i = encode_thumb_immediate(i1)) != -1)
    T2_SUBSI(_jit, r0, r0, i);
  else if ((i = encode_thumb_immediate(-i1)) != -1)
    T2_SUBSI(_jit, r0, r0, i);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i1);
    T2_SUBS(_jit, r0, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
  return T2_CC_B(_jit, cc);
}

static jit_reloc_t
bosubr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bsubr(_jit, ARM_CC_VS, r0, r1);
}

static jit_reloc_t
bosubi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bsubi(_jit, ARM_CC_VS, r0, i1);
}

static jit_reloc_t
bosubr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bsubr(_jit, ARM_CC_LO, r0, r1);
}

static jit_reloc_t
bosubi_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bsubi(_jit, ARM_CC_LO, r0, i1);
}

static jit_reloc_t
bxsubr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bsubr(_jit, ARM_CC_VC, r0, r1);
}

static jit_reloc_t
bxsubi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bsubi(_jit, ARM_CC_VC, r0, i1);
}

static jit_reloc_t
bxsubr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bsubr(_jit, ARM_CC_HS, r0, r1);
}

static jit_reloc_t
bxsubi_u(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bsubi(_jit, ARM_CC_HS, r0, i1);
}

static jit_reloc_t
bmxr(jit_state_t *_jit, int cc, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_TST(_jit, r0, r1);
  else
    T2_TST(_jit, r0, r1);
  return T2_CC_B(_jit, cc);
}

static jit_reloc_t
bmxi(jit_state_t *_jit, int cc, int32_t r0, jit_word_t i1)
{
  int i;
  if ((i = encode_thumb_immediate(i1)) != -1)
    T2_TSTI(_jit, r0, i);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i1);
    T2_TST(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
  return T2_CC_B(_jit, cc);
}

static jit_reloc_t
bmsr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bmxr(_jit, ARM_CC_NE, r0, r1);
}

static jit_reloc_t
bmsi(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bmxi(_jit, ARM_CC_NE, r0, i1);
}

static jit_reloc_t
bmcr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bmxr(_jit, ARM_CC_EQ, r0, r1);
}

static jit_reloc_t
bmci(jit_state_t *_jit, int32_t r0, int32_t i1)
{
  return bmxi(_jit, ARM_CC_EQ, r0, i1);
}

static void
ldr_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  T2_LDRSBI(_jit, r0, r1, 0);
}

static void
ldi_c(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  T2_LDRSBI(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
ldxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8)
    T1_LDRSB(_jit, r0, r1, r2);
  else
    T2_LDRSB(_jit, r0, r1, r2);
}

#define jit_ldrt_strt_p() 0

static void
ldxi_c(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
    
  if (jit_ldrt_strt_p() && i0 >= 0 && i0 <= 255)
    T2_LDRSBI(_jit, r0, r1, i0);
  else if (i0 < 0 && i0 >= -255)
    T2_LDRSBIN(_jit, r0, r1, -i0);
  else if (i0 >= 0 && i0 <= 4095)
    T2_LDRSBWI(_jit, r0, r1, i0);
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    if ((r0|r1) < 8)
      T1_LDRSB(_jit, r0, r1, r0);
    else
      T2_LDRSB(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    if ((r0|r1|jit_gpr_regno(reg)) < 8)
      T1_LDRSB(_jit, r0, r1, jit_gpr_regno(reg));
    else
      T2_LDRSB(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldr_uc(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  T2_LDRBI(_jit, r0, r1, 0);
}

static void
ldi_uc(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  T2_LDRBI(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
ldxr_uc(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8)
    T1_LDRB(_jit, r0, r1, r2);
  else
    T2_LDRB(_jit, r0, r1, r2);
}

static void
ldxi_uc(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if ((r0|r1) < 8 && i0 >= 0 && i0 < 0x20)
    T1_LDRBI(_jit, r0, r1, i0);
  else if (jit_ldrt_strt_p() && i0 >= 0 && i0 <= 255)
    T2_LDRBI(_jit, r0, r1, i0);
  else if (i0 < 0 && i0 >= -255)
    T2_LDRBIN(_jit, r0, r1, -i0);
  else if (i0 >= 0 && i0 <= 4095)
    T2_LDRBWI(_jit, r0, r1, i0);
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    if ((r0|r1) < 8)
      T1_LDRB(_jit, r0, r1, r0);
    else
      T2_LDRB(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    if ((r0|r1|jit_gpr_regno(reg)) < 8)
      T1_LDRB(_jit, r0, r1, jit_gpr_regno(reg));
    else
      T2_LDRB(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldr_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  T2_LDRSHI(_jit, r0, r1, 0);
}

static void
ldi_s(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  T2_LDRSHI(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
ldxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8)
    T1_LDRSH(_jit, r0, r1, r2);
  else
    T2_LDRSH(_jit, r0, r1, r2);
}

static void
ldxi_s(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (jit_ldrt_strt_p() && i0 >= 0 && i0 <= 255)
    T2_LDRSHI(_jit, r0, r1, i0);
  else if (i0 < 0 && i0 >= -255)
    T2_LDRSHIN(_jit, r0, r1, -i0);
  else if (i0 >= 0 && i0 <= 4095)
    T2_LDRSHWI(_jit, r0, r1, i0);
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    if ((r0|r1) < 8)
      T1_LDRSH(_jit, r0, r1, r0);
    else
      T2_LDRSH(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    if ((r0|r1|jit_gpr_regno(reg)) < 8)
      T1_LDRSH(_jit, r0, r1, jit_gpr_regno(reg));
    else
      T2_LDRSH(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  T2_LDRHI(_jit, r0, r1, 0);
}

static void
ldi_us(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  T2_LDRHI(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
ldxr_us(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  
  if ((r0|r1|r2) < 8)
    T1_LDRH(_jit, r0, r1, r2);
  else
    T2_LDRH(_jit, r0, r1, r2);
}

static void
ldxi_us(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if ((r0|r1) < 8 && i0 >= 0 && !(i0 & 1) && (i0 >> 1) < 0x20)
    T1_LDRHI(_jit, r0, r1, i0 >> 1);
  else if (jit_ldrt_strt_p() && i0 >= 0 && i0 <= 255)
    T2_LDRHI(_jit, r0, r1, i0);
  else if (i0 < 0 && i0 >= -255)
    T2_LDRHIN(_jit, r0, r1, -i0);
  else if (i0 >= 0 && i0 <= 4095)
    T2_LDRHWI(_jit, r0, r1, i0);
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    if ((r0|r1) < 8)
      T1_LDRH(_jit, r0, r1, r0);
    else
      T2_LDRH(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    if ((r0|r1|jit_gpr_regno(reg)) < 8)
      T1_LDRH(_jit, r0, r1, jit_gpr_regno(reg));
    else
      T2_LDRH(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldr_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  T2_LDRI(_jit, r0, r1, 0);
}

static void
ldi_i(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  T2_LDRI(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
ldxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8)
    T1_LDR(_jit, r0, r1, r2);
  else
    T2_LDR(_jit, r0, r1, r2);
}

static void
ldxi_i(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if ((r0|r1) < 8 && i0 >= 0 && !(i0 & 3) && (i0 >> 2) < 0x20)
    T1_LDRI(_jit, r0, r1, i0 >> 2);
  else if (r1 == jit_gpr_regno(JIT_SP) && r0 < 8 &&
           i0 >= 0 && !(i0 & 3) && (i0 >> 2) <= 255)
    T1_LDRISP(_jit, r0, i0 >> 2);
  else if (jit_ldrt_strt_p() && i0 >= 0 && i0 <= 255)
    T2_LDRI(_jit, r0, r1, i0);
  else if (i0 < 0 && i0 > -255)
    T2_LDRIN(_jit, r0, r1, -i0);
  else if (i0 >= 0 && i0 <= 4095)
    T2_LDRWI(_jit, r0, r1, i0);
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    if ((r0|r1) < 8)
      T1_LDR(_jit, r0, r1, r0);
    else
      T2_LDR(_jit, r0, r1, r0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    if ((r0|r1|jit_gpr_regno(reg)) < 8)
      T1_LDR(_jit, r0, r1, jit_gpr_regno(reg));
    else
      T2_LDR(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
str_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  T2_STRBI(_jit, r1, r0, 0);
}

static void
sti_c(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  T2_STRBI(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
stxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8)
    T1_STRB(_jit, r2, r1, r0);
  else
    T2_STRB(_jit, r2, r1, r0);
}

static void
stxi_c(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8 && i0 >= 0 && i0 < 0x20)
    T1_STRBI(_jit, r1, r0, i0);
  else if (jit_ldrt_strt_p() && i0 >= 0 && i0 <= 255)
    T2_STRBI(_jit, r1, r0, i0);
  else if (i0 < 0 && i0 >= -255)
    T2_STRBIN(_jit, r1, r0, -i0);
  else if (i0 >= 0 && i0 <= 4095)
    T2_STRBWI(_jit, r1, r0, i0);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    if ((r0|r1|jit_gpr_regno(reg)) < 8)
      T1_STRB(_jit, r1, r0, jit_gpr_regno(reg));
    else
      T2_STRB(_jit, r1, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
str_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  T2_STRHI(_jit, r1, r0, 0);
}

static void
sti_s(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  T2_STRHI(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
stxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8)
    T1_STRH(_jit, r2, r1, r0);
  else
    T2_STRH(_jit, r2, r1, r0);
}

static void
stxi_s(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8 && i0 >= 0 && !(i0 & 1) && (i0 >> 1) < 0x20)
    T1_STRHI(_jit, r1, r0, i0 >> 1);
  else if (jit_ldrt_strt_p() && i0 >= 0 && i0 <= 255)
    T2_STRHI(_jit, r1, r0, i0);
  else if (i0 < 0 && i0 >= -255)
    T2_STRHIN(_jit, r1, r0, -i0);
  else if (i0 >= 0 && i0 <= 4095)
    T2_STRHWI(_jit, r1, r0, i0);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    if ((r0|r1|jit_gpr_regno(reg)) < 8)
      T1_STRH(_jit, r1, r0, jit_gpr_regno(reg));
    else
      T2_STRH(_jit, r1, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
str_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  T2_STRI(_jit, r1, r0, 0);
}

static void
sti_i(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  T2_STRI(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
stxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if ((r0|r1|r2) < 8)
    T1_STR(_jit, r2, r1, r0);
  else
    T2_STR(_jit, r2, r1, r0);
}

static void
stxi_i(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8 && i0 >= 0 && !(i0 & 3) && (i0 >> 2) < 0x20)
    T1_STRI(_jit, r1, r0, i0 >> 2);
  else if (r0 == jit_gpr_regno(JIT_SP) && r1 < 8 &&
           i0 >= 0 && !(i0 & 3) && (i0 >> 2) <= 255)
    T1_STRISP(_jit, r1, i0 >> 2);
  else if (jit_ldrt_strt_p() && i0 >= 0 && i0 <= 255)
    T2_STRI(_jit, r1, r0, i0);
  else if (i0 < 0 && i0 >= -255)
    T2_STRIN(_jit, r1, r0, -i0);
  else if (i0 >= 0 && i0 <= 4095)
    T2_STRWI(_jit, r1, r0, i0);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    if ((r0|r1|jit_gpr_regno(reg)) < 8)
      T1_STR(_jit, r1, r0, jit_gpr_regno(reg));
    else
      T2_STR(_jit, r1, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
bswapr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_REV(_jit, r0, r1);
  else
    T2_REV(_jit, r0, r1);
  rshi_u(_jit, r0, r0, 16);
}

/* inline glibc htonl (without register clobber) */
static void
bswapr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_REV(_jit, r0, r1);
  else
    T2_REV(_jit, r0, r1);
}

static void
extr_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  
  if ((r0|r1) < 8)
    T1_SXTB(_jit, r0, r1);
  else
    T2_SXTB(_jit, r0, r1);
}

static void
extr_uc(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_UXTB(_jit, r0, r1);
  else
    T2_UXTB(_jit, r0, r1);
}

static void
extr_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_SXTH(_jit, r0, r1);
  else
    T2_SXTH(_jit, r0, r1);
}

static void
extr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if ((r0|r1) < 8)
    T1_UXTH(_jit, r0, r1);
  else
    T2_UXTH(_jit, r0, r1);
}

static void
callr(jit_state_t *_jit, int32_t r0)
{
  T1_BLX(_jit, r0);
}

static void
calli(jit_state_t *_jit, jit_word_t i0)
{
  if (i0 & 1)
    jit_patch_there(_jit, T2_BLI(_jit), (void*)(i0 & ~1));
  else
    jit_patch_there(_jit, T2_BLXI(_jit), (void*)i0);
}

static void
ret(jit_state_t *_jit)
{
  T1_BX(_jit, jit_gpr_regno(_LR));
}

static void
reti(jit_state_t *_jit, int32_t i0)
{
  movi(_jit, jit_gpr_regno(_R0), i0);
  ret(_jit);
}

static void
retr(jit_state_t *_jit, int32_t r0)
{
  movr(_jit, jit_gpr_regno(_R0), r0);
  ret(_jit);
}

static void
retval_c(jit_state_t *_jit, int32_t r0)
{
  extr_c(_jit, r0, jit_gpr_regno(_R0));
}

static void
retval_uc(jit_state_t *_jit, int32_t r0)
{
  extr_uc(_jit, r0, jit_gpr_regno(_R0));
}

static void
retval_s(jit_state_t *_jit, int32_t r0)
{
  extr_s(_jit, r0, jit_gpr_regno(_R0));
}

static void
retval_us(jit_state_t *_jit, int32_t r0)
{
  extr_us(_jit, r0, jit_gpr_regno(_R0));
}

static void
retval_i(jit_state_t *_jit, int32_t r0)
{
  movr(_jit, r0, jit_gpr_regno(_R0));
}

struct veneer
{
  uint16_t ldr;
  uint16_t br;
  uint32_t addr;
};

static void
patch_veneer(uint32_t *loc, jit_pointer_t addr)
{
  struct veneer *v = (struct veneer*) v;
  v->addr = (uintptr_t) addr;
}

static void
emit_veneer(jit_state_t *_jit, jit_pointer_t target)
{
  uint16_t thumb1_ldr = 0x4800;
  int32_t tmp = jit_gpr_regno(get_temp_gpr(_jit));
  ASSERT(tmp < 8);
  // Loaded addr is 4 bytes after the LDR, which is aligned, so offset is 0.
  emit_u16(_jit, thumb1_ldr | (tmp << 8));
  T1_MOV(_jit, jit_gpr_regno(_PC), tmp);
  unget_temp_gpr(_jit);
  emit_u32(_jit, (uint32_t) target);
}
