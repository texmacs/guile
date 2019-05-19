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

/* as per vfp_regno macro, required due to "support" to soft float registers
 * or using integer registers as arguments to float operations */
#define _D8_REGNO               32
#define ARM_V_F64               0x00000100
#define ARM_VADD_F              0x0e300a00
#define ARM_VSUB_F              0x0e300a40
#define ARM_VMUL_F              0x0e200a00
#define ARM_VDIV_F              0x0e800a00
#define ARM_VABS_F              0x0eb00ac0
#define ARM_VNEG_F              0x0eb10a40
#define ARM_VSQRT_F             0x0eb10ac0
#define ARM_VMOV_F              0x0eb00a40
#define ARM_VMOV_A_S            0x0e100a10 /* vmov rn, sn */
#define ARM_VMOV_S_A            0x0e000a10 /* vmov sn, rn */
#define ARM_VMOV_AA_D           0x0c500b10 /* vmov rn,rn, dn */
#define ARM_VMOV_D_AA           0x0c400b10 /* vmov dn, rn,rn */
#define ARM_VCMP                0x0eb40a40
#define ARM_VMRS                0x0ef10a10
#define ARM_VCVT_2I             0x00040000 /* to integer */
#define ARM_VCVT_2S             0x00010000 /* to signed */
#define ARM_VCVT_RS             0x00000080 /* round to zero or signed */
#define ARM_VCVT                0x0eb80a40
#define ARM_VCVT_S32_F32        ARM_VCVT|ARM_VCVT_2I|ARM_VCVT_2S|ARM_VCVT_RS
#define ARM_VCVT_S32_F64        ARM_VCVT|ARM_VCVT_2I|ARM_VCVT_2S|ARM_VCVT_RS|ARM_V_F64
#define ARM_VCVT_F32_S32        ARM_VCVT|ARM_VCVT_RS
#define ARM_VCVT_F64_S32        ARM_VCVT|ARM_VCVT_RS|ARM_V_F64
#define ARM_VCVT_F              0x0eb70ac0
#define ARM_VCVT_F32_F64        ARM_VCVT_F
#define ARM_VCVT_F64_F32        ARM_VCVT_F|ARM_V_F64
#define ARM_V_D                 0x00400000
#define ARM_V_N                 0x00000080
#define ARM_V_M                 0x00000020
#define ARM_V_I32               0x00200000
#define ARM_VMOVI               0x02800010
#define ARM_VMVNI               0x02800030
#define ARM_VLDR                0x0d100a00
#define ARM_VSTR                0x0d000a00
#define ARM_VM                  0x0c000a00
#define ARM_VMOV_A_D            0x0e100b10
#define ARM_VMOV_D_A            0x0e000b10

#define vfp_regno(rn)         (((rn) - 16) >> 1)

static void
vodi(jit_state_t *_jit, int oi, int r0)
{
  jit_thumb_t thumb;
  assert(!(oi  & 0x0000f000));
  assert(!(r0 & 1));  r0 = vfp_regno(r0);
  thumb.i = oi|(_u4(r0)<<12);
  iss(thumb.s[0], thumb.s[1]);
}

static void
_voqi(jit_state_t *_jit, int oi, int r0)
{
  jit_thumb_t thumb;
  assert(!(oi  & 0x0000f000));
  assert(!(r0 & 3));  r0 = vfp_regno(r0);
  thumb.i = oi|(_u4(r0)<<12);
  iss(thumb.s[0], thumb.s[1]);
}

static void
vo_ss(jit_state_t *_jit, int o, int r0, int r1)
{
  assert(!(o  & 0xf000f00f));
  if (r0 & 1) o |= ARM_V_D;   r0 = vfp_regno(r0);
  if (r1 & 1) o |= ARM_V_M;   r1 = vfp_regno(r1);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r0)<<12)|_u4(r1));
}

static void
vo_dd(jit_state_t *_jit, int o, int r0, int r1)
{
  assert(!(o  & 0xf000f00f));
  assert(!(r0 & 1) && !(r1 & 1));
  r0 = vfp_regno(r0); r1 = vfp_regno(r1);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r0)<<12)|_u4(r1));
}

static void
vo_qd(jit_state_t *_jit, int o, int r0, int r1)
{
  assert(!(o  & 0xf000f00f));
  assert(!(r0 & 3) && !(r1 & 1));
  r0 = vfp_regno(r0); r1 = vfp_regno(r1);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r0)<<12)|_u4(r1));
}

static void
vo_qq(jit_state_t *_jit, int o, int r0, int r1)
{
  assert(!(o  & 0xf000f00f));
  assert(!(r0 & 3) && !(r1 & 3));
  r0 = vfp_regno(r0); r1 = vfp_regno(r1);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r0)<<12)|_u4(r1));
}

static void
vorr_(jit_state_t *_jit, int o, int r0, int r1)
{
  assert(!(o  & 0xf000f00f));
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12));
}

static void
vors_(jit_state_t *_jit, int o, int r0, int r1)
{
  assert(!(o  & 0xf000f00f));
  if (r1 & 1) o |= ARM_V_N;   r1 = vfp_regno(r1);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12));
}

static void
vorv_(jit_state_t *_jit, int o, int r0, int r1)
{
  assert(!(o  & 0xf000f00f));
  if (r1 & 1) o |= ARM_V_M;   r1 = vfp_regno(r1);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12));
}

static void
vori_(jit_state_t *_jit, int o, int r0, int r1)
{
  assert(!(o  & 0xf000f00f));
  /* use same bit pattern, to set opc1... */
  if (r1 & 1) o |= ARM_V_I32; r1 = vfp_regno(r1);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12));
}

static void
vorrd(jit_state_t *_jit, int o, int r0, int r1, int r2)
{
  assert(!(o  & 0xf00ff00f));
  assert(!(r2 & 1));
  r2 = vfp_regno(r2);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2));
}

static void
vosss(jit_state_t *_jit, int o, int r0, int r1, int r2)
{
  assert(!(o  & 0xf00ff00f));
  if (r0 & 1) o |= ARM_V_D;   r0 = vfp_regno(r0);
  if (r1 & 1) o |= ARM_V_N;   r1 = vfp_regno(r1);
  if (r2 & 1) o |= ARM_V_M;   r2 = vfp_regno(r2);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2));
}

static void
voddd(jit_state_t *_jit, int o, int r0, int r1, int r2)
{
  assert(!(o  & 0xf00ff00f));
  assert(!(r0 & 1) && !(r1 & 1) && !(r2 & 1));
  r0 = vfp_regno(r0); r1 = vfp_regno(r1);     r2 = vfp_regno(r2);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2));
}

static void
voqdd(jit_state_t *_jit, int o, int r0, int r1, int r2)
{
  assert(!(o  & 0xf00ff00f));
  assert(!(r0 & 3) && !(r1 & 1) && !(r2 & 1));
  r0 = vfp_regno(r0); r1 = vfp_regno(r1);     r2 = vfp_regno(r2);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2));
}

static void
voqqd(jit_state_t *_jit, int o, int r0, int r1, int r2)
{
  assert(!(o  & 0xf00ff00f));
  assert(!(r0 & 3) && !(r1 & 3) && !(r2 & 1));
  r0 = vfp_regno(r0); r1 = vfp_regno(r1);     r2 = vfp_regno(r2);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2));
}

static void
voqqq(jit_state_t *_jit, int o, int r0, int r1, int r2)
{
  assert(!(o  & 0xf00ff00f));
  assert(!(r0 & 3) && !(r1 & 3) && !(r2 & 3));
  r0 = vfp_regno(r0); r1 = vfp_regno(r1);     r2 = vfp_regno(r2);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2));
}

static void
vldst(jit_state_t *_jit, int o, int r0, int r1, int i0)
{
  /* i0 << 2 is byte offset */
  assert(!(o  & 0xf00ff0ff));
  if (r0 & 1) {
    assert(!(o & ARM_V_F64));
    o |= ARM_V_D;
  }
  r0 = vfp_regno(r0);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u8(i0));
}

static void
vorsl(jit_state_t *_jit, int o, int r0, int r1, int i0)
{
  assert(!(o  & 0xf00ff0ff));
  /* save i0 double precision registers */
  if (o & ARM_V_F64)          i0 <<= 1;
  /* if (r1 & 1) cc & ARM_V_F64 must be false */
  if (r1 & 1) o |= ARM_V_D;   r1 = vfp_regno(r1);
  assert(i0 && !(i0 & 1) && r1 + i0 <= 32);
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r0)<<16)|(_u4(r1)<<12)|_u8(i0));
}

static void
VADD_F32(jit_state_t *_jit, int32_t r0,r1,r2)
{
  vosss(_jit,ARM_VADD_F,r0,r1,r2);
}

static void
VADD_F64(jit_state_t *_jit, int32_t r0,r1,r2)
{
  voddd(_jit,ARM_VADD_F|ARM_V_F64,r0,r1,r2);
}

static void
VSUB_F32(jit_state_t *_jit, int32_t r0,r1,r2)
{
  vosss(_jit,ARM_VSUB_F,r0,r1,r2);
}

static void
VSUB_F64(jit_state_t *_jit, int32_t r0,r1,r2)
{
  voddd(_jit,ARM_VSUB_F|ARM_V_F64,r0,r1,r2);
}

static void
VMUL_F32(jit_state_t *_jit, int32_t r0,r1,r2)
{
  vosss(_jit,ARM_VMUL_F,r0,r1,r2);
}

static void
VMUL_F64(jit_state_t *_jit, int32_t r0,r1,r2)
{
  voddd(_jit,ARM_VMUL_F|ARM_V_F64,r0,r1,r2);
}

static void
VDIV_F32(jit_state_t *_jit, int32_t r0,r1,r2)
{
  vosss(_jit,ARM_VDIV_F,r0,r1,r2);
}

static void
VDIV_F64(jit_state_t *_jit, int32_t r0,r1,r2)
{
  voddd(_jit,ARM_VDIV_F|ARM_V_F64,r0,r1,r2);
}

static void
VABS_F32(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VABS_F,r0,r1);
}

static void
VABS_F64(jit_state_t *_jit, int32_t r0,r1)
{
  vo_dd(_jit,ARM_VABS_F|ARM_V_F64,r0,r1);
}

static void
VNEG_F32(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VNEG_F,r0,r1);
}

static void
VNEG_F64(jit_state_t *_jit, int32_t r0,r1)
{
  vo_dd(_jit,ARM_VNEG_F|ARM_V_F64,r0,r1);
}

static void
VSQRT_F32(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VSQRT_F,r0,r1);
}

static void
VSQRT_F64(jit_state_t *_jit, int32_t r0,r1)
{
  vo_dd(_jit,ARM_VSQRT_F|ARM_V_F64,r0,r1);
}

static void
VMOV_F32(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VMOV_F,r0,r1);
}

static void
VMOV_F64(jit_state_t *_jit, int32_t r0,r1)
{
  vo_dd(_jit,ARM_VMOV_F|ARM_V_F64,r0,r1);
}

static void
VMOV_AA_D(jit_state_t *_jit, int32_t r0,r1,r2)
{
  vorrd(_jit,ARM_VMOV_AA_D,r0,r1,r2);
}

static void
VMOV_D_AA(jit_state_t *_jit, int32_t r0,r1,r2)
{
  vorrd(_jit,ARM_VMOV_D_AA,r1,r2,r0);
}

static void
VMOV_A_S(jit_state_t *_jit, int32_t r0,r1)
{
  vors_(_jit,ARM_VMOV_A_S,r0,r1);
}

static void
VMOV_S_A(jit_state_t *_jit, int32_t r0,r1)
{
  vors_(_jit,ARM_VMOV_S_A,r1,r0);
}

static void
VCMP_F32(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VCMP,r0,r1);
}

static void
VCMP_F64(jit_state_t *_jit, int32_t r0,r1)
{
  vo_dd(_jit,ARM_VCMP|ARM_V_F64,r0,r1);
}

static void
VMRS(jit_state_t *_jit, int32_t r0)
{
  vorr_(_jit,ARM_VMRS,r0,0);
}

static void
VCVT_S32_F32(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VCVT_S32_F32,r0,r1);
}

static void
VCVT_S32_F64(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VCVT_S32_F64,r0,r1);
}

static void
VCVT_F32_S32(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VCVT_F32_S32,r0,r1);
}

static void
VCVT_F64_S32(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VCVT_F64_S32,r0,r1);
}

static void
VCVT_F32_F64(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VCVT_F32_F64,r0,r1);
}

static void
VCVT_F64_F32(jit_state_t *_jit, int32_t r0,r1)
{
  vo_ss(_jit,ARM_VCVT_F64_F32,r0,r1);
}

static void
VMOV_A_S32(jit_state_t *_jit, int32_t r0,r1)
{
  vori_(_jit,ARM_VMOV_A_D,r0,r1);
}

static void
VMOV_V_I32(jit_state_t *_jit, int32_t r0,r1)
{
  vori_(_jit,ARM_VMOV_D_A,r1,r0);
}

/* "oi" should be the result of encode_vfp_double */
static void
VIMM(jit_state_t *_jit, int32_t oi,r0)
{
  vodi(_jit, oi,r0);
}

/* index is multipled by four */
static void
VLDRN_F32(jit_state_t *_jit, int32_t r0,r1,i0)
{
  vldst(_jit,ARM_VLDR,r0,r1,i0);
}

static void
VLDR_F32(jit_state_t *_jit, int32_t r0,r1,i0)
{
  vldst(_jit,ARM_VLDR|ARM_P,r0,r1,i0);
}

static void
VLDRN_F64(jit_state_t *_jit, int32_t r0,r1,i0)
{
  vldst(_jit,ARM_VLDR|ARM_V_F64,r0,r1,i0);
}

static void
VLDR_F64(jit_state_t *_jit, int32_t r0,r1,i0)
{
  vldst(_jit,ARM_VLDR|ARM_V_F64|ARM_P,r0,r1,i0);
}

static void
VSTRN_F32(jit_state_t *_jit, int32_t r0,r1,i0)
{
  vldst(_jit,ARM_VSTR,r0,r1,i0);
}

static void
VSTR_F32(jit_state_t *_jit, int32_t r0,r1,i0)
{
  vldst(_jit,ARM_VSTR|ARM_P,r0,r1,i0);
}

static void
VSTRN_F64(jit_state_t *_jit, int32_t r0,r1,i0)
{
  vldst(_jit,ARM_VSTR|ARM_V_F64,r0,r1,i0);
}

static void
VSTR_F64(jit_state_t *_jit, int32_t r0,r1,i0)
{
  vldst(_jit,ARM_VSTR|ARM_V_F64|ARM_P,r0,r1,i0);
}

static void
vfp_absr_f(jit_state_t *_jit, int32_t r0,r1)
{
  VABS_F32(_jit, r0,r1);
}

static void
vfp_absr_d(jit_state_t *_jit, int32_t r0,r1)
{
  VABS_F64(_jit, r0,r1);
}

static void
vfp_negr_f(jit_state_t *_jit, int32_t r0,r1)
{
  VNEG_F32(_jit, r0,r1);
}

static void
vfp_negr_d(jit_state_t *_jit, int32_t r0,r1)
{
  VNEG_F64(_jit, r0,r1);
}

static void
vfp_sqrtr_f(jit_state_t *_jit, int32_t r0,r1)
{
  VSQRT_F32(_jit, r0,r1);
}

static void
vfp_sqrtr_d(jit_state_t *_jit, int32_t r0,r1)
{
  VSQRT_F64(_jit, r0,r1);
}

static void
vfp_addr_f(jit_state_t *_jit, int32_t r0,r1,r2)
{
  VADD_F32(_jit, r0,r1,r2);
}

static void
vfp_addr_d(jit_state_t *_jit, int32_t r0,r1,r2)
{
  VADD_F64(_jit, r0,r1,r2);
}

static void
vfp_subr_f(jit_state_t *_jit, int32_t r0,r1,r2)
{
  VSUB_F32(_jit, r0,r1,r2);
}

static void
vfp_subr_d(jit_state_t *_jit, int32_t r0,r1,r2)
{
  VSUB_F64(_jit, r0,r1,r2);
}

static void
vfp_mulr_f(jit_state_t *_jit, int32_t r0,r1,r2)
{
  VMUL_F32(_jit, r0,r1,r2);
}

static void
vfp_mulr_d(jit_state_t *_jit, int32_t r0,r1,r2)
{
  VMUL_F64(_jit, r0,r1,r2);
}

static void
vfp_divr_f(jit_state_t *_jit, int32_t r0,r1,r2)
{
  VDIV_F32(_jit, r0,r1,r2);
}

static void
vfp_divr_d(jit_state_t *_jit, int32_t r0,r1,r2)
{
  VDIV_F64(_jit, r0,r1,r2);
}

static jit_reloc_t
vfp_bltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_MI, r0, r1);
}

static jit_reloc_t
vfp_bltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_MI, r0, r1);
}

static jit_reloc_t
vfp_bler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_LS, r0, r1);
}

static jit_reloc_t
vfp_bler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_LS, r0, r1);
}

static jit_reloc_t
vfp_beqr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_EQ, r0, r1);
}

static jit_reloc_t
vfp_beqr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_EQ, r0, r1);
}

static jit_reloc_t
vfp_bger_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_GE, r0, r1);
}

static jit_reloc_t
vfp_bger_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_GE, r0, r1);
}

static jit_reloc_t
vfp_bgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_GT, r0, r1);
}

static jit_reloc_t
vfp_bgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_GT, r0, r1);
}

static jit_reloc_t
vfp_bner_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_NE, r0, r1);
}

static jit_reloc_t
vfp_bner_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_NE, r0, r1);
}

static jit_reloc_t
vfp_bunltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbncmp_f(_jit, ARM_CC_GE, r0, r1);
}

static jit_reloc_t
vfp_bunltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbncmp_d(_jit, ARM_CC_GE, r0, r1);
}

static jit_reloc_t
vfp_bunler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbncmp_f(_jit, ARM_CC_GT, r0, r1);
}

static jit_reloc_t
vfp_bunler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbncmp_d(_jit, ARM_CC_GT, r0, r1);
}

static jit_reloc_t
vfp_bungtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_HI, r0, r1);
}

static jit_reloc_t
vfp_bungtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_HI, r0, r1);
}

static jit_reloc_t
vfp_bordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_VC, r0, r1);
}

static jit_reloc_t
vfp_bordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_VC, r0, r1);
}

static jit_reloc_t
vfp_bunordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_VS, r0, r1);
}

static jit_reloc_t
vfp_bunordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_VS, r0, r1);
}

static void
vfp_ldr_f(jit_state_t *_jit, int32_t r0,r1)
{
  VLDR_F32(_jit, r0,r1,0);
}

static void
vfp_ldr_d(jit_state_t *_jit, int32_t r0,r1)
{
  VLDR_F64(_jit, r0,r1,0);
}

static void
vfp_str_f(jit_state_t *_jit, int32_t r0,r1)
{
  VSTR_F32(_jit, r1,r0,0);
}

static void
vfp_str_d(jit_state_t *_jit, int32_t r0,r1)
{
  VSTR_F64(_jit, r1,r0,0);
}

static void
vfp_movr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1) {
    if (jit_fpr_p(r1)) {
      if (jit_fpr_p(r0))
        VMOV_F32(r0, r1);
      else
        VMOV_A_S(r0, r1);
    }
    else if (jit_fpr_p(r0))
      VMOV_S_A(r0, r1);
    else
      movr(r0, r1);
  }
}

static void
vfp_movr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1) {
    if (jit_fpr_p(r1)) {
      if (jit_fpr_p(r0))
        VMOV_F64(r0, r1);
      else
        VMOV_AA_D(r0, r0 + 1, r1);
    }
    else if (jit_fpr_p(r0))
      VMOV_D_AA(r0, r1, r1 + 1);
    else {
      /* minor consistency check */
      assert(r0 + 1 != r1 && r0 -1 != r1);
      movr(r0, r1);
      movr(r0 + 1, r1 + 1);
    }
  }
}

static int
encode_vfp_double(int mov, int inv, unsigned lo, unsigned hi)
{
  int           code, mode, imm, mask;

  if (hi != lo) {
    if (mov && !inv) {
      /* (I64)
       *        aaaaaaaabbbbbbbbccccccccddddddddeeeeeeeeffffffffgggggggghhhhhhhh
       */
      for (mode = 0, mask = 0xff; mode < 4; mask <<= 8, mode++) {
        imm = lo & mask;
        if (imm != mask && imm != 0)
          goto fail;
        imm = hi & mask;
        if (imm != mask && imm != 0)
          goto fail;
      }
      mode = 0xe20;
      imm = (((hi & 0x80000000) >> 24) | ((hi & 0x00800000) >> 17) |
             ((hi & 0x00008000) >> 10) | ((hi & 0x00000080) >>  3) |
             ((lo & 0x80000000) >> 28) | ((lo & 0x00800000) >> 21) |
             ((lo & 0x00008000) >> 14) | ((lo & 0x00000080) >>  7));
      goto success;
    }
    goto fail;
  }
  /*  (I32)
   *  00000000 00000000 00000000 abcdefgh
   *  00000000 00000000 abcdefgh 00000000
   *  00000000 abcdefgh 00000000 00000000
   *  abcdefgh 00000000 00000000 00000000 */
  for (mode = 0, mask = 0xff; mode < 4; mask <<= 8, mode++) {
    if ((lo & mask) == lo) {
      imm = lo >> (mode << 3);
      mode <<= 9;
      goto success;
    }
  }
  /*  (I16)
   *  00000000 abcdefgh 00000000 abcdefgh
   *  abcdefgh 00000000 abcdefgh 00000000 */
  for (mode = 0, mask = 0xff; mode < 2; mask <<= 8, mode++) {
    if ((lo & mask) && ((lo & (mask << 16)) >> 16) == (lo & mask)) {
      imm = lo >> (mode << 3);
      mode = 0x800 | (mode << 9);
      goto success;
    }
  }
  if (mov) {
    /*  (I32)
     *  00000000 00000000 abcdefgh 11111111
     *  00000000 abcdefgh 11111111 11111111 */
    for (mode = 0, mask = 0xff; mode < 2;
         mask = (mask << 8) | 0xff, mode++) {
      if ((lo & mask) == mask &&
          !((lo & ~mask) >> 8) &&
          (imm = lo >> (8 + (mode << 8)))) {
        mode = 0xc00 | (mode << 8);
        goto success;
      }
    }
    if (!inv) {
      /* (F32)
       *  aBbbbbbc defgh000 00000000 00000000
       *  from the ARM Architecture Reference Manual:
       *  In this entry, B = NOT(b). The bit pattern represents the
       *  floating-point number (-1)^s* 2^exp * mantissa, where
       *  S = UInt(a),
       *  exp = UInt(NOT(b):c:d)-3 and
       *  mantissa = (16+UInt(e:f:g:h))/16. */
      if ((lo & 0x7ffff) == 0 &&
          (((lo & 0x7e000000) == 0x3e000000) ||
           ((lo & 0x7e000000) == 0x40000000))) {
        mode = 0xf00;
        imm = ((lo >> 24) & 0x80) | ((lo >> 19) & 0x7f);
        goto success;
      }
    }
  }

fail:
  /* need another approach (load from memory, move from arm register, etc) */
  return (-1);

success:
  code = inv ? ARM_VMVNI : ARM_VMOVI;
  switch ((mode & 0xf00) >> 8) {
  case 0x0:     case 0x2:       case 0x4:       case 0x6:
  case 0x8:     case 0xa:
    if (inv)    mode |= 0x20;
    if (!mov)   mode |= 0x100;
    break;
  case 0x1:     case 0x3:       case 0x5:       case 0x7:
    /* should actually not reach here */
    assert(!inv);
  case 0x9:     case 0xb:
    assert(!mov);
    break;
  case 0xc:     case 0xd:
    /* should actually not reach here */
    assert(inv);
  case 0xe:
    assert(mode & 0x20);
    assert(mov && !inv);
    break;
  default:
    assert(!(mode & 0x20));
    break;
  }
  imm = ((imm & 0x80) << 17) | ((imm & 0x70) << 12) | (imm & 0x0f);
  code |= mode | imm;
    
  if (code & 0x1000000)
    code |= 0xff000000;
  else
    code |= 0xef000000;

  return (code);
}

static void
_vfp_movi_f(jit_state_t *_jit, int32_t r0, jit_float32_t i0)
{
  union {
    int32_t i;
    jit_float32_t   f;
  } u;
  int32_t             reg;
  int32_t             code;
  u.f = i0;
  if (jit_fpr_p(r0)) {
    /* float arguments are packed, for others,
     * lightning only address even registers */
    if (!(r0 & 1) && (r0 - 16) >= 0 &&
        ((code = encode_vfp_double(1, 0, u.i, u.i)) != -1 ||
         (code = encode_vfp_double(1, 1, ~u.i, ~u.i)) != -1))
      VIMM(code, r0);
    else {
      reg = jit_get_reg(jit_class_gpr);
      movi(rn(reg), u.i);
      VMOV_S_A(r0, rn(reg));
      jit_unget_reg(reg);
    }
  }
  else
    movi(r0, u.i);
}

static void
_vfp_movi_d(jit_state_t *_jit, int32_t r0, jit_float64_t i0)
{
  union {
    int32_t i[2];
    jit_float64_t   d;
  } u;
  int32_t             code;
  int32_t             rg0, rg1;
  u.d = i0;
  if (jit_fpr_p(r0)) {
    if ((code = encode_vfp_double(1, 0, u.i[0], u.i[1])) != -1 ||
        (code = encode_vfp_double(1, 1, ~u.i[0], ~u.i[1])) != -1)
      VIMM(code, r0);
    else {
      rg0 = jit_get_reg(jit_class_gpr);
      rg1 = jit_get_reg(jit_class_gpr);
      movi(rn(rg0), u.i[0]);
      movi(rn(rg1), u.i[1]);
      VMOV_D_AA(r0, rn(rg0), rn(rg1));
      jit_unget_reg(rg1);
      jit_unget_reg(rg0);
    }
  }
  else {
    movi(r0, u.i[0]);
    movi(r0 + 1, u.i[1]);
  }
}

static void
_vfp_extr_d_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t             reg;
  if (jit_fpr_p(r1)) {
    if (jit_fpr_p(r0))
      VCVT_F64_F32(r0, r1);
    else {
      reg = jit_get_reg(jit_class_fpr);
      VCVT_F64_F32(rn(reg), r1);
      VMOV_A_S(r0, rn(reg));
      jit_unget_reg(reg);
    }
  }
  else {
    reg = jit_get_reg(jit_class_fpr);
    VMOV_S_A(rn(reg), r1);
    VCVT_F64_F32(rn(reg), rn(reg));
    if (jit_fpr_p(r0))
      VMOV_F32(r0, rn(reg));
    else
      VMOV_A_S(r0, rn(reg));
    jit_unget_reg(reg);
  }
}

static void
_vfp_extr_f_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t             reg;
  if (jit_fpr_p(r1)) {
    if (jit_fpr_p(r0))
      VCVT_F32_F64(r0, r1);
    else {
      reg = jit_get_reg(jit_class_fpr);
      VCVT_F32_F64(rn(reg), r1);
      VMOV_AA_D(r0, r0 + 1, rn(reg));
      jit_unget_reg(reg);
    }
  }
  else {
    reg = jit_get_reg(jit_class_fpr);
    VMOV_D_AA(rn(reg), r1, r1 + 1);
    VCVT_F32_F64(rn(reg), rn(reg));
    if (jit_fpr_p(r0))
      VMOV_F64(r0, rn(reg));
    else
      VMOV_AA_D(r0, r0 + 1, rn(reg));
    jit_unget_reg(reg);
  }
}

static void
_vfp_extr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t             reg;
  if (jit_fpr_p(r0)) {
    VMOV_V_I32(r0, r1);
    VCVT_F32_S32(r0, r0);
  }
  else {
    reg = jit_get_reg(jit_class_fpr);
    VMOV_V_I32(rn(reg), r1);
    VCVT_F32_S32(rn(reg), rn(reg));
    VMOV_F32(r0, rn(reg));
    jit_unget_reg(reg);
  }
}

static void
_vfp_extr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t             reg;
  if (jit_fpr_p(r0)) {
    VMOV_V_I32(r0, r1);
    VCVT_F64_S32(r0, r0);
  }
  else {
    reg = jit_get_reg(jit_class_fpr);
    VMOV_V_I32(rn(reg), r1);
    VCVT_F64_S32(rn(reg), rn(reg));
    VMOV_F64(r0, rn(reg));
    jit_unget_reg(reg);
  }
}

static void
_vfp_truncr_f_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t             reg;
  reg = jit_get_reg(jit_class_fpr);
  if (jit_fpr_p(r1))
    VCVT_S32_F32(rn(reg), r1);
  else {
    VMOV_V_I32(rn(reg), r1);
    VCVT_S32_F32(rn(reg), rn(reg));
  }
  VMOV_A_S32(r0, rn(reg));
  jit_unget_reg(reg);
}

static void
_vfp_truncr_d_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t             reg;
  reg = jit_get_reg(jit_class_fpr);
  if (jit_fpr_p(r1))
    VCVT_S32_F64(rn(reg), r1);
  else {
    VMOV_V_I32(rn(reg), r1);
    VCVT_S32_F64(rn(reg), rn(reg));
  }
  VMOV_A_S32(r0, rn(reg));
  jit_unget_reg(reg);
}

static void
_vfp_cmp_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t             rg0, rg1;
  if (jit_fpr_p(r0)) {
    if (jit_fpr_p(r1))
      VCMP_F32(r0, r1);
    else {
      rg1 = jit_get_reg(jit_class_fpr);
      VMOV_S_A(rn(rg1), r1);
      VCMP_F32(r0, rn(rg1));
      jit_unget_reg(rg1);
    }
  }
  else {
    rg0 = jit_get_reg(jit_class_fpr);
    VMOV_S_A(rn(rg0), r0);
    if (jit_fpr_p(r1))
      VCMP_F32(rn(rg0), r1);
    else {
      rg1 = jit_get_reg(jit_class_fpr);
      VMOV_S_A(rn(rg1), r1);
      VCMP_F32(rn(rg0), rn(rg1));
      jit_unget_reg(rg1);
    }
    jit_unget_reg(rg0);
  }
}

static void
_vfp_cmp_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t             rg0, rg1;
  if (jit_fpr_p(r0)) {
    if (jit_fpr_p(r1))
      VCMP_F64(r0, r1);
    else {
      rg1 = jit_get_reg(jit_class_fpr);
      VMOV_D_AA(rn(rg1), r1, r1 + 1);
      VCMP_F64(r0, rn(rg1));
      jit_unget_reg(rg1);
    }
  }
  else {
    rg0 = jit_get_reg(jit_class_fpr);
    VMOV_D_AA(rn(rg0), r0, r0 + 1);
    if (jit_fpr_p(r1))
      VCMP_F64(rn(rg0), r1);
    else {
      rg1 = jit_get_reg(jit_class_fpr);
      VMOV_D_AA(rn(rg1), r1, r1 + 1);
      VCMP_F64(rn(rg0), rn(rg1));
      jit_unget_reg(rg1);
    }
    jit_unget_reg(rg0);
  }
}

static jit_word_t
_vbcmp_x(jit_state_t *_jit, int cc, jit_word_t i0)
{
  jit_word_t          d, w;
  VMRS(_R15_REGNO);
  w = _jit->pc.w;
    
  d = ((i0 - w) >> 1) - 2;
  assert(_s20P(d));
  T2_CC_B(cc, encode_thumb_cc_jump(d));
    
  return (w);
}


static jit_word_t
_vbcmp_f(jit_state_t *_jit, int cc,
         jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_f(r0, r1);
  return (vbcmp_x(cc, i0));
}

static jit_word_t
_vbcmp_d(jit_state_t *_jit, int cc,
         jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_d(r0, r1);
  return (vbcmp_x(cc, i0));
}

static jit_word_t
_vbncmp_x(jit_state_t *_jit, int cc, jit_word_t i0)
{
  jit_word_t          d, p, w;
  VMRS(_R15_REGNO);
  p = _jit->pc.w;
    
  T2_CC_B(cc, 0);
  w = _jit->pc.w;
  d = ((i0 - w) >> 1) - 2;
  assert(_s20P(d));
  T2_B(encode_thumb_jump(d));
    
  patch_at(arm_patch_jump, p, _jit->pc.w);
  return (w);
}

static jit_word_t
_vbncmp_f(jit_state_t *_jit, int cc,
          jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_f(r0, r1);
  return (vbncmp_x(cc, i0));
}

static jit_word_t
_vbncmp_d(jit_state_t *_jit, int cc,
          jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_d(r0, r1);
  return (vbncmp_x(cc, i0));
}

static jit_word_t
_vfp_buneqr_x(jit_state_t *_jit, jit_word_t i0)
{
  jit_word_t          d, p, q, w;
  VMRS(_R15_REGNO);
  p = _jit->pc.w;
    
  T2_CC_B(ARM_CC_VS, 0);
  q = _jit->pc.w;
  T2_CC_B(ARM_CC_NE, 0);
  patch_at(arm_patch_jump, p, _jit->pc.w);
  w = _jit->pc.w;
  d = ((i0 - w) >> 1) - 2;
  assert(_s20P(d));
  T2_B(encode_thumb_jump(d));
    
  patch_at(arm_patch_jump, q, _jit->pc.w);
  return (w);
}

static jit_word_t
_vfp_buneqr_f(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_f(r0, r1);
  return (vfp_buneqr_x(i0));
}

static jit_word_t
_vfp_buneqr_d(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_d(r0, r1);
  return (vfp_buneqr_x(i0));
}

static jit_word_t
_vfp_bunger_x(jit_state_t *_jit, jit_word_t i0)
{
  jit_word_t          d, p, w;
  VMRS(_R15_REGNO);
  p = _jit->pc.w;
    
  T2_CC_B(ARM_CC_MI, 0);
  w = _jit->pc.w;
  d = ((i0 - w) >> 1) - 2;
  assert(_s20P(d));
  T2_CC_B(ARM_CC_HS, encode_thumb_cc_jump(d));
    
  patch_at(arm_patch_jump, p, _jit->pc.w);
  return (w);
}

static jit_word_t
_vfp_bunger_f(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_f(r0, r1);
  return (vfp_bunger_x(i0));
}

static jit_word_t
_vfp_bunger_d(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_d(r0, r1);
  return (vfp_bunger_x(i0));
}

static jit_word_t
_vfp_bltgtr_x(jit_state_t *_jit, jit_word_t i0)
{
  jit_word_t          d, p, q, w;
  VMRS(_R15_REGNO);
  p = _jit->pc.w;
    
  T2_CC_B(ARM_CC_VS, 0);
  q = _jit->pc.w;
  T2_CC_B(ARM_CC_EQ, 0);
  w = _jit->pc.w;
  d = ((i0 - w) >> 1) - 2;
  assert(_s20P(d));
  T2_B(encode_thumb_jump(d));
    
  patch_at(arm_patch_jump, p, _jit->pc.w);
  patch_at(arm_patch_jump, q, _jit->pc.w);
  return (w);
}

static jit_word_t
_vfp_bltgtr_f(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_f(r0, r1);
  return (vfp_bltgtr_x(i0));
}

static jit_word_t
_vfp_bltgtr_d(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  vfp_cmp_d(r0, r1);
  return (vfp_bltgtr_x(i0));
}

static void
_vfp_ldi_f(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  int32_t             gpr;
  if (jit_fpr_p(r0)) {
    gpr = jit_get_reg(jit_class_gpr);
    movi(rn(gpr), i0);
    VLDR_F32(r0, rn(gpr), 0);
    jit_unget_reg(gpr);
  }
  else
    ldi_i(r0, i0);
}

static void
_vfp_ldi_d(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  int32_t             reg;
  reg = jit_get_reg(jit_class_gpr);
  movi(rn(reg), i0);
  if (jit_fpr_p(r0))
    VLDR_F64(r0, rn(reg), 0);
  else {
    ldr_i(r0, rn(reg));
    ldxi_i(r0 + 1, rn(reg), 4);
  }
  jit_unget_reg(reg);
}

static void
_vfp_ldxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  int32_t             reg;
  if (jit_fpr_p(r0)) {
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r1, r2);
    VLDR_F32(r0, rn(reg), 0);
    jit_unget_reg(reg);
  }
  else
    ldxr_i(r0, r1, r2);
}

static void
_vfp_ldxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  int32_t             reg;
  reg = jit_get_reg(jit_class_gpr);
  addr(rn(reg), r1, r2);
  if (jit_fpr_p(r0))
    VLDR_F64(r0, rn(reg), 0);
  else {
    ldr_i(r0, rn(reg));
    ldxi_i(r0 + 1, rn(reg), 4);
  }
  jit_unget_reg(reg);
}

static void
_vfp_ldxi_f(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t             reg;
  if (jit_fpr_p(r0)) {
    if (i0 >= 0) {
      assert(!(i0 & 3));
      if (i0 < 1024)
        VLDR_F32(r0, r1, i0 >> 2);
      else {
        reg = jit_get_reg(jit_class_gpr);
        addi(rn(reg), r1, i0);
        VLDR_F32(r0, rn(reg), 0);
        jit_unget_reg(reg);
      }
    }
    else {
      i0 = -i0;
      assert(!(i0 & 3));
      if (i0 < 1024)
        VLDRN_F32(r0, r1, i0 >> 2);
      else {
        reg = jit_get_reg(jit_class_gpr);
        subi(rn(reg), r1, i0);
        VLDR_F32(r0, rn(reg), 0);
        jit_unget_reg(reg);
      }
    }
  }
  else
    ldxi_i(r0, r1, i0);
}

static void
_vfp_ldxi_d(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  int32_t             reg;
  if (jit_fpr_p(r0)) {
    if (i0 >= 0) {
      assert(!(i0 & 3));
      if (i0 < 1024)
        VLDR_F64(r0, r1, i0 >> 2);
      else {
        reg = jit_get_reg(jit_class_gpr);
        addi(rn(reg), r1, i0);
        VLDR_F64(r0, rn(reg), 0);
        jit_unget_reg(reg);
      }
    }
    else {
      i0 = -i0;
      assert(!(i0 & 3));
      if (i0 < 1024)
        VLDRN_F64(r0, r1, i0 >> 2);
      else {
        reg = jit_get_reg(jit_class_gpr);
        subi(rn(reg), r1, i0);
        VLDR_F64(r0, rn(reg), 0);
        jit_unget_reg(reg);
      }
    }
  }
  else {
    reg = jit_get_reg(jit_class_gpr);
    addi(rn(reg), r1, i0);
    ldr_i(r0, rn(reg));
    ldxi_i(r0 + 1, rn(reg), 4);
    jit_unget_reg(reg);
  }
}

static void
_vfp_sti_f(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  int32_t             reg;
  if (jit_fpr_p(r0)) {
    reg = jit_get_reg(jit_class_gpr);
    movi(rn(reg), i0);
    VSTR_F32(r0, rn(reg), 0);
    jit_unget_reg(reg);
  }
  else
    sti_i(i0, r0);
}

static void
_vfp_sti_d(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  int32_t             reg;
  reg = jit_get_reg(jit_class_gpr);
  movi(rn(reg), i0);
  if (jit_fpr_p(r0))
    VSTR_F64(r0, rn(reg), 0);
  else {
    str_i(rn(reg), r0);
    stxi_i(4, rn(reg), r0 + 1);
  }
  jit_unget_reg(reg);
}

static void
_vfp_stxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  int32_t             reg;
  if (jit_fpr_p(r2)) {
    reg = jit_get_reg(jit_class_gpr);
    addr(rn(reg), r0, r1);
    VSTR_F32(r2, rn(reg), 0);
    jit_unget_reg(reg);
  }
  else
    stxr_i(r0, r1, r2);
}

static void
_vfp_stxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  int32_t             reg;
  reg = jit_get_reg(jit_class_gpr);
  addr(rn(reg), r0, r1);
  if (jit_fpr_p(r2))
    VSTR_F64(r2, rn(reg), 0);
  else {
    str_i(rn(reg), r2);
    stxi_i(4, rn(reg), r2 + 1);
  }
  jit_unget_reg(reg);
}

static void
_vfp_stxi_f(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  int32_t             reg;
  if (jit_fpr_p(r1)) {
    if (i0 >= 0) {
      assert(!(i0 & 3));
      if (i0 < 1024)
        VSTR_F32(r1, r0, i0 >> 2);
      else {
        reg = jit_get_reg(jit_class_gpr);
        addi(rn(reg), r0, i0);
        VSTR_F32(r1, rn(reg), 0);
        jit_unget_reg(reg);
      }
    }
    else {
      i0 = -i0;
      assert(!(i0 & 3));
      if (i0 < 1024)
        VSTRN_F32(r1, r0, i0 >> 2);
      else {
        reg = jit_get_reg(jit_class_gpr);
        subi(rn(reg), r0, i0);
        VSTR_F32(r1, rn(reg), 0);
        jit_unget_reg(reg);
      }
    }
  }
  else
    stxi_i(i0, r0, r1);
}

static void
_vfp_stxi_d(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  int32_t             reg;
  if (jit_fpr_p(r1)) {
    if (i0 >= 0) {
      assert(!(i0 & 3));
      if (i0 < 0124)
        VSTR_F64(r1, r0, i0 >> 2);
      else {
        reg = jit_get_reg(jit_class_gpr);
        addi(rn(reg), r0, i0);
        VSTR_F64(r1, rn(reg), 0);
        jit_unget_reg(reg);
      }
    }
    else {
      i0 = -i0;
      assert(!(i0 & 3));
      if (i0 < 1024)
        VSTRN_F64(r1, r0, i0 >> 2);
      else {
        reg = jit_get_reg(jit_class_gpr);
        subi(rn(reg), r0, i0);
        VSTR_F64(r1, rn(reg), 0);
        jit_unget_reg(reg);
      }
    }
  }
  else {
    reg = jit_get_reg(jit_class_gpr);
    addi(rn(reg), r0, i0);
    str_i(rn(reg), r1);
    stxi_i(4, rn(reg), r1 + 1);
    jit_unget_reg(reg);
  }
}

static void
_vfp_vaarg_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t             reg;

  assert(_jitc->function->self.call & jit_call_varargs);

  /* Adjust pointer. */
  reg = jit_get_reg(jit_class_gpr);
  andi(rn(reg), r1, 7);
  addr(r1, r1, rn(reg));
  jit_unget_reg(reg);

  /* Load argument. */
  vfp_ldr_d(r0, r1);

  /* Update stack pointer. */
  addi(r1, r1, sizeof(jit_float64_t));
}
