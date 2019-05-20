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
#define ARM_P                   0x00800000 /* positive offset */
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

#define vfp_regno(rn)         ((rn) >> 1)

static void
vodi(jit_state_t *_jit, int oi, int r0)
{
  ASSERT(!(oi  & 0x0000f000));
  ASSERT(!(r0 & 1));
  r0 >>= 1;
  emit_wide_thumb(_jit, oi|(_u4(r0)<<12));
}

static void
vo_ss(jit_state_t *_jit, int o, int r0, int r1)
{
  ASSERT(!(o  & 0xf000f00f));
  if (r0 & 1) o |= ARM_V_D;
  if (r1 & 1) o |= ARM_V_M;
  r0 >>= 1; r1 >>= 1;
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r0)<<12)|_u4(r1));
}

static void
vo_dd(jit_state_t *_jit, int o, int r0, int r1)
{
  ASSERT(!(o  & 0xf000f00f));
  ASSERT(!(r0 & 1) && !(r1 & 1));
  r0 >>= 1; r1 >>= 1;
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r0)<<12)|_u4(r1));
}

static void
vors_(jit_state_t *_jit, int o, int r0, int r1)
{
  ASSERT(!(o  & 0xf000f00f));
  if (r1 & 1) o |= ARM_V_N;
  r1 >>= 1;
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12));
}

static void
vori_(jit_state_t *_jit, int o, int r0, int r1)
{
  ASSERT(!(o  & 0xf000f00f));
  /* use same bit pattern, to set opc1... */
  if (r1 & 1) o |= ARM_V_I32;
  r1 >>= 1;
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12));
}

static void
vorrd(jit_state_t *_jit, int o, int r0, int r1, int r2)
{
  ASSERT(!(o  & 0xf00ff00f));
  ASSERT(!(r2 & 1));
  r2 >>= 1;
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2));
}

static void
vosss(jit_state_t *_jit, int o, int r0, int r1, int r2)
{
  ASSERT(!(o  & 0xf00ff00f));
  if (r0 & 1) o |= ARM_V_D;
  if (r1 & 1) o |= ARM_V_N;
  if (r2 & 1) o |= ARM_V_M;
  r0 >>= 1; r1 >>= 1; r2 >>= 1;
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2));
}

static void
voddd(jit_state_t *_jit, int o, int r0, int r1, int r2)
{
  ASSERT(!(o  & 0xf00ff00f));
  ASSERT(!(r0 & 1) && !(r1 & 1) && !(r2 & 1));
  r0 >>= 1; r1 >>= 1; r2 >>= 1;
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u4(r2));
}

static void
vldst(jit_state_t *_jit, int o, int r0, int r1, int i0)
{
  /* i0 << 2 is byte offset */
  ASSERT(!(o  & 0xf00ff0ff));
  if (r0 & 1) {
    ASSERT(!(o & ARM_V_F64));
    o |= ARM_V_D;
  }
  r0 >>= 1;
  emit_wide_thumb(_jit, ARM_CC_AL|o|(_u4(r1)<<16)|(_u4(r0)<<12)|_u8(i0));
}

static void
VADD_F32(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  vosss(_jit,ARM_VADD_F,r0,r1,r2);
}

static void
VADD_F64(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  voddd(_jit,ARM_VADD_F|ARM_V_F64,r0,r1,r2);
}

static void
VSUB_F32(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  vosss(_jit,ARM_VSUB_F,r0,r1,r2);
}

static void
VSUB_F64(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  voddd(_jit,ARM_VSUB_F|ARM_V_F64,r0,r1,r2);
}

static void
VMUL_F32(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  vosss(_jit,ARM_VMUL_F,r0,r1,r2);
}

static void
VMUL_F64(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  voddd(_jit,ARM_VMUL_F|ARM_V_F64,r0,r1,r2);
}

static void
VDIV_F32(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  vosss(_jit,ARM_VDIV_F,r0,r1,r2);
}

static void
VDIV_F64(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  voddd(_jit,ARM_VDIV_F|ARM_V_F64,r0,r1,r2);
}

static void
VABS_F32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VABS_F,r0,r1);
}

static void
VABS_F64(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_dd(_jit,ARM_VABS_F|ARM_V_F64,r0,r1);
}

static void
VNEG_F32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VNEG_F,r0,r1);
}

static void
VNEG_F64(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_dd(_jit,ARM_VNEG_F|ARM_V_F64,r0,r1);
}

static void
VSQRT_F32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VSQRT_F,r0,r1);
}

static void
VSQRT_F64(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_dd(_jit,ARM_VSQRT_F|ARM_V_F64,r0,r1);
}

static void
VMOV_F32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VMOV_F,r0,r1);
}

static void
VMOV_F64(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_dd(_jit,ARM_VMOV_F|ARM_V_F64,r0,r1);
}

static void
VMOV_D_AA(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  vorrd(_jit,ARM_VMOV_D_AA,r1,r2,r0);
}

static void
VMOV_S_A(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vors_(_jit,ARM_VMOV_S_A,r1,r0);
}

static void
VCMP_F32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VCMP,r0,r1);
}

static void
VCMP_F64(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_dd(_jit,ARM_VCMP|ARM_V_F64,r0,r1);
}

static void
VMRS(jit_state_t *_jit)
{
  emit_wide_thumb(_jit, ARM_CC_AL|ARM_VMRS|(0xf<<12));
}

static void
VCVT_S32_F32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VCVT_S32_F32,r0,r1);
}

static void
VCVT_S32_F64(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VCVT_S32_F64,r0,r1);
}

static void
VCVT_F32_S32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VCVT_F32_S32,r0,r1);
}

static void
VCVT_F64_S32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VCVT_F64_S32,r0,r1);
}

static void
VCVT_F32_F64(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VCVT_F32_F64,r0,r1);
}

static void
VCVT_F64_F32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vo_ss(_jit,ARM_VCVT_F64_F32,r0,r1);
}

static void
VMOV_A_S32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vori_(_jit,ARM_VMOV_A_D,r0,r1);
}

static void
VMOV_V_I32(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  vori_(_jit,ARM_VMOV_D_A,r1,r0);
}

/* "oi" should be the result of encode_vfp_double */
static void
VIMM(jit_state_t *_jit, int32_t oi, int32_t r0)
{
  vodi(_jit, oi,r0);
}

/* index is multipled by four */
static void
VLDRN_F32(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  vldst(_jit,ARM_VLDR,r0,r1,i0);
}

static void
VLDR_F32(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  vldst(_jit,ARM_VLDR|ARM_P,r0,r1,i0);
}

static void
VLDRN_F64(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  vldst(_jit,ARM_VLDR|ARM_V_F64,r0,r1,i0);
}

static void
VLDR_F64(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  vldst(_jit,ARM_VLDR|ARM_V_F64|ARM_P,r0,r1,i0);
}

static void
VSTRN_F32(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  vldst(_jit,ARM_VSTR,r0,r1,i0);
}

static void
VSTR_F32(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  vldst(_jit,ARM_VSTR|ARM_P,r0,r1,i0);
}

static void
VSTRN_F64(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  vldst(_jit,ARM_VSTR|ARM_V_F64,r0,r1,i0);
}

static void
VSTR_F64(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  vldst(_jit,ARM_VSTR|ARM_V_F64|ARM_P,r0,r1,i0);
}

static void
absr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VABS_F32(_jit, r0,r1);
}

static void
absr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VABS_F64(_jit, r0,r1);
}

static void
negr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VNEG_F32(_jit, r0,r1);
}

static void
negr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VNEG_F64(_jit, r0,r1);
}

static void
sqrtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VSQRT_F32(_jit, r0,r1);
}

static void
sqrtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VSQRT_F64(_jit, r0,r1);
}

static void
addr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  VADD_F32(_jit, r0,r1,r2);
}

static void
addr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  VADD_F64(_jit, r0,r1,r2);
}

static void
subr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  VSUB_F32(_jit, r0,r1,r2);
}

static void
subr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  VSUB_F64(_jit, r0,r1,r2);
}

static void
mulr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  VMUL_F32(_jit, r0,r1,r2);
}

static void
mulr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  VMUL_F64(_jit, r0,r1,r2);
}

static void
divr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  VDIV_F32(_jit, r0,r1,r2);
}

static void
divr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  VDIV_F64(_jit, r0,r1,r2);
}

static void
cmp_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VCMP_F32(_jit, r0, r1);
}

static void
cmp_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VCMP_F64(_jit, r0, r1);
}

static jit_reloc_t
vbcmp_x(jit_state_t *_jit, int cc)
{
  VMRS(_jit);
  return T2_CC_B(_jit, cc);
}

static jit_reloc_t
vbcmp_f(jit_state_t *_jit, int cc, int32_t r0, int32_t r1)
{
  cmp_f(_jit, r0, r1);
  return vbcmp_x(_jit, cc);
}

static jit_reloc_t
vbcmp_d(jit_state_t *_jit, int cc, int32_t r0, int32_t r1)
{
  cmp_d(_jit, r0, r1);
  return vbcmp_x(_jit, cc);
}

static jit_reloc_t
vbncmp_x(jit_state_t *_jit, int cc)
{
  VMRS(_jit);
  jit_reloc_t cont = T2_CC_B(_jit, cc);
  jit_reloc_t ret = T2_B(_jit);
  jit_patch_here(_jit, cont);
  return ret;
}

static jit_reloc_t
vbncmp_f(jit_state_t *_jit, int cc, int32_t r0, int32_t r1)
{
  cmp_f(_jit, r0, r1);
  return vbncmp_x(_jit, cc);
}

static jit_reloc_t
vbncmp_d(jit_state_t *_jit, int cc, int32_t r0, int32_t r1)
{
  cmp_d(_jit, r0, r1);
  return vbncmp_x(_jit, cc);
}

static jit_reloc_t
bltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_MI, r0, r1);
}

static jit_reloc_t
bltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_MI, r0, r1);
}

static jit_reloc_t
bler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_LS, r0, r1);
}

static jit_reloc_t
bler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_LS, r0, r1);
}

static jit_reloc_t
beqr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_EQ, r0, r1);
}

static jit_reloc_t
beqr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_EQ, r0, r1);
}

static jit_reloc_t
bger_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_GE, r0, r1);
}

static jit_reloc_t
bger_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_GE, r0, r1);
}

static jit_reloc_t
bgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_GT, r0, r1);
}

static jit_reloc_t
bgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_GT, r0, r1);
}

static jit_reloc_t
bner_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_NE, r0, r1);
}

static jit_reloc_t
bner_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_NE, r0, r1);
}

static jit_reloc_t
bunltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbncmp_f(_jit, ARM_CC_GE, r0, r1);
}

static jit_reloc_t
bunltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbncmp_d(_jit, ARM_CC_GE, r0, r1);
}

static jit_reloc_t
bunler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbncmp_f(_jit, ARM_CC_GT, r0, r1);
}

static jit_reloc_t
bunler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbncmp_d(_jit, ARM_CC_GT, r0, r1);
}

static jit_reloc_t
bungtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_HI, r0, r1);
}

static jit_reloc_t
bungtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_HI, r0, r1);
}

static jit_reloc_t
bordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_VC, r0, r1);
}

static jit_reloc_t
bordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_VC, r0, r1);
}

static jit_reloc_t
bunordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_f(_jit, ARM_CC_VS, r0, r1);
}

static jit_reloc_t
bunordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return vbcmp_d(_jit, ARM_CC_VS, r0, r1);
}

static jit_reloc_t
buneqr_x(jit_state_t *_jit)
{
  VMRS(_jit);
  jit_reloc_t a = T2_CC_B(_jit, ARM_CC_VS);
  jit_reloc_t b = T2_CC_B(_jit, ARM_CC_NE);
  jit_patch_here(_jit, a);
  jit_reloc_t ret = T2_B(_jit);
  jit_patch_here(_jit, b);
  return ret;
}

static jit_reloc_t
buneqr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  cmp_f(_jit, r0, r1);
  return buneqr_x(_jit);
}

static jit_reloc_t
buneqr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  cmp_d(_jit, r0, r1);
  return buneqr_x(_jit);
}

static jit_reloc_t
bunger_x(jit_state_t *_jit)
{
  VMRS(_jit);
  jit_reloc_t a = T2_CC_B(_jit, ARM_CC_MI);
  jit_reloc_t ret = T2_CC_B(_jit, ARM_CC_HS);
  jit_patch_here(_jit, a);
  return ret;
}

static jit_reloc_t
bunger_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  cmp_f(_jit, r0, r1);
  return bunger_x(_jit);
}

static jit_reloc_t
bunger_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  cmp_d(_jit, r0, r1);
  return bunger_x(_jit);
}

static jit_reloc_t
bltgtr_x(jit_state_t *_jit)
{
  VMRS(_jit);
  jit_reloc_t a = T2_CC_B(_jit, ARM_CC_VS);
  jit_reloc_t b = T2_CC_B(_jit, ARM_CC_EQ);
  jit_reloc_t ret = T2_B(_jit);
  jit_patch_here(_jit, a);
  jit_patch_here(_jit, b);
  return ret;
}

static jit_reloc_t
bltgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  cmp_f(_jit, r0, r1);
  return bltgtr_x(_jit);
}

static jit_reloc_t
bltgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  cmp_d(_jit, r0, r1);
  return bltgtr_x(_jit);
}

static void
ldr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VLDR_F32(_jit, r0,r1,0);
}

static void
ldr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VLDR_F64(_jit, r0,r1,0);
}

static void
str_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VSTR_F32(_jit, r1,r0,0);
}

static void
str_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VSTR_F64(_jit, r1,r0,0);
}

static void
movr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    VMOV_F32(_jit, r0, r1);
}

static void
movr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    VMOV_F64(_jit, r0, r1);
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
  return -1;

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
    ASSERT(!inv);
  case 0x9:     case 0xb:
    ASSERT(!mov);
    break;
  case 0xc:     case 0xd:
    /* should actually not reach here */
    ASSERT(inv);
  case 0xe:
    ASSERT(mode & 0x20);
    ASSERT(mov && !inv);
    break;
  default:
    ASSERT(!(mode & 0x20));
    break;
  }
  imm = ((imm & 0x80) << 17) | ((imm & 0x70) << 12) | (imm & 0x0f);
  code |= mode | imm;
    
  if (code & 0x1000000)
    code |= 0xff000000;
  else
    code |= 0xef000000;

  return code;
}

static void
movi_f(jit_state_t *_jit, int32_t r0, jit_float32_t i0)
{
  union { int32_t i; jit_float32_t f; } u = { .f = i0 };
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), u.i);
  VMOV_S_A(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
movi_d(jit_state_t *_jit, int32_t r0, jit_float64_t i0)
{
  union { int32_t i[2]; jit_float64_t d; } u = { .d = i0 };
  int32_t code;
  if ((code = encode_vfp_double(1, 0, u.i[0], u.i[1])) != -1 ||
      (code = encode_vfp_double(1, 1, ~u.i[0], ~u.i[1])) != -1)
    VIMM(_jit, code, r0);
  else {
    jit_gpr_t rg0 = get_temp_gpr(_jit);
    jit_gpr_t rg1 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(rg0), u.i[0]);
    movi(_jit, jit_gpr_regno(rg1), u.i[1]);
    VMOV_D_AA(_jit, r0, jit_gpr_regno(rg0), jit_gpr_regno(rg1));
    unget_temp_gpr(_jit);
    unget_temp_gpr(_jit);
  }
}

static void
extr_d_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VCVT_F64_F32(_jit, r0, r1);
}

static void
extr_f_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VCVT_F32_F64(_jit, r0, r1);
}

static void
extr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VMOV_V_I32(_jit, r0, r1);
  VCVT_F32_S32(_jit, r0, r0);
}

static void
extr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  VMOV_V_I32(_jit, r0, r1);
  VCVT_F64_S32(_jit, r0, r0);
}

static void
truncr_f_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_fpr_t reg = get_temp_fpr(_jit);
  VCVT_S32_F32(_jit, jit_fpr_regno(reg), r1);
  VMOV_A_S32(_jit, r0, jit_fpr_regno(reg));
  unget_temp_fpr(_jit);
}

static void
truncr_d_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_fpr_t reg = get_temp_fpr(_jit);
  VCVT_S32_F64(_jit, jit_fpr_regno(reg), r1);
  VMOV_A_S32(_jit, r0, jit_fpr_regno(reg));
  unget_temp_fpr(_jit);
}

static void
ldi_f(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t gpr = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(gpr), i0);
  VLDR_F32(_jit, r0, jit_gpr_regno(gpr), 0);
  unget_temp_gpr(_jit);
}

static void
ldi_d(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  VLDR_F64(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
ldxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(reg), r1, r2);
  VLDR_F32(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
ldxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(reg), r1, r2);
  VLDR_F64(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
ldxi_f(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 >= 0) {
    ASSERT(!(i0 & 3));
    if (i0 < 1024)
      VLDR_F32(_jit, r0, r1, i0 >> 2);
    else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      addi(_jit, jit_gpr_regno(reg), r1, i0);
      VLDR_F32(_jit, r0, jit_gpr_regno(reg), 0);
      unget_temp_gpr(_jit);
    }
  }
  else {
    i0 = -i0;
    ASSERT(!(i0 & 3));
    if (i0 < 1024)
      VLDRN_F32(_jit, r0, r1, i0 >> 2);
    else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      subi(_jit, jit_gpr_regno(reg), r1, i0);
      VLDR_F32(_jit, r0, jit_gpr_regno(reg), 0);
      unget_temp_gpr(_jit);
    }
  }
}

static void
ldxi_d(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 >= 0) {
    ASSERT(!(i0 & 3));
    if (i0 < 1024)
      VLDR_F64(_jit, r0, r1, i0 >> 2);
    else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      addi(_jit, jit_gpr_regno(reg), r1, i0);
      VLDR_F64(_jit, r0, jit_gpr_regno(reg), 0);
      unget_temp_gpr(_jit);
    }
  }
  else {
    i0 = -i0;
    ASSERT(!(i0 & 3));
    if (i0 < 1024)
      VLDRN_F64(_jit, r0, r1, i0 >> 2);
    else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      subi(_jit, jit_gpr_regno(reg), r1, i0);
      VLDR_F64(_jit, r0, jit_gpr_regno(reg), 0);
      unget_temp_gpr(_jit);
    }
  }
}

static void
sti_f(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  VSTR_F32(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
sti_d(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  VSTR_F64(_jit, r0, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
stxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(reg), r0, r1);
  VSTR_F32(_jit, r2, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
stxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(reg), r0, r1);
  VSTR_F64(_jit, r2, jit_gpr_regno(reg), 0);
  unget_temp_gpr(_jit);
}

static void
stxi_f(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (i0 >= 0) {
    ASSERT(!(i0 & 3));
    if (i0 < 1024)
      VSTR_F32(_jit, r1, r0, i0 >> 2);
    else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      addi(_jit, jit_gpr_regno(reg), r0, i0);
      VSTR_F32(_jit, r1, jit_gpr_regno(reg), 0);
      unget_temp_gpr(_jit);
    }
  }
  else {
    i0 = -i0;
    ASSERT(!(i0 & 3));
    if (i0 < 1024)
      VSTRN_F32(_jit, r1, r0, i0 >> 2);
    else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      subi(_jit, jit_gpr_regno(reg), r0, i0);
      VSTR_F32(_jit, r1, jit_gpr_regno(reg), 0);
      unget_temp_gpr(_jit);
    }
  }
}

static void
stxi_d(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (i0 >= 0) {
    ASSERT(!(i0 & 3));
    if (i0 < 0124)
      VSTR_F64(_jit, r1, r0, i0 >> 2);
    else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      addi(_jit, jit_gpr_regno(reg), r0, i0);
      VSTR_F64(_jit, r1, jit_gpr_regno(reg), 0);
      unget_temp_gpr(_jit);
    }
  }
  else {
    i0 = -i0;
    ASSERT(!(i0 & 3));
    if (i0 < 1024)
      VSTRN_F64(_jit, r1, r0, i0 >> 2);
    else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      subi(_jit, jit_gpr_regno(reg), r0, i0);
      VSTR_F64(_jit, r1, jit_gpr_regno(reg), 0);
      unget_temp_gpr(_jit);
    }
  }
}

static void
retr_d(jit_state_t *_jit, int32_t r)
{
  movr_d(_jit, jit_fpr_regno(_D0), r);
  ret(_jit);
}

static void
retr_f(jit_state_t *_jit, int32_t r)
{
  movr_f(_jit, jit_fpr_regno(_S0), r);
  ret(_jit);
}

static void
retval_f(jit_state_t *_jit, int32_t r0)
{
  movr_f(_jit, r0, jit_fpr_regno(_S0));
}

static void
retval_d(jit_state_t *_jit, int32_t r0)
{
  movr_d(_jit, r0, jit_fpr_regno(_D0));
}
