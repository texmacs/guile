/*
 * Copyright (C) 2013-2019  Free Software Foundation, Inc.
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

static void
osvvv(jit_state_t *_jit, int32_t Op, int32_t Sz, int32_t Rd, int32_t Rn,
      int32_t Rm)
{
  uint32_t inst = Op;
  inst = write_size_bitfield(inst, Sz);
  inst = write_Rd_bitfield(inst, Rd);
  inst = write_Rn_bitfield(inst, Rn);
  inst = write_Rm_bitfield(inst, Rm);
  emit_u32_with_pool(_jit, inst);
}

static void
osvv_(jit_state_t *_jit, int32_t Op, int32_t Sz, int32_t Rd, int32_t Rn)
{
  uint32_t inst = Op;
  inst = write_size_bitfield(inst, Sz);
  inst = write_Rd_bitfield(inst, Rd);
  inst = write_Rn_bitfield(inst, Rn);
  emit_u32_with_pool(_jit, inst);
}

static void
os_vv(jit_state_t *_jit, int32_t Op, int32_t Sz, int32_t Rn, int32_t Rm)
{
  uint32_t inst = Op;
  inst = write_size_bitfield(inst, Sz);
  inst = write_Rn_bitfield(inst, Rn);
  inst = write_Rm_bitfield(inst, Rm);
  emit_u32_with_pool(_jit, inst);
}

#define A64_SCVTF                     0x1e220000
#define A64_FMOVWV                    0x1e260000
#define A64_FMOVVW                    0x1e270000
#define A64_FMOVXV                    0x9e260000
#define A64_FMOVVX                    0x9e270000
#define A64_FCVTZS                    0x1e380000
#define A64_FCMPE                     0x1e202010
#define A64_FMOV                      0x1e204000
#define A64_FABS                      0x1e20c000
#define A64_FNEG                      0x1e214000
#define A64_FSQRT                     0x1e21c000
#define A64_FCVTS                     0x1e224000
#define A64_FCVTD                     0x1e22c000
#define A64_FMUL                      0x1e200800
#define A64_FDIV                      0x1e201800
#define A64_FADD                      0x1e202800
#define A64_FSUB                      0x1e203800

static void
FCMPES(jit_state_t *_jit, int32_t Rn, int32_t Rm)
{
  os_vv(_jit, A64_FCMPE, 0, Rn, Rm);
}

static void
FCMPED(jit_state_t *_jit, int32_t Rn, int32_t Rm)
{
  os_vv(_jit, A64_FCMPE, 1, Rn, Rm);
}

static void
FMOVS(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FMOV, 0, Rd, Rn);
}

static void
FMOVD(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FMOV, 1, Rd, Rn);
}

static void
FMOVWS(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FMOVWV, 0, Rd, Rn);
}

static void
FMOVSW(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FMOVVW, 0, Rd, Rn);
}

static void
FMOVXD(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FMOVXV, 1, Rd, Rn);
}

static void
FMOVDX(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FMOVVX, 1, Rd, Rn);
}

static void
FCVT_SD(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FCVTS, 1, Rd, Rn);
}

static void
FCVT_DS(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FCVTD, 0, Rd, Rn);
}

static void
SCVTFS(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_SCVTF|XS, 0, Rd, Rn);
}

static void
SCVTFD(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_SCVTF|XS, 1, Rd, Rn);
}

static void
FCVTSZ_WS(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FCVTZS, 0, Rd, Rn);
}

static void
FCVTSZ_WD(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FCVTZS, 1, Rd, Rn);
}

static void
FCVTSZ_XS(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FCVTZS|XS, 0, Rd, Rn);
}

static void
FCVTSZ_XD(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FCVTZS|XS, 1, Rd, Rn);
}

static void
FABSS(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FABS, 0, Rd, Rn);
}

static void
FABSD(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FABS, 1, Rd, Rn);
}

static void
FNEGS(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FNEG, 0, Rd, Rn);
}

static void
FNEGD(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FNEG, 1, Rd, Rn);
}

static void
FSQRTS(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FSQRT, 0, Rd, Rn);
}

static void
FSQRTD(jit_state_t *_jit, int32_t Rd, int32_t Rn)
{
  osvv_(_jit, A64_FSQRT, 1, Rd, Rn);
}

static void
FADDS(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm)
{
  osvvv(_jit, A64_FADD, 0, Rd, Rn, Rm);
}

static void
FADDD(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm)
{
  osvvv(_jit, A64_FADD, 1, Rd, Rn, Rm);
}

static void
FSUBS(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm)
{
  osvvv(_jit, A64_FSUB, 0, Rd, Rn, Rm);
}

static void
FSUBD(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm)
{
  osvvv(_jit, A64_FSUB, 1, Rd, Rn, Rm);
}

static void
FMULS(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm)
{
  osvvv(_jit, A64_FMUL, 0, Rd, Rn, Rm);
}

static void
FMULD(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm)
{
  osvvv(_jit, A64_FMUL, 1, Rd, Rn, Rm);
}

static void
FDIVS(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm)
{
  osvvv(_jit, A64_FDIV, 0, Rd, Rn, Rm);
}

static void
FDIVD(jit_state_t *_jit, int32_t Rd, int32_t Rn, int32_t Rm)
{
  osvvv(_jit, A64_FDIV, 1, Rd, Rn, Rm);
}

static void
truncr_f_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCVTSZ_XS(_jit, r0, r1);
}

static void
truncr_d_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCVTSZ_XD(_jit, r0, r1);
}

static void
addr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  FADDS(_jit, r0, r1, r2);
}

static void
subr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  FSUBS(_jit, r0, r1, r2);
}

static void
mulr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  FMULS(_jit, r0, r1, r2);
}

static void
divr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  FDIVS(_jit, r0, r1, r2);
}

static void
absr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FABSS(_jit, r0, r1);
}

static void
negr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FNEGS(_jit, r0, r1);
}

static void
sqrtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FSQRTS(_jit, r0, r1);
}

static void
extr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  SCVTFS(_jit, r0, r1);
}

static void
extr_d_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCVT_SD(_jit, r0, r1);
}

static jit_reloc_t
fbccr(jit_state_t *_jit, int32_t cc, int32_t r0, int32_t r1)
{
  FCMPES(_jit, r0, r1);
  return B_C(_jit, cc);
}

static jit_reloc_t
bltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_MI,r0, r1);
}

static jit_reloc_t
bler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_LS,r0, r1);
}

static jit_reloc_t
beqr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_EQ,r0, r1);
}

static jit_reloc_t
bger_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_GE,r0, r1);
}

static jit_reloc_t
bgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_GT,r0, r1);
}

static jit_reloc_t
bner_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_NE,r0, r1);
}

static jit_reloc_t
bunltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_LT,r0, r1);
}

static jit_reloc_t
bunler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_LE,r0, r1);
}

static jit_reloc_t
bunger_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_PL,r0, r1);
}

static jit_reloc_t
bungtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_HI,r0, r1);
}

static jit_reloc_t
bordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_VC,r0, r1);
}

static jit_reloc_t
bunordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return fbccr(_jit, BCC_VS, r0, r1);
}

static void
addr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  FADDD(_jit, r0, r1, r2);
}

static void
subr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  FSUBD(_jit, r0, r1, r2);
}

static void
mulr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  FMULD(_jit, r0, r1, r2);
}

static void
divr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  FDIVD(_jit, r0, r1, r2);
}

static void
absr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FABSD(_jit, r0, r1);
}

static void
negr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FNEGD(_jit, r0, r1);
}

static void
sqrtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FSQRTD(_jit, r0, r1);
}

static void
extr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  SCVTFD(_jit, r0, r1);
}

static void
extr_f_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCVT_DS(_jit, r0, r1);
}

static jit_reloc_t
dbccr(jit_state_t *_jit, int32_t cc, int32_t r0, int32_t r1)
{
  FCMPED(_jit, r0, r1);
  return B_C(_jit, cc);
}

static jit_reloc_t
bltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_MI, r0, r1);
}

static jit_reloc_t
bler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_LS, r0, r1);
}

static jit_reloc_t
beqr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_EQ, r0, r1);
}

static jit_reloc_t
bger_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_GE, r0, r1);
}

static jit_reloc_t
bgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_GT, r0, r1);
}

static jit_reloc_t
bner_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_NE, r0, r1);
}

static jit_reloc_t
bunltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_LT, r0, r1);
}

static jit_reloc_t
bunler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_LE, r0, r1);
}

static jit_reloc_t
bunger_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_PL, r0, r1);
}

static jit_reloc_t
bungtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_HI, r0, r1);
}

static jit_reloc_t
bordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_VC, r0, r1);
}

static jit_reloc_t
bunordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return dbccr(_jit, BCC_VS, r0, r1);
}


static void
truncr_f_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCVTSZ_WS(_jit, r0, r1);
  extr_i(_jit, r0, r0);
}

static void
truncr_d_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCVTSZ_WD(_jit, r0, r1);
  extr_i(_jit, r0, r0);
}

static void
ldr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  ldr_i(_jit, jit_gpr_regno(reg), r1);
  FMOVSW(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
ldi_f(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  ldi_i(_jit, jit_gpr_regno(reg), i0);
  FMOVSW(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
ldxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  ldxr_i(_jit, jit_gpr_regno(reg), r1, r2);
  FMOVSW(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
ldxi_f(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  ldxi_i(_jit, jit_gpr_regno(reg), r1, i0);
  FMOVSW(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
str_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  FMOVWS(_jit, jit_gpr_regno(reg), r1);
  str_i(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
sti_f(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  FMOVWS(_jit, jit_gpr_regno(reg), r0);
  sti_i(_jit, i0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
stxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  FMOVWS(_jit, jit_gpr_regno(reg), r2);
  stxr_i(_jit, r0, r1, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
stxi_f(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  FMOVWS(_jit, jit_gpr_regno(reg), r1);
  stxi_i(_jit, i0, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
movr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    FMOVS(_jit, r0, r1);
}

static void
movi_f(jit_state_t *_jit, int32_t r0, float i0)
{
  union {
    int32_t i;
    float   f;
  } u;
  u.f = i0;
  if (u.i == 0)
    FMOVSW(_jit, r0, WZR_REGNO);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    /* prevent generating unused top 32 bits */
    movi(_jit, jit_gpr_regno(reg), ((jit_word_t)u.i) & 0xffffffff);
    FMOVSW(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static jit_reloc_t
buneqr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCMPES(_jit, r0, r1);
  jit_reloc_t unordered = B_C(_jit, BCC_VS); /* unordered satisfies condition */
  jit_reloc_t neq = B_C(_jit, BCC_NE); /* not equal (or unordered) does not satisfy */
  jit_patch_here(_jit, unordered);
  jit_reloc_t ret = B(_jit);
  jit_patch_here(_jit, neq);
  return ret;
}

static jit_reloc_t
bltgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCMPES(_jit, r0, r1);
  jit_reloc_t unordered = B_C(_jit, BCC_VS); /* jump over if unordered */
  jit_reloc_t eq = B_C(_jit, BCC_EQ); /* jump over if equal */
  jit_reloc_t ret = B(_jit);
  jit_patch_here(_jit, unordered);
  jit_patch_here(_jit, eq);
  return ret;
}

static void
ldr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  ldr_l(_jit, jit_gpr_regno(reg), r1);
  FMOVDX(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
ldi_d(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  ldi_l(_jit, jit_gpr_regno(reg), i0);
  FMOVDX(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
ldxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  ldxr_l(_jit, jit_gpr_regno(reg), r1, r2);
  FMOVDX(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
ldxi_d(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  ldxi_l(_jit, jit_gpr_regno(reg), r1, i0);
  FMOVDX(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
str_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  FMOVXD(_jit, jit_gpr_regno(reg), r1);
  str_l(_jit, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
sti_d(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  FMOVXD(_jit, jit_gpr_regno(reg), r0);
  sti_l(_jit, i0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
stxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  FMOVXD(_jit, jit_gpr_regno(reg), r2);
  stxr_l(_jit, r0, r1, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
stxi_d(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  FMOVXD(_jit, jit_gpr_regno(reg), r1);
  stxi_l(_jit, i0, r0, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
movr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    FMOVD(_jit, r0, r1);
}

static void
movi_d(jit_state_t *_jit, int32_t r0, double i0)
{
  union {
    int64_t l;
    double   d;
  } u;
  u.d = i0;
  if (u.l == 0)
    FMOVDX(_jit, r0, XZR_REGNO);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), u.l);
    FMOVDX(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static jit_reloc_t
buneqr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCMPED(_jit, r0, r1);
  jit_reloc_t unordered = B_C(_jit, BCC_VS); /* unordered satisfies condition */
  jit_reloc_t neq = B_C(_jit, BCC_NE); /* not equal (or unordered) does not satisfy */
  jit_patch_here(_jit, unordered);
  jit_reloc_t ret = B(_jit);
  jit_patch_here(_jit, neq);
  return ret;
}

static jit_reloc_t
bltgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  FCMPED(_jit, r0, r1);
  jit_reloc_t unordered = B_C(_jit, BCC_VS); /* jump over if unordered */
  jit_reloc_t eq = B_C(_jit, BCC_EQ); /* jump over if equal */
  jit_reloc_t ret = B(_jit);
  jit_patch_here(_jit, unordered);
  jit_patch_here(_jit, eq);
  return ret;
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
  movr_f(_jit, jit_fpr_regno(_D0), r);
  ret(_jit);
}

static void
retval_f(jit_state_t *_jit, int32_t r0)
{
  movr_f(_jit, r0, jit_fpr_regno(_D0));
}

static void
retval_d(jit_state_t *_jit, int32_t r0)
{
  movr_d(_jit, r0, jit_fpr_regno(_D0));
}
