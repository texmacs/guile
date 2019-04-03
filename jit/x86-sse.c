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

#define _XMM0_REGNO                     0
#define _XMM1_REGNO                     1
#define _XMM2_REGNO                     2
#define _XMM3_REGNO                     3
#define _XMM4_REGNO                     4
#define _XMM5_REGNO                     5
#define _XMM6_REGNO                     6
#define _XMM7_REGNO                     7
#define _XMM8_REGNO                     8
#define _XMM9_REGNO                     9
#define _XMM10_REGNO                    10
#define _XMM11_REGNO                    11
#define _XMM12_REGNO                    12
#define _XMM13_REGNO                    13
#define _XMM14_REGNO                    14
#define _XMM15_REGNO                    15
#define X86_SSE_MOV                     0x10
#define X86_SSE_MOV1                    0x11
#define X86_SSE_MOVLP                   0x12
#define X86_SSE_MOVHP                   0x16
#define X86_SSE_MOVA                    0x28
#define X86_SSE_CVTIS                   0x2a
#define X86_SSE_CVTTSI                  0x2c
#define X86_SSE_CVTSI                   0x2d
#define X86_SSE_UCOMI                   0x2e
#define X86_SSE_COMI                    0x2f
#define X86_SSE_ROUND                   0x3a
#define X86_SSE_SQRT                    0x51
#define X86_SSE_RSQRT                   0x52
#define X86_SSE_RCP                     0x53
#define X86_SSE_AND                     0x54
#define X86_SSE_ANDN                    0x55
#define X86_SSE_OR                      0x56
#define X86_SSE_XOR                     0x57
#define X86_SSE_ADD                     0x58
#define X86_SSE_MUL                     0x59
#define X86_SSE_CVTSD                   0x5a
#define X86_SSE_CVTDT                   0x5b
#define X86_SSE_SUB                     0x5c
#define X86_SSE_MIN                     0x5d
#define X86_SSE_DIV                     0x5e
#define X86_SSE_MAX                     0x5f
#define X86_SSE_X2G                     0x6e
#define X86_SSE_EQB                     0x74
#define X86_SSE_EQW                     0x75
#define X86_SSE_EQD                     0x76
#define X86_SSE_G2X                     0x7e
#define X86_SSE_MOV2                    0xd6

static void
sser(jit_state_t *_jit, int32_t c, int32_t r0, int32_t r1)
{
  rex(_jit, 0, 0, r0, 0, r1);
  ic(_jit, 0x0f);
  ic(_jit, c);
  mrm(_jit, 0x03, r7(r0), r7(r1));
}

static void
ssexr(jit_state_t *_jit, int32_t p, int32_t c,
      int32_t r0, int32_t r1)
{
  ic(_jit, p);
  rex(_jit, 0, 0, r0, 0, r1);
  ic(_jit, 0x0f);
  ic(_jit, c);
  mrm(_jit, 0x03, r7(r0), r7(r1));
}

static void
ssexi(jit_state_t *_jit, int32_t c, int32_t r0,
      int32_t m, int32_t i)
{
  ic(_jit, 0x66);
  rex(_jit, 0, 0, 0, 0, r0);
  ic(_jit, 0x0f);
  ic(_jit, c);
  mrm(_jit, 0x03, r7(m), r7(r0));
  ic(_jit, i);
}

static void
sselxr(jit_state_t *_jit, int32_t p, int32_t c, int32_t r0, int32_t r1)
{
  if (__X64 && !__X64_32) {
    ic(_jit, p);
    rex(_jit, 0, 1, r0, 0, r1);
    ic(_jit, 0x0f);
    ic(_jit, c);
    mrm(_jit, 0x03, r7(r0), r7(r1));
  } else {
    ssexr(_jit, p, c, r0, r1);
  }
}

static void
ssexrx(jit_state_t *_jit, int32_t px, int32_t code, int32_t md,
       int32_t rb, int32_t ri, int32_t ms, int32_t rd)
{
  ic(_jit, px);
  rex(_jit, 0, 0, rd, ri, rb);
  ic(_jit, 0x0f);
  ic(_jit, code);
  rx(_jit, rd, md, rb, ri, ms);
}

static void
movdlxr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0x66, X86_SSE_X2G, r0, r1);
}
static void
movdqxr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  sselxr(_jit, 0x66, X86_SSE_X2G, r0, r1);
}

static void
movssmr(jit_state_t *_jit, int32_t md, int32_t rb, int32_t ri, int32_t ms, int32_t rd)
{
  ssexrx(_jit, 0xf3, X86_SSE_MOV, md, rb, ri, ms, rd);
}
static void
movsdmr(jit_state_t *_jit, int32_t md, int32_t rb, int32_t ri, int32_t ms, int32_t rd)
{
  ssexrx(_jit, 0xf2, X86_SSE_MOV, md, rb, ri, ms, rd);
}
static void
movssrm(jit_state_t *_jit, int32_t rs, int32_t md, int32_t mb, int32_t mi, int32_t ms)
{
  ssexrx(_jit, 0xf3, X86_SSE_MOV1, md, mb, mi, ms, rs);
}
static void
movsdrm(jit_state_t *_jit, int32_t rs, int32_t md, int32_t mb, int32_t mi, int32_t ms)
{
  ssexrx(_jit, 0xf2, X86_SSE_MOV1, md, mb, mi, ms, rs);
}

static void
movr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    ssexr(_jit, 0xf3, X86_SSE_MOV, r0, r1);
}

static void
movr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    ssexr(_jit, 0xf2, X86_SSE_MOV, r0, r1);
}

static void
pushr_d(jit_state_t *_jit, int32_t r0)
{
  jit_gpr_t tmp = get_temp_gpr(_jit);
  movdqxr(_jit, rn(tmp), r0);
  pushr(_jit, rn(tmp));
  unget_temp_gpr(_jit);
}

static void
popr_d(jit_state_t *_jit, int32_t r0)
{
  jit_gpr_t tmp = get_temp_gpr(_jit);
  popr(_jit, rn(tmp));
  ssexr(_jit, 0x66, X86_SSE_G2X, r0, rn(tmp));
  unget_temp_gpr(_jit);
}

static jit_fpr_t
get_temp_xpr(jit_state_t *_jit)
{
  /* Reserve XMM7 for the JIT.  */
  ASSERT(!_jit->temp_fpr_saved);
  _jit->temp_fpr_saved = 1;
  return JIT_FPR(_XMM7);
}

static void
unget_temp_xpr(jit_state_t *_jit)
{
  ASSERT(_jit->temp_fpr_saved);
  _jit->temp_fpr_saved = 0;
}

static void
addssr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf3, X86_SSE_ADD, r0, r1);
}
static void
addsdr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf2, X86_SSE_ADD, r0, r1);
}
static void
subssr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf3, X86_SSE_SUB, r0, r1);
}
static void
subsdr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf2, X86_SSE_SUB, r0, r1);
}
static void
mulssr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf3, X86_SSE_MUL, r0, r1);
}
static void
mulsdr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf2, X86_SSE_MUL, r0, r1);
}
static void
divssr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf3, X86_SSE_DIV, r0, r1);
}
static void
divsdr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf2, X86_SSE_DIV, r0, r1);
}
static void
andpsr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  sser(_jit,        X86_SSE_AND, r0, r1);
}
static void
andpdr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0x66, X86_SSE_AND, r0, r1);
}
static void
truncr_f_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf3, X86_SSE_CVTTSI, r0, r1);
}
static void
truncr_d_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf2, X86_SSE_CVTTSI, r0, r1);
}
#if __X64
static void
truncr_f_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  sselxr(_jit, 0xf3, X86_SSE_CVTTSI, r0, r1);
}
static void
truncr_d_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  sselxr(_jit, 0xf2, X86_SSE_CVTTSI, r0, r1);
}
#endif
static void
extr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  sselxr(_jit, 0xf3, X86_SSE_CVTIS, r0, r1);
}
static void
extr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  sselxr(_jit, 0xf2, X86_SSE_CVTIS, r0, r1);
}

static void
extr_f_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf3, X86_SSE_CVTSD, r0, r1);
}
static void
extr_d_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf2, X86_SSE_CVTSD, r0, r1);
}
static void
ucomissr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  sser(_jit, X86_SSE_UCOMI, r0, r1);
}
static void
ucomisdr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0x66, X86_SSE_UCOMI, r0, r1);
}
static void
xorpsr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  sser(_jit, X86_SSE_XOR, r0, r1);
}
static void
xorpdr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0x66, X86_SSE_XOR, r0, r1);
}
static void
pcmpeqlr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0x66, X86_SSE_EQD, r0, r1);
}
static void
psrl(jit_state_t *_jit, int32_t r0, int32_t i0)
{
  ssexi(_jit, 0x72, r0, 0x02, i0);
}
static void
psrq(jit_state_t *_jit, int32_t r0, int32_t i0)
{
  ssexi(_jit, 0x73, r0, 0x02, i0);
}
static void
pslq(jit_state_t *_jit, int32_t r0, int32_t i0)
{
  ssexi(_jit, 0x73, r0, 0x06, i0);
}
static void
sqrtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf3, X86_SSE_SQRT, r0, r1);
}
static void
sqrtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ssexr(_jit, 0xf2, X86_SSE_SQRT, r0, r1);
}
static void
ldr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  movssmr(_jit, 0, r1, _NOREG, _SCL1, r0);
}
static void
str_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  movssrm(_jit, r1, 0, r0, _NOREG, _SCL1);
}
static void
ldr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  movsdmr(_jit, 0, r1, _NOREG, _SCL1, r0);
}
static void
str_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  movsdrm(_jit, r1, 0, r0, _NOREG, _SCL1);
}

static void
movi_f(jit_state_t *_jit, int32_t r0, jit_float32_t i0)
{
  union {
    int32_t i;
    jit_float32_t f;
  } data;

  data.f = i0;
  if (data.f == 0.0 && !(data.i & 0x80000000))
    xorpsr(_jit, r0, r0);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, rn(reg), data.i);
    movdlxr(_jit, r0, rn(reg));
    unget_temp_gpr(_jit);
  }
}

static void
movi_d(jit_state_t *_jit, int32_t r0, jit_float64_t i0)
{
  union {
    int32_t ii[2];
    jit_word_t w;
    jit_float64_t d;
  } data;

  data.d = i0;
  if (data.d == 0.0 && !(data.ii[1] & 0x80000000))
    xorpdr(_jit, r0, r0);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
#if __X64 && !__X64_32
    movi(_jit, rn(reg), data.w);
    movdqxr(_jit, r0, rn(reg));
    unget_temp_gpr(_jit);
#else
    movi(_jit, rn(reg), data.ii[0]);
    stxi_i(CVT_OFFSET, _RBP_REGNO, rn(reg));
    movi(_jit, rn(reg), data.ii[1]);
    stxi_i(CVT_OFFSET + 4, _RBP_REGNO, rn(reg));
    unget_temp_gpr(_jit);
    ldxi_d(_jit, r0, _RBP_REGNO, CVT_OFFSET);
#endif
  }
}

static void
retval_f(jit_state_t *_jit, int32_t r0)
{
#if __X64
  movr_f(_jit, r0, rn(JIT_FRET));
#endif
}

static void
retval_d(jit_state_t *_jit, int32_t r0)
{
#if __X64
  movr_d(_jit, r0, rn(JIT_FRET));
#endif
}

static void
retr_f(jit_state_t *_jit, int32_t u)
{
  movr_f(_jit, rn(JIT_FRET), u);
  ret(_jit);
}

static void
retr_d(jit_state_t *_jit, int32_t u)
{
  movr_d(_jit, rn(JIT_FRET), u);
  ret(_jit);
}

static void
addr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    addssr(_jit, r0, r2);
  else if (r0 == r2)
    addssr(_jit, r0, r1);
  else {
    movr_f(_jit, r0, r1);
    addssr(_jit, r0, r2);
  }
}

static void
addr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    addsdr(_jit, r0, r2);
  else if (r0 == r2)
    addsdr(_jit, r0, r1);
  else {
    movr_d(_jit, r0, r1);
    addsdr(_jit, r0, r2);
  }
}

static void
subr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    subssr(_jit, r0, r2);
  else if (r0 == r2) {
    jit_fpr_t reg = get_temp_xpr(_jit);
    movr_f(_jit, rn(reg), r0);
    movr_f(_jit, r0, r1);
    subssr(_jit, r0, rn(reg));
    unget_temp_xpr(_jit);
  }
  else {
    movr_f(_jit, r0, r1);
    subssr(_jit, r0, r2);
  }
}

static void
subr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    subsdr(_jit, r0, r2);
  else if (r0 == r2) {
    jit_fpr_t reg = get_temp_xpr(_jit);
    movr_d(_jit, rn(reg), r0);
    movr_d(_jit, r0, r1);
    subsdr(_jit, r0, rn(reg));
    unget_temp_xpr(_jit);
  }
  else {
    movr_d(_jit, r0, r1);
    subsdr(_jit, r0, r2);
  }
}

static void
mulr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    mulssr(_jit, r0, r2);
  else if (r0 == r2)
    mulssr(_jit, r0, r1);
  else {
    movr_f(_jit, r0, r1);
    mulssr(_jit, r0, r2);
  }
}

static void
mulr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    mulsdr(_jit, r0, r2);
  else if (r0 == r2)
    mulsdr(_jit, r0, r1);
  else {
    movr_d(_jit, r0, r1);
    mulsdr(_jit, r0, r2);
  }
}

static void
divr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    divssr(_jit, r0, r2);
  else if (r0 == r2) {
    jit_fpr_t reg = get_temp_xpr(_jit);
    movr_f(_jit, rn(reg), r0);
    movr_f(_jit, r0, r1);
    divssr(_jit, r0, rn(reg));
    unget_temp_xpr(_jit);
  }
  else {
    movr_f(_jit, r0, r1);
    divssr(_jit, r0, r2);
  }
}

static void
divr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    divsdr(_jit, r0, r2);
  else if (r0 == r2) {
    jit_fpr_t reg = get_temp_xpr(_jit);
    movr_d(_jit, rn(reg), r0);
    movr_d(_jit, r0, r1);
    divsdr(_jit, r0, rn(reg));
    unget_temp_xpr(_jit);
  }
  else {
    movr_d(_jit, r0, r1);
    divsdr(_jit, r0, r2);
  }
}

static void
absr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 == r1) {
    jit_fpr_t reg = get_temp_xpr(_jit);
    pcmpeqlr(_jit, rn(reg), rn(reg));
    psrl(_jit, rn(reg), 1);
    andpsr(_jit, r0, rn(reg));
    unget_temp_xpr(_jit);
  }
  else {
    pcmpeqlr(_jit, r0, r0);
    psrl(_jit, r0, 1);
    andpsr(_jit, r0, r1);
  }
}

static void
absr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 == r1) {
    jit_fpr_t reg = get_temp_xpr(_jit);
    pcmpeqlr(_jit, rn(reg), rn(reg));
    psrq(_jit, rn(reg), 1);
    andpdr(_jit, r0, rn(reg));
    unget_temp_xpr(_jit);
  }
  else {
    pcmpeqlr(_jit, r0, r0);
    psrq(_jit, r0, 1);
    andpdr(_jit, r0, r1);
  }
}

static void
negr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t ireg = get_temp_gpr(_jit);
  imovi(_jit, rn(ireg), 0x80000000);
  if (r0 == r1) {
    jit_fpr_t freg = get_temp_xpr(_jit);
    movdlxr(_jit, rn(freg), rn(ireg));
    xorpsr(_jit, r0, rn(freg));
    unget_temp_xpr(_jit);
  } else {
    movdlxr(_jit, r0, rn(ireg));
    xorpsr(_jit, r0, r1);
  }
  unget_temp_gpr(_jit);
}

static void
negr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t ireg = get_temp_gpr(_jit);
  imovi(_jit, rn(ireg), 0x80000000);
  if (r0 == r1) {
    jit_fpr_t freg = get_temp_xpr(_jit);
    movdlxr(_jit, rn(freg), rn(ireg));
    pslq(_jit, rn(freg), 32);
    xorpdr(_jit, r0, rn(freg));
    unget_temp_xpr(_jit);
  } else {
    movdlxr(_jit, r0, rn(ireg));
    pslq(_jit, r0, 32);
    xorpdr(_jit, r0, r1);
  }
  unget_temp_gpr(_jit);
}

#if __X32
# define sse_address_p(i0) 1
#elif __X64_32
# define sse_address_p(i0) ((jit_word_t)(i0) >= 0)
# else
# define sse_address_p(i0) can_sign_extend_int_p(i0)
#endif

static void
ldi_f(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (sse_address_p(i0))
    movssmr(_jit, i0, _NOREG, _NOREG, _SCL1, r0);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, rn(reg), i0);
    ldr_f(_jit, r0, rn(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
#if __X64_32
  jit_gpr_t reg = get_temp_gpr(_jit);
  addr(_jit, rn(reg), r1, r2);
  ldr_f(_jit, r0, rn(reg));
  unget_temp_gpr(_jit);
#else
  movssmr(_jit, 0, r1, r2, _SCL1, r0);
#endif
}

static void
ldxi_f(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0))
    movssmr(_jit, i0, r1, _NOREG, _SCL1, r0);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
#if __X64_32
    addi(rn(reg), r1, i0);
    ldr_f(_jit, r0, rn(reg));
#else
    movi(_jit, rn(reg), i0);
    ldxr_f(_jit, r0, r1, rn(reg));
#endif
    unget_temp_gpr(_jit);
  }
}

static void
sti_f(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  if (sse_address_p(i0))
    movssrm(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, rn(reg), i0);
    str_f(_jit, rn(reg), r0);
    unget_temp_gpr(_jit);
  }
}

static void
stxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
#if __X64_32
  jit_gpr_t reg = get_temp_gpr(_jit);
  addr(_jit, rn(reg), r0, r1);
  str_f(_jit, rn(reg), r2);
  unget_temp_gpr(_jit);
#else
  movssrm(_jit, r2, 0, r0, r1, _SCL1);
#endif
}

static void
stxi_f(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (can_sign_extend_int_p(i0))
    movssrm(_jit, r1, i0, r0, _NOREG, _SCL1);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
#if __X64_32
    addi(rn(reg), r0, i0);
    str_f(_jit, rn(reg), r1);
#else
    movi(_jit, rn(reg), i0);
    stxr_f(_jit, rn(reg), r0, r1);
#endif
    unget_temp_gpr(_jit);
  }
}

static jit_reloc_t
bltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r1, r0);
  return ja(_jit);
}

static jit_reloc_t
bler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r1, r0);
  return jae(_jit);
}

static jit_reloc_t
beqr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  jit_reloc_t pos = jps(_jit);
  jit_reloc_t ret = je(_jit);
  jit_patch_here(_jit, pos);
  return ret;
}

static jit_reloc_t
bger_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  return jae(_jit);
}

static jit_reloc_t
bgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  return ja(_jit);
}

static jit_reloc_t
bner_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  jit_reloc_t pos = jps(_jit);
  jit_reloc_t zero = jzs(_jit);
  jit_patch_here(_jit, pos);
  jit_reloc_t ret = jmp(_jit);
  jit_patch_here(_jit, zero);
  return ret;
}

static jit_reloc_t
bunltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  return jnae(_jit);
}

static jit_reloc_t
bunler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  return jna(_jit);
}

static jit_reloc_t
buneqr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  return je(_jit);
}

static jit_reloc_t
bunger_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r1, r0);
  return jna(_jit);
}

static jit_reloc_t
bungtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r1, r0);
  return jnae(_jit);
}

static jit_reloc_t
bltgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  return jne(_jit);
}

static jit_reloc_t
bordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  return jnp(_jit);
}

static jit_reloc_t
bunordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomissr(_jit, r0, r1);
  return jp(_jit);
}

static void
ldi_d(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (sse_address_p(i0))
    movsdmr(_jit, i0, _NOREG, _NOREG, _SCL1, r0);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, rn(reg), i0);
    ldr_d(_jit, r0, rn(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
#if __X64_32
  jit_gpr_t reg = get_temp_gpr(_jit);
  addr(_jit, rn(reg), r1, r2);
  ldr_d(_jit, r0, rn(reg));
  unget_temp_gpr(_jit);
#else
  movsdmr(_jit, 0, r1, r2, _SCL1, r0);
#endif
}

static void
ldxi_d(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0))
    movsdmr(_jit, i0, r1, _NOREG, _SCL1, r0);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
#if __X64_32
    addi(rn(reg), r1, i0);
    ldr_d(_jit, r0, rn(reg));
#else
    movi(_jit, rn(reg), i0);
    ldxr_d(_jit, r0, r1, rn(reg));
#endif
    unget_temp_gpr(_jit);
  }
}

static void
sti_d(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  if (sse_address_p(i0))
    movsdrm(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, rn(reg), i0);
    str_d(_jit, rn(reg), r0);
    unget_temp_gpr(_jit);
  }
}

static void
stxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
#if __X64_32
  jit_gpr_t reg = get_temp_gpr(_jit);
  addr(_jit, rn(reg), r0, r1);
  str_d(_jit, rn(reg), r2);
  unget_temp_gpr(_jit);
#else
  movsdrm(_jit, r2, 0, r0, r1, _SCL1);
#endif
}

static void
stxi_d(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (can_sign_extend_int_p(i0))
    movsdrm(_jit, r1, i0, r0, _NOREG, _SCL1);
  else {
    jit_gpr_t reg = get_temp_gpr(_jit);
#if __X64_32
    addi(rn(reg), r0, i0);
    str_d(_jit, rn(reg), r1);
#else
    movi(_jit, rn(reg), i0);
    stxr_d(_jit, rn(reg), r0, r1);
#endif
    unget_temp_gpr(_jit);
  }
}

static jit_reloc_t
bltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r1, r0);
  return ja(_jit);
}

static jit_reloc_t
bler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r1, r0);
  return jae(_jit);
}

static jit_reloc_t
beqr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  jit_reloc_t pos = jps(_jit);
  jit_reloc_t ret = je(_jit);
  jit_patch_here(_jit, pos);
  return ret;
}

static jit_reloc_t
bger_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  return jae(_jit);
}

static jit_reloc_t
bgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  return ja(_jit);
}

static jit_reloc_t
bner_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  jit_reloc_t pos = jps(_jit);
  jit_reloc_t zero = jzs(_jit);
  jit_patch_here(_jit, pos);
  jit_reloc_t ret = jmp(_jit);
  jit_patch_here(_jit, zero);
  return ret;
}

static jit_reloc_t
bunltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  return jnae(_jit);
}

static jit_reloc_t
bunler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  return jna(_jit);
}

static jit_reloc_t
buneqr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  return je(_jit);
}

static jit_reloc_t
bunger_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r1, r0);
  return jna(_jit);
}

static jit_reloc_t
bungtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r1, r0);
  return jnae(_jit);
}

static jit_reloc_t
bltgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  return jne(_jit);
}

static jit_reloc_t
bordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  return jnp(_jit);
}

static jit_reloc_t
bunordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ucomisdr(_jit, r0, r1);
  return jp(_jit);
}
