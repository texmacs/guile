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
 *      Paulo Cesar Pereira de Andrade
 */

/* avoid using it due to partial stalls */
#define USE_INC_DEC                     0

#if __X32
# define WIDE 0
# define IF_WIDE(wide, narrow) narrow
#else
# define WIDE 1
# define IF_WIDE(wide, narrow) wide
#endif

#define _RAX_REGNO                      0
#define _RCX_REGNO                      1
#define _RDX_REGNO                      2
#define _RBX_REGNO                      3
#define _RSP_REGNO                      4
#define _RBP_REGNO                      5
#define _RSI_REGNO                      6
#define _RDI_REGNO                      7
#define _R8_REGNO                       8
#define _R9_REGNO                       9
#define _R10_REGNO                      10
#define _R11_REGNO                      11
#define _R12_REGNO                      12
#define _R13_REGNO                      13
#define _R14_REGNO                      14
#define _R15_REGNO                      15
#define r7(reg)                 ((reg) & 7)
#define r8(reg)                 ((reg) & 15)
#if __X32
# define reg8_p(rn) ((rn) >= _RAX_REGNO && (rn) <= _RBX_REGNO)
#else
# define reg8_p(rn) 1
#endif

#define can_sign_extend_int_p(im)                                       \
  IF_WIDE((((im) >= 0 && (long long)(im) <=  0x7fffffffLL) ||           \
           ((im) <  0 && (long long)(im) >  -0x80000000LL)),            \
          1)
#define can_zero_extend_int_p(im)                                       \
  IF_WIDE(((im) >= 0 && (im) < 0x80000000LL),                           \
          1)
#define fits_uint32_p(im)                                               \
  IF_WIDE((((im) & 0xffffffff00000000LL) == 0),                         \
          1)

#define _SCL1      0x00
#define _SCL2      0x01
#define _SCL4      0x02
#define _SCL8      0x03

#define X86_ADD    0
#define X86_OR     1 << 3
#define X86_ADC    2 << 3
#define X86_SBB    3 << 3
#define X86_AND    4 << 3
#define X86_SUB    5 << 3
#define X86_XOR    6 << 3
#define X86_CMP    7 << 3
#define X86_ROL    0
#define X86_ROR    1
#define X86_RCL    2
#define X86_RCR    3
#define X86_SHL    4
#define X86_SHR    5
#define X86_SAR    7
#define X86_NOT    2
#define X86_NEG    3
#define X86_MUL    4
#define X86_IMUL   5
#define X86_DIV    6
#define X86_IDIV   7

#define FOR_EACH_CC(M) \
  M(o,   O,   0x0)     \
  M(no,  NO,  0x1)     \
  M(nae, NAE, 0x2)     \
  M(b,   B,   0x2)     \
  M(c,   C,   0x2)     \
  M(ae,  AE,  0x3)     \
  M(nb,  NB,  0x3)     \
  M(nc,  NC,  0x3)     \
  M(e,   E,   0x4)     \
  M(z,   Z,   0x4)     \
  M(ne,  NE,  0x5)     \
  M(nz,  NZ,  0x5)     \
  M(be,  BE,  0x6)     \
  M(na,  NA,  0x6)     \
  M(a,   A,   0x7)     \
  M(nbe, NBE, 0x7)     \
  M(s,   S,   0x8)     \
  M(ns,  NS,  0x9)     \
  M(p,   P,   0xa)     \
  M(pe,  PE,  0xa)     \
  M(np,  NP,  0xb)     \
  M(po,  PO,  0xb)     \
  M(l,   L,   0xc)     \
  M(nge, NGE, 0xc)     \
  M(ge,  GE,  0xd)     \
  M(nl_, NL,  0xd)     \
  M(le,  LE,  0xe)     \
  M(ng,  NG,  0xe)     \
  M(g,   G,   0xf)     \
  M(nle, NLE, 0xf)     \
  /* EOL */

enum x86_cc
{
#define DEFINE_ENUM(cc, CC, code) X86_CC_##CC = code,
  FOR_EACH_CC(DEFINE_ENUM)
#undef DEFINE_ENUM
};

static inline void
mrm(jit_state_t *_jit, uint8_t md, uint8_t r, uint8_t m)
{
  emit_u8(_jit, (md<<6) | (r<<3) | m);
}

static inline void
sib(jit_state_t *_jit, uint8_t sc, uint8_t i, uint8_t b)
{
  emit_u8(_jit, (sc<<6) | (i<<3) | b);
}

static inline void
ic(jit_state_t *_jit, uint8_t c)
{
  emit_u8(_jit, c);
}

static inline void
is(jit_state_t *_jit, uint16_t s)
{
  emit_u16(_jit, s);
}

static inline void
ii(jit_state_t *_jit, uint32_t i)
{
  emit_u32(_jit, i);
}

#if __X64
static inline void
il(jit_state_t *_jit, uint64_t l)
{
  emit_u64(_jit, l);
}
#endif

static void
rex(jit_state_t *_jit, int32_t l, int32_t w,
    int32_t r, int32_t x, int32_t b)
{
#if __X64
  int32_t v = 0x40 | (w << 3);

  if (r != _NOREG)
    v |= (r & 8) >> 1;
  if (x != _NOREG)
    v |= (x & 8) >> 2;
  if (b != _NOREG)
    v |= (b & 8) >> 3;
  if (l || v != 0x40)
    ic(_jit, v);
#endif
}

static void
rx(jit_state_t *_jit, int32_t rd, int32_t md,
   int32_t rb, int32_t ri, int32_t ms)
{
  if (ri == _NOREG) {
    if (rb == _NOREG) {
#if __X32
      mrm(_jit, 0x00, r7(rd), 0x05);
#else
      mrm(_jit, 0x00, r7(rd), 0x04);
      sib(_jit, _SCL1, 0x04, 0x05);
#endif
      ii(_jit, md);
    } else if (r7(rb) == _RSP_REGNO) {
      if (md == 0) {
        mrm(_jit, 0x00, r7(rd), 0x04);
        sib(_jit, ms, 0x04, 0x04);
      }
      else if ((int8_t)md == md) {
        mrm(_jit, 0x01, r7(rd), 0x04);
        sib(_jit, ms, 0x04, 0x04);
        ic(_jit, md);
      } else {
        mrm(_jit, 0x02, r7(rd), 0x04);
        sib(_jit, ms, 0x04, 0x04);
        ii(_jit, md);
      }
    } else {
      if (md == 0 && r7(rb) != _RBP_REGNO)
        mrm(_jit, 0x00, r7(rd), r7(rb));
      else if ((int8_t)md == md) {
        mrm(_jit, 0x01, r7(rd), r7(rb));
        ic(_jit, md);
      } else {
        mrm(_jit, 0x02, r7(rd), r7(rb));
        ii(_jit, md);
      }
    }
  }
  else if (rb == _NOREG) {
    mrm(_jit, 0x00, r7(rd), 0x04);
    sib(_jit, ms, r7(ri), 0x05);
    ii(_jit, md);
  }
  else if (r8(ri) != _RSP_REGNO) {
    if (md == 0 && r7(rb) != _RBP_REGNO) {
      mrm(_jit, 0x00, r7(rd), 0x04);
      sib(_jit, ms, r7(ri), r7(rb));
    } else if ((int8_t)md == md) {
      mrm(_jit, 0x01, r7(rd), 0x04);
      sib(_jit, ms, r7(ri), r7(rb));
      ic(_jit, md);
    } else {
      mrm(_jit, 0x02, r7(rd), 0x04);
      sib(_jit, ms, r7(ri), r7(rb));
      ic(_jit, md);
    }
  } else {
    fprintf(stderr, "illegal index register");
    abort();
  }
}

static void
pushr(jit_state_t *_jit, int32_t r0)
{
  _jit->frame_size += __WORDSIZE / 8;
  rex(_jit, 0, WIDE, 0, 0, r0);
  ic(_jit, 0x50 | r7(r0));
}

static void
popr(jit_state_t *_jit, int32_t r0)
{
  _jit->frame_size -= __WORDSIZE / 8;
  rex(_jit, 0, WIDE, 0, 0, r0);
  ic(_jit, 0x58 | r7(r0));
}

static void
nop(jit_state_t *_jit, int32_t count)
{
  switch (count) {
  case 0:
    break;
  case 1: /* NOP */
    ic(_jit, 0x90);
    break;
  case 2: /* 66 NOP */
    ic(_jit, 0x66); ic(_jit, 0x90);
    break;
  case 3: /* NOP DWORD ptr [EAX] */
    ic(_jit, 0x0f); ic(_jit, 0x1f); ic(_jit, 0x00);
    break;
  case 4: /* NOP DWORD ptr [EAX + 00H] */
    ic(_jit, 0x0f); ic(_jit, 0x1f); ic(_jit, 0x40); ic(_jit, 0x00);
    break;
  case 5: /* NOP DWORD ptr [EAX + EAX*1 + 00H] */
    ic(_jit, 0x0f); ic(_jit, 0x1f); ic(_jit, 0x44); ic(_jit, 0x00);
    ic(_jit, 0x00);
    break;
  case 6: /* 66 NOP DWORD ptr [EAX + EAX*1 + 00H] */
    ic(_jit, 0x66); ic(_jit, 0x0f); ic(_jit, 0x1f); ic(_jit, 0x44);
    ic(_jit, 0x00); ic(_jit, 0x00);
    break;
  case 7: /* NOP DWORD ptr [EAX + 00000000H] */
    ic(_jit, 0x0f); ic(_jit, 0x1f); ic(_jit, 0x80); ii(_jit, 0x0000);
    break;
  case 8: /* NOP DWORD ptr [EAX + EAX*1 + 00000000H] */
    ic(_jit, 0x0f); ic(_jit, 0x1f); ic(_jit, 0x84); ic(_jit, 0x00);
    ii(_jit, 0x0000);
    break;
  case 9: /* 66 NOP DWORD ptr [EAX + EAX*1 + 00000000H] */
    ic(_jit, 0x66); ic(_jit, 0x0f); ic(_jit, 0x1f); ic(_jit, 0x84);
    ic(_jit, 0x00); ii(_jit, 0x0000);
    break;
  default:
    abort();
  }
}

static void
movr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1) {
    rex(_jit, 0, 1, r1, _NOREG, r0);
    ic(_jit, 0x89);
    ic(_jit, 0xc0 | (r1 << 3) | r7(r0));
  }
}

static void
movcr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x0f);
  ic(_jit, 0xbe);
  mrm(_jit, 0x03, r7(r0), r7(r1));
}

static void
movcr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x0f);
  ic(_jit, 0xb6);
  mrm(_jit, 0x03, r7(r0), r7(r1));
}

static void
movsr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x0f);
  ic(_jit, 0xbf);
  mrm(_jit, 0x03, r7(r0), r7(r1));
}

static void
movsr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x0f);
  ic(_jit, 0xb7);
  mrm(_jit, 0x03, r7(r0), r7(r1));
}

#if __X64
static void
movir(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, 1, r0, _NOREG, r1);
  ic(_jit, 0x63);
  mrm(_jit, 0x03, r7(r0), r7(r1));
}

static void
movir_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, 0, r1, _NOREG, r0);
  ic(_jit, 0x89);
  ic(_jit, 0xc0 | (r1 << 3) | r7(r0));
}
#endif

static jit_reloc_t
mov_addr(jit_state_t *_jit, int32_t r0)
{
  uint8_t *pc_start = _jit->pc.uc;
  rex(_jit, 0, WIDE, _NOREG, _NOREG, r0);
  ic(_jit, 0xb8 | r7(r0));
  ptrdiff_t inst_start = _jit->pc.uc - pc_start;
  return emit_abs_reloc(_jit, inst_start);
}

static void
imovi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
#if __X64
  if (fits_uint32_p(i0)) {
    rex(_jit, 0, 0, _NOREG, _NOREG, r0);
    ic(_jit, 0xb8 | r7(r0));
    ii(_jit, i0);
  } else {
    rex(_jit, 0, 1, _NOREG, _NOREG, r0);
    ic(_jit, 0xb8 | r7(r0));
    il(_jit, i0);
  }
#else
  ic(_jit, 0xb8 | r7(r0));
  ii(_jit, i0);
#endif
}

static void
alur(jit_state_t *_jit, int32_t code, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r1, _NOREG, r0);
  ic(_jit, code | 0x01);
  mrm(_jit, 0x03, r7(r1), r7(r0));
}

static inline void
icmpr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return alur(_jit, X86_CMP, r0, r1);
}
static inline void
iaddr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return alur(_jit, X86_ADD, r0, r1);
}
static inline void
iaddxr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return alur(_jit, X86_ADC, r0, r1);
}
static inline void
isubr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return alur(_jit, X86_SUB, r0, r1);
}
static inline void
isubxr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return alur(_jit, X86_SBB, r0, r1);
}
static inline void
iandr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return alur(_jit, X86_AND, r0, r1);
}
static inline void
iorr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return alur(_jit, X86_OR, r0, r1);
}
static inline void
ixorr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return alur(_jit, X86_XOR, r0, r1);
}

static void
movi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (i0)
    imovi(_jit, r0, i0);
  else
    ixorr(_jit, r0, r0);
}

static void
alui(jit_state_t *_jit, int32_t code, int32_t r0, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, _NOREG, _NOREG, r0);
    if ((int8_t)i0 == i0) {
      ic(_jit, 0x83);
      ic(_jit, 0xc0 | code | r7(r0));
      ic(_jit, i0);
    } else {
      if (r0 == _RAX_REGNO) {
        ic(_jit, code | 0x05);
      } else {
        ic(_jit, 0x81);
        ic(_jit, 0xc0 | code | r7(r0));
      }
      ii(_jit, i0);
    }
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    alur(_jit, code, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static inline void
icmpi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  return alui(_jit, X86_CMP, r0, i0);
}
static inline void
iaddi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  return alui(_jit, X86_ADD, r0, i0);
}
static inline void
iaddxi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  return alui(_jit, X86_ADC, r0, i0);
}
static inline void
isubi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  return alui(_jit, X86_SUB, r0, i0);
}
static inline void
isubxi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  return alui(_jit, X86_SBB, r0, i0);
}
static inline void
iandi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  return alui(_jit, X86_AND, r0, i0);
}
static inline void
iori(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  return alui(_jit, X86_OR, r0, i0);
}
static inline void
ixori(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  return alui(_jit, X86_XOR, r0, i0);
}

static void
unr(jit_state_t *_jit, int32_t code, int32_t r0)
{
  rex(_jit, 0, WIDE, _NOREG, _NOREG, r0);
  ic(_jit, 0xf7);
  mrm(_jit, 0x03, code, r7(r0));
}

static inline void
umulr(jit_state_t *_jit, int32_t r0)
{
  return unr(_jit, X86_IMUL, r0);
}
static inline void
umulr_u(jit_state_t *_jit, int32_t r0)
{
  return unr(_jit, X86_MUL, r0);
}
static inline void
idivr(jit_state_t *_jit, int32_t r0)
{
  return unr(_jit, X86_IDIV, r0);
}
static inline void
idivr_u(jit_state_t *_jit, int32_t r0)
{
  return unr(_jit, X86_DIV, r0);
}
static inline void
inegr(jit_state_t *_jit, int32_t r0)
{
  return unr(_jit, X86_NEG, r0);
}
static inline void
icomr(jit_state_t *_jit, int32_t r0)
{
  return unr(_jit, X86_NOT, r0);
}

#if USE_INC_DEC
static void
incr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  movr(_jit, r0, r1);
#  if __X64
  rex(_jit, 0, WIDE, _NOREG, _NOREG, r0);
  ic(_jit, 0xff);
  ic(_jit, 0xc0 | r7(r0));
#  else
  ic(_jit, 0x40 | r7(r0));
#  endif
}

static void
decr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  movr(_jit, r0, r1);
#  if __X64
  rex(_jit, 0, WIDE, _NOREG, _NOREG, r0);
  ic(_jit, 0xff);
  ic(_jit, 0xc8 | r7(r0));
#  else
  ic(_jit, 0x48 | r7(r0));
#  endif
}
#endif

static void
lea(jit_state_t *_jit, int32_t md, int32_t rb,
     int32_t ri, int32_t ms, int32_t rd)
{
  rex(_jit, 0, WIDE, rd, ri, rb);
  ic(_jit, 0x8d);
  rx(_jit, rd, md, rb, ri, ms);
}

static void
xchgr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r1, _NOREG, r0);
  ic(_jit, 0x87);
  mrm(_jit, 0x03, r7(r1), r7(r0));
}

static void
xchgrm(jit_state_t *_jit, int32_t val_and_dst, int32_t loc)
{
  rex(_jit, 0, WIDE, val_and_dst, _NOREG, loc);
  ic(_jit, 0x87);
  rx(_jit, val_and_dst, 0, loc, _NOREG, _SCL1);
}

static void
lock(jit_state_t *_jit)
{
  ic(_jit, 0xf0);
}

static void
cmpxchgmr(jit_state_t *_jit, int32_t loc, int32_t desired)
{
  lock(_jit);
  rex(_jit, 0, WIDE, desired, _NOREG, loc);
  ic(_jit, 0x0f);
  ic(_jit, 0xb1);
  rx(_jit, desired, 0, loc, _NOREG, _SCL1);
}

static void
testr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r1, _NOREG, r0);
  ic(_jit, 0x85);
  mrm(_jit, 0x03, r7(r1), r7(r0));
}

static void
testi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  rex(_jit, 0, WIDE, _NOREG, _NOREG, r0);
  if (r0 == _RAX_REGNO) {
    ic(_jit, 0xa9);
  } else {
    ic(_jit, 0xf7);
    mrm(_jit, 0x03, 0x00, r7(r0));
  }
  ii(_jit, i0);
}

static void
negr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 == r1) {
    inegr(_jit, r0);
  } else {
    ixorr(_jit, r0, r0);
    isubr(_jit, r0, r1);
  }
}

static void
addr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    iaddr(_jit, r0, r2);
  else if (r0 == r2)
    iaddr(_jit, r0, r1);
  else
    lea(_jit, 0, r1, r2, _SCL1, r0);
}

static void
addi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 == 0)
    movr(_jit, r0, r1);
#if USE_INC_DEC
  else if (i0 == 1)
    incr(_jit, r0, r1);
  else if (i0 == -1)
    decr(_jit, r0, r1);
#endif
  else if (can_sign_extend_int_p(i0)) {
    if (r0 == r1)
      iaddi(_jit, r0, i0);
    else
      lea(_jit, i0, r1, _NOREG, _SCL1, r0);
  }
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    iaddr(_jit, r0, r1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    iaddr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
addcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r2) {
    iaddr(_jit, r0, r1);
  } else {
    movr(_jit, r0, r1);
    iaddr(_jit, r0, r2);
  }
}

static void
addci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    movr(_jit, r0, r1);
    iaddi(_jit, r0, i0);
  }
  else if (r0 == r1) {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    iaddr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  } else {
    movi(_jit, r0, i0);
    iaddr(_jit, r0, r1);
  }
}

static void
addxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r2) {
    iaddxr(_jit, r0, r1);
  } else {
    movr(_jit, r0, r1);
    iaddxr(_jit, r0, r2);
  }
}

static void
addxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    movr(_jit, r0, r1);
    iaddxi(_jit, r0, i0);
  }
  else if (r0 == r1) {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    iaddxr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  } else {
    movi(_jit, r0, i0);
    iaddxr(_jit, r0, r1);
  }
}

static void
subr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r1 == r2)
    ixorr(_jit, r0, r0);
  else if (r0 == r2) {
    isubr(_jit, r0, r1);
    inegr(_jit, r0);
  } else {
    movr(_jit, r0, r1);
    isubr(_jit, r0, r2);
  }
}

static void
subi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 == 0)
    movr(_jit, r0, r1);
#if USE_INC_DEC
  else if (i0 == 1)
    decr(_jit, r0, r1);
  else if (i0 == -1)
    incr(_jit, r0, r1);
#endif
  else if (can_sign_extend_int_p(i0)) {
    if (r0 == r1)
      isubi(_jit, r0, i0);
    else
      lea(_jit, -i0, r1, _NOREG, _SCL1, r0);
  }
  else if (r0 != r1) {
    movi(_jit, r0, -i0);
    iaddr(_jit, r0, r1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    isubr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
subcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r2 && r0 != r1) {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movr(_jit, jit_gpr_regno(reg), r0);
    movr(_jit, r0, r1);
    isubr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  } else {
    movr(_jit, r0, r1);
    isubr(_jit, r0, r2);
  }
}

static void
subci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  movr(_jit, r0, r1);
  if (can_sign_extend_int_p(i0)) {
    isubi(_jit, r0, i0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    isubr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
subxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r2 && r0 != r1) {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movr(_jit, jit_gpr_regno(reg), r0);
    movr(_jit, r0, r1);
    isubxr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  } else {
    movr(_jit, r0, r1);
    isubxr(_jit, r0, r2);
  }
}

static void
subxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  movr(_jit, r0, r1);
  if (can_sign_extend_int_p(i0)) {
    isubxi(_jit, r0, i0);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    imovi(_jit, jit_gpr_regno(reg), i0);
    isubxr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
irotshr(jit_state_t *_jit, int32_t code, int32_t r0)
{
  rex(_jit, 0, WIDE, _RCX_REGNO, _NOREG, r0);
  ic(_jit, 0xd3);
  mrm(_jit, 0x03, code, r7(r0));
}

static void
rotshr(jit_state_t *_jit, int32_t code,
        int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == _RCX_REGNO) {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movr(_jit, jit_gpr_regno(reg), r1);
    if (r2 != _RCX_REGNO)
      movr(_jit, _RCX_REGNO, r2);
    irotshr(_jit, code, jit_gpr_regno(reg));
    movr(_jit, _RCX_REGNO, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  } else if (r2 != _RCX_REGNO) {
    /* Already know that R0 isn't RCX.  */
    pushr(_jit, _RCX_REGNO);
    if (r1 == _RCX_REGNO) {
      if (r0 == r2)
        xchgr(_jit, r0, _RCX_REGNO);
      else {
        movr(_jit, r0, r1);
        movr(_jit, _RCX_REGNO, r2);
      }
    } else {
      movr(_jit, _RCX_REGNO, r2);
      movr(_jit, r0, r1);
    }
    irotshr(_jit, code, r0);
    popr(_jit, _RCX_REGNO);
  } else {
    movr(_jit, r0, r1);
    irotshr(_jit, code, r0);
  }
}

static void
irotshi(jit_state_t *_jit, int32_t code, int32_t r0, jit_word_t i0)
{
  rex(_jit, 0, WIDE, _NOREG, _NOREG, r0);
  if (i0 == 1) {
    ic(_jit, 0xd1);
    mrm(_jit, 0x03, code, r7(r0));
  } else {
    ic(_jit, 0xc1);
    mrm(_jit, 0x03, code, r7(r0));
    ic(_jit, i0);
  }
}

static void
rotshi(jit_state_t *_jit, int32_t code,
       int32_t r0, int32_t r1, jit_word_t i0)
{
  movr(_jit, r0, r1);
  if (i0)
    irotshi(_jit, code, r0, i0);
}

static void
lshi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 == 0)
    movr(_jit, r0, r1);
  else if (i0 <= 3)
    lea(_jit, 0, _NOREG, r1, i0 == 1 ? _SCL2 : i0 == 2 ? _SCL4 : _SCL8, r0);
  else
    rotshi(_jit, X86_SHL, r0, r1, i0);
}

static void
lshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return rotshr(_jit, X86_SHL, r0, r1, r2);
}

static void
rshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return rotshr(_jit, X86_SAR, r0, r1, r2);
}

static void
rshi(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  return rotshi(_jit, X86_SAR, r0, r1, i0);
}

static void
rshr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return rotshr(_jit, X86_SHR, r0, r1, r2);
}

static void
rshi_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  return rotshi(_jit, X86_SHR, r0, r1, i0);
}

static void
imulr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x0f);
  ic(_jit, 0xaf);
  mrm(_jit, 0x03, r7(r0), r7(r1));
}

static void
imuli(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, r0, _NOREG, r1);
    if ((int8_t)i0 == i0) {
      ic(_jit, 0x6b);
      mrm(_jit, 0x03, r7(r0), r7(r1));
      ic(_jit, i0);
    } else {
      ic(_jit, 0x69);
      mrm(_jit, 0x03, r7(r0), r7(r1));
      ii(_jit, i0);
    }
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    imulr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
mulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r0 == r1)
    imulr(_jit, r0, r2);
  else if (r0 == r2) {
    imulr(_jit, r0, r1);
  } else {
    movr(_jit, r0, r1);
    imulr(_jit, r0, r2);
  }
}

static int
ffsw(jit_word_t i)
{
  if (sizeof(int) == sizeof(i))
    return ffs(i);
  int bit = ffs((int)i);
  if (bit == 0) {
    bit = ffs((int)((uint64_t)i >> 32));
    if (bit)
      bit += 32;
  }
  return bit;
}

static void
muli(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  switch (i0) {
  case 0:
    ixorr(_jit, r0, r0);
    break;
  case 1:
    movr(_jit, r0, r1);
    break;
  case -1:
    negr(_jit, r0, r1);
    break;
  case 2:
    lea(_jit, 0, _NOREG, r1, _SCL2, r0);
    break;
  case 4:
    lea(_jit, 0, _NOREG, r1, _SCL4, r0);
    break;
  case 8:
    lea(_jit, 0, _NOREG, r1, _SCL8, r0);
    break;
  default:
    if (i0 > 0 && !(i0 & (i0 - 1)))
      lshi(_jit, r0, r1, ffsw(i0) - 1);
    else if (can_sign_extend_int_p(i0))
      imuli(_jit, r0, r1, i0);
    else if (r0 != r1) {
      movi(_jit, r0, i0);
      imulr(_jit, r0, r1);
    }
    else
      imuli(_jit, r0, r0, i0);
    break;
  }
}

static void
iqmulr(jit_state_t *_jit, int32_t r0, int32_t r1,
        int32_t r2, int32_t r3, jit_bool_t sign)
{
  if (r0 != _RAX_REGNO && r1 != _RAX_REGNO)
    pushr(_jit, _RAX_REGNO);
  if (r0 != _RDX_REGNO && r1 != _RDX_REGNO)
    pushr(_jit, _RDX_REGNO);

  int32_t mul;
  if (r3 == _RAX_REGNO) {
    mul = r2;
  } else {
    mul = r3;
    movr(_jit, _RAX_REGNO, r2);
  }
  if (sign)
    umulr(_jit, mul);
  else
    umulr_u(_jit, mul);

  if (r0 == _RDX_REGNO && r1 == _RAX_REGNO) {
    xchgr(_jit, _RAX_REGNO, _RDX_REGNO);
  } else {
    if (r0 != _RDX_REGNO)
      movr(_jit, r0, _RAX_REGNO);
    movr(_jit, r1, _RDX_REGNO);
    if (r0 == _RDX_REGNO)
      movr(_jit, r0, _RAX_REGNO);
  }

  if (r0 != _RDX_REGNO && r1 != _RDX_REGNO)
    popr(_jit, _RDX_REGNO);
  if (r0 != _RAX_REGNO && r1 != _RAX_REGNO)
    popr(_jit, _RAX_REGNO);
}

static void
qmulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqmulr(_jit, r0, r1, r2, r3, 1);
}

static void
qmulr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqmulr(_jit, r0, r1, r2, r3, 0);
}

static void
iqmuli(jit_state_t *_jit, int32_t r0, int32_t r1,
        int32_t r2, jit_word_t i0, jit_bool_t sign)
{
  if (i0 == 0) {
    ixorr(_jit, r0, r0);
    ixorr(_jit, r1, r1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    if (sign)
      qmulr(_jit, r0, r1, r2, jit_gpr_regno(reg));
    else
      qmulr_u(_jit, r0, r1, r2, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
qmuli(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  return iqmuli(_jit, r0, r1, r2, i0, 1);
}

static void
qmuli_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  return iqmuli(_jit, r0, r1, r2, i0, 0);
}

static void
sign_extend_rdx_rax(jit_state_t *_jit)
{
  rex(_jit, 0, WIDE, 0, 0, 0);
  ic(_jit, 0x99);
}

static void
divremr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2,
         jit_bool_t sign, jit_bool_t divide)
{
  if (r0 != _RAX_REGNO)
    pushr(_jit, _RAX_REGNO);
  if (r0 != _RDX_REGNO)
    pushr(_jit, _RDX_REGNO);

  int tmp_divisor = 0;
  if (r2 == _RAX_REGNO || r2 == _RDX_REGNO) {
    jit_gpr_t tmp = get_temp_gpr(_jit);
    movr(_jit, jit_gpr_regno(tmp), r2);
    r2 = jit_gpr_regno(tmp);
    tmp_divisor = 1;
  }

  movr(_jit, _RAX_REGNO, r1);

  if (sign) {
    sign_extend_rdx_rax(_jit);
    idivr(_jit, r2);
  } else {
    ixorr(_jit, _RDX_REGNO, _RDX_REGNO);
    idivr_u(_jit, r2);
  }

  if (divide)
    movr(_jit, r0, _RAX_REGNO);
  else
    movr(_jit, r0, _RDX_REGNO);

  if (tmp_divisor)
    unget_temp_gpr(_jit);

  if (r0 != _RDX_REGNO)
    popr(_jit, _RDX_REGNO);
  if (r0 != _RAX_REGNO)
    popr(_jit, _RAX_REGNO);
}

static void
divremi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0,
         jit_bool_t sign, jit_bool_t divide)
{
  jit_gpr_t tmp = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(tmp), i0);

  divremr(_jit, r0, r1, jit_gpr_regno(tmp), sign, divide);
}

static void
divr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return divremr(_jit, r0, r1, r2, 1, 1);
}

static void
divi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  return divremi(_jit, r0, r1, i0, 1, 1);
}

static void
divr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return divremr(_jit, r0, r1, r2, 0, 1);
}

static void
divi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  return divremi(_jit, r0, r1, i0, 0, 1);
}


static void
remr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return divremr(_jit, r0, r1, r2, 1, 0);
}

static void
remi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  return divremi(_jit, r0, r1, i0, 1, 0);
}

static void
remr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  return divremr(_jit, r0, r1, r2, 0, 0);
}

static void
remi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  return divremi(_jit, r0, r1, i0, 0, 0);
}

static void
iqdivr(jit_state_t *_jit, int32_t r0, int32_t r1,
        int32_t r2, int32_t r3, jit_bool_t sign)
{
  if (r0 != _RAX_REGNO && r1 != _RAX_REGNO)
    pushr(_jit, _RAX_REGNO);
  if (r0 != _RDX_REGNO && r1 != _RDX_REGNO)
    pushr(_jit, _RDX_REGNO);

  int tmp_divisor = 0;
  if (r3 == _RAX_REGNO || r3 == _RDX_REGNO) {
    jit_gpr_t tmp = get_temp_gpr(_jit);
    movr(_jit, jit_gpr_regno(tmp), r3);
    r3 = jit_gpr_regno(tmp);
    tmp_divisor = 1;
  }

  movr(_jit, _RAX_REGNO, r2);

  if (sign) {
    sign_extend_rdx_rax(_jit);
    idivr(_jit, r3);
  } else {
    ixorr(_jit, _RDX_REGNO, _RDX_REGNO);
    idivr_u(_jit, r3);
  }

  if (r0 == _RDX_REGNO && r1 == _RAX_REGNO) {
    xchgr(_jit, _RAX_REGNO, _RDX_REGNO);
  } else {
    if (r0 != _RDX_REGNO)
      movr(_jit, r0, _RAX_REGNO);
    movr(_jit, r1, _RDX_REGNO);
    if (r0 == _RDX_REGNO)
      movr(_jit, r0, _RAX_REGNO);
  }

  if (tmp_divisor)
    unget_temp_gpr(_jit);

  if (r0 != _RDX_REGNO && r1 != _RDX_REGNO)
    popr(_jit, _RDX_REGNO);
  if (r0 != _RAX_REGNO && r1 != _RAX_REGNO)
    popr(_jit, _RAX_REGNO);
}

static void
qdivr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqdivr(_jit, r0, r1, r2, r3, 1);
}

static void
qdivr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  return iqdivr(_jit, r0, r1, r2, r3, 0);
}

static void
iqdivi(jit_state_t *_jit, int32_t r0, int32_t r1,
       int32_t r2, jit_word_t i0, jit_bool_t sign)
{
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i0);
  if (sign)
    qdivr(_jit, r0, r1, r2, jit_gpr_regno(reg));
  else
    qdivr_u(_jit, r0, r1, r2, jit_gpr_regno(reg));
  unget_temp_gpr(_jit);
}

static void
qdivi(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  return iqdivi(_jit, r0, r1, r2, i0, 1);
}

static void
qdivi_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  return iqdivi(_jit, r0, r1, r2, i0, 0);
}

static void
comr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  movr(_jit, r0, r1);
  icomr(_jit, r0);
}

static void
andr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r1 == r2)
    movr(_jit, r0, r1);
  else if (r0 == r1)
    iandr(_jit, r0, r2);
  else if (r0 == r2) {
    iandr(_jit, r0, r1);
  } else {
    movr(_jit, r0, r1);
    iandr(_jit, r0, r2);
  }
}

static void
andi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{

  if (i0 == 0)
    ixorr(_jit, r0, r0);
  else if (i0 == -1)
    movr(_jit, r0, r1);
  else if (r0 == r1) {
    if (can_sign_extend_int_p(i0)) {
      iandi(_jit, r0, i0);
    } else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      movi(_jit, jit_gpr_regno(reg), i0);
      iandr(_jit, r0, jit_gpr_regno(reg));
      unget_temp_gpr(_jit);
    }
  } else {
    movi(_jit, r0, i0);
    iandr(_jit, r0, r1);
  }
}

static void
orr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r1 == r2) {
    movr(_jit, r0, r1);
  } else if (r0 == r1) {
    iorr(_jit, r0, r2);
  } else if (r0 == r2) {
    iorr(_jit, r0, r1);
  } else {
    movr(_jit, r0, r1);
    iorr(_jit, r0, r2);
  }
}

static void
ori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 == 0)
    movr(_jit, r0, r1);
  else if (i0 == -1)
    movi(_jit, r0, -1);
  else if (can_sign_extend_int_p(i0)) {
    movr(_jit, r0, r1);
    iori(_jit, r0, i0);
  }
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    iorr(_jit, r0, r1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    iorr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
xorr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (r1 == r2)
    ixorr(_jit, r0, r0);
  else if (r0 == r1)
    ixorr(_jit, r0, r2);
  else if (r0 == r2) {
    ixorr(_jit, r0, r1);
  } else {
    movr(_jit, r0, r1);
    ixorr(_jit, r0, r2);
  }
}

static void
xori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (i0 == 0)
    movr(_jit, r0, r1);
  else if (i0 == -1)
    comr(_jit, r0, r1);
  else if (can_sign_extend_int_p(i0)) {
    movr(_jit, r0, r1);
    ixori(_jit, r0, i0);
  }
  else if (r0 != r1) {
    movi(_jit, r0, i0);
    ixorr(_jit, r0, r1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ixorr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
extr_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (reg8_p(r1)) {
    movcr(_jit, r0, r1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movr(_jit, jit_gpr_regno(reg), r1);
    movcr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
extr_uc(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (reg8_p(r1)) {
    movcr_u(_jit, r0, r1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movr(_jit, jit_gpr_regno(reg), r1);
    movcr_u(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
extr_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return movsr(_jit, r0, r1);
}

static void
extr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return movsr_u(_jit, r0, r1);
}

#if __X64
static void
extr_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return movir(_jit, r0, r1);
}
static void
extr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return movir_u(_jit, r0, r1);
}
#endif

static void
bswapr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  extr_us(_jit, r0, r1);
  ic(_jit, 0x66);
  rex(_jit, 0, 0, _NOREG, _NOREG, r0);
  ic(_jit, 0xc1);
  mrm(_jit, 0x03, X86_ROR, r7(r0));
  ic(_jit, 8);
}

static void
bswapr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  movr(_jit, r0, r1);
  rex(_jit, 0, 0, _NOREG, _NOREG, r0);
  ic(_jit, 0x0f);
  ic(_jit, 0xc8 | r7(r0));
}

#if __X64
static void
bswapr_ul(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  movr(_jit, r0, r1);
  rex(_jit, 0, 1, _NOREG, _NOREG, r0);
  ic(_jit, 0x0f);
  ic(_jit, 0xc8 | r7(r0));
}
#endif

static void
ldr_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x0f);
  ic(_jit, 0xbe);
  rx(_jit, r0, 0, r1, _NOREG, _SCL1);
}

static void
ldi_c(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, r0, _NOREG, _NOREG);
    ic(_jit, 0x0f);
    ic(_jit, 0xbe);
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldr_c(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldr_uc(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x0f);
  ic(_jit, 0xb6);
  rx(_jit, r0, 0, r1, _NOREG, _SCL1);
}

static void
ldi_uc(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, r0, _NOREG, _NOREG);
    ic(_jit, 0x0f);
    ic(_jit, 0xb6);
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldr_uc(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldr_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x0f);
  ic(_jit, 0xbf);
  rx(_jit, r0, 0, r1, _NOREG, _SCL1);
}

static void
ldi_s(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, r0, _NOREG, _NOREG);
    ic(_jit, 0x0f);
    ic(_jit, 0xbf);
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldr_s(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x0f);
  ic(_jit, 0xb7);
  rx(_jit, r0, 0, r1, _NOREG, _SCL1);
}

static void
ldi_us(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, r0, _NOREG, _NOREG);
    ic(_jit, 0x0f);
    ic(_jit, 0xb7);
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldr_us(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldr_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
#if __X64
  rex(_jit, 0, WIDE, r0, _NOREG, r1);
  ic(_jit, 0x63);
#else
  ic(_jit, 0x8b);
#endif
  rx(_jit, r0, 0, r1, _NOREG, _SCL1);
}

static void
ldi_i(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
#if __X64
    rex(_jit, 0, WIDE, r0, _NOREG, _NOREG);
    ic(_jit, 0x63);
#else
    ic(_jit, 0x8b);
#endif
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldr_i(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

#if __X64
static void
ldr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, 0, r0, _NOREG, r1);
  ic(_jit, 0x63);
  rx(_jit, r0, 0, r1, _NOREG, _SCL1);
}

static void
ldi_ui(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, 0, r0, _NOREG, _NOREG);
    ic(_jit, 0x63);
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldr_ui(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldr_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, 1, r0, _NOREG, r1);
  ic(_jit, 0x8b);
  rx(_jit, r0, 0, r1, _NOREG, _SCL1);
}

static void
ldi_l(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, 1, r0, _NOREG, _NOREG);
    ic(_jit, 0x8b);
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldr_l(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}
#endif

static void
ldxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  rex(_jit, 0, WIDE, r0, r1, r2);
  ic(_jit, 0x0f);
  ic(_jit, 0xbe);
  rx(_jit, r0, 0, r2, r1, _SCL1);
}

static void
ldxi_c(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, r0, _NOREG, r1);
    ic(_jit, 0x0f);
    ic(_jit, 0xbe);
    rx(_jit, r0, i0, r1, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldxr_c(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldxr_uc(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  rex(_jit, 0, WIDE, r0, r1, r2);
  ic(_jit, 0x0f);
  ic(_jit, 0xb6);
  rx(_jit, r0, 0, r2, r1, _SCL1);
}

static void
ldxi_uc(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, r0, _NOREG, r1);
    ic(_jit, 0x0f);
    ic(_jit, 0xb6);
    rx(_jit, r0, i0, r1, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldxr_uc(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  rex(_jit, 0, WIDE, r0, r1, r2);
  ic(_jit, 0x0f);
  ic(_jit, 0xbf);
  rx(_jit, r0, 0, r2, r1, _SCL1);
}

static void
ldxi_s(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, r0, _NOREG, r1);
    ic(_jit, 0x0f);
    ic(_jit, 0xbf);
    rx(_jit, r0, i0, r1, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldxr_s(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldxr_us(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  rex(_jit, 0, WIDE, r0, r1, r2);
  ic(_jit, 0x0f);
  ic(_jit, 0xb7);
  rx(_jit, r0, 0, r2, r1, _SCL1);
}

static void
ldxi_us(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, WIDE, r0, _NOREG, r1);
    ic(_jit, 0x0f);
    ic(_jit, 0xb7);
    rx(_jit, r0, i0, r1, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldxr_us(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
#if __X64
  rex(_jit, 0, WIDE, r0, r1, r2);
  ic(_jit, 0x63);
#else
  ic(_jit, 0x8b);
#endif
  rx(_jit, r0, 0, r2, r1, _SCL1);
}

static void
ldxi_i(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
#if __X64
    rex(_jit, 0, WIDE, r0, _NOREG, r1);
    ic(_jit, 0x63);
#else
    ic(_jit, 0x8b);
#endif
    rx(_jit, r0, i0, r1, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldxr_i(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

#if __X64
static void
ldxr_ui(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  rex(_jit, 0, 0, r0, r1, r2);
  ic(_jit, 0x8b);
  rx(_jit, r0, 0, r2, r1, _SCL1);
}

static void
ldxi_ui(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, 0, r0, _NOREG, r1);
    ic(_jit, 0x8b);
    rx(_jit, r0, i0, r1, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldxr_ui(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}

static void
ldxr_l(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  rex(_jit, 0, 1, r0, r1, r2);
  ic(_jit, 0x8b);
  rx(_jit, r0, 0, r2, r1, _SCL1);
}

static void
ldxi_l(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, 1, r0, _NOREG, r1);
    ic(_jit, 0x8b);
    rx(_jit, r0, i0, r1, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    ldxr_l(_jit, r0, r1, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
}
#endif

static void stxi_c(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1);

static void
str_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (reg8_p(r1)) {
    rex(_jit, 0, 0, r1, _NOREG, r0);
    ic(_jit, 0x88);
    rx(_jit, r1, 0, r0, _NOREG, _SCL1);
  } else {
    // See comment in stxi_c.
    return stxi_c(_jit, 0, r0, r1);
  }
}

static void
sti_c(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  if (can_sign_extend_int_p(i0)) {
    if (reg8_p(r0)) {
      rex(_jit, 0, 0, r0, _NOREG, _NOREG);
      ic(_jit, 0x88);
      rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
    } else {
      jit_gpr_t reg = get_temp_gpr(_jit);
      movr(_jit, jit_gpr_regno(reg), r0);
      rex(_jit, 0, 0, jit_gpr_regno(reg), _NOREG, _NOREG);
      ic(_jit, 0x88);
      rx(_jit, jit_gpr_regno(reg), i0, _NOREG, _NOREG, _SCL1);
      unget_temp_gpr(_jit);
    }
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    str_c(_jit, jit_gpr_regno(reg), r0);
    unget_temp_gpr(_jit);
  }
}

static void
str_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  ic(_jit, 0x66);
  rex(_jit, 0, 0, r1, _NOREG, r0);
  ic(_jit, 0x89);
  rx(_jit, r1, 0, r0, _NOREG, _SCL1);
}

static void
sti_s(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  if (can_sign_extend_int_p(i0)) {
    ic(_jit, 0x66);
    rex(_jit, 0, 0, r0, _NOREG, _NOREG);
    ic(_jit, 0x89);
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    str_s(_jit, jit_gpr_regno(reg), r0);
    unget_temp_gpr(_jit);
  }
}

static void
str_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, 0, r1, _NOREG, r0);
  ic(_jit, 0x89);
  rx(_jit, r1, 0, r0, _NOREG, _SCL1);
}

static void
sti_i(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, 0, r0, _NOREG, _NOREG);
    ic(_jit, 0x89);
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    str_i(_jit, jit_gpr_regno(reg), r0);
    unget_temp_gpr(_jit);
  }
}

#if __X64
static void
str_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  rex(_jit, 0, 1, r1, _NOREG, r0);
  ic(_jit, 0x89);
  rx(_jit, r1, 0, r0, _NOREG, _SCL1);
}

static void
sti_l(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, 1, r0, _NOREG, _NOREG);
    ic(_jit, 0x89);
    rx(_jit, r0, i0, _NOREG, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    str_l(_jit, jit_gpr_regno(reg), r0);
    unget_temp_gpr(_jit);
  }
}
#endif

static void
stxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  if (reg8_p(r2)) {
    rex(_jit, 0, 0, r2, r1, r0);
    ic(_jit, 0x88);
    rx(_jit, r2, 0, r0, r1, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movr(_jit, jit_gpr_regno(reg), r2);
    rex(_jit, 0, 0, jit_gpr_regno(reg), r1, r0);
    ic(_jit, 0x88);
    rx(_jit, jit_gpr_regno(reg), 0, r0, r1, _SCL1);
    unget_temp_gpr(_jit);
  }
}

static void
stxi_c(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (can_sign_extend_int_p(i0)) {
    if (reg8_p(r1)) {
      rex(_jit, 0, 0, r1, _NOREG, r0);
      ic(_jit, 0x88);
      rx(_jit, r1, i0, r0, _NOREG, _SCL1);
    } else {
      // Here we have a hack.  Normally tmp registers are just for the
      // backend's use, but there are cases in which jit_move_operands
      // can use a temp register too.  In a move of an operand to memory
      // this would result in two simultaneous uses of a temp register.
      // Oddly this situation only applies on 32-bit x86 with byte
      // stores -- this is the only platform on which reg8_p can be
      // false -- so we just make a special case here.
      ASSERT(r0 != r1);
      int32_t tmp = r0 == _RAX_REGNO ? _RCX_REGNO : _RAX_REGNO;
      ASSERT(reg8_p(tmp));
      pushr(_jit, tmp);
      movr(_jit, tmp, r1);
      if (r0 == _RSP_REGNO)
        i0 += __WORDSIZE / 8;
      rex(_jit, 0, 0, tmp, _NOREG, r0);
      ic(_jit, 0x88);
      rx(_jit, tmp, i0, r0, _NOREG, _SCL1);
      popr(_jit, tmp);
    }
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    stxr_c(_jit, jit_gpr_regno(reg), r0, r1);
    unget_temp_gpr(_jit);
  }
}

static void
stxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  ic(_jit, 0x66);
  rex(_jit, 0, 0, r2, r1, r0);
  ic(_jit, 0x89);
  rx(_jit, r2, 0, r0, r1, _SCL1);
}

static void
stxi_s(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (can_sign_extend_int_p(i0)) {
    ic(_jit, 0x66);
    rex(_jit, 0, 0, r1, _NOREG, r0);
    ic(_jit, 0x89);
    rx(_jit, r1, i0, r0, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    stxr_s(_jit, jit_gpr_regno(reg), r0, r1);
    unget_temp_gpr(_jit);
  }
}

static void
stxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  rex(_jit, 0, 0, r2, r1, r0);
  ic(_jit, 0x89);
  rx(_jit, r2, 0, r0, r1, _SCL1);
}

static void
stxi_i(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, 0, r1, _NOREG, r0);
    ic(_jit, 0x89);
    rx(_jit, r1, i0, r0, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    stxr_i(_jit, jit_gpr_regno(reg), r0, r1);
    unget_temp_gpr(_jit);
  }
}

#if __X64
static void
stxr_l(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  rex(_jit, 0, 1, r2, r1, r0);
  ic(_jit, 0x89);
  rx(_jit, r2, 0, r0, r1, _SCL1);
}

static void
stxi_l(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (can_sign_extend_int_p(i0)) {
    rex(_jit, 0, 1, r1, _NOREG, r0);
    ic(_jit, 0x89);
    rx(_jit, r1, i0, r0, _NOREG, _SCL1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i0);
    stxr_l(_jit, jit_gpr_regno(reg), r0, r1);
    unget_temp_gpr(_jit);
  }
}
#endif

static jit_reloc_t
jccs(jit_state_t *_jit, int32_t code)
{
  ic(_jit, 0x70 | code);
  return emit_rel8_reloc(_jit, 1);
}

static jit_reloc_t
jcc(jit_state_t *_jit, int32_t code)
{
  ic(_jit, 0x0f);
  ic(_jit, 0x80 | code);
  return emit_rel32_reloc(_jit, 2);
}

static void
jcci(jit_state_t *_jit, int32_t code, jit_word_t i0)
{
  ptrdiff_t rel8 = i0 - (_jit->pc.w + 1 + 1);
  ptrdiff_t rel32 = i0 - (_jit->pc.w + 2 + 4);
  if (INT8_MIN <= rel8 && rel8 <= INT8_MAX)
    {
      ic(_jit, 0x70 | code);
      ic(_jit, rel8);
    }
  else
    {
      ASSERT(INT32_MIN <= rel32 && rel32 <= INT32_MAX);
      ic(_jit, 0x0f);
      ic(_jit, 0x80 | code);
      ii(_jit, rel32);
    }
}

#define DEFINE_JUMPS(cc, CC, code)                                      \
  static inline jit_reloc_t j##cc(jit_state_t *_jit)                    \
  {                                                                     \
    return jcc(_jit, X86_CC_##CC);                                      \
  }                                                                     \
  static inline jit_reloc_t j##cc##s(jit_state_t *_jit)                 \
  {                                                                     \
    return jccs(_jit, X86_CC_##CC);                                     \
  }
FOR_EACH_CC(DEFINE_JUMPS)
#undef DEFINE_JUMPS

static jit_reloc_t
jcr(jit_state_t *_jit, int32_t code, int32_t r0, int32_t r1)
{
  alur(_jit, X86_CMP, r0, r1);
  return jcc(_jit, code);
}

static jit_reloc_t
jci(jit_state_t *_jit, int32_t code, int32_t r0, jit_word_t i0)
{
  alui(_jit, X86_CMP, r0, i0);
  return jcc(_jit, code);
}

static jit_reloc_t
jci0(jit_state_t *_jit, int32_t code, int32_t r0)
{
  testr(_jit, r0, r0);
  return jcc(_jit, code);
}

static jit_reloc_t
bltr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr(_jit, X86_CC_L, r0, r1);
}

static jit_reloc_t
blti(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1) return jci (_jit, X86_CC_L, r0, i1);
  else    return jci0(_jit, X86_CC_S, r0);
}

static jit_reloc_t
bltr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr(_jit, X86_CC_B, r0, r1);
}

static jit_reloc_t
blti_u(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1) return jci (_jit, X86_CC_B, r0, i1);
  else    return jci0(_jit, X86_CC_B, r0);
}

static jit_reloc_t
bler(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr (_jit, X86_CC_LE, r0, r1);
}

static jit_reloc_t
blei(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1) return jci (_jit, X86_CC_LE, r0, i1);
  else    return jci0(_jit, X86_CC_LE, r0);
}

static jit_reloc_t
bler_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr (_jit, X86_CC_BE, r0, r1);
}

static jit_reloc_t
blei_u(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1) return jci (_jit, X86_CC_BE, r0, i1);
  else    return jci0(_jit, X86_CC_BE, r0);
}

static jit_reloc_t
beqr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr (_jit, X86_CC_E, r0, r1);
}

static jit_reloc_t
beqi(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1) return jci (_jit, X86_CC_E, r0, i1);
  else    return jci0(_jit, X86_CC_E, r0);
}

static jit_reloc_t
bger(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr (_jit, X86_CC_GE, r0, r1);
}

static jit_reloc_t
bgei(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1) return jci (_jit, X86_CC_GE, r0, i1);
  else    return jci0(_jit, X86_CC_NS, r0);
}

static jit_reloc_t
bger_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr (_jit, X86_CC_AE, r0, r1);
}

static jit_reloc_t
bgei_u(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  return jci (_jit, X86_CC_AE, r0, i1);
}

static jit_reloc_t
bgtr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr(_jit, X86_CC_G, r0, r1);
}

static jit_reloc_t
bgti(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  return jci(_jit, X86_CC_G, r0, i1);
}

static jit_reloc_t
bgtr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr(_jit, X86_CC_A, r0, r1);
}

static jit_reloc_t
bgti_u(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1) return jci (_jit, X86_CC_A, r0, i1);
  else    return jci0(_jit, X86_CC_NE, r0);
}

static jit_reloc_t
bner(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return jcr(_jit, X86_CC_NE, r0, r1);
}

static jit_reloc_t
bnei(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (i1) return jci (_jit, X86_CC_NE, r0, i1);
  else    return jci0(_jit, X86_CC_NE, r0);
}

static jit_reloc_t
bmsr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  testr(_jit, r0, r1);
  return jnz(_jit);
}

static jit_reloc_t
bmsi(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_zero_extend_int_p(i1)) {
    testi(_jit, r0, i1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i1);
    testr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
  return jnz(_jit);
}

static jit_reloc_t
bmcr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  testr(_jit, r0, r1);
  return jz(_jit);
}

static jit_reloc_t
bmci(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_zero_extend_int_p(i1)) {
    testi(_jit, r0, i1);
  } else {
    jit_gpr_t reg = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(reg), i1);
    testr(_jit, r0, jit_gpr_regno(reg));
    unget_temp_gpr(_jit);
  }
  return jz(_jit);
}

static jit_reloc_t
boaddr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  iaddr(_jit, r0, r1);
  return jo(_jit);
}

static jit_reloc_t
boaddi(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_sign_extend_int_p(i1)) {
    iaddi(_jit, r0, i1);
    return jo(_jit);
  }
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i1);
  unget_temp_gpr(_jit);
  return boaddr(_jit, r0, jit_gpr_regno(reg));
}

static jit_reloc_t
boaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  iaddr(_jit, r0, r1);
  return jc(_jit);
}

static jit_reloc_t
boaddi_u(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_sign_extend_int_p(i1)) {
    iaddi(_jit, r0, i1);
    return jc(_jit);
  }
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i1);
  unget_temp_gpr(_jit);
  return boaddr_u(_jit, r0, jit_gpr_regno(reg));
}

static jit_reloc_t
bxaddr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  iaddr(_jit, r0, r1);
  return jno(_jit);
}

static jit_reloc_t
bxaddi(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_sign_extend_int_p(i1)) {
    iaddi(_jit, r0, i1);
    return jno(_jit);
  }
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i1);
  unget_temp_gpr(_jit);
  return bxaddr(_jit, r0, jit_gpr_regno(reg));
}

static jit_reloc_t
bxaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  iaddr(_jit, r0, r1);
  return jnc(_jit);
}

static jit_reloc_t
bxaddi_u(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_sign_extend_int_p(i1)) {
    iaddi(_jit, r0, i1);
    return jnc(_jit);
  }
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i1);
  unget_temp_gpr(_jit);
  return bxaddr_u(_jit, r0, jit_gpr_regno(reg));
}

static jit_reloc_t
bosubr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  isubr(_jit, r0, r1);
  return jo(_jit);
}

static jit_reloc_t
bosubi(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_sign_extend_int_p(i1)) {
    isubi(_jit, r0, i1);
    return jo(_jit);
  }
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i1);
  unget_temp_gpr(_jit);
  return bosubr(_jit, r0, jit_gpr_regno(reg));
}

static jit_reloc_t
bosubr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  isubr(_jit, r0, r1);
  return jc(_jit);
}

static jit_reloc_t
bosubi_u(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_sign_extend_int_p(i1)) {
    isubi(_jit, r0, i1);
    return jc(_jit);
  }
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i1);
  unget_temp_gpr(_jit);
  return bosubr_u(_jit, r0, jit_gpr_regno(reg));
}

static jit_reloc_t
bxsubr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  isubr(_jit, r0, r1);
  return jno(_jit);
}

static jit_reloc_t
bxsubi(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_sign_extend_int_p(i1)) {
    isubi(_jit, r0, i1);
    return jno(_jit);
  }
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i1);
  unget_temp_gpr(_jit);
  return bxsubr(_jit, r0, jit_gpr_regno(reg));
}

static jit_reloc_t
bxsubr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  isubr(_jit, r0, r1);
  return jnc(_jit);
}

static jit_reloc_t
bxsubi_u(jit_state_t *_jit, int32_t r0, jit_word_t i1)
{
  if (can_sign_extend_int_p(i1)) {
    isubi(_jit, r0, i1);
    return jnc(_jit);
  }
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), i1);
  unget_temp_gpr(_jit);
  return bxsubr_u(_jit, r0, jit_gpr_regno(reg));
}

static void
callr(jit_state_t *_jit, int32_t r0)
{
  rex(_jit, 0, 0, _NOREG, _NOREG, r0);
  ic(_jit, 0xff);
  mrm(_jit, 0x03, 0x02, r7(r0));
}

static void
calli(jit_state_t *_jit, jit_word_t i0)
{
  ptrdiff_t rel32 = i0 - (_jit->pc.w + 1 + 4);
  if (INT32_MIN <= rel32 && rel32 <= INT32_MAX)
    {
      ic(_jit, 0xe8);
      ii(_jit, rel32);
    }
  else
    {
      jit_gpr_t reg = get_temp_gpr(_jit);
      jit_patch_there(_jit, mov_addr(_jit, jit_gpr_regno(reg)), (void*)i0);
      callr(_jit, jit_gpr_regno(reg));
      unget_temp_gpr(_jit);
    }
}

static void
jmpi_with_link(jit_state_t *_jit, jit_word_t i0)
{
  return calli(_jit, i0);
}

static void
pop_link_register(jit_state_t *_jit)
{
  /* Treat this instruction as having no effect on the stack size; its
   * effect is non-local (across functions) and handled manually.  */

  int saved_frame_size = _jit->frame_size;
  popr(_jit, jit_gpr_regno (JIT_LR));
  _jit->frame_size = saved_frame_size;
}

static void
push_link_register(jit_state_t *_jit)
{
  /* See comment in pop_link_register.  */

  int saved_frame_size = _jit->frame_size;
  pushr(_jit, jit_gpr_regno (JIT_LR));
  _jit->frame_size = saved_frame_size;
}

static void
jmpr(jit_state_t *_jit, int32_t r0)
{
  rex(_jit, 0, WIDE, _NOREG, _NOREG, r0);
  ic(_jit, 0xff);
  mrm(_jit, 0x03, 0x04, r7(r0));
}

static void
jmpi(jit_state_t *_jit, jit_word_t i0)
{
  ptrdiff_t rel8 = i0 - (_jit->pc.w + 1 + 1);
  ptrdiff_t rel32 = i0 - (_jit->pc.w + 1 + 4);
  if (INT8_MIN <= rel8 && rel8 <= INT8_MAX)
    {
      ic(_jit, 0xeb);
      ic(_jit, rel8);
    }
  else if (INT32_MIN <= rel32 && rel32 <= INT32_MAX)
    {
      ic(_jit, 0xe9);
      ii(_jit, rel32);
    }
  else
    {
      jit_gpr_t reg = get_temp_gpr(_jit);
      jit_patch_there(_jit, mov_addr(_jit, jit_gpr_regno(reg)), (void*)i0);
      jmpr(_jit, jit_gpr_regno(reg));
      unget_temp_gpr(_jit);
    }
}

static jit_reloc_t
jmp(jit_state_t *_jit)
{
  ic(_jit, 0xe9);
  return emit_rel32_reloc(_jit, 1);
}

static void
ret(jit_state_t *_jit)
{
  ic(_jit, 0xc3);
}

static void
retr(jit_state_t *_jit, int32_t r0)
{
  movr(_jit, _RAX_REGNO, r0);
  ret(_jit);
}

static void
reti(jit_state_t *_jit, jit_word_t i0)
{
  movi(_jit, _RAX_REGNO, i0);
  ret(_jit);
}

static void
retval_c(jit_state_t *_jit, int32_t r0)
{
  extr_c(_jit, r0, _RAX_REGNO);
}

static void
retval_uc(jit_state_t *_jit, int32_t r0)
{
  extr_uc(_jit, r0, _RAX_REGNO);
}

static void
retval_s(jit_state_t *_jit, int32_t r0)
{
  extr_s(_jit, r0, _RAX_REGNO);
}

static void
retval_us(jit_state_t *_jit, int32_t r0)
{
  extr_us(_jit, r0, _RAX_REGNO);
}

static void
retval_i(jit_state_t *_jit, int32_t r0)
{
#if __X32
  movr(_jit, r0, _RAX_REGNO);
#else
  extr_i(_jit, r0, _RAX_REGNO);
#endif
}

#if __X64
static void
retval_ui(jit_state_t *_jit, int32_t r0)
{
  extr_ui(_jit, r0, _RAX_REGNO);
}

static void
retval_l(jit_state_t *_jit, int32_t r0)
{
  movr(_jit, r0, _RAX_REGNO);
}
#endif

static void
mfence(jit_state_t *_jit)
{
  ic(_jit, 0x0f);
  ic(_jit, 0xae);
  ic(_jit, 0xf0);
}

static void
ldr_atomic(jit_state_t *_jit, int32_t dst, int32_t loc)
{
#if __X64
  ldr_l(_jit, dst, loc);
#else
  ldr_i(_jit, dst, loc);
#endif
}

static void
str_atomic(jit_state_t *_jit, int32_t loc, int32_t val)
{
#if __X64
  str_l(_jit, loc, val);
#else
  str_i(_jit, loc, val);
#endif
  mfence(_jit);
}

static void
swap_atomic(jit_state_t *_jit, int32_t dst, int32_t loc, int32_t val)
{
  if (dst == val) {
    xchgrm(_jit, dst, loc);
  } else {
    int32_t tmp = jit_gpr_regno(get_temp_gpr(_jit));
    movr(_jit, tmp, val);
    xchgrm(_jit, tmp, loc);
    movr(_jit, dst, tmp);
    unget_temp_gpr(_jit);
  }
}

static void
cas_atomic(jit_state_t *_jit, int32_t dst, int32_t loc, int32_t expected,
           int32_t desired)
{
  ASSERT(loc != expected);
  ASSERT(loc != desired);

  if (dst == jit_gpr_regno(_RAX)) {
    if (loc == dst) {
      int32_t tmp = jit_gpr_regno(get_temp_gpr(_jit));
      movr(_jit, tmp ,loc);
      movr(_jit, dst, expected);
      cmpxchgmr(_jit, tmp, desired);
      unget_temp_gpr(_jit);
    } else {
      movr(_jit, dst, expected);
      cmpxchgmr(_jit, loc, desired);
    }
  } else if (loc == jit_gpr_regno(_RAX)) {
    int32_t tmp = jit_gpr_regno(get_temp_gpr(_jit));
    movr(_jit, tmp, loc);
    movr(_jit, jit_gpr_regno(_RAX), expected);
    cmpxchgmr(_jit, tmp, desired);
    movr(_jit, dst, jit_gpr_regno(_RAX));
    movr(_jit, loc, tmp);
    unget_temp_gpr(_jit);
  } else if (expected == jit_gpr_regno(_RAX)) {
    int32_t tmp = jit_gpr_regno(get_temp_gpr(_jit));
    movr(_jit, tmp, expected);
    cmpxchgmr(_jit, loc, desired);
    movr(_jit, dst, jit_gpr_regno(_RAX));
    movr(_jit, expected, tmp);
    unget_temp_gpr(_jit);
  } else if (desired == jit_gpr_regno(_RAX)) {
    int32_t tmp = jit_gpr_regno(get_temp_gpr(_jit));
    movr(_jit, tmp, desired);
    movr(_jit, jit_gpr_regno(_RAX), expected);
    cmpxchgmr(_jit, loc, tmp);
    movr(_jit, dst, jit_gpr_regno(_RAX));
    movr(_jit, desired, tmp);
    unget_temp_gpr(_jit);
  } else {
    int32_t tmp = jit_gpr_regno(get_temp_gpr(_jit));
    movr(_jit, tmp, jit_gpr_regno(_RAX));
    movr(_jit, jit_gpr_regno(_RAX), expected);
    cmpxchgmr(_jit, loc, desired);
    movr(_jit, dst, jit_gpr_regno(_RAX));
    movr(_jit, jit_gpr_regno(_RAX), tmp);
    unget_temp_gpr(_jit);
  }
}

static void
breakpoint(jit_state_t *_jit)
{
  ic(_jit, 0xcc);
}
