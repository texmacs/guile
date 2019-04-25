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

/*
 * Types
 */
#if __X32 || __CYGWIN__
typedef jit_pointer_t jit_va_list_t;
#else
typedef struct jit_va_list {
  int32_t             gpoff;
  int32_t             fpoff;
  jit_pointer_t       over;
  jit_pointer_t       save;
  /* Declared explicitly as int64 for the x32 abi */
  int64_t             rdi;
  int64_t             rsi;
  int64_t             rdx;
  int64_t             rcx;
  int64_t             r8;
  int64_t             r9;
  jit_float64_t       xmm0;
  jit_float64_t       _up0;
  jit_float64_t       xmm1;
  jit_float64_t       _up1;
  jit_float64_t       xmm2;
  jit_float64_t       _up2;
  jit_float64_t       xmm3;
  jit_float64_t       _up3;
  jit_float64_t       xmm4;
  jit_float64_t       _up4;
  jit_float64_t       xmm5;
  jit_float64_t       _up5;
  jit_float64_t       xmm6;
  jit_float64_t       _up6;
  jit_float64_t       xmm7;
  jit_float64_t       _up7;
} jit_va_list_t;
#endif

jit_cpu_t               jit_cpu;

#include "x86-cpu.c"
#include "x86-sse.c"

jit_bool_t
jit_get_cpu(void)
{
  union {
    struct {
      uint32_t sse3               : 1;
      uint32_t pclmulqdq    : 1;
      uint32_t dtes64       : 1;    /* amd reserved */
      uint32_t monitor      : 1;
      uint32_t ds_cpl       : 1;    /* amd reserved */
      uint32_t vmx          : 1;    /* amd reserved */
      uint32_t smx          : 1;    /* amd reserved */
      uint32_t est          : 1;    /* amd reserved */
      uint32_t tm2          : 1;    /* amd reserved */
      uint32_t ssse3        : 1;
      uint32_t cntx_id      : 1;    /* amd reserved */
      uint32_t __reserved0  : 1;
      uint32_t fma          : 1;
      uint32_t cmpxchg16b   : 1;
      uint32_t xtpr         : 1;    /* amd reserved */
      uint32_t pdcm         : 1;    /* amd reserved */
      uint32_t __reserved1  : 1;
      uint32_t pcid         : 1;    /* amd reserved */
      uint32_t dca          : 1;    /* amd reserved */
      uint32_t sse4_1       : 1;
      uint32_t sse4_2       : 1;
      uint32_t x2apic       : 1;    /* amd reserved */
      uint32_t movbe        : 1;    /* amd reserved */
      uint32_t popcnt       : 1;
      uint32_t tsc          : 1;    /* amd reserved */
      uint32_t aes          : 1;
      uint32_t xsave        : 1;
      uint32_t osxsave      : 1;
      uint32_t avx          : 1;
      uint32_t __reserved2  : 1;    /* amd F16C */
      uint32_t __reserved3  : 1;
      uint32_t __alwayszero : 1;    /* amd RAZ */
    } bits;
    jit_uword_t     cpuid;
  } ecx;
  union {
    struct {
      uint32_t fpu          : 1;
      uint32_t vme          : 1;
      uint32_t de           : 1;
      uint32_t pse          : 1;
      uint32_t tsc          : 1;
      uint32_t msr          : 1;
      uint32_t pae          : 1;
      uint32_t mce          : 1;
      uint32_t cmpxchg8b    : 1;
      uint32_t apic         : 1;
      uint32_t __reserved0  : 1;
      uint32_t sep          : 1;
      uint32_t mtrr         : 1;
      uint32_t pge          : 1;
      uint32_t mca          : 1;
      uint32_t cmov         : 1;
      uint32_t pat          : 1;
      uint32_t pse36        : 1;
      uint32_t psn          : 1;    /* amd reserved */
      uint32_t clfsh        : 1;
      uint32_t __reserved1  : 1;
      uint32_t ds           : 1;    /* amd reserved */
      uint32_t acpi         : 1;    /* amd reserved */
      uint32_t mmx          : 1;
      uint32_t fxsr         : 1;
      uint32_t sse          : 1;
      uint32_t sse2         : 1;
      uint32_t ss           : 1;    /* amd reserved */
      uint32_t htt          : 1;
      uint32_t tm           : 1;    /* amd reserved */
      uint32_t __reserved2  : 1;
      uint32_t pbe          : 1;    /* amd reserved */
    } bits;
    jit_uword_t     cpuid;
  } edx;
#if __X32
  int ac, flags;
#endif
  jit_uword_t         eax, ebx;

#if __X32
  /* adapted from glibc __sysconf */
  __asm__ volatile ("pushfl;\n\t"
                    "popl %0;\n\t"
                    "movl $0x240000, %1;\n\t"
                    "xorl %0, %1;\n\t"
                    "pushl %1;\n\t"
                    "popfl;\n\t"
                    "pushfl;\n\t"
                    "popl %1;\n\t"
                    "xorl %0, %1;\n\t"
                    "pushl %0;\n\t"
                    "popfl"
                    : "=r" (flags), "=r" (ac));

  /* i386 or i486 without cpuid */
  if ((ac & (1 << 21)) == 0)
    /* probably without x87 as well */
    return false;
#endif

    /* query %eax = 1 function */
  __asm__ volatile (
#if __X32 || __X64_32
                    "xchgl %%ebx, %1; cpuid; xchgl %%ebx, %1"
#else
                    "xchgq %%rbx, %1; cpuid; xchgq %%rbx, %1"
#endif
                    : "=a" (eax), "=r" (ebx),
                    "=c" (ecx.cpuid), "=d" (edx.cpuid)
                    : "0" (1));

  jit_cpu.fpu         = edx.bits.fpu;
  jit_cpu.cmpxchg8b   = edx.bits.cmpxchg8b;
  jit_cpu.cmov        = edx.bits.cmov;
  jit_cpu.mmx         = edx.bits.mmx;
  jit_cpu.sse         = edx.bits.sse;
  jit_cpu.sse2        = edx.bits.sse2;
  jit_cpu.sse3        = ecx.bits.sse3;
  jit_cpu.pclmulqdq   = ecx.bits.pclmulqdq;
  jit_cpu.ssse3       = ecx.bits.ssse3;
  jit_cpu.fma         = ecx.bits.fma;
  jit_cpu.cmpxchg16b  = ecx.bits.cmpxchg16b;
  jit_cpu.sse4_1      = ecx.bits.sse4_1;
  jit_cpu.sse4_2      = ecx.bits.sse4_2;
  jit_cpu.movbe       = ecx.bits.movbe;
  jit_cpu.popcnt      = ecx.bits.popcnt;
  jit_cpu.aes         = ecx.bits.aes;
  jit_cpu.avx         = ecx.bits.avx;

    /* query %eax = 0x80000001 function */
  __asm__ volatile (
#if __X64
#  if __X64_32
                    "xchgl %%ebx, %1; cpuid; xchgl %%ebx, %1"
#  else
                    "xchgq %%rbx, %1; cpuid; xchgq %%rbx, %1"
#  endif
                    : "=a" (eax), "=r" (ebx),
                    "=c" (ecx.cpuid), "=d" (edx.cpuid)
                    : "0" (0x80000001));
  jit_cpu.lahf        = ecx.cpuid & 1;
#endif

  return jit_cpu.sse2;
}

jit_bool_t
jit_init(jit_state_t *_jit)
{
  return jit_cpu.sse2;
}

void
jit_epilog(jit_state_t *_jit)
{
  /* TODO: Restore registers.  */
}

static jit_bool_t
is_fpr_arg(enum jit_operand_abi arg)
{
  switch (arg)
    {
    case JIT_OPERAND_ABI_UINT8:
    case JIT_OPERAND_ABI_INT8:
    case JIT_OPERAND_ABI_UINT16:
    case JIT_OPERAND_ABI_INT16:
    case JIT_OPERAND_ABI_UINT32:
    case JIT_OPERAND_ABI_INT32:
    case JIT_OPERAND_ABI_UINT64:
    case JIT_OPERAND_ABI_INT64:
    case JIT_OPERAND_ABI_POINTER:
      return 0;
    case JIT_OPERAND_ABI_FLOAT:
    case JIT_OPERAND_ABI_DOUBLE:
      return 1;
    default:
      abort();
    }
}

static jit_bool_t
is_gpr_arg(enum jit_operand_abi arg)
{
  return !is_fpr_arg(arg);
}

static void
abi_imm_to_gpr(jit_state_t *_jit, enum jit_operand_abi abi, jit_gpr_t dst,
               intptr_t imm)
{
  switch (abi) {
  case JIT_OPERAND_ABI_UINT8:
    ASSERT(0 <= imm);
    ASSERT(imm <= UINT8_MAX);
    break;
  case JIT_OPERAND_ABI_INT8:
    ASSERT(INT8_MIN <= imm);
    ASSERT(imm <= INT8_MAX);
    break;
  case JIT_OPERAND_ABI_UINT16:
    ASSERT(0 <= imm);
    ASSERT(imm <= UINT16_MAX);
    break;
  case JIT_OPERAND_ABI_INT16:
    ASSERT(INT16_MIN <= imm);
    ASSERT(imm <= INT16_MAX);
    break;
  case JIT_OPERAND_ABI_UINT32:
    ASSERT(0 <= imm);
    ASSERT(imm <= UINT32_MAX);
    break;
  case JIT_OPERAND_ABI_INT32:
    ASSERT(INT32_MIN <= imm);
    ASSERT(imm <= INT32_MAX);
    break;
#if __WORDSIZE > 32
  case JIT_OPERAND_ABI_UINT64:
  case JIT_OPERAND_ABI_INT64:
    break;
#endif
  case JIT_OPERAND_ABI_POINTER:
    break;
  default:
    abort();
  }
  jit_movi (_jit, dst, imm);
}

static void
abi_gpr_to_mem(jit_state_t *_jit, enum jit_operand_abi abi,
               jit_gpr_t base, ptrdiff_t offset, jit_gpr_t src)
{
  switch (abi) {
  case JIT_OPERAND_ABI_UINT8:
  case JIT_OPERAND_ABI_INT8:
    jit_stxi_c(_jit, offset, base, src);
    break;
  case JIT_OPERAND_ABI_UINT16:
  case JIT_OPERAND_ABI_INT16:
    jit_stxi_s(_jit, offset, base, src);
    break;
  case JIT_OPERAND_ABI_UINT32:
  case JIT_OPERAND_ABI_INT32:
#if __WORDSIZE == 32
  case JIT_OPERAND_ABI_POINTER:
#endif
    jit_stxi_i(_jit, offset, base, src);
    break;
#if __WORDSIZE == 64
  case JIT_OPERAND_ABI_UINT64:
  case JIT_OPERAND_ABI_INT64:
  case JIT_OPERAND_ABI_POINTER:
    jit_stxi_l(_jit, offset, base, src);
    break;
#endif
  default:
    abort();
  }
}

static void
abi_fpr_to_mem(jit_state_t *_jit, enum jit_operand_abi abi,
               jit_gpr_t base, ptrdiff_t offset, jit_fpr_t src)
{
  switch (abi) {
  case JIT_OPERAND_ABI_FLOAT:
    jit_stxi_f(_jit, offset, base, src);
    break;
  case JIT_OPERAND_ABI_DOUBLE:
    jit_stxi_d(_jit, offset, base, src);
    break;
  default:
    abort();
  }
}

static void
abi_mem_to_gpr(jit_state_t *_jit, enum jit_operand_abi abi,
               jit_gpr_t dst, jit_gpr_t base, ptrdiff_t offset)
{
  switch (abi) {
  case JIT_OPERAND_ABI_UINT8:
    jit_ldxi_uc(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_INT8:
    jit_ldxi_c(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_UINT16:
    jit_ldxi_us(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_INT16:
    jit_ldxi_s(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_UINT32:
    jit_ldxi_ui(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_INT32:
    jit_ldxi_i(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_UINT64:
    jit_ldxi_l(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_INT64:
    jit_ldxi_l(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_POINTER:
    jit_ldxi_l(_jit, dst, base, offset);
    break;
  default:
    abort();
  }
}

static void
abi_mem_to_fpr(jit_state_t *_jit, enum jit_operand_abi abi,
               jit_fpr_t dst, jit_gpr_t base, ptrdiff_t offset)
{
  switch (abi) {
  case JIT_OPERAND_ABI_FLOAT:
    jit_ldxi_f(_jit, dst, base, offset);
    break;
  case JIT_OPERAND_ABI_DOUBLE:
    jit_ldxi_d(_jit, dst, base, offset);
    break;
  default:
    abort();
  }
}

static void
abi_imm_to_mem(jit_state_t *_jit, enum jit_operand_abi abi, jit_gpr_t base,
               ptrdiff_t offset, intmax_t imm)
{
  ASSERT(!is_fpr_arg(abi));

  jit_gpr_t tmp = get_temp_gpr(_jit);
  abi_imm_to_gpr(_jit, abi, tmp, imm);
  abi_gpr_to_mem(_jit, abi, base, offset, tmp);
  unget_temp_gpr(_jit);
}

static void
abi_mem_to_mem(jit_state_t *_jit, enum jit_operand_abi abi, jit_gpr_t base,
               ptrdiff_t offset, jit_gpr_t src_base, ptrdiff_t src_offset)
{
  if (is_gpr_arg (abi)) {
    jit_gpr_t tmp = get_temp_gpr(_jit);
    abi_mem_to_gpr(_jit, abi, tmp, src_base, src_offset);
    abi_gpr_to_mem(_jit, abi, base, offset, tmp);
    unget_temp_gpr(_jit);
  } else {
    jit_fpr_t tmp = get_temp_xpr(_jit);
    abi_mem_to_fpr(_jit, abi, tmp, src_base, src_offset);
    abi_fpr_to_mem(_jit, abi, base, offset, tmp);
    unget_temp_xpr(_jit);
  }
}

#define MOVE_KIND(a, b) ((((int) a) << 4) | ((int) b))

#define MOVE_KIND_ENUM(a, b) \
  MOVE_##a##_TO_##b = MOVE_KIND(JIT_OPERAND_KIND_##a, JIT_OPERAND_KIND_##b)
enum move_kind {
  MOVE_KIND_ENUM(IMM, GPR),
  MOVE_KIND_ENUM(GPR, GPR),
  MOVE_KIND_ENUM(MEM, GPR),
  MOVE_KIND_ENUM(FPR, FPR),
  MOVE_KIND_ENUM(MEM, FPR),
  MOVE_KIND_ENUM(IMM, MEM),
  MOVE_KIND_ENUM(GPR, MEM),
  MOVE_KIND_ENUM(FPR, MEM),
  MOVE_KIND_ENUM(MEM, MEM)
};
#undef MOVE_KIND_ENUM

static void
move_operand(jit_state_t *_jit, jit_operand_t dst, jit_operand_t src)
{
  switch (MOVE_KIND (src.kind, dst.kind)) {
  case MOVE_IMM_TO_GPR:
    return abi_imm_to_gpr(_jit, src.abi, dst.loc.gpr.gpr, src.loc.imm);

  case MOVE_GPR_TO_GPR:
    return jit_movr(_jit, dst.loc.gpr.gpr, src.loc.gpr.gpr);

  case MOVE_MEM_TO_GPR:
    return abi_mem_to_gpr(_jit, src.abi, dst.loc.gpr.gpr, src.loc.mem.base,
                          src.loc.mem.offset);

  case MOVE_FPR_TO_FPR:
    return jit_movr_d(_jit, dst.loc.fpr, src.loc.fpr);

  case MOVE_MEM_TO_FPR:
    return abi_mem_to_fpr(_jit, src.abi, dst.loc.fpr, src.loc.mem.base,
                          src.loc.mem.offset);

  case MOVE_IMM_TO_MEM:
    return abi_imm_to_mem(_jit, src.abi, dst.loc.mem.base, dst.loc.mem.offset,
                          src.loc.imm);

  case MOVE_GPR_TO_MEM:
    return abi_gpr_to_mem(_jit, src.abi, dst.loc.mem.base, dst.loc.mem.offset,
                          src.loc.gpr.gpr);

  case MOVE_FPR_TO_MEM:
    return abi_fpr_to_mem(_jit, src.abi, dst.loc.mem.base, dst.loc.mem.offset,
                          src.loc.fpr);

  case MOVE_MEM_TO_MEM:
    return abi_mem_to_mem(_jit, src.abi, dst.loc.mem.base, dst.loc.mem.offset,
                          src.loc.mem.base, src.loc.mem.offset);

  default:
    abort();
  }
}

// A direct transliteration of "Tilting at windmills with Coq: formal
// verification of a compilation algorithm for parallel moves" by
// Laurence Rideau, Bernard Paul Serpette, and Xavier Leroy:
// https://xavierleroy.org/publi/parallel-move.pdf

enum move_status { TO_MOVE, BEING_MOVED, MOVED };

static inline int
already_in_place(jit_operand_t src, jit_operand_t dst)
{
  switch (MOVE_KIND(src.kind, dst.kind)) {
  case MOVE_GPR_TO_GPR:
    return jit_same_gprs (src.loc.gpr.gpr, dst.loc.gpr.gpr);
  case MOVE_FPR_TO_FPR:
    return jit_same_fprs (src.loc.fpr, dst.loc.fpr);
  case MOVE_MEM_TO_MEM:
    return jit_same_gprs (src.loc.mem.base, dst.loc.mem.base) &&
      src.loc.mem.offset == dst.loc.mem.offset;
  default:
    return 0;
  }
}

static inline int
write_would_clobber(jit_operand_t src, jit_operand_t dst)
{
  if (already_in_place (src, dst))
    return 1;

  if (MOVE_KIND(src.kind, dst.kind) == MOVE_MEM_TO_GPR)
    return jit_same_gprs(src.loc.mem.base, dst.loc.gpr.gpr);

  return 0;
}

static inline ptrdiff_t
operand_addend(jit_operand_t op)
{
  switch (op.kind) {
  case JIT_OPERAND_KIND_GPR:
    return op.loc.gpr.addend;
  case JIT_OPERAND_KIND_MEM:
    return op.loc.mem.addend;
  default:
    abort();
  }
}

static void
move_one(jit_state_t *_jit, jit_operand_t *dst, jit_operand_t *src,
         size_t argc, enum move_status *status, size_t i)
{
  int tmp_gpr = 0, tmp_fpr = 0;

  if (already_in_place(src[i], dst[i]))
    return;

  status[i] = BEING_MOVED;
  for (size_t j = 0; j < argc; j++) {
    if (write_would_clobber(src[j], dst[i])) {
      switch (status[j]) {
      case TO_MOVE:
        move_one(_jit, dst, src, argc, status, j);
        break;
      case BEING_MOVED: {
        jit_operand_t tmp;
        if (is_fpr_arg (src[j].kind)) {
          tmp_fpr = 1;
          tmp = jit_operand_fpr(src[j].abi, get_temp_xpr(_jit));
        } else {
          tmp_gpr = 1;
          /* Preserve addend, if any, from source operand, to be applied
             at the end.  */
          tmp = jit_operand_gpr_with_addend(src[j].abi, get_temp_gpr(_jit),
                                            operand_addend(src[j]));
        }
        move_operand (_jit, tmp, src[j]);
        src[j] = tmp;
        break;
      }
      case MOVED:
        break;
      default:
        abort ();
      }
    }
  }

  move_operand (_jit, dst[i], src[i]);
  status[i] = MOVED;
  if (tmp_gpr)
    unget_temp_gpr(_jit);
  else if (tmp_fpr)
    unget_temp_xpr(_jit);
}

static void
apply_addend(jit_state_t *_jit, jit_operand_t dst, jit_operand_t src)
{
  switch (MOVE_KIND(src.kind, dst.kind)) {
  case MOVE_GPR_TO_GPR:
  case MOVE_MEM_TO_GPR:
    if (operand_addend(src))
      jit_addi(_jit, dst.loc.gpr.gpr, dst.loc.gpr.gpr, operand_addend(src));
    break;
  case MOVE_GPR_TO_MEM:
  case MOVE_MEM_TO_MEM:
    if (operand_addend(src)) {
      jit_gpr_t tmp = get_temp_gpr(_jit);
      abi_mem_to_gpr(_jit, dst.abi, tmp, dst.loc.mem.base, dst.loc.mem.offset);
      jit_addi(_jit, tmp, tmp, operand_addend(src));
      abi_gpr_to_mem(_jit, dst.abi, dst.loc.mem.base, dst.loc.mem.offset, tmp);
    }
    break;
  default:
    break;
  }
}

/* Preconditions: No dest operand is IMM.  No dest operand aliases
   another dest operand.  No dest MEM operand uses a base register which
   is used as a dest GPR.  No dst operand has an addend.  The registers
   returned by get_temp_gpr and get_temp_fpr do not appear in source or
   dest args.  */
void
jit_move_operands(jit_state_t *_jit, jit_operand_t *dst, jit_operand_t *src,
                  size_t argc)
{
  // Check preconditions, except the condition about tmp registers.
  {
    uint64_t src_gprs = 0;
    uint64_t dst_gprs = 0;
    uint64_t dst_fprs = 0;
    uint64_t dst_mem_base_gprs = 0;
    for (size_t i = 0; i < argc; i++) {
      switch (src[i].kind) {
      case JIT_OPERAND_KIND_GPR:
        src_gprs |= 1ULL << rn(src[i].loc.gpr.gpr);
        break;
      case JIT_OPERAND_KIND_FPR:
      case JIT_OPERAND_KIND_IMM:
      case JIT_OPERAND_KIND_MEM:
        break;
      default:
        abort();
      }
      switch (dst[i].kind) {
      case JIT_OPERAND_KIND_GPR: {
        ASSERT(dst[i].loc.gpr.addend == 0);
        uint64_t bit = 1ULL << rn(dst[i].loc.gpr.gpr);
        ASSERT((dst_gprs & bit) == 0);
        dst_gprs |= bit;
        break;
      }
      case JIT_OPERAND_KIND_FPR: {
        uint64_t bit = 1ULL << rn(dst[i].loc.fpr);
        ASSERT((dst_fprs & bit) == 0);
        dst_fprs |= bit;
        break;
      }
      case JIT_OPERAND_KIND_MEM: {
        ASSERT(dst[i].loc.mem.addend == 0);
        uint64_t bit = 1ULL << rn(dst[i].loc.mem.base);
        dst_mem_base_gprs |= bit;
        break;
      }
      case JIT_OPERAND_KIND_IMM:
      default:
        abort();
        break;
      }
    }
    ASSERT(((src_gprs | dst_gprs) & dst_mem_base_gprs) == 0);
  }

  enum move_status status[argc];
  for (size_t i = 0; i < argc; i++)
    status[i] = TO_MOVE;
  for (size_t i = 0; i < argc; i++)
    if (status[i] == TO_MOVE)
      move_one(_jit, dst, src, argc, status, i);

  // Apply addends at the end.  We could do it earlier in some cases but
  // at least at the end we know that an in-place increment of one
  // operand won't alias another.
  for (size_t i = 0; i < argc; i++)
    apply_addend(_jit, dst[i], src[i]);
}

static const jit_gpr_t abi_gpr_args[] = {
#if __X32
  /* No GPRs in args.  */
#elif __CYGWIN__
  _RCX, _RDX, _R8, _R9
#else
  _RDI, _RSI, _RDX, _RCX, _R8, _R9
#endif
};

static const jit_fpr_t abi_fpr_args[] = {
#if __X32
  /* No FPRs in args.  */
#elif __CYGWIN__
  _XMM0, _XMM1, _XMM2, _XMM3
#else
  _XMM0, _XMM1, _XMM2, _XMM3, _XMM4, _XMM5, _XMM6, _XMM7
#endif
};

static const int abi_gpr_arg_count = sizeof(abi_gpr_args) / sizeof(abi_gpr_args[0]);
static const int abi_fpr_arg_count = sizeof(abi_fpr_args) / sizeof(abi_fpr_args[0]);

struct abi_arg_iterator
{
  const jit_operand_t *args;
  size_t argc;

  size_t arg_idx;
  size_t gpr_idx;
  size_t fpr_idx;
  size_t stack_size;
  size_t stack_padding;
};

static size_t
jit_operand_abi_sizeof(enum jit_operand_abi abi)
{
  switch (abi) {
  case JIT_OPERAND_ABI_UINT8:
  case JIT_OPERAND_ABI_INT8:
    return 1;
  case JIT_OPERAND_ABI_UINT16:
  case JIT_OPERAND_ABI_INT16:
    return 2;
  case JIT_OPERAND_ABI_UINT32:
  case JIT_OPERAND_ABI_INT32:
    return 4;
  case JIT_OPERAND_ABI_UINT64:
  case JIT_OPERAND_ABI_INT64:
    return 8;
  case JIT_OPERAND_ABI_POINTER:
    return CHOOSE_32_64(4, 8);
  case JIT_OPERAND_ABI_FLOAT:
    return 4;
  case JIT_OPERAND_ABI_DOUBLE:
    return 8;
  default:
    abort();
  }
}

static size_t
round_size_up_to_words(size_t bytes)
{
  size_t word_size = CHOOSE_32_64(4, 8);
  size_t words = (bytes + word_size - 1) / word_size;
  return words * word_size;
}

static void
reset_abi_arg_iterator(struct abi_arg_iterator *iter, size_t argc,
                       const jit_operand_t *args)
{
  memset(iter, 0, sizeof *iter);
  iter->argc = argc;
  iter->args = args;
}

static void
next_abi_arg(struct abi_arg_iterator *iter, jit_operand_t *arg)
{
  ASSERT(iter->arg_idx < iter->argc);
  enum jit_operand_abi abi = iter->args[iter->arg_idx].abi;
  if (is_gpr_arg(abi) && iter->gpr_idx < abi_gpr_arg_count) {
    *arg = jit_operand_gpr (abi, abi_gpr_args[iter->gpr_idx++]);
#ifdef __CYGWIN__
    iter->fpr_idx++;
#endif
  } else if (is_fpr_arg(abi) && iter->fpr_idx < abi_fpr_arg_count) {
    *arg = jit_operand_fpr (abi, abi_fpr_args[iter->fpr_idx++]);
#ifdef __CYGWIN__
    iter->gpr_idx++;
#endif
  } else {
    *arg = jit_operand_mem (abi, JIT_SP, iter->stack_size);
    size_t bytes = jit_operand_abi_sizeof (abi);
    iter->stack_size += round_size_up_to_words (bytes);
  }
  iter->arg_idx++;
}

// Precondition: stack is already aligned.
static size_t
prepare_call_args(jit_state_t *_jit, size_t argc, jit_operand_t args[])
{
  jit_operand_t dst[argc];
  struct abi_arg_iterator iter;
  
  // Compute shuffle destinations and space for spilled arguments.
  reset_abi_arg_iterator(&iter, argc, args);
  for (size_t i = 0; i < argc; i++)
    next_abi_arg(&iter, &dst[i]);

  size_t stack_size = iter.stack_size;

  // Reserve space for spilled arguments, and fix up SP-relative
  // operands.
  if (stack_size)
    {
      // Align stack to 16-byte boundaries on 64-bit targets.
      if (__WORDSIZE == 64)
        stack_size = (stack_size + 15) & ~15;
      jit_subi(_jit, JIT_SP, JIT_SP, stack_size);
      for (size_t i = 0; i < argc; i++) {
        switch(args[i].kind) {
        case JIT_OPERAND_KIND_GPR:
          if (jit_same_gprs (args[i].loc.mem.base, JIT_SP))
            args[i].loc.gpr.addend += stack_size;
          break;
        case JIT_OPERAND_KIND_MEM:
          if (jit_same_gprs (args[i].loc.mem.base, JIT_SP))
            args[i].loc.mem.offset += stack_size;
          break;
        default:
          break;
        }
      }
    }

  jit_move_operands(_jit, dst, args, argc);

  return stack_size;
}

void
jit_calli(jit_state_t *_jit, jit_pointer_t f, size_t argc, jit_operand_t args[])
{
  size_t spill_size = prepare_call_args(_jit, argc, args);

  calli(_jit, (jit_word_t)f);

  if (spill_size)
    jit_addi(_jit, JIT_SP, JIT_SP, spill_size);
}

void
jit_callr(jit_state_t *_jit, jit_gpr_t f, size_t argc, jit_operand_t args[])
{
  size_t spill_size = prepare_call_args(_jit, argc, args);

  callr(_jit, rn(f));

  if (spill_size)
    jit_addi(_jit, JIT_SP, JIT_SP, spill_size);
}

void
jit_locate_args(jit_state_t *_jit, size_t argc, jit_operand_t args[])
{
  struct abi_arg_iterator iter;
    
  reset_abi_arg_iterator(&iter, argc, args);
  for (size_t i = 0; i < argc; i++)
    next_abi_arg(&iter, &args[i]);
}

/* Precondition: args are distinct locations of type GPR or FPR.  All
   addends of arg operands are zero.  No GPR arg is SP.  */
void
jit_load_args(jit_state_t *_jit, size_t argc, jit_operand_t args[])
{
  jit_operand_t src[argc];

  memcpy(src, args, sizeof(src[0]) * argc);

  jit_locate_args(_jit, argc, src);
  jit_move_operands(_jit, args, src, argc);
}

void
jit_flush(void *fptr, void *tptr)
{
}

static void
jit_try_shorten(jit_state_t *_jit, jit_reloc_t reloc)
{
}
