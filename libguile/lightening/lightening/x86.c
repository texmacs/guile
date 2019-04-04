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

#if __X32
# define jit_arg_reg_p(i)              0
# define jit_arg_f_reg_p(i)            0
# define stack_framesize               20
# define stack_adjust                  12
# define CVT_OFFSET                    -12
# define REAL_WORDSIZE                 4
# define va_gp_increment               4
# define va_fp_increment               8
#else
# if __CYGWIN__
#  define jit_arg_reg_p(i)            ((i) >= 0 && (i) < 4)
#  define jit_arg_f_reg_p(i)          jit_arg_reg_p(i)
#  define stack_framesize             152
#  define va_fp_increment             8
# else
#  define jit_arg_reg_p(i)            ((i) >= 0 && (i) < 6)
#  define jit_arg_f_reg_p(i)          ((i) >= 0 && (i) < 8)
#  define stack_framesize             56
#  define first_gp_argument           rdi
#  define first_gp_offset             offsetof(jit_va_list_t, rdi)
#  define first_gp_from_offset(gp)    ((gp) / 8)
#  define last_gp_argument            r9
#  define va_gp_max_offset                                            \
      (offsetof(jit_va_list_t, r9) - offsetof(jit_va_list_t, rdi) + 8)
#  define first_fp_argument           xmm0
#  define first_fp_offset             offsetof(jit_va_list_t, xmm0)
#  define last_fp_argument            xmm7
#  define va_fp_max_offset                                            \
      (offsetof(jit_va_list_t, xmm7) - offsetof(jit_va_list_t, rdi) + 16)
#  define va_fp_increment             16
#  define first_fp_from_offset(fp)    (((fp) - va_gp_max_offset) / 16)
# endif
# define va_gp_increment               8
# define stack_adjust                  8
# define CVT_OFFSET                    -8
# define REAL_WORDSIZE                 8
#endif

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
static const jit_register_t _rvs[] = {
#if __X32
  { rc(gpr) | rc(rg8) | 0,            "%eax" },
  { rc(gpr) | rc(rg8) | 1,            "%ecx" },
  { rc(gpr) | rc(rg8) | 2,            "%edx" },
  { rc(sav) | rc(rg8) | rc(gpr) | 3,  "%ebx" },
  { rc(sav) | rc(gpr) | 6,            "%esi" },
  { rc(sav) | rc(gpr) | 7,            "%edi" },
  { rc(sav) | 4,                      "%esp" },
  { rc(sav) | 5,                      "%ebp" },
  { rc(xpr) | rc(fpr) | 0,            "%xmm0" },
  { rc(xpr) | rc(fpr) | 1,            "%xmm1" },
  { rc(xpr) | rc(fpr) | 2,            "%xmm2" },
  { rc(xpr) | rc(fpr) | 3,            "%xmm3" },
  { rc(xpr) | rc(fpr) | 4,            "%xmm4" },
  { rc(xpr) | rc(fpr) | 5,            "%xmm5" },
  { rc(xpr) | rc(fpr) | 6,            "%xmm6" },
  { rc(xpr) | rc(fpr) | 7,            "%xmm7" },
#elif __CYGWIN__
  { rc(gpr) | rc(rg8) | 0,            "%rax" },
  { rc(gpr) | rc(rg8) | rc(rg8) | 10, "%r10" },
  { rc(gpr) | rc(rg8) | rc(rg8) | 11, "%r11" },
  { rc(sav) | rc(rg8) | rc(gpr) | 3,  "%rbx" },
  { rc(sav) | rc(gpr) | 7,            "%rdi" },
  { rc(sav) | rc(gpr) | 6,            "%rsi" },
  { rc(sav) | rc(gpr) | 12,           "%r12" },
  { rc(sav) | rc(gpr) | 13,           "%r13" },
  { rc(sav) | rc(gpr) | 14,           "%r14" },
  { rc(sav) | rc(gpr) | 15,           "%r15" },
  { rc(arg) | rc(rg8) | rc(gpr) | 9,  "%r9" },
  { rc(arg) | rc(rg8) | rc(gpr) | 8,  "%r8" },
  { rc(arg) | rc(rg8) | rc(gpr) | 2,  "%rdx" },
  { rc(arg) | rc(rg8) | rc(gpr) | 1,  "%rcx" },
  { rc(sav) | 4,                      "%rsp" },
  { rc(sav) | 5,                      "%rbp" },
  { rc(xpr) | rc(fpr) | 4,            "%xmm4" },
  { rc(xpr) | rc(fpr) | 5,            "%xmm5" },
  { rc(sav) | rc(xpr) | rc(fpr) | 6,  "%xmm6" },
  { rc(sav) | rc(xpr) | rc(fpr) | 7,  "%xmm7" },
  { rc(sav) | rc(xpr) | rc(fpr) | 8,  "%xmm8" },
  { rc(sav) | rc(xpr) | rc(fpr) | 9,  "%xmm9" },
  { rc(sav) | rc(xpr) | rc(fpr) | 10, "%xmm10" },
  { rc(sav) | rc(xpr) | rc(fpr) | 11, "%xmm11" },
  { rc(sav) | rc(xpr) | rc(fpr) | 12, "%xmm12" },
  { rc(sav) | rc(xpr) | rc(fpr) | 13, "%xmm13" },
  { rc(sav) | rc(xpr) | rc(fpr) | 14, "%xmm14" },
  { rc(sav) | rc(xpr) | rc(fpr) | 15, "%xmm15" },
  { rc(xpr) | rc(arg) | rc(fpr) | 3,  "%xmm3" },
  { rc(xpr) | rc(arg) | rc(fpr) | 2,  "%xmm2" },
  { rc(xpr) | rc(arg) | rc(fpr) | 1,  "%xmm1" },
  { rc(xpr) | rc(arg) | rc(fpr) | 0,  "%xmm0" },
#else
  /* %rax is a pseudo flag argument for varargs functions */
  { rc(arg) | rc(gpr) | rc(rg8) | 0,  "%rax" },
  { rc(gpr) | rc(rg8) | 10,           "%r10" },
  { rc(gpr) | rc(rg8) | 11,           "%r11" },
  { rc(gpr) | rc(rg8) | 12,           "%r12" },
  { rc(sav) | rc(rg8) | rc(gpr) | 3,  "%rbx" },
  { rc(sav) | rc(rg8) | rc(gpr) | 13, "%r13" },
  { rc(sav) | rc(rg8) | rc(gpr) | 14, "%r14" },
  { rc(sav) | rc(rg8) | rc(gpr) | 15, "%r15" },
  { rc(arg) | rc(rg8) | rc(gpr) | 9,  "%r9" },
  { rc(arg) | rc(rg8) | rc(gpr) | 8,  "%r8" },
  { rc(arg) | rc(rg8) | rc(gpr) | 1,  "%rcx" },
  { rc(arg) | rc(rg8) | rc(gpr) | 2,  "%rdx" },
  { rc(arg) | rc(rg8) | rc(gpr) | 6,  "%rsi" },
  { rc(arg) | rc(rg8) | rc(gpr) | 7,  "%rdi" },
  { rc(sav) | 4,                      "%rsp" },
  { rc(sav) | 5,                      "%rbp" },
  { rc(xpr) | rc(fpr) | 8,            "%xmm8" },
  { rc(xpr) | rc(fpr) | 9,            "%xmm9" },
  { rc(xpr) | rc(fpr) | 10,           "%xmm10" },
  { rc(xpr) | rc(fpr) | 11,           "%xmm11" },
  { rc(xpr) | rc(fpr) | 12,           "%xmm12" },
  { rc(xpr) | rc(fpr) | 13,           "%xmm13" },
  { rc(xpr) | rc(fpr) | 14,           "%xmm14" },
  { rc(xpr) | rc(fpr) | 15,           "%xmm15" },
  { rc(xpr) | rc(arg) | rc(fpr) | 7,  "%xmm7" },
  { rc(xpr) | rc(arg) | rc(fpr) | 6,  "%xmm6" },
  { rc(xpr) | rc(arg) | rc(fpr) | 5,  "%xmm5" },
  { rc(xpr) | rc(arg) | rc(fpr) | 4,  "%xmm4" },
  { rc(xpr) | rc(arg) | rc(fpr) | 3,  "%xmm3" },
  { rc(xpr) | rc(arg) | rc(fpr) | 2,  "%xmm2" },
  { rc(xpr) | rc(arg) | rc(fpr) | 1,  "%xmm1" },
  { rc(xpr) | rc(arg) | rc(fpr) | 0,  "%xmm0" },
#endif
  { _NOREG,                           "<none>" },
};

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
is_fpr_arg(jit_arg_abi_t arg)
{
  switch (arg)
    {
    case JIT_ARG_ABI_UINT8:
    case JIT_ARG_ABI_INT8:
    case JIT_ARG_ABI_UINT16:
    case JIT_ARG_ABI_INT16:
    case JIT_ARG_ABI_UINT32:
    case JIT_ARG_ABI_INT32:
    case JIT_ARG_ABI_UINT64:
    case JIT_ARG_ABI_INT64:
    case JIT_ARG_ABI_POINTER:
      return 0;
    case JIT_ARG_ABI_FLOAT:
    case JIT_ARG_ABI_DOUBLE:
      return 1;
    default:
      abort();
    }
}

static jit_bool_t
is_gpr_arg(jit_arg_abi_t arg)
{
  return !is_fpr_arg(arg);
}

static const jit_gpr_t abi_gpr_args[] = {
#if __X32
  /* No GPRs in args.  */
#elif __CYGWIN__
  JIT_GPR(_RCX), JIT_GPR(_RDX), JIT_GPR(_R8), JIT_GPR(_R9)
#else
  JIT_GPR(_RDI), JIT_GPR(_RSI), JIT_GPR(_RDX), JIT_GPR(_RCX), JIT_GPR(_R8), JIT_GPR(_R9)
#endif
};

static const jit_fpr_t abi_fpr_args[] = {
#if __X32
  /* No FPRs in args.  */
#elif __CYGWIN__
  JIT_FPR(_XMM0), JIT_FPR(_XMM1), JIT_FPR(_XMM2), JIT_FPR(_XMM3)
#else
  JIT_FPR(_XMM0), JIT_FPR(_XMM1), JIT_FPR(_XMM2), JIT_FPR(_XMM3), JIT_FPR(_XMM4), JIT_FPR(_XMM5), JIT_FPR(_XMM6), JIT_FPR(_XMM7)
#endif
};

static const int abi_gpr_arg_count = sizeof(abi_gpr_args) / sizeof(abi_gpr_args[0]);
static const int abi_fpr_arg_count = sizeof(abi_fpr_args) / sizeof(abi_fpr_args[0]);

struct abi_arg_iterator
{
  const jit_arg_abi_t *abi;
  size_t argc;

  size_t arg_idx;
  size_t gpr_idx;
  size_t fpr_idx;
  size_t stack_size;
};

static size_t
jit_arg_abi_sizeof(jit_arg_abi_t abi)
{
  switch (abi) {
  case JIT_ARG_ABI_UINT8:
  case JIT_ARG_ABI_INT8:
    return 1;
  case JIT_ARG_ABI_UINT16:
  case JIT_ARG_ABI_INT16:
    return 2;
  case JIT_ARG_ABI_UINT32:
  case JIT_ARG_ABI_INT32:
    return 4;
  case JIT_ARG_ABI_UINT64:
  case JIT_ARG_ABI_INT64:
    return 8;
  case JIT_ARG_ABI_POINTER:
    return CHOOSE_32_64(4, 8);
  case JIT_ARG_ABI_FLOAT:
    return 4;
  case JIT_ARG_ABI_DOUBLE:
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
                       const jit_arg_abi_t *abi)
{
  memset(iter, 0, sizeof *iter);
  iter->argc = argc;
  iter->abi = abi;
}

static void
next_abi_arg(struct abi_arg_iterator *iter, jit_arg_t *arg)
{
  ASSERT(iter->arg_idx < iter->argc);
  jit_arg_abi_t abi = iter->abi[iter->arg_idx];
  if (is_gpr_arg(abi)) {
    if (iter->gpr_idx < abi_gpr_arg_count) {
      arg->kind = JIT_ARG_LOC_GPR;
      arg->loc.gpr = abi_gpr_args[iter->gpr_idx++];
#ifdef __CYGWIN__
      iter->fpr_idx++;
#endif
    } else {
      arg->kind = JIT_ARG_LOC_MEM;
      arg->loc.mem.base = JIT_GPR(_RSP);
      arg->loc.mem.offset = iter->stack_size;
      size_t bytes = jit_arg_abi_sizeof (abi);
      iter->stack_size += round_size_up_to_words (bytes);
    }
  } else {
    ASSERT(is_fpr_arg(abi));
    if (iter->fpr_idx < abi_fpr_arg_count) {
      arg->kind = JIT_ARG_LOC_FPR;
      arg->loc.fpr = abi_fpr_args[iter->fpr_idx++];
#ifdef __CYGWIN__
      iter->gpr_idx++;
#endif
    } else {
      arg->kind = JIT_ARG_LOC_MEM;
      arg->loc.mem.base = JIT_GPR(_RSP);
      arg->loc.mem.offset = iter->stack_size;
      size_t bytes = jit_arg_abi_sizeof (abi);
      iter->stack_size += round_size_up_to_words (bytes);
    }
  }
  iter->arg_idx++;
}

static void
abi_imm_to_gpr(jit_state_t *_jit, jit_arg_abi_t abi, jit_gpr_t dst, intptr_t imm)
{
  switch (abi) {
  case JIT_ARG_ABI_UINT8:
    ASSERT(0 <= imm);
    ASSERT(imm <= UINT8_MAX);
    break;
  case JIT_ARG_ABI_INT8:
    ASSERT(INT8_MIN <= imm);
    ASSERT(imm <= INT8_MAX);
    break;
  case JIT_ARG_ABI_UINT16:
    ASSERT(0 <= imm);
    ASSERT(imm <= UINT16_MAX);
    break;
  case JIT_ARG_ABI_INT16:
    ASSERT(INT16_MIN <= imm);
    ASSERT(imm <= INT16_MAX);
    break;
  case JIT_ARG_ABI_UINT32:
    ASSERT(0 <= imm);
    ASSERT(imm <= UINT32_MAX);
    break;
  case JIT_ARG_ABI_INT32:
    ASSERT(INT32_MIN <= imm);
    ASSERT(imm <= INT32_MAX);
    break;
#if __WORDSIZE > 32
  case JIT_ARG_ABI_UINT64:
  case JIT_ARG_ABI_INT64:
    break;
#endif
  case JIT_ARG_ABI_POINTER:
    break;
  default:
    abort();
  }
  jit_movi (_jit, dst, imm);
}

static void
abi_gpr_to_mem(jit_state_t *_jit, jit_arg_abi_t abi,
               jit_gpr_t src, jit_gpr_t base, ptrdiff_t offset)
{
  switch (abi) {
  case JIT_ARG_ABI_UINT8:
  case JIT_ARG_ABI_INT8:
    jit_stxi_c(_jit, offset, base, src);
    break;
  case JIT_ARG_ABI_UINT16:
  case JIT_ARG_ABI_INT16:
    jit_stxi_s(_jit, offset, base, src);
    break;
  case JIT_ARG_ABI_UINT32:
  case JIT_ARG_ABI_INT32:
#if __WORDSIZE == 32
  case JIT_ARG_ABI_POINTER:
#endif
    jit_stxi_i(_jit, offset, base, src);
    break;
#if __WORDSIZE == 64
  case JIT_ARG_ABI_UINT64:
  case JIT_ARG_ABI_INT64:
  case JIT_ARG_ABI_POINTER:
    jit_stxi_l(_jit, offset, base, src);
    break;
#endif
  default:
    abort();
  }
}

static void
abi_fpr_to_mem(jit_state_t *_jit, jit_arg_abi_t abi,
               jit_fpr_t src, jit_gpr_t base, ptrdiff_t offset)
{
  switch (abi) {
  case JIT_ARG_ABI_FLOAT:
    jit_stxi_f(_jit, offset, base, src);
    break;
  case JIT_ARG_ABI_DOUBLE:
    jit_stxi_d(_jit, offset, base, src);
    break;
  default:
    abort();
  }
}

static void
abi_mem_to_gpr(jit_state_t *_jit, jit_arg_abi_t abi,
               jit_gpr_t dst, jit_gpr_t base, ptrdiff_t offset)
{
  switch (abi) {
  case JIT_ARG_ABI_UINT8:
    jit_ldxi_uc(_jit, dst, base, offset);
    break;
  case JIT_ARG_ABI_INT8:
    jit_ldxi_c(_jit, dst, base, offset);
    break;
  case JIT_ARG_ABI_UINT16:
    jit_ldxi_us(_jit, dst, base, offset);
    break;
  case JIT_ARG_ABI_INT16:
    jit_ldxi_s(_jit, dst, base, offset);
    break;
  case JIT_ARG_ABI_UINT32:
    jit_ldxi_ui(_jit, dst, base, offset);
    break;
  case JIT_ARG_ABI_INT32:
    jit_ldxi_i(_jit, dst, base, offset);
    break;
  case JIT_ARG_ABI_UINT64:
    jit_ldxi_l(_jit, dst, base, offset);
    break;
  case JIT_ARG_ABI_INT64:
    jit_ldxi_l(_jit, dst, base, offset);
    break;
  case JIT_ARG_ABI_POINTER:
    jit_ldxi_l(_jit, dst, base, offset);
    break;
  default:
    abort();
  }
}

static void
abi_mem_to_fpr(jit_state_t *_jit, jit_arg_abi_t abi,
               jit_fpr_t dst, jit_gpr_t base, ptrdiff_t offset)
{
  switch (abi) {
  case JIT_ARG_ABI_FLOAT:
    jit_ldxi_f(_jit, dst, base, offset);
    break;
  case JIT_ARG_ABI_DOUBLE:
    jit_ldxi_d(_jit, dst, base, offset);
    break;
  default:
    abort();
  }
}

static void
store_mem_abi_arg(jit_state_t *_jit, jit_arg_abi_t abi,
                  jit_arg_t *arg, jit_gpr_t base, ptrdiff_t offset)
{
  switch (arg->kind) {
  case JIT_ARG_LOC_GPR:
    abi_gpr_to_mem(_jit, abi, arg->loc.gpr, base, offset);
    break;

  case JIT_ARG_LOC_FPR:
    abi_fpr_to_mem(_jit, abi, arg->loc.fpr, base, offset);
    break;

  case JIT_ARG_LOC_MEM:
    if (is_gpr_arg(abi)) {
      jit_gpr_t tmp = get_temp_gpr(_jit);
      abi_mem_to_gpr(_jit, abi, tmp, arg->loc.mem.base, arg->loc.mem.offset);
      abi_gpr_to_mem(_jit, abi, tmp, base, offset);
      unget_temp_gpr(_jit);
    } else {
      jit_fpr_t tmp = get_temp_xpr(_jit);
      abi_mem_to_fpr(_jit, abi, tmp, arg->loc.mem.base, arg->loc.mem.offset);
      abi_fpr_to_mem(_jit, abi, tmp, base, offset);
      unget_temp_xpr(_jit);
    }
    break;

  case JIT_ARG_LOC_IMM: {
    if (is_gpr_arg(abi)) {
      jit_gpr_t tmp = get_temp_gpr(_jit);
      abi_imm_to_gpr(_jit, abi, tmp, arg->loc.imm);
      abi_gpr_to_mem(_jit, abi, tmp, base, offset);
      unget_temp_gpr(_jit);
    } else {
      /* Floating-point immediates not supported yet.  */
      abort ();
    }
    break;
  }

  default:
    abort();
  }

  arg->kind = JIT_ARG_LOC_MEM;
  arg->loc.mem.base = base;
  arg->loc.mem.offset = offset;
}

static void
shuffle_gpr_arg(jit_state_t *_jit, jit_gpr_t dst, size_t argc,
                jit_arg_t *args, size_t idx)
{
  ASSERT(args[idx].kind == JIT_ARG_LOC_GPR);

  if (rn(args[idx].loc.gpr) == rn(dst))
    return;

  /* Arg in a reg but it's not the right one.  See if this reg
     holds some other arg, and swap if so.  */
  for (size_t j=idx+1; j<argc; j++)
    if (args[j].kind == JIT_ARG_LOC_GPR && rn(args[j].loc.gpr) == rn(dst))
      {
        xchgr(_jit, rn(args[idx].loc.gpr), rn(dst));
        args[j].loc.gpr = args[idx].loc.gpr;
        args[idx].loc.gpr = dst;
        /* Could be this register holds a value for more than one argument;
           update subsequent args if any.  */
        for (size_t k=j+1; k<argc; k++)
          if (args[k].kind == JIT_ARG_LOC_GPR && rn(args[k].loc.gpr) == rn(dst))
            args[k].loc.gpr = args[j].loc.gpr;
        return;
      }

  /* Arg in reg, but it's not the right one, and the desired reg
     is free.  */
  jit_movr(_jit, dst, args[idx].loc.gpr);
  args[idx].loc.gpr = dst;
}

static void
shuffle_fpr_arg(jit_state_t *_jit, jit_fpr_t dst, size_t argc,
                jit_arg_t *args, size_t idx)
{
  ASSERT(args[idx].kind == JIT_ARG_LOC_FPR);

  if (rn(args[idx].loc.fpr) == rn(dst))
    return;

  /* Arg in a reg but it's not the right one.  See if this reg
     holds some other arg, and swap if so.  */
  for (size_t j=idx+1; j<argc; j++)
    if (args[j].kind == JIT_ARG_LOC_FPR && rn(args[j].loc.fpr) == rn(dst))
      {
        jit_fpr_t tmp = get_temp_xpr(_jit);
        jit_movr_d (_jit, tmp, args[idx].loc.fpr);
        jit_movr_d (_jit, args[idx].loc.fpr, dst);
        jit_movr_d (_jit, dst, tmp);
        unget_temp_xpr(_jit);
        args[j].loc.fpr = args[idx].loc.fpr;
        args[idx].loc.fpr = dst;
        /* Could be this register holds a value for more than one argument;
           update subsequent args if any.  */
        for (size_t k=j+1; k<argc; k++)
          if (args[k].kind == JIT_ARG_LOC_FPR && rn(args[k].loc.fpr) == rn(dst))
            args[k].loc.fpr = args[j].loc.fpr;
        return;
      }

  /* Arg in reg, but it's not the right one, and the desired reg
     is free.  */
  jit_movr_d(_jit, dst, args[idx].loc.fpr);
  args[idx].loc.fpr = dst;
}

static void
prepare_args(jit_state_t *_jit, size_t argc, const jit_arg_abi_t abi[],
             jit_arg_t args[])
{
  jit_arg_t scratch;
  struct abi_arg_iterator iter;
  
  // Compute stack arg size.
  reset_abi_arg_iterator(&iter, argc, abi);
  for (size_t i = 0; i < argc; i++)
    next_abi_arg(&iter, &scratch);

  // Put all ABI memory arguments in place.  We do this first because it might
  // free up some registers.
  if (iter.stack_size)
    {
      size_t stack_size = iter.stack_size;
      subi(_jit, _RSP_REGNO, _RSP_REGNO, stack_size);
      reset_abi_arg_iterator(&iter, argc, abi);
      for (size_t i = 0; i < argc; i++) {
        next_abi_arg(&iter, &scratch);
        if (scratch.kind == JIT_ARG_LOC_MEM)
          store_mem_abi_arg(_jit, abi[i], &args[i],
                            scratch.loc.mem.base, scratch.loc.mem.offset);
      }
    }

  // We move on now to the ABI register arguments.  All args whose values are in
  // registers are ABI register arguments, but they might not be the right
  // register for the correponding ABI argument.  Note that there may be ABI
  // register arguments whose values are still in memory or as immediates; we
  // will load them later.
  reset_abi_arg_iterator(&iter, argc, abi);
  for (size_t i = 0; i < argc; i++)
    {
      next_abi_arg(&iter, &scratch);
      switch (scratch.kind) {
      case JIT_ARG_LOC_GPR:
        if (args[i].kind == JIT_ARG_LOC_GPR)
          shuffle_gpr_arg(_jit, scratch.loc.gpr, argc, args, i);
        break;
        
      case JIT_ARG_LOC_FPR:
        if (args[i].kind == JIT_ARG_LOC_FPR)
          shuffle_fpr_arg(_jit, scratch.loc.fpr, argc, args, i);
        break;

      default:
        break;
      }
    }
  
  // The only thing that's left is ABI register arguments whose values are still
  // in memory or immediates; load them now.
  reset_abi_arg_iterator(&iter, argc, abi);
  for (size_t i = 0; i < argc; i++)
    {
      next_abi_arg(&iter, &scratch);
      switch (scratch.kind) {
      case JIT_ARG_LOC_GPR:
        if (args[i].kind == JIT_ARG_LOC_MEM) {
          abi_mem_to_gpr(_jit, abi[i], scratch.loc.gpr, args[i].loc.mem.base,
                         args[i].loc.mem.offset);
          args[i].kind = JIT_ARG_LOC_GPR;
          args[i].loc.gpr = scratch.loc.gpr;
        } else if (args[i].kind == JIT_ARG_LOC_IMM) {
          abi_imm_to_gpr(_jit, abi[i], scratch.loc.gpr, args[i].loc.imm);
          args[i].kind = JIT_ARG_LOC_GPR;
          args[i].loc.gpr = scratch.loc.gpr;
        }
        break;
        
      case JIT_ARG_LOC_FPR:
        if (args[i].kind == JIT_ARG_LOC_MEM) {
          abi_mem_to_fpr(_jit, abi[i], scratch.loc.fpr, args[i].loc.mem.base,
                         args[i].loc.mem.offset);
          args[i].kind = JIT_ARG_LOC_FPR;
          args[i].loc.fpr = scratch.loc.fpr;
        } else if (args[i].kind == JIT_ARG_LOC_IMM) {
          /* Currently unsupported.  */
          abort ();
        }
        break;

      default:
        break;
      }
    }
}

static void
cleanup_stack_after_call(jit_state_t *_jit, size_t argc,
                         const jit_arg_abi_t abi[])
{
  jit_arg_t scratch;
  struct abi_arg_iterator iter;

  // Compute stack arg size.
  reset_abi_arg_iterator(&iter, argc, abi);
  for (size_t i = 0; i < argc; i++)
    next_abi_arg(&iter, &scratch);

  if (iter.stack_size)
    jit_addi(_jit, JIT_SP, JIT_SP, iter.stack_size);
}

void
jit_calli(jit_state_t *_jit, jit_pointer_t f,
          size_t argc, const jit_arg_abi_t abi[], jit_arg_t args[])
{
  prepare_args(_jit, argc, abi, args);

  calli(_jit, (jit_word_t)f);

  cleanup_stack_after_call(_jit, argc, abi);
}

void
jit_callr(jit_state_t *_jit, jit_gpr_t f,
          size_t argc, const jit_arg_abi_t abi[], jit_arg_t args[])
{
  prepare_args(_jit, argc, abi, args);

  callr(_jit, rn(f));

  cleanup_stack_after_call(_jit, argc, abi);
}

void
jit_receive(jit_state_t *_jit,
            size_t argc, const jit_arg_abi_t abi[], jit_arg_t args[])
{
  struct abi_arg_iterator iter;
    
  reset_abi_arg_iterator(&iter, argc, abi);
  for (size_t i = 0; i < argc; i++)
    next_abi_arg(&iter, &args[i]);
}

void
jit_load_args(jit_state_t *_jit, size_t argc,
              const jit_arg_abi_t abi[], jit_arg_t args[],
              const jit_anyreg_t regs[])
{
  /* First shuffle any arguments that are already in registers into
     position.  */
  for (size_t i = 0; i < argc; i++) {
    switch (args[i].kind) {
    case JIT_ARG_LOC_IMM:
      abort();
    case JIT_ARG_LOC_GPR:
      shuffle_gpr_arg(_jit, regs[i].gpr, argc, args, i);
      break;
    case JIT_ARG_LOC_FPR:
      shuffle_fpr_arg(_jit, regs[i].fpr, argc, args, i);
      break;
    case JIT_ARG_LOC_MEM:
      break;
    default:
      abort();
    }
  }

  /* Now load spilled arguments from memory into registers.  */
  for (size_t i = 0; i < argc; i++) {
    if (args[i].kind == JIT_ARG_LOC_MEM) {
      if (is_gpr_arg(abi[i]))
        abi_mem_to_gpr(_jit, abi[i], regs[i].gpr, args[i].loc.mem.base,
                       args[i].loc.mem.offset);
      else
        abi_mem_to_fpr(_jit, abi[i], regs[i].fpr, args[i].loc.mem.base,
                       args[i].loc.mem.offset);
    }
  }
}

void
jit_flush(void *fptr, void *tptr)
{
}

static void
jit_try_shorten(jit_state_t *_jit, jit_reloc_t reloc)
{
}
