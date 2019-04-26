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

void
jit_flush(void *fptr, void *tptr)
{
}

static void
jit_try_shorten(jit_state_t *_jit, jit_reloc_t reloc)
{
}
