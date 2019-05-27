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
 *	Paulo Cesar Pereira de Andrade
 *	Andy Wingo
 */

#ifndef _jit_h
#define _jit_h

#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stddef.h>

#include "lightening/endian.h"

CHOOSE_32_64(typedef int32_t jit_word_t,
             typedef int64_t jit_word_t);
CHOOSE_32_64(typedef uint32_t jit_uword_t,
             typedef uint64_t jit_uword_t);
typedef float		jit_float32_t;
typedef double		jit_float64_t;
typedef void*		jit_pointer_t;
typedef int		jit_bool_t;

typedef void*		jit_addr_t;
typedef ptrdiff_t	jit_off_t;
typedef intptr_t	jit_imm_t;
typedef uintptr_t	jit_uimm_t;

typedef struct jit_gpr { uint8_t regno; } jit_gpr_t;
typedef struct jit_fpr { uint8_t regno; } jit_fpr_t;

// Precondition: regno between 0 and 63, inclusive.
#define JIT_GPR(regno) ((jit_gpr_t) { regno })
#define JIT_FPR(regno) ((jit_fpr_t) { regno })

static inline uint8_t jit_gpr_regno (jit_gpr_t reg) { return reg.regno; }
static inline uint8_t jit_fpr_regno (jit_fpr_t reg) { return reg.regno; }

static inline jit_bool_t
jit_same_gprs (jit_gpr_t a, jit_gpr_t b)
{
  return jit_gpr_regno (a) == jit_gpr_regno (b);
}

static inline jit_bool_t
jit_same_fprs (jit_fpr_t a, jit_fpr_t b)
{
  return jit_fpr_regno (a) == jit_fpr_regno (b);
}

#if defined(__i386__) || defined(__x86_64__)
#  include "lightening/x86.h"
#elif defined(__mips__)
#  include "lightening/mips.h"
#elif defined(__arm__)
#  include "lightening/arm.h"
#elif defined(__ppc__) || defined(__powerpc__)
#  include "lightening/ppc.h"
#elif defined(__aarch64__)
#  include "lightening/aarch64.h"
#elif defined(__s390__) || defined(__s390x__)
#  include "lightening/s390.h"
#endif

enum jit_reloc_kind
{
  JIT_RELOC_ABSOLUTE,
  JIT_RELOC_REL8,
  JIT_RELOC_REL16,
  JIT_RELOC_REL32,
  JIT_RELOC_REL64,
#ifdef JIT_NEEDS_LITERAL_POOL
  JIT_RELOC_JMP_WITH_VENEER,
  JIT_RELOC_JCC_WITH_VENEER,
  JIT_RELOC_LOAD_FROM_POOL,
#endif
};

typedef struct jit_reloc
{
  uint8_t kind;
  uint8_t inst_start_offset;
  uint8_t pc_base_offset;
  uint8_t rsh;
  uint32_t offset;
} jit_reloc_t;

#if defined(__GNUC__) && (__GNUC__ >= 4)
#  define JIT_API		extern __attribute__ ((__visibility__("hidden")))
#else
#  define JIT_API		extern
#endif

typedef struct jit_state	jit_state_t;

enum jit_operand_abi
{
  JIT_OPERAND_ABI_UINT8,
  JIT_OPERAND_ABI_INT8,
  JIT_OPERAND_ABI_UINT16,
  JIT_OPERAND_ABI_INT16,
  JIT_OPERAND_ABI_UINT32,
  JIT_OPERAND_ABI_INT32,
  JIT_OPERAND_ABI_UINT64,
  JIT_OPERAND_ABI_INT64,
  JIT_OPERAND_ABI_POINTER,
  JIT_OPERAND_ABI_FLOAT,
  JIT_OPERAND_ABI_DOUBLE,
  JIT_OPERAND_ABI_WORD = CHOOSE_32_64(JIT_OPERAND_ABI_INT32,
                                      JIT_OPERAND_ABI_INT64)
};

enum jit_operand_kind
{
  JIT_OPERAND_KIND_IMM,
  JIT_OPERAND_KIND_GPR,
  JIT_OPERAND_KIND_FPR,
  JIT_OPERAND_KIND_MEM
};

typedef struct jit_operand
{
  enum jit_operand_abi abi;
  enum jit_operand_kind kind;
  union
  {
    intptr_t imm;
    struct { jit_gpr_t gpr; ptrdiff_t addend; } gpr;
    jit_fpr_t fpr;
    struct { jit_gpr_t base; ptrdiff_t offset; ptrdiff_t addend; } mem;
  } loc;
} jit_operand_t;

static inline jit_operand_t
jit_operand_imm (enum jit_operand_abi abi, intptr_t imm)
{
  return (jit_operand_t){ abi, JIT_OPERAND_KIND_IMM, { .imm = imm } };
}

static inline jit_operand_t
jit_operand_gpr_with_addend (enum jit_operand_abi abi, jit_gpr_t gpr,
                             ptrdiff_t addend)
{
  return (jit_operand_t){ abi, JIT_OPERAND_KIND_GPR,
      { .gpr = { gpr, addend } } };
}

static inline jit_operand_t
jit_operand_gpr (enum jit_operand_abi abi, jit_gpr_t gpr)
{
  return jit_operand_gpr_with_addend (abi, gpr, 0);
}

static inline jit_operand_t
jit_operand_fpr (enum jit_operand_abi abi, jit_fpr_t fpr)
{
  return (jit_operand_t){ abi, JIT_OPERAND_KIND_FPR, { .fpr = fpr } };
}

static inline jit_operand_t
jit_operand_mem_with_addend (enum jit_operand_abi abi, jit_gpr_t base,
                             ptrdiff_t offset, ptrdiff_t addend)
{
  return (jit_operand_t){ abi, JIT_OPERAND_KIND_MEM,
      { .mem = { base, offset, addend } } };
}

static inline jit_operand_t
jit_operand_mem (enum jit_operand_abi abi, jit_gpr_t base, ptrdiff_t offset)
{
  return jit_operand_mem_with_addend (abi, base, offset, 0);
}

static inline jit_operand_t
jit_operand_addi (jit_operand_t op, ptrdiff_t addend)
{
  switch (op.kind) {
  case JIT_OPERAND_KIND_GPR:
    return jit_operand_gpr_with_addend (op.abi, op.loc.gpr.gpr,
                                        op.loc.gpr.addend + addend);
  case JIT_OPERAND_KIND_MEM:
    return jit_operand_mem_with_addend (op.abi, op.loc.mem.base,
                                        op.loc.mem.offset,
                                        op.loc.mem.addend + addend);
  default:
    abort ();
  }
}

JIT_API jit_bool_t init_jit(void);

JIT_API jit_state_t *jit_new_state(void* (*alloc_fn)(size_t),
                                   void (*free_fn)(void*));
JIT_API void jit_destroy_state(jit_state_t*);

JIT_API void jit_begin(jit_state_t*, uint8_t*, size_t);
JIT_API jit_bool_t jit_has_overflow(jit_state_t*);
JIT_API void jit_reset(jit_state_t*);
JIT_API void* jit_end(jit_state_t*, size_t*);

JIT_API void jit_align(jit_state_t*, unsigned);

JIT_API jit_pointer_t jit_address(jit_state_t*);
typedef void (*jit_function_pointer_t)();
JIT_API jit_function_pointer_t jit_address_to_function_pointer(jit_pointer_t);
JIT_API void jit_patch_here(jit_state_t*, jit_reloc_t);
JIT_API void jit_patch_there(jit_state_t*, jit_reloc_t, jit_pointer_t);

JIT_API void jit_move_operands (jit_state_t *_jit, jit_operand_t *dst,
                                jit_operand_t *src, size_t argc);

JIT_API size_t jit_align_stack (jit_state_t *_jit, size_t expand);
JIT_API void jit_shrink_stack (jit_state_t *_jit, size_t diff);

JIT_API size_t jit_enter_jit_abi (jit_state_t *_jit,
                                  size_t v, size_t vf, size_t frame_size);
JIT_API void jit_leave_jit_abi (jit_state_t *_jit,
                                size_t v, size_t vf, size_t frame_size);

/* Note that all functions that take jit_operand_t args[] use the args
   as scratch space while shuffling values into position.  */
JIT_API void jit_calli(jit_state_t *, jit_pointer_t f,
                       size_t argc, jit_operand_t args[]);
JIT_API void jit_callr(jit_state_t *, jit_gpr_t f,
                       size_t argc, jit_operand_t args[]);
JIT_API void jit_locate_args(jit_state_t*, size_t argc, jit_operand_t args[]);
JIT_API void jit_load_args(jit_state_t*, size_t argc, jit_operand_t dst[]);

static inline void
jit_calli_0(jit_state_t *_jit, jit_pointer_t f)
{
  return jit_calli(_jit, f, 0, NULL);
}

static inline void
jit_calli_1(jit_state_t *_jit, jit_pointer_t f, jit_operand_t arg)
{
  jit_operand_t args[] = { arg };
  return jit_calli(_jit, f, 1, args);
}

static inline void
jit_calli_2(jit_state_t *_jit, jit_pointer_t f, jit_operand_t a,
            jit_operand_t b)
{
  jit_operand_t args[] = { a, b };
  return jit_calli(_jit, f, 2, args);
}

static inline void
jit_calli_3(jit_state_t *_jit, jit_pointer_t f, jit_operand_t a,
            jit_operand_t b, jit_operand_t c)
{
  jit_operand_t args[] = { a, b, c };
  return jit_calli(_jit, f, 3, args);
}

static inline void
jit_callr_0(jit_state_t *_jit, jit_gpr_t f)
{
  return jit_callr(_jit, f, 0, NULL);
}

static inline void
jit_callr_1(jit_state_t *_jit, jit_gpr_t f, jit_operand_t arg)
{
  jit_operand_t args[] = { arg };
  return jit_callr(_jit, f, 1, args);
}

static inline void
jit_callr_2(jit_state_t *_jit, jit_gpr_t f, jit_operand_t a, jit_operand_t b)
{
  jit_operand_t args[] = { a, b };
  return jit_callr(_jit, f, 2, args);
}

static inline void
jit_callr_3(jit_state_t *_jit, jit_gpr_t f, jit_operand_t a, jit_operand_t b,
            jit_operand_t c)
{
  jit_operand_t args[] = { a, b, c };
  return jit_callr(_jit, f, 3, args);
}

static inline void
jit_load_args_1(jit_state_t *_jit, jit_operand_t a)
{
  jit_operand_t args[] = { a };
  return jit_load_args(_jit, 1, args);
}

static inline void
jit_load_args_2(jit_state_t *_jit, jit_operand_t a, jit_operand_t b)
{
  jit_operand_t args[] = { a, b };
  return jit_load_args(_jit, 2, args);
}

static inline void
jit_load_args_3(jit_state_t *_jit, jit_operand_t a, jit_operand_t b,
                jit_operand_t c)
{
  jit_operand_t args[] = { a, b, c };
  return jit_load_args(_jit, 3, args);
}

#define JIT_PROTO_0(stem, ret) \
  ret jit_##stem (jit_state_t* _jit)
#define JIT_PROTO_1(stem, ret, ta) \
  ret jit_##stem (jit_state_t* _jit, jit_##ta##_t a)
#define JIT_PROTO_2(stem, ret, ta, tb) \
  ret jit_##stem (jit_state_t* _jit, jit_##ta##_t a, jit_##tb##_t b)
#define JIT_PROTO_3(stem, ret, ta, tb, tc) \
  ret jit_##stem (jit_state_t* _jit, jit_##ta##_t a, jit_##tb##_t b, jit_##tc##_t c)
#define JIT_PROTO_4(stem, ret, ta, tb, tc, td) \
  ret jit_##stem (jit_state_t* _jit, jit_##ta##_t a, jit_##tb##_t b, jit_##tc##_t c, jit_##td##_t d)

#define JIT_PROTO_RFF__(stem) JIT_PROTO_2(stem, jit_reloc_t, fpr, fpr)
#define JIT_PROTO_RGG__(stem) JIT_PROTO_2(stem, jit_reloc_t, gpr, gpr)
#define JIT_PROTO_RG___(stem) JIT_PROTO_1(stem, jit_reloc_t, gpr)
#define JIT_PROTO_RGi__(stem) JIT_PROTO_2(stem, jit_reloc_t, gpr, imm)
#define JIT_PROTO_RGu__(stem) JIT_PROTO_2(stem, jit_reloc_t, gpr, uimm)
#define JIT_PROTO_R____(stem) JIT_PROTO_0(stem, jit_reloc_t)
#define JIT_PROTO__FFF_(stem) JIT_PROTO_3(stem, void, fpr, fpr, fpr)
#define JIT_PROTO__FF__(stem) JIT_PROTO_2(stem, void, fpr, fpr)
#define JIT_PROTO__FGG_(stem) JIT_PROTO_3(stem, void, fpr, gpr, gpr)
#define JIT_PROTO__FG__(stem) JIT_PROTO_2(stem, void, fpr, gpr)
#define JIT_PROTO__FGo_(stem) JIT_PROTO_3(stem, void, fpr, gpr, off)
#define JIT_PROTO__F___(stem) JIT_PROTO_1(stem, void, fpr)
#define JIT_PROTO__Fd__(stem) JIT_PROTO_2(stem, void, fpr, float64)
#define JIT_PROTO__Ff__(stem) JIT_PROTO_2(stem, void, fpr, float32)
#define JIT_PROTO__Fp__(stem) JIT_PROTO_2(stem, void, fpr, pointer)
#define JIT_PROTO__GF__(stem) JIT_PROTO_2(stem, void, gpr, fpr)
#define JIT_PROTO__GGF_(stem) JIT_PROTO_3(stem, void, gpr, gpr, fpr)
#define JIT_PROTO__GGGG(stem) JIT_PROTO_4(stem, void, gpr, gpr, gpr, gpr)
#define JIT_PROTO__GGG_(stem) JIT_PROTO_3(stem, void, gpr, gpr, gpr)
#define JIT_PROTO__GGGi(stem) JIT_PROTO_4(stem, void, gpr, gpr, gpr, imm)
#define JIT_PROTO__GGGu(stem) JIT_PROTO_4(stem, void, gpr, gpr, gpr, uimm)
#define JIT_PROTO__GG__(stem) JIT_PROTO_2(stem, void, gpr, gpr)
#define JIT_PROTO__GGi_(stem) JIT_PROTO_3(stem, void, gpr, gpr, imm)
#define JIT_PROTO__GGo_(stem) JIT_PROTO_3(stem, void, gpr, gpr, off)
#define JIT_PROTO__GGu_(stem) JIT_PROTO_3(stem, void, gpr, gpr, uimm)
#define JIT_PROTO__G___(stem) JIT_PROTO_1(stem, void, gpr)
#define JIT_PROTO__Gi__(stem) JIT_PROTO_2(stem, void, gpr, imm)
#define JIT_PROTO__Gp__(stem) JIT_PROTO_2(stem, void, gpr, pointer)
#define JIT_PROTO______(stem) JIT_PROTO_0(stem, void)
#define JIT_PROTO__i___(stem) JIT_PROTO_1(stem, void, imm)
#define JIT_PROTO__oGF_(stem) JIT_PROTO_3(stem, void, off, gpr, fpr)
#define JIT_PROTO__oGG_(stem) JIT_PROTO_3(stem, void, off, gpr, gpr)
#define JIT_PROTO__pF__(stem) JIT_PROTO_2(stem, void, pointer, fpr)
#define JIT_PROTO__pG__(stem) JIT_PROTO_2(stem, void, pointer, gpr)
#define JIT_PROTO__p___(stem) JIT_PROTO_1(stem, void, pointer)

#define FOR_EACH_INSTRUCTION(M)		\
          M(_GGG_, addr)		\
          M(_FFF_, addr_f)		\
          M(_FFF_, addr_d)		\
          M(_GGi_, addi)		\
          M(_GGG_, addcr)		\
          M(_GGi_, addci)		\
          M(_GGG_, addxr)		\
          M(_GGi_, addxi)		\
          M(_GGG_, subr)		\
          M(_FFF_, subr_f)		\
          M(_FFF_, subr_d)		\
          M(_GGi_, subi)		\
          M(_GGG_, subcr)		\
          M(_GGi_, subci)		\
          M(_GGG_, subxr)		\
          M(_GGi_, subxi)		\
          M(_GGG_, mulr)		\
          M(_FFF_, mulr_f)		\
          M(_FFF_, mulr_d)		\
          M(_GGi_, muli)		\
          M(_GGGG, qmulr)		\
          M(_GGGi, qmuli)		\
          M(_GGGG, qmulr_u)		\
          M(_GGGu, qmuli_u)		\
          M(_GGG_, divr)		\
          M(_FFF_, divr_f)		\
          M(_FFF_, divr_d)		\
          M(_GGi_, divi)		\
          M(_GGG_, divr_u)		\
          M(_GGu_, divi_u)		\
          M(_GGGG, qdivr)		\
          M(_GGGi, qdivi)		\
          M(_GGGG, qdivr_u)		\
          M(_GGGu, qdivi_u)		\
          M(_GGG_, remr)		\
          M(_GGi_, remi)		\
          M(_GGG_, remr_u)		\
          M(_GGu_, remi_u)		\
  					\
          M(_GGG_, andr)		\
          M(_GGu_, andi)		\
          M(_GGG_, orr)			\
          M(_GGu_, ori)			\
          M(_GGG_, xorr)		\
          M(_GGu_, xori)		\
  					\
          M(_GGG_, lshr)		\
          M(_GGu_, lshi)		\
          M(_GGG_, rshr)		\
          M(_GGu_, rshi)		\
          M(_GGG_, rshr_u)		\
          M(_GGu_, rshi_u)		\
  					\
          M(_GG__, negr)		\
          M(_GG__, comr)		\
  					\
          M(_GG__, movr)		\
          M(_Gi__, movi)		\
          M(RG___, mov_addr)		\
          M(_GG__, extr_c)		\
          M(_GG__, extr_uc)		\
          M(_GG__, extr_s)		\
          M(_GG__, extr_us)		\
  WHEN_64(M(_GG__, extr_i))		\
  WHEN_64(M(_GG__, extr_ui))		\
  					\
          M(_GG__, bswapr_us)		\
          M(_GG__, bswapr_ui)		\
  WHEN_64(M(_GG__, bswapr_ul))		\
  					\
          M(_GG__, ldr_c)		\
          M(_Gp__, ldi_c)		\
          M(_GG__, ldr_uc)		\
          M(_Gp__, ldi_uc)		\
          M(_GG__, ldr_s)		\
          M(_Gp__, ldi_s)		\
          M(_GG__, ldr_us)		\
          M(_Gp__, ldi_us)		\
          M(_GG__, ldr_i)		\
          M(_Gp__, ldi_i)		\
  WHEN_64(M(_GG__, ldr_ui))		\
  WHEN_64(M(_Gp__, ldi_ui))		\
  WHEN_64(M(_GG__, ldr_l))		\
  WHEN_64(M(_Gp__, ldi_l))		\
          M(_FG__, ldr_f)		\
          M(_Fp__, ldi_f)		\
          M(_FG__, ldr_d)		\
          M(_Fp__, ldi_d)		\
  					\
          M(_GGG_, ldxr_c)		\
          M(_GGo_, ldxi_c)		\
          M(_GGG_, ldxr_uc)		\
          M(_GGo_, ldxi_uc)		\
          M(_GGG_, ldxr_s)		\
          M(_GGo_, ldxi_s)		\
          M(_GGG_, ldxr_us)		\
          M(_GGo_, ldxi_us)		\
          M(_GGG_, ldxr_i)		\
          M(_GGo_, ldxi_i)		\
  WHEN_64(M(_GGG_, ldxr_ui))		\
  WHEN_64(M(_GGo_, ldxi_ui))		\
  WHEN_64(M(_GGG_, ldxr_l))		\
  WHEN_64(M(_GGo_, ldxi_l))		\
          M(_FGG_, ldxr_f)		\
          M(_FGo_, ldxi_f)		\
          M(_FGG_, ldxr_d)		\
          M(_FGo_, ldxi_d)		\
					\
          M(_GG__, ldr_atomic)		\
          M(_GG__, str_atomic)		\
          M(_GGG_, swap_atomic)		\
          M(_GGGG, cas_atomic)		\
  					\
          M(_GG__, str_c)		\
          M(_pG__, sti_c)		\
          M(_GG__, str_s)		\
          M(_pG__, sti_s)		\
          M(_GG__, str_i)		\
          M(_pG__, sti_i)		\
  WHEN_64(M(_GG__, str_l))		\
  WHEN_64(M(_pG__, sti_l))		\
          M(_GF__, str_f)		\
          M(_pF__, sti_f)		\
          M(_GF__, str_d)		\
          M(_pF__, sti_d)		\
  					\
          M(_GGG_, stxr_c)		\
          M(_oGG_, stxi_c)		\
          M(_GGG_, stxr_s)		\
          M(_oGG_, stxi_s)		\
          M(_GGG_, stxr_i)		\
          M(_oGG_, stxi_i)		\
  WHEN_64(M(_GGG_, stxr_l))		\
  WHEN_64(M(_oGG_, stxi_l))		\
          M(_GGF_, stxr_f)		\
          M(_oGF_, stxi_f)		\
          M(_GGF_, stxr_d)		\
          M(_oGF_, stxi_d)		\
  					\
          M(RGG__, bltr)		\
          M(RFF__, bltr_f)		\
          M(RFF__, bltr_d)		\
          M(RGi__, blti)		\
          M(RGG__, bltr_u)		\
          M(RGu__, blti_u)		\
          M(RGG__, bler)		\
          M(RFF__, bler_f)		\
          M(RFF__, bler_d)		\
          M(RGi__, blei)		\
          M(RGG__, bler_u)		\
          M(RGu__, blei_u)		\
          M(RGG__, beqr)		\
          M(RFF__, beqr_f)		\
          M(RFF__, beqr_d)		\
          M(RGi__, beqi)		\
          M(RGG__, bger)		\
          M(RFF__, bger_f)		\
          M(RFF__, bger_d)		\
          M(RGi__, bgei)		\
          M(RGG__, bger_u)		\
          M(RGu__, bgei_u)		\
          M(RGG__, bgtr)		\
          M(RFF__, bgtr_f)		\
          M(RFF__, bgtr_d)		\
          M(RGi__, bgti)		\
          M(RGG__, bgtr_u)		\
          M(RGu__, bgti_u)		\
          M(RGG__, bner)		\
          M(RFF__, bner_f)		\
          M(RFF__, bner_d)		\
          M(RGi__, bnei)		\
  					\
          M(RFF__, bunltr_f)		\
          M(RFF__, bunltr_d)		\
          M(RFF__, bunler_f)		\
          M(RFF__, bunler_d)		\
          M(RFF__, buneqr_f)		\
          M(RFF__, buneqr_d)		\
          M(RFF__, bunger_f)		\
          M(RFF__, bunger_d)		\
          M(RFF__, bungtr_f)		\
          M(RFF__, bungtr_d)		\
          M(RFF__, bltgtr_f)		\
          M(RFF__, bltgtr_d)		\
          M(RFF__, bordr_f)		\
          M(RFF__, bordr_d)		\
          M(RFF__, bunordr_f)		\
          M(RFF__, bunordr_d)		\
  					\
          M(RGG__, bmsr)		\
          M(RGu__, bmsi)		\
          M(RGG__, bmcr)		\
          M(RGu__, bmci)		\
  					\
          M(RGG__, boaddr)		\
          M(RGi__, boaddi)		\
          M(RGG__, boaddr_u)		\
          M(RGu__, boaddi_u)		\
          M(RGG__, bxaddr)		\
          M(RGi__, bxaddi)		\
          M(RGG__, bxaddr_u)		\
          M(RGu__, bxaddi_u)		\
          M(RGG__, bosubr)		\
          M(RGi__, bosubi)		\
          M(RGG__, bosubr_u)		\
          M(RGu__, bosubi_u)		\
          M(RGG__, bxsubr)		\
          M(RGi__, bxsubi)		\
          M(RGG__, bxsubr_u)		\
          M(RGu__, bxsubi_u)		\
  					\
          M(_G___, jmpr)		\
          M(_p___, jmpi)		\
          M(R____, jmp)			\
  					\
          M(_____, ret)			\
          M(_G___, retr)		\
          M(_F___, retr_f)		\
          M(_F___, retr_d)		\
          M(_i___, reti)		\
          M(_G___, retval_c)		\
          M(_G___, retval_uc)		\
          M(_G___, retval_s)		\
          M(_G___, retval_us)		\
          M(_G___, retval_i)		\
  WHEN_64(M(_G___, retval_ui))		\
  WHEN_64(M(_G___, retval_l))		\
          M(_F___, retval_f)		\
          M(_F___, retval_d)		\
  					\
          M(_FF__, negr_f)		\
          M(_FF__, negr_d)		\
          M(_FF__, absr_f)		\
          M(_FF__, absr_d)		\
          M(_FF__, sqrtr_f)		\
          M(_FF__, sqrtr_d)		\
  					\
          M(_GF__, truncr_f_i)		\
          M(_FG__, extr_f)		\
          M(_FG__, extr_d)		\
          M(_FF__, extr_d_f)		\
          M(_FF__, extr_f_d)		\
          M(_FF__, movr_f)		\
          M(_FF__, movr_d)		\
          M(_Ff__, movi_f)		\
          M(_Fd__, movi_d)		\
          M(_GF__, truncr_d_i)		\
  WHEN_64(M(_GF__, truncr_f_l))		\
  WHEN_64(M(_GF__, truncr_d_l))		\
          /* EOL */

#define DECLARE_INSTRUCTION(kind, stem) JIT_API JIT_PROTO_##kind(stem);
FOR_EACH_INSTRUCTION(DECLARE_INSTRUCTION)
#undef DECLARE_INSTRUCTION

#if __WORDSIZE == 32
#  define jit_ldr(j,u,v)	jit_ldr_i(j,u,v)
#  define jit_ldi(j,u,v)	jit_ldi_i(j,u,v)
#  define jit_ldxr(j,u,v,w)	jit_ldxr_i(j,u,v,w)
#  define jit_ldxi(j,u,v,w)	jit_ldxi_i(j,u,v,w)
#  define jit_str(j,u,v)	jit_str_i(j,u,v)
#  define jit_sti(j,u,v)	jit_sti_i(j,u,v)
#  define jit_stxr(j,u,v,w)	jit_stxr_i(j,u,v,w)
#  define jit_stxi(j,u,v,w)	jit_stxi_i(j,u,v,w)
#  define jit_retval(j,u)	jit_retval_i(j,u)
#  define jit_bswapr(j,u,v)	jit_bswapr_ui(j,u,v)
#  define jit_truncr_d(j,u,v)	jit_truncr_d_i(j,u,v)
#  define jit_truncr_f(j,u,v)	jit_truncr_f_i(j,u,v)
#else
#  define jit_ldr(j,u,v)	jit_ldr_l(j,u,v)
#  define jit_ldi(j,u,v)	jit_ldi_l(j,u,v)
#  define jit_ldxr(j,u,v,w)	jit_ldxr_l(j,u,v,w)
#  define jit_ldxi(j,u,v,w)	jit_ldxi_l(j,u,v,w)
#  define jit_str(j,u,v)	jit_str_l(j,u,v)
#  define jit_sti(j,u,v)	jit_sti_l(j,u,v)
#  define jit_stxr(j,u,v,w)	jit_stxr_l(j,u,v,w)
#  define jit_stxi(j,u,v,w)	jit_stxi_l(j,u,v,w)
#  define jit_retval(j,u)	jit_retval_l(j,u)
#  define jit_bswapr(j,u,v)	jit_bswapr_ul(j,u,v)
#  define jit_truncr_d(j,u,v)	jit_truncr_d_l(j,u,v)
#  define jit_truncr_f(j,u,v)	jit_truncr_f_l(j,u,v)
#endif

#endif /* _jit_h */
