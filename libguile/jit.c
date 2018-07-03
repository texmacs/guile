/* Copyright 2018
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#if ENABLE_JIT
#include <lightning.h>
#endif

#include "threads.h"
#include "vm-operations.h"

#include "jit.h"

typedef struct {
  scm_thread *thread;
  const uint32_t *start;
  const uint32_t *end;
} scm_jit_state;

static void
bad_instruction(scm_jit_state *state, const uint32_t *ip)
{
  abort ();
}

static void
compile_halt (scm_jit_state *state, const uint32_t *ip)
{
  bad_instruction (state, ip);
}

static void
compile_call (scm_jit_state *state, const uint32_t *ip, uint32_t a, uint32_t b)
{
}

static void
compile_call_label (scm_jit_state *state, const uint32_t *ip, uint32_t a, uint32_t b, int32_t offset)
{
}

static void
compile_tail_call (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_tail_call_label (scm_jit_state *state, const uint32_t *ip, uint32_t a, int32_t offset)
{
}

static void
compile_tail_call_shuffle (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_receive (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a, uint32_t b)
{
}

static void
compile_receive_values (scm_jit_state *state, const uint32_t *ip, uint32_t a, uint8_t b, uint32_t c)
{
}

static void
compile_return_values (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_subr_call (scm_jit_state *state, const uint32_t *ip)
{
}

static void
compile_foreign_call (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_continuation_call (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_compose_continuation (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_tail_apply (scm_jit_state *state, const uint32_t *ip)
{
}

static void
compile_call_cc (scm_jit_state *state, const uint32_t *ip)
{
}

static void
compile_abort (scm_jit_state *state, const uint32_t *ip)
{
}

static void
compile_builtin_ref (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a)
{
}

static void
compile_throw (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_throw_value (scm_jit_state *state, const uint32_t *ip, uint32_t a, int32_t offset)
{
}

static void
compile_throw_value_and_data (scm_jit_state *state, const uint32_t *ip, uint32_t a, int32_t offset)
{
}

static void
compile_assert_nargs_ee (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_assert_nargs_ge (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_assert_nargs_le (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_alloc_frame (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_reset_frame (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_push (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_pop (scm_jit_state *state, const uint32_t *ip, uint32_t dst)
{
}

static void
compile_drop (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_assert_nargs_ee_locals (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_bind_kwargs (scm_jit_state *state, const uint32_t *ip, uint32_t a, uint8_t b, uint32_t c, uint32_t d, int32_t offset)
{
}

static void
compile_bind_rest (scm_jit_state *state, const uint32_t *ip, uint32_t dst)
{
}

static void
compile_allocate_words (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a)
{
}

static void
compile_allocate_words_immediate (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a)
{
}

static void
compile_scm_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_scm_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_scm_ref_tag (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_scm_set_tag (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_scm_ref_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_scm_set_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_word_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_word_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_word_ref_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_word_set_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_pointer_ref_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_pointer_set_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_tail_pointer_ref_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_mov (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a)
{
}

static void
compile_long_mov (scm_jit_state *state, const uint32_t *ip, uint32_t dst, uint32_t a)
{
}

static void
compile_long_fmov (scm_jit_state *state, const uint32_t *ip, uint32_t dst, uint32_t a)
{
}

static void
compile_call_scm_from_scm_scm (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b, uint32_t c)
{
}

static void
compile_call_scm_from_scm_uimm (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b, uint32_t c)
{
}

static void
compile_call_scm_u64_u64 (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c, uint32_t d)
{
}

static void
compile_call_scm_from_scm (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a, uint32_t b)
{
}

static void
compile_call_f64_from_scm (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a, uint32_t b)
{
}

static void
compile_call_u64_from_scm (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a, uint32_t b)
{
}

static void
compile_make_short_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, SCM a)
{
}

static void
compile_make_long_immediate (scm_jit_state *state, const uint32_t *ip, uint32_t dst, SCM a)
{
}

static void
compile_make_long_long_immediate (scm_jit_state *state, const uint32_t *ip, uint32_t dst, SCM a)
{
}

static void
compile_make_non_immediate (scm_jit_state *state, const uint32_t *ip, uint32_t dst, int32_t offset)
{
}

static void
compile_static_ref (scm_jit_state *state, const uint32_t *ip, uint32_t dst, int32_t offset)
{
}

static void
compile_static_set (scm_jit_state *state, const uint32_t *ip, uint32_t a, int32_t offset)
{
}

static void
compile_static_patch (scm_jit_state *state, const uint32_t *ip, int32_t a, int32_t b)
{
}

static void
compile_prompt (scm_jit_state *state, const uint32_t *ip, uint32_t a, uint8_t b, uint32_t c, int32_t offset)
{
}

static void
compile_load_label (scm_jit_state *state, const uint32_t *ip, uint32_t dst, int32_t offset)
{
}

static void
compile_call_s64_from_scm (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a, uint32_t b)
{
}

static void
compile_call_scm_from_u64 (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a, uint32_t b)
{
}

static void
compile_call_scm_from_s64 (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a, uint32_t b)
{
}

static void
compile_tag_char (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a)
{
}

static void
compile_untag_char (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a)
{
}

static void
compile_atomic_ref_scm_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_atomic_set_scm_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_atomic_scm_swap_immediate (scm_jit_state *state, const uint32_t *ip, uint32_t dst, uint32_t a, uint8_t b, uint32_t c)
{
}

static void
compile_atomic_scm_compare_and_swap_immediate (scm_jit_state *state, const uint32_t *ip, uint32_t dst, uint32_t a, uint8_t b, uint32_t c, uint32_t d)
{
}

static void
compile_call_thread_scm_scm (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b, uint32_t c)
{
}

static void
compile_call_thread (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_call_scm_from_thread_scm (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a, uint32_t b)
{
}

static void
compile_call_thread_scm (scm_jit_state *state, const uint32_t *ip, uint32_t a, uint32_t b)
{
}

static void
compile_call_scm_from_scm_u64 (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b, uint32_t c)
{
}

static void
compile_call_scm_from_thread (scm_jit_state *state, const uint32_t *ip, uint32_t dst, uint32_t a)
{
}

static void
compile_fadd (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_fsub (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_fmul (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_fdiv (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_uadd (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_usub (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_umul (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_uadd_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_usub_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_umul_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_load_f64 (scm_jit_state *state, const uint32_t *ip, uint32_t dst, double a)
{
}

static void
compile_load_u64 (scm_jit_state *state, const uint32_t *ip, uint32_t dst, uint64_t a)
{
}

static void
compile_load_s64 (scm_jit_state *state, const uint32_t *ip, uint32_t dst, int64_t a)
{
}

static void
compile_current_thread (scm_jit_state *state, const uint32_t *ip, uint32_t dst)
{
}

static void
compile_ulogand (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_ulogior (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_ulogsub (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_ursh (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_ulsh (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_ursh_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_ulsh_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_ulogxor (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_handle_interrupts (scm_jit_state *state, const uint32_t *ip)
{
}

static void
compile_return_from_interrupt (scm_jit_state *state, const uint32_t *ip)
{
}

static void
compile_u64_numerically_equal (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_u64_less (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_s64_numerically_equal (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_s64_less (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_f64_numerically_equal (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_f64_less (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_numerically_equal (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_less (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_check_arguments (scm_jit_state *state, const uint32_t *ip, uint32_t a)
{
}

static void
compile_check_positional_arguments (scm_jit_state *state, const uint32_t *ip, uint32_t a, uint32_t b)
{
}

static void
compile_immediate_tag_equals (scm_jit_state *state, const uint32_t *ip, uint32_t a, uint16_t b, uint16_t c)
{
}

static void
compile_heap_tag_equals (scm_jit_state *state, const uint32_t *ip, uint32_t a, uint16_t b, uint16_t c)
{
}

static void
compile_eq (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_j (scm_jit_state *state, const uint32_t *ip, int32_t offset)
{
}

static void
compile_jl (scm_jit_state *state, const uint32_t *ip, int32_t offset)
{
}

static void
compile_je (scm_jit_state *state, const uint32_t *ip, int32_t offset)
{
}

static void
compile_jnl (scm_jit_state *state, const uint32_t *ip, int32_t offset)
{
}

static void
compile_jne (scm_jit_state *state, const uint32_t *ip, int32_t offset)
{
}

static void
compile_jge (scm_jit_state *state, const uint32_t *ip, int32_t offset)
{
}

static void
compile_jnge (scm_jit_state *state, const uint32_t *ip, int32_t offset)
{
}

static void
compile_heap_numbers_equal (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_untag_fixnum (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a)
{
}

static void
compile_tag_fixnum (scm_jit_state *state, const uint32_t *ip, uint16_t dst, uint16_t a)
{
}

static void
compile_srsh (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_srsh_immediate (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_s64_imm_numerically_equal (scm_jit_state *state, const uint32_t *ip, uint16_t a, int16_t b)
{
}

static void
compile_u64_imm_less (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_imm_u64_less (scm_jit_state *state, const uint32_t *ip, uint16_t a, uint16_t b)
{
}

static void
compile_s64_imm_less (scm_jit_state *state, const uint32_t *ip, uint16_t a, int16_t b)
{
}

static void
compile_imm_s64_less (scm_jit_state *state, const uint32_t *ip, uint16_t a, int16_t b)
{
}

static void
compile_u8_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_u16_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_u32_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_u64_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_u8_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_u16_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_u32_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_u64_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_s8_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_s16_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_s32_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_s64_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_s8_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_s16_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_s32_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_s64_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_f32_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_f64_ref (scm_jit_state *state, const uint32_t *ip, uint8_t dst, uint8_t a, uint8_t b)
{
}

static void
compile_f32_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}

static void
compile_f64_set (scm_jit_state *state, const uint32_t *ip, uint8_t a, uint8_t b, uint8_t c)
{
}


#define UNPACK_8_8_8(op,a,b,c)            \
  do                                      \
    {                                     \
      a = (op >> 8) & 0xff;               \
      b = (op >> 16) & 0xff;              \
      c = op >> 24;                       \
    }                                     \
  while (0)

#define UNPACK_8_16(op,a,b)               \
  do                                      \
    {                                     \
      a = (op >> 8) & 0xff;               \
      b = op >> 16;                       \
    }                                     \
  while (0)

#define UNPACK_12_12(op,a,b)              \
  do                                      \
    {                                     \
      a = (op >> 8) & 0xfff;              \
      b = op >> 20;                       \
    }                                     \
  while (0)

#define UNPACK_24(op,a)                   \
  do                                      \
    {                                     \
      a = op >> 8;                        \
    }                                     \
  while (0)

#define UNPACK_8_24(op,a,b)               \
  do                                      \
    {                                     \
      a = op & 0xff;                      \
      b = op >> 8;                        \
    }                                     \
  while (0)

#define UNPACK_16_16(op,a,b)              \
  do                                      \
    {                                     \
      a = op & 0xffff;                    \
      b = op >> 16;                       \
    }                                     \
  while (0)

#define COMPILE_OP1(t0) \
  COMPILE_##t0
#define COMPILE_OP2(t0, t1) \
  COMPILE_##t0##__##t1
#define COMPILE_OP3(t0, t1, t2) \
  COMPILE_##t0##__##t1##__##t2
#define COMPILE_OP4(t0, t1, t2, t3) \
  COMPILE_##t0##__##t1##__##t2##__##t3
#define COMPILE_OP5(t0, t1, t2, t3, t4) \
  COMPILE_##t0##__##t1##__##t2##__##t3##__##t4

#define COMPILE_DOP1(t0)                 COMPILE_OP1(t0)
#define COMPILE_DOP2(t0, t1)             COMPILE_OP2(t0, t1)
#define COMPILE_DOP3(t0, t1, t2)         COMPILE_OP3(t0, t1, t2)
#define COMPILE_DOP4(t0, t1, t2, t3)     COMPILE_OP4(t0, t1, t2, t3)
#define COMPILE_DOP5(t0, t1, t2, t3, t4) COMPILE_OP5(t0, t1, t2, t3, t4)

#define COMPILE_NOP(state, ip, comp) \
  {                                   \
    bad_instruction(state, ip);       \
    return ip;                        \
  }

#define COMPILE_X32(state, ip, comp)                                    \
  {                                                                     \
    comp (state, ip);                                                   \
    return ip + 1;                                                      \
  }

#define COMPILE_X8_C24(state, ip, comp)                                 \
  {                                                                     \
    uint32_t a;                                                         \
    UNPACK_24 (ip[0], a);                                               \
    comp (state, ip, a);                                                \
    return ip + 1;                                                      \
  }
#define COMPILE_X8_F24(state, ip, comp)                                 \
  COMPILE_X8_C24 (state, ip, comp)
#define COMPILE_X8_S24(state, ip, comp)                                 \
  COMPILE_X8_C24 (state, ip, comp)

#define COMPILE_X8_L24(state, ip, comp)                                 \
  {                                                                     \
    int32_t a = ip[0];                                                  \
    a >>= 8; /* Sign extension.  */                                     \
    comp (state, ip, a);                                                \
    return ip + 1;                                                      \
  }
#define COMPILE_X8_C12_C12(state, ip, comp)                             \
  {                                                                     \
    uint16_t a, b;                                                      \
    UNPACK_12_12 (ip[0], a, b);                                         \
    comp (state, ip, a, b);                                             \
    return ip + 1;                                                      \
  }
#define COMPILE_X8_S12_C12(state, ip, comp)                             \
  COMPILE_X8_C12_C12 (state, ip, comp)
#define COMPILE_X8_S12_S12(state, ip, comp)                             \
  COMPILE_X8_C12_C12 (state, ip, comp)

#define COMPILE_X8_S12_Z12(state, ip, comp)                             \
  {                                                                     \
    uint16_t a = (ip[0] >> 8) & 0xfff;                                  \
    int16_t b = ((int32_t) ip[0]) >> 20; /* Sign extension.  */         \
    comp (state, ip, a, b);                                             \
    return ip + 1;                                                      \
  }

#define COMPILE_X8_S8_C8_S8(state, ip, comp)                            \
  {                                                                     \
    uint8_t a, b, c;                                                    \
    UNPACK_8_8_8 (ip[0], a, b, c);                                      \
    comp (state, ip, a, b, c);                                          \
    return ip + 1;                                                      \
  }
#define COMPILE_X8_S8_S8_C8(state, ip, comp)                            \
  COMPILE_X8_S8_C8_S8 (state, ip, comp)
#define COMPILE_X8_S8_S8_S8(state, ip, comp)                            \
  COMPILE_X8_S8_C8_S8 (state, ip, comp)

#define COMPILE_X8_S8_I16(state, ip, comp)                              \
  {                                                                     \
    uint8_t a;                                                          \
    scm_t_bits b;                                                       \
    UNPACK_8_16 (ip[0], a, b);                                          \
    comp (state, ip, a, SCM_PACK (b));                                  \
    return ip + 1;                                                      \
  }

#define COMPILE_X32__C32(state, ip, comp)                               \
  {                                                                     \
    comp (state, ip, ip[1]);                                            \
    return ip + 2;                                                      \
  }

#define COMPILE_X8_C24__L32(state, ip, comp)                            \
  {                                                                     \
    uint32_t a;                                                         \
    int32_t b;                                                          \
    UNPACK_24 (ip[0], a);                                               \
    b = ip[1];                                                          \
    comp (state, ip, a, b);                                             \
    return ip + 2;                                                      \
  }
#define COMPILE_X8_S24__L32(state, ip, comp)                            \
  COMPILE_X8_C24__L32 (state, ip, comp)
#define COMPILE_X8_S24__LO32(state, ip, comp)                           \
  COMPILE_X8_C24__L32 (state, ip, comp)
#define COMPILE_X8_S24__N32(state, ip, comp)                            \
  COMPILE_X8_C24__L32 (state, ip, comp)
#define COMPILE_X8_S24__R32(state, ip, comp)                            \
  COMPILE_X8_C24__L32 (state, ip, comp)

#define COMPILE_X8_C24__X8_C24(state, ip, comp)                         \
  {                                                                     \
    uint32_t a, b;                                                      \
    UNPACK_24 (ip[0], a);                                               \
    UNPACK_24 (ip[1], b);                                               \
    comp (state, ip, a, b);                                             \
    return ip + 2;                                                      \
  }
#define COMPILE_X8_F24__X8_C24(state, ip, comp)                         \
  COMPILE_X8_C24__X8_C24(state, ip, comp)
#define COMPILE_X8_F24__X8_F24(state, ip, comp)                         \
  COMPILE_X8_C24__X8_C24(state, ip, comp)
#define COMPILE_X8_S24__X8_S24(state, ip, comp)                         \
  COMPILE_X8_C24__X8_C24(state, ip, comp)

#define COMPILE_X8_F12_F12__X8_C24(state, ip, comp)                     \
  {                                                                     \
    uint16_t a, b;                                                      \
    uint32_t c;                                                         \
    UNPACK_12_12 (ip[0], a, b);                                         \
    UNPACK_24 (ip[1], c);                                               \
    comp (state, ip, a, b, c);                                          \
    return ip + 2;                                                      \
  }

#define COMPILE_X8_F24__B1_X7_C24(state, ip, comp)                      \
  {                                                                     \
    uint32_t a, c;                                                      \
    uint8_t b;                                                          \
    UNPACK_24 (ip[0], a);                                               \
    b = ip[1] & 0x1;                                                    \
    UNPACK_24 (ip[1], c);                                               \
    comp (state, ip, a, b, c);                                          \
    return ip + 2;                                                      \
  }

#define COMPILE_X8_S12_S12__C32(state, ip, comp)                        \
  {                                                                     \
    uint16_t a, b;                                                      \
    uint32_t c;                                                         \
    UNPACK_12_12 (ip[0], a, b);                                         \
    c = ip[1];                                                          \
    comp (state, ip, a, b, c);                                          \
    return ip + 2;                                                      \
  }

#define COMPILE_X8_S24__C16_C16(state, ip, comp)                        \
  {                                                                     \
    uint32_t a;                                                         \
    uint16_t b, c;                                                      \
    UNPACK_24 (ip[0], a);                                               \
    UNPACK_16_16 (ip[1], b, c);                                         \
    comp (state, ip, a, b, c);                                          \
    return ip + 2;                                                      \
  }

#define COMPILE_X8_S24__C32(state, ip, comp)                            \
  {                                                                     \
    uint32_t a, b;                                                      \
    UNPACK_24 (ip[0], a);                                               \
    b = ip[1];                                                          \
    comp (state, ip, a, b);                                             \
    return ip + 2;                                                      \
  }

#define COMPILE_X8_S24__I32(state, ip, comp)                            \
  {                                                                     \
    uint32_t a;                                                         \
    scm_t_bits b;                                                       \
    UNPACK_24 (ip[0], a);                                               \
    b = ip[1];                                                          \
    comp (state, ip, a, SCM_PACK (b));                                  \
    return ip + 2;                                                      \
  }

#define COMPILE_X8_S8_S8_C8__C32(state, ip, comp)                       \
  {                                                                     \
    uint8_t a, b, c;                                                    \
    uint32_t d;                                                         \
    UNPACK_8_8_8 (ip[0], a, b, c);                                      \
    d = ip[1];                                                          \
    comp (state, ip, a, b, c, d);                                       \
    return ip + 2;                                                      \
  }
#define COMPILE_X8_S8_S8_S8__C32(state, ip, comp)                       \
  COMPILE_X8_S8_S8_C8__C32(state, ip, comp)

#define COMPILE_X32__LO32__L32(state, ip, comp)                         \
  {                                                                     \
    int32_t a = ip[1], b = ip[2];                                       \
    comp (state, ip, a, b);                                             \
    return ip + 3;                                                      \
  }

#define COMPILE_X8_F24__X8_C24__L32(state, ip, comp)                    \
  {                                                                     \
    uint32_t a, b;                                                      \
    int32_t c;                                                          \
    UNPACK_24 (ip[0], a);                                               \
    UNPACK_24 (ip[1], b);                                               \
    c = ip[2];                                                          \
    comp (state, ip, a, b, c);                                          \
    return ip + 3;                                                      \
  }

#define COMPILE_X8_S24__A32__B32(state, ip, comp)                       \
  {                                                                     \
    uint32_t a;                                                         \
    uint64_t b;                                                         \
    UNPACK_24 (ip[0], a);                                               \
    b = (((uint64_t) ip[1]) << 32) | ((uint64_t) ip[2]);                \
    if (b > (uint64_t) UINTPTR_MAX) abort ();                           \
    comp (state, ip, a, SCM_PACK ((uintptr_t) b));                      \
    return ip + 3;                                                      \
  }

#define COMPILE_X8_S24__AF32__BF32(state, ip, comp)                     \
  {                                                                     \
    uint32_t a;                                                         \
    union { uint64_t u; double d; } b;                                  \
    UNPACK_24 (ip[0], a);                                               \
    b.u = (((uint64_t) ip[1]) << 32) | ((uint64_t) ip[2]);              \
    comp (state, ip, a, b.d);                                           \
    return ip + 3;                                                      \
  }

#define COMPILE_X8_S24__AS32__BS32(state, ip, comp)                     \
  {                                                                     \
    uint32_t a;                                                         \
    uint64_t b;                                                         \
    UNPACK_24 (ip[0], a);                                               \
    b = (((uint64_t) ip[1]) << 32) | ((uint64_t) ip[2]);                \
    comp (state, ip, a, (int64_t) b);                                   \
    return ip + 3;                                                      \
  }

#define COMPILE_X8_S24__AU32__BU32(state, ip, comp)                     \
  {                                                                     \
    uint32_t a;                                                         \
    uint64_t b;                                                         \
    UNPACK_24 (ip[0], a);                                               \
    b = (((uint64_t) ip[1]) << 32) | ((uint64_t) ip[2]);                \
    comp (state, ip, a, b);                                             \
    return ip + 3;                                                      \
  }

#define COMPILE_X8_S24__B1_X7_F24__X8_L24(state, ip, comp)              \
  {                                                                     \
    uint32_t a, c;                                                      \
    uint8_t b;                                                          \
    int32_t d;                                                          \
    UNPACK_24 (ip[0], a);                                               \
    b = ip[1] & 0x1;                                                    \
    UNPACK_24 (ip[1], c);                                               \
    d = ip[2]; d >>= 8; /* Sign extension.  */                          \
    comp (state, ip, a, b, c, d);                                       \
    return ip + 3;                                                      \
  }

#define COMPILE_X8_S24__X8_S24__C8_S24(state, ip, comp)                 \
  {                                                                     \
    uint32_t a, b, d;                                                   \
    uint8_t c;                                                          \
    UNPACK_24 (ip[0], a);                                               \
    UNPACK_24 (ip[1], b);                                               \
    UNPACK_8_24 (ip[2], c, d);                                          \
    comp (state, ip, a, b, c, d);                                       \
    return ip + 3;                                                      \
  }

#define COMPILE_X8_C24__C8_C24__X8_C24__N32(state, ip, comp)            \
  {                                                                     \
    uint32_t a, c, d;                                                   \
    uint8_t b;                                                          \
    int32_t e;                                                          \
    UNPACK_24 (ip[0], a);                                               \
    UNPACK_8_24 (ip[1], b, c);                                          \
    UNPACK_24 (ip[2], d);                                               \
    e = ip[3]; e >>= 8; /* Sign extension.  */                          \
    comp (state, ip, a, b, c, d, e);                                    \
    return ip + 4;                                                      \
  }

#define COMPILE_X8_S24__X8_S24__C8_S24__X8_S24(state, ip, comp)         \
  {                                                                     \
    uint32_t a, b, d, e;                                                \
    uint8_t c;                                                          \
    UNPACK_24 (ip[0], a);                                               \
    UNPACK_24 (ip[1], b);                                               \
    UNPACK_8_24 (ip[2], c, d);                                          \
    UNPACK_24 (ip[3], e);                                               \
    comp (state, ip, a, b, c, d, e);                                    \
    return ip + 4;                                                      \
  }

static const uint32_t *
compile1 (scm_jit_state *state, const uint32_t *ip)
{
  switch (ip[0] & 0xff)
    {
#define COMPILE1(code, cname, name, arity) \
    case code: COMPILE_##arity(state, ip, compile_##cname)
      FOR_EACH_VM_OPERATION(COMPILE1)
#undef COMPILE1
    default:
      abort ();
    }
}

static void
compile (scm_jit_state *state)
{
  const uint32_t *ip = state->start;
  while (ip < state->end)
    ip = compile1 (state, ip);
}

void
scm_init_jit (void)
{
}
