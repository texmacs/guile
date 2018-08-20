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

#include <stdio.h> // FIXME: Remove me!

#if ENABLE_JIT
#include <lightning.h>
#endif

#include "frames.h"
#include "gsubr.h"
#include "instructions.h"
#include "intrinsics.h"
#include "threads.h"
#include "vm-builtins.h"
#include "vm-operations.h"

#include "jit.h"




static void (*enter_mcode) (scm_thread *thread, const uint8_t *mcode);
static void *exit_mcode;
static void *handle_interrupts_trampoline;
static void compute_mcode (scm_thread *, struct scm_jit_function_data *);

struct scm_jit_state {
  jit_state_t *jit;
  scm_thread *thread;
  const uint32_t *start;
  uint32_t *ip;
  const uint32_t *end;
  int32_t frame_size;
  uint8_t hooks_enabled;
};

typedef struct scm_jit_state scm_jit_state;

/* Lightning routines take an implicit parameter, _jit.  All functions
   that call lightning API should have a parameter "scm_jit_state *j";
   this definition makes lightning load its state from that
   parameter.  */
#define _jit (j->jit)

/* From the Lightning documentation:

     'frame' receives an integer argument that defines the size in bytes
     for the stack frame of the current, 'C' callable, jit function.  To
     calculate this value, a good formula is maximum number of arguments
     to any called native function times eight, plus the sum of the
     arguments to any call to 'jit_allocai'.  GNU lightning
     automatically adjusts this value for any backend specific stack
     memory it may need, or any alignment constraint.

   Here we assume that we don't have intrinsics with more than 8
   arguments.  */
static const uint32_t entry_frame_size = 8 * 8;

static const uint32_t program_word_offset_free_variable = 2;

static const uint32_t frame_offset_mra = 0 * sizeof(union scm_vm_stack_element);
static const uint32_t frame_offset_vra = 1 * sizeof(union scm_vm_stack_element);
static const uint32_t frame_offset_prev = 2 * sizeof(union scm_vm_stack_element);
static const uint32_t frame_overhead_slots = 3;

#define DEFINE_THREAD_OFFSET(f)                                         \
  static const uint32_t thread_offset_##f =                             \
    offsetof (struct scm_thread, f)

DEFINE_THREAD_OFFSET (handle);
DEFINE_THREAD_OFFSET (pending_asyncs);
DEFINE_THREAD_OFFSET (block_asyncs);

#define DEFINE_THREAD_VP_OFFSET(f)                                      \
  static const uint32_t thread_offset_##f =                             \
    offsetof (struct scm_thread, vm) + offsetof (struct scm_vm, f)

DEFINE_THREAD_VP_OFFSET (fp);
DEFINE_THREAD_VP_OFFSET (sp);
DEFINE_THREAD_VP_OFFSET (ip);
DEFINE_THREAD_VP_OFFSET (compare_result);
DEFINE_THREAD_VP_OFFSET (sp_min_since_gc);
DEFINE_THREAD_VP_OFFSET (stack_limit);
DEFINE_THREAD_VP_OFFSET (trace_level);

static const jit_gpr_t THREAD = JIT_V0;
static const jit_gpr_t SP = JIT_V1;

static const jit_gpr_t T0 = JIT_R0;
static const jit_gpr_t T1 = JIT_R1;
static const jit_gpr_t T2 = JIT_R2;
static const jit_gpr_t T3 = JIT_V2;

/* Sometimes you want to call out the fact that T3 is preserved across
   calls.  In that case, use T3_PRESERVED.  */
static const jit_gpr_t T3_PRESERVED = JIT_V2;

#ifdef WORDS_BIGENDIAN
#define BIGENDIAN 1
#else
#define BIGENDIAN 0
#endif

#if BIGENDIAN
static const uint32_t uint32_offset_low_byte = 3;
#else
static const uint32_t uint32_offset_low_byte = 0;
#endif

#if SCM_SIZEOF_UINTPTR_T == 4
static const uint32_t log2_sizeof_uintptr_t = 2;
#elif SCM_SIZEOF_UINTPTR_T == 8
static const uint32_t log2_sizeof_uintptr_t = 3;
#else
#error unhandled uintptr_t size
#endif

static void
emit_reload_sp (scm_jit_state *j)
{
  jit_ldxi (SP, THREAD, thread_offset_sp);
}

static void
emit_store_sp (scm_jit_state *j)
{
  jit_stxi (thread_offset_sp, THREAD, SP);
}

static void
emit_load_fp (scm_jit_state *j, jit_gpr_t dst)
{
  jit_ldxi (dst, THREAD, thread_offset_fp);
}

static void
emit_store_fp (scm_jit_state *j, jit_gpr_t fp)
{
  jit_stxi (thread_offset_fp, THREAD, fp);
}

static void
emit_subtract_stack_slots (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t src,
                           uint32_t n)
{
  jit_subi (dst, src, n * sizeof (union scm_vm_stack_element));
}

static void
emit_load_mra (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t fp)
{
  if (frame_offset_mra != 0)
    abort ();
  jit_ldr (dst, fp);
}

static jit_node_t *
emit_store_mra (scm_jit_state *j, jit_gpr_t fp, jit_gpr_t t)
{
  jit_node_t *addr = jit_movi (t, 0); /* patched later */
  if (frame_offset_mra != 0)
    abort ();
  jit_str (fp, t);
  return addr;
}

static void
emit_load_vra (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t fp)
{
  jit_ldxi (dst, fp, frame_offset_vra);
}

static void
emit_store_vra (scm_jit_state *j, jit_gpr_t fp, jit_gpr_t t, const uint32_t *vra)
{
  jit_movi (t, (intptr_t) vra);
  jit_stxi (frame_offset_vra, fp, t);
}

static void
emit_load_prev_fp_offset (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t fp)
{
  jit_ldxi (dst, fp, frame_offset_prev);
}

static void
emit_store_prev_fp_offset (scm_jit_state *j, jit_gpr_t fp, jit_gpr_t t,
                           uint32_t n)
{
  jit_movi (t, n);
  jit_stxi (frame_offset_prev, fp, t);
}

static void
emit_store_ip (scm_jit_state *j, jit_gpr_t ip)
{
  jit_stxi (thread_offset_ip, THREAD, ip);
}

static void
emit_store_current_ip (scm_jit_state *j, jit_gpr_t t)
{
  jit_movi (t, (intptr_t) j->ip);
  emit_store_ip (j, t);
}

static void
emit_load_compare_result (scm_jit_state *j, jit_gpr_t dst)
{
  jit_ldxi_uc (dst, THREAD, thread_offset_compare_result);
}

static void
emit_store_compare_result (scm_jit_state *j, jit_gpr_t src)
{
  jit_stxi_c (thread_offset_compare_result, THREAD, src);
}

static void
emit_reset_frame (scm_jit_state *j, jit_gpr_t fp, uint32_t nlocals)
{
  emit_subtract_stack_slots (j, SP, fp, nlocals);
  emit_store_sp (j);
}

static void
emit_call (scm_jit_state *j, void *f)
{
  jit_prepare ();
  jit_finishi (f);
}

static void
emit_call_r (scm_jit_state *j, void *f, jit_gpr_t a)
{
  jit_prepare ();
  jit_pushargr (a);
  jit_finishi (f);
}

static void
emit_call_i (scm_jit_state *j, void *f, intptr_t a)
{
  jit_prepare ();
  jit_pushargi (a);
  jit_finishi (f);
}

static void
emit_call_r_r (scm_jit_state *j, void *f, jit_gpr_t a, jit_gpr_t b)
{
  jit_prepare ();
  jit_pushargr (a);
  jit_pushargr (b);
  jit_finishi (f);
}

static void
emit_call_r_i (scm_jit_state *j, void *f, jit_gpr_t a, intptr_t b)
{
  jit_prepare ();
  jit_pushargr (a);
  jit_pushargi ((intptr_t) b);
  jit_finishi (f);
}

static void
emit_call_r_r_r (scm_jit_state *j, void *f, jit_gpr_t a, jit_gpr_t b,
                 jit_gpr_t c)
{
  jit_prepare ();
  jit_pushargr (a);
  jit_pushargr (b);
  jit_pushargr (c);
  jit_finishi (f);
}

static void
emit_alloc_frame_for_sp (scm_jit_state *j, jit_gpr_t fp, jit_gpr_t t)
{
  jit_node_t *k, *fast, *watermark;

  jit_ldxi (t, THREAD, thread_offset_sp_min_since_gc);
  fast = jit_bger (SP, t);
  jit_ldxi (t, THREAD, thread_offset_stack_limit);
  watermark = jit_bger (SP, t);

  /* Slow case: call out to expand stack.  */
  emit_store_current_ip (j, t);
  emit_call_r_r (j, scm_vm_intrinsics.expand_stack, THREAD, SP);
  emit_reload_sp (j);
  k = jit_jmpi ();

  /* Past sp_min_since_gc, but within stack_limit: update watermark and
     fall through.  */
  jit_patch (watermark);
  jit_stxi (thread_offset_sp_min_since_gc, THREAD, SP);
  jit_patch (fast);
  /* Fast case: Just update sp.  */
  emit_store_sp (j);
  jit_patch (k);
}

static void
emit_alloc_frame (scm_jit_state *j, jit_gpr_t fp, jit_gpr_t t, uint32_t nlocals)
{
  emit_subtract_stack_slots (j, SP, fp, nlocals);
  emit_alloc_frame_for_sp (j, fp, t);
}

static void
emit_get_callee_vcode (scm_jit_state *j, jit_gpr_t dst)
{
  emit_call_r (j, scm_vm_intrinsics.get_callee_vcode, THREAD);
  jit_retval (dst);
}

static void
emit_get_vcode_low_byte (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t addr)
{
  if (uint32_offset_low_byte == 0)
    jit_ldr_uc (dst, addr);
  else
    jit_ldxi_uc (dst, addr, uint32_offset_low_byte);
}

static void
emit_get_ip_relative_addr (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t ip,
                           uint32_t offset)
{
  uint32_t byte_offset = offset * sizeof (uint32_t);
  jit_ldxi (dst, ip, byte_offset);
  jit_lshi (dst, dst, 2); /* Multiply by sizeof (uint32_t) */
  jit_addr (dst, dst, ip);
}

static void
emit_exit (scm_jit_state *j)
{
  jit_patch_abs (jit_jmpi (), exit_mcode);
}

static jit_node_t*
emit_push_frame (scm_jit_state *j, uint32_t proc_slot, uint32_t nlocals,
                 const uint32_t *vra)
{
  jit_gpr_t fp = T0, old_fp = T1;
  jit_node_t *continuation;

  emit_load_fp (j, old_fp);
  emit_subtract_stack_slots (j, fp, old_fp, proc_slot);
  continuation = emit_store_mra (j, fp, T1);
  emit_store_vra (j, fp, T1, vra);
  emit_store_prev_fp_offset (j, fp, T1, proc_slot);
  emit_store_fp (j, fp);
  emit_reset_frame (j, fp, nlocals);

  return continuation;
}

static void
emit_indirect_tail_call (scm_jit_state *j)
{
  jit_node_t *not_instrumented, *no_mcode;

  emit_get_callee_vcode (j, T0);

  emit_get_vcode_low_byte (j, T1, T0);
  not_instrumented = jit_bnei (T1, scm_op_instrument_entry);
  emit_get_ip_relative_addr (j, T1, T0, 1);
  jit_ldr (T1, T1);
  no_mcode = jit_beqi (T1, 0);
  jit_jmpr (T1);

  jit_patch (not_instrumented);
  jit_patch (no_mcode);

  emit_store_ip (j, T0);
  emit_exit (j);
}

static void
emit_direct_tail_call (scm_jit_state *j, const uint32_t *vcode)
{
  if ((vcode[0] & 0xff) != scm_op_instrument_entry)
    {
      jit_movi (T0, (intptr_t) vcode);
      emit_store_ip (j, T0);
      emit_exit (j);
    }
  else
    {
      struct scm_jit_function_data *data;
      data = (struct scm_jit_function_data *) (vcode + (int32_t)(vcode[1]));

      if (data->mcode)
        {
          jit_patch_abs (jit_jmpi (), data->mcode);
        }
      else
        {
          jit_node_t *no_mcode;

          jit_ldi (T0, &data->mcode);
          no_mcode = jit_beqi (T0, 0);
          jit_jmpr (T0);
          jit_patch (no_mcode);
          jit_movi (T0, (intptr_t) vcode);
          emit_store_ip (j, T0);
          emit_exit (j);
        }
    }
}

static void
emit_fp_ref_scm (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t fp, uint32_t slot)
{
  jit_ldxi (dst, fp, -8 * (slot + 1));
}

static void
emit_fp_set_scm (scm_jit_state *j, jit_gpr_t fp, uint32_t slot, jit_gpr_t val)
{
  jit_stxi (-8 * (slot + 1), fp, val);
}

static void
emit_sp_ref_scm (scm_jit_state *j, jit_gpr_t dst, uint32_t slot)
{
  if (slot == 0)
    jit_ldr (dst, SP);
  else
    jit_ldxi (dst, SP, 8 * slot);
}

static void
emit_sp_set_scm (scm_jit_state *j, uint32_t slot, jit_gpr_t val)
{
  if (slot == 0)
    jit_str (SP, val);
  else
    jit_stxi (8 * slot, SP, val);
}

static void
emit_mov (scm_jit_state *j, uint32_t dst, uint32_t src, jit_gpr_t t)
{
  /* FIXME: The compiler currently emits "push" for SCM, F64, U64,
     and S64 variables.  However SCM values are the usual case, and
     on a 32-bit machine it might be cheaper to move a SCM than to
     move a 64-bit number.  */
  if (sizeof (void*) < sizeof (union scm_vm_stack_element))
    {
      uintptr_t src_offset = src * sizeof (union scm_vm_stack_element);
      uintptr_t dst_offset = dst * sizeof (union scm_vm_stack_element);

      jit_ldxi (t, SP, src_offset + sizeof (void*));
      jit_stxi (dst_offset + sizeof (void*), SP, t);
      if (src_offset == 0)
        jit_ldr (t, SP);
      else
        jit_ldxi (t, SP, src_offset);
      if (dst_offset == 0)
        jit_str (SP, t);
      else
        jit_stxi (dst_offset, SP, t);
    }
  else
    {
      emit_sp_ref_scm (j, t, src);
      emit_sp_set_scm (j, dst, t);
    }
}

static void
emit_run_hook (scm_jit_state *j, jit_gpr_t t, scm_t_thread_intrinsic f)
{
  jit_node_t *k;
  jit_ldxi_ui (T0, THREAD, thread_offset_trace_level);
  k = jit_beqi (T0, 0);
  emit_store_current_ip (j, T0);
  emit_call_r (j, f, THREAD);
  emit_reload_sp (j);
  jit_patch (k);
}

static jit_node_t*
emit_branch_if_frame_locals_count_less_than (scm_jit_state *j, jit_gpr_t fp,
                                             jit_gpr_t t, uint32_t nlocals)
{
  jit_subr (t, fp, SP);
  return jit_blti (t, nlocals * sizeof (union scm_vm_stack_element));
}

static jit_node_t*
emit_branch_if_frame_locals_count_eq (scm_jit_state *j, jit_gpr_t fp,
                                      jit_gpr_t t, uint32_t nlocals)
{
  jit_subr (t, fp, SP);
  return jit_beqi (t, nlocals * sizeof (union scm_vm_stack_element));
}

static jit_node_t*
emit_branch_if_frame_locals_count_greater_than (scm_jit_state *j, jit_gpr_t fp,
                                                jit_gpr_t t, uint32_t nlocals)
{
  jit_subr (t, fp, SP);
  return jit_bgti (t, nlocals * sizeof (union scm_vm_stack_element));
}

static void
emit_load_fp_slot (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t fp, uint32_t slot)
{
  jit_subi (dst, fp, (slot + 1) * sizeof (union scm_vm_stack_element));
}

static jit_node_t *
emit_branch_if_immediate (scm_jit_state *j, jit_gpr_t r)
{
  return jit_bmsi (r, 6);
}

static void
emit_load_heap_object_word (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t r,
                            uint32_t word)
{
  if (word == 0)
    jit_ldr (dst, r);
  else
    jit_ldxi (dst, r, word * sizeof(SCM));
}

static void
emit_load_heap_object_tc (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t r,
                          scm_t_bits mask)
{
  emit_load_heap_object_word (j, dst, r, 0);
  jit_andi (dst, dst, mask);
}

static jit_node_t *
emit_branch_if_heap_object_has_tc (scm_jit_state *j, jit_gpr_t r, jit_gpr_t t,
                                   scm_t_bits mask, scm_t_bits tc)
{
  emit_load_heap_object_tc (j, t, r, mask);
  return jit_beqi (t, tc);
}

static jit_node_t *
emit_branch_if_heap_object_not_tc (scm_jit_state *j, jit_gpr_t r, jit_gpr_t t,
                                   scm_t_bits mask, scm_t_bits tc)
{
  emit_load_heap_object_tc (j, t, r, mask);
  return jit_bnei (t, tc);
}

static jit_node_t *
emit_branch_if_heap_object_not_tc7 (scm_jit_state *j, jit_gpr_t r, jit_gpr_t t,
                                    scm_t_bits tc7)
{
  return emit_branch_if_heap_object_not_tc (j, r, t, 0x7f, tc7);
}

static jit_node_t*
emit_entry_trampoline (scm_jit_state *j)
{
  jit_node_t *thread, *ip, *exit;
  jit_prolog ();
  jit_frame (entry_frame_size);
  thread = jit_arg ();
  ip = jit_arg ();
  /* Load our reserved registers: THREAD and SP.  */
  jit_getarg (THREAD, thread);
  emit_reload_sp (j);
  /* Jump to the mcode!  */
  jit_getarg (JIT_R0, ip);
  jit_jmpr (JIT_R0);
  exit = jit_indirect ();
  /* When mcode returns, interpreter should continue with vp->ip.  */
  jit_ret ();
  return exit;
}

static void
emit_handle_interrupts_trampoline (scm_jit_state *j)
{
  jit_prolog ();
  jit_tramp (entry_frame_size);

  /* Precondition: IP synced, MRA in T0.  */
  emit_call_r_r (j, scm_vm_intrinsics.push_interrupt_frame, THREAD, T0);
  emit_reload_sp (j);
  emit_direct_tail_call (j, scm_vm_intrinsics.handle_interrupt_code);
}

static scm_i_pthread_once_t initialize_handle_interrupts_trampoline_once =
  SCM_I_PTHREAD_ONCE_INIT;
static void
initialize_handle_interrupts_trampoline (void)
{
  scm_thread *thread = SCM_I_CURRENT_THREAD;
  scm_jit_state saved_jit_state, *j = thread->jit_state;

  memcpy (&saved_jit_state, j, sizeof (*j));

  j->jit = jit_new_state ();
  emit_handle_interrupts_trampoline (j);
  handle_interrupts_trampoline = jit_emit ();
  jit_clear_state ();

  memcpy (j, &saved_jit_state, sizeof (*j));
}

static void
emit_free_variable_ref (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t prog,
                        size_t n)
{
  emit_load_heap_object_word (j, dst, prog,
                              n + program_word_offset_free_variable);
}

/* Use when you know that the u64 value will be within the size_t range,
   for example when it's ensured by the compiler.  */
static void
emit_sp_ref_sz (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  if (BIGENDIAN && sizeof (size_t) == 4)
    jit_ldxi (dst, SP, src * 8 + 4);
  else
    jit_ldxi (dst, SP, src * 8);
}

static void
emit_sp_set_sz (scm_jit_state *j, uint32_t dst, jit_gpr_t src)
{
  size_t offset = dst * 8;

  if (sizeof (size_t) == 4)
    {
      size_t lo, hi;
      if (BIGENDIAN)
        lo = offset + 4, hi = offset;
      else
        lo = offset, hi = offset + 4;
      
      jit_stxi (lo, SP, src);
      /* Set high word to 0.  Clobber src.  */
      jit_xorr (src, src, src);
      jit_stxi (hi, SP, src);
    }
  else
    jit_stxi (offset, SP, src);
}

#if SIZEOF_UINTPTR_T >= 8
static void
emit_sp_ref_u64 (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  size_t offset = src * 8;

  if (offset == 0)
    jit_ldr (dst, SP);
  else
    jit_ldxi (dst, SP, offset);
}

static void
emit_sp_set_u64 (scm_jit_state *j, uint32_t dst, jit_gpr_t src)
{
  size_t offset = dst * 8;

  if (dst == 0)
    jit_str (SP, src);
  else
    jit_stxi (offset, SP, src);
}

static void
emit_sp_ref_s64 (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  emit_sp_ref_u64 (j, dst, src);
}

static void
emit_sp_set_s64 (scm_jit_state *j, uint32_t dst, jit_gpr_t src)
{
  emit_sp_set_u64 (j, dst, src);
}

static void
emit_sp_ref_ptr (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  emit_sp_ref_u64 (j, dst, src);
}
#else
static void
emit_sp_ref_u64 (scm_jit_state *j, jit_gpr_t dst_lo, jit_gpr_t dst_hi,
                 uint32_t src)
{
  size_t offset = src * 8;
  jit_gpr_t first, second;

#if BIGENDIAN
  first = dst_hi, second = dst_lo;
#else
  first = dst_lo, second = dst_hi;
#endif

  if (offset == 0)
    jit_ldr (first, SP);
  else
    jit_ldxi (first, SP, offset);
  jit_ldxi (second, SP, offset + 4);
}

static void
emit_sp_set_u64 (scm_jit_state *j, uint32_t dst, jit_gpr_t lo, jit_gpr_t hi)
{
  size_t offset = dst * 8;
  jit_gpr_t first, second;

#if BIGENDIAN
  first = hi, second = lo;
#else
  first = lo, second = hi;
#endif

  if (offset == 0)
    jit_str (SP, first);
  else
    jit_stxi (offset, SP, first);
  jit_stxi (offset + 4, SP, second);
}

static void
emit_sp_ref_s64 (scm_jit_state *j, jit_gpr_t dst_lo, jit_gpr_t dst_hi,
                 uint32_t src)
{
  emit_sp_ref_u64 (j, dst_lo, dst_hi, src);
}

static void
emit_sp_set_s64 (scm_jit_state *j, uint32_t dst, jit_gpr_t lo, jit_gpr_t hi)
{
  emit_sp_set_u64 (j, dst, lo, hi);
}

static void
emit_sp_ref_u64_lower_half (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  size_t offset = src * 8;

  if (offset == 0)
    emit_ldr (dst, SP);
  else
    emit_ldxi (dst, SP, offset);
}

static void
emit_sp_ref_ptr (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  emit_sp_ref_u64_lower_half (j, dst, src);
}
#endif

static void
emit_sp_ref_f64 (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  size_t offset = src * 8;

  if (offset == 0)
    jit_ldr_d (dst, SP);
  else
    jit_ldxi_d (dst, SP, offset);
}

static void
emit_sp_set_f64 (scm_jit_state *j, uint32_t dst, jit_gpr_t src)
{
  size_t offset = dst * 8;

  if (offset == 0)
    jit_str_d (SP, src);
  else
    jit_stxi_d (offset, SP, src);
}

static void
add_inter_instruction_patch (scm_jit_state *j, jit_node_t *label,
                             const uint32_t *target)
{
  abort ();
}



static void
bad_instruction (scm_jit_state *j)
{
  abort ();
}

static void
compile_halt (scm_jit_state *j)
{
  bad_instruction (j);
}

static void
compile_call (scm_jit_state *j, uint32_t proc, uint32_t nlocals)
{
  /* 2 = size of call inst */
  jit_node_t *mcont = emit_push_frame (j, proc, nlocals, j->ip + 2);

  emit_indirect_tail_call (j);

  jit_patch (mcont);

  j->frame_size = -1;
}

static void
compile_call_label (scm_jit_state *j, uint32_t proc, uint32_t nlocals, const uint32_t *vcode)
{
  /* 2 = size of call-label inst */
  jit_node_t *mcont = emit_push_frame (j, proc, nlocals, j->ip + 3);

  emit_direct_tail_call (j, vcode);

  jit_patch (mcont);

  j->frame_size = -1;
}

static void
compile_tail_call (scm_jit_state *j)
{
  emit_indirect_tail_call (j);

  j->frame_size = -1;
}

static void
compile_tail_call_label (scm_jit_state *j, const uint32_t *vcode)
{
  emit_direct_tail_call (j, vcode);

  j->frame_size = -1;
}

static void
compile_instrument_entry (scm_jit_state *j, void *data)
{
  if (j->hooks_enabled)
    emit_run_hook (j, T0, scm_vm_intrinsics.invoke_apply_hook);
}

static void
compile_instrument_loop (scm_jit_state *j, void *data)
{
  /* Nothing to do.  */
}

static void
compile_receive (scm_jit_state *j, uint16_t dst, uint16_t proc, uint32_t nlocals)
{
  jit_gpr_t fp = T0, t = T1;
  jit_node_t *k;

  emit_load_fp (j, fp);
  k = emit_branch_if_frame_locals_count_greater_than (j, fp, t, proc);
  emit_store_current_ip (j, T0);
  emit_call (j, scm_vm_intrinsics.error_no_values);
  jit_patch (k);
  emit_fp_ref_scm (j, t, fp, proc);
  emit_fp_set_scm (j, fp, dst, t);
  emit_reset_frame (j, fp, nlocals);

  j->frame_size = nlocals;
}

static void
compile_receive_values (scm_jit_state *j, uint32_t proc, uint8_t allow_extra,
                        uint32_t nvalues)
{
  jit_gpr_t fp = T0, t = T1;

  emit_load_fp (j, fp);
  if (allow_extra)
    {
      jit_node_t *k;
      k = emit_branch_if_frame_locals_count_greater_than (j, fp, t,
                                                          proc + nvalues - 1);
      emit_store_current_ip (j, T0);
      emit_call (j, scm_vm_intrinsics.error_not_enough_values);
      jit_patch (k);
    }
  else
    {
      jit_node_t *k;
      k = emit_branch_if_frame_locals_count_eq (j, fp, t, proc + nvalues);
      emit_store_current_ip (j, T0);
      emit_call_i (j, scm_vm_intrinsics.error_wrong_number_of_values, nvalues);
      jit_patch (k);

      j->frame_size = proc + nvalues;
    }
}

static void
compile_shuffle_down (scm_jit_state *j, uint16_t from, uint16_t to)
{
  jit_gpr_t fp = T0, walk = T0, t = T1;
  size_t offset = (from - to) * sizeof (union scm_vm_stack_element);
  jit_node_t *done, *head, *back;

  emit_load_fp (j, fp);
  emit_load_fp_slot (j, walk, fp, from);
  done = jit_bltr (walk, SP);
  head = jit_label ();
  jit_ldr (t, walk);
  jit_stxi (offset, walk, t);
  jit_subi (walk, walk, sizeof (union scm_vm_stack_element));
  back = jit_bltr (walk, SP);
  jit_patch_at (back, head);
  jit_patch (done);
  jit_addi (SP, SP, offset);
  emit_store_sp (j);

  if (j->frame_size >= 0)
    j->frame_size -= (from - to);
}

static void
compile_return_values (scm_jit_state *j)
{
  jit_gpr_t old_fp = T0, offset = T1, new_fp = T1, ra = T1;
  jit_node_t *interp;
  if (j->hooks_enabled)
    emit_run_hook (j, T0, scm_vm_intrinsics.invoke_return_hook);

  emit_load_fp (j, old_fp);
  emit_load_prev_fp_offset (j, offset, old_fp);
  jit_lshi (offset, offset, 3); /* Multiply by sizeof (scm_vm_stack_element) */
  jit_addr (new_fp, old_fp, offset);
  emit_store_fp (j, new_fp);

  emit_load_mra (j, ra, old_fp);
  interp = jit_beqi (ra, 0);
  jit_jmpr (ra);

  jit_patch (interp);
  emit_load_vra (j, ra, old_fp);
  emit_store_ip (j, ra);
  emit_exit (j);
}

static void
compile_subr_call (scm_jit_state *j, uint32_t idx)
{
  jit_gpr_t fp = T0, t = T1, ret = T2;
  void *subr;
  uint32_t i;
  jit_node_t *immediate, *not_values, *k;

  if (j->frame_size < 0)
    abort ();

  subr = scm_subr_function_by_index (idx);
  emit_store_current_ip (j, t);
  emit_load_fp (j, fp);
  jit_prepare ();
  for (i = 0; i < j->frame_size; i++)
    {
      emit_fp_ref_scm (j, t, fp, i);
      jit_pushargr (t);
    }
  jit_finishi (subr);
  jit_retval (ret);

  emit_load_fp (j, fp);

  immediate = emit_branch_if_immediate (j, ret);
  not_values = emit_branch_if_heap_object_not_tc7 (j, ret, t, scm_tc7_values);
  emit_call_r_r (j, scm_vm_intrinsics.unpack_values_object, THREAD, ret);
  emit_reload_sp (j);
  k = jit_jmpi ();

  jit_patch (immediate);
  jit_patch (not_values);
  emit_subtract_stack_slots (j, SP, fp, 1);
  emit_store_sp (j);
  jit_str (SP, ret);
  jit_patch (k);
}

static void
compile_foreign_call (scm_jit_state *j, uint16_t cif_idx, uint16_t ptr_idx)
{
  emit_store_current_ip (j, T0);
  emit_load_fp (j, T0);
  emit_fp_ref_scm (j, T0, T0, 0);
  emit_free_variable_ref (j, T1, T0, cif_idx);
  emit_free_variable_ref (j, T2, T0, ptr_idx);

  /* FIXME: Inline the foreign call.  */
  emit_call_r_r_r (j, scm_vm_intrinsics.foreign_call, THREAD, T1, T2);
  emit_reload_sp (j);

  j->frame_size = 2; /* Return value and errno.  */
}

static void
compile_continuation_call (scm_jit_state *j, uint32_t contregs_idx)
{
  emit_store_current_ip (j, T0);
  emit_load_fp (j, T0);
  emit_fp_ref_scm (j, T0, T0, 0);
  emit_free_variable_ref (j, T0, T0, contregs_idx);
  emit_call_r_r (j, scm_vm_intrinsics.reinstate_continuation_x, THREAD, T0);
  /* Does not fall through.  */

  j->frame_size = -1;
}

static void
compile_compose_continuation (scm_jit_state *j, uint32_t cont_idx)
{
  jit_node_t *interp;

  emit_store_current_ip (j, T0);
  emit_load_fp (j, T0);
  emit_fp_ref_scm (j, T0, T0, 0);
  emit_free_variable_ref (j, T0, T0, cont_idx);
  emit_call_r_r (j, scm_vm_intrinsics.compose_continuation, THREAD, T0);
  jit_retval (T0);
  emit_reload_sp (j);
  interp = jit_bnei (T0, 0);
  jit_jmpr (T0);

  jit_patch (interp);
  emit_exit (j);

  j->frame_size = -1;
}

static void
compile_capture_continuation (scm_jit_state *j, uint32_t dst)
{
  emit_store_current_ip (j, T0);
  emit_call_r (j, scm_vm_intrinsics.capture_continuation, THREAD);
  jit_retval (T0);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_abort (scm_jit_state *j)
{
  jit_node_t *k, *interp;

  jit_movi (T0, (intptr_t) (j->ip + 1));
  emit_store_ip (j, T0);
  k = jit_movi (T0, 0);
  emit_call_r_r (j, scm_vm_intrinsics.abort_to_prompt, THREAD, T0);
  jit_retval (T3_PRESERVED);
  
  if (j->hooks_enabled)
    emit_run_hook (j, T0, scm_vm_intrinsics.invoke_abort_hook);

  interp = jit_bnei (T3_PRESERVED, 0);
  emit_reload_sp (j);
  jit_jmpr (T3_PRESERVED);

  jit_patch (interp);
  emit_exit (j);

  jit_patch (k);

  j->frame_size = -1;
}

static void
compile_builtin_ref (scm_jit_state *j, uint16_t dst, uint16_t idx)
{
  SCM builtin = scm_vm_builtin_ref (idx);

  jit_movi (T0, SCM_UNPACK (builtin));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_throw (scm_jit_state *j, uint16_t key, uint16_t args)
{
  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, key);
  emit_sp_ref_scm (j, T1, args);
  emit_call_r_r (j, scm_vm_intrinsics.throw_, T0, T1);
  /* throw_ does not return.  */
}

static void
compile_throw_value (scm_jit_state *j, uint32_t val,
                     const void *key_subr_and_message)
{
  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, val);
  emit_call_r_i (j, scm_vm_intrinsics.throw_with_value, T0,
                 (intptr_t) key_subr_and_message);
  /* throw_with_value does not return.  */
}

static void
compile_throw_value_and_data (scm_jit_state *j, uint32_t val,
                              const void *key_subr_and_message)
{
  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, val);
  emit_call_r_i (j, scm_vm_intrinsics.throw_with_value_and_data, T0,
                 (intptr_t) key_subr_and_message);
  /* throw_with_value_and_data does not return.  */
}

static void
compile_assert_nargs_ee (scm_jit_state *j, uint32_t nlocals)
{
  jit_node_t *k;
  jit_gpr_t fp = T0, t = T1;

  emit_load_fp (j, fp);
  k = emit_branch_if_frame_locals_count_eq (j, fp, t, nlocals);
  emit_store_current_ip (j, t);
  emit_call_r (j, scm_vm_intrinsics.error_wrong_num_args, THREAD);
  jit_patch (k);

  j->frame_size = nlocals;
}

static void
compile_assert_nargs_ge (scm_jit_state *j, uint32_t nlocals)
{
  if (nlocals > 0)
    {
      jit_gpr_t fp = T0, t = T1;
      jit_node_t *k;
      emit_load_fp (j, fp);
      k = emit_branch_if_frame_locals_count_greater_than (j, fp, t, nlocals-1);
      emit_store_current_ip (j, t);
      emit_call_r (j, scm_vm_intrinsics.error_wrong_num_args, THREAD);
      jit_patch (k);
    }
}

static void
compile_assert_nargs_le (scm_jit_state *j, uint32_t nlocals)
{
  jit_node_t *k;
  jit_gpr_t fp = T0, t = T1;

  emit_load_fp (j, fp);
  k = emit_branch_if_frame_locals_count_less_than (j, fp, t, nlocals + 1);
  emit_store_current_ip (j, t);
  emit_call_r (j, scm_vm_intrinsics.error_wrong_num_args, THREAD);
  jit_patch (k);
}

static void
compile_alloc_frame (scm_jit_state *j, uint32_t nlocals)
{
  jit_gpr_t fp = T0, t = T1;

  /* Possible for assert-nargs-ee/locals with no extra locals.  */
  if (j->frame_size == nlocals)
    return;

  emit_load_fp (j, fp);
  if (j->frame_size < 0)
    jit_movr (T3_PRESERVED, SP);
  emit_alloc_frame (j, fp, t, nlocals);

  if (j->frame_size >= 0)
    {
      int32_t slots = nlocals - j->frame_size;

      if (slots > 0)
        {
          jit_movi (T0, SCM_UNPACK (SCM_UNDEFINED));
          while (slots-- > 0)
            emit_sp_set_scm (j, slots, T0);
        }
    }
  else
    {
      jit_node_t *head, *k, *back;
      jit_movi (T0, SCM_UNPACK (SCM_UNDEFINED));
      k = jit_bler (T3_PRESERVED, SP);
      head = jit_label ();
      jit_str (T3_PRESERVED, T0);
      jit_subi (T3_PRESERVED, T3_PRESERVED, sizeof (union scm_vm_stack_element));
      back = jit_bner (T3_PRESERVED, SP);
      jit_patch_at (back, head);
      jit_patch (k);
    }

  j->frame_size = nlocals;
}

static void
compile_reset_frame (scm_jit_state *j, uint32_t nlocals)
{
  jit_gpr_t fp = T0;
  emit_load_fp (j, fp);
  emit_reset_frame (j, fp, nlocals);

  j->frame_size = nlocals;
}

static void
compile_push (scm_jit_state *j, uint32_t src)
{
  jit_gpr_t fp = T0, t = T1;
  emit_load_fp (j, fp);
  jit_subi (SP, SP, sizeof (union scm_vm_stack_element));
  emit_alloc_frame_for_sp (j, fp, t);
  emit_mov (j, 0, src + 1, t);

  if (j->frame_size >= 0)
    j->frame_size++;
}

static void
compile_pop (scm_jit_state *j, uint32_t dst)
{
  emit_mov (j, dst + 1, 0, T0);
  jit_addi (SP, SP, sizeof (union scm_vm_stack_element));
  emit_store_sp (j);

  if (j->frame_size >= 0)
    j->frame_size--;
}

static void
compile_drop (scm_jit_state *j, uint32_t nvalues)
{
  jit_addi (SP, SP, nvalues * sizeof (union scm_vm_stack_element));
  emit_store_sp (j);

  if (j->frame_size >= 0)
    j->frame_size -= nvalues;
}

static void
compile_assert_nargs_ee_locals (scm_jit_state *j, uint16_t expected,
                                uint16_t nlocals)
{
  compile_assert_nargs_ee (j, expected);
  compile_alloc_frame (j, expected + nlocals);
}

static void
compile_expand_apply_argument (scm_jit_state *j)
{
  emit_store_current_ip (j, T0);
  emit_call_r (j, scm_vm_intrinsics.expand_apply_argument, THREAD);
  emit_reload_sp (j);

  j->frame_size = -1;
}

static void
compile_bind_kwargs (scm_jit_state *j, uint32_t nreq, uint8_t flags,
                     uint32_t nreq_and_opt, uint32_t ntotal, const void *kw)
{
  uint8_t allow_other_keys = flags & 0x1, has_rest = flags & 0x2;
  jit_gpr_t t = T0, npositional = T1, fp = T1;

  emit_store_current_ip (j, t);

  jit_prepare ();
  jit_pushargr (THREAD);
  jit_pushargi (nreq);
  jit_pushargi (nreq_and_opt - nreq);
  jit_finishi (scm_vm_intrinsics.compute_kwargs_npositional);
  jit_retval_i (npositional);

  jit_prepare ();
  jit_pushargr (THREAD);
  jit_pushargr (npositional);
  jit_pushargi (ntotal);
  jit_pushargi ((intptr_t) kw);
  jit_pushargi (!has_rest);
  jit_pushargi (allow_other_keys);
  jit_finishi (scm_vm_intrinsics.bind_kwargs);
  
  emit_reload_sp (j);

  if (has_rest)
    {
      emit_call_r_i (j, scm_vm_intrinsics.cons_rest, THREAD, ntotal);
      jit_retval (t);
      emit_load_fp (j, fp);
      emit_fp_set_scm (j, fp, nreq_and_opt, t);
    }
  else
    emit_load_fp (j, fp);

  emit_reset_frame (j, T1, ntotal);
  j->frame_size = ntotal;
}

static void
compile_bind_rest (scm_jit_state *j, uint32_t dst)
{
  jit_node_t *k, *cons;
  jit_gpr_t fp = T0, t = T1;
  
  emit_load_fp (j, fp);
  cons = emit_branch_if_frame_locals_count_greater_than (j, fp, t, dst);

  compile_alloc_frame (j, dst + 1);
  jit_movi (t, SCM_UNPACK (SCM_EOL));
  emit_sp_set_scm (j, 0, t);
  k = jit_jmpi ();

  jit_patch (cons);
  emit_store_current_ip (j, t);
  emit_call_r_i (j, scm_vm_intrinsics.cons_rest, THREAD, dst);
  jit_retval (t);
  emit_sp_set_scm (j, 0, t);
  compile_reset_frame (j, dst + 1);
  
  jit_patch (k);
}

static void
compile_allocate_words (scm_jit_state *j, uint16_t dst, uint16_t nwords)
{
  jit_gpr_t t = T0;

  emit_store_current_ip (j, t);
  emit_sp_ref_sz (j, t, nwords);
  emit_call_r_r (j, scm_vm_intrinsics.allocate_words, THREAD, t);
  jit_retval (t);
  emit_sp_set_scm (j, dst, t);
}

static void
compile_allocate_words_immediate (scm_jit_state *j, uint16_t dst, uint16_t nwords)
{
  jit_gpr_t t = T0;

  emit_store_current_ip (j, t);
  jit_movi (t, nwords);
  emit_call_r_r (j, scm_vm_intrinsics.allocate_words, THREAD, t);
  jit_retval (t);
  emit_sp_set_scm (j, dst, t);
}

static void
compile_scm_ref (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, idx);
  jit_lshi (T1, T1, log2_sizeof_uintptr_t);
  jit_ldxr (T0, T0, T1);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_scm_set (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, idx);
  emit_sp_ref_scm (j, T2, val);
  jit_lshi (T1, T1, log2_sizeof_uintptr_t);
  jit_stxr (T0, T1, T2);
}

static void
compile_scm_ref_tag (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t tag)
{
  emit_sp_ref_scm (j, T0, obj);
  jit_ldr (T0, T0);
  jit_subi (T0, T0, tag);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_scm_set_tag (scm_jit_state *j, uint8_t obj, uint8_t tag, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, val);
  jit_addi (T1, T1, tag);
  jit_str (T0, T1);
}

static void
compile_scm_ref_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  jit_ldxi (T0, T0, idx * sizeof (SCM));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_scm_set_immediate (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, val);
  jit_stxi (idx * sizeof (SCM), T0, T1);
}

static void
compile_word_ref (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, idx);
  jit_lshi (T1, T1, log2_sizeof_uintptr_t);
  jit_ldxr (T0, T0, T1);
  emit_sp_set_sz (j, dst, T0);
}

static void
compile_word_set (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, idx);
  emit_sp_ref_sz (j, T2, val);
  jit_lshi (T1, T1, log2_sizeof_uintptr_t);
  jit_stxr (T0, T1, T2);
}

static void
compile_word_ref_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  jit_ldxi (T0, T0, idx * sizeof (SCM));
  emit_sp_set_sz (j, dst, T0);
}

static void
compile_word_set_immediate (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, val);
  jit_stxi (idx * sizeof (SCM), T0, T1);
}

static void
compile_pointer_ref_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  jit_ldxi (T0, T0, idx * sizeof (SCM));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_pointer_set_immediate (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, val);
  jit_stxi (idx * sizeof (SCM), T0, T1);
}

static void
compile_tail_pointer_ref_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  jit_addi (T0, T0, idx * sizeof (SCM));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_mov (scm_jit_state *j, uint16_t dst, uint16_t src)
{
  emit_mov (j, dst, src, T0);
}

static void
compile_long_mov (scm_jit_state *j, uint32_t dst, uint32_t src)
{
  emit_mov (j, dst, src, T0);
}

static void
compile_long_fmov (scm_jit_state *j, uint32_t dst, uint32_t src)
{
  jit_gpr_t fp = T0, t = T1;
  emit_load_fp (j, fp);
  emit_fp_ref_scm (j, t, fp, src);
  emit_fp_set_scm (j, fp, dst, t);
}

static void
compile_call_scm_from_scm_scm (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);
  emit_call_r_r (j, intrinsic, T0, T1);
  jit_retval (T0);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_scm_from_scm_uimm (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  jit_prepare ();
  jit_pushargr (T0);
  jit_pushargi (b);
  jit_finishi (intrinsic);
  jit_retval (T0);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_scm_sz_u32 (scm_jit_state *j, uint8_t a, uint8_t b, uint8_t c, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_sz (j, T1, b);
  emit_sp_ref_sz (j, T2, c);
  jit_prepare ();
  jit_pushargr (T0);
  jit_pushargr (T1);
  jit_pushargr (T2);
  jit_finishi (intrinsic);
  emit_reload_sp (j);
}

static void
compile_call_scm_from_scm (scm_jit_state *j, uint16_t dst, uint16_t a, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  jit_prepare ();
  jit_pushargr (T0);
  jit_finishi (intrinsic);
  jit_retval (T0);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_f64_from_scm (scm_jit_state *j, uint16_t dst, uint16_t a, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  jit_prepare ();
  jit_pushargr (T0);
  jit_finishi (intrinsic);
  jit_retval (JIT_F0);
  emit_reload_sp (j);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_call_u64_from_scm (scm_jit_state *j, uint16_t dst, uint16_t a, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
#if INDIRECT_INT64_INTRINSICS
  jit_prepare ();
  jit_addi (T1, SP, dst * sizeof (union scm_vm_stack_element));
  jit_pushargr (T1);
  jit_pushargr (T0);
  jit_finishi (intrinsic);
#else
  jit_prepare ();
  jit_pushargr (T0);
  jit_finishi (intrinsic);
  jit_retval (T0);
  emit_sp_set_u64 (j, dst, T0);
#endif
}

static void
compile_make_short_immediate (scm_jit_state *j, uint8_t dst, SCM a)
{
  jit_movi (T0, SCM_UNPACK (a));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_make_long_immediate (scm_jit_state *j, uint32_t dst, SCM a)
{
  jit_movi (T0, SCM_UNPACK (a));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_make_long_long_immediate (scm_jit_state *j, uint32_t dst, SCM a)
{
  jit_movi (T0, SCM_UNPACK (a));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_make_non_immediate (scm_jit_state *j, uint32_t dst, const void *data)
{
  jit_movi (T0, (uintptr_t)data);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_static_ref (scm_jit_state *j, uint32_t dst, void *loc)
{
  jit_ldi (T0, loc);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_static_set (scm_jit_state *j, uint32_t obj, void *loc)
{
  emit_sp_ref_scm (j, T0, obj);
  jit_sti (loc, T0);
}

static void
compile_static_patch (scm_jit_state *j, void *dst, const void *src)
{
  jit_movi (T0, (uintptr_t) src);
  jit_sti (dst, T0);
}

static void
compile_prompt (scm_jit_state *j, uint32_t tag, uint8_t escape_only_p,
                uint32_t proc_slot, const uint32_t *vcode)
{
  jit_node_t *mra;
  emit_store_current_ip (j, T0);
  jit_prepare ();
  jit_pushargr (THREAD);
  jit_pushargi (escape_only_p);
  emit_sp_ref_scm (j, T0, tag);
  jit_pushargr (T0);
  emit_load_fp (j, T1);
  jit_subi (T1, T1, proc_slot * sizeof (union scm_vm_stack_element));
  jit_pushargr (T1);
  jit_pushargi ((uintptr_t) vcode);
  mra = jit_movi (T2, 0);
  jit_finishi (scm_vm_intrinsics.push_prompt);
  add_inter_instruction_patch (j, mra, vcode);
}

static void
compile_load_label (scm_jit_state *j, uint32_t dst, const uint32_t *vcode)
{
  jit_movi (T0, (uintptr_t) vcode);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_u64 (j, dst, T0);
#else
  jit_movi (T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_call_s64_from_scm (scm_jit_state *j, uint16_t dst, uint16_t a, uint32_t idx)
{
  compile_call_u64_from_scm (j, dst, a, idx);
}

static void
compile_call_scm_from_u64 (scm_jit_state *j, uint16_t dst, uint16_t src, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  jit_prepare ();
#if INDIRECT_INT64_INTRINSICS
  jit_addi (T0, SP, src * sizeof (union scm_vm_stack_element));
#else
  emit_sp_ref_u64 (j, T0, src);
  jit_pushargr (T0);
#endif
  jit_finishi (intrinsic);
  jit_retval (T0);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_scm_from_s64 (scm_jit_state *j, uint16_t dst, uint16_t a, uint32_t b)
{
  compile_call_scm_from_u64 (j, dst, a, b);
}

static void
compile_tag_char (scm_jit_state *j, uint16_t dst, uint16_t src)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, src);
#else
  emit_sp_ref_u64_lower_half (j, T0, src);
#endif
  jit_lshi (T0, T0, 8);
  jit_addi (T0, T0, scm_tc8_char);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_untag_char (scm_jit_state *j, uint16_t dst, uint16_t src)
{
  emit_sp_ref_scm (j, T0, src);
  jit_rshi (T0, T0, 8);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_u64 (j, dst, T0);
#else
  jit_movi (T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_atomic_ref_scm_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t offset)
{
  emit_sp_ref_scm (j, T0, obj);
  jit_addi (T0, T0, offset * sizeof (SCM));
  emit_call_r (j, scm_vm_intrinsics.atomic_ref_scm, T0);
  jit_retval (T0);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_atomic_set_scm_immediate (scm_jit_state *j, uint8_t obj, uint8_t offset, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, val);
  jit_addi (T0, T0, offset * sizeof (SCM));
  emit_call_r_r (j, scm_vm_intrinsics.atomic_set_scm, T0, T1);
}

static void
compile_atomic_scm_swap_immediate (scm_jit_state *j, uint32_t dst, uint32_t obj, uint8_t offset, uint32_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, val);
  jit_addi (T0, T0, offset * sizeof (SCM));
  emit_call_r_r (j, scm_vm_intrinsics.atomic_swap_scm, T0, T1);
  jit_retval (T0);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_atomic_scm_compare_and_swap_immediate (scm_jit_state *j, uint32_t dst,
                                               uint32_t obj, uint8_t offset,
                                               uint32_t expected, uint32_t desired)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, expected);
  emit_sp_ref_scm (j, T2, desired);
  jit_addi (T0, T0, offset * sizeof (SCM));
  emit_call_r_r_r (j, scm_vm_intrinsics.atomic_swap_scm, T0, T1, T3);
  jit_retval (T0);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_thread_scm_scm (scm_jit_state *j, uint16_t a, uint16_t b, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);
  emit_call_r_r_r (j, intrinsic, THREAD, T0, T1);
  emit_reload_sp (j);
}

static void
compile_call_thread (scm_jit_state *j, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_call_r (j, intrinsic, THREAD);
  emit_reload_sp (j);
}

static void
compile_call_scm_from_thread_scm (scm_jit_state *j, uint16_t dst, uint16_t a, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_call_r_r (j, intrinsic, THREAD, T0);
  jit_retval (T0);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_thread_scm (scm_jit_state *j, uint32_t a, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_call_r_r (j, intrinsic, THREAD, T0);
  emit_reload_sp (j);
}

static void
compile_call_scm_from_scm_u64 (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  jit_prepare ();
  jit_pushargr (T0);
#if INDIRECT_INT64_INTRINSICS
  jit_addi (T1, SP, b * sizeof (union scm_vm_stack_element));
#else
  emit_sp_ref_u64 (j, T1, b);
  jit_pushargr (T1);
#endif
  jit_finishi (intrinsic);
  jit_retval (T0);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_scm_from_thread (scm_jit_state *j, uint32_t dst, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_call_r (j, intrinsic, THREAD);
  jit_retval (T0);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_fadd (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  jit_addr_d (JIT_F0, JIT_F0, JIT_F1);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_fsub (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  jit_subr_d (JIT_F0, JIT_F0, JIT_F1);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_fmul (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  jit_mulr_d (JIT_F0, JIT_F0, JIT_F1);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_fdiv (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  jit_divr_d (JIT_F0, JIT_F0, JIT_F1);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_uadd (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  jit_addr (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, VT3, b);
  jit_addcr (T0, T0, T2);
  jit_addxr (T1, T1, T3);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_usub (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  jit_subr (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, VT3, b);
  jit_subcr (T0, T0, T2);
  jit_subxr (T1, T1, T3);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_umul (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  jit_mulr (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: This is untested!  */
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, VT3, b);
  jit_mulr (T1, T1, T2);      /* High A times low B */
  jit_mulr (VT3, VT3, T0);    /* High B times low A */
  jit_addr (T1, T1, VT3);       /* Add high results, throw away overflow */
  jit_qmulr_u (T0, T2, T0, T2); /* Low A times low B */
  jit_addr (T1, T1, T2);        /* Add high result of low product */
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_uadd_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  jit_addi (T0, T0, a);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  jit_addci (T0, T0, a);
  jit_addxi (T1, T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_usub_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  jit_subi (T0, T0, a);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  jit_subci (T0, T0, a);
  jit_subxi (T1, T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_umul_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  jit_muli (T0, T0, b);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: This is untested!  */
  emit_sp_ref_u64 (j, T0, T1, a);
  jit_muli (T1, T1, b);         /* High A times low B */
  /* High B times low A is 0.  */
  jit_movi (T2, b);
  jit_qmulr_u (T0, T2, T0, T2); /* Low A times low B */
  jit_addr (T1, T1, T2);        /* Add high result of low product */
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_load_f64 (scm_jit_state *j, uint32_t dst, double a)
{
  jit_movi_d (JIT_F0, a);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_load_u64 (scm_jit_state *j, uint32_t dst, uint64_t a)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_movi (T0, a);
  emit_sp_set_u64 (j, dst, T0);
#else
  jit_movi (T0, a & 0xffffffff);
  jit_movi (T1, a >> 32);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_load_s64 (scm_jit_state *j, uint32_t dst, int64_t a)
{
  compile_load_u64 (j, dst, a);
}

static void
compile_current_thread (scm_jit_state *j, uint32_t dst)
{
  jit_ldxi (T0, THREAD, thread_offset_handle);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_ulogand (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  jit_andr (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3, b);
  jit_andr (T0, T0, T2);
  jit_andr (T1, T1, T3);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ulogior (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  jit_orr (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3, b);
  jit_orr (T0, T0, T2);
  jit_orr (T1, T1, T3);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ulogsub (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  jit_comr (T1, T1);
  jit_andr (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3, b);
  jit_comr (T2, T2);
  jit_comr (T3, T3);
  jit_andr (T0, T0, T2);
  jit_andr (T1, T1, T3);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ursh (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  jit_andi (T1, T1, 63);
  jit_rshr_u (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  jit_node_t *zero, *both, *done;

  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3, b);
  jit_andi (T2, T2, 63);
  zero = jit_beqi (T2, 0);
  both = jit_blti (T2, 32);

  /* 32 <= s < 64: hi = 0, lo = hi >> (s-32) */
  jit_subi (T2, 32);
  jit_rshr_u (T0, T1, T2);
  jit_movi (T1, 0);
  done = jit_jmpi ();

  jit_patch (both);
  /* 0 < s < 32: hi = hi >> s, lo = lo >> s + hi << (32-s) */
  jit_negr (T3, T2);
  jit_addi (T3, T3, 32);
  jit_lshr (T3, T1, T3);
  jit_rshr_u (T1, T1, T2);
  jit_rshr_u (T0, T0, T2);
  jit_addr (T0, T0, T3);

  jit_patch (done);
  jit_patch (zero);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ulsh (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  jit_andi (T1, T1, 63);
  jit_lshr (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  jit_node_t *zero, *both, *done;

  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3, b);
  jit_andi (T2, T2, 63);
  zero = jit_beqi (T2, 0);
  both = jit_blti (T2, 32);

  /* 32 <= s < 64: hi = lo << (s-32), lo = 0 */
  jit_subi (T2, 32);
  jit_lshr (T1, T0, T2);
  jit_movi (T0, 0);
  done = jit_jmpi ();

  jit_patch (both);
  /* 0 < s < 32: hi = hi << s + lo >> (32-s), lo = lo << s */
  jit_negr (T3, T2);
  jit_addi (T3, T3, 32);
  jit_rshr_u (T3, T0, T3);
  jit_lshr (T1, T1, T2);
  jit_lshr (T0, T0, T2);
  jit_addr (T1, T1, T3);

  jit_patch (done);
  jit_patch (zero);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ursh_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  b &= 63;

#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  jit_rshi_u (T0, T0, b);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  emit_sp_ref_u64 (j, T0, T1, a);
  if (b == 0)
    {
      /* Nothing to do.  */
    }
  else if (b >= 32)
    {
      /*  hi = 0, lo = hi >> (s-32) */
      jit_rshi_u (T0, T1, b - 32);
      jit_movi (T1, 0);
    }
  else
    {
      /* 0 < s < 32: hi = hi >> s, lo = lo >> s + hi << (32-s) */
      jit_lshi (T2, T1, 32 - b);
      jit_rshi_u (T1, T1, b);
      jit_rshi_u (T0, T0, b);
      jit_addr (T0, T0, T2);
    }
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ulsh_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  b &= 63;

#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  jit_lshi (T0, T0, b);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  emit_sp_ref_u64 (j, T0, T1, a);
  if (b == 0)
    {
      /* Nothing to do.  */
    }
  else if (b >= 32)
    {
      /* hi = lo << (s-32), lo = 0 */
      jit_lshr (T1, T0, b - 32);
      jit_movi (T0, 0);
    }
  else
    {
      /* hi = hi << s + lo >> (32-s), lo = lo << s */
      jit_rshi_u (T2, T0, 32 - b);
      jit_lshi (T1, T1, b);
      jit_lshi (T0, T0, b);
      jit_addr (T1, T1, T2);
    }
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ulogxor (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  jit_xorr (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3, b);
  jit_xorr (T0, T0, T2);
  jit_xorr (T1, T1, T3);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_handle_interrupts (scm_jit_state *j)
{
  jit_node_t *again, *mra, *none_pending, *blocked;

  /* The slow case is a fair amount of code, so generate it once for the
     whole process and share that code.  */
  scm_i_pthread_once (&initialize_handle_interrupts_trampoline_once,
                      initialize_handle_interrupts_trampoline);

  again = jit_label ();
  jit_addi (T0, THREAD, thread_offset_pending_asyncs);
  emit_call_r (j, scm_vm_intrinsics.atomic_ref_scm, T0);
  jit_retval (T0);
  none_pending = jit_beqi (T0, SCM_UNPACK (SCM_EOL));
  jit_ldxi_i (T0, THREAD, thread_offset_block_asyncs);
  blocked = jit_beqi (T0, 0);

  emit_store_current_ip (j, T0);
  mra = jit_movi (T0, 0);
  jit_patch_at (mra, again);
  jit_patch_abs (jit_jmpi (), handle_interrupts_trampoline);

  jit_patch (none_pending);
  jit_patch (blocked);
}

static void
compile_return_from_interrupt (scm_jit_state *j)
{
  jit_gpr_t old_fp = T0, offset = T1, new_fp = T1, ra = T1;
  jit_node_t *interp;

  if (j->hooks_enabled)
    emit_run_hook (j, T0, scm_vm_intrinsics.invoke_return_hook);

  emit_load_fp (j, old_fp);
  emit_load_prev_fp_offset (j, offset, old_fp);
  jit_lshi (offset, offset, 3); /* Multiply by sizeof (scm_vm_stack_element) */
  jit_addr (new_fp, old_fp, offset);
  emit_store_fp (j, new_fp);

  emit_load_mra (j, ra, old_fp);
  interp = jit_beqi (ra, 0);
  jit_addi (SP, old_fp, frame_overhead_slots * sizeof (union scm_vm_stack_element));
  emit_store_sp (j);
  jit_jmpr (ra);

  jit_patch (interp);
  emit_load_vra (j, ra, old_fp);
  emit_store_ip (j, ra);
  jit_addi (SP, old_fp, frame_overhead_slots * sizeof (union scm_vm_stack_element));
  emit_store_sp (j);
  emit_exit (j);
}

static void
compile_u64_numerically_equal (scm_jit_state *j, uint16_t a, uint16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_node_t *k;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  k = jit_bner (T0, T1);
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  jit_patch (k);
#else
  jit_node_t *k1, *k2, *k3;
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3, b);
  k1 = jit_bner (T0, T2);
  k2 = jit_bner (T1, T3);
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  k3 = jit_jmpi ();
  jit_patch (k1);
  jit_patch (k2);
  jit_movi (T2, SCM_F_COMPARE_NONE);
#endif
  emit_store_compare_result (j, T2);
}

static void
compile_u64_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_node_t *k;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  k = jit_bger_u (T0, T1);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  jit_patch (k);
#else
  jit_node_t *k1, *k2, *less, *k3;
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3, b);
  less = jit_bltr_u (T1, T3);
  k1 = jit_bner (T1, T3);
  k2 = jit_bger_u (T0, T2);
  jit_patch (less);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  k3 = jit_jmpi ();
  jit_patch (k1);
  jit_patch (k2);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  jit_patch (k3);
#endif
  emit_store_compare_result (j, T2);
}

static void
compile_s64_numerically_equal (scm_jit_state *j, uint16_t a, uint16_t b)
{
  compile_u64_numerically_equal (j, a, b);
}

static void
compile_s64_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_node_t *k;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  k = jit_bger (T0, T1);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  jit_patch (k);
#else
  jit_node_t *k1, *k2, *less, *k3;
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3, b);
  less = jit_bltr (T1, T3);
  k1 = jit_bner (T1, T3);
  k2 = jit_bger (T0, T2);
  jit_patch (less);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  k3 = jit_jmpi ();
  jit_patch (k1);
  jit_patch (k2);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  jit_patch (k3);
#endif
  emit_store_compare_result (j, T2);
}

static void
compile_f64_numerically_equal (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_node_t *k;
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  k = jit_beqr_d (JIT_F0, JIT_F1);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  jit_patch (k);
  emit_store_compare_result (j, T2);
}

static void
compile_f64_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_node_t *less, *ge, *k1, *k2;
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  less = jit_bltr_d (JIT_F0, JIT_F1);
  ge = jit_bger_d (JIT_F0, JIT_F1);
  jit_movi (T2, SCM_F_COMPARE_INVALID);
  k1 = jit_jmpi ();
  jit_patch (ge);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  k2 = jit_jmpi ();
  jit_patch (less);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  jit_patch (k1);
  jit_patch (k2);
  emit_store_compare_result (j, T2);
}

static void
compile_numerically_equal (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_node_t *k;
  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);
  emit_call_r_r (j, scm_vm_intrinsics.numerically_equal_p, T0, T1);
  jit_retval (T0);
  emit_reload_sp (j);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  k = jit_bnei (T0, 0);
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  jit_patch (k);
  emit_store_compare_result (j, T2);
}

static void
compile_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);
  emit_call_r_r (j, scm_vm_intrinsics.less_p, T0, T1);
  jit_retval (T2);
  emit_reload_sp (j);
  emit_store_compare_result (j, T2);
}

static void
compile_check_arguments (scm_jit_state *j, uint32_t expected)
{
  jit_node_t *eq;
  jit_gpr_t fp = T0, t = T1, res = T2;
  
  emit_load_fp (j, fp);
  jit_movi (res, SCM_F_COMPARE_EQUAL);
  eq = emit_branch_if_frame_locals_count_eq (j, fp, t, expected);
  if (expected > 0)
    {
      jit_node_t *k2, *ge;
      ge = emit_branch_if_frame_locals_count_greater_than (j, fp, t, expected-1);
      jit_movi (res, SCM_F_COMPARE_LESS_THAN);
      k2 = jit_jmpi ();
      jit_patch (ge);
      jit_movi (res, SCM_F_COMPARE_NONE);
      jit_patch (k2);
    }
  else
    jit_movi (res, SCM_F_COMPARE_NONE);
  jit_patch (eq);
  emit_store_compare_result (j, T2);
}

static void
compile_check_positional_arguments (scm_jit_state *j, uint32_t nreq, uint32_t expected)
{
  jit_node_t *k, *head, *lt, *eq, *done1, *done2;
  jit_gpr_t walk = T0, npos = T1, obj = T2, t = T3, res = T0;

  emit_load_fp (j, walk);
  if (nreq == 0) abort ();
  emit_subtract_stack_slots (j, walk, walk, nreq-1);
  jit_movi (npos, nreq - 1);
  
  head = jit_label ();
  jit_addi (npos, npos, 1);
  emit_subtract_stack_slots (j, walk, walk, 1);
  k = jit_beqr (walk, SP);
  jit_ldr (obj, walk);
  jit_patch_at (emit_branch_if_immediate (j, obj), head);
  jit_patch_at (emit_branch_if_heap_object_not_tc7 (j, obj, t, scm_tc7_keyword),
                head);
  jit_patch (k);

  lt = jit_blti (npos, expected);
  eq = jit_beqi (npos, expected);
  jit_movi (res, SCM_F_COMPARE_NONE);
  done1 = jit_jmpi ();
  jit_patch (lt);
  jit_movi (res, SCM_F_COMPARE_LESS_THAN);
  done2 = jit_jmpi ();
  jit_patch (eq);
  jit_movi (res, SCM_F_COMPARE_EQUAL);
  jit_patch (done1);
  jit_patch (done2);
  jit_stxi (thread_offset_compare_result, THREAD, res);
}

static void
compile_immediate_tag_equals (scm_jit_state *j, uint32_t a, uint16_t mask,
                              uint16_t expected)
{
  jit_node_t *k;
  emit_sp_ref_scm (j, T0, a);
  jit_andi (T0, T0, mask);
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  k = jit_beqi (T0, expected);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  jit_patch (k);
  emit_store_compare_result (j, T2);
}

static void
compile_heap_tag_equals (scm_jit_state *j, uint32_t obj,
                         uint16_t mask, uint16_t expected)
{
  jit_node_t *k;
  emit_sp_ref_scm (j, T0, obj);
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  k = emit_branch_if_heap_object_has_tc (j, T0, T0, mask, expected);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  jit_patch (k);
  emit_store_compare_result (j, T2);
}

static void
compile_eq (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_node_t *k;
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  k = jit_beqi (T0, T1);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  jit_patch (k);
  emit_store_compare_result (j, T2);
}

static void
compile_j (scm_jit_state *j, const uint32_t *vcode)
{
  jit_node_t *jmp;
  jmp = jit_jmpi ();
  add_inter_instruction_patch (j, jmp, vcode);
}

static void
compile_jl (scm_jit_state *j, const uint32_t *vcode)
{
  jit_node_t *jmp;
  emit_load_compare_result (j, T0);
  jmp = jit_beqi (T0, SCM_F_COMPARE_LESS_THAN);
  add_inter_instruction_patch (j, jmp, vcode);
}

static void
compile_je (scm_jit_state *j, const uint32_t *vcode)
{
  jit_node_t *jmp;
  emit_load_compare_result (j, T0);
  jmp = jit_beqi (T0, SCM_F_COMPARE_EQUAL);
  add_inter_instruction_patch (j, jmp, vcode);
}

static void
compile_jnl (scm_jit_state *j, const uint32_t *vcode)
{
  jit_node_t *jmp;
  emit_load_compare_result (j, T0);
  jmp = jit_bnei (T0, SCM_F_COMPARE_LESS_THAN);
  add_inter_instruction_patch (j, jmp, vcode);
}

static void
compile_jne (scm_jit_state *j, const uint32_t *vcode)
{
  jit_node_t *jmp;
  emit_load_compare_result (j, T0);
  jmp = jit_bnei (T0, SCM_F_COMPARE_EQUAL);
  add_inter_instruction_patch (j, jmp, vcode);
}

static void
compile_jge (scm_jit_state *j, const uint32_t *vcode)
{
  jit_node_t *jmp;
  emit_load_compare_result (j, T0);
  jmp = jit_beqi (T0, SCM_F_COMPARE_NONE);
  add_inter_instruction_patch (j, jmp, vcode);
}

static void
compile_jnge (scm_jit_state *j, const uint32_t *vcode)
{
  jit_node_t *jmp;
  emit_load_compare_result (j, T0);
  jmp = jit_bnei (T0, SCM_F_COMPARE_NONE);
  add_inter_instruction_patch (j, jmp, vcode);
}

static void
compile_heap_numbers_equal (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_node_t *k;
  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);
  emit_call_r_r (j, scm_vm_intrinsics.heap_numbers_equal_p, T0, T1);
  jit_retval (T0);
  emit_reload_sp (j);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  k = jit_bnei (T0, 0);
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  jit_patch (k);
  emit_store_compare_result (j, T2);
}

static void
compile_untag_fixnum (scm_jit_state *j, uint16_t dst, uint16_t a)
{
  emit_sp_ref_scm (j, T0, a);
  jit_rshi (T0, T0, 2);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_s64 (j, dst, T0);
#else
  /* FIXME: Untested!  */
  jit_movi (T1, T0);
  jit_rshi (T1, T1, 31);
  emit_sp_set_s64 (j, dst, T0, T1);
#endif
}

static void
compile_tag_fixnum (scm_jit_state *j, uint16_t dst, uint16_t a)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_s64 (j, T0, a);
#else
  emit_sp_ref_s32 (j, T0, a);
#endif
  jit_lshi (T0, T0, 2);
  jit_addi (T0, T0, scm_tc2_int);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_srsh (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_s64 (j, T0, a);
  emit_sp_ref_s64 (j, T1, b);
  jit_andi (T1, T1, 63);
  jit_rshr (T0, T0, T1);
  emit_sp_set_s64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  jit_node_t *zero, *both, *done;

  emit_sp_ref_s64 (j, T0, T1, a);
  emit_sp_ref_s64 (j, T2, T3, b);
  jit_andi (T2, T2, 63);
  zero = jit_beqi (T2, 0);
  both = jit_blti (T2, 32);

  /* 32 <= s < 64: hi = hi >> 31, lo = hi >> (s-32) */
  jit_subi (T2, 32);
  jit_rshr (T0, T1, T2);
  jit_rshi (T1, T1, 31);
  done = jit_jmpi ();

  jit_patch (both);
  /* 0 < s < 32: hi = hi >> s, lo = lo >> s + hi << (32-s) */
  jit_negr (T3, T2);
  jit_addi (T3, T3, 32);
  jit_lshr (T3, T1, T3);
  jit_rshr (T1, T1, T2);
  jit_rshr_u (T0, T0, T2);
  jit_addr (T0, T0, T3);

  jit_patch (done);
  jit_patch (zero);
  emit_sp_set_s64 (j, dst, T0, T1);
#endif
}

static void
compile_srsh_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  b &= 63;

#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_s64 (j, T0, a);
  jit_rshi (T0, T0, b);
  emit_sp_set_s64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  emit_sp_ref_s64 (j, T0, T1, a);
  if (b == 0)
    {
      /* Nothing to do.  */
    }
  else if (b >= 32)
    {
      /*  hi = sign-ext, lo = hi >> (s-32) */
      jit_rshi (T0, T1, b - 32);
      jit_rshi (T1, T1, 31);
    }
  else
    {
      /* 0 < s < 32: hi = hi >> s, lo = lo >> s + hi << (32-s) */
      jit_lshi (T2, T1, 32 - b);
      jit_rshi (T1, T1, b);
      jit_rshi_u (T0, T0, b);
      jit_addr (T0, T0, T2);
    }
  emit_sp_set_s64 (j, dst, T0, T1);
#endif
}

static void
compile_s64_imm_numerically_equal (scm_jit_state *j, uint16_t a, int16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_node_t *k;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_u64 (j, T0, a);
  k = jit_bnei (T0, b);
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  jit_patch (k);
#else
  jit_node_t *k1, *k2;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_u64 (j, T0, T1, a);
  k1 = jit_bnei (T0, b);
  k2 = jit_bnei (T1, b < 0 ? -1 : 0);
  jit_movi (T2, SCM_F_COMPARE_EQUAL);
  jit_patch (k1);
  jit_patch (k2);
#endif
  emit_store_compare_result (j, T2);
}

static void
compile_u64_imm_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_node_t *k;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_u64 (j, T0, a);
  k = jit_bgei_u (T0, b);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  jit_patch (k);
#else
  jit_node_t *k1, *k2;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_u64 (j, T0, T1, a);
  k1 = jit_bgei_u (T0, b);
  k2 = jit_bnei (T1, 0);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  jit_patch (k1);
  jit_patch (k2);
#endif
  emit_store_compare_result (j, T2);
}

static void
compile_imm_u64_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_node_t *k;
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  emit_sp_ref_u64 (j, T0, a);
  k = jit_bgti_u (T0, b);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  jit_patch (k);
#else
  jit_node_t *k1, *k2;
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  emit_sp_ref_u64 (j, T0, T1, a);
  k1 = jit_bnei (T1, 0);
  k2 = jit_bgti_u (T0, b);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  jit_patch (k1);
  jit_patch (k2);
#endif
  emit_store_compare_result (j, T2);
}

static void
compile_s64_imm_less (scm_jit_state *j, uint16_t a, int16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_node_t *k;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_s64 (j, T0, a);
  k = jit_bgei (T0, b);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  jit_patch (k);
#else
  jit_node_t *k1, *k2, *k3;
  int32_t sign = b < 0 ? -1 : 0;
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  emit_sp_ref_s64 (j, T0, T1, a);
  k1 = jit_blti (T1, sign);
  k2 = jit_bnei (T1, sign);
  k3 = jit_blti (T0, b);
  jit_patch (k2);
  jit_movi (T2, SCM_F_COMPARE_NONE);
  jit_patch (k1);
  jit_patch (k3);
#endif
  emit_store_compare_result (j, T2);
}

static void
compile_imm_s64_less (scm_jit_state *j, uint16_t a, int16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_node_t *k;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_s64 (j, T0, a);
  k = jit_blei (T0, b);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  jit_patch (k);
#else
  jit_node_t *k1, *k2, *k3;
  int32_t sign = b < 0 ? -1 : 0;
  jit_movi (T2, SCM_F_COMPARE_NONE);
  emit_sp_ref_s64 (j, T0, T1, a);
  k1 = jit_blti (T1, sign);
  k2 = jit_bnei (T1, sign);
  k3 = jit_blei (T0, b);
  jit_patch (k2);
  jit_movi (T2, SCM_F_COMPARE_LESS_THAN);
  jit_patch (k1);
  jit_patch (k3);
#endif
  emit_store_compare_result (j, T2);
}

static void
compile_u8_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_uc (T0, T0, T1);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_u64 (j, dst, T0);
#else
  jit_movi (T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_u16_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_us (T0, T0, T1);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_u64 (j, dst, T0);
#else
  jit_movi (T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_u32_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  jit_ldxr_ui (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  jit_ldxr (T0, T0, T1);
  jit_movi (T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_u64_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  jit_ldxr (T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  jit_addr (T0, T0, T1);
  if (BIGENDIAN)
    {
      jit_ldxi (T1, T0, 4);
      jit_ldr (T0, T0);
    }
  else
    {
      jit_ldr (T1, T0);
      jit_ldxi (T0, T0, 4);
    }
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_u8_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T2, v);
#else
  emit_sp_ref_u64_lower_half (j, T2, v);
#endif
  jit_stxr_c (T0, T1, T2);
}

static void
compile_u16_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T2, v);
#else
  emit_sp_ref_u64_lower_half (j, T2, v);
#endif
  jit_stxr_s (T0, T1, T2);
}

static void
compile_u32_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T2, v);
  jit_stxr_i (T0, T1, T2);
#else
  emit_sp_ref_u64_lower_half (j, T2, v);
  jit_stxr (T0, T1, T2);
#endif
}

static void
compile_u64_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T2, v);
  jit_stxr (T0, T1, T2);
#else
  jit_addr (T0, T0, T1);
  emit_sp_ref_u64 (j, T1, T2, v);
  if (BIGENDIAN)
    {
      jit_str (T0, T2);
      jit_stxi (4, T0, T1);
    }
  else
    {
      jit_str (T0, T1);
      jit_stxi (4, T0, T2);
    }
#endif
}

static void
compile_s8_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_c (T0, T0, T1);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_s64 (j, dst, T0);
#else
  jit_rshi (T1, T0, 7);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_s16_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_s (T0, T0, T1);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_s64 (j, dst, T0);
#else
  jit_rshi (T1, T0, 15);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_s32_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_i (T0, T0, T1);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_s64 (j, dst, T0);
#else
  jit_rshi (T1, T0, 31);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_s64_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  compile_u64_ref (j, dst, ptr, idx);
}

static void
compile_s8_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  compile_u8_set (j, ptr, idx, v);
}

static void
compile_s16_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  compile_u16_set (j, ptr, idx, v);
}

static void
compile_s32_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  compile_u32_set (j, ptr, idx, v);
}

static void
compile_s64_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  compile_u64_set (j, ptr, idx, v);
}

static void
compile_f32_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_f (JIT_F0, T0, T1);
  jit_extr_f_d (JIT_F0, JIT_F0);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_f64_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_d (JIT_F0, T0, T1);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_f32_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  emit_sp_ref_f64 (j, JIT_F0, v);
  jit_extr_d_f (JIT_F0, JIT_F0);
  jit_stxr_d (T0, T1, JIT_F0);
}

static void
compile_f64_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  emit_sp_ref_f64 (j, JIT_F0, v);
  jit_stxr_d (T0, T1, JIT_F0);
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

#define COMPILE_NOP(j, comp)          \
  {                                   \
    bad_instruction(j);               \
  }

#define COMPILE_X32(j, comp)                                            \
  {                                                                     \
    comp (j);                                                           \
    j->ip += 1;                                                         \
  }

#define COMPILE_X8_C24(j, comp)                                         \
  {                                                                     \
    uint32_t a;                                                         \
    UNPACK_24 (j->ip[0], a);                                            \
    comp (j, a);                                                        \
    j->ip += 1;                                                         \
  }
#define COMPILE_X8_F24(j, comp)                                         \
  COMPILE_X8_C24 (j, comp)
#define COMPILE_X8_S24(j, comp)                                         \
  COMPILE_X8_C24 (j, comp)

#define COMPILE_X8_L24(j, comp)                                         \
  {                                                                     \
    int32_t a = j->ip[0];                                               \
    a >>= 8; /* Sign extension.  */                                     \
    comp (j, j->ip + a);                                                \
    j->ip += 1;                                                         \
  }
#define COMPILE_X8_C12_C12(j, comp)                                     \
  {                                                                     \
    uint16_t a, b;                                                      \
    UNPACK_12_12 (j->ip[0], a, b);                                      \
    comp (j, a, b);                                                     \
    j->ip += 1;                                                         \
  }
#define COMPILE_X8_S12_C12(j, comp)                                     \
  COMPILE_X8_C12_C12 (j, comp)
#define COMPILE_X8_S12_S12(j, comp)                                     \
  COMPILE_X8_C12_C12 (j, comp)
#define COMPILE_X8_F12_F12(j, comp)                                     \
  COMPILE_X8_C12_C12 (j, comp)

#define COMPILE_X8_S12_Z12(j, comp)                                     \
  {                                                                     \
    uint16_t a = (j->ip[0] >> 8) & 0xfff;                               \
    int16_t b = ((int32_t) j->ip[0]) >> 20; /* Sign extension.  */      \
    comp (j, a, b);                                                     \
    j->ip += 1;                                                         \
  }

#define COMPILE_X8_S8_C8_S8(j, comp)                                    \
  {                                                                     \
    uint8_t a, b, c;                                                    \
    UNPACK_8_8_8 (j->ip[0], a, b, c);                                   \
    comp (j, a, b, c);                                                  \
    j->ip += 1;                                                         \
  }
#define COMPILE_X8_S8_S8_C8(j, comp)                                    \
  COMPILE_X8_S8_C8_S8 (j, comp)
#define COMPILE_X8_S8_S8_S8(j, comp)                                    \
  COMPILE_X8_S8_C8_S8 (j, comp)

#define COMPILE_X8_S8_I16(j, comp)                                      \
  {                                                                     \
    uint8_t a;                                                          \
    scm_t_bits b;                                                       \
    UNPACK_8_16 (j->ip[0], a, b);                                       \
    comp (j, a, SCM_PACK (b));                                          \
    j->ip += 1;                                                         \
  }

#define COMPILE_X32__C32(j, comp)                                       \
  {                                                                     \
    comp (j, j->ip[1]);                                                 \
    j->ip += 2;                                                         \
  }

#define COMPILE_X32__L32(j, comp)                                       \
  {                                                                     \
    int32_t a = j->ip[1];                                               \
    comp (j, j->ip + a);                                                \
    j->ip += 2;                                                         \
  }
#define COMPILE_X32__N32(j, comp)                                       \
  COMPILE_X32__L32 (j, comp)

#define COMPILE_X8_C24__L32(j, comp)                                    \
  {                                                                     \
    uint32_t a;                                                         \
    int32_t b;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    b = j->ip[1];                                                       \
    comp (j, a, j->ip + b);                                             \
    j->ip += 2;                                                         \
  }
#define COMPILE_X8_S24__L32(j, comp)                                    \
  COMPILE_X8_C24__L32 (j, comp)
#define COMPILE_X8_S24__LO32(j, comp)                                   \
  COMPILE_X8_C24__L32 (j, comp)
#define COMPILE_X8_S24__N32(j, comp)                                    \
  COMPILE_X8_C24__L32 (j, comp)
#define COMPILE_X8_S24__R32(j, comp)                                    \
  COMPILE_X8_C24__L32 (j, comp)

#define COMPILE_X8_C24__X8_C24(j, comp)                                 \
  {                                                                     \
    uint32_t a, b;                                                      \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_24 (j->ip[1], b);                                            \
    comp (j, a, b);                                                     \
    j->ip += 2;                                                         \
  }
#define COMPILE_X8_F24__X8_C24(j, comp)                                 \
  COMPILE_X8_C24__X8_C24(j, comp)
#define COMPILE_X8_F24__X8_F24(j, comp)                                 \
  COMPILE_X8_C24__X8_C24(j, comp)
#define COMPILE_X8_S24__X8_S24(j, comp)                                 \
  COMPILE_X8_C24__X8_C24(j, comp)

#define COMPILE_X8_F12_F12__X8_C24(j, comp)                             \
  {                                                                     \
    uint16_t a, b;                                                      \
    uint32_t c;                                                         \
    UNPACK_12_12 (j->ip[0], a, b);                                      \
    UNPACK_24 (j->ip[1], c);                                            \
    comp (j, a, b, c);                                                  \
    j->ip += 2;                                                         \
  }

#define COMPILE_X8_F24__B1_X7_C24(j, comp)                              \
  {                                                                     \
    uint32_t a, c;                                                      \
    uint8_t b;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    b = j->ip[1] & 0x1;                                                 \
    UNPACK_24 (j->ip[1], c);                                            \
    comp (j, a, b, c);                                                  \
    j->ip += 2;                                                         \
  }

#define COMPILE_X8_S12_S12__C32(j, comp)                                \
  {                                                                     \
    uint16_t a, b;                                                      \
    uint32_t c;                                                         \
    UNPACK_12_12 (j->ip[0], a, b);                                      \
    c = j->ip[1];                                                       \
    comp (j, a, b, c);                                                  \
    j->ip += 2;                                                         \
  }

#define COMPILE_X8_S24__C16_C16(j, comp)                                \
  {                                                                     \
    uint32_t a;                                                         \
    uint16_t b, c;                                                      \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_16_16 (j->ip[1], b, c);                                      \
    comp (j, a, b, c);                                                  \
    j->ip += 2;                                                         \
  }

#define COMPILE_X8_S24__C32(j, comp)                                    \
  {                                                                     \
    uint32_t a, b;                                                      \
    UNPACK_24 (j->ip[0], a);                                            \
    b = j->ip[1];                                                       \
    comp (j, a, b);                                                     \
    j->ip += 2;                                                         \
  }

#define COMPILE_X8_S24__I32(j, comp)                                    \
  {                                                                     \
    uint32_t a;                                                         \
    scm_t_bits b;                                                       \
    UNPACK_24 (j->ip[0], a);                                            \
    b = j->ip[1];                                                       \
    comp (j, a, SCM_PACK (b));                                          \
    j->ip += 2;                                                         \
  }

#define COMPILE_X8_S8_S8_C8__C32(j, comp)                               \
  {                                                                     \
    uint8_t a, b, c;                                                    \
    uint32_t d;                                                         \
    UNPACK_8_8_8 (j->ip[0], a, b, c);                                   \
    d = j->ip[1];                                                       \
    comp (j, a, b, c, d);                                               \
    j->ip += 2;                                                         \
  }
#define COMPILE_X8_S8_S8_S8__C32(j, comp)                               \
  COMPILE_X8_S8_S8_C8__C32(j, comp)

#define COMPILE_X32__LO32__L32(j, comp)                                 \
  {                                                                     \
    int32_t a = j->ip[1], b = j->ip[2];                                 \
    comp (j, j->ip + a, j->ip + b);                                     \
    j->ip += 3;                                                         \
  }

#define COMPILE_X8_F24__X8_C24__L32(j, comp)                            \
  {                                                                     \
    uint32_t a, b;                                                      \
    int32_t c;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_24 (j->ip[1], b);                                            \
    c = j->ip[2];                                                       \
    comp (j, a, b, j->ip + c);                                          \
    j->ip += 3;                                                         \
  }

#define COMPILE_X8_S24__A32__B32(j, comp)                               \
  {                                                                     \
    uint32_t a;                                                         \
    uint64_t b;                                                         \
    UNPACK_24 (j->ip[0], a);                                            \
    b = (((uint64_t) j->ip[1]) << 32) | ((uint64_t) j->ip[2]);          \
    if (b > (uint64_t) UINTPTR_MAX) abort ();                           \
    comp (j, a, SCM_PACK ((uintptr_t) b));                              \
    j->ip += 3;                                                         \
  }

#define COMPILE_X8_S24__AF32__BF32(j, comp)                             \
  {                                                                     \
    uint32_t a;                                                         \
    union { uint64_t u; double d; } b;                                  \
    UNPACK_24 (j->ip[0], a);                                            \
    b.u = (((uint64_t) j->ip[1]) << 32) | ((uint64_t) j->ip[2]);        \
    comp (j, a, b.d);                                                   \
    j->ip += 3;                                                         \
  }

#define COMPILE_X8_S24__AS32__BS32(j, comp)                             \
  {                                                                     \
    uint32_t a;                                                         \
    uint64_t b;                                                         \
    UNPACK_24 (j->ip[0], a);                                            \
    b = (((uint64_t) j->ip[1]) << 32) | ((uint64_t) j->ip[2]);          \
    comp (j, a, (int64_t) b);                                           \
    j->ip += 3;                                                         \
  }

#define COMPILE_X8_S24__AU32__BU32(j, comp)                             \
  {                                                                     \
    uint32_t a;                                                         \
    uint64_t b;                                                         \
    UNPACK_24 (j->ip[0], a);                                            \
    b = (((uint64_t) j->ip[1]) << 32) | ((uint64_t) j->ip[2]);          \
    comp (j, a, b);                                                     \
    j->ip += 3;                                                         \
  }

#define COMPILE_X8_S24__B1_X7_F24__X8_L24(j, comp)                      \
  {                                                                     \
    uint32_t a, c;                                                      \
    uint8_t b;                                                          \
    int32_t d;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    b = j->ip[1] & 0x1;                                                 \
    UNPACK_24 (j->ip[1], c);                                            \
    d = j->ip[2]; d >>= 8; /* Sign extension.  */                       \
    comp (j, a, b, c, j->ip + d);                                       \
    j->ip += 3;                                                         \
  }

#define COMPILE_X8_S24__X8_S24__C8_S24(j, comp)                         \
  {                                                                     \
    uint32_t a, b, d;                                                   \
    uint8_t c;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_24 (j->ip[1], b);                                            \
    UNPACK_8_24 (j->ip[2], c, d);                                       \
    comp (j, a, b, c, d);                                               \
    j->ip += 3;                                                         \
  }

#define COMPILE_X8_C24__C8_C24__X8_C24__N32(j, comp)                    \
  {                                                                     \
    uint32_t a, c, d;                                                   \
    uint8_t b;                                                          \
    int32_t e;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_8_24 (j->ip[1], b, c);                                       \
    UNPACK_24 (j->ip[2], d);                                            \
    e = j->ip[3]; e >>= 8; /* Sign extension.  */                       \
    comp (j, a, b, c, d, j->ip + e);                                    \
    j->ip += 4;                                                         \
  }

#define COMPILE_X8_S24__X8_S24__C8_S24__X8_S24(j, comp)                 \
  {                                                                     \
    uint32_t a, b, d, e;                                                \
    uint8_t c;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_24 (j->ip[1], b);                                            \
    UNPACK_8_24 (j->ip[2], c, d);                                       \
    UNPACK_24 (j->ip[3], e);                                            \
    comp (j, a, b, c, d, e);                                            \
    j->ip += 4;                                                         \
  }

static void
compile1 (scm_jit_state *j)
{
  switch (j->ip[0] & 0xff)
    {
#define COMPILE1(code, cname, name, arity) \
      case code: COMPILE_##arity(j, compile_##cname); break;
      FOR_EACH_VM_OPERATION(COMPILE1)
#undef COMPILE1
    default:
      abort ();
    }
}

static void
compile (scm_jit_state *j)
{
  jit_prolog ();
  jit_tramp (entry_frame_size);

  j->ip = (uint32_t *) j->start;
  while (j->ip < j->end)
    {
      fprintf (stderr, "compile %p <= %p < %p\n", j->start, j->ip, j->end);
      compile1 (j);
    }
}

static scm_i_pthread_once_t initialize_jit_once = SCM_I_PTHREAD_ONCE_INIT;

static void
initialize_jit (void)
{
  scm_thread *thread = SCM_I_CURRENT_THREAD;
  scm_jit_state *j;
  jit_node_t *exit;

  init_jit (NULL);

  /* Init the thread's jit state so we can emit the entry
     trampoline.  */
  j = scm_gc_malloc_pointerless (sizeof (*j), "jit state");
  memset (j, 0, sizeof (*j));
  thread->jit_state = j;

  j->jit = jit_new_state ();
  exit = emit_entry_trampoline (j);
  enter_mcode = jit_emit ();
  exit_mcode = jit_address (exit);
  jit_clear_state ();
  j->jit = NULL;
}

static void
compute_mcode (scm_thread *thread, struct scm_jit_function_data *data)
{
  scm_jit_state *j = thread->jit_state;

  if (!j)
    {
      scm_i_pthread_once (&initialize_jit_once, initialize_jit);
      j = thread->jit_state;
      /* Count be the initialize_jit_once inits the jit state.  */
      if (!j)
        {
          j = scm_gc_malloc_pointerless (sizeof (*j), "jit state");
          memset (j, 0, sizeof (*j));
          thread->jit_state = j;
        }
    }

  j->thread = thread;
  j->start = (const uint32_t *) (((char *)data) + data->start);
  j->end = (const uint32_t *) (((char *)data) + data->end);

  if (j->start >= j->end)
    abort ();

  j->frame_size = -1;
  j->hooks_enabled = 0; /* ? */

  j->jit = jit_new_state ();

  compile (j);

  data->mcode = jit_emit ();
  {
    jit_word_t size = 0;
    jit_get_code (&size);
    fprintf (stderr, "mcode: %p,+%zu\n", data->mcode, size);
  }

  jit_clear_state ();
  j->jit = NULL;

  j->start = j->end = j->ip = NULL;
  j->frame_size = -1;
}

/* This is a temporary function; just here while we're still kicking the
   tires.  */
static SCM
scm_sys_jit_compile (SCM fn)
{
  uint32_t *code;
  struct scm_jit_function_data *data;

  if (!SCM_PROGRAM_P (fn))
    scm_wrong_type_arg ("%jit-compile", 1, fn);

  code = SCM_PROGRAM_CODE (fn);
  if (code[0] != scm_op_instrument_entry)
    scm_wrong_type_arg ("%jit-compile", 1, fn);

  fprintf (stderr, "compiling function at %p\n", code);
  data = (struct scm_jit_function_data *) (code + (int32_t)code[1]);
  fprintf (stderr, "data %p start=%d, end=%d\n", data, data->start, data->end);

  compute_mcode (SCM_I_CURRENT_THREAD, data);

  return SCM_UNSPECIFIED;
}

const uint8_t *
scm_jit_compute_mcode (scm_thread *thread, struct scm_jit_function_data *data)
{
  const uint32_t *start = (const uint32_t *) (((char *)data) + data->start);

  /* Until the JIT is tested, don't automatically JIT-compile code.
     Just return whatever code is already there.  If we decide to buy
     later, replace with something that wires up a call to
     "compute_mcode".  */
  if (start == thread->vm.ip)
    return data->mcode;

  return NULL;
}

void
scm_jit_enter_mcode (scm_thread *thread, const uint8_t *mcode)
{
  // fprintf (stderr, "entering mcode! %p\n", mcode);
  enter_mcode (thread, mcode);
}

void
scm_jit_state_free (scm_jit_state *j)
{
  if (j)
    {
      jit_destroy_state ();
      j->jit = NULL;
    }
}

void
scm_init_jit (void)
{
  scm_c_define_gsubr ("%jit-compile", 1, 0, 0, (scm_t_subr) scm_sys_jit_compile);
}
