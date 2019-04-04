/* Copyright 2018-2019
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

/* All of this whole file is within an ENABLE_JIT flag.  */
#if ENABLE_JIT

#include <stdio.h>
#include <sys/mman.h>

#include <lightening.h>

#include "frames.h"
#include "gsubr.h"
#include "instructions.h"
#include "intrinsics.h"
#include "simpos.h" /* scm_getenv_int */
#include "threads.h"
#include "vm-builtins.h"
#include "vm-operations.h"

#include "jit.h"




/* Guile's just-in-time (JIT) compiler is a simple "template JIT".  It
   produces machine code corresponding to each VM instruction,
   substituting in the arguments from the bytecode.  The generated code
   performs the same operations on the Guile program state the VM
   interpreter would: the same stack reads and writes, the same calls,
   the same control flow: the same thing.  It's a very simple JIT.

   This JIT uses GNU Lightning, a library for generating assembly code.
   It has backends for every architecture you can think of.  Lightning
   exposes a minimum of 3 "volatile" or "scratch" registers, those that
   may be overwritten by called functions, and 3 "non-volatile" or
   "preserved" registers, those whose values will persist over calls.
   Guile's JIT uses two preserved registers for itself, to store the
   current thread and the current stack pointer.  The other four
   registers are available for the JIT.  However as Guile's JIT is
   really simple and doesn't do register allocation, no other register
   is live between bytecodes; the other four registers are just scratch
   space.

   Machine code emitted by the JIT (mcode) should only ever be entered
   from the interpreter (the VM).  To enter bytecode, the interpreter
   calls an "entry trampoline" that saves the needed non-volatile
   registers, reserves some stack space, loads the thread and stack
   pointer into the reserved registers, then jumps into the mcode.  The
   mcode then does its thing.

   When mcode needs to call out to another function, e.g. via the "call"
   instruction, it makes a new frame in just the same way the VM would,
   with the difference that it also sets the machine return address
   (mRA) in the stack frame, in addition to the virtual (bytecode)
   return address (vRA).  If the callee has mcode, then the caller jumps
   to the callee's mcode.  It's a jump, not a call, as the stack is
   maintained on the side: it's not the stack used by the e.g. x86
   "call" instruction.

   When mcode calls a function that doesn't have vcode, or returns to a
   continuation that doesn't have vcode, the mcode simply returns to the
   VM interpreter, allowing the interpreter to pick up from there.  The
   return actually happens via an exit trampoline, which restores the
   saved register values.

   Every function in Guile's VM begins with an "instrument-entry"
   instruction.  The instruction links to a statically allocated "struct
   scm_jit_function_data" corresponding to that function.  When the
   interpreter sees instrument-entry, first it checks that if the
   function has mcode, by looking in the scm_jit_function_data.  If it
   has mcode, the interpreter enters mcode directly, as described above.

   If a function doesn't have mcode, "instrument-entry" will increment a
   counter in the scm_jit_function_data.  If the counter exceeds a
   threshold, the interpreter will ask the JIT compiler to produce
   mcode.  If the JIT compiler was able to do so (always possible except
   in case of resource exhaustion), then it sets the mcode pointer in
   the scm_jit_function_data, and returns the mcode pointer to the
   interpreter.  At that point the interpreter will enter mcode.

   If the counter value does not exceed the threshold, then the VM
   will interpret the function instead of running compiled code.

   Additionally, Guile puts an "instrument-loop" instruction into the
   body of each loop iteration.  It works similarly, except that the
   returned mcode pointer starts in the middle of the function, at the
   point that corresponds to the program point of the "instrument-loop"
   instruction.  The idea is that some functions have long-running loops
   in them, and it would be a shame to have to wait until the next time
   they're called to enter mcode.  Being able to "tier up" from inside a
   loop reduces overall program latency.

   Think of the JIT as microarchitecture.  The interpreter specifies the
   architecture of the VM, in terms of the stack, stack and frame
   pointers, and a virtual instruction pointer.  Sometimes this
   architectural state is manipulated by the interpreter.  Sometimes
   it's compiled down to native code.  But the existence of native code
   is a detail that's fully encapsulated; systems-oriented Guile Scheme
   can walk stacks, throw errors, reinstate partial continuations, and
   so on without being aware of the existence of the JIT.  */




static const uint32_t default_jit_threshold = 50000;

/* Threshold for when to JIT-compile a function.  Set from the
   GUILE_JIT_THRESHOLD environment variable.  */
uint32_t scm_jit_counter_threshold = -1;

/* If positive, stop JIT compilation after the Nth compilation.  Useful
   for hunting down bugs.  */
static int jit_stop_after = -1;

/* If nonzero, pause when stopping JIT compilation after the Nth
   compilation.  For debugging.  */
static int jit_pause_when_stopping = 0;

/* Log level for JIT events.  0 means off.  */
static int jit_log_level = 0;

/* Entry trampoline: saves registers, initializes THREAD and SP
   registers, and jumps into mcode. */
static void (*enter_mcode) (scm_thread *thread, const uint8_t *mcode);

/* Exit trampoline: restores registers and returns to interpreter.  */
static void *exit_mcode;

/* Handle interrupts trampoline: the slow path of the handle-interrupts
   instruction, compiled as a stub on the side to reduce code size.  */
static void *handle_interrupts_trampoline;

/* Thread-local buffer into which to write code.  */
struct code_arena
{
  uint8_t *base;
  size_t used;
  size_t size;
  struct code_arena *prev;
};

/* Branches between instructions.  */
struct pending_reloc
{
  jit_reloc_t reloc;
  ptrdiff_t target_vcode_offset;
};

/* State of the JIT compiler for the current thread.  */
struct scm_jit_state {
  jit_state_t *jit;
  scm_thread *thread;
  const uint32_t *start;
  uint32_t *ip;
  uint32_t *next_ip;
  const uint32_t *end;
  uint32_t *entry;
  uint8_t *op_attrs;
  struct pending_reloc *relocs;
  size_t reloc_idx;
  size_t reloc_count;
  void **labels;
  int32_t frame_size;
  uint32_t register_state;
  jit_gpr_t sp_cache_gpr;
  jit_fpr_t sp_cache_fpr;
  uint32_t sp_cache_gpr_idx;
  uint32_t sp_cache_fpr_idx;
  struct code_arena *code_arena;
};

typedef struct scm_jit_state scm_jit_state;

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
DEFINE_THREAD_VP_OFFSET (sp_min_since_gc);
DEFINE_THREAD_VP_OFFSET (stack_limit);

/* The current scm_thread*.  Preserved across callouts.  */
static const jit_gpr_t THREAD = JIT_V0;

/* The current stack pointer.  Clobbered across callouts.  Can be
   reloaded from the thread.  Note that any callout that might
   recursively enter the VM may move the stack pointer.  */
static const jit_gpr_t SP = JIT_R0;

/* During calls and returns -- the parts of the code that manipulate the
   frame pointer -- the current frame pointer is stored in FP.
   Otherwise this is a temp register.  It can always be reloaded from
   THREAD.  Like SP, it can move.  */
static const jit_gpr_t FP = JIT_R1;

/* Scratch registers.  */
static const jit_gpr_t T0 = JIT_V1;
static const jit_gpr_t T1 = JIT_V2;
static const jit_gpr_t T2 = JIT_R2;
SCM_UNUSED static const jit_gpr_t T3_OR_FP = JIT_R1;
SCM_UNUSED static const jit_gpr_t T4_OR_SP = JIT_R0;

/* Sometimes you want to call out the fact that T0 and T1 are preserved
   across calls.  In that case, use these.  */
static const jit_gpr_t T0_PRESERVED = JIT_V1;
static const jit_gpr_t T1_PRESERVED = JIT_V2;

static const uint32_t SP_IN_REGISTER = 0x1;
static const uint32_t FP_IN_REGISTER = 0x2;
static const uint32_t SP_CACHE_GPR = 0x4;
static const uint32_t SP_CACHE_FPR = 0x8;

static const uint8_t OP_ATTR_BLOCK = 0x1;
static const uint8_t OP_ATTR_ENTRY = 0x2;

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

#define LENGTH_NOP 0
#define LENGTH_OP1(a) 1
#define LENGTH_OP2(a,b) 2
#define LENGTH_OP3(a,b,c) 3
#define LENGTH_OP4(a,b,c,d) 4
#define LENGTH_DOP1(a) 1
#define LENGTH_DOP2(a,b) 2
#define LENGTH_DOP3(a,b,c) 3
#define LENGTH_DOP4(a,b,c,d) 4

static const uint8_t op_lengths[256] = {
#define OP_LENGTH(code, cname, name, arity) LENGTH_##arity,
FOR_EACH_VM_OPERATION(OP_LENGTH)
#undef OP_LENGTH
};

static void die (int line, const char *msg) SCM_NORETURN;
static void
die (int line, const char *msg)
{
  fprintf (stderr, "jit.c:%d: fatal: %s\n", line, msg);
  abort ();
}

#define DIE(msg) die(__LINE__, msg)

#define ASSERT(x)                                                       \
  do { if (SCM_UNLIKELY (!(x))) DIE ("assertion failed"); } while (0)

#define UNREACHABLE()                                                   \
  DIE ("unreachable")

#define _LOG(level, ...)                                                \
  do {                                                                  \
    if (SCM_UNLIKELY (jit_log_level >= level))                          \
      fprintf (stderr, "jit: " __VA_ARGS__);                            \
  } while (0)

enum {
  LOG_LEVEL_NONE,
  LOG_LEVEL_INFO,
  LOG_LEVEL_DEBUG,
  LOG_LEVEL_LOG
};

#define INFO(...)  _LOG(LOG_LEVEL_INFO, __VA_ARGS__)
#define DEBUG(...) _LOG(LOG_LEVEL_DEBUG, __VA_ARGS__)
#define LOG(...)   _LOG(LOG_LEVEL_LOG, __VA_ARGS__)

static void
reset_register_state (scm_jit_state *j, uint32_t state)
{
  j->register_state = state;
}

static void
clear_register_state (scm_jit_state *j, uint32_t state)
{
  j->register_state &= ~state;
}

static void
clear_scratch_register_state (scm_jit_state *j)
{
  reset_register_state (j, 0);
}

static void
set_register_state (scm_jit_state *j, uint32_t state)
{
  j->register_state |= state;
}

static uint32_t
has_register_state (scm_jit_state *j, uint32_t state)
{
  return (j->register_state & state) == state;
}

#define ASSERT_HAS_REGISTER_STATE(state) ASSERT (has_register_state (j, state))

static void
record_gpr_clobber (scm_jit_state *j, jit_gpr_t r)
{
  if (jit_same_gprs (j->sp_cache_gpr, r))
    clear_register_state (j, SP_CACHE_GPR);

  if (jit_same_gprs (r, SP))
    clear_register_state (j, SP_IN_REGISTER);
  else if (jit_same_gprs (r, FP))
    clear_register_state (j, FP_IN_REGISTER);
}

static void
record_fpr_clobber (scm_jit_state *j, jit_fpr_t r)
{
  if (jit_same_fprs (j->sp_cache_fpr, r))
    clear_register_state (j, SP_CACHE_FPR);
}

static void
set_sp_cache_gpr (scm_jit_state *j, uint32_t idx, jit_gpr_t r)
{
  set_register_state (j, SP_CACHE_GPR);
  j->sp_cache_gpr_idx = idx;
  if (j->sp_cache_fpr_idx == idx)
    clear_register_state (j, SP_CACHE_FPR);
}

static void
set_sp_cache_fpr (scm_jit_state *j, uint32_t idx, jit_fpr_t r)
{
  set_register_state (j, SP_CACHE_FPR);
  j->sp_cache_fpr_idx = idx;
  if (j->sp_cache_gpr_idx == idx)
    clear_register_state (j, SP_CACHE_GPR);
}

/* Q: When should I use emit_retval instead of jit_retval?  When to use
   emit_movi, emit_ldxi?

   A: Generally you should use the emit_ variants instead of the jit_
   variants.  Guile's JIT compiler has a primitive form of local
   (intrablock) register allocation that records recent stores.  A
   subsequent load might be able to replace a register read instead of a
   memory load.  This simple allocator works for straight-line code, and
   it works as long as register writes are recorded.  The JIT itself
   will clear the register allocator state at control-flow joins, but
   control flow within an instruction needs to be careful.

   It's OK to use the jit_emit, jit_retval etc primitives if you
   manually make corresponding changes to the register_state, perhaps by
   inserting record_gpr_clobber calls.  If the register is later
   clobbered by e.g. emit_sp_set_scm, sometimes those can be omitted
   though.  Also, if your instruction includes a call, that code will
   invalidate any cached register-stack-index associations, so if
   there's a call, maybe you can avoid calling emit_*.

   Note of course that an association between registers and
   stack-indexed locals is also invalidated if the stack frame expands
   via alloc-frame or push, or shrinks via reset-frame, pop, drop,
   etc.  */
static void
emit_retval (scm_jit_state *j, jit_gpr_t r)
{
  jit_retval (j->jit, r);
  record_gpr_clobber (j, r);
}

static void
emit_retval_d (scm_jit_state *j, jit_fpr_t r)
{
  jit_retval_d (j->jit, r);
  record_fpr_clobber (j, r);
}

static void
emit_movi (scm_jit_state *j, jit_gpr_t r, jit_word_t i)
{
  jit_movi (j->jit, r, i);
  record_gpr_clobber (j, r);
}

static jit_reloc_t
emit_mov_addr (scm_jit_state *j, jit_gpr_t r)
{
  record_gpr_clobber (j, r);
  return jit_mov_addr (j->jit, r);
}

static void
emit_ldxi (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t src, jit_word_t offset)
{
  if (offset == 0)
    jit_ldr (j->jit, dst, src);
  else
    jit_ldxi (j->jit, dst, src, offset);
  record_gpr_clobber (j, dst);
}

#define DEFINE_CLOBBER_RECORDING_EMITTER_R(stem, typ)                   \
static void                                                             \
emit_##stem (scm_jit_state *j, jit_##typ##_t dst, jit_##typ##_t a)      \
{                                                                       \
  jit_##stem (j->jit, dst, a);                                          \
  record_##typ##_clobber (j, dst);                                      \
}

#define DEFINE_CLOBBER_RECORDING_EMITTER_P(stem, typ)                   \
static void                                                             \
emit_##stem (scm_jit_state *j, jit_##typ##_t dst, jit_pointer_t a)      \
{                                                                       \
  jit_##stem (j->jit, dst, a);                                          \
  record_##typ##_clobber (j, dst);                                      \
}

#define DEFINE_CLOBBER_RECORDING_EMITTER_R_I(stem, typ)                 \
static void                                                             \
emit_##stem (scm_jit_state *j, jit_##typ##_t dst,                       \
            jit_##typ##_t a, jit_word_t b)                              \
{                                                                       \
  jit_##stem (j->jit, dst, a, b);                                       \
  record_##typ##_clobber (j, dst);                                      \
}

#define DEFINE_CLOBBER_RECORDING_EMITTER_R_R(stem, typ)                 \
static void                                                             \
emit_##stem (scm_jit_state *j, jit_##typ##_t dst,                       \
            jit_##typ##_t a, jit_##typ##_t b)                           \
{                                                                       \
  jit_##stem (j->jit, dst, a, b);                                       \
  record_##typ##_clobber (j, dst);                                      \
}

#define DEFINE_CLOBBER_RECORDING_EMITTER_R_R_2(stem, typ)               \
static void                                                             \
emit_##stem (scm_jit_state *j,                                          \
             jit_##typ##_t dst1, jit_##typ##_t dst2,                    \
             jit_##typ##_t a, jit_##typ##_t b)                          \
{                                                                       \
  jit_##stem (dst1, dst2, a, b);                                        \
  record_##typ##_clobber (j, dst1);                                     \
  record_##typ##_clobber (j, dst2);                                     \
}

DEFINE_CLOBBER_RECORDING_EMITTER_R(ldr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_P(ldi, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R(movr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R(comr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(ldxr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(addi, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(addr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(addr_d, fpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(subi, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(subr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(subr_d, fpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(muli, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(mulr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(mulr_d, fpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(divr_d, fpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(andi, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(andr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(orr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(xorr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(rshi, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(rshi_u, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(rshr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(rshr_u, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(lshi, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(lshr, gpr)

#if SIZEOF_UINTPTR_T < 8
DEFINE_CLOBBER_RECORDING_EMITTER_R(negr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(addci, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(addcr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(addxi, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(addxr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(subci, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(subcr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_I(subxi, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R(subxr, gpr)
DEFINE_CLOBBER_RECORDING_EMITTER_R_R_2(qmulr_u, gpr)
#endif

static void
emit_reload_sp (scm_jit_state *j)
{
  emit_ldxi (j, SP, THREAD, thread_offset_sp);
  set_register_state (j, SP_IN_REGISTER);
}

static void
emit_store_sp (scm_jit_state *j)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);
  jit_stxi (j->jit, thread_offset_sp, THREAD, SP);
}

static void
emit_reload_fp (scm_jit_state *j)
{
  emit_ldxi (j, FP, THREAD, thread_offset_fp);
  set_register_state (j, FP_IN_REGISTER);
}

static void
emit_store_fp (scm_jit_state *j)
{
  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER);
  jit_stxi (j->jit, thread_offset_fp, THREAD, FP);
}

static uint32_t
save_reloadable_register_state (scm_jit_state *j)
{
  return j->register_state & (SP_IN_REGISTER | FP_IN_REGISTER);
}

static void
restore_reloadable_register_state (scm_jit_state *j, uint32_t state)
{
  if ((state & SP_IN_REGISTER) && !has_register_state (j, SP_IN_REGISTER))
    emit_reload_sp (j);
  if ((state & FP_IN_REGISTER) && !has_register_state (j, FP_IN_REGISTER))
    emit_reload_fp (j);
}

static void
emit_subtract_stack_slots (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t src,
                           uint32_t n)
{
  emit_subi (j, dst, src, n * sizeof (union scm_vm_stack_element));
}

static void
emit_load_mra (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t fp)
{
  emit_ldxi (j, dst, fp, frame_offset_mra);
}

static jit_reloc_t
emit_store_mra (scm_jit_state *j, jit_gpr_t fp, jit_gpr_t t)
{
  jit_reloc_t reloc = emit_mov_addr (j, t);
  ASSERT (frame_offset_mra == 0);
  jit_str (j->jit, fp, t);
  return reloc;
}

static void
emit_load_vra (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t fp)
{
  emit_ldxi (j, dst, fp, frame_offset_vra);
}

static void
emit_store_vra (scm_jit_state *j, jit_gpr_t fp, jit_gpr_t t, const uint32_t *vra)
{
  emit_movi (j, t, (intptr_t) vra);
  jit_stxi (j->jit, frame_offset_vra, fp, t);
}

static void
emit_load_prev_fp_offset (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t fp)
{
  emit_ldxi (j, dst, fp, frame_offset_prev);
}

static void
emit_store_prev_fp_offset (scm_jit_state *j, jit_gpr_t fp, jit_gpr_t t,
                           uint32_t n)
{
  emit_movi (j, t, n);
  jit_stxi (j->jit, frame_offset_prev, fp, t);
}

static void
emit_store_ip (scm_jit_state *j, jit_gpr_t ip)
{
  jit_stxi (j->jit, thread_offset_ip, THREAD, ip);
}

static void
emit_store_current_ip (scm_jit_state *j, jit_gpr_t t)
{
  emit_movi (j, t, (intptr_t) j->ip);
  emit_store_ip (j, t);
}

static void
emit_pop_fp (scm_jit_state *j, jit_gpr_t old_fp)
{
  emit_ldxi (j, old_fp, THREAD, thread_offset_fp);
  emit_load_prev_fp_offset (j, FP, old_fp);
  emit_lshi (j, FP, FP, 3); /* Multiply by sizeof (scm_vm_stack_element) */
  emit_addr (j, FP, old_fp, FP);
  set_register_state (j, FP_IN_REGISTER);
  emit_store_fp (j);
}

static void
emit_reset_frame (scm_jit_state *j, uint32_t nlocals)
{
  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER);
  emit_subtract_stack_slots (j, SP, FP, nlocals);
  set_register_state (j, SP_IN_REGISTER);
  emit_store_sp (j);
  clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);
}

static void
emit_call (scm_jit_state *j, void *f)
{
  jit_calli (j->jit, f, 0, NULL, NULL);
  clear_scratch_register_state (j);
}

static void
emit_call_r (scm_jit_state *j, void *f, jit_gpr_t a)
{
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER };
  jit_arg_t args[] = { { JIT_ARG_LOC_GPR, { .gpr = a } } };

  jit_calli (j->jit, f, 1, abi, args);
  clear_scratch_register_state (j);
}

static void
emit_call_i (scm_jit_state *j, void *f, intptr_t a)
{
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER };
  jit_arg_t args[] = { { JIT_ARG_LOC_IMM, { .imm = a } } };

  jit_calli (j->jit, f, 1, abi, args);
  clear_scratch_register_state (j);
}

static void
emit_call_r_r (scm_jit_state *j, void *f, jit_gpr_t a, jit_gpr_t b)
{
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER };
  jit_arg_t args[] = { { JIT_ARG_LOC_GPR, { .gpr = a } },
                       { JIT_ARG_LOC_GPR, { .gpr = b } } };

  jit_calli (j->jit, f, 2, abi, args);
  clear_scratch_register_state (j);
}

static void
emit_call_r_i (scm_jit_state *j, void *f, jit_gpr_t a, intptr_t b)
{
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER };
  jit_arg_t args[] = { { JIT_ARG_LOC_GPR, { .gpr = a } },
                       { JIT_ARG_LOC_IMM, { .imm = b } } };

  jit_calli (j->jit, f, 2, abi, args);
  clear_scratch_register_state (j);
}

static void
emit_call_r_r_r (scm_jit_state *j, void *f, jit_gpr_t a, jit_gpr_t b,
                 jit_gpr_t c)
{
  const jit_arg_abi_t abi[] =
    { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER };
  jit_arg_t args[] = { { JIT_ARG_LOC_GPR, { .gpr = a } },
                       { JIT_ARG_LOC_GPR, { .gpr = b } },
                       { JIT_ARG_LOC_GPR, { .gpr = c } } };

  jit_calli (j->jit, f, 3, abi, args);
  clear_scratch_register_state (j);
}

static void
emit_alloc_frame_for_sp (scm_jit_state *j, jit_gpr_t t)
{
  jit_reloc_t k, fast, watermark;
  uint32_t saved_state = save_reloadable_register_state (j);

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  emit_ldxi (j, t, THREAD, thread_offset_sp_min_since_gc);
  fast = jit_bger (j->jit, SP, t);
  emit_ldxi (j, t, THREAD, thread_offset_stack_limit);
  watermark = jit_bger (j->jit, SP, t);

  /* Slow case: call out to expand stack.  */
  emit_store_current_ip (j, t);
  emit_call_r_r (j, scm_vm_intrinsics.expand_stack, THREAD, SP);
  restore_reloadable_register_state (j, saved_state);
  k = jit_jmp (j->jit);

  /* Past sp_min_since_gc, but within stack_limit: update watermark and
     fall through.  */
  jit_patch_here (j->jit, watermark);
  jit_stxi (j->jit, thread_offset_sp_min_since_gc, THREAD, SP);
  jit_patch_here (j->jit, fast);
  /* Fast case: Just update sp.  */
  emit_store_sp (j);
  jit_patch_here (j->jit, k);

  clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);
}

static void
emit_alloc_frame (scm_jit_state *j, jit_gpr_t t, uint32_t nlocals)
{
  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER);
  emit_subtract_stack_slots (j, SP, FP, nlocals);
  set_register_state (j, SP_IN_REGISTER);
  emit_alloc_frame_for_sp (j, t);
}

static void
emit_get_callee_vcode (scm_jit_state *j, jit_gpr_t dst)
{
  emit_call_r (j, scm_vm_intrinsics.get_callee_vcode, THREAD);
  emit_retval (j, dst);
  emit_reload_sp (j);
  emit_reload_fp (j);
}

static void
emit_get_vcode_low_byte (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t addr)
{
  if (uint32_offset_low_byte == 0)
    jit_ldr_uc (j->jit, dst, addr);
  else
    jit_ldxi_uc (j->jit, dst, addr, uint32_offset_low_byte);
  record_gpr_clobber (j, dst);
}

static void
emit_get_ip_relative_addr (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t ip,
                           uint32_t offset)
{
  uint32_t byte_offset = offset * sizeof (uint32_t);
  jit_ldxi_i (j->jit, dst, ip, byte_offset);
  record_gpr_clobber (j, dst);
  emit_lshi (j, dst, dst, 2); /* Multiply by sizeof (uint32_t) */
  emit_addr (j, dst, dst, ip);
}

static void
emit_exit (scm_jit_state *j)
{
  jit_jmpi (j->jit, exit_mcode);
}

static jit_reloc_t
emit_push_frame (scm_jit_state *j, uint32_t proc_slot, uint32_t nlocals,
                 const uint32_t *vra)
{
  jit_gpr_t t = T0;
  jit_reloc_t continuation;

  emit_reload_fp (j);
  emit_subtract_stack_slots (j, FP, FP, proc_slot);
  set_register_state (j, FP_IN_REGISTER);
  continuation = emit_store_mra (j, FP, t);
  emit_store_vra (j, FP, t, vra);
  emit_store_prev_fp_offset (j, FP, t, proc_slot);
  emit_store_fp (j);
  emit_reset_frame (j, nlocals);

  return continuation;
}

static void
emit_indirect_tail_call (scm_jit_state *j)
{
  jit_reloc_t not_instrumented, no_mcode;

  emit_get_callee_vcode (j, T0);

  /* FIXME: If all functions start with instrument-entry, no need for
     this check.  */
  emit_get_vcode_low_byte (j, T1, T0);
  not_instrumented = jit_bnei (j->jit, T1, scm_op_instrument_entry);

  emit_get_ip_relative_addr (j, T1, T0, 1);
  emit_ldxi (j, T1, T1, 0);
  no_mcode = jit_beqi (j->jit, T1, 0);
  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER | SP_IN_REGISTER);
  jit_jmpr (j->jit, T1);

  jit_patch_here (j->jit, not_instrumented);
  jit_patch_here (j->jit, no_mcode);

  emit_store_ip (j, T0);
  emit_exit (j);
}

static void
emit_direct_tail_call (scm_jit_state *j, const uint32_t *vcode)
{
  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER | SP_IN_REGISTER);

  if (vcode == j->start)
    {
      jit_jmpi (j->jit, j->labels[0]);
    }
  else if ((vcode[0] & 0xff) != scm_op_instrument_entry)
    {
      emit_movi (j, T0, (intptr_t) vcode);
      emit_store_ip (j, T0);
      emit_exit (j);
    }
  else
    {
      struct scm_jit_function_data *data;
      data = (struct scm_jit_function_data *) (vcode + (int32_t)(vcode[1]));

      if (data->mcode)
        {
          /* FIXME: Jump indirectly, to allow mcode to be changed
             (e.g. to add/remove breakpoints or hooks).  */
          jit_jmpi (j->jit, data->mcode);
        }
      else
        {
          jit_reloc_t no_mcode;

          /* No need to track clobbers.  */
          jit_ldi (j->jit, T0, &data->mcode);
          no_mcode = jit_beqi (j->jit, T0, 0);
          jit_jmpr (j->jit, T0);
          jit_patch_here (j->jit, no_mcode);
          jit_movi (j->jit, T0, (intptr_t) vcode);
          emit_store_ip (j, T0);
          emit_exit (j);
        }
    }
}

static jit_arg_t
fp_scm_operand (scm_jit_state *j, uint32_t slot)
{
  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER);

  return (jit_arg_t) { JIT_ARG_LOC_MEM,
      { .mem = { FP, -8 * ((ptrdiff_t) slot + 1) } } };
}

static void
emit_fp_ref_scm (scm_jit_state *j, jit_gpr_t dst, uint32_t slot)
{
  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER);

  emit_ldxi (j, dst, FP, -8 * ((ptrdiff_t) slot + 1));
}

static void
emit_fp_set_scm (scm_jit_state *j, uint32_t slot, jit_gpr_t val)
{
  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER);

  jit_stxi (j->jit, -8 * ((ptrdiff_t) slot + 1), FP, val);
  clear_register_state (j, SP_CACHE_GPR);
}

static jit_arg_t
sp_scm_operand (scm_jit_state *j, uint32_t slot)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  return (jit_arg_t) { JIT_ARG_LOC_MEM, { .mem = { SP, 8 * slot } } };
}

static void
emit_sp_ref_scm (scm_jit_state *j, jit_gpr_t dst, uint32_t slot)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  emit_ldxi (j, dst, SP, 8 * slot);
}

static void
emit_sp_set_scm (scm_jit_state *j, uint32_t slot, jit_gpr_t val)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  if (slot == 0)
    jit_str (j->jit, SP, val);
  else
    jit_stxi (j->jit, 8 * slot, SP, val);

  set_sp_cache_gpr (j, slot, val);
}

/* Use when you know that the u64 value will be within the size_t range,
   for example when it's ensured by the compiler.  */
static jit_arg_t
sp_sz_operand (scm_jit_state *j, uint32_t src)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  if (BIGENDIAN && sizeof (size_t) == 4)
    return (jit_arg_t) { JIT_ARG_LOC_MEM, { .mem = { SP, src * 8 + 4 } } };
  else
    return (jit_arg_t) { JIT_ARG_LOC_MEM, { .mem = { SP, src * 8 } } };
}

static void
emit_sp_ref_sz (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  if (BIGENDIAN && sizeof (size_t) == 4)
    emit_ldxi (j, dst, SP, src * 8 + 4);
  else
    emit_ldxi (j, dst, SP, src * 8);
}

static void
emit_sp_set_sz (scm_jit_state *j, uint32_t dst, jit_gpr_t src)
{
  size_t offset = dst * 8;

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  if (sizeof (size_t) == 4)
    {
      size_t lo, hi;
      if (BIGENDIAN)
        lo = offset + 4, hi = offset;
      else
        lo = offset, hi = offset + 4;
      
      jit_stxi (j->jit, lo, SP, src);
      /* Set high word to 0.  Clobber src.  */
      emit_xorr (j, src, src, src);
      jit_stxi (j->jit, hi, SP, src);
    }
  else
    {
      jit_stxi (j->jit, offset, SP, src);
      set_sp_cache_gpr (j, dst, src);
    }
}

static jit_arg_t
sp_u64_operand (scm_jit_state *j, uint32_t slot)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  return (jit_arg_t) { JIT_ARG_LOC_MEM, { .mem = { SP, 8 * slot } } };
}

#if SIZEOF_UINTPTR_T >= 8
static void
emit_sp_ref_u64 (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  size_t offset = src * 8;

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  emit_ldxi (j, dst, SP, offset);
}

static void
emit_sp_set_u64 (scm_jit_state *j, uint32_t dst, jit_gpr_t src)
{
  size_t offset = dst * 8;

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  if (dst == 0)
    jit_str (j->jit, SP, src);
  else
    jit_stxi (j->jit, offset, SP, src);

  set_sp_cache_gpr (j, dst, src);
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

#else /* SCM_SIZEOF_UINTPTR_T >= 8 */

static jit_arg_t
sp_s32_operand (scm_jit_state *j, uint32_t src)
{
  return sp_sz_operand (j, src);
}

static void
emit_sp_ref_s32 (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  emit_sp_ref_sz (j, dst, src);
}

static void
emit_sp_ref_u64 (scm_jit_state *j, jit_gpr_t dst_lo, jit_gpr_t dst_hi,
                 uint32_t src)
{
  size_t offset = src * 8;
  jit_gpr_t first, second;

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

#if BIGENDIAN
  first = dst_hi, second = dst_lo;
#else
  first = dst_lo, second = dst_hi;
#endif

  emit_ldxi (j, first, SP, offset);
  emit_ldxi (j, second, SP, offset + 4);
}

static void
emit_sp_set_u64 (scm_jit_state *j, uint32_t dst, jit_gpr_t lo, jit_gpr_t hi)
{
  size_t offset = dst * 8;
  jit_gpr_t first, second;

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

#if BIGENDIAN
  first = hi, second = lo;
#else
  first = lo, second = hi;
#endif

  if (offset == 0)
    jit_str (j->jit, SP, first);
  else
    jit_stxi (j->jit, offset, SP, first);
  jit_stxi (j->jit, offset + 4, SP, second);

  clear_register_state (j, SP_CACHE_GPR);
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

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  emit_ldxi (j, dst, SP, offset);
}

static void
emit_sp_ref_ptr (scm_jit_state *j, jit_gpr_t dst, uint32_t src)
{
  emit_sp_ref_u64_lower_half (j, dst, src);
}
#endif /* SCM_SIZEOF_UINTPTR_T >= 8 */

static void
emit_sp_ref_f64 (scm_jit_state *j, jit_fpr_t dst, uint32_t src)
{
  size_t offset = src * 8;

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  if (offset == 0)
    jit_ldr_d (j->jit, dst, SP);
  else
    jit_ldxi_d (j->jit, dst, SP, offset);

  record_fpr_clobber (j, dst);
}

static void
emit_sp_set_f64 (scm_jit_state *j, uint32_t dst, jit_fpr_t src)
{
  size_t offset = dst * 8;

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);

  if (offset == 0)
    jit_str_d (j->jit, SP, src);
  else
    jit_stxi_d (j->jit, offset, SP, src);

  set_sp_cache_fpr (j, dst, src);
}

static void
emit_mov (scm_jit_state *j, uint32_t dst, uint32_t src, jit_gpr_t t)
{
  emit_sp_ref_scm (j, t, src);
  emit_sp_set_scm (j, dst, t);

  /* FIXME: The compiler currently emits "push", "mov", etc for SCM,
     F64, U64, and S64 variables.  However SCM values are the usual
     case, and on a 32-bit machine it might be cheaper to move a SCM
     than to move a 64-bit number.  */
  if (sizeof (void*) < sizeof (union scm_vm_stack_element))
    {
      /* Copy the high word as well.  */
      uintptr_t src_offset = src * sizeof (union scm_vm_stack_element);
      uintptr_t dst_offset = dst * sizeof (union scm_vm_stack_element);

      jit_ldxi (j->jit, t, SP, src_offset + sizeof (void*));
      jit_stxi (j->jit, dst_offset + sizeof (void*), SP, t);

      clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);
    }
  else
    /* In any case since we move the register using GPRs, it won't be in
       a cached FPR.  */
    clear_register_state (j, SP_CACHE_FPR);
}

static jit_reloc_t
emit_branch_if_frame_locals_count_less_than (scm_jit_state *j, jit_gpr_t t,
                                             uint32_t nlocals)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER | FP_IN_REGISTER);

  emit_subr (j, t, FP, SP);
  return jit_blti (j->jit, t, nlocals * sizeof (union scm_vm_stack_element));
}

static jit_reloc_t
emit_branch_if_frame_locals_count_eq (scm_jit_state *j, jit_gpr_t t,
                                      uint32_t nlocals)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER | FP_IN_REGISTER);

  emit_subr (j, t, FP, SP);
  return jit_beqi (j->jit, t, nlocals * sizeof (union scm_vm_stack_element));
}

static jit_reloc_t
emit_branch_if_frame_locals_count_not_eq (scm_jit_state *j, jit_gpr_t t,
                                          uint32_t nlocals)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER | FP_IN_REGISTER);

  emit_subr (j, t, FP, SP);
  return jit_bnei (j->jit, t, nlocals * sizeof (union scm_vm_stack_element));
}

static jit_reloc_t
emit_branch_if_frame_locals_count_greater_than (scm_jit_state *j, jit_gpr_t t,
                                                uint32_t nlocals)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER | FP_IN_REGISTER);

  emit_subr (j, t, FP, SP);
  return jit_bgti (j->jit, t, nlocals * sizeof (union scm_vm_stack_element));
}

static void
emit_load_fp_slot (scm_jit_state *j, jit_gpr_t dst, uint32_t slot)
{
  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER);

  emit_subi (j, dst, FP, (slot + 1) * sizeof (union scm_vm_stack_element));
}

static jit_reloc_t
emit_branch_if_immediate (scm_jit_state *j, jit_gpr_t r)
{
  return jit_bmsi (j->jit, r, 6);
}

static void
emit_load_heap_object_word (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t r,
                            uint32_t word)
{
  emit_ldxi (j, dst, r, word * sizeof(SCM));
}

static void
emit_load_heap_object_tc (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t r,
                          scm_t_bits mask)
{
  emit_load_heap_object_word (j, dst, r, 0);
  emit_andi (j, dst, dst, mask);
}

static jit_reloc_t
emit_branch_if_heap_object_has_tc (scm_jit_state *j, jit_gpr_t r, jit_gpr_t t,
                                   scm_t_bits mask, scm_t_bits tc)
{
  emit_load_heap_object_tc (j, t, r, mask);
  return jit_beqi (j->jit, t, tc);
}

static jit_reloc_t
emit_branch_if_heap_object_not_tc (scm_jit_state *j, jit_gpr_t r, jit_gpr_t t,
                                   scm_t_bits mask, scm_t_bits tc)
{
  emit_load_heap_object_tc (j, t, r, mask);
  return jit_bnei (j->jit, t, tc);
}

static jit_reloc_t
emit_branch_if_heap_object_not_tc7 (scm_jit_state *j, jit_gpr_t r, jit_gpr_t t,
                                    scm_t_bits tc7)
{
  return emit_branch_if_heap_object_not_tc (j, r, t, 0x7f, tc7);
}

static void
emit_entry_trampoline (scm_jit_state *j)
{
  const jit_gpr_t gprs[] = { JIT_R0, JIT_R1, JIT_R2, JIT_V0, JIT_V1, JIT_V2 };
  size_t gpr_count = sizeof(gprs) / sizeof(gprs[0]);
  const jit_fpr_t fprs[] = { JIT_F0, JIT_F1, JIT_F2 };
  size_t fpr_count = sizeof(fprs) / sizeof(fprs[0]);

  /* Save values of callee-save registers.  */
  for (size_t i = 0; i < gpr_count; i++)
    if (jit_gpr_is_callee_save (j->jit, gprs[i]))
      jit_pushr (j->jit, gprs[i]);

  for (size_t i = 0; i < fpr_count; i++)
    if (jit_fpr_is_callee_save (j->jit, fprs[i]))
      jit_pushr_d (j->jit, fprs[i]);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER };
  jit_arg_t args[2];
  const jit_anyreg_t regs[] = { { .gpr=THREAD }, { .gpr=T0 } };

  /* Load our reserved registers: THREAD and SP.  Also load IP for the
     mcode jump.  */
  jit_receive(j->jit, 2, abi, args);
  jit_load_args(j->jit, 2, abi, args, regs);
  emit_reload_sp (j);

  /* Load FP, set during call sequences.  */
  emit_reload_fp (j);

  /* Jump to the mcode!  */
  jit_jmpr (j->jit, T0);

  /* Initialize global exit_mcode to point here.  */
  exit_mcode = jit_address (j->jit);

  /* Restore callee-save registers.  */
  for (size_t i = 0; i < fpr_count; i++)
    if (jit_fpr_is_callee_save (j->jit, fprs[fpr_count - i - 1]))
      jit_popr_d (j->jit, fprs[fpr_count - i - 1]);

  for (size_t i = 0; i < gpr_count; i++)
    if (jit_gpr_is_callee_save (j->jit, gprs[gpr_count - i - 1]))
      jit_popr (j->jit, gprs[gpr_count - i - 1]);

  /* When mcode finishes, interpreter will continue with vp->ip.  */
  jit_ret (j->jit);
}

static void
emit_handle_interrupts_trampoline (scm_jit_state *j)
{
  /* Precondition: IP synced, MRA in T0.  */
  emit_call_r_r (j, scm_vm_intrinsics.push_interrupt_frame, THREAD, T0);
  emit_reload_sp (j);
  emit_reload_fp (j);
  emit_direct_tail_call (j, scm_vm_intrinsics.handle_interrupt_code);
}

/* To limit the number of mmap calls and re-emission of JIT code, use
   256 kB code arenas.  Unused pages won't be resident.  Assume pages
   are power-of-two-sized and this size is a multiple of the page size
   on all architectures.  */
static const size_t default_code_arena_size = 0x40000;

static struct code_arena *
allocate_code_arena (size_t size, struct code_arena *prev)
{
  struct code_arena *ret = malloc (sizeof (struct code_arena));

  if (!ret) return NULL;

  memset (ret, 0, sizeof (*ret));
  ret->used = 0;
  ret->size = size;
  ret->prev = prev;
  ret->base = mmap (NULL, ret->size,
                    PROT_EXEC | PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  if (ret->base == MAP_FAILED)
    {
      perror ("allocating JIT code buffer failed");
      free (ret);
      return NULL;
    }

  INFO ("allocated code arena, %p-%p\n", ret->base, ret->base + ret->size);

  return ret;
}

static void *
emit_code (scm_jit_state *j, void (*emit) (scm_jit_state *))
{
  if (!j->code_arena)
    j->code_arena = allocate_code_arena (default_code_arena_size, NULL);

  if (!j->code_arena)
    /* Resource exhaustion; turn off JIT.  */
    return NULL;

  while (1)
    {
      struct code_arena *arena = j->code_arena;

      jit_begin(j->jit, arena->base + arena->used, arena->size - arena->used);

      uint8_t *ret = jit_address (j->jit);

      emit (j);

      if (!jit_has_overflow (j->jit))
        {
          size_t size;
          jit_end (j->jit, &size);
          ASSERT (size <= (arena->size - arena->used));
          DEBUG ("mcode: %p,+%zu\n", ret, size);
          arena->used += size;
          /* Align next JIT to 16-byte boundaries to optimize initial
             icache fetch.  */
          arena->used = (arena->used + 15) & ~15;
          /* Assertion should not be invalidated as arena size is a
             multiple of 16.  */
          ASSERT (arena->used <= arena->size);
          return ret;
        }
      else
        {
          jit_reset (j->jit);
          if (arena->used == 0)
            {
              /* Code too big to fit into empty arena; allocate a larger
                 one.  */
              INFO ("code didn't fit in empty arena of size %zu\n", arena->size);
              arena = allocate_code_arena (arena->size * 2, arena->prev);
              if (!arena)
                return NULL;
              munmap (j->code_arena->base, j->code_arena->size);
              free (j->code_arena);
              j->code_arena = arena;
            }
          else
            {
              /* Arena full; allocate another.  */
              /* FIXME: If partial code that we wrote crosses a page
                 boundary, we could tell the OS to forget about the tail
                 pages.  */
              INFO ("code didn't fit in arena tail %zu\n",
                    arena->size - arena->used);
              arena = allocate_code_arena (arena->size, arena);
              if (!arena)
                return NULL;
              j->code_arena = arena;
            }
        }
    }
}

static void
emit_free_variable_ref (scm_jit_state *j, jit_gpr_t dst, jit_gpr_t prog,
                        size_t n)
{
  emit_load_heap_object_word (j, dst, prog,
                              n + program_word_offset_free_variable);
}

static void
add_inter_instruction_patch (scm_jit_state *j, jit_reloc_t reloc,
                             const uint32_t *target)
{
  ASSERT (j->start <= target && target < j->end);
  ptrdiff_t offset = target - j->start;

  if (j->labels[offset])
    {
      jit_patch_there (j->jit, reloc, j->labels[offset]);
      return;
    }

  if (j->reloc_idx >= j->reloc_count)
    {
      size_t count = j->reloc_count * 2;
      if (!count) count = 10;
      size_t size = sizeof(*j->relocs) * count;
      ASSERT(size / sizeof(*j->relocs) == count);
      struct pending_reloc *relocs = realloc (j->relocs, size);
      if (relocs)
        {
          j->reloc_count = count;
          j->relocs = relocs;
        }
    }

  ASSERT (j->reloc_idx < j->reloc_count);
  j->relocs[j->reloc_idx].reloc = reloc;
  j->relocs[j->reloc_idx].target_vcode_offset = offset;
  j->reloc_idx++;
}



static void
bad_instruction (scm_jit_state *j)
{
  ASSERT (0);
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
  jit_reloc_t mcont = emit_push_frame (j, proc, nlocals, j->ip + 2);

  emit_indirect_tail_call (j);

  jit_patch_here (j->jit, mcont);

  reset_register_state (j, FP_IN_REGISTER | SP_IN_REGISTER);
  j->frame_size = -1;
}

static void
compile_call_label (scm_jit_state *j, uint32_t proc, uint32_t nlocals, const uint32_t *vcode)
{
  /* 2 = size of call-label inst */
  jit_reloc_t mcont = emit_push_frame (j, proc, nlocals, j->ip + 3);

  emit_direct_tail_call (j, vcode);

  jit_patch_here (j->jit, mcont);

  reset_register_state (j, FP_IN_REGISTER | SP_IN_REGISTER);
  j->frame_size = -1;
}

static void
compile_tail_call (scm_jit_state *j)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);
  restore_reloadable_register_state (j, FP_IN_REGISTER);

  emit_indirect_tail_call (j);

  j->frame_size = -1;
}

static void
compile_tail_call_label (scm_jit_state *j, const uint32_t *vcode)
{
  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER);
  restore_reloadable_register_state (j, FP_IN_REGISTER);

  emit_direct_tail_call (j, vcode);

  j->frame_size = -1;
}

static void
compile_instrument_entry (scm_jit_state *j, void *data)
{
}

static void
compile_instrument_loop (scm_jit_state *j, void *data)
{
  /* Nothing to do.  */
}

static void
compile_receive (scm_jit_state *j, uint16_t dst, uint16_t proc, uint32_t nlocals)
{
  jit_gpr_t t = T0;
  jit_reloc_t k;
  uint32_t saved_state = j->register_state;

  k = emit_branch_if_frame_locals_count_greater_than (j, t, proc);
  emit_store_current_ip (j, T0);
  emit_call (j, scm_vm_intrinsics.error_no_values);
  j->register_state = saved_state;
  jit_patch_here (j->jit, k);
  emit_fp_ref_scm (j, t, proc);
  emit_fp_set_scm (j, dst, t);
  emit_reset_frame (j, nlocals);

  j->frame_size = nlocals;
}

static void
compile_receive_values (scm_jit_state *j, uint32_t proc, uint8_t allow_extra,
                        uint32_t nvalues)
{
  jit_gpr_t t = T0;
  uint32_t saved_state = j->register_state;

  if (allow_extra)
    {
      jit_reloc_t k;
      k = emit_branch_if_frame_locals_count_greater_than (j, t, proc+nvalues-1);
      emit_store_current_ip (j, T0);
      emit_call (j, scm_vm_intrinsics.error_not_enough_values);
      j->register_state = saved_state;
      jit_patch_here (j->jit, k);
    }
  else
    {
      jit_reloc_t k;
      k = emit_branch_if_frame_locals_count_eq (j, t, proc + nvalues);
      emit_store_current_ip (j, T0);
      emit_call_i (j, scm_vm_intrinsics.error_wrong_number_of_values, nvalues);
      j->register_state = saved_state;
      jit_patch_here (j->jit, k);

      j->frame_size = proc + nvalues;
    }

  clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);
}

static void
compile_shuffle_down (scm_jit_state *j, uint16_t from, uint16_t to)
{
  jit_gpr_t walk = T0, t = T1;
  size_t offset = (from - to) * sizeof (union scm_vm_stack_element);

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER | FP_IN_REGISTER);

  emit_load_fp_slot (j, walk, from);
  jit_reloc_t done = jit_bltr (j->jit, walk, SP);
  void *head = jit_address (j->jit);
  jit_ldr (j->jit, t, walk);
  jit_stxi (j->jit, offset, walk, t);
  jit_subi (j->jit, walk, walk, sizeof (union scm_vm_stack_element));
  jit_patch_there (j->jit, jit_bger (j->jit, walk, SP), head);
  jit_patch_here (j->jit, done);
  jit_addi (j->jit, SP, SP, offset);
  emit_store_sp (j);

  clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);

  if (j->frame_size >= 0)
    j->frame_size -= (from - to);
}

static void
compile_return_values (scm_jit_state *j)
{
  jit_gpr_t old_fp = T0, ra = T1;
  jit_reloc_t interp;

  emit_pop_fp (j, old_fp);

  emit_load_mra (j, ra, old_fp);
  interp = jit_beqi (j->jit, ra, 0);
  jit_jmpr (j->jit, ra);

  jit_patch_here (j->jit, interp);
  emit_load_vra (j, ra, old_fp);
  emit_store_ip (j, ra);
  emit_exit (j);

  j->frame_size = -1;
}

static void
compile_subr_call (scm_jit_state *j, uint32_t idx)
{
  jit_gpr_t t = T0, ret = T1;
  void *subr;
  jit_reloc_t immediate, not_values, k;
  jit_arg_abi_t abi[10];
  jit_arg_t args[10];

  ASSERT (j->frame_size > 0);
  size_t argc = j->frame_size - 1;
  ASSERT (argc <= 10);

  subr = scm_subr_function_by_index (idx);
  emit_store_current_ip (j, t);
  for (size_t i = 2; i <= j->frame_size; i++)
    {
      abi[i - 2] = JIT_ARG_ABI_POINTER;
      args[i - 2] = sp_scm_operand (j, (j->frame_size - i));
    }
  jit_calli (j->jit, subr, argc, abi, args);
  clear_scratch_register_state (j);
  jit_retval (j->jit, ret);

  immediate = emit_branch_if_immediate (j, ret);
  not_values = emit_branch_if_heap_object_not_tc7 (j, ret, t, scm_tc7_values);
  emit_call_r_r (j, scm_vm_intrinsics.unpack_values_object, THREAD, ret);
  emit_reload_fp (j);
  emit_reload_sp (j);
  k = jit_jmp (j->jit);

  jit_patch_here (j->jit, immediate);
  jit_patch_here (j->jit, not_values);
  emit_reload_fp (j);
  emit_subtract_stack_slots (j, SP, FP, 1);
  set_register_state (j, SP_IN_REGISTER);
  emit_store_sp (j);
  jit_str (j->jit, SP, ret);
  jit_patch_here (j->jit, k);

  clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);

  j->frame_size = -1;
}

static void
compile_foreign_call (scm_jit_state *j, uint16_t cif_idx, uint16_t ptr_idx)
{
  uint32_t saved_state;

  ASSERT (j->frame_size >= 0);

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, j->frame_size - 1);
  emit_free_variable_ref (j, T1, T0, cif_idx);
  emit_free_variable_ref (j, T2, T0, ptr_idx);

  /* FIXME: Inline the foreign call.  */
  saved_state = save_reloadable_register_state (j);
  emit_call_r_r_r (j, scm_vm_intrinsics.foreign_call, THREAD, T1, T2);
  restore_reloadable_register_state (j, saved_state);

  j->frame_size = 2; /* Return value and errno.  */
}

static void
compile_continuation_call (scm_jit_state *j, uint32_t contregs_idx)
{
  emit_reload_fp (j);
  emit_store_current_ip (j, T0);
  emit_fp_ref_scm (j, T0, 0);
  emit_free_variable_ref (j, T0, T0, contregs_idx);
  emit_call_r_r (j, scm_vm_intrinsics.reinstate_continuation_x, THREAD, T0);
  /* Does not fall through.  */

  j->frame_size = -1;
}

static void
compile_compose_continuation (scm_jit_state *j, uint32_t cont_idx)
{
  jit_reloc_t interp;

  ASSERT_HAS_REGISTER_STATE (SP_IN_REGISTER | FP_IN_REGISTER);

  emit_store_current_ip (j, T0);
  emit_fp_ref_scm (j, T0, 0);
  emit_free_variable_ref (j, T0, T0, cont_idx);
  emit_call_r_r (j, scm_vm_intrinsics.compose_continuation, THREAD, T0);
  jit_retval (j->jit, T0);
  interp = jit_beqi (j->jit, T0, 0);
  emit_reload_sp (j);
  emit_reload_fp (j);
  jit_jmpr (j->jit, T0);

  jit_patch_here (j->jit, interp);
  emit_exit (j);

  j->frame_size = -1;
}

static void
compile_capture_continuation (scm_jit_state *j, uint32_t dst)
{
  emit_store_current_ip (j, T0);
  emit_call_r (j, scm_vm_intrinsics.capture_continuation, THREAD);
  jit_retval (j->jit, T0);
  emit_reload_sp (j);
  emit_reload_fp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_abort (scm_jit_state *j)
{
  jit_reloc_t k, interp;

  jit_movi (j->jit, T0, (intptr_t) (j->ip + 1));
  emit_store_ip (j, T0);
  k = jit_mov_addr (j->jit, T0);
  emit_call_r_r (j, scm_vm_intrinsics.abort_to_prompt, THREAD, T0);
  jit_retval (j->jit, T1_PRESERVED);
  
  interp = jit_beqi (j->jit, T1_PRESERVED, 0);
  emit_reload_sp (j);
  emit_reload_fp (j);
  jit_jmpr (j->jit, T1_PRESERVED);

  jit_patch_here (j->jit, interp);
  emit_exit (j);

  jit_patch_here (j->jit, k);

  j->frame_size = -1;
}

static void
compile_builtin_ref (scm_jit_state *j, uint16_t dst, uint16_t idx)
{
  SCM builtin = scm_vm_builtin_ref (idx);

  emit_movi (j, T0, SCM_UNPACK (builtin));
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
  jit_reloc_t k;
  jit_gpr_t t = T0;
  uint32_t saved_state = j->register_state;

  k = emit_branch_if_frame_locals_count_eq (j, t, nlocals);
  emit_store_current_ip (j, t);
  emit_call_r (j, scm_vm_intrinsics.error_wrong_num_args, THREAD);
  jit_patch_here (j->jit, k);

  j->register_state = saved_state;
  j->frame_size = nlocals;
}

static void
compile_assert_nargs_ge (scm_jit_state *j, uint32_t nlocals)
{
  if (nlocals > 0)
    {
      jit_gpr_t t = T0;
      jit_reloc_t k;
      uint32_t saved_state = j->register_state;

      k = emit_branch_if_frame_locals_count_greater_than (j, t, nlocals-1);
      emit_store_current_ip (j, t);
      emit_call_r (j, scm_vm_intrinsics.error_wrong_num_args, THREAD);
      jit_patch_here (j->jit, k);
      j->register_state = saved_state;
    }
}

static void
compile_assert_nargs_le (scm_jit_state *j, uint32_t nlocals)
{
  jit_reloc_t k;
  jit_gpr_t t = T0;
  uint32_t saved_state = j->register_state;

  k = emit_branch_if_frame_locals_count_less_than (j, t, nlocals + 1);
  emit_store_current_ip (j, t);
  emit_call_r (j, scm_vm_intrinsics.error_wrong_num_args, THREAD);
  jit_patch_here (j->jit, k);

  j->register_state = saved_state;
}

static void
compile_alloc_frame (scm_jit_state *j, uint32_t nlocals)
{
  jit_gpr_t t = T0, saved_frame_size = T1_PRESERVED;

  if (j->frame_size < 0)
    jit_subr (j->jit, saved_frame_size, FP, SP);

  /* This will clear the regalloc, so no need to track clobbers.  */
  emit_alloc_frame (j, t, nlocals);

  if (j->frame_size >= 0)
    {
      int32_t slots = nlocals - j->frame_size;

      if (slots > 0)
        {
          jit_movi (j->jit, t, SCM_UNPACK (SCM_UNDEFINED));
          while (slots-- > 0)
            emit_sp_set_scm (j, slots, t);
        }
    }
  else
    {
      jit_gpr_t walk = saved_frame_size;

      jit_subr (j->jit, walk, FP, saved_frame_size);
      jit_reloc_t k = jit_bler (j->jit, walk, SP);
      jit_movi (j->jit, t, SCM_UNPACK (SCM_UNDEFINED));
      void *head = jit_address (j->jit);
      jit_subi (j->jit, walk, walk, sizeof (union scm_vm_stack_element));
      jit_str (j->jit, walk, t);
      jit_patch_there (j->jit, jit_bner (j->jit, walk, SP), head);
      jit_patch_here (j->jit, k);
    }

  j->frame_size = nlocals;
}

static void
compile_reset_frame (scm_jit_state *j, uint32_t nlocals)
{
  restore_reloadable_register_state (j, FP_IN_REGISTER);
  emit_reset_frame (j, nlocals);

  j->frame_size = nlocals;
}

static void
compile_push (scm_jit_state *j, uint32_t src)
{
  jit_gpr_t t = T0;
  jit_subi (j->jit, SP, SP, sizeof (union scm_vm_stack_element));
  emit_alloc_frame_for_sp (j, t);
  emit_mov (j, 0, src + 1, t);

  clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);

  if (j->frame_size >= 0)
    j->frame_size++;
}

static void
compile_pop (scm_jit_state *j, uint32_t dst)
{
  emit_mov (j, dst + 1, 0, T0);
  jit_addi (j->jit, SP, SP, sizeof (union scm_vm_stack_element));
  emit_store_sp (j);

  clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);

  if (j->frame_size >= 0)
    j->frame_size--;
}

static void
compile_drop (scm_jit_state *j, uint32_t nvalues)
{
  jit_addi (j->jit, SP, SP, nvalues * sizeof (union scm_vm_stack_element));
  emit_store_sp (j);

  clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);

  if (j->frame_size >= 0)
    j->frame_size -= nvalues;
}

static void
compile_assert_nargs_ee_locals (scm_jit_state *j, uint16_t expected,
                                uint16_t nlocals)
{
  compile_assert_nargs_ee (j, expected);
  if (nlocals)
    compile_alloc_frame (j, expected + nlocals);
}

static void
compile_expand_apply_argument (scm_jit_state *j)
{
  emit_store_current_ip (j, T0);
  emit_call_r (j, scm_vm_intrinsics.expand_apply_argument, THREAD);
  emit_reload_sp (j);
  emit_reload_fp (j);

  j->frame_size = -1;
}

static void
compile_bind_kwargs (scm_jit_state *j, uint32_t nreq, uint8_t flags,
                     uint32_t nreq_and_opt, uint32_t ntotal, const void *kw)
{
  uint8_t allow_other_keys = flags & 0x1, has_rest = flags & 0x2;
  jit_gpr_t t = T0, npositional = T1;

  emit_store_current_ip (j, t);

  {
    const jit_arg_abi_t abi[] =
      { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_UINT32, JIT_ARG_ABI_UINT32 };
    jit_arg_t args[] =
      { { JIT_ARG_LOC_GPR, { .gpr = THREAD } },
        { JIT_ARG_LOC_IMM, { .imm = nreq } },
        { JIT_ARG_LOC_IMM, { .imm = nreq_and_opt - nreq } } };

    jit_calli (j->jit, scm_vm_intrinsics.compute_kwargs_npositional,
               3, abi, args);
  }
  clear_scratch_register_state (j);
  jit_retval_i (j->jit, npositional);

  {
    const jit_arg_abi_t abi[] =
      { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_UINT32, JIT_ARG_ABI_UINT32,
        JIT_ARG_ABI_POINTER, JIT_ARG_ABI_UINT8, JIT_ARG_ABI_UINT8 };
    jit_arg_t args[] =
      { { JIT_ARG_LOC_GPR, { .gpr = THREAD } },
        { JIT_ARG_LOC_GPR, { .gpr = npositional } },
        { JIT_ARG_LOC_IMM, { .imm = ntotal } },
        { JIT_ARG_LOC_IMM, { .imm = (intptr_t)kw } },
        { JIT_ARG_LOC_IMM, { .imm = !has_rest } },
        { JIT_ARG_LOC_IMM, { .imm = allow_other_keys } } };

    jit_calli (j->jit, scm_vm_intrinsics.bind_kwargs, 6, abi, args);
  }
  clear_scratch_register_state (j);
  
  if (has_rest)
    {
      emit_call_r_i (j, scm_vm_intrinsics.cons_rest, THREAD, ntotal);
      jit_retval (j->jit, t);
      emit_reload_fp (j);
      emit_fp_set_scm (j, nreq_and_opt, t);
    }
  else
    emit_reload_fp (j);

  emit_reset_frame (j, ntotal);
  j->frame_size = ntotal;
}

static void
compile_bind_rest (scm_jit_state *j, uint32_t dst)
{
  jit_reloc_t k, cons;
  jit_gpr_t t = T1;
  
  cons = emit_branch_if_frame_locals_count_greater_than (j, t, dst);

  compile_alloc_frame (j, dst + 1);
  emit_movi (j, t, SCM_UNPACK (SCM_EOL));
  emit_sp_set_scm (j, 0, t);
  k = jit_jmp (j->jit);

  jit_patch_here (j->jit, cons);
  emit_store_current_ip (j, t);
  emit_call_r_i (j, scm_vm_intrinsics.cons_rest, THREAD, dst);
  emit_retval (j, t);
  compile_reset_frame (j, dst + 1);
  emit_sp_set_scm (j, 0, t);
  
  jit_patch_here (j->jit, k);
}

static void
compile_allocate_words (scm_jit_state *j, uint16_t dst, uint16_t nwords)
{
  jit_gpr_t t = T0;

  emit_store_current_ip (j, t);
  emit_sp_ref_sz (j, t, nwords);
  emit_call_r_r (j, scm_vm_intrinsics.allocate_words, THREAD, t);
  emit_retval (j, t);
  record_gpr_clobber (j, t);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, t);
}

static void
compile_allocate_words_immediate (scm_jit_state *j, uint16_t dst, uint16_t nwords)
{
  jit_gpr_t t = T0;

  emit_store_current_ip (j, t);
  emit_movi (j, t, nwords);
  emit_call_r_r (j, scm_vm_intrinsics.allocate_words, THREAD, t);
  emit_retval (j, t);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, t);
}

static void
compile_scm_ref (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, idx);
  emit_lshi (j, T1, T1, log2_sizeof_uintptr_t);
  emit_ldxr (j, T0, T0, T1);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_scm_set (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, idx);
  emit_sp_ref_scm (j, T2, val);
  emit_lshi (j, T1, T1, log2_sizeof_uintptr_t);
  jit_stxr (j->jit, T0, T1, T2);
}

static void
compile_scm_ref_tag (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t tag)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_ldr (j, T0, T0);
  emit_subi (j, T0, T0, tag);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_scm_set_tag (scm_jit_state *j, uint8_t obj, uint8_t tag, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, val);
  emit_addi (j, T1, T1, tag);
  jit_str (j->jit, T0, T1);
}

static void
compile_scm_ref_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_ldxi (j, T0, T0, idx * sizeof (SCM));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_scm_set_immediate (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, val);
  jit_stxi (j->jit, idx * sizeof (SCM), T0, T1);
}

static void
compile_word_ref (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, idx);
  emit_lshi (j, T1, T1, log2_sizeof_uintptr_t);
  emit_ldxr (j, T0, T0, T1);
  emit_sp_set_sz (j, dst, T0);
}

static void
compile_word_set (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, idx);
  emit_sp_ref_sz (j, T2, val);
  emit_lshi (j, T1, T1, log2_sizeof_uintptr_t);
  jit_stxr (j->jit, T0, T1, T2);
}

static void
compile_word_ref_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_ldxi (j, T0, T0, idx * sizeof (SCM));
  emit_sp_set_sz (j, dst, T0);
}

static void
compile_word_set_immediate (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_sz (j, T1, val);
  jit_stxi (j->jit, idx * sizeof (SCM), T0, T1);
}

static void
compile_pointer_ref_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_ldxi (j, T0, T0, idx * sizeof (SCM));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_pointer_set_immediate (scm_jit_state *j, uint8_t obj, uint8_t idx, uint8_t val)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, val);
  jit_stxi (j->jit, idx * sizeof (SCM), T0, T1);
}

static void
compile_tail_pointer_ref_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t idx)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_addi (j, T0, T0, idx * sizeof (SCM));
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
  jit_gpr_t t = T0;
  restore_reloadable_register_state (j, FP_IN_REGISTER);
  emit_fp_ref_scm (j, t, src);
  emit_fp_set_scm (j, dst, t);
}

static void
compile_call_scm_from_scm_scm (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];
  int has_fast = 0;
  jit_reloc_t fast;

  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);

  switch ((enum scm_vm_intrinsic) idx)
    {
    case SCM_VM_INTRINSIC_ADD:
      {
        jit_reloc_t a_not_inum = jit_bmci (j->jit, T0, scm_tc2_int);
        jit_reloc_t b_not_inum = jit_bmci (j->jit, T1, scm_tc2_int);
        jit_subi (j->jit, T0, T0, scm_tc2_int);
        fast = jit_bxaddr (j->jit, T0, T1);
        has_fast = 1;
        /* Restore previous value before slow path.  */
        jit_subr (j->jit, T0, T0, T1);
        jit_addi (j->jit, T0, T0, scm_tc2_int);
        jit_patch_here (j->jit, a_not_inum);
        jit_patch_here (j->jit, b_not_inum);
        break;
      }
    case SCM_VM_INTRINSIC_SUB:
      {
        jit_reloc_t a_not_inum = jit_bmci (j->jit, T0, scm_tc2_int);
        jit_reloc_t b_not_inum = jit_bmci (j->jit, T1, scm_tc2_int);
        jit_subi (j->jit, T1, T1, scm_tc2_int);
        fast = jit_bxsubr (j->jit, T0, T1);
        has_fast = 1;
        /* Restore previous values before slow path.  */
        jit_addr (j->jit, T0, T0, T1);
        jit_addi (j->jit, T1, T1, scm_tc2_int);
        jit_patch_here (j->jit, a_not_inum);
        jit_patch_here (j->jit, b_not_inum);
        break;
      }
    default:
      break;
    }

  emit_store_current_ip (j, T2);
  emit_call_r_r (j, intrinsic, T0, T1);
  emit_retval (j, T0);
  emit_reload_sp (j);

  if (has_fast)
    jit_patch_here (j->jit, fast);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_scm_from_scm_uimm (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];
  int has_fast = 0;
  jit_reloc_t fast;

  emit_sp_ref_scm (j, T0, a);

  switch ((enum scm_vm_intrinsic) idx)
    {
    case SCM_VM_INTRINSIC_ADD_IMMEDIATE:
      {
        scm_t_bits addend = b << 2;
        jit_reloc_t not_inum = jit_bmci (j->jit, T0, 2);
        fast = jit_bxaddi (j->jit, T0, addend);
        has_fast = 1;
        /* Restore previous value before slow path.  */
        jit_subi (j->jit, T0, T0, addend);
        jit_patch_here (j->jit, not_inum);
        break;
      }
    case SCM_VM_INTRINSIC_SUB_IMMEDIATE:
      {
        scm_t_bits subtrahend = b << 2;
        jit_reloc_t not_inum = jit_bmci (j->jit, T0, 2);
        fast = jit_bxsubi (j->jit, T0, subtrahend);
        has_fast = 1;
        /* Restore previous value before slow path.  */
        jit_addi (j->jit, T0, T0, subtrahend);
        jit_patch_here (j->jit, not_inum);
        break;
      }
    default:
      break;
    }

  emit_store_current_ip (j, T1);
  emit_call_r_i (j, intrinsic, T0, b);
  emit_retval (j, T0);
  emit_reload_sp (j);

  if (has_fast)
    jit_patch_here (j->jit, fast);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_scm_sz_u32 (scm_jit_state *j, uint8_t a, uint8_t b, uint8_t c, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);

  const jit_arg_abi_t abi[] =
    { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_INTMAX, JIT_ARG_ABI_UINT32 };
  jit_arg_t args[] = { sp_scm_operand (j, a),
                       sp_sz_operand (j, b),
                       sp_sz_operand (j, c) };
  jit_calli (j->jit, intrinsic, 3, abi, args);
  clear_scratch_register_state (j);
  emit_reload_sp (j);
}

static void
compile_call_scm_from_scm (scm_jit_state *j, uint16_t dst, uint16_t a, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER };
  jit_arg_t args[] = { sp_scm_operand (j, a) };
  jit_calli (j->jit, intrinsic, 1, abi, args);
  clear_scratch_register_state (j);
  emit_retval (j, T0);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_f64_from_scm (scm_jit_state *j, uint16_t dst, uint16_t a, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER };
  jit_arg_t args[] = { sp_scm_operand (j, a) };
  jit_calli (j->jit, intrinsic, 1, abi, args);
  clear_scratch_register_state (j);
  emit_retval_d (j, JIT_F0);
  emit_reload_sp (j);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_call_u64_from_scm (scm_jit_state *j, uint16_t dst, uint16_t a, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
#if INDIRECT_INT64_INTRINSICS
  // FIXME
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER };
  jit_arg_t args[] = { sp_u64_loc_operand (j, dst), sp_scm_operand (j, a) };
  jit_calli (j->jit, intrinsic, 2, abi, args);
  clear_scratch_register_state (j);
  emit_reload_sp (j);
#else
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER };
  jit_arg_t args[] = { sp_scm_operand (j, a) };
  jit_calli (j->jit, intrinsic, 1, abi, args);
  clear_scratch_register_state (j);
  emit_retval (j, T0);
  emit_reload_sp (j);
  emit_sp_set_u64 (j, dst, T0);
#endif
}

static void
compile_make_short_immediate (scm_jit_state *j, uint8_t dst, SCM a)
{
  emit_movi (j, T0, SCM_UNPACK (a));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_make_long_immediate (scm_jit_state *j, uint32_t dst, SCM a)
{
  emit_movi (j, T0, SCM_UNPACK (a));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_make_long_long_immediate (scm_jit_state *j, uint32_t dst, SCM a)
{
  emit_movi (j, T0, SCM_UNPACK (a));
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_make_non_immediate (scm_jit_state *j, uint32_t dst, const void *data)
{
  emit_movi (j, T0, (uintptr_t)data);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_static_ref (scm_jit_state *j, uint32_t dst, void *loc)
{
  emit_ldi (j, T0, loc);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_static_set (scm_jit_state *j, uint32_t obj, void *loc)
{
  emit_sp_ref_scm (j, T0, obj);
  jit_sti (j->jit, loc, T0);
}

static void
compile_static_patch (scm_jit_state *j, void *dst, const void *src)
{
  emit_movi (j, T0, (uintptr_t) src);
  jit_sti (j->jit, dst, T0);
}

static void
compile_prompt (scm_jit_state *j, uint32_t tag, uint8_t escape_only_p,
                uint32_t proc_slot, const uint32_t *vcode)
{
  emit_store_current_ip (j, T0);
  const jit_arg_abi_t abi[] =
    { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_UINT8, JIT_ARG_ABI_POINTER,
      JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER };

  emit_reload_fp (j);
  jit_subi (j->jit, FP, FP, proc_slot * sizeof (union scm_vm_stack_element));
  jit_reloc_t mra = emit_mov_addr (j, T2);

  jit_arg_t args[] = { { JIT_ARG_LOC_GPR, { .gpr = THREAD } },
                       { JIT_ARG_LOC_IMM, { .imm = escape_only_p } },
                       sp_scm_operand (j, tag),
                       { JIT_ARG_LOC_GPR, { .gpr = FP } },
                       { JIT_ARG_LOC_IMM, { .imm = (uintptr_t)vcode } },
                       { JIT_ARG_LOC_GPR, { .gpr = T2 } } };
  jit_calli (j->jit, scm_vm_intrinsics.push_prompt, 6, abi, args);
  clear_scratch_register_state (j);
  emit_reload_sp (j);
  emit_reload_fp (j);
  add_inter_instruction_patch (j, mra, vcode);
}

static void
compile_load_label (scm_jit_state *j, uint32_t dst, const uint32_t *vcode)
{
  emit_movi (j, T0, (uintptr_t) vcode);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_movi (j, T1, 0);
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
#if INDIRECT_INT64_INTRINSICS
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER };
  // jit_addi (j->jit, T0, SP, src * sizeof (union scm_vm_stack_element));
  jit_arg_t args[] = { sp_u64_loc_operand (j, dst) };
#else
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_UINT64 };
  jit_arg_t args[] = { sp_u64_operand (j, dst) };
#endif
  jit_calli (j->jit, intrinsic, 1, abi, args);
  clear_scratch_register_state (j);
  emit_retval (j, T0);
  emit_reload_sp (j);
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
  emit_lshi (j, T0, T0, 8);
  emit_addi (j, T0, T0, scm_tc8_char);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_untag_char (scm_jit_state *j, uint16_t dst, uint16_t src)
{
  emit_sp_ref_scm (j, T0, src);
  emit_rshi (j, T0, T0, 8);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_movi (j, T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_atomic_scm_ref_immediate (scm_jit_state *j, uint8_t dst, uint8_t obj, uint8_t offset)
{
  emit_sp_ref_scm (j, T0, obj);
#if defined(__i386__) || defined(__x86_64__)
  /* Disassembly of atomic_ref_scm is just a mov.  */
  emit_ldxi (j, T0, T0, offset * sizeof (SCM));
#else
  emit_addi (j, T0, T0, offset * sizeof (SCM));
  emit_movr (j, T1_PRESERVED, SP);
  emit_call_r (j, scm_vm_intrinsics.atomic_ref_scm, T0);
  emit_retval (j, T0);
  emit_movr (j, SP, T1_PRESERVED);
  set_register_state (j, SP_IN_REGISTER);
#endif
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_atomic_scm_set_immediate (scm_jit_state *j, uint8_t obj, uint8_t offset, uint8_t val)
{
  emit_sp_ref_scm (j, T1, obj);
  emit_sp_ref_scm (j, T2, val);
  emit_addi (j, T1, T1, offset * sizeof (SCM));
  emit_movr (j, T0_PRESERVED, SP);
  emit_call_r_r (j, scm_vm_intrinsics.atomic_set_scm, T1, T2);
  emit_movr (j, SP, T0_PRESERVED);
  set_register_state (j, SP_IN_REGISTER);
}

static void
compile_atomic_scm_swap_immediate (scm_jit_state *j, uint32_t dst, uint32_t obj, uint8_t offset, uint32_t val)
{
  emit_sp_ref_scm (j, T1, obj);
  emit_sp_ref_scm (j, T2, val);
  emit_addi (j, T1, T1, offset * sizeof (SCM));
  emit_movr (j, T0_PRESERVED, SP);
  emit_call_r_r (j, scm_vm_intrinsics.atomic_swap_scm, T1, T2);
  emit_retval (j, T1);
  emit_movr (j, SP, T0_PRESERVED);
  set_register_state (j, SP_IN_REGISTER);
  emit_sp_set_scm (j, dst, T1);
}

static void
compile_atomic_scm_compare_and_swap_immediate (scm_jit_state *j, uint32_t dst,
                                               uint32_t obj, uint8_t offset,
                                               uint32_t expected, uint32_t desired)
{
  emit_sp_ref_scm (j, T0, obj);
  emit_sp_ref_scm (j, T1, expected);
  emit_sp_ref_scm (j, T2, desired);
  emit_addi (j, T0, T0, offset * sizeof (SCM));
  emit_call_r_r_r (j, scm_vm_intrinsics.atomic_compare_and_swap_scm, T0, T1, T2);
  emit_retval (j, T0);
  emit_reload_sp (j);
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
  emit_retval (j, T0);
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
#if INDIRECT_INT64_INTRINSICS
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER };
  // jit_addi (j->jit, T0, SP, src * sizeof (union scm_vm_stack_element));
  jit_arg_t args[] = { sp_scm_operand (j, a), sp_u64_loc_operand (j, b) };
#else
  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_UINT64 };
  jit_arg_t args[] = { sp_scm_operand (j, a), sp_u64_operand (j, b) };
#endif
  jit_calli (j->jit, intrinsic, 2, abi, args);
  clear_scratch_register_state (j);
  emit_retval (j, T0);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_call_scm_from_thread (scm_jit_state *j, uint32_t dst, uint32_t idx)
{
  void *intrinsic = ((void **) &scm_vm_intrinsics)[idx];

  emit_store_current_ip (j, T0);
  emit_call_r (j, intrinsic, THREAD);
  emit_retval (j, T0);
  emit_reload_sp (j);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_fadd (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  emit_addr_d (j, JIT_F0, JIT_F0, JIT_F1);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_fsub (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  emit_subr_d (j, JIT_F0, JIT_F0, JIT_F1);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_fmul (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  emit_mulr_d (j, JIT_F0, JIT_F0, JIT_F1);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_fdiv (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  emit_divr_d (j, JIT_F0, JIT_F0, JIT_F1);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_uadd (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  emit_addr (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  emit_addcr (j, T0, T0, T2);
  emit_addxr (j, T1, T1, T3_OR_FP);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_usub (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  emit_subr (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  emit_subcr (j, T0, T0, T2);
  emit_subxr (j, T1, T1, T3_OR_FP);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_umul (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  emit_mulr (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: This is untested!  */
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  emit_mulr (j, T1, T1, T2);      /* High A times low B */
  emit_mulr (j, T3_OR_FP, T3_OR_FP, T0); /* High B times low A */
  emit_addr (j, T1, T1, T3_OR_FP); /* Add high results, throw away overflow */
  emit_qmulr_u (j, T0, T2, T0, T2); /* Low A times low B */
  emit_addr (j, T1, T1, T2);        /* Add high result of low product */
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_uadd_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_addi (j, T0, T0, b);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_addci (j, T0, T0, b);
  emit_addxi (j, T1, T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_usub_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_subi (j, T0, T0, b);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_subci (j, T0, T0, b);
  emit_subxi (j, T1, T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_umul_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_muli (j, T0, T0, b);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: This is untested!  */
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_muli (j, T1, T1, b);         /* High A times low B */
  /* High B times low A is 0.  */
  emit_movi (j, T2, b);
  emit_qmulr_u (j, T0, T2, T0, T2); /* Low A times low B */
  emit_addr (j, T1, T1, T2);        /* Add high result of low product */
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_load_f64 (scm_jit_state *j, uint32_t dst, double a)
{
  jit_movi_d (j->jit, JIT_F0, a);
  record_fpr_clobber (j, JIT_F0);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_load_u64 (scm_jit_state *j, uint32_t dst, uint64_t a)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_movi (j, T0, a);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_movi (j, T0, a & 0xffffffff);
  emit_movi (j, T1, a >> 32);
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
  emit_ldxi (j, T0, THREAD, thread_offset_handle);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_ulogand (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  emit_andr (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  emit_andr (j, T0, T0, T2);
  emit_andr (j, T1, T1, T3_OR_FP);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ulogior (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  emit_orr (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  emit_orr (j, T0, T0, T2);
  emit_orr (j, T1, T1, T3_OR_FP);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ulogsub (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  emit_comr (j, T1, T1);
  emit_andr (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  emit_comr (j, T2, T2);
  emit_comr (j, T3_OR_FP, T3_OR_FP);
  emit_andr (j, T0, T0, T2);
  emit_andr (j, T1, T1, T3_OR_FP);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ursh (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  emit_andi (j, T1, T1, 63);
  emit_rshr_u (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  jit_reloc_t zero, both, done;

  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  emit_andi (j, T2, T2, 63);
  zero = jit_beqi (j->jit, T2, 0);
  both = jit_blti (j->jit, T2, 32);

  /* 32 <= s < 64: hi = 0, lo = hi >> (s-32) */
  emit_subi (j, T2, T2, 32);
  emit_rshr_u (j, T0, T1, T2);
  emit_movi (j, T1, 0);
  done = jit_jmp (j->jit);

  jit_patch_here (j->jit, both);
  /* 0 < s < 32: hi = hi >> s, lo = lo >> s + hi << (32-s) */
  emit_negr (j, T3_OR_FP, T2);
  emit_addi (j, T3_OR_FP, T3_OR_FP, 32);
  emit_lshr (j, T3_OR_FP, T1, T3_OR_FP);
  emit_rshr_u (j, T1, T1, T2);
  emit_rshr_u (j, T0, T0, T2);
  emit_addr (j, T0, T0, T3_OR_FP);

  jit_patch_here (j->jit, done);
  jit_patch_here (j->jit, zero);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ulsh (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  emit_andi (j, T1, T1, 63);
  emit_lshr (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  jit_reloc_t zero, both, done;

  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  emit_andi (j, T2, T2, 63);
  zero = jit_beqi (j->jit, T2, 0);
  both = jit_blti (j->jit, T2, 32);

  /* 32 <= s < 64: hi = lo << (s-32), lo = 0 */
  emit_subi (j, T2, T2, 32);
  emit_lshr (j, T1, T0, T2);
  emit_movi (j, T0, 0);
  done = jit_jmp (j->jit);

  jit_patch_here (j->jit, both);
  /* 0 < s < 32: hi = hi << s + lo >> (32-s), lo = lo << s */
  emit_negr (j, T3_OR_FP, T2);
  emit_addi (j, T3_OR_FP, T3_OR_FP, 32);
  emit_rshr_u (j, T3_OR_FP, T0, T3_OR_FP);
  emit_lshr (j, T1, T1, T2);
  emit_lshr (j, T0, T0, T2);
  emit_addr (j, T1, T1, T3_OR_FP);

  jit_patch_here (j->jit, done);
  jit_patch_here (j->jit, zero);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_ursh_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  b &= 63;

#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T0, a);
  emit_rshi_u (j, T0, T0, b);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  emit_sp_ref_u64 (j, T0, T1, a);
  if (b == 0)
    {
      /* Nothing to do.  */
    }
  else if (b < 32)
    {
      /* 0 < s < 32: hi = hi >> s, lo = lo >> s + hi << (32-s) */
      emit_lshi (j, T2, T1, 32 - b);
      emit_rshi_u (j, T1, T1, b);
      emit_rshi_u (j, T0, T0, b);
      emit_addr (j, T0, T0, T2);
    }
  else if (b == 32)
    {
      /*  hi = 0, lo = hi */
      emit_movi (j, T0, T1);
      emit_movi (j, T1, 0);
    }
  else /* b > 32 */
    {
      /*  hi = 0, lo = hi >> (s-32) */
      emit_rshi_u (j, T0, T1, b - 32);
      emit_movi (j, T1, 0);
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
  emit_lshi (j, T0, T0, b);
  emit_sp_set_u64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  emit_sp_ref_u64 (j, T0, T1, a);
  if (b == 0)
    {
      /* Nothing to do.  */
    }
  else if (b < 32)
    {
      /* hi = hi << s + lo >> (32-s), lo = lo << s */
      emit_rshi_u (j, T2, T0, 32 - b);
      emit_lshi (j, T1, T1, b);
      emit_lshi (j, T0, T0, b);
      emit_addr (j, T1, T1, T2);
    }
  else if (b == 32)
    {
      /*  hi = lo, lo = 0 */
      emit_movi (j, T1, T0);
      emit_movi (j, T0, 0);
    }
  else /* b > 32 */
    {
      /* hi = lo << (s-32), lo = 0 */
      emit_lshr (j, T1, T0, b - 32);
      emit_movi (j, T0, 0);
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
  emit_xorr (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  emit_xorr (j, T0, T0, T2);
  emit_xorr (j, T1, T1, T3_OR_FP);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_handle_interrupts (scm_jit_state *j)
{
  uint32_t saved_state = save_reloadable_register_state (j);

  /* This instruction invalidates SP_CACHE_GPR / SP_CACHE_FPR.  */

  void *again = jit_address (j->jit);

#if defined(__i386__) || defined(__x86_64__)
  /* Disassembly of atomic_ref_scm is just a mov.  */
  jit_ldxi (j->jit, T0, THREAD, thread_offset_pending_asyncs);
#else
  jit_addi (j->jit, T0, THREAD, thread_offset_pending_asyncs);
  emit_call_r (j, scm_vm_intrinsics.atomic_ref_scm, T0);
  emit_retval (j, T0);
  restore_reloadable_register_state (j, saved_state);
#endif
  jit_reloc_t none_pending = jit_beqi (j->jit, T0, SCM_UNPACK (SCM_EOL));
  jit_ldxi_i (j->jit, T0, THREAD, thread_offset_block_asyncs);
  jit_reloc_t blocked = jit_bnei (j->jit, T0, 0);

  emit_store_current_ip (j, T0);
  emit_movi (j, T0, (uintptr_t)again);
  jit_jmpi (j->jit, handle_interrupts_trampoline);

  jit_patch_here (j->jit, none_pending);
  jit_patch_here (j->jit, blocked);
  j->register_state = saved_state;
}

static void
compile_return_from_interrupt (scm_jit_state *j)
{
  jit_gpr_t old_fp = T0, ra = T1;
  jit_reloc_t interp;

  emit_pop_fp (j, old_fp);

  emit_load_mra (j, ra, old_fp);
  interp = jit_beqi (j->jit, ra, 0);
  jit_addi (j->jit, SP, old_fp, frame_overhead_slots * sizeof (union scm_vm_stack_element));
  set_register_state (j, SP_IN_REGISTER);
  emit_store_sp (j);
  jit_jmpr (j->jit, ra);

  jit_patch_here (j->jit, interp);
  emit_load_vra (j, ra, old_fp);
  emit_store_ip (j, ra);
  jit_addi (j->jit, SP, old_fp, frame_overhead_slots * sizeof (union scm_vm_stack_element));
  set_register_state (j, SP_IN_REGISTER);
  emit_store_sp (j);
  emit_exit (j);

  clear_register_state (j, SP_CACHE_GPR | SP_CACHE_FPR);
}

static enum scm_opcode
fuse_conditional_branch (scm_jit_state *j, uint32_t **target)
{
  uint8_t next = j->next_ip[0] & 0xff;

  switch (next)
    {
    case scm_op_jl:
    case scm_op_je:
    case scm_op_jnl:
    case scm_op_jne:
    case scm_op_jge:
    case scm_op_jnge:
      *target = j->next_ip + (((int32_t) j->next_ip[0]) >> 8);
      j->next_ip += op_lengths[next];
      return next;
    default:
      ASSERT (0);
    }
}

static void
compile_u64_numerically_equal (scm_jit_state *j, uint16_t a, uint16_t b)
{
  uint32_t *target;
#if SIZEOF_UINTPTR_T >= 8
  jit_reloc_t k;
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k = jit_beqr (j->jit, T0, T1);
      break;
    case scm_op_jne:
      k = jit_bner (j->jit, T0, T1);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
#else
  jit_reloc_t k1, k2;
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k1 = jit_bner (j->jit, T0, T2);
      k2 = jit_beqr (j->jit, T1, T3_OR_FP);
      jit_patch_here (j->jit, k1);
      add_inter_instruction_patch (j, k2, target);
      break;
    case scm_op_jne:
      k1 = jit_bner (j->jit, T0, T2);
      k2 = jit_bner (j->jit, T1, T3_OR_FP);
      add_inter_instruction_patch (j, k1, target);
      add_inter_instruction_patch (j, k2, target);
      break;
    default:
      UNREACHABLE ();
    }
#endif
}

static void
compile_u64_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
  uint32_t *target;
#if SIZEOF_UINTPTR_T >= 8
  jit_reloc_t k;
  emit_sp_ref_u64 (j, T0, a);
  emit_sp_ref_u64 (j, T1, b);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k = jit_bltr_u (j->jit, T0, T1);
      break;
    case scm_op_jnl:
      k = jit_bger_u (j->jit, T0, T1);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
#else
  jit_reloc_t k1, k2, k3;
  emit_sp_ref_u64 (j, T0, T1, a);
  emit_sp_ref_u64 (j, T2, T3_OR_FP, b);
  k1 = jit_bltr_u (j->jit, T1, T3_OR_FP);
  k2 = jit_bner (j->jit, T1, T3_OR_FP);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k3 = jit_bltr_u (j->jit, T0, T2);
      jit_patch_here (j->jit, k2);
      add_inter_instruction_patch (j, k1, target);
      add_inter_instruction_patch (j, k3, target);
      break;
    case scm_op_jnl:
      k3 = jit_bger_u (j->jit, T0, T2);
      jit_patch_here (j->jit, k1);
      add_inter_instruction_patch (j, k2, target);
      add_inter_instruction_patch (j, k3, target);
      break;
    default:
      UNREACHABLE ();
    }
#endif
}

static void
compile_s64_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
  uint32_t *target;
#if SIZEOF_UINTPTR_T >= 8
  jit_reloc_t k;
  emit_sp_ref_s64 (j, T0, a);
  emit_sp_ref_s64 (j, T1, b);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k = jit_bltr (j->jit, T0, T1);
      break;
    case scm_op_jnl:
      k = jit_bger (j->jit, T0, T1);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
#else
  jit_reloc_t k1, k2, k3;
  emit_sp_ref_s64 (j, T0, T1, a);
  emit_sp_ref_s64 (j, T2, T3_OR_FP, b);
  k1 = jit_bltr (j->jit, T1, T3_OR_FP);
  k2 = jit_bner (j->jit, T1, T3_OR_FP);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k3 = jit_bltr (j->jit, T0, T2);
      jit_patch_here (j->jit, k2);
      add_inter_instruction_patch (j, k1, target);
      add_inter_instruction_patch (j, k3, target);
      break;
    case scm_op_jnl:
      k3 = jit_bger (j->jit, T0, T2);
      jit_patch_here (j->jit, k1);
      add_inter_instruction_patch (j, k2, target);
      add_inter_instruction_patch (j, k3, target);
      break;
    default:
      UNREACHABLE ();
    }
#endif
}

static void
compile_f64_numerically_equal (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k = jit_beqr_d (j->jit, JIT_F0, JIT_F1);
      break;
    case scm_op_jne:
      k = jit_bner_d (j->jit, JIT_F0, JIT_F1);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
}

static void
compile_f64_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_f64 (j, JIT_F0, a);
  emit_sp_ref_f64 (j, JIT_F1, b);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k = jit_bltr_d (j->jit, JIT_F0, JIT_F1);
      break;
    case scm_op_jnl:
      k = jit_bunger_d (j->jit, JIT_F0, JIT_F1);
      break;
    case scm_op_jge:
      k = jit_bger_d (j->jit, JIT_F0, JIT_F1);
      break;
    case scm_op_jnge:
      k = jit_bunltr_d (j->jit, JIT_F0, JIT_F1);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
}

static void
compile_numerically_equal (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_reloc_t k;
  uint32_t *target;

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);
  emit_call_r_r (j, scm_vm_intrinsics.numerically_equal_p, T0, T1);
  emit_retval (j, T0);
  emit_reload_sp (j);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k = jit_bnei (j->jit, T0, 0);
      break;
    case scm_op_jne:
      k = jit_beqi (j->jit, T0, 0);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
}

static void
compile_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
#if 0
  jit_reloc_t fast, k2, k3;
#endif
  jit_reloc_t k1;
  uint32_t *target;
  enum scm_opcode op = fuse_conditional_branch (j, &target);

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);

#if 0
  emit_andr (j, T2, T0, T1);
  fast = jit_bmsi (j->jit, T2, scm_tc2_int);
#endif

  emit_call_r_r (j, scm_vm_intrinsics.less_p, T0, T1);
  emit_retval (j, T0);
  emit_reload_sp (j);
  switch (op)
    {
    case scm_op_jl:
      k1 = jit_beqi (j->jit, T0, SCM_F_COMPARE_LESS_THAN);
      break;
    case scm_op_jnl:
      k1 = jit_bnei (j->jit, T0, SCM_F_COMPARE_LESS_THAN);
      break;
    case scm_op_jge:
      k1 = jit_beqi (j->jit, T0, SCM_F_COMPARE_NONE);
      break;
    case scm_op_jnge:
      k1 = jit_bnei (j->jit, T0, SCM_F_COMPARE_NONE);
      break;
    default:
      UNREACHABLE ();
    }
#if 0
  k2 = jit_jmp (j->jit);

  jit_patch_here (j->jit, fast);
  switch (op)
    {
    case scm_op_jl:
    case scm_op_jnge:
      k3 = jit_bltr (j->jit, T0, T1);
      break;
    case scm_op_jnl:
    case scm_op_jge:
      k3 = jit_bger (j->jit, T0, T1);
      break;
    default:
      UNREACHABLE ();
    }

  jit_patch_here (j->jit, k2);
#endif

  add_inter_instruction_patch (j, k1, target);
#if 0
  add_inter_instruction_patch (j, k3, target);
#endif
}

static void
compile_check_arguments (scm_jit_state *j, uint32_t expected)
{
  jit_reloc_t k;
  uint32_t *target;
  jit_gpr_t t = T0;
  
  emit_reload_fp (j);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jne:
      k = emit_branch_if_frame_locals_count_not_eq (j, t, expected);
      break;
    case scm_op_jl:
      k = emit_branch_if_frame_locals_count_less_than (j, t, expected);
      break;
    case scm_op_jge:
      /* The arguments<=? instruction sets NONE to indicate
         greater-than, whereas for <, NONE usually indicates
         greater-than-or-equal, hence the name jge.  So we need to fuse
         to greater-than, not greater-than-or-equal.  Perhaps we just
         need to rename jge to br-if-none.  */
      k = emit_branch_if_frame_locals_count_greater_than (j, t, expected);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
}

static void
compile_check_positional_arguments (scm_jit_state *j, uint32_t nreq, uint32_t expected)
{
  uint32_t *target;
  jit_reloc_t lt, gt;
  jit_gpr_t walk = T0, min = T1, obj = T2;

  ASSERT_HAS_REGISTER_STATE (FP_IN_REGISTER | SP_IN_REGISTER);

  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jge:
      /* Like arguments<=?, this instruction sets NONE to indicate
         greater-than, whereas for <, NONE usually indicates
         greater-than-or-equal, hence the name jge.  So we need to fuse
         to greater-than, not greater-than-or-equal.  Perhaps we just
         need to rename jge to br-if-none.  */
      /* Break to target if npos > expected.  */
      break;
    default:
      UNREACHABLE ();
    }

  emit_subtract_stack_slots (j, min, FP, expected);
  emit_subtract_stack_slots (j, walk, FP, nreq);
  
  void *head = jit_address (j->jit);
  /* npos > expected if walk < min.  */
  gt = jit_bltr (j->jit, walk, min);
  emit_subtract_stack_slots (j, walk, walk, 1);
  lt = jit_bltr (j->jit, walk, SP);
  emit_ldr (j, obj, walk);
  jit_patch_there
    (j->jit,
     emit_branch_if_immediate (j, obj),
     head);
  jit_patch_there
    (j->jit,
     emit_branch_if_heap_object_not_tc7 (j, obj, obj, scm_tc7_keyword),
     head);
  jit_patch_here (j->jit, lt);
  add_inter_instruction_patch (j, gt, target);
}

static void
compile_immediate_tag_equals (scm_jit_state *j, uint32_t a, uint16_t mask,
                              uint16_t expected)
{
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_scm (j, T0, a);
  emit_andi (j, T0, T0, mask);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k = jit_beqi (j->jit, T0, expected);
      break;
    case scm_op_jne:
      k = jit_bnei (j->jit, T0, expected);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
}

static void
compile_heap_tag_equals (scm_jit_state *j, uint32_t obj,
                         uint16_t mask, uint16_t expected)
{
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_scm (j, T0, obj);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k = emit_branch_if_heap_object_has_tc (j, T0, T0, mask, expected);
      break;
    case scm_op_jne:
      k = emit_branch_if_heap_object_not_tc (j, T0, T0, mask, expected);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
}

static void
compile_eq (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k = jit_beqr (j->jit, T0, T1);
      break;
    case scm_op_jne:
      k = jit_bner (j->jit, T0, T1);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
}

static void
compile_j (scm_jit_state *j, const uint32_t *vcode)
{
  jit_reloc_t jmp;
  jmp = jit_jmp (j->jit);
  add_inter_instruction_patch (j, jmp, vcode);
}

static void
compile_jl (scm_jit_state *j, const uint32_t *vcode)
{
  UNREACHABLE (); /* All tests should fuse their following branches.  */
}

static void
compile_je (scm_jit_state *j, const uint32_t *vcode)
{
  UNREACHABLE (); /* All tests should fuse their following branches.  */
}

static void
compile_jnl (scm_jit_state *j, const uint32_t *vcode)
{
  UNREACHABLE (); /* All tests should fuse their following branches.  */
}

static void
compile_jne (scm_jit_state *j, const uint32_t *vcode)
{
  UNREACHABLE (); /* All tests should fuse their following branches.  */
}

static void
compile_jge (scm_jit_state *j, const uint32_t *vcode)
{
  UNREACHABLE (); /* All tests should fuse their following branches.  */
}

static void
compile_jnge (scm_jit_state *j, const uint32_t *vcode)
{
  UNREACHABLE (); /* All tests should fuse their following branches.  */
}

static void
compile_heap_numbers_equal (scm_jit_state *j, uint16_t a, uint16_t b)
{
  jit_reloc_t k;
  uint32_t *target;

  emit_store_current_ip (j, T0);
  emit_sp_ref_scm (j, T0, a);
  emit_sp_ref_scm (j, T1, b);
  emit_call_r_r (j, scm_vm_intrinsics.heap_numbers_equal_p, T0, T1);
  emit_retval (j, T0);
  emit_reload_sp (j);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k = jit_bnei (j->jit, T0, 0);
      break;
    case scm_op_jne:
      k = jit_beqi (j->jit, T0, 0);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
}

static void
compile_untag_fixnum (scm_jit_state *j, uint16_t dst, uint16_t a)
{
  emit_sp_ref_scm (j, T0, a);
  emit_rshi (j, T0, T0, 2);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_s64 (j, dst, T0);
#else
  /* FIXME: Untested!  */
  emit_rshi (j, T1, T0, 31);
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
  emit_lshi (j, T0, T0, 2);
  emit_addi (j, T0, T0, scm_tc2_int);
  emit_sp_set_scm (j, dst, T0);
}

static void
compile_srsh (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_s64 (j, T0, a);
  emit_sp_ref_s64 (j, T1, b);
  emit_andi (j, T1, T1, 63);
  emit_rshr (j, T0, T0, T1);
  emit_sp_set_s64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  jit_reloc_t zero, both, done;

  emit_sp_ref_s64 (j, T0, T1, a);
  emit_sp_ref_s64 (j, T2, T3_OR_FP, b);
  emit_andi (j, T2, T2, 63);
  zero = jit_beqi (j->jit, T2, 0);
  both = jit_blti (j->jit, T2, 32);

  /* 32 <= s < 64: hi = hi >> 31, lo = hi >> (s-32) */
  emit_subi (j, T2, T2, 32);
  emit_rshr (j, T0, T1, T2);
  emit_rshi (j, T1, T1, 31);
  done = jit_jmp (j->jit);

  jit_patch_here (j->jit, both);
  /* 0 < s < 32: hi = hi >> s, lo = lo >> s + hi << (32-s) */
  emit_negr (j, T3_OR_FP, T2);
  emit_addi (j, T3_OR_FP, T3_OR_FP, 32);
  emit_lshr (j, T3_OR_FP, T1, T3_OR_FP);
  emit_rshr (j, T1, T1, T2);
  emit_rshr_u (j, T0, T0, T2);
  emit_addr (j, T0, T0, T3_OR_FP);

  jit_patch_here (j->jit, done);
  jit_patch_here (j->jit, zero);
  emit_sp_set_s64 (j, dst, T0, T1);
#endif
}

static void
compile_srsh_immediate (scm_jit_state *j, uint8_t dst, uint8_t a, uint8_t b)
{
  b &= 63;

#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_s64 (j, T0, a);
  emit_rshi (j, T0, T0, b);
  emit_sp_set_s64 (j, dst, T0);
#else
  /* FIXME: Not tested.  */
  emit_sp_ref_s64 (j, T0, T1, a);
  if (b == 0)
    {
      /* Nothing to do.  */
    }
  else if (b < 32)
    {
      /* 0 < s < 32: hi = hi >> s, lo = lo >> s + hi << (32-s) */
      emit_lshi (j, T2, T1, 32 - b);
      emit_rshi (j, T1, T1, b);
      emit_rshi_u (j, T0, T0, b);
      emit_addr (j, T0, T0, T2);
    }
  else if (b == 32)
    {
      /*  hi = sign-ext, lo = hi */
      emit_movr (j, T0, T1);
      emit_rshi (j, T1, T1, 31);
    }
  else /* b > 32 */
    {
      /*  hi = sign-ext, lo = hi >> (s-32) */
      emit_rshi (j, T0, T1, b - 32);
      emit_rshi (j, T1, T1, 31);
    }
  emit_sp_set_s64 (j, dst, T0, T1);
#endif
}

static void
compile_s64_imm_numerically_equal (scm_jit_state *j, uint16_t a, int16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_s64 (j, T0, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k = jit_beqi (j->jit, T0, b);
      break;
    case scm_op_jne:
      k = jit_bnei (j->jit, T0, b);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
#else
  jit_reloc_t k1, k2;
  uint32_t *target;

  emit_sp_ref_s64 (j, T0, T1, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_je:
      k1 = jit_bnei (j->jit, T0, b);
      k2 = jit_beqi (j->jit, T1, b < 0 ? -1 : 0);
      jit_patch_here (j->jit, k1);
      add_inter_instruction_patch (j, k2, target);
      break;
    case scm_op_jne:
      k1 = jit_bnei (j->jit, T0, b);
      k2 = jit_bnei (j->jit, T1, b < 0 ? -1 : 0);
      add_inter_instruction_patch (j, k1, target);
      add_inter_instruction_patch (j, k2, target);
      break;
    default:
      UNREACHABLE ();
    }
#endif
}

static void
compile_u64_imm_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_u64 (j, T0, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k = jit_blti_u (j->jit, T0, b);
      break;
    case scm_op_jnl:
      k = jit_bgei_u (j->jit, T0, b);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
#else
  jit_reloc_t k1, k2;
  uint32_t *target;

  emit_sp_ref_u64 (j, T0, T1, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k1 = jit_bnei (j->jit, T1, 0);
      k2 = jit_blti_u (j->jit, T0, b);
      jit_patch_here (j->jit, k1);
      add_inter_instruction_patch (j, k2, target);
      break;
    case scm_op_jnl:
      k1 = jit_bnei (j->jit, T1, 0);
      k2 = jit_bgei_u (j->jit, T0, b);
      add_inter_instruction_patch (j, k1, target);
      add_inter_instruction_patch (j, k2, target);
      break;
    default:
      UNREACHABLE ();
    }
#endif
}

static void
compile_imm_u64_less (scm_jit_state *j, uint16_t a, uint16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_u64 (j, T0, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k = jit_bgti_u (j->jit, T0, b);
      break;
    case scm_op_jnl:
      k = jit_blei_u (j->jit, T0, b);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
#else
  jit_reloc_t k1, k2;
  uint32_t *target;

  emit_sp_ref_u64 (j, T0, T1, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k1 = jit_bnei (j->jit, T1, 0);
      k2 = jit_bgti_u (j->jit, T0, b);
      add_inter_instruction_patch (j, k1, target);
      add_inter_instruction_patch (j, k2, target);
      break;
    case scm_op_jnl:
      k1 = jit_bnei (j->jit, T1, 0);
      k2 = jit_blei_u (j->jit, T0, b);
      jit_patch_here (j->jit, k1);
      add_inter_instruction_patch (j, k2, target);
      break;
    default:
      UNREACHABLE ();
    }
#endif
}

static void
compile_s64_imm_less (scm_jit_state *j, uint16_t a, int16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_s64 (j, T0, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k = jit_blti (j->jit, T0, b);
      break;
    case scm_op_jnl:
      k = jit_bgei (j->jit, T0, b);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
#else
  jit_reloc_t k1, k2, k3;
  int32_t sign = b < 0 ? -1 : 0;
  uint32_t *target;

  emit_sp_ref_s64 (j, T0, T1, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k1 = jit_blti (j->jit, T1, sign);
      k2 = jit_bnei (j->jit, T1, sign);
      k3 = jit_blti (j->jit, T0, b);
      add_inter_instruction_patch (j, k1, target);
      jit_patch_here (j->jit, k2);
      add_inter_instruction_patch (j, k3, target);
      break;
    case scm_op_jnl:
      k1 = jit_blti (j->jit, T1, sign);
      k2 = jit_bnei (j->jit, T1, sign);
      k3 = jit_bgei (j->jit, T0, b);
      jit_patch_here (j->jit, k1);
      add_inter_instruction_patch (j, k2, target);
      add_inter_instruction_patch (j, k3, target);
      break;
    default:
      UNREACHABLE ();
    }
#endif
}

static void
compile_imm_s64_less (scm_jit_state *j, uint16_t a, int16_t b)
{
#if SIZEOF_UINTPTR_T >= 8
  jit_reloc_t k;
  uint32_t *target;

  emit_sp_ref_s64 (j, T0, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k = jit_bgti (j->jit, T0, b);
      break;
    case scm_op_jnl:
      k = jit_blei (j->jit, T0, b);
      break;
    default:
      UNREACHABLE ();
    }
  add_inter_instruction_patch (j, k, target);
#else
  jit_reloc_t k1, k2, k3;
  int32_t sign = b < 0 ? -1 : 0;
  uint32_t *target;

  emit_sp_ref_s64 (j, T0, T1, a);
  switch (fuse_conditional_branch (j, &target))
    {
    case scm_op_jl:
      k1 = jit_blti (j->jit, T1, sign);
      k2 = jit_bnei (j->jit, T1, sign);
      k3 = jit_bgti (j->jit, T0, b);
      jit_patch_here (j->jit, k1);
      add_inter_instruction_patch (j, k2, target);
      add_inter_instruction_patch (j, k3, target);
      break;
    case scm_op_jnl:
      k1 = jit_blti (j->jit, T1, sign);
      k2 = jit_bnei (j->jit, T1, sign);
      k3 = jit_blei (j->jit, T0, b);
      add_inter_instruction_patch (j, k1, target);
      jit_patch_here (j->jit, k2);
      add_inter_instruction_patch (j, k3, target);
      break;
    default:
      UNREACHABLE ();
    }
#endif
}

static void
compile_u8_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_uc (j->jit, T0, T0, T1);
  record_gpr_clobber (j, T0);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_movi (j, T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_u16_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_us (j->jit, T0, T0, T1);
  record_gpr_clobber (j, T0);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_movi (j, T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_u32_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  jit_ldxr_ui (j->jit, T0, T0, T1);
  record_gpr_clobber (j, T0);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_ldxr (j, T0, T0, T1);
  emit_movi (j, T1, 0);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_u64_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  emit_ldxr (j, T0, T0, T1);
  emit_sp_set_u64 (j, dst, T0);
#else
  emit_addr (j, T0, T0, T1);
  if (BIGENDIAN)
    {
      emit_ldxi (j, T1, T0, 4);
      emit_ldr (j, T0, T0);
    }
  else
    {
      emit_ldr (j, T1, T0);
      emit_ldxi (j, T0, T0, 4);
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
  jit_stxr_c (j->jit, T0, T1, T2);
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
  jit_stxr_s (j->jit, T0, T1, T2);
}

static void
compile_u32_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T2, v);
  jit_stxr_i (j->jit, T0, T1, T2);
#else
  emit_sp_ref_u64_lower_half (j, T2, v);
  jit_stxr (j->jit, T0, T1, T2);
#endif
}

static void
compile_u64_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_ref_u64 (j, T2, v);
  jit_stxr (j->jit, T0, T1, T2);
#else
  jit_addr (j->jit, T0, T0, T1);
  emit_sp_ref_u64 (j, T1, T2, v);
  if (BIGENDIAN)
    {
      jit_str (j->jit, T0, T2);
      jit_stxi (j->jit, 4, T0, T1);
    }
  else
    {
      jit_str (j->jit, T0, T1);
      jit_stxi (j->jit, 4, T0, T2);
    }
#endif
}

static void
compile_s8_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_c (j->jit, T0, T0, T1);
  record_gpr_clobber (j, T0);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_s64 (j, dst, T0);
#else
  emit_rshi (j, T1, T0, 7);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_s16_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_s (j->jit, T0, T0, T1);
  record_gpr_clobber (j, T0);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_s64 (j, dst, T0);
#else
  emit_rshi (j, T1, T0, 15);
  emit_sp_set_u64 (j, dst, T0, T1);
#endif
}

static void
compile_s32_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_i (j->jit, T0, T0, T1);
  record_gpr_clobber (j, T0);
#if SIZEOF_UINTPTR_T >= 8
  emit_sp_set_s64 (j, dst, T0);
#else
  emit_rshi (j, T1, T0, 31);
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
  jit_ldxr_f (j->jit, JIT_F0, T0, T1);
  record_fpr_clobber (j, JIT_F0);
  jit_extr_f_d (j->jit, JIT_F0, JIT_F0);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_f64_ref (scm_jit_state *j, uint8_t dst, uint8_t ptr, uint8_t idx)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  jit_ldxr_d (j->jit, JIT_F0, T0, T1);
  record_fpr_clobber (j, JIT_F0);
  emit_sp_set_f64 (j, dst, JIT_F0);
}

static void
compile_f32_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  emit_sp_ref_f64 (j, JIT_F0, v);
  jit_extr_d_f (j->jit, JIT_F0, JIT_F0);
  record_fpr_clobber (j, JIT_F0);
  jit_stxr_f (j->jit, T0, T1, JIT_F0);
}

static void
compile_f64_set (scm_jit_state *j, uint8_t ptr, uint8_t idx, uint8_t v)
{
  emit_sp_ref_ptr (j, T0, ptr);
  emit_sp_ref_sz (j, T1, idx);
  emit_sp_ref_f64 (j, JIT_F0, v);
  jit_stxr_d (j->jit, T0, T1, JIT_F0);
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
    bad_instruction (j);              \
  }

#define COMPILE_X32(j, comp)                                            \
  {                                                                     \
    comp (j);                                                           \
  }

#define COMPILE_X8_C24(j, comp)                                         \
  {                                                                     \
    uint32_t a;                                                         \
    UNPACK_24 (j->ip[0], a);                                            \
    comp (j, a);                                                        \
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
  }
#define COMPILE_X8_C12_C12(j, comp)                                     \
  {                                                                     \
    uint16_t a, b;                                                      \
    UNPACK_12_12 (j->ip[0], a, b);                                      \
    comp (j, a, b);                                                     \
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
  }

#define COMPILE_X8_S8_C8_S8(j, comp)                                    \
  {                                                                     \
    uint8_t a, b, c;                                                    \
    UNPACK_8_8_8 (j->ip[0], a, b, c);                                   \
    comp (j, a, b, c);                                                  \
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
  }

#define COMPILE_X32__C32(j, comp)                                       \
  {                                                                     \
    comp (j, j->ip[1]);                                                 \
  }

#define COMPILE_X32__L32(j, comp)                                       \
  {                                                                     \
    int32_t a = j->ip[1];                                               \
    comp (j, j->ip + a);                                                \
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
  }

#define COMPILE_X8_F24__B1_X7_C24(j, comp)                              \
  {                                                                     \
    uint32_t a, c;                                                      \
    uint8_t b;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    b = j->ip[1] & 0x1;                                                 \
    UNPACK_24 (j->ip[1], c);                                            \
    comp (j, a, b, c);                                                  \
  }

#define COMPILE_X8_S12_S12__C32(j, comp)                                \
  {                                                                     \
    uint16_t a, b;                                                      \
    uint32_t c;                                                         \
    UNPACK_12_12 (j->ip[0], a, b);                                      \
    c = j->ip[1];                                                       \
    comp (j, a, b, c);                                                  \
  }

#define COMPILE_X8_S24__C16_C16(j, comp)                                \
  {                                                                     \
    uint32_t a;                                                         \
    uint16_t b, c;                                                      \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_16_16 (j->ip[1], b, c);                                      \
    comp (j, a, b, c);                                                  \
  }

#define COMPILE_X8_S24__C32(j, comp)                                    \
  {                                                                     \
    uint32_t a, b;                                                      \
    UNPACK_24 (j->ip[0], a);                                            \
    b = j->ip[1];                                                       \
    comp (j, a, b);                                                     \
  }

#define COMPILE_X8_S24__I32(j, comp)                                    \
  {                                                                     \
    uint32_t a;                                                         \
    scm_t_bits b;                                                       \
    UNPACK_24 (j->ip[0], a);                                            \
    b = j->ip[1];                                                       \
    comp (j, a, SCM_PACK (b));                                          \
  }

#define COMPILE_X8_S8_S8_C8__C32(j, comp)                               \
  {                                                                     \
    uint8_t a, b, c;                                                    \
    uint32_t d;                                                         \
    UNPACK_8_8_8 (j->ip[0], a, b, c);                                   \
    d = j->ip[1];                                                       \
    comp (j, a, b, c, d);                                               \
  }
#define COMPILE_X8_S8_S8_S8__C32(j, comp)                               \
  COMPILE_X8_S8_S8_C8__C32(j, comp)

#define COMPILE_X32__LO32__L32(j, comp)                                 \
  {                                                                     \
    int32_t a = j->ip[1], b = j->ip[2];                                 \
    comp (j, j->ip + a, j->ip + b);                                     \
  }

#define COMPILE_X8_F24__X8_C24__L32(j, comp)                            \
  {                                                                     \
    uint32_t a, b;                                                      \
    int32_t c;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_24 (j->ip[1], b);                                            \
    c = j->ip[2];                                                       \
    comp (j, a, b, j->ip + c);                                          \
  }

#define COMPILE_X8_S24__A32__B32(j, comp)                               \
  {                                                                     \
    uint32_t a;                                                         \
    uint64_t b;                                                         \
    UNPACK_24 (j->ip[0], a);                                            \
    b = (((uint64_t) j->ip[1]) << 32) | ((uint64_t) j->ip[2]);          \
    ASSERT (b <= (uint64_t) UINTPTR_MAX);                               \
    comp (j, a, SCM_PACK ((uintptr_t) b));                              \
  }

#define COMPILE_X8_S24__AF32__BF32(j, comp)                             \
  {                                                                     \
    uint32_t a;                                                         \
    union { uint64_t u; double d; } b;                                  \
    UNPACK_24 (j->ip[0], a);                                            \
    b.u = (((uint64_t) j->ip[1]) << 32) | ((uint64_t) j->ip[2]);        \
    comp (j, a, b.d);                                                   \
  }

#define COMPILE_X8_S24__AS32__BS32(j, comp)                             \
  {                                                                     \
    uint32_t a;                                                         \
    uint64_t b;                                                         \
    UNPACK_24 (j->ip[0], a);                                            \
    b = (((uint64_t) j->ip[1]) << 32) | ((uint64_t) j->ip[2]);          \
    comp (j, a, (int64_t) b);                                           \
  }

#define COMPILE_X8_S24__AU32__BU32(j, comp)                             \
  {                                                                     \
    uint32_t a;                                                         \
    uint64_t b;                                                         \
    UNPACK_24 (j->ip[0], a);                                            \
    b = (((uint64_t) j->ip[1]) << 32) | ((uint64_t) j->ip[2]);          \
    comp (j, a, b);                                                     \
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
  }

#define COMPILE_X8_S24__X8_S24__C8_S24(j, comp)                         \
  {                                                                     \
    uint32_t a, b, d;                                                   \
    uint8_t c;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_24 (j->ip[1], b);                                            \
    UNPACK_8_24 (j->ip[2], c, d);                                       \
    comp (j, a, b, c, d);                                               \
  }

#define COMPILE_X8_C24__C8_C24__X8_C24__N32(j, comp)                    \
  {                                                                     \
    uint32_t a, c, d;                                                   \
    uint8_t b;                                                          \
    int32_t e;                                                          \
    UNPACK_24 (j->ip[0], a);                                            \
    UNPACK_8_24 (j->ip[1], b, c);                                       \
    UNPACK_24 (j->ip[2], d);                                            \
    e = j->ip[3];                                                       \
    comp (j, a, b, c, d, j->ip + e);                                    \
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
  }

static uint8_t first_seen[256];

static void
compile1 (scm_jit_state *j)
{
  uint8_t opcode = j->ip[0] & 0xff;

  if (!first_seen[opcode])
    {
      const char *n;
      switch (opcode)
        {
#define NAME(code, cname, name, arity) case code: n = name; break;
          FOR_EACH_VM_OPERATION(NAME)
#undef NAME
        default:
          UNREACHABLE ();
        }
      first_seen[opcode] = 1;
      DEBUG ("Instruction first seen at vcode %p: %s\n", j->ip, n);
    }

  j->next_ip = j->ip + op_lengths[opcode];

  switch (opcode)
    {
#define COMPILE1(code, cname, name, arity) \
      case code: COMPILE_##arity(j, compile_##cname); break;
      FOR_EACH_VM_OPERATION(COMPILE1)
#undef COMPILE1
    default:
      UNREACHABLE ();
    }

  j->ip = j->next_ip;
}

static void
analyze (scm_jit_state *j)
{
  memset (j->op_attrs, 0, j->end - j->start);

  j->op_attrs[0] = OP_ATTR_BLOCK | OP_ATTR_ENTRY;

  for (j->ip = (uint32_t *) j->start; j->ip < j->end; j->ip = j->next_ip)
    {
      uint8_t opcode = j->ip[0] & 0xff;
      uint8_t attrs = 0;
      uint32_t *target;

      j->next_ip = j->ip + op_lengths[opcode];

      switch (opcode)
        {
        case scm_op_check_arguments:
        case scm_op_check_positional_arguments:
          attrs |= OP_ATTR_ENTRY;
          /* Fall through. */
        case scm_op_u64_numerically_equal:
        case scm_op_u64_less:
        case scm_op_s64_less:
        case scm_op_f64_numerically_equal:
        case scm_op_f64_less:
        case scm_op_numerically_equal:
        case scm_op_less:
        case scm_op_immediate_tag_equals:
        case scm_op_heap_tag_equals:
        case scm_op_eq:
        case scm_op_heap_numbers_equal:
        case scm_op_s64_imm_numerically_equal:
        case scm_op_u64_imm_less:
        case scm_op_imm_u64_less:
        case scm_op_s64_imm_less:
        case scm_op_imm_s64_less:
          attrs |= OP_ATTR_BLOCK;
          fuse_conditional_branch (j, &target);
          j->op_attrs[target - j->start] |= attrs;
          break;

        case scm_op_j:
          target = j->ip + (((int32_t)j->ip[0]) >> 8);
          j->op_attrs[target - j->start] |= OP_ATTR_BLOCK;
          break;

        case scm_op_call:
        case scm_op_call_label:
          attrs = OP_ATTR_BLOCK;
          target = j->next_ip;
          j->op_attrs[target - j->start] |= OP_ATTR_BLOCK | OP_ATTR_ENTRY;
          break;

        case scm_op_prompt:
          target = j->ip + (((int32_t) j->ip[2]) >> 8);
          j->op_attrs[target - j->start] |= OP_ATTR_BLOCK | OP_ATTR_ENTRY;
          break;

        default:
          break;
        }
    }

  /* Even in loops, the entry should be a jump target.  */
  ASSERT (j->op_attrs[j->entry - j->start] & OP_ATTR_BLOCK);
}

static void
compile (scm_jit_state *j)
{
  j->ip = (uint32_t *) j->start;
  set_register_state (j, SP_IN_REGISTER | FP_IN_REGISTER);

  for (ptrdiff_t offset = 0; j->ip + offset < j->end; offset++)
    j->labels[offset] = NULL;

  j->reloc_idx = 0;

  while (j->ip < j->end)
    {
      ptrdiff_t offset = j->ip - j->start;
      uint8_t attrs = j->op_attrs[offset];
      j->labels[offset] = jit_address (j->jit);
      if (attrs & OP_ATTR_BLOCK)
        {
          uint32_t state = SP_IN_REGISTER;
          if (attrs & OP_ATTR_ENTRY)
            state |= FP_IN_REGISTER;
          j->register_state = state;
        }
      compile1 (j);

      if (jit_has_overflow (j->jit))
        return;
    }

  for (size_t i = 0; i < j->reloc_idx; i++)
    {
      void *target = j->labels[j->relocs[i].target_vcode_offset];
      ASSERT(target);
      jit_patch_there (j->jit, j->relocs[i].reloc, target);
    }
}

static scm_i_pthread_once_t initialize_jit_once = SCM_I_PTHREAD_ONCE_INIT;

static void*
jit_alloc_fn (size_t size)
{
  return scm_gc_malloc_pointerless (size, "jit state");
}

static void
jit_free_fn (void *unused)
{
}

static scm_jit_state *
initialize_thread_jit_state (scm_thread *thread)
{
  scm_jit_state *j;

  ASSERT (!thread->jit_state);

  j = scm_gc_malloc (sizeof (*j), "jit state");
  memset (j, 0, sizeof (*j));
  thread->jit_state = j;
  j->jit = jit_new_state (jit_alloc_fn, jit_free_fn);

  return j;
}

static void
initialize_jit (void)
{
  scm_jit_state *j;

  if (!init_jit ())
    {
      scm_jit_counter_threshold = -1;
      fprintf (stderr, "JIT failed to initialize\n");
      fprintf (stderr, "disabling automatic JIT compilation\n");
      return;
    }

  /* Init the thread's jit state so we can emit the entry
     trampoline and the handle-interrupts trampoline.  */
  j = initialize_thread_jit_state (SCM_I_CURRENT_THREAD);

  enter_mcode = emit_code (j, emit_entry_trampoline);
  ASSERT (enter_mcode);

  handle_interrupts_trampoline =
    emit_code (j, emit_handle_interrupts_trampoline);
  ASSERT (handle_interrupts_trampoline);
}

static uint8_t *
compute_mcode (scm_thread *thread, uint32_t *entry_ip,
               struct scm_jit_function_data *data)
{
  scm_jit_state *j = thread->jit_state;
  uint8_t *entry_mcode;

  if (!j)
    {
      scm_i_pthread_once (&initialize_jit_once, initialize_jit);
      if (scm_jit_counter_threshold == -1)
        {
          /* initialization failed!  */
          return NULL;
        }

      j = thread->jit_state;
      /* It's possible that initialize_jit_once inits this thread's jit
         state.  */
      if (!j)
        j = initialize_thread_jit_state (thread);
    }

  j->thread = thread;
  j->start = (const uint32_t *) (((char *)data) + data->start);
  j->end = (const uint32_t *) (((char *)data) + data->end);
  j->entry = entry_ip;

  ASSERT (j->start < j->end);
  ASSERT (j->start <= j->entry);
  ASSERT (j->entry < j->end);

  j->op_attrs = calloc ((j->end - j->start), sizeof (*j->op_attrs));
  ASSERT (j->op_attrs);
  j->labels = calloc ((j->end - j->start), sizeof (*j->labels));
  ASSERT (j->labels);

  j->frame_size = -1;

  INFO ("vcode: start=%p,+%zu entry=+%zu\n", j->start, j->end - j->start,
        j->entry - j->start);

  analyze (j);

  data->mcode = emit_code (j, compile);
  if (data->mcode)
    entry_mcode = j->labels[j->entry - j->start];
  else
    entry_mcode = NULL;

  free (j->op_attrs);
  j->op_attrs = NULL;
  free (j->labels);
  j->labels = NULL;
  free (j->relocs);
  j->relocs = NULL;
  j->reloc_idx = 0;
  j->reloc_count = 0;

  j->start = j->end = j->ip = j->entry = NULL;
  j->frame_size = -1;

  return entry_mcode;
}

const uint8_t *
scm_jit_compute_mcode (scm_thread *thread, struct scm_jit_function_data *data)
{
  const uint32_t *vcode_start = (const uint32_t *) (((char *)data) + data->start);

  if (data->mcode)
    {
      if (vcode_start == thread->vm.ip)
        return data->mcode;

      /* FIXME: The function has mcode, compiled via some other
         activation (possibly in another thread), but right now we're
         currently in an interpreted loop (not at the beginning of the
         function).  We should re-compute the offset into the mcode.
         For now though, just punt.  */
      return NULL;
    }
  else
    {
      uint8_t *mcode = compute_mcode (thread, thread->vm.ip, data);

      if (!mcode)
        {
          scm_jit_counter_threshold = -1;
          fprintf (stderr, "JIT failed due to resource exhaustion\n");
          fprintf (stderr, "disabling automatic JIT compilation\n");
        }
      else if (--jit_stop_after == 0)
        {
          scm_jit_counter_threshold = -1;
          fprintf (stderr, "stopping automatic JIT compilation, as requested\n");
          if (jit_pause_when_stopping)
            {
              fprintf (stderr, "sleeping for 30s; to debug:\n");
              fprintf (stderr, "   gdb -p %d\n\n", getpid ());
              sleep (30);
            }
        }

      return mcode;
    }
}

void
scm_jit_enter_mcode (scm_thread *thread, const uint8_t *mcode)
{
  LOG ("entering mcode: %p\n", mcode);
  enter_mcode (thread, mcode);
  LOG ("exited mcode\n");
}

void
scm_jit_state_free (scm_jit_state *j)
{
  /* Nothing to do; we leave j->jit NULL between compilations.  */
}

void
scm_init_jit (void)
{
  scm_jit_counter_threshold = scm_getenv_int ("GUILE_JIT_THRESHOLD",
                                              default_jit_threshold);
  jit_stop_after = scm_getenv_int ("GUILE_JIT_STOP_AFTER", -1);
  jit_pause_when_stopping = scm_getenv_int ("GUILE_JIT_PAUSE_WHEN_STOPPING", 0);
  jit_log_level = scm_getenv_int ("GUILE_JIT_LOG", 0);
}

#endif /* ENABLE_JIT */
