/* Copyright 2001,2009-2015,2017-2018
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

/* This file is included in vm.c multiple times.  */


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

/* Assign some registers by hand.  There used to be a bigger list here,
   but it was never tested, and in the case of x86-32, was a source of
   compilation failures.  It can be revived if it's useful, but my naive
   hope is that simply annotating the locals with "register" will be a
   sufficient hint to the compiler.  */
#ifdef __GNUC__
# if defined __x86_64__
/* GCC 4.6 chooses %rbp for IP_REG and %rbx for SP_REG, which works
   well.  Tell it to keep the jump table in a r12, which is
   callee-saved.  */
#  define JT_REG asm ("r12")
# endif
#endif

#ifndef IP_REG
# define IP_REG
#endif
#ifndef FP_REG
# define FP_REG
#endif
#ifndef JT_REG
# define JT_REG
#endif

#define VP (&thread->vm)

#define VM_ASSERT(condition, handler)     \
  do {                                    \
    if (SCM_UNLIKELY (!(condition)))      \
      {                                   \
        SYNC_IP();                        \
        handler;                          \
      }                                   \
  } while (0)

#ifdef VM_ENABLE_ASSERTIONS
# define ASSERT(condition) VM_ASSERT (condition, abort())
#else
# define ASSERT(condition)
#endif

#if VM_USE_HOOKS
#define RUN_HOOK(h)                                          \
  do {                                                       \
    if (SCM_UNLIKELY (VP->h##_hook_enabled))                 \
      {                                                      \
        SYNC_IP ();                                          \
        invoke_##h##_hook (thread);                          \
        CACHE_SP ();                                         \
      }                                                      \
  } while (0)
#else
#define RUN_HOOK(h)
#endif

#define APPLY_HOOK()                  RUN_HOOK (apply)
#define RETURN_HOOK()                 RUN_HOOK (return)
#define NEXT_HOOK()                   RUN_HOOK (next)
#define ABORT_HOOK()                  RUN_HOOK (abort)




/* Virtual Machine

   The VM has three state bits: the instruction pointer (IP), the frame
   pointer (FP), and the stack pointer (SP).  We cache the IP in a
   machine register, local to the VM, because it is used extensively by
   the VM.  We do the same for SP.  The FP is used more by code outside
   the VM than by the VM itself, we don't bother caching it locally.

   Keeping VP->ip in sync with the local IP would be a big lose, as it
   is updated so often.  Instead of updating VP->ip all the time, we
   call SYNC_IP whenever we would need to know the IP of the top frame.
   In practice, we need to SYNC_IP whenever we call out of the VM to a
   function that would like to walk the stack, perhaps as the result of
   an exception.  On the other hand, we do always keep VP->sp in sync
   with the local SP.

   One more thing.  We allow the stack to move, when it expands.
   Therefore if you call out to a C procedure that could call Scheme
   code, or otherwise push anything on the stack, you will need to
   CACHE_SP afterwards to restore the possibly-changed stack pointer.  */

#define SYNC_IP() VP->ip = (ip)

#define CACHE_SP() sp = VP->sp
#define CACHE_REGISTER()                        \
  do {                                          \
    ip = VP->ip;                                \
    CACHE_SP ();                                \
  } while (0)


#define CALL_INTRINSIC(x, args) \
  (((struct scm_vm_intrinsics *) (void*) intrinsics)->x args)

/* Reserve stack space for a frame.  Will check that there is sufficient
   stack space for N locals, including the procedure.  Invoke after
   preparing the new frame and setting the fp and ip.

   If there is not enough space for this frame, we try to expand the
   stack, possibly relocating it somewhere else in the address space.
   Because of the possible relocation, no pointer into the stack besides
   FP is valid across an ALLOC_FRAME call.  Be careful!  */
#define ALLOC_FRAME(n)                                              \
  do {                                                              \
    sp = VP->fp - (n);                                              \
    if (sp < VP->sp_min_since_gc)                                   \
      {                                                             \
        if (SCM_UNLIKELY (sp < VP->stack_limit))                    \
          {                                                         \
            SYNC_IP ();                                             \
            CALL_INTRINSIC (expand_stack, (thread, sp));            \
            CACHE_SP ();                                            \
          }                                                         \
        else                                                        \
          VP->sp_min_since_gc = VP->sp = sp;                        \
      }                                                             \
    else                                                            \
      VP->sp = sp;                                                  \
  } while (0)

/* Reset the current frame to hold N locals.  Used when we know that no
   stack expansion is needed.  Note that in some cases this may lower
   SP, e.g. after a return but where there are more locals below, but we
   know it was preceded by an alloc-frame in that case, so no stack need
   be allocated.

   As an optimization, we don't update sp_min_since_gc in this case; the
   principal place stacks are expanded is in ALLOC_FRAME. it doesn't
   need to strictly be the min since GC, as it's just an optimization to
   prevent passing too-large of a range to madvise.  */
#define RESET_FRAME(n)                                              \
  do {                                                              \
    VP->sp = sp = VP->fp - (n);                                     \
  } while (0)

/* Compute the number of locals in the frame.  At a call, this is equal
   to the number of actual arguments when a function is first called,
   plus one for the function.  */
#define FRAME_LOCALS_COUNT() (VP->fp - sp)
#define FRAME_LOCALS_COUNT_FROM(slot) (FRAME_LOCALS_COUNT () - slot)


#ifdef HAVE_LABELS_AS_VALUES
# define BEGIN_DISPATCH_SWITCH /* */
# define END_DISPATCH_SWITCH /* */
# define NEXT(n)                                \
  do                                            \
    {                                           \
      ip += n;                                  \
      NEXT_HOOK ();                             \
      op = *ip;                                 \
      goto *jump_table[op & 0xff];              \
    }                                           \
  while (0)
# define VM_DEFINE_OP(opcode, tag, name, meta)  \
  op_##tag:
#else
# define BEGIN_DISPATCH_SWITCH                  \
  vm_start:                                     \
    NEXT_HOOK ();                               \
    op = *ip;                                   \
  switch (op & 0xff)                            \
    {
# define END_DISPATCH_SWITCH                    \
    }
# define NEXT(n)                                \
  do                                            \
    {                                           \
      ip += n;                                  \
      goto vm_start;                            \
    }                                           \
  while (0)
# define VM_DEFINE_OP(opcode, tag, name, meta)  \
  op_##tag:                                     \
  case opcode:
#endif

#define FP_SLOT(i)	        SCM_FRAME_SLOT (VP->fp, i)
#define FP_REF(i)		SCM_FRAME_LOCAL (VP->fp, i)
#define FP_SET(i,o)		SCM_FRAME_LOCAL (VP->fp, i) = o

#define SP_REF_SLOT(i)		(sp[i])
#define SP_SET_SLOT(i,o)	(sp[i] = o)

#define SP_REF(i)		(sp[i].as_scm)
#define SP_SET(i,o)		(sp[i].as_scm = o)

#define SP_REF_F64(i)		(sp[i].as_f64)
#define SP_SET_F64(i,o)		(sp[i].as_f64 = o)

#define SP_REF_U64(i)		(sp[i].as_u64)
#define SP_SET_U64(i,o)		(sp[i].as_u64 = o)

#define SP_REF_S64(i)		(sp[i].as_s64)
#define SP_SET_S64(i,o)		(sp[i].as_s64 = o)

#define SP_REF_PTR(i)		(sp[i].as_ptr)
#define SP_SET_PTR(i,o)		(sp[i].as_ptr = o)

/* Return true (non-zero) if PTR has suitable alignment for TYPE.  */
#define ALIGNED_P(ptr, type)			\
  ((uintptr_t) (ptr) % alignof_type (type) == 0)

static SCM
VM_NAME (scm_thread *thread)
{
  /* Instruction pointer: A pointer to the opcode that is currently
     running.  */
  register uint32_t *ip IP_REG;

  /* Stack pointer: A pointer to the hot end of the stack, off of which
     we index arguments and local variables.  Pushed at function calls,
     popped on returns.  */
  register union scm_vm_stack_element *sp FP_REG;

  /* Current opcode: A cache of *ip.  */
  register uint32_t op;

  void **intrinsics = (void**) &scm_vm_intrinsics;

#ifdef HAVE_LABELS_AS_VALUES
  static const void *jump_table_[256] = {
#define LABEL_ADDR(opcode, tag, name, meta) &&op_##tag,
      FOR_EACH_VM_OPERATION(LABEL_ADDR)
#undef LABEL_ADDR
  };
  register const void **jump_table JT_REG;
  /* Attempt to keep JUMP_TABLE_POINTER in a register.  This saves one
     load instruction at each instruction dispatch.  */
  jump_table = jump_table_;
#endif

  /* Load VM registers. */
  CACHE_REGISTER ();

  /* Start processing!  */
  NEXT (0);

  BEGIN_DISPATCH_SWITCH;
  

  

  /* halt _:24
   *
   * Bring the VM to a halt, returning all the values from the stack.
   */
  VM_DEFINE_OP (0, halt, "halt", OP1 (X32))
    {
      size_t frame_size = 3;
      /* Empty frame, then values.  */
      size_t first_value = frame_size;
      uint32_t nvals = FRAME_LOCALS_COUNT_FROM (first_value);
      union scm_vm_stack_element *fp;
      SCM ret;

      if (nvals == 1)
        ret = FP_REF (first_value);
      else
        {
          uint32_t n;
          SYNC_IP ();
          VM_ASSERT (nvals <= (UINTPTR_MAX >> 8), abort ());
          ret = scm_words ((nvals << 8) | scm_tc7_values, nvals + 1);
          for (n = 0; n < nvals; n++)
            SCM_SET_CELL_OBJECT (ret, n+1, FP_REF (first_value + n));
        }

      fp = VP->fp;
      VP->fp = SCM_FRAME_DYNAMIC_LINK (fp);
      VP->ip = SCM_FRAME_VIRTUAL_RETURN_ADDRESS (fp);
      VP->sp = SCM_FRAME_PREVIOUS_SP (fp);

      return ret;
    }

  /* instrument-entry _:24 data:32
   *
   * Increase execution counter for this function and potentially tier
   * up to the next JIT level.  DATA is an offset to a structure
   * recording execution counts and the next-level JIT code
   * corresponding to this function.  Also run the apply hook.
   */
  VM_DEFINE_OP (1, instrument_entry, "instrument-entry", OP2 (X32, N32))
    {
#if ENABLE_JIT
      if (!VP->disable_mcode)
        {
          struct scm_jit_function_data *data;

          int32_t data_offset = ip[1];
          data = (struct scm_jit_function_data *) (ip + data_offset);

          if (data->mcode)
            {
              SYNC_IP ();
              scm_jit_enter_mcode (thread, data->mcode);
              CACHE_REGISTER ();
              NEXT (0);
            }

          if (data->counter >= scm_jit_counter_threshold)
            {
              const uint8_t *mcode;

              SYNC_IP ();
              mcode = scm_jit_compute_mcode (thread, data);

              if (mcode)
                {
                  scm_jit_enter_mcode (thread, mcode);
                  CACHE_REGISTER ();
                  NEXT (0);
                }
            }
          else
            data->counter += SCM_JIT_COUNTER_ENTRY_INCREMENT;
        }
#endif

      APPLY_HOOK ();

      NEXT (2);
    }

  /* instrument-loop _:24 data:32
   *
   * Increase execution counter for this function and potentially tier
   * up to the next JIT level.  DATA is an offset to a structure
   * recording execution counts and the next-level JIT code
   * corresponding to this function.
   */
  VM_DEFINE_OP (2, instrument_loop, "instrument-loop", OP2 (X32, N32))
    {
#if ENABLE_JIT
      if (!VP->disable_mcode)
        {
          int32_t data_offset = ip[1];
          struct scm_jit_function_data *data;

          data = (struct scm_jit_function_data *) (ip + data_offset);

          if (data->counter >= scm_jit_counter_threshold)
            {
              const uint8_t *mcode;

              SYNC_IP ();
              mcode = scm_jit_compute_mcode (thread, data);

              if (mcode)
                {
                  scm_jit_enter_mcode (thread, mcode);
                  CACHE_REGISTER ();
                  NEXT (0);
                }
            }
          else
            data->counter += SCM_JIT_COUNTER_LOOP_INCREMENT;
        }
#endif

      NEXT (2);
    }

  /* call proc:24 _:8 nlocals:24
   *
   * Call a procedure.  PROC is the local corresponding to a procedure.
   * The three values below PROC will be overwritten by the saved call
   * frame data.  The new frame will have space for NLOCALS locals: one
   * for the procedure, and the rest for the arguments which should
   * already have been pushed on.
   *
   * When the call returns, execution proceeds with the next
   * instruction.  There may be any number of values on the return
   * stack; the precise number can be had by subtracting the address of
   * slot PROC-1 from the post-call SP.
   */
  VM_DEFINE_OP (3, call, "call", OP2 (X8_F24, X8_C24))
    {
      uint32_t proc, nlocals;
      union scm_vm_stack_element *old_fp, *new_fp;

      UNPACK_24 (op, proc);
      UNPACK_24 (ip[1], nlocals);

      old_fp = VP->fp;
      new_fp = SCM_FRAME_SLOT (old_fp, proc - 1);
      SCM_FRAME_SET_DYNAMIC_LINK (new_fp, old_fp);
      SCM_FRAME_SET_VIRTUAL_RETURN_ADDRESS (new_fp, ip + 2);
      SCM_FRAME_SET_MACHINE_RETURN_ADDRESS (new_fp, 0);
      VP->fp = new_fp;

      RESET_FRAME (nlocals);
      ip = CALL_INTRINSIC (get_callee_vcode, (thread));
      CACHE_SP ();

      NEXT (0);
    }

  /* call-label proc:24 _:8 nlocals:24 label:32
   *
   * Call a procedure in the same compilation unit.
   *
   * This instruction is just like "call", except that instead of
   * dereferencing PROC to find the call target, the call target is
   * known to be at LABEL, a signed 32-bit offset in 32-bit units from
   * the current IP.  Since PROC is not used to compute the callee code,
   * it may be some other representation of the closure.
   */
  VM_DEFINE_OP (4, call_label, "call-label", OP3 (X8_F24, X8_C24, L32))
    {
      uint32_t proc, nlocals;
      int32_t label;
      union scm_vm_stack_element *old_fp, *new_fp;

      UNPACK_24 (op, proc);
      UNPACK_24 (ip[1], nlocals);
      label = ip[2];

      old_fp = VP->fp;
      new_fp = SCM_FRAME_SLOT (old_fp, proc - 1);
      SCM_FRAME_SET_DYNAMIC_LINK (new_fp, old_fp);
      SCM_FRAME_SET_VIRTUAL_RETURN_ADDRESS (new_fp, ip + 3);
      SCM_FRAME_SET_MACHINE_RETURN_ADDRESS (new_fp, 0);
      VP->fp = new_fp;

      RESET_FRAME (nlocals);

      ip += label;

      NEXT (0);
    }

  /* tail-call _:24
   *
   * Tail-call the procedure in slot 0 with the arguments in the current
   * stack frame.  Requires that the procedure and all of the arguments
   * have already been shuffled into position.
   */
  VM_DEFINE_OP (5, tail_call, "tail-call", OP1 (X32))
    {
      ip = CALL_INTRINSIC (get_callee_vcode, (thread));
      CACHE_SP ();
      NEXT (0);
    }

  /* tail-call-label _:24 label:32
   *
   * Tail-call a known procedure.  As call is to call-label, tail-call
   * is to tail-call-label.
   */
  VM_DEFINE_OP (6, tail_call_label, "tail-call-label", OP2 (X32, L32))
    {
      int32_t label;
      
      label = ip[1];

      ip += label;

      NEXT (0);
    }

  /* return-values _:24
   *
   * Return all values from a call frame.
   */
  VM_DEFINE_OP (7, return_values, "return-values", OP1 (X32))
    {
      union scm_vm_stack_element *old_fp;
      uint8_t *mcode;

      RETURN_HOOK ();

      old_fp = VP->fp;
      VP->fp = SCM_FRAME_DYNAMIC_LINK (old_fp);

#if ENABLE_JIT
      if (!VP->disable_mcode)
        {
          mcode = SCM_FRAME_MACHINE_RETURN_ADDRESS (old_fp);
          if (mcode)
            {
              scm_jit_enter_mcode (thread, mcode);
              CACHE_REGISTER ();
              NEXT (0);
            }
        }
#endif

      ip = SCM_FRAME_VIRTUAL_RETURN_ADDRESS (old_fp);
      NEXT (0);
    }

  /* receive dst:12 proc:12 _:8 nlocals:24
   *
   * Receive a single return value from a call whose procedure was in
   * PROC, asserting that the call actually returned at least one
   * value.  Afterwards, resets the frame to NLOCALS locals.
   */
  VM_DEFINE_OP (8, receive, "receive", DOP2 (X8_F12_F12, X8_C24))
    {
      uint16_t dst, proc;
      uint32_t nlocals;
      UNPACK_12_12 (op, dst, proc);
      UNPACK_24 (ip[1], nlocals);
      VM_ASSERT (FRAME_LOCALS_COUNT () > proc,
                 CALL_INTRINSIC (error_no_values, ()));
      FP_SET (dst, FP_REF (proc));
      RESET_FRAME (nlocals);
      NEXT (2);
    }

  /* receive-values proc:24 allow-extra?:1 _:7 nvalues:24
   *
   * Receive a return of multiple values from a call whose procedure was
   * in PROC.  If fewer than NVALUES values were returned, signal an
   * error.  Unless ALLOW-EXTRA? is true, require that the number of
   * return values equals NVALUES exactly.  After receive-values has
   * run, the values can be copied down via `mov'.
   */
  VM_DEFINE_OP (9, receive_values, "receive-values", OP2 (X8_F24, B1_X7_C24))
    {
      uint32_t proc, nvalues;
      UNPACK_24 (op, proc);
      UNPACK_24 (ip[1], nvalues);
      if (ip[1] & 0x1)
        VM_ASSERT (FRAME_LOCALS_COUNT () >= proc + nvalues,
                   CALL_INTRINSIC (error_not_enough_values, ()));
      else
        VM_ASSERT (FRAME_LOCALS_COUNT () == proc + nvalues,
                   CALL_INTRINSIC (error_wrong_number_of_values, (nvalues)));
      NEXT (2);
    }

  /* assert-nargs-ee expected:24
   * assert-nargs-ge expected:24
   * assert-nargs-le expected:24
   *
   * If the number of actual arguments is not ==, >=, or <= EXPECTED,
   * respectively, signal an error.
   */
  VM_DEFINE_OP (10, assert_nargs_ee, "assert-nargs-ee", OP1 (X8_C24))
    {
      uint32_t expected;
      UNPACK_24 (op, expected);
      VM_ASSERT (FRAME_LOCALS_COUNT () == expected,
                 CALL_INTRINSIC (error_wrong_num_args, (thread)));
      NEXT (1);
    }
  VM_DEFINE_OP (11, assert_nargs_ge, "assert-nargs-ge", OP1 (X8_C24))
    {
      uint32_t expected;
      UNPACK_24 (op, expected);
      VM_ASSERT (FRAME_LOCALS_COUNT () >= expected,
                 CALL_INTRINSIC (error_wrong_num_args, (thread)));
      NEXT (1);
    }
  VM_DEFINE_OP (12, assert_nargs_le, "assert-nargs-le", OP1 (X8_C24))
    {
      uint32_t expected;
      UNPACK_24 (op, expected);
      VM_ASSERT (FRAME_LOCALS_COUNT () <= expected,
                 CALL_INTRINSIC (error_wrong_num_args, (thread)));
      NEXT (1);
    }

  /* assert-nargs-ee/locals expected:12 nlocals:12
   *
   * Equivalent to a sequence of assert-nargs-ee and reserve-locals.  The
   * number of locals reserved is EXPECTED + NLOCALS.
   */
  VM_DEFINE_OP (13, assert_nargs_ee_locals, "assert-nargs-ee/locals", OP1 (X8_C12_C12))
    {
      uint16_t expected, nlocals;
      UNPACK_12_12 (op, expected, nlocals);
      VM_ASSERT (FRAME_LOCALS_COUNT () == expected,
                 CALL_INTRINSIC (error_wrong_num_args, (thread)));
      ALLOC_FRAME (expected + nlocals);
      while (nlocals--)
        SP_SET (nlocals, SCM_UNDEFINED);

      NEXT (1);
    }

  /* arguments<=? expected:24
   *
   * Set the LESS_THAN, EQUAL, or NONE comparison result values if the
   * number of arguments is respectively less than, equal to, or greater
   * than EXPECTED.
   */
  VM_DEFINE_OP (14, check_arguments, "arguments<=?", OP1 (X8_C24))
    {
      uint8_t compare_result;
      uint32_t expected;
      ptrdiff_t nargs;

      UNPACK_24 (op, expected);
      nargs = FRAME_LOCALS_COUNT ();

      if (nargs < (ptrdiff_t) expected)
        compare_result = SCM_F_COMPARE_LESS_THAN;
      else if (nargs == (ptrdiff_t) expected)
        compare_result = SCM_F_COMPARE_EQUAL;
      else
        compare_result = SCM_F_COMPARE_NONE;

      VP->compare_result = compare_result;

      NEXT (1);
    }

  /* positional-arguments<=? nreq:24 _:8 expected:24
   *
   * Set the LESS_THAN, EQUAL, or NONE comparison result values if the
   * number of positional arguments is less than, equal to, or greater
   * than EXPECTED.  The first NREQ arguments are positional arguments,
   * as are the subsequent arguments that are not keywords.
   */
  VM_DEFINE_OP (15, check_positional_arguments, "positional-arguments<=?", OP2 (X8_C24, X8_C24))
    {
      uint8_t compare_result;
      uint32_t nreq, expected;
      ptrdiff_t nargs, npos;

      UNPACK_24 (op, nreq);
      UNPACK_24 (ip[1], expected);
      nargs = FRAME_LOCALS_COUNT ();

      /* Precondition: at least NREQ arguments.  */
      for (npos = nreq; npos < nargs && npos <= expected; npos++)
        if (scm_is_keyword (FP_REF (npos)))
          break;

      if (npos < (ptrdiff_t) expected)
        compare_result = SCM_F_COMPARE_LESS_THAN;
      else if (npos == (ptrdiff_t) expected)
        compare_result = SCM_F_COMPARE_EQUAL;
      else
        compare_result = SCM_F_COMPARE_NONE;

      VP->compare_result = compare_result;

      NEXT (2);
    }

  /* bind-kwargs nreq:24 flags:8 nreq-and-opt:24 _:8 ntotal:24 kw-offset:32
   *
   * flags := allow-other-keys:1 has-rest:1 _:6
   *
   * Find the last positional argument, and shuffle all the rest above
   * NTOTAL.  Initialize the intervening locals to SCM_UNDEFINED.  Then
   * load the constant at KW-OFFSET words from the current IP, and use it
   * to bind keyword arguments.  If HAS-REST, collect all shuffled
   * arguments into a list, and store it in NREQ-AND-OPT.  Finally, clear
   * the arguments that we shuffled up.
   *
   * A macro-mega-instruction.
   */
  VM_DEFINE_OP (16, bind_kwargs, "bind-kwargs", OP4 (X8_C24, C8_C24, X8_C24, N32))
    {
      uint32_t nreq, nreq_and_opt, ntotal, npositional;
      int32_t kw_offset;
      scm_t_bits kw_bits;
      SCM kw;
      uint8_t allow_other_keys, has_rest;

      UNPACK_24 (op, nreq);
      allow_other_keys = ip[1] & 0x1;
      has_rest = ip[1] & 0x2;
      UNPACK_24 (ip[1], nreq_and_opt);
      UNPACK_24 (ip[2], ntotal);
      kw_offset = ip[3];
      kw_bits = (scm_t_bits) (ip + kw_offset);
      VM_ASSERT (!(kw_bits & 0x7), abort());
      kw = SCM_PACK (kw_bits);

      /* Note that if nopt == 0 then npositional = nreq.  */
      npositional = CALL_INTRINSIC (compute_kwargs_npositional,
                                    (thread, nreq, nreq_and_opt - nreq));

      SYNC_IP ();
      CALL_INTRINSIC (bind_kwargs,
                      (thread, npositional, ntotal, kw, !has_rest,
                       allow_other_keys));
      CACHE_SP ();

      if (has_rest)
        FP_SET (nreq_and_opt, CALL_INTRINSIC (cons_rest, (thread, ntotal)));

      RESET_FRAME (ntotal);

      NEXT (4);
    }

  /* bind-rest dst:24
   *
   * Collect any arguments at or above DST into a list, and store that
   * list at DST.
   */
  VM_DEFINE_OP (17, bind_rest, "bind-rest", DOP1 (X8_F24))
    {
      uint32_t dst, nargs;
      SCM rest = SCM_EOL;

      UNPACK_24 (op, dst);
      nargs = FRAME_LOCALS_COUNT ();

      if (nargs <= dst)
        {
          ALLOC_FRAME (dst + 1);
          while (nargs < dst)
            FP_SET (nargs++, SCM_UNDEFINED);
        }
      else
        {
          SYNC_IP ();
          rest = CALL_INTRINSIC (cons_rest, (thread, dst));
          RESET_FRAME (dst + 1);
        }

      FP_SET (dst, rest);

      NEXT (1);
    }

  /* alloc-frame nlocals:24
   *
   * Ensure that there is space on the stack for NLOCALS local variables.
   * setting any new stack slots to SCM_UNDEFINED.
   */
  VM_DEFINE_OP (18, alloc_frame, "alloc-frame", OP1 (X8_C24))
    {
      uint32_t nlocals, nargs;
      UNPACK_24 (op, nlocals);

      nargs = FRAME_LOCALS_COUNT ();
      ALLOC_FRAME (nlocals);
      while (nlocals-- > nargs)
        FP_SET (nlocals, SCM_UNDEFINED);

      NEXT (1);
    }

  /* reset-frame nlocals:24
   *
   * Like alloc-frame, but doesn't check that the stack is big enough,
   * and doesn't reset stack slots to SCM_UNDEFINED.  Used to reset the
   * frame size to something less than the size that was previously set
   * via alloc-frame.
   */
  VM_DEFINE_OP (19, reset_frame, "reset-frame", OP1 (X8_C24))
    {
      uint32_t nlocals;
      UNPACK_24 (op, nlocals);
      RESET_FRAME (nlocals);
      NEXT (1);
    }

  /* mov dst:12 src:12
   *
   * Copy a value from one local slot to another.
   */
  VM_DEFINE_OP (20, mov, "mov", DOP1 (X8_S12_S12))
    {
      uint16_t dst;
      uint16_t src;

      UNPACK_12_12 (op, dst, src);
      /* FIXME: The compiler currently emits "mov" for SCM, F64, U64,
         and S64 variables.  However SCM values are the usual case, and
         on a 32-bit machine it might be cheaper to move a SCM than to
         move a 64-bit number.  */
      SP_SET_SLOT (dst, SP_REF_SLOT (src));

      NEXT (1);
    }

  /* long-mov dst:24 _:8 src:24
   *
   * Copy a value from one local slot to another.
   */
  VM_DEFINE_OP (21, long_mov, "long-mov", DOP2 (X8_S24, X8_S24))
    {
      uint32_t dst;
      uint32_t src;

      UNPACK_24 (op, dst);
      UNPACK_24 (ip[1], src);
      /* FIXME: The compiler currently emits "long-mov" for SCM, F64,
         U64, and S64 variables.  However SCM values are the usual case,
         and on a 32-bit machine it might be cheaper to move a SCM than
         to move a 64-bit number.  */
      SP_SET_SLOT (dst, SP_REF_SLOT (src));

      NEXT (2);
    }

  /* long-fmov dst:24 _:8 src:24
   *
   * Copy a value from one local slot to another.  Slot indexes are
   * relative to the FP.
   */
  VM_DEFINE_OP (22, long_fmov, "long-fmov", DOP2 (X8_F24, X8_F24))
    {
      uint32_t dst;
      uint32_t src;

      UNPACK_24 (op, dst);
      UNPACK_24 (ip[1], src);
      FP_SET (dst, FP_REF (src));

      NEXT (2);
    }

  /* push src:24
   *
   * Push SRC onto the stack.
   */
  VM_DEFINE_OP (23, push, "push", OP1 (X8_S24))
    {
      uint32_t src;
      union scm_vm_stack_element val;

      /* FIXME: The compiler currently emits "push" for SCM, F64, U64,
         and S64 variables.  However SCM values are the usual case, and
         on a 32-bit machine it might be cheaper to move a SCM than to
         move a 64-bit number.  */
      UNPACK_24 (op, src);
      val = SP_REF_SLOT (src);
      ALLOC_FRAME (FRAME_LOCALS_COUNT () + 1);
      SP_SET_SLOT (0, val);
      NEXT (1);
    }

  /* pop dst:24
   *
   * Pop the stack, storing to DST.
   */
  VM_DEFINE_OP (24, pop, "pop", DOP1 (X8_S24))
    {
      uint32_t dst;
      union scm_vm_stack_element val;

      /* FIXME: The compiler currently emits "pop" for SCM, F64, U64,
         and S64 variables.  However SCM values are the usual case, and
         on a 32-bit machine it might be cheaper to move a SCM than to
         move a 64-bit number.  */
      UNPACK_24 (op, dst);
      val = SP_REF_SLOT (0);
      VP->sp = sp = sp + 1;
      SP_SET_SLOT (dst, val);
      NEXT (1);
    }

  /* drop count:24
   *
   * Drop some number of values from the stack.
   */
  VM_DEFINE_OP (25, drop, "drop", OP1 (X8_C24))
    {
      uint32_t count;

      UNPACK_24 (op, count);
      VP->sp = sp = sp + count;
      NEXT (1);
    }

  /* shuffle-down from:12 to:12
   *
   * Shuffle down values from FROM to TO, reducing the frame size by
   * (FROM-TO) slots.  Part of the internal implementation of
   * call-with-values, values, and apply.
   */
  VM_DEFINE_OP (26, shuffle_down, "shuffle-down", OP1 (X8_F12_F12))
    {
      uint32_t n, from, to, nlocals;

      UNPACK_12_12 (op, from, to);

      VM_ASSERT (from > to, abort ());
      nlocals = FRAME_LOCALS_COUNT ();

      for (n = 0; from + n < nlocals; n++)
        FP_SET (to + n, FP_REF (from + n));

      RESET_FRAME (to + n);

      NEXT (1);
    }

  /* expand-apply-argument _:24
   *
   * Take the last local in a frame and expand it out onto the stack, as
   * for the last argument to "apply".
   */
  VM_DEFINE_OP (27, expand_apply_argument, "expand-apply-argument", OP1 (X32))
    {
      SYNC_IP ();
      CALL_INTRINSIC (expand_apply_argument, (thread));
      CACHE_SP ();

      NEXT (1);
    }

  /* subr-call idx:24
   *
   * Call a subr, passing all locals in this frame as arguments, and
   * storing the results on the stack, ready to be returned.  This
   * instruction is part of the trampolines created in gsubr.c, and is
   * not generated by the compiler.
   */
  VM_DEFINE_OP (28, subr_call, "subr-call", OP1 (X8_C24))
    {
      SCM ret;
      uint32_t idx;

      UNPACK_24 (op, idx);

      SYNC_IP ();
      ret = scm_apply_subr (sp, idx, FRAME_LOCALS_COUNT ());

      if (SCM_UNLIKELY (scm_is_values (ret)))
        {
          CALL_INTRINSIC (unpack_values_object, (thread, ret));
          CACHE_SP ();
          NEXT (1);
        }
      else
        {
          RESET_FRAME (1);
          SP_SET (0, ret);
          NEXT (1);
        }
    }

  /* foreign-call cif-idx:12 ptr-idx:12
   *
   * Call a foreign function.  Fetch the CIF and foreign pointer from
   * the CIF-IDX and PTR-IDX closure slots of the callee.  Arguments are
   * taken from the stack, and results placed on the stack, ready to be
   * returned.  This instruction is part of the trampolines created by
   * the FFI, and is not generated by the compiler.
   */
  VM_DEFINE_OP (29, foreign_call, "foreign-call", OP1 (X8_C12_C12))
    {
      uint16_t cif_idx, ptr_idx;
      SCM closure, cif, pointer;

      UNPACK_12_12 (op, cif_idx, ptr_idx);

      closure = FP_REF (0);
      cif = SCM_PROGRAM_FREE_VARIABLE_REF (closure, cif_idx);
      pointer = SCM_PROGRAM_FREE_VARIABLE_REF (closure, ptr_idx);

      SYNC_IP ();
      CALL_INTRINSIC (foreign_call, (thread, cif, pointer));
      CACHE_SP ();

      NEXT (1);
    }

  /* continuation-call contregs:24
   *
   * Return to a continuation, nonlocally.  The arguments to the
   * continuation are taken from the stack.  CONTREGS is a free variable
   * containing the reified continuation.  This instruction is part of
   * the implementation of undelimited continuations, and is not
   * generated by the compiler.
   */
  VM_DEFINE_OP (30, continuation_call, "continuation-call", OP1 (X8_C24))
    {
      SCM contregs;
      uint32_t contregs_idx;

      UNPACK_24 (op, contregs_idx);

      contregs =
        SCM_PROGRAM_FREE_VARIABLE_REF (FP_REF (0), contregs_idx);

      SYNC_IP ();
      CALL_INTRINSIC (reinstate_continuation_x, (thread, contregs));

      /* no NEXT */
      abort ();
    }

  /* compose-continuation cont:24
   *
   * Compose a partial continuation with the current continuation.  The
   * arguments to the continuation are taken from the stack.  CONT is a
   * free variable containing the reified continuation.  This
   * instruction is part of the implementation of partial continuations,
   * and is not generated by the compiler.
   */
  VM_DEFINE_OP (31, compose_continuation, "compose-continuation", OP1 (X8_C24))
    {
      SCM vmcont;
      uint32_t cont_idx;
      uint8_t *mcode;

      UNPACK_24 (op, cont_idx);
      vmcont = SCM_PROGRAM_FREE_VARIABLE_REF (FP_REF (0), cont_idx);

      SYNC_IP ();
      mcode = CALL_INTRINSIC (compose_continuation, (thread, vmcont));

#if ENABLE_JIT
      if (mcode && !VP->disable_mcode)
        {
          scm_jit_enter_mcode (thread, mcode);
          CACHE_REGISTER ();
          NEXT (0);
        }
      else
#endif
        {
          CACHE_REGISTER ();
          NEXT (0);
        }
    }

  /* capture-continuation dst:24
   *
   * Capture the current continuation.  This instruction is part of the
   * implementation of `call/cc', and is not generated by the compiler.
   */
  VM_DEFINE_OP (32, capture_continuation, "capture-continuation", DOP1 (X8_S24))
    {
      uint32_t dst;

      UNPACK_24 (op, dst);

      SYNC_IP ();
      SP_SET (dst, CALL_INTRINSIC (capture_continuation, (thread)));

      NEXT (1);
    }

  /* abort _:24
   *
   * Abort to a prompt handler.  The tag is expected in r1, and the rest
   * of the values in the frame are returned to the prompt handler.
   * This corresponds to a tail application of abort-to-prompt.
   */
  VM_DEFINE_OP (33, abort, "abort", OP1 (X32))
    {
      uint8_t *mcode = NULL;

      /* FIXME: Really we should capture the caller's registers.  Until
         then, manually advance the IP so that when the prompt resumes,
         it continues with the next instruction.  */
      ip++;
      SYNC_IP ();
      mcode = CALL_INTRINSIC (abort_to_prompt, (thread, mcode));

      /* If abort_to_prompt returned, that means there were no
         intervening C frames to jump over, so we just continue
         directly.  */

      CACHE_REGISTER ();
      ABORT_HOOK ();

#if ENABLE_JIT
      if (mcode && !VP->disable_mcode)
        {
          scm_jit_enter_mcode (thread, mcode);
          CACHE_REGISTER ();
        }
#endif

      NEXT (0);
    }

  /* prompt tag:24 escape-only?:1 _:7 proc-slot:24 _:8 handler-offset:24
   *
   * Push a new prompt on the dynamic stack, with a tag from TAG and a
   * handler at HANDLER-OFFSET words from the current IP.  The handler
   * will expect a multiple-value return as if from a call with the
   * procedure at PROC-SLOT.
   */
  VM_DEFINE_OP (34, prompt, "prompt", OP3 (X8_S24, B1_X7_F24, X8_L24))
    {
      uint32_t tag, proc_slot;
      int32_t offset;
      uint8_t escape_only_p;
      uint8_t *mra = NULL;

      UNPACK_24 (op, tag);
      escape_only_p = ip[1] & 0x1;
      UNPACK_24 (ip[1], proc_slot);
      offset = ip[2];
      offset >>= 8; /* Sign extension */
  
      /* Push the prompt onto the dynamic stack. */
      SYNC_IP ();
      CALL_INTRINSIC (push_prompt, (thread, escape_only_p, SP_REF (tag),
                                    VP->fp - proc_slot, ip + offset, mra));

      NEXT (3);
    }

  /* builtin-ref dst:12 idx:12
   *
   * Load a builtin stub by index into DST.
   */
  VM_DEFINE_OP (35, builtin_ref, "builtin-ref", DOP1 (X8_S12_C12))
    {
      uint16_t dst, idx;

      UNPACK_12_12 (op, dst, idx);
      SP_SET (dst, scm_vm_builtin_ref (idx));

      NEXT (1);
    }

  /* throw key:12 args:12
   *
   * Throw to KEY and ARGS.  ARGS should be a list.
   */
  VM_DEFINE_OP (36, throw, "throw", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      SCM key, args;

      UNPACK_12_12 (op, a, b);

      key = SP_REF (a);
      args = SP_REF (b);

      SYNC_IP ();
      CALL_INTRINSIC (throw_, (key, args));

      abort (); /* never reached */
    }

  /* throw/value val:24 key-subr-and-message:32
   *
   * Raise an error, indicating VAL as the bad value.
   * KEY-SUBR-AND-MESSAGE should be a vector, where the first element is
   * the symbol to which to throw, the second is the procedure in which
   * to signal the error (a string) or #f, and the third is a format
   * string for the message, with one template.
   */
  VM_DEFINE_OP (37, throw_value, "throw/value", OP2 (X8_S24, N32))
    {
      uint32_t a;
      int32_t offset;
      scm_t_bits key_subr_and_message_bits;
      SCM val, key_subr_and_message;

      UNPACK_24 (op, a);
      val = SP_REF (a);

      offset = ip[1];
      key_subr_and_message_bits = (scm_t_bits) (ip + offset);
      VM_ASSERT (!(key_subr_and_message_bits & 0x7), abort());
      key_subr_and_message = SCM_PACK (key_subr_and_message_bits);

      SYNC_IP ();
      CALL_INTRINSIC (throw_with_value, (val, key_subr_and_message));

      abort (); /* never reached */
    }

  /* throw/value+data val:24 key-subr-and-message:32
   *
   * Raise an error, indicating VAL as the bad value.
   * KEY-SUBR-AND-MESSAGE should be a vector, where the first element is
   * the symbol to which to throw, the second is the procedure in which
   * to signal the error (a string) or #f, and the third is a format
   * string for the message, with one template.
   */
  VM_DEFINE_OP (38, throw_value_and_data, "throw/value+data", OP2 (X8_S24, N32))
    {
      uint32_t a;
      int32_t offset;
      scm_t_bits key_subr_and_message_bits;
      SCM val, key_subr_and_message;

      UNPACK_24 (op, a);
      val = SP_REF (a);

      offset = ip[1];
      key_subr_and_message_bits = (scm_t_bits) (ip + offset);
      VM_ASSERT (!(key_subr_and_message_bits & 0x7), abort());
      key_subr_and_message = SCM_PACK (key_subr_and_message_bits);

      SYNC_IP ();
      CALL_INTRINSIC (throw_with_value_and_data, (val, key_subr_and_message));

      abort (); /* never reached */
    }

  /* handle-interrupts _:24
   *
   * Handle pending interrupts.
   */
  VM_DEFINE_OP (39, handle_interrupts, "handle-interrupts", OP1 (X32))
    {
      if (SCM_LIKELY (scm_is_null
                      (scm_atomic_ref_scm (&thread->pending_asyncs))))
        NEXT (1);

      if (thread->block_asyncs > 0)
        NEXT (1);

      SYNC_IP ();
      CALL_INTRINSIC (push_interrupt_frame, (thread, 0));
      CACHE_SP ();
      ip = scm_vm_intrinsics.handle_interrupt_code;

      NEXT (0);
    }

  /* return-from-interrupt _:24
   *
   * Return from handling an interrupt, discarding any return values and
   * stripping away the interrupt frame.
   */
  VM_DEFINE_OP (40, return_from_interrupt, "return-from-interrupt", OP1 (X32))
    {
      union scm_vm_stack_element *fp = VP->fp;

      ip = SCM_FRAME_VIRTUAL_RETURN_ADDRESS (fp);
      VP->fp = SCM_FRAME_DYNAMIC_LINK (fp);
      VP->sp = sp = SCM_FRAME_PREVIOUS_SP (fp);

      NEXT (0);
    }

  /* call-thread _:24 IDX:32
   *
   * Call the void-returning instrinsic with index IDX, passing the
   * current scm_thread* as the argument.
   */
  VM_DEFINE_OP (41, call_thread, "call-thread", OP2 (X32, C32))
    {
      scm_t_thread_intrinsic intrinsic;

      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      intrinsic (thread);
      CACHE_SP ();

      NEXT (2);
    }

  /* call-thread-scm a:24 IDX:32
   *
   * Call the void-returning instrinsic with index IDX, passing the
   * current scm_thread* and the SCM local A as arguments.
   */
  VM_DEFINE_OP (42, call_thread_scm, "call-thread-scm", OP2 (X8_S24, C32))
    {
      uint32_t a;
      scm_t_thread_scm_intrinsic intrinsic;

      UNPACK_24 (op, a);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      intrinsic (thread, SP_REF (a));
      CACHE_SP ();

      NEXT (2);
    }

  /* call-thread-scm-scm a:12 b:12 IDX:32
   *
   * Call the void-returning instrinsic with index IDX, passing the
   * current scm_thread* and the SCM locals A and B as arguments.
   */
  VM_DEFINE_OP (43, call_thread_scm_scm, "call-thread-scm-scm", OP2 (X8_S12_S12, C32))
    {
      uint16_t a, b;
      scm_t_thread_scm_scm_intrinsic intrinsic;

      UNPACK_12_12 (op, a, b);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      intrinsic (thread, SP_REF (a), SP_REF (b));
      CACHE_SP ();

      NEXT (2);
    }

  /* call-scm-sz-u32 a:8 b:8 c:8 IDX:32
   *
   * Call the void-returning instrinsic with index IDX, passing the
   * locals A, B, and C as arguments.  A is a SCM value, while B and C
   * are raw u64 values which fit into size_t and uint32_t types,
   * respectively.
   */
  VM_DEFINE_OP (44, call_scm_sz_u32, "call-scm-sz-u32", OP2 (X8_S8_S8_S8, C32))
    {
      uint8_t a, b, c;
      scm_t_scm_sz_u32_intrinsic intrinsic;

      UNPACK_8_8_8 (op, a, b, c);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      intrinsic (SP_REF (a), SP_REF_U64 (b), SP_REF_U64 (c));
      CACHE_SP ();

      NEXT (2);
    }

  /* call-scm<-thread dst:24 IDX:32
   *
   * Call the SCM-returning instrinsic with index IDX, passing the
   * current scm_thread* as argument.  Place the SCM result in DST.
   */
  VM_DEFINE_OP (45, call_scm_from_thread, "call-scm<-thread", DOP2 (X8_S24, C32))
    {
      uint32_t dst;
      scm_t_scm_from_thread_intrinsic intrinsic;
      SCM res;

      UNPACK_24 (op, dst);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (thread);
      CACHE_SP ();

      SP_SET (dst, res);

      NEXT (2);
    }

  /* call-s64<-scm dst:12 a:12 IDX:32
   *
   * Call the int64_t-returning instrinsic with index IDX, passing the
   * SCM local A as argument.  Place the s64 result in DST.
   */
  VM_DEFINE_OP (46, call_s64_from_scm, "call-s64<-scm", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      scm_t_s64_from_scm_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
#if INDIRECT_INT64_INTRINSICS
      intrinsic (& SP_REF_S64 (dst), SP_REF (src));
#else
      {
        int64_t res = intrinsic (SP_REF (src));
        SP_SET_S64 (dst, res);
      }
#endif

      /* No CACHE_SP () after the intrinsic, as the indirect variants
         have an out argument that points at the stack; stack relocation
         during this kind of intrinsic is not supported!  */

      NEXT (2);
    }

  /* call-scm<-u64 dst:12 a:12 IDX:32
   *
   * Call the SCM-returning instrinsic with index IDX, passing the
   * uint64_t local A as argument.  Place the SCM result in DST.
   */
  VM_DEFINE_OP (47, call_scm_from_u64, "call-scm<-u64", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      SCM res;
      scm_t_scm_from_u64_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
#if INDIRECT_INT64_INTRINSICS
      res = intrinsic (& SP_REF_U64 (src));
#else
      res = intrinsic (SP_REF_U64 (src));
#endif
      SP_SET (dst, res);

      /* No CACHE_SP () after the intrinsic, as the indirect variants
         pass stack pointers directly; stack relocation during this kind
         of intrinsic is not supported!  */

      NEXT (2);
    }

  /* call-scm<-s64 dst:12 a:12 IDX:32
   *
   * Call the SCM-returning instrinsic with index IDX, passing the
   * int64_t local A as argument.  Place the SCM result in DST.
   */
  VM_DEFINE_OP (48, call_scm_from_s64, "call-scm<-s64", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      SCM res;
      scm_t_scm_from_s64_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
#if INDIRECT_INT64_INTRINSICS
      res = intrinsic (& SP_REF_S64 (src));
#else
      res = intrinsic (SP_REF_S64 (src));
#endif
      CACHE_SP ();
      SP_SET (dst, res);

      NEXT (2);
    }

  /* call-scm<-scm dst:12 a:12 IDX:32
   *
   * Call the SCM-returning instrinsic with index IDX, passing the SCM
   * local A as argument.  Place the SCM result in DST.
   */
  VM_DEFINE_OP (49, call_scm_from_scm, "call-scm<-scm", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      SCM res;
      scm_t_scm_from_scm_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (SP_REF (src));
      CACHE_SP ();
      SP_SET (dst, res);

      NEXT (2);
    }

  /* call-f64<-scm dst:12 a:12 IDX:32
   *
   * Call the double-returning instrinsic with index IDX, passing the
   * SCM local A as argument.  Place the f64 result in DST.
   */
  VM_DEFINE_OP (50, call_f64_from_scm, "call-f64<-scm", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      double res;
      scm_t_f64_from_scm_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (SP_REF (src));
      CACHE_SP ();
      SP_SET_F64 (dst, res);

      NEXT (2);
    }

  /* call-u64<-scm dst:12 a:12 IDX:32
   *
   * Call the uint64_t-returning instrinsic with index IDX, passing the
   * SCM local A as argument.  Place the u64 result in DST.
   */
  VM_DEFINE_OP (51, call_u64_from_scm, "call-u64<-scm", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      scm_t_u64_from_scm_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
#if INDIRECT_INT64_INTRINSICS
      intrinsic (& SP_REF_U64 (dst), SP_REF (src));
#else
      {
        uint64_t res = intrinsic (SP_REF (src));
        SP_SET_U64 (dst, res);
      }
#endif

      /* No CACHE_SP () after the intrinsic, as the indirect variants
         have an out argument that points at the stack; stack relocation
         during this kind of intrinsic is not supported!  */

      NEXT (2);
    }

  /* call-scm<-scm-scm dst:8 a:8 b:8 IDX:32
   *
   * Call the SCM-returning instrinsic with index IDX, passing the SCM
   * locals A and B as arguments.  Place the SCM result in DST.
   */
  VM_DEFINE_OP (52, call_scm_from_scm_scm, "call-scm<-scm-scm", DOP2 (X8_S8_S8_S8, C32))
    {
      uint8_t dst, a, b;
      SCM res;
      scm_t_scm_from_scm_scm_intrinsic intrinsic;

      UNPACK_8_8_8 (op, dst, a, b);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (SP_REF (a), SP_REF (b));
      CACHE_SP ();
      SP_SET (dst, res);

      NEXT (2);
    }

  /* call-scm<-scm-uimm dst:8 a:8 b:8 IDX:32
   *
   * Call the SCM-returning instrinsic with index IDX, passing the SCM
   * local A and the uint8_t immediate B as arguments.  Place the SCM
   * result in DST.
   */
  VM_DEFINE_OP (53, call_scm_from_scm_uimm, "call-scm<-scm-uimm", DOP2 (X8_S8_S8_C8, C32))
    {
      uint8_t dst, a, b;
      SCM res;
      scm_t_scm_from_scm_uimm_intrinsic intrinsic;

      UNPACK_8_8_8 (op, dst, a, b);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (SP_REF (a), b);
      CACHE_SP ();
      SP_SET (dst, res);

      NEXT (2);
    }

  /* call-scm<-thread-scm dst:12 a:12 IDX:32
   *
   * Call the SCM-returning instrinsic with index IDX, passing the
   * current scm_thread* and SCM local A as arguments.  Place the SCM
   * result in DST.
   */
  VM_DEFINE_OP (54, call_scm_from_thread_scm, "call-scm<-thread-scm", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      scm_t_scm_from_thread_scm_intrinsic intrinsic;
      SCM res;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (thread, SP_REF (src));
      CACHE_SP ();

      SP_SET (dst, res);

      NEXT (2);
    }

  /* call-scm<-scm-u64 dst:8 a:8 b:8 IDX:32
   *
   * Call the SCM-returning instrinsic with index IDX, passing SCM local
   * A and u64 local B as arguments.  Place the SCM result in DST.
   */
  VM_DEFINE_OP (55, call_scm_from_scm_u64, "call-scm<-scm-u64", DOP2 (X8_S8_S8_S8, C32))
    {
      uint8_t dst, a, b;
      SCM res;
      scm_t_scm_from_scm_u64_intrinsic intrinsic;

      UNPACK_8_8_8 (op, dst, a, b);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
#if INDIRECT_INT64_INTRINSICS
      res = intrinsic (SP_REF (a), & SP_REF_U64 (b));
#else
      res = intrinsic (SP_REF (a), SP_REF_U64 (b));
#endif
      CACHE_SP ();

      SP_SET (dst, res);

      NEXT (2);
    }

  /* make-short-immediate dst:8 low-bits:16
   *
   * Make an immediate whose low bits are LOW-BITS, and whose top bits are
   * 0.
   */
  VM_DEFINE_OP (56, make_short_immediate, "make-short-immediate", DOP1 (X8_S8_I16))
    {
      uint8_t dst;
      scm_t_bits val;

      UNPACK_8_16 (op, dst, val);
      SP_SET (dst, SCM_PACK (val));
      NEXT (1);
    }

  /* make-long-immediate dst:24 low-bits:32
   *
   * Make an immediate whose low bits are LOW-BITS, and whose top bits are
   * 0.
   */
  VM_DEFINE_OP (57, make_long_immediate, "make-long-immediate", DOP2 (X8_S24, I32))
    {
      uint32_t dst;
      scm_t_bits val;

      UNPACK_24 (op, dst);
      val = ip[1];
      SP_SET (dst, SCM_PACK (val));
      NEXT (2);
    }

  /* make-long-long-immediate dst:24 high-bits:32 low-bits:32
   *
   * Make an immediate with HIGH-BITS and LOW-BITS.
   */
  VM_DEFINE_OP (58, make_long_long_immediate, "make-long-long-immediate", DOP3 (X8_S24, A32, B32))
    {
      uint32_t dst;
      scm_t_bits val;

      UNPACK_24 (op, dst);
#if SIZEOF_UINTPTR_T > 4
      val = ip[1];
      val <<= 32;
      val |= ip[2];
#else
      ASSERT (ip[1] == 0);
      val = ip[2];
#endif
      SP_SET (dst, SCM_PACK (val));
      NEXT (3);
    }

  /* make-non-immediate dst:24 offset:32
   *
   * Load a pointer to statically allocated memory into DST.  The
   * object's memory is will be found OFFSET 32-bit words away from the
   * current instruction pointer.  OFFSET is a signed value.  The
   * intention here is that the compiler would produce an object file
   * containing the words of a non-immediate object, and this
   * instruction creates a pointer to that memory, effectively
   * resurrecting that object.
   *
   * Whether the object is mutable or immutable depends on where it was
   * allocated by the compiler, and loaded by the loader.
   */
  VM_DEFINE_OP (59, make_non_immediate, "make-non-immediate", DOP2 (X8_S24, N32))
    {
      uint32_t dst;
      int32_t offset;
      uint32_t* loc;
      scm_t_bits unpacked;

      UNPACK_24 (op, dst);
      offset = ip[1];
      loc = ip + offset;
      unpacked = (scm_t_bits) loc;

      VM_ASSERT (!(unpacked & 0x7), abort());

      SP_SET (dst, SCM_PACK (unpacked));

      NEXT (2);
    }

  /* load-label dst:24 offset:32
   *
   * Load a label OFFSET words away from the current IP and write it to
   * DST.  OFFSET is a signed 32-bit integer.
   */
  VM_DEFINE_OP (60, load_label, "load-label", DOP2 (X8_S24, L32))
    {
      uint32_t dst;
      int32_t offset;

      UNPACK_24 (op, dst);
      offset = ip[1];

      SP_SET_U64 (dst, (uintptr_t) (ip + offset));

      NEXT (2);
    }

  /* load-f64 dst:24 high-bits:32 low-bits:32
   *
   * Make a double-precision floating-point value with HIGH-BITS and
   * LOW-BITS.
   */
  VM_DEFINE_OP (61, load_f64, "load-f64", DOP3 (X8_S24, AF32, BF32))
    {
      uint32_t dst;
      uint64_t val;

      UNPACK_24 (op, dst);
      val = ip[1];
      val <<= 32;
      val |= ip[2];
      SP_SET_U64 (dst, val);
      NEXT (3);
    }

  /* load-u64 dst:24 high-bits:32 low-bits:32
   *
   * Make an unsigned 64-bit integer with HIGH-BITS and LOW-BITS.
   */
  VM_DEFINE_OP (62, load_u64, "load-u64", DOP3 (X8_S24, AU32, BU32))
    {
      uint32_t dst;
      uint64_t val;

      UNPACK_24 (op, dst);
      val = ip[1];
      val <<= 32;
      val |= ip[2];
      SP_SET_U64 (dst, val);
      NEXT (3);
    }

  /* load-s64 dst:24 high-bits:32 low-bits:32
   *
   * Make an unsigned 64-bit integer with HIGH-BITS and LOW-BITS.
   */
  VM_DEFINE_OP (63, load_s64, "load-s64", DOP3 (X8_S24, AS32, BS32))
    {
      uint32_t dst;
      uint64_t val;

      UNPACK_24 (op, dst);
      val = ip[1];
      val <<= 32;
      val |= ip[2];
      SP_SET_U64 (dst, val);
      NEXT (3);
    }

  /* current-thread dst:24
   *
   * Write the current thread into DST.
   */
  VM_DEFINE_OP (64, current_thread, "current-thread", DOP1 (X8_S24))
    {
      uint32_t dst;

      UNPACK_24 (op, dst);
      SP_SET (dst, thread->handle);

      NEXT (1);
    }

  /* allocate-words dst:12 count:12
   *
   * Allocate a fresh GC-traced object consisting of COUNT words and
   * store it into DST.  COUNT is a u64 local.
   */
  VM_DEFINE_OP (65, allocate_words, "allocate-words", DOP1 (X8_S12_S12))
    {
      uint16_t dst, size;

      UNPACK_12_12 (op, dst, size);

      SYNC_IP ();
      SP_SET (dst, CALL_INTRINSIC (allocate_words, (thread, SP_REF_U64 (size))));
      NEXT (1);
    }

  /* allocate-words/immediate dst:12 count:12
   *
   * Allocate a fresh GC-traced object consisting of COUNT words and
   * store it into DST.  COUNT is an immediate.
   */
  VM_DEFINE_OP (66, allocate_words_immediate, "allocate-words/immediate", DOP1 (X8_S12_C12))
    {
      uint16_t dst, size;

      UNPACK_12_12 (op, dst, size);

      SYNC_IP ();
      SP_SET (dst, CALL_INTRINSIC (allocate_words, (thread, size)));

      NEXT (1);
    }

  /* scm-ref dst:8 obj:8 idx:8
   *
   * Load the SCM object at word offset IDX from local OBJ, and store it
   * to DST.
   */
  VM_DEFINE_OP (67, scm_ref, "scm-ref", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET (dst, SCM_CELL_OBJECT (SP_REF (obj), SP_REF_U64 (idx)));

      NEXT (1);
    }

  /* scm-set! obj:8 idx:8 val:8
   *
   * Store the SCM local VAL into object OBJ at word offset IDX.
   */
  VM_DEFINE_OP (68, scm_set, "scm-set!", OP1 (X8_S8_S8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_OBJECT (SP_REF (obj), SP_REF_U64 (idx), SP_REF (val));

      NEXT (1);
    }

  /* scm-ref/tag dst:8 obj:8 tag:8
   *
   * Load the first word of OBJ, subtract the immediate TAG, and store
   * the resulting SCM to DST.
   */
  VM_DEFINE_OP (69, scm_ref_tag, "scm-ref/tag", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, tag;

      UNPACK_8_8_8 (op, dst, obj, tag);

      SP_SET (dst, SCM_PACK (SCM_CELL_WORD_0 (SP_REF (obj)) - tag));

      NEXT (1);
    }

  /* scm-set!/tag obj:8 tag:8 val:8
   *
   * Set the first word of OBJ to the SCM value VAL plus the immediate
   * value TAG.
   */
  VM_DEFINE_OP (70, scm_set_tag, "scm-set!/tag", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, tag, val;

      UNPACK_8_8_8 (op, obj, tag, val);

      SCM_SET_CELL_WORD_0 (SP_REF (obj), SCM_UNPACK (SP_REF (val)) + tag);

      NEXT (1);
    }

  /* scm-ref/immediate dst:8 obj:8 idx:8
   *
   * Load the SCM object at word offset IDX from local OBJ, and store it
   * to DST.  IDX is a uint8_t immediate.
   */
  VM_DEFINE_OP (71, scm_ref_immediate, "scm-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET (dst, SCM_CELL_OBJECT (SP_REF (obj), idx));

      NEXT (1);
    }

  /* scm-set!/immediate obj:8 idx:8 val:8
   *
   * Store the SCM local VAL into object OBJ at word offset IDX.  IDX is
   * a uint8_t immediate.
   */
  VM_DEFINE_OP (72, scm_set_immediate, "scm-set!/immediate", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_OBJECT (SP_REF (obj), idx, SP_REF (val));

      NEXT (1);
    }

  /* word-ref dst:8 obj:8 idx:8
   *
   * Load the word at offset IDX from local OBJ, and store it to u64
   * DST.
   */
  VM_DEFINE_OP (73, word_ref, "word-ref", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET_U64 (dst, SCM_CELL_WORD (SP_REF (obj), SP_REF_U64 (idx)));

      NEXT (1);
    }

  /* word-set! obj:8 idx:8 val:8
   *
   * Store the u64 local VAL into object OBJ at word offset IDX.
   */
  VM_DEFINE_OP (74, word_set, "word-set!", OP1 (X8_S8_S8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_WORD (SP_REF (obj), SP_REF_U64 (idx), SP_REF_U64 (val));

      NEXT (1);
    }

  /* word-ref/immediate dst:8 obj:8 idx:8
   *
   * Load the word at offset IDX from local OBJ, and store it to u64
   * DST.  IDX is a uint8_t immediate.
   */
  VM_DEFINE_OP (75, word_ref_immediate, "word-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET_U64 (dst, SCM_CELL_WORD (SP_REF (obj), idx));

      NEXT (1);
    }

  /* word-set!/immediate obj:8 idx:8 val:8
   *
   * Store the u64 local VAL into object OBJ at word offset IDX.  IDX is
   * a uint8_t immediate.
   */
  VM_DEFINE_OP (76, word_set_immediate, "word-set!/immediate", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_WORD (SP_REF (obj), idx, SP_REF_U64 (val));

      NEXT (1);
    }

  /* pointer-ref/immediate dst:8 obj:8 idx:8
   *
   * Load the pointer at offset IDX from local OBJ, and store it to DST.
   * IDX is a uint8_t immediate.
   */
  VM_DEFINE_OP (77, pointer_ref_immediate, "pointer-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET_PTR (dst, (void*) SCM_CELL_WORD (SP_REF (obj), idx));

      NEXT (1);
    }

  /* pointer-set!/immediate obj:8 idx:8 val:8
   *
   * Store the pointer local VAL into object OBJ at offset IDX.  IDX is
   * a uint8_t immediate.
   */
  VM_DEFINE_OP (78, pointer_set_immediate, "pointer-set!/immediate", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_WORD (SP_REF (obj), idx, (uintptr_t) SP_REF_PTR (val));

      NEXT (1);
    }

  /* tail-pointer-ref/immediate dst:8 obj:8 idx:8
   *
   * Compute the address of word offset IDX from local OBJ, and store it
   * to DST.  IDX is a uint8_t immediate.
   */
  VM_DEFINE_OP (79, tail_pointer_ref_immediate, "tail-pointer-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET_PTR (dst, ((scm_t_bits *) SCM2PTR (SP_REF (obj))) + idx);

      NEXT (1);
    }

  /* atomic-scm-ref/immediate dst:8 obj:8 idx:8
   *
   * Atomically reference the SCM object at word offset IDX from local
   * OBJ, and store it to DST, using the sequential consistency memory
   * model.  IDX is a uint8_t immediate.
   */
  VM_DEFINE_OP (80, atomic_scm_ref_immediate, "atomic-scm-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, offset;
      SCM *loc;
      UNPACK_8_8_8 (op, dst, obj, offset);
      loc = SCM_CELL_OBJECT_LOC (SP_REF (obj), offset);
      SP_SET (dst, CALL_INTRINSIC (atomic_ref_scm, (loc)));
      NEXT (1);
    }

  /* atomic-scm-set!/immediate obj:8 idx:8 val:8
   *
   * Atomically store the SCM local VAL into object OBJ at word offset
   * IDX, using the sequentially consistent memory model.  IDX is a
   * uint8_t immediate.
   */
  VM_DEFINE_OP (81, atomic_scm_set_immediate, "atomic-scm-set!/immediate", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, offset, val;
      SCM *loc;
      UNPACK_8_8_8 (op, obj, offset, val);
      loc = SCM_CELL_OBJECT_LOC (SP_REF (obj), offset);
      CALL_INTRINSIC (atomic_set_scm, (loc, SP_REF (val)));
      NEXT (1);
    }

  /* atomic-scm-swap!/immediate dst:24 _:8 obj:24 idx:8 val:24
   *
   * Atomically swap the SCM value stored in object OBJ at word offset
   * IDX with VAL, using the sequentially consistent memory model.  IDX
   * is a uint8_t immediate.  Return the previous value to DST.
   */
  VM_DEFINE_OP (82, atomic_scm_swap_immediate, "atomic-scm-swap!/immediate", DOP3 (X8_S24, X8_S24, C8_S24))
    {
      uint32_t dst, obj, val;
      uint8_t offset;
      SCM *loc;
      UNPACK_24 (op, dst);
      UNPACK_24 (ip[1], obj);
      UNPACK_8_24 (ip[2], offset, val);
      loc = SCM_CELL_OBJECT_LOC (SP_REF (obj), offset);
      SP_SET (dst, CALL_INTRINSIC (atomic_swap_scm, (loc, SP_REF (val))));
      NEXT (3);
    }

  /* atomic-scm-compare-and-swap!/immediate dst:24 _:8 obj:24 idx:8 expected:24 _:8 desired:24
   *
   * Atomically swap the SCM value stored in object OBJ at word offset
   * IDX with DESIRED, if and only if the value that was there was
   * EXPECTED, using the sequentially consistent memory model.  IDX is a
   * uint8_t immediate.  Return the value that was stored at IDX from
   * OBJ in DST.
   */
  VM_DEFINE_OP (83, atomic_scm_compare_and_swap_immediate, "atomic-scm-compare-and-swap!/immediate", DOP4 (X8_S24, X8_S24, C8_S24, X8_S24))
    {
      uint32_t dst, obj, expected, desired;
      uint8_t offset;
      SCM *loc;
      SCM got;
      UNPACK_24 (op, dst);
      UNPACK_24 (ip[1], obj);
      UNPACK_8_24 (ip[2], offset, expected);
      UNPACK_24 (ip[3], desired);
      loc = SCM_CELL_OBJECT_LOC (SP_REF (obj), offset);
      got = CALL_INTRINSIC (atomic_compare_and_swap_scm,
                            (loc, SP_REF (expected), SP_REF (desired)));
      SP_SET (dst, got);
      NEXT (4);
    }

  /* static-ref dst:24 offset:32
   *
   * Load a SCM value into DST.  The SCM value will be fetched from
   * memory, OFFSET 32-bit words away from the current instruction
   * pointer.  OFFSET is a signed value.
   *
   * The intention is for this instruction to be used to load constants
   * that the compiler is unable to statically allocate, like symbols.
   * These values would be initialized when the object file loads.
   */
  VM_DEFINE_OP (84, static_ref, "static-ref", DOP2 (X8_S24, R32))
    {
      uint32_t dst;
      int32_t offset;
      uint32_t* loc;
      uintptr_t loc_bits;

      UNPACK_24 (op, dst);
      offset = ip[1];
      loc = ip + offset;
      loc_bits = (uintptr_t) loc;
      VM_ASSERT (ALIGNED_P (loc, SCM), abort());

      SP_SET (dst, *((SCM *) loc_bits));

      NEXT (2);
    }

  /* static-set! src:24 offset:32
   *
   * Store a SCM value into memory, OFFSET 32-bit words away from the
   * current instruction pointer.  OFFSET is a signed value.
   */
  VM_DEFINE_OP (85, static_set, "static-set!", OP2 (X8_S24, LO32))
    {
      uint32_t src;
      int32_t offset;
      uint32_t* loc;

      UNPACK_24 (op, src);
      offset = ip[1];
      loc = ip + offset;
      VM_ASSERT (ALIGNED_P (loc, SCM), abort());

      *((SCM *) loc) = SP_REF (src);

      NEXT (2);
    }

  /* static-patch! _:24 dst-offset:32 src-offset:32
   *
   * Patch a pointer at DST-OFFSET to point to SRC-OFFSET.  Both offsets
   * are signed 32-bit values, indicating a memory address as a number
   * of 32-bit words away from the current instruction pointer.
   */
  VM_DEFINE_OP (86, static_patch, "static-patch!", OP3 (X32, LO32, L32))
    {
      int32_t dst_offset, src_offset;
      void *src;
      void** dst_loc;

      dst_offset = ip[1];
      src_offset = ip[2];

      dst_loc = (void **) (ip + dst_offset);
      src = ip + src_offset;
      VM_ASSERT (ALIGNED_P (dst_loc, void*), abort());

      *dst_loc = src;

      NEXT (3);
    }

  /* tag-char dst:12 src:12
   *
   * Make a SCM character whose integer value is the u64 in SRC, and
   * store it in DST.
   */
  VM_DEFINE_OP (87, tag_char, "tag-char", DOP1 (X8_S12_S12))
    {
      uint16_t dst, src;
      UNPACK_12_12 (op, dst, src);
      SP_SET (dst,
              SCM_MAKE_ITAG8 ((scm_t_bits) (scm_t_wchar) SP_REF_U64 (src),
                              scm_tc8_char));
      NEXT (1);
    }

  /* untag-char dst:12 src:12
   *
   * Extract the integer value from the SCM character SRC, and store the
   * resulting u64 in DST.
   */
  VM_DEFINE_OP (88, untag_char, "untag-char", DOP1 (X8_S12_S12))
    {
      uint16_t dst, src;
      UNPACK_12_12 (op, dst, src);
      SP_SET_U64 (dst, SCM_CHAR (SP_REF (src)));
      NEXT (1);
    }

  /* tag-fixnum dst:12 src:12
   *
   * Make a SCM integer whose value is the s64 in SRC, and store it in
   * DST.
   */
  VM_DEFINE_OP (89, tag_fixnum, "tag-fixnum", DOP1 (X8_S12_S12))
    {
      uint16_t dst, src;

      UNPACK_12_12 (op, dst, src);

      SP_SET (dst, SCM_I_MAKINUM (SP_REF_S64 (src)));

      NEXT (1);
    }

  /* untag-fixnum dst:12 src:12
   *
   * Extract the integer value from the SCM integer SRC, and store the
   * resulting s64 in DST.
   */
  VM_DEFINE_OP (90, untag_fixnum, "untag-fixnum", DOP1 (X8_S12_S12))
    {
      uint16_t dst, src;

      UNPACK_12_12 (op, dst, src);

      SP_SET_S64 (dst, SCM_I_INUM (SP_REF (src)));

      NEXT (1);
    }

  /* uadd dst:8 a:8 b:8
   *
   * Add A to B, and place the result in DST.  The operands and the
   * result are unboxed unsigned 64-bit integers.  Overflow will wrap
   * around.
   */
  VM_DEFINE_OP (91, uadd, "uadd", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;
      UNPACK_8_8_8 (op, dst, a, b);
      SP_SET_U64 (dst, SP_REF_U64 (a) + SP_REF_U64 (b));
      NEXT (1);
    }

  /* usub dst:8 a:8 b:8
   *
   * Subtract B from A, and place the result in DST.  The operands and
   * the result are unboxed unsigned 64-bit integers.  Overflow will
   * wrap around.
   */
  VM_DEFINE_OP (92, usub, "usub", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;
      UNPACK_8_8_8 (op, dst, a, b);
      SP_SET_U64 (dst, SP_REF_U64 (a) - SP_REF_U64 (b));
      NEXT (1);
    }

  /* umul dst:8 a:8 b:8
   *
   * Multiply A and B, and place the result in DST.  The operands and
   * the result are unboxed unsigned 64-bit integers.  Overflow will
   * wrap around.
   */
  VM_DEFINE_OP (93, umul, "umul", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;
      UNPACK_8_8_8 (op, dst, a, b);
      SP_SET_U64 (dst, SP_REF_U64 (a) * SP_REF_U64 (b));
      NEXT (1);
    }

  /* uadd/immediate dst:8 src:8 imm:8
   *
   * Add the unsigned 64-bit value from SRC with the unsigned 8-bit
   * value IMM and place the raw unsigned 64-bit result in DST.
   * Overflow will wrap around.
   */
  VM_DEFINE_OP (94, uadd_immediate, "uadd/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, src, imm;
      uint64_t x;

      UNPACK_8_8_8 (op, dst, src, imm);
      x = SP_REF_U64 (src);
      SP_SET_U64 (dst, x + (uint64_t) imm);
      NEXT (1);
    }

  /* usub/immediate dst:8 src:8 imm:8
   *
   * Subtract the unsigned 8-bit value IMM from the unsigned 64-bit
   * value in SRC and place the raw unsigned 64-bit result in DST.
   * Overflow will wrap around.
   */
  VM_DEFINE_OP (95, usub_immediate, "usub/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, src, imm;
      uint64_t x;

      UNPACK_8_8_8 (op, dst, src, imm);
      x = SP_REF_U64 (src);
      SP_SET_U64 (dst, x - (uint64_t) imm);
      NEXT (1);
    }

  /* umul/immediate dst:8 src:8 imm:8
   *
   * Multiply the unsigned 64-bit value from SRC by the unsigned 8-bit
   * value IMM and place the raw unsigned 64-bit result in DST.
   * Overflow will wrap around.
   */
  VM_DEFINE_OP (96, umul_immediate, "umul/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, src, imm;
      uint64_t x;

      UNPACK_8_8_8 (op, dst, src, imm);
      x = SP_REF_U64 (src);
      SP_SET_U64 (dst, x * (uint64_t) imm);
      NEXT (1);
    }

  /* ulogand dst:8 a:8 b:8
   *
   * Place the bitwise AND of the u64 values in A and B into DST.
   */
  VM_DEFINE_OP (97, ulogand, "ulogand", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) & SP_REF_U64 (b));

      NEXT (1);
    }

  /* ulogior dst:8 a:8 b:8
   *
   * Place the bitwise inclusive OR of the u64 values in A and B into
   * DST.
   */
  VM_DEFINE_OP (98, ulogior, "ulogior", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) | SP_REF_U64 (b));

      NEXT (1);
    }

  /* ulogsub dst:8 a:8 b:8
   *
   * Place the (A & ~B) of the u64 values A and B into DST.
   */
  VM_DEFINE_OP (99, ulogsub, "ulogsub", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) & ~SP_REF_U64 (b));

      NEXT (1);
    }

  /* ulogxor dst:8 a:8 b:8
   *
   * Place the bitwise exclusive OR of the u64 values in A and B into
   * DST.
   */
  VM_DEFINE_OP (100, ulogxor, "ulogxor", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) ^ SP_REF_U64 (b));

      NEXT (1);
    }

  /* ursh dst:8 a:8 b:8
   *
   * Shift the u64 value in A right by B bits, and place the result in
   * DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (101, ursh, "ursh", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) >> (SP_REF_U64 (b) & 63));

      NEXT (1);
    }

  /* srsh dst:8 a:8 b:8
   *
   * Shift the s64 value in A right by B bits, and place the result in
   * DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (102, srsh, "srsh", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_S64 (dst, SCM_SRS (SP_REF_S64 (a), (SP_REF_U64 (b) & 63)));

      NEXT (1);
    }

  /* ulsh dst:8 a:8 b:8
   *
   * Shift the u64 value in A left by B bits, and place the result in
   * DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (103, ulsh, "ulsh", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) << (SP_REF_U64 (b) & 63));

      NEXT (1);
    }

  /* ursh/immediate dst:8 a:8 b:8
   *
   * Shift the u64 value in A right by the immediate B bits, and place
   * the result in DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (104, ursh_immediate, "ursh/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) >> (b & 63));

      NEXT (1);
    }

  /* srsh/immediate dst:8 a:8 b:8
   *
   * Shift the s64 value in A right by the immediate B bits, and place
   * the result in DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (105, srsh_immediate, "srsh/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_S64 (dst, SCM_SRS (SP_REF_S64 (a), b & 63));

      NEXT (1);
    }

  /* ulsh/immediate dst:8 a:8 b:8
   *
   * Shift the u64 value in A left by the immediate B bits, and place
   * the result in DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (106, ulsh_immediate, "ulsh/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) << (b & 63));

      NEXT (1);
    }

  /* fadd dst:8 a:8 b:8
   *
   * Add A to B, and place the result in DST.  The operands and the
   * result are unboxed double-precision floating-point numbers.
   */
  VM_DEFINE_OP (107, fadd, "fadd", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;
      UNPACK_8_8_8 (op, dst, a, b);
      SP_SET_F64 (dst, SP_REF_F64 (a) + SP_REF_F64 (b));
      NEXT (1);
    }

  /* fsub dst:8 a:8 b:8
   *
   * Subtract B from A, and place the result in DST.  The operands and
   * the result are unboxed double-precision floating-point numbers.
   */
  VM_DEFINE_OP (108, fsub, "fsub", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;
      UNPACK_8_8_8 (op, dst, a, b);
      SP_SET_F64 (dst, SP_REF_F64 (a) - SP_REF_F64 (b));
      NEXT (1);
    }

  /* fmul dst:8 a:8 b:8
   *
   * Multiply A and B, and place the result in DST.  The operands and
   * the result are unboxed double-precision floating-point numbers.
   */
  VM_DEFINE_OP (109, fmul, "fmul", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;
      UNPACK_8_8_8 (op, dst, a, b);
      SP_SET_F64 (dst, SP_REF_F64 (a) * SP_REF_F64 (b));
      NEXT (1);
    }

  /* fdiv dst:8 a:8 b:8
   *
   * Divide A by B, and place the result in DST.  The operands and the
   * result are unboxed double-precision floating-point numbers.
   */
  VM_DEFINE_OP (110, fdiv, "fdiv", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;
      UNPACK_8_8_8 (op, dst, a, b);
      SP_SET_F64 (dst, SP_REF_F64 (a) / SP_REF_F64 (b));
      NEXT (1);
    }

  /* u64=? a:12 b:12
   *
   * Set the comparison result to EQUAL if the u64 values A and B are
   * the same, or NONE otherwise.
   */
  VM_DEFINE_OP (111, u64_numerically_equal, "u64=?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      uint64_t x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF_U64 (a);
      y = SP_REF_U64 (b);

      VP->compare_result = x == y ? SCM_F_COMPARE_EQUAL : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* u64<? a:12 b:12
   *
   * Set the comparison result to LESS_THAN if the u64 value A is less
   * than the u64 value B are the same, or NONE otherwise.
   */
  VM_DEFINE_OP (112, u64_less, "u64<?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      uint64_t x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF_U64 (a);
      y = SP_REF_U64 (b);

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* s64<? a:12 b:12
   *
   * Set the comparison result to LESS_THAN if the s64 value A is less
   * than the s64 value B are the same, or NONE otherwise.
   */
  VM_DEFINE_OP (113, s64_less, "s64<?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      int64_t x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF_S64 (a);
      y = SP_REF_S64 (b);

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* s64-imm=? a:12 b:12
   *
   * Set the comparison result to EQUAL if the s64 value A is equal to
   * the immediate s64 value B, or NONE otherwise.
   */
  VM_DEFINE_OP (114, s64_imm_numerically_equal, "s64-imm=?", OP1 (X8_S12_Z12))
    {
      uint16_t a;
      int64_t x, y;

      a = (op >> 8) & 0xfff;
      x = SP_REF_S64 (a);

      y = ((int32_t) op) >> 20; /* Sign extension.  */

      VP->compare_result = x == y ? SCM_F_COMPARE_EQUAL : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* u64-imm<? a:12 b:12
   *
   * Set the comparison result to LESS_THAN if the u64 value A is less
   * than the immediate u64 value B, or NONE otherwise.
   */
  VM_DEFINE_OP (115, u64_imm_less, "u64-imm<?", OP1 (X8_S12_C12))
    {
      uint16_t a;
      uint64_t x, y;

      UNPACK_12_12 (op, a, y);
      x = SP_REF_U64 (a);

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* imm-u64<? a:12 b:12
   *
   * Set the comparison result to LESS_THAN if the u64 immediate B is
   * less than the u64 value A, or NONE otherwise.
   */
  VM_DEFINE_OP (116, imm_u64_less, "imm-u64<?", OP1 (X8_S12_C12))
    {
      uint16_t a;
      uint64_t x, y;

      UNPACK_12_12 (op, a, x);
      y = SP_REF_U64 (a);

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* s64-imm<? a:12 b:12
   *
   * Set the comparison result to LESS_THAN if the s64 value A is less
   * than the immediate s64 value B, or NONE otherwise.
   */
  VM_DEFINE_OP (117, s64_imm_less, "s64-imm<?", OP1 (X8_S12_Z12))
    {
      uint16_t a;
      int64_t x, y;

      a = (op >> 8) & 0xfff;
      x = SP_REF_S64 (a);

      y = ((int32_t) op) >> 20; /* Sign extension.  */

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* imm-s64<? a:12 b:12
   *
   * Set the comparison result to LESS_THAN if the s64 immediate B is
   * less than the s64 value A, or NONE otherwise.
   */
  VM_DEFINE_OP (118, imm_s64_less, "imm-s64<?", OP1 (X8_S12_Z12))
    {
      uint16_t a;
      int64_t x, y;

      a = (op >> 8) & 0xfff;
      y = SP_REF_S64 (a);

      x = ((int32_t) op) >> 20; /* Sign extension.  */

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* f64=? a:12 b:12
   *
   * Set the comparison result to EQUAL if the f64 value A is equal to
   * the f64 value B, or NONE otherwise.
   */
  VM_DEFINE_OP (119, f64_numerically_equal, "f64=?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      double x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF_F64 (a);
      y = SP_REF_F64 (b);

      if (x == y)
        VP->compare_result = SCM_F_COMPARE_EQUAL;
      else
        /* This is also the case for NaN.  */
        VP->compare_result = SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* f64<? a:12 b:12
   *
   * Set the comparison result to LESS_THAN if the f64 value A is less
   * than the f64 value B, NONE if A is greater than or equal to B, or
   * INVALID otherwise.
   */
  VM_DEFINE_OP (120, f64_less, "f64<?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      double x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF_F64 (a);
      y = SP_REF_F64 (b);

      if (x < y)
        VP->compare_result = SCM_F_COMPARE_LESS_THAN;
      else if (x >= y)
        VP->compare_result = SCM_F_COMPARE_NONE;
      else
        /* NaN.  */
        VP->compare_result = SCM_F_COMPARE_INVALID;

      NEXT (1);
    }

  /* =? a:12 b:12
   *
   * Set the comparison result to EQUAL if the SCM values A and B are
   * numerically equal, in the sense of "=".  Set to NONE otherwise.
   */
  VM_DEFINE_OP (121, numerically_equal, "=?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      SCM x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF (a);
      y = SP_REF (b);

      SYNC_IP ();
      if (CALL_INTRINSIC (numerically_equal_p, (x, y)))
        VP->compare_result = SCM_F_COMPARE_EQUAL;
      else
        VP->compare_result = SCM_F_COMPARE_NONE;
      CACHE_SP ();
      NEXT (1);
    }

  /* heap-numbers-equal? a:12 b:12
   *
   * Set the comparison result to EQUAL if the SCM values A and B are
   * numerically equal, in the sense of "=".  Set to NONE otherwise.  It
   * is known that both A and B are heap numbers.
   */
  VM_DEFINE_OP (122, heap_numbers_equal, "heap-numbers-equal?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      SCM x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF (a);
      y = SP_REF (b);

      SYNC_IP ();
      if (CALL_INTRINSIC (heap_numbers_equal_p, (x, y)))
        VP->compare_result = SCM_F_COMPARE_EQUAL;
      else
        VP->compare_result = SCM_F_COMPARE_NONE;
      CACHE_SP ();
      NEXT (1);
    }

  /* <? a:12 b:12
   *
   * Set the comparison result to LESS_THAN if the SCM value A is less
   * than the SCM value B, NONE if A is greater than or equal to B, or
   * INVALID otherwise.
   */
  VM_DEFINE_OP (123, less, "<?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      SCM x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF (a);
      y = SP_REF (b);

      SYNC_IP ();
      VP->compare_result = CALL_INTRINSIC (less_p, (x, y));
      CACHE_SP ();
      NEXT (1);
    }

  /* immediate-tag=? obj:24 mask:16 tag:16
   *
   * Set the comparison result to EQUAL if the result of a bitwise AND
   * between the bits of SCM value A and the immediate MASK is TAG, or
   * NONE otherwise.
   */
  VM_DEFINE_OP (124, immediate_tag_equals, "immediate-tag=?", OP2 (X8_S24, C16_C16))
    {
      uint32_t a;
      uint16_t mask, expected;
      SCM x;

      UNPACK_24 (op, a);
      UNPACK_16_16 (ip[1], mask, expected);
      x = SP_REF (a);

      if ((SCM_UNPACK (x) & mask) == expected)
        VP->compare_result = SCM_F_COMPARE_EQUAL;
      else
        VP->compare_result = SCM_F_COMPARE_NONE;

      NEXT (2);
    }

  /* heap-tag=? obj:24 mask:16 tag:16
   *
   * Set the comparison result to EQUAL if the result of a bitwise AND
   * between the first word of SCM value A and the immediate MASK is
   * TAG, or NONE otherwise.
   */
  VM_DEFINE_OP (125, heap_tag_equals, "heap-tag=?", OP2 (X8_S24, C16_C16))
    {
      uint32_t a;
      uint16_t mask, expected;
      SCM x;

      UNPACK_24 (op, a);
      UNPACK_16_16 (ip[1], mask, expected);
      x = SP_REF (a);

      if ((SCM_CELL_TYPE (x) & mask) == expected)
        VP->compare_result = SCM_F_COMPARE_EQUAL;
      else
        VP->compare_result = SCM_F_COMPARE_NONE;

      NEXT (2);
    }

  /* eq? a:12 b:12
   *
   * Set the comparison result to EQUAL if the SCM values A and B are
   * eq?, or NONE otherwise.
   */
  VM_DEFINE_OP (126, eq, "eq?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      SCM x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF (a);
      y = SP_REF (b);

      if (scm_is_eq (x, y))
        VP->compare_result = SCM_F_COMPARE_EQUAL;
      else
        VP->compare_result = SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  /* j offset:24
   *
   * Add OFFSET, a signed 24-bit number, to the current instruction
   * pointer.
   */
  VM_DEFINE_OP (127, j, "j", OP1 (X8_L24))
    {
      int32_t offset = op;
      offset >>= 8; /* Sign-extending shift. */
      NEXT (offset);
    }

  /* jl offset:24
   *
   * If the last comparison result is equal to SCM_F_COMPARE_LESS_THAN, add
   * OFFSET, a signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (128, jl, "jl", OP1 (X8_L24))
    {
      if (VP->compare_result == SCM_F_COMPARE_LESS_THAN)
        {
          int32_t offset = op;
          offset >>= 8; /* Sign-extending shift. */
          NEXT (offset);
        }
      else
        NEXT (1);
    }

  /* je offset:24
   *
   * If the last comparison result was EQUAL, then add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (129, je, "je", OP1 (X8_L24))
    {
      if (VP->compare_result == SCM_F_COMPARE_EQUAL)
        {
          int32_t offset = op;
          offset >>= 8; /* Sign-extending shift. */
          NEXT (offset);
        }
      else
        NEXT (1);
    }

  /* jnl offset:24
   *
   * If the last comparison result was not LESS_THAN, then add OFFSET, a
   * signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (130, jnl, "jnl", OP1 (X8_L24))
    {
      if (VP->compare_result != SCM_F_COMPARE_LESS_THAN)
        {
          int32_t offset = op;
          offset >>= 8; /* Sign-extending shift. */
          NEXT (offset);
        }
      else
        NEXT (1);
    }

  /* jne offset:24
   *
   * If the last comparison result was not EQUAL, then add OFFSET, a
   * signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (131, jne, "jne", OP1 (X8_L24))
    {
      if (VP->compare_result != SCM_F_COMPARE_EQUAL)
        {
          int32_t offset = op;
          offset >>= 8; /* Sign-extending shift. */
          NEXT (offset);
        }
      else
        NEXT (1);
    }

  /* jge offset:24
   *
   * If the last comparison result was NONE, then add OFFSET, a signed
   * 24-bit number, to the current instruction pointer.
   *
   * This is intended for use after a "<?" comparison, and is different
   * from "jnl" in the way it handles not-a-number (NaN) values: "<?"
   * sets INVALID instead of NONE if either value is a NaN.  For exact
   * numbers, "jge" is the same as "jnl".
   */
  VM_DEFINE_OP (132, jge, "jge", OP1 (X8_L24))
    {
      if (VP->compare_result == SCM_F_COMPARE_NONE)
        {
          int32_t offset = op;
          offset >>= 8; /* Sign-extending shift. */
          NEXT (offset);
        }
      else
        NEXT (1);
    }

  /* jnge offset:24
   *
   * If the last comparison result was not NONE, then add OFFSET, a
   * signed 24-bit number, to the current instruction pointer.
   *
   * This is intended for use after a "<?" comparison, and is different
   * from "jl" in the way it handles not-a-number (NaN) values: "<?"
   * sets INVALID instead of NONE if either value is a NaN.  For exact
   * numbers, "jnge" is the same as "jl".
   */
  VM_DEFINE_OP (133, jnge, "jnge", OP1 (X8_L24))
    {
      if (VP->compare_result != SCM_F_COMPARE_NONE)
        {
          int32_t offset = op;
          offset >>= 8; /* Sign-extending shift. */
          NEXT (offset);
        }
      else
        NEXT (1);
    }

#define PTR_REF(type, slot)                                             \
  do {                                                                  \
    uint8_t dst, a, b;                                                  \
    char *ptr;                                                          \
    size_t idx;                                                         \
    type val;                                                           \
    UNPACK_8_8_8 (op, dst, a, b);                                       \
    ptr = SP_REF_PTR (a);                                               \
    idx = SP_REF_U64 (b);                                               \
    memcpy (&val, ptr + idx, sizeof (val));                             \
    SP_SET_ ## slot (dst, val);                                         \
    NEXT (1);                                                           \
  } while (0)

#define PTR_SET(type, slot)                                             \
  do {                                                                  \
    uint8_t a, b, c;                                                    \
    char *ptr;                                                          \
    size_t idx;                                                         \
    type val;                                                           \
    UNPACK_8_8_8 (op, a, b, c);                                         \
    ptr = SP_REF_PTR (a);                                               \
    idx = SP_REF_U64 (b);                                               \
    val = SP_REF_ ## slot (c);                                          \
    memcpy (ptr + idx, &val, sizeof (val));                             \
    NEXT (1);                                                           \
  } while (0)

  /* u8-ref dst:8 ptr:8 idx:8
   *
   * Load the u8 at byte offset IDX from pointer PTR, and store it to
   * u64 DST.
   */
  VM_DEFINE_OP (134, u8_ref, "u8-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (uint8_t, U64);

  /* u16-ref dst:8 ptr:8 idx:8
   *
   * Load the u16 at byte offset IDX from pointer PTR, and store it to
   * u64 DST.
   */
  VM_DEFINE_OP (135, u16_ref, "u16-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (uint16_t, U64);

  /* u32-ref dst:8 ptr:8 idx:8
   *
   * Load the u32 at byte offset IDX from pointer PTR, and store it to
   * u64 DST.
   */
  VM_DEFINE_OP (136, u32_ref, "u32-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (uint32_t, U64);

  /* u64-ref dst:8 ptr:8 idx:8
   *
   * Load the u64 at byte offset IDX from pointer PTR, and store it to
   * u64 DST.
   */
  VM_DEFINE_OP (137, u64_ref, "u64-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (uint64_t, U64);

  /* u8-set! ptr:8 idx:8 val:8
   *
   * Store the u64 value VAL into the u8 at byte offset IDX from pointer
   * PTR.
   */
  VM_DEFINE_OP (138, u8_set, "u8-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (uint8_t, U64);

  /* u16-set! ptr:8 idx:8 val:8
   *
   * Store the u64 value VAL into the u16 at byte offset IDX from
   * pointer PTR.
   */
  VM_DEFINE_OP (139, u16_set, "u16-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (uint16_t, U64);

  /* u32-set! ptr:8 idx:8 val:8
   *
   * Store the u64 value VAL into the u32 at byte offset IDX from
   * pointer PTR.
   */
  VM_DEFINE_OP (140, u32_set, "u32-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (uint32_t, U64);

  /* u64-set! ptr:8 idx:8 val:8
   *
   * Store the u64 value VAL into the u64 at byte offset IDX from
   * pointer PTR.
   */
  VM_DEFINE_OP (141, u64_set, "u64-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (uint64_t, U64);

  /* s8-ref dst:8 ptr:8 idx:8
   *
   * Load the s8 at byte offset IDX from pointer PTR, and store it to
   * s64 DST.
   */
  VM_DEFINE_OP (142, s8_ref, "s8-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (int8_t, S64);

  /* s16-ref dst:8 ptr:8 idx:8
   *
   * Load the s16 at byte offset IDX from pointer PTR, and store it to
   * s64 DST.
   */
  VM_DEFINE_OP (143, s16_ref, "s16-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (int16_t, S64);

  /* s32-ref dst:8 ptr:8 idx:8
   *
   * Load the s32 at byte offset IDX from pointer PTR, and store it to
   * s64 DST.
   */
  VM_DEFINE_OP (144, s32_ref, "s32-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (int32_t, S64);

  /* s64-ref dst:8 ptr:8 idx:8
   *
   * Load the s64 at byte offset IDX from pointer PTR, and store it to
   * s64 DST.
   */
  VM_DEFINE_OP (145, s64_ref, "s64-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (int64_t, S64);

  /* s8-set! ptr:8 idx:8 val:8
   *
   * Store the s64 value VAL into the s8 at byte offset IDX from pointer
   * PTR.
   */
  VM_DEFINE_OP (146, s8_set, "s8-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (int8_t, S64);

  /* s16-set! ptr:8 idx:8 val:8
   *
   * Store the s64 value VAL into the s16 at byte offset IDX from
   * pointer PTR.
   */
  VM_DEFINE_OP (147, s16_set, "s16-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (int16_t, S64);

  /* s32-set! ptr:8 idx:8 val:8
   *
   * Store the s64 value VAL into the s32 at byte offset IDX from
   * pointer PTR.
   */
  VM_DEFINE_OP (148, s32_set, "s32-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (int32_t, S64);

  /* s64-set! ptr:8 idx:8 val:8
   *
   * Store the s64 value VAL into the s64 at byte offset IDX from
   * pointer PTR.
   */
  VM_DEFINE_OP (149, s64_set, "s64-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (int64_t, S64);

  /* f32-ref dst:8 ptr:8 idx:8
   *
   * Load the f32 at byte offset IDX from pointer PTR, and store it to
   * f64 DST.
   */
  VM_DEFINE_OP (150, f32_ref, "f32-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (float, F64);

  /* f64-ref dst:8 ptr:8 idx:8
   *
   * Load the f64 at byte offset IDX from pointer PTR, and store it to
   * f64 DST.
   */
  VM_DEFINE_OP (151, f64_ref, "f64-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (double, F64);

  /* f32-set! ptr:8 idx:8 val:8
   *
   * Store the f64 value VAL into the f32 at byte offset IDX from
   * pointer PTR.
   */
  VM_DEFINE_OP (152, f32_set, "f32-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (float, F64);

  /* s64-set! ptr:8 idx:8 val:8
   *
   * Store the f64 value VAL into the f8 at byte offset IDX from pointer
   * PTR.
   */
  VM_DEFINE_OP (153, f64_set, "f64-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (double, F64);

  VM_DEFINE_OP (154, unused_154, NULL, NOP)
  VM_DEFINE_OP (155, unused_155, NULL, NOP)
  VM_DEFINE_OP (156, unused_156, NULL, NOP)
  VM_DEFINE_OP (157, unused_157, NULL, NOP)
  VM_DEFINE_OP (158, unused_158, NULL, NOP)
  VM_DEFINE_OP (159, unused_159, NULL, NOP)
  VM_DEFINE_OP (160, unused_160, NULL, NOP)
  VM_DEFINE_OP (161, unused_161, NULL, NOP)
  VM_DEFINE_OP (162, unused_162, NULL, NOP)
  VM_DEFINE_OP (163, unused_163, NULL, NOP)
  VM_DEFINE_OP (164, unused_164, NULL, NOP)
  VM_DEFINE_OP (165, unused_165, NULL, NOP)
  VM_DEFINE_OP (166, unused_166, NULL, NOP)
  VM_DEFINE_OP (167, unused_167, NULL, NOP)
  VM_DEFINE_OP (168, unused_168, NULL, NOP)
  VM_DEFINE_OP (169, unused_169, NULL, NOP)
  VM_DEFINE_OP (170, unused_170, NULL, NOP)
  VM_DEFINE_OP (171, unused_171, NULL, NOP)
  VM_DEFINE_OP (172, unused_172, NULL, NOP)
  VM_DEFINE_OP (173, unused_173, NULL, NOP)
  VM_DEFINE_OP (174, unused_174, NULL, NOP)
  VM_DEFINE_OP (175, unused_175, NULL, NOP)
  VM_DEFINE_OP (176, unused_176, NULL, NOP)
  VM_DEFINE_OP (177, unused_177, NULL, NOP)
  VM_DEFINE_OP (178, unused_178, NULL, NOP)
  VM_DEFINE_OP (179, unused_179, NULL, NOP)
  VM_DEFINE_OP (180, unused_180, NULL, NOP)
  VM_DEFINE_OP (181, unused_181, NULL, NOP)
  VM_DEFINE_OP (182, unused_182, NULL, NOP)
  VM_DEFINE_OP (183, unused_183, NULL, NOP)
  VM_DEFINE_OP (184, unused_184, NULL, NOP)
  VM_DEFINE_OP (185, unused_185, NULL, NOP)
  VM_DEFINE_OP (186, unused_186, NULL, NOP)
  VM_DEFINE_OP (187, unused_187, NULL, NOP)
  VM_DEFINE_OP (188, unused_188, NULL, NOP)
  VM_DEFINE_OP (189, unused_189, NULL, NOP)
  VM_DEFINE_OP (190, unused_190, NULL, NOP)
  VM_DEFINE_OP (191, unused_191, NULL, NOP)
  VM_DEFINE_OP (192, unused_192, NULL, NOP)
  VM_DEFINE_OP (193, unused_193, NULL, NOP)
  VM_DEFINE_OP (194, unused_194, NULL, NOP)
  VM_DEFINE_OP (195, unused_195, NULL, NOP)
  VM_DEFINE_OP (196, unused_196, NULL, NOP)
  VM_DEFINE_OP (197, unused_197, NULL, NOP)
  VM_DEFINE_OP (198, unused_198, NULL, NOP)
  VM_DEFINE_OP (199, unused_199, NULL, NOP)
  VM_DEFINE_OP (200, unused_200, NULL, NOP)
  VM_DEFINE_OP (201, unused_201, NULL, NOP)
  VM_DEFINE_OP (202, unused_202, NULL, NOP)
  VM_DEFINE_OP (203, unused_203, NULL, NOP)
  VM_DEFINE_OP (204, unused_204, NULL, NOP)
  VM_DEFINE_OP (205, unused_205, NULL, NOP)
  VM_DEFINE_OP (206, unused_206, NULL, NOP)
  VM_DEFINE_OP (207, unused_207, NULL, NOP)
  VM_DEFINE_OP (208, unused_208, NULL, NOP)
  VM_DEFINE_OP (209, unused_209, NULL, NOP)
  VM_DEFINE_OP (210, unused_210, NULL, NOP)
  VM_DEFINE_OP (211, unused_211, NULL, NOP)
  VM_DEFINE_OP (212, unused_212, NULL, NOP)
  VM_DEFINE_OP (213, unused_213, NULL, NOP)
  VM_DEFINE_OP (214, unused_214, NULL, NOP)
  VM_DEFINE_OP (215, unused_215, NULL, NOP)
  VM_DEFINE_OP (216, unused_216, NULL, NOP)
  VM_DEFINE_OP (217, unused_217, NULL, NOP)
  VM_DEFINE_OP (218, unused_218, NULL, NOP)
  VM_DEFINE_OP (219, unused_219, NULL, NOP)
  VM_DEFINE_OP (220, unused_220, NULL, NOP)
  VM_DEFINE_OP (221, unused_221, NULL, NOP)
  VM_DEFINE_OP (222, unused_222, NULL, NOP)
  VM_DEFINE_OP (223, unused_223, NULL, NOP)
  VM_DEFINE_OP (224, unused_224, NULL, NOP)
  VM_DEFINE_OP (225, unused_225, NULL, NOP)
  VM_DEFINE_OP (226, unused_226, NULL, NOP)
  VM_DEFINE_OP (227, unused_227, NULL, NOP)
  VM_DEFINE_OP (228, unused_228, NULL, NOP)
  VM_DEFINE_OP (229, unused_229, NULL, NOP)
  VM_DEFINE_OP (230, unused_230, NULL, NOP)
  VM_DEFINE_OP (231, unused_231, NULL, NOP)
  VM_DEFINE_OP (232, unused_232, NULL, NOP)
  VM_DEFINE_OP (233, unused_233, NULL, NOP)
  VM_DEFINE_OP (234, unused_234, NULL, NOP)
  VM_DEFINE_OP (235, unused_235, NULL, NOP)
  VM_DEFINE_OP (236, unused_236, NULL, NOP)
  VM_DEFINE_OP (237, unused_237, NULL, NOP)
  VM_DEFINE_OP (238, unused_238, NULL, NOP)
  VM_DEFINE_OP (239, unused_239, NULL, NOP)
  VM_DEFINE_OP (240, unused_240, NULL, NOP)
  VM_DEFINE_OP (241, unused_241, NULL, NOP)
  VM_DEFINE_OP (242, unused_242, NULL, NOP)
  VM_DEFINE_OP (243, unused_243, NULL, NOP)
  VM_DEFINE_OP (244, unused_244, NULL, NOP)
  VM_DEFINE_OP (245, unused_245, NULL, NOP)
  VM_DEFINE_OP (246, unused_246, NULL, NOP)
  VM_DEFINE_OP (247, unused_247, NULL, NOP)
  VM_DEFINE_OP (248, unused_248, NULL, NOP)
  VM_DEFINE_OP (249, unused_249, NULL, NOP)
  VM_DEFINE_OP (250, unused_250, NULL, NOP)
  VM_DEFINE_OP (251, unused_251, NULL, NOP)
  VM_DEFINE_OP (252, unused_252, NULL, NOP)
  VM_DEFINE_OP (253, unused_253, NULL, NOP)
  VM_DEFINE_OP (254, unused_254, NULL, NOP)
  VM_DEFINE_OP (255, unused_255, NULL, NOP)
    {

      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  END_DISPATCH_SWITCH;
}


#undef ABORT_HOOK
#undef ALIGNED_P
#undef APPLY_HOOK
#undef BEGIN_DISPATCH_SWITCH
#undef CACHE_REGISTER
#undef END_DISPATCH_SWITCH
#undef FP_REF
#undef FP_SET
#undef FP_SLOT
#undef SP_REF
#undef SP_SET
#undef NEXT
#undef NEXT_HOOK
#undef RETURN_HOOK
#undef RUN_HOOK
#undef SYNC_IP
#undef UNPACK_8_8_8
#undef UNPACK_8_16
#undef UNPACK_12_12
#undef UNPACK_24
#undef VM_DEFINE_OP
#undef VM_INSTRUCTION_TO_LABEL
#undef VM_USE_HOOKS
#undef VP

/*
(defun renumber-ops ()
  "start from top of buffer and renumber 'VM_DEFINE_FOO (\n' sequences"
  (interactive "")
  (save-excursion
    (let ((counter -1)) (goto-char (point-min))
      (while (re-search-forward "^ *VM_DEFINE_[^ ]+ (\\([^,]+\\)," (point-max) t)
        (replace-match
         (number-to-string (setq counter (1+ counter)))
          t t nil 1)))))
(renumber-ops)
*/
