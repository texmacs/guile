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
#define RUN_HOOK(exp)                                   \
  do {                                                  \
    if (SCM_UNLIKELY (VP->trace_level))                 \
      {                                                 \
        SYNC_IP ();                                     \
        exp;                                            \
        CACHE_SP ();                                    \
      }                                                 \
  } while (0)
#else
#define RUN_HOOK(exp)
#endif
#define RUN_HOOK0(h)      RUN_HOOK (vm_dispatch_##h##_hook (thread))
#define RUN_HOOK1(h, arg) RUN_HOOK (vm_dispatch_##h##_hook (thread, arg))

#define APPLY_HOOK()                  RUN_HOOK0 (apply)
#define PUSH_CONTINUATION_HOOK()      RUN_HOOK0 (push_continuation)
#define POP_CONTINUATION_HOOK(old_fp) RUN_HOOK1 (pop_continuation, old_fp)
#define NEXT_HOOK()                   RUN_HOOK0 (next)
#define ABORT_CONTINUATION_HOOK()     RUN_HOOK0 (abort)




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
   stack expansion is needed.  */
#define RESET_FRAME(n)                                              \
  do {                                                              \
    VP->sp = sp = VP->fp - (n);                                     \
    if (sp < VP->sp_min_since_gc)                                   \
      VP->sp_min_since_gc = sp;                                     \
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
  

  

  /*
   * Call and return
   */

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

      VP->ip = SCM_FRAME_VIRTUAL_RETURN_ADDRESS (VP->fp);
      VP->sp = SCM_FRAME_PREVIOUS_SP (VP->fp);
      VP->fp = SCM_FRAME_DYNAMIC_LINK (VP->fp);

      return ret;
    }

  /* call proc:24 _:8 nlocals:24
   *
   * Call a procedure.  PROC is the local corresponding to a procedure.
   * The two values below PROC will be overwritten by the saved call
   * frame data.  The new frame will have space for NLOCALS locals: one
   * for the procedure, and the rest for the arguments which should
   * already have been pushed on.
   *
   * When the call returns, execution proceeds with the next
   * instruction.  There may be any number of values on the return
   * stack; the precise number can be had by subtracting the address of
   * PROC from the post-call SP.
   */
  VM_DEFINE_OP (1, call, "call", OP2 (X8_F24, X8_C24))
    {
      uint32_t proc, nlocals;
      union scm_vm_stack_element *old_fp;

      UNPACK_24 (op, proc);
      UNPACK_24 (ip[1], nlocals);

      PUSH_CONTINUATION_HOOK ();

      old_fp = VP->fp;
      VP->fp = SCM_FRAME_SLOT (old_fp, proc - 1);
      SCM_FRAME_SET_DYNAMIC_LINK (VP->fp, old_fp);
      SCM_FRAME_SET_VIRTUAL_RETURN_ADDRESS (VP->fp, ip + 2);
      SCM_FRAME_SET_MACHINE_RETURN_ADDRESS (VP->fp, 0);

      RESET_FRAME (nlocals);

      if (SCM_LIKELY (SCM_PROGRAM_P (FP_REF (0))))
        ip = SCM_PROGRAM_CODE (FP_REF (0));
      else
        {
          SYNC_IP ();
          CALL_INTRINSIC (apply_non_program, (thread));
          CACHE_REGISTER ();
        }

      APPLY_HOOK ();

      NEXT (0);
    }

  /* call-label proc:24 _:8 nlocals:24 label:32
   *
   * Call a procedure in the same compilation unit.
   *
   * This instruction is just like "call", except that instead of
   * dereferencing PROC to find the call target, the call target is
   * known to be at LABEL, a signed 32-bit offset in 32-bit units from
   * the current IP.  Since PROC is not dereferenced, it may be some
   * other representation of the closure.
   */
  VM_DEFINE_OP (2, call_label, "call-label", OP3 (X8_F24, X8_C24, L32))
    {
      uint32_t proc, nlocals;
      int32_t label;
      union scm_vm_stack_element *old_fp;

      UNPACK_24 (op, proc);
      UNPACK_24 (ip[1], nlocals);
      label = ip[2];

      PUSH_CONTINUATION_HOOK ();

      old_fp = VP->fp;
      VP->fp = SCM_FRAME_SLOT (old_fp, proc - 1);
      SCM_FRAME_SET_DYNAMIC_LINK (VP->fp, old_fp);
      SCM_FRAME_SET_VIRTUAL_RETURN_ADDRESS (VP->fp, ip + 3);
      SCM_FRAME_SET_MACHINE_RETURN_ADDRESS (VP->fp, 0);

      RESET_FRAME (nlocals);

      ip += label;

      APPLY_HOOK ();

      NEXT (0);
    }

  /* tail-call _:24
   *
   * Tail-call the procedure in slot 0 with the arguments in the current
   * stack frame.  Requires that the procedure and all of the arguments
   * have already been shuffled into position.
   */
  VM_DEFINE_OP (3, tail_call, "tail-call", OP1 (X32))
    {
      if (SCM_LIKELY (SCM_PROGRAM_P (FP_REF (0))))
        ip = SCM_PROGRAM_CODE (FP_REF (0));
      else
        {
          SYNC_IP ();
          CALL_INTRINSIC (apply_non_program, (thread));
          CACHE_REGISTER ();
        }

      APPLY_HOOK ();

      NEXT (0);
    }

  /* tail-call-label _:24 label:32
   *
   * Tail-call a known procedure.  As call is to call-label, tail-call
   * is to tail-call-label.
   */
  VM_DEFINE_OP (4, tail_call_label, "tail-call-label", OP2 (X32, L32))
    {
      int32_t label;
      
      label = ip[1];

      ip += label;

      APPLY_HOOK ();

      NEXT (0);
    }

  VM_DEFINE_OP (5, unused_5, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* receive dst:12 proc:12 _:8 nlocals:24
   *
   * Receive a single return value from a call whose procedure was in
   * PROC, asserting that the call actually returned at least one
   * value.  Afterwards, resets the frame to NLOCALS locals.
   */
  VM_DEFINE_OP (6, receive, "receive", DOP2 (X8_F12_F12, X8_C24))
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
  VM_DEFINE_OP (7, receive_values, "receive-values", OP2 (X8_F24, B1_X7_C24))
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

  /* shuffle-down from:12 to:12
   *
   * Shuffle down values from FROM to TO, reducing the frame size by
   * (FROM-TO) slots.  Part of the internal implementation of
   * call-with-values, values, and apply.
   */
  VM_DEFINE_OP (8, shuffle_down, "shuffle-down", OP1 (X8_F12_F12))
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

  /* return-values _:24
   *
   * Return all values from a call frame.
   */
  VM_DEFINE_OP (9, return_values, "return-values", OP1 (X32))
    {
      union scm_vm_stack_element *old_fp;
      size_t frame_size = 3;

      old_fp = VP->fp;
      ip = SCM_FRAME_VIRTUAL_RETURN_ADDRESS (VP->fp);
      VP->fp = SCM_FRAME_DYNAMIC_LINK (VP->fp);

      /* Clear stack frame.  */
      while (frame_size--)
        old_fp[frame_size].as_scm = SCM_BOOL_F;

      POP_CONTINUATION_HOOK (old_fp);

      NEXT (0);
    }


  

  /*
   * Specialized call stubs
   */

  /* subr-call _:24
   *
   * Call a subr, passing all locals in this frame as arguments.  Return
   * from the calling frame.  This instruction is part of the
   * trampolines created in gsubr.c, and is not generated by the
   * compiler.
   */
  VM_DEFINE_OP (10, subr_call, "subr-call", OP1 (X32))
    {
      SCM ret;

      SYNC_IP ();
      ret = scm_apply_subr (sp, FRAME_LOCALS_COUNT ());
      CACHE_SP ();

      if (SCM_UNLIKELY (scm_is_values (ret)))
        {
          size_t n, nvals = scm_i_nvalues (ret);
          ALLOC_FRAME (nvals);
          for (n = 0; n < nvals; n++)
            FP_SET (n, scm_i_value_ref (ret, n));
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
   * CIF-IDX and PTR-IDX, both free variables.  Return from the calling
   * frame.  Arguments are taken from the stack.  This instruction is
   * part of the trampolines created by the FFI, and is not generated by
   * the compiler.
   */
  VM_DEFINE_OP (11, foreign_call, "foreign-call", OP1 (X8_C12_C12))
    {
      uint16_t cif_idx, ptr_idx;
      SCM closure, cif, pointer, ret, err;

      UNPACK_12_12 (op, cif_idx, ptr_idx);

      closure = FP_REF (0);
      cif = SCM_PROGRAM_FREE_VARIABLE_REF (closure, cif_idx);
      pointer = SCM_PROGRAM_FREE_VARIABLE_REF (closure, ptr_idx);

      SYNC_IP ();
      ret = CALL_INTRINSIC (foreign_call, (cif, pointer, &err, sp));
      CACHE_SP ();

      ALLOC_FRAME (2);
      SP_SET (1, ret);
      SP_SET (0, err);

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
  VM_DEFINE_OP (12, continuation_call, "continuation-call", OP1 (X8_C24))
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
  VM_DEFINE_OP (13, compose_continuation, "compose-continuation", OP1 (X8_C24))
    {
      SCM vmcont;
      uint32_t cont_idx;

      UNPACK_24 (op, cont_idx);
      vmcont = SCM_PROGRAM_FREE_VARIABLE_REF (FP_REF (0), cont_idx);

      SYNC_IP ();
      CALL_INTRINSIC (compose_continuation, (thread, vmcont));
      CACHE_REGISTER ();
      NEXT (0);
    }

  VM_DEFINE_OP (14, unused_14, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* call/cc _:24
   *
   * Capture the current continuation, and tail-apply the procedure in
   * local slot 1 to it.  This instruction is part of the implementation
   * of `call/cc', and is not generated by the compiler.
   */
  VM_DEFINE_OP (15, call_cc, "call/cc", OP1 (X32))
    {
      SCM cont;

      SYNC_IP ();
      cont = CALL_INTRINSIC (capture_continuation, (thread));

      RESET_FRAME (2);

      SP_SET (1, SP_REF (0));
      SP_SET (0, cont);

      if (SCM_LIKELY (SCM_PROGRAM_P (SP_REF (1))))
        ip = SCM_PROGRAM_CODE (SP_REF (1));
      else
        {
          CALL_INTRINSIC (apply_non_program, (thread));
          CACHE_REGISTER ();
        }

      APPLY_HOOK ();

      NEXT (0);
    }

  /* abort _:24
   *
   * Abort to a prompt handler.  The tag is expected in r1, and the rest
   * of the values in the frame are returned to the prompt handler.
   * This corresponds to a tail application of abort-to-prompt.
   */
  VM_DEFINE_OP (16, abort, "abort", OP1 (X32))
    {
      /* FIXME: Really we should capture the caller's registers.  Until
         then, manually advance the IP so that when the prompt resumes,
         it continues with the next instruction.  */
      ip++;
      SYNC_IP ();
      CALL_INTRINSIC (abort_to_prompt, (thread));

      /* If abort_to_prompt returned, that means there were no
         intervening C frames to jump over, so we just continue
         directly.  */
      CACHE_REGISTER ();
      ABORT_CONTINUATION_HOOK ();
      NEXT (0);
    }

  /* builtin-ref dst:12 idx:12
   *
   * Load a builtin stub by index into DST.
   */
  VM_DEFINE_OP (17, builtin_ref, "builtin-ref", DOP1 (X8_S12_C12))
    {
      uint16_t dst, idx;

      UNPACK_12_12 (op, dst, idx);
      SP_SET (dst, scm_vm_builtin_ref (idx));

      NEXT (1);
    }


  

  /*
   * Function prologues
   */

  /* throw key:12 args:12
   *
   * Throw to KEY and ARGS.  ARGS should be a list.
   */
  VM_DEFINE_OP (18, throw, "throw", OP1 (X8_S12_S12))
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
  VM_DEFINE_OP (19, throw_value, "throw/value", OP2 (X8_S24, N32))
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
  VM_DEFINE_OP (20, throw_value_and_data, "throw/value+data", OP2 (X8_S24, N32))
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

  /* assert-nargs-ee expected:24
   * assert-nargs-ge expected:24
   * assert-nargs-le expected:24
   *
   * If the number of actual arguments is not ==, >=, or <= EXPECTED,
   * respectively, signal an error.
   */
  VM_DEFINE_OP (21, assert_nargs_ee, "assert-nargs-ee", OP1 (X8_C24))
    {
      uint32_t expected;
      UNPACK_24 (op, expected);
      VM_ASSERT (FRAME_LOCALS_COUNT () == expected,
                 CALL_INTRINSIC (error_wrong_num_args, (FP_REF (0))));
      NEXT (1);
    }
  VM_DEFINE_OP (22, assert_nargs_ge, "assert-nargs-ge", OP1 (X8_C24))
    {
      uint32_t expected;
      UNPACK_24 (op, expected);
      VM_ASSERT (FRAME_LOCALS_COUNT () >= expected,
                 CALL_INTRINSIC (error_wrong_num_args, (FP_REF (0))));
      NEXT (1);
    }
  VM_DEFINE_OP (23, assert_nargs_le, "assert-nargs-le", OP1 (X8_C24))
    {
      uint32_t expected;
      UNPACK_24 (op, expected);
      VM_ASSERT (FRAME_LOCALS_COUNT () <= expected,
                 CALL_INTRINSIC (error_wrong_num_args, (FP_REF (0))));
      NEXT (1);
    }

  /* alloc-frame nlocals:24
   *
   * Ensure that there is space on the stack for NLOCALS local variables,
   * setting them all to SCM_UNDEFINED, except those nargs values that
   * were passed as arguments and procedure.
   */
  VM_DEFINE_OP (24, alloc_frame, "alloc-frame", OP1 (X8_C24))
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
   * Like alloc-frame, but doesn't check that the stack is big enough.
   * Used to reset the frame size to something less than the size that
   * was previously set via alloc-frame.
   */
  VM_DEFINE_OP (25, reset_frame, "reset-frame", OP1 (X8_C24))
    {
      uint32_t nlocals;
      UNPACK_24 (op, nlocals);
      RESET_FRAME (nlocals);
      NEXT (1);
    }

  /* push src:24
   *
   * Push SRC onto the stack.
   */
  VM_DEFINE_OP (26, push, "push", OP1 (X8_S24))
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
  VM_DEFINE_OP (27, pop, "pop", DOP1 (X8_S24))
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
  VM_DEFINE_OP (28, drop, "drop", OP1 (X8_C24))
    {
      uint32_t count;

      UNPACK_24 (op, count);
      VP->sp = sp = sp + count;
      NEXT (1);
    }

  /* assert-nargs-ee/locals expected:12 nlocals:12
   *
   * Equivalent to a sequence of assert-nargs-ee and reserve-locals.  The
   * number of locals reserved is EXPECTED + NLOCALS.
   */
  VM_DEFINE_OP (29, assert_nargs_ee_locals, "assert-nargs-ee/locals", OP1 (X8_C12_C12))
    {
      uint16_t expected, nlocals;
      UNPACK_12_12 (op, expected, nlocals);
      VM_ASSERT (FRAME_LOCALS_COUNT () == expected,
                 CALL_INTRINSIC (error_wrong_num_args, (FP_REF (0))));
      ALLOC_FRAME (expected + nlocals);
      while (nlocals--)
        SP_SET (nlocals, SCM_UNDEFINED);

      NEXT (1);
    }

  /* expand-apply-argument _:24
   *
   * Take the last local in a frame and expand it out onto the stack, as
   * for the last argument to "apply".
   */
  VM_DEFINE_OP (30, expand_apply_argument, "expand-apply-argument", OP1 (X32))
    {
      int list_len;
      SCM list;

      list = SP_REF (0);

      SYNC_IP ();
      list_len = CALL_INTRINSIC (rest_arg_length, (list));

      ALLOC_FRAME (FRAME_LOCALS_COUNT () - 1 + list_len);

      while (list_len--)
        {
          SP_SET (list_len, SCM_CAR (list));
          list = SCM_CDR (list);
        }

      NEXT (1);
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
  VM_DEFINE_OP (31, bind_kwargs, "bind-kwargs", OP4 (X8_C24, C8_C24, X8_C24, N32))
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
  VM_DEFINE_OP (32, bind_rest, "bind-rest", DOP1 (X8_F24))
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


  

  VM_DEFINE_OP (33, allocate_words, "allocate-words", DOP1 (X8_S12_S12))
    {
      uint16_t dst, size;

      UNPACK_12_12 (op, dst, size);

      SYNC_IP ();
      SP_SET (dst, CALL_INTRINSIC (allocate_words, (thread, SP_REF_U64 (size))));
      NEXT (1);
    }

  VM_DEFINE_OP (34, allocate_words_immediate, "allocate-words/immediate", DOP1 (X8_S12_C12))
    {
      uint16_t dst, size;

      UNPACK_12_12 (op, dst, size);

      SYNC_IP ();
      SP_SET (dst, CALL_INTRINSIC (allocate_words, (thread, size)));

      NEXT (1);
    }

  VM_DEFINE_OP (35, scm_ref, "scm-ref", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET (dst, SCM_CELL_OBJECT (SP_REF (obj), SP_REF_U64 (idx)));

      NEXT (1);
    }

  VM_DEFINE_OP (36, scm_set, "scm-set!", OP1 (X8_S8_S8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_OBJECT (SP_REF (obj), SP_REF_U64 (idx), SP_REF (val));

      NEXT (1);
    }

  VM_DEFINE_OP (37, scm_ref_tag, "scm-ref/tag", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, tag;

      UNPACK_8_8_8 (op, dst, obj, tag);

      SP_SET (dst, SCM_PACK (SCM_CELL_WORD_0 (SP_REF (obj)) - tag));

      NEXT (1);
    }

  VM_DEFINE_OP (38, scm_set_tag, "scm-set!/tag", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, tag, val;

      UNPACK_8_8_8 (op, obj, tag, val);

      SCM_SET_CELL_WORD_0 (SP_REF (obj), SCM_UNPACK (SP_REF (val)) + tag);

      NEXT (1);
    }

  VM_DEFINE_OP (39, scm_ref_immediate, "scm-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET (dst, SCM_CELL_OBJECT (SP_REF (obj), idx));

      NEXT (1);
    }

  VM_DEFINE_OP (40, scm_set_immediate, "scm-set!/immediate", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_OBJECT (SP_REF (obj), idx, SP_REF (val));

      NEXT (1);
    }

  VM_DEFINE_OP (41, word_ref, "word-ref", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET_U64 (dst, SCM_CELL_WORD (SP_REF (obj), SP_REF_U64 (idx)));

      NEXT (1);
    }

  VM_DEFINE_OP (42, word_set, "word-set!", OP1 (X8_S8_S8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_WORD (SP_REF (obj), SP_REF_U64 (idx), SP_REF_U64 (val));

      NEXT (1);
    }

  VM_DEFINE_OP (43, word_ref_immediate, "word-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET_U64 (dst, SCM_CELL_WORD (SP_REF (obj), idx));

      NEXT (1);
    }

  VM_DEFINE_OP (44, word_set_immediate, "word-set!/immediate", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_WORD (SP_REF (obj), idx, SP_REF_U64 (val));

      NEXT (1);
    }

  VM_DEFINE_OP (45, pointer_ref_immediate, "pointer-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET_PTR (dst, (void*) SCM_CELL_WORD (SP_REF (obj), idx));

      NEXT (1);
    }

  VM_DEFINE_OP (46, pointer_set_immediate, "pointer-set!/immediate", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, idx, val;

      UNPACK_8_8_8 (op, obj, idx, val);

      SCM_SET_CELL_WORD (SP_REF (obj), idx, (uintptr_t) SP_REF_PTR (val));

      NEXT (1);
    }

  VM_DEFINE_OP (47, tail_pointer_ref_immediate, "tail-pointer-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, idx;

      UNPACK_8_8_8 (op, dst, obj, idx);

      SP_SET_PTR (dst, ((scm_t_bits *) SCM2PTR (SP_REF (obj))) + idx);

      NEXT (1);
    }

  

  /*
   * Lexical binding instructions
   */

  /* mov dst:12 src:12
   *
   * Copy a value from one local slot to another.
   */
  VM_DEFINE_OP (48, mov, "mov", DOP1 (X8_S12_S12))
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
  VM_DEFINE_OP (49, long_mov, "long-mov", DOP2 (X8_S24, X8_S24))
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
  VM_DEFINE_OP (50, long_fmov, "long-fmov", DOP2 (X8_F24, X8_F24))
    {
      uint32_t dst;
      uint32_t src;

      UNPACK_24 (op, dst);
      UNPACK_24 (ip[1], src);
      FP_SET (dst, FP_REF (src));

      NEXT (2);
    }

  VM_DEFINE_OP (51, call_scm_from_scm_scm, "call-scm<-scm-scm", DOP2 (X8_S8_S8_S8, C32))
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

  VM_DEFINE_OP (52, call_scm_from_scm_uimm, "call-scm<-scm-uimm", DOP2 (X8_S8_S8_C8, C32))
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

  VM_DEFINE_OP (53, call_scm_u64_u64, "call-scm-u64-u64", OP2 (X8_S8_S8_S8, C32))
    {
      uint8_t a, b, c;
      scm_t_scm_u64_u64_intrinsic intrinsic;

      UNPACK_8_8_8 (op, a, b, c);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      intrinsic (SP_REF (a), SP_REF_U64 (b), SP_REF_U64 (c));
      CACHE_SP ();

      NEXT (2);
    }

  VM_DEFINE_OP (54, call_scm_from_scm, "call-scm<-scm", DOP2 (X8_S12_S12, C32))
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

  VM_DEFINE_OP (55, call_f64_from_scm, "call-f64<-scm", DOP2 (X8_S12_S12, C32))
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

  VM_DEFINE_OP (56, call_u64_from_scm, "call-u64<-scm", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      uint64_t res;
      scm_t_u64_from_scm_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (SP_REF (src));
      CACHE_SP ();
      SP_SET_U64 (dst, res);

      NEXT (2);
    }


  

  /*
   * Immediates and statically allocated non-immediates
   */

  /* make-short-immediate dst:8 low-bits:16
   *
   * Make an immediate whose low bits are LOW-BITS, and whose top bits are
   * 0.
   */
  VM_DEFINE_OP (57, make_short_immediate, "make-short-immediate", DOP1 (X8_S8_I16))
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
  VM_DEFINE_OP (58, make_long_immediate, "make-long-immediate", DOP2 (X8_S24, I32))
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
  VM_DEFINE_OP (59, make_long_long_immediate, "make-long-long-immediate", DOP3 (X8_S24, A32, B32))
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
  VM_DEFINE_OP (60, make_non_immediate, "make-non-immediate", DOP2 (X8_S24, N32))
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
  VM_DEFINE_OP (61, static_ref, "static-ref", DOP2 (X8_S24, R32))
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
  VM_DEFINE_OP (62, static_set, "static-set!", OP2 (X8_S24, LO32))
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
  VM_DEFINE_OP (63, static_patch, "static-patch!", OP3 (X32, LO32, L32))
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

  

  VM_DEFINE_OP (64, unused_64, NULL, NOP)
  VM_DEFINE_OP (65, unused_65, NULL, NOP)
  VM_DEFINE_OP (66, unused_66, NULL, NOP)
  VM_DEFINE_OP (67, unused_67, NULL, NOP)
  VM_DEFINE_OP (68, unused_68, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  

  /*
   * The dynamic environment
   */

  /* prompt tag:24 escape-only?:1 _:7 proc-slot:24 _:8 handler-offset:24
   *
   * Push a new prompt on the dynamic stack, with a tag from TAG and a
   * handler at HANDLER-OFFSET words from the current IP.  The handler
   * will expect a multiple-value return as if from a call with the
   * procedure at PROC-SLOT.
   */
  VM_DEFINE_OP (69, prompt, "prompt", OP3 (X8_S24, B1_X7_F24, X8_L24))
    {
      uint32_t tag, proc_slot;
      int32_t offset;
      uint8_t escape_only_p;

      UNPACK_24 (op, tag);
      escape_only_p = ip[1] & 0x1;
      UNPACK_24 (ip[1], proc_slot);
      offset = ip[2];
      offset >>= 8; /* Sign extension */
  
      /* Push the prompt onto the dynamic stack. */
      SYNC_IP ();
      CALL_INTRINSIC (push_prompt, (thread, escape_only_p, SP_REF (tag),
                                    VP->fp - proc_slot, ip + offset));

      NEXT (3);
    }

  VM_DEFINE_OP (70, unused_70, NULL, NOP)
  VM_DEFINE_OP (71, unused_71, NULL, NOP)
  VM_DEFINE_OP (72, unused_72, NULL, NOP)
  VM_DEFINE_OP (73, unused_73, NULL, NOP)
  VM_DEFINE_OP (74, unused_74, NULL, NOP)
  VM_DEFINE_OP (75, unused_75, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* load-label dst:24 offset:32
   *
   * Load a label OFFSET words away from the current IP and write it to
   * DST.  OFFSET is a signed 32-bit integer.
   */
  VM_DEFINE_OP (76, load_label, "load-label", DOP2 (X8_S24, L32))
    {
      uint32_t dst;
      int32_t offset;

      UNPACK_24 (op, dst);
      offset = ip[1];

      SP_SET_U64 (dst, (uintptr_t) (ip + offset));

      NEXT (2);
    }

  VM_DEFINE_OP (77, call_s64_from_scm, "call-s64<-scm", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      int64_t res;
      scm_t_s64_from_scm_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (SP_REF (src));
      CACHE_SP ();
      SP_SET_S64 (dst, res);

      NEXT (2);
    }

  VM_DEFINE_OP (78, call_scm_from_u64, "call-scm<-u64", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      SCM res;
      scm_t_scm_from_u64_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (SP_REF_U64 (src));
      CACHE_SP ();
      SP_SET (dst, res);

      NEXT (2);
    }

  VM_DEFINE_OP (79, call_scm_from_s64, "call-scm<-s64", DOP2 (X8_S12_S12, C32))
    {
      uint16_t dst, src;
      SCM res;
      scm_t_scm_from_s64_intrinsic intrinsic;

      UNPACK_12_12 (op, dst, src);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (SP_REF_S64 (src));
      CACHE_SP ();
      SP_SET (dst, res);

      NEXT (2);
    }

  VM_DEFINE_OP (80, unused_80, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  

  VM_DEFINE_OP (81, tag_char, "tag-char", DOP1 (X8_S12_S12))
    {
      uint16_t dst, src;
      UNPACK_12_12 (op, dst, src);
      SP_SET (dst,
              SCM_MAKE_ITAG8 ((scm_t_bits) (scm_t_wchar) SP_REF_U64 (src),
                              scm_tc8_char));
      NEXT (1);
    }

  VM_DEFINE_OP (82, untag_char, "untag-char", DOP1 (X8_S12_S12))
    {
      uint16_t dst, src;
      UNPACK_12_12 (op, dst, src);
      SP_SET_U64 (dst, SCM_CHAR (SP_REF (src)));
      NEXT (1);
    }

  VM_DEFINE_OP (83, atomic_ref_scm_immediate, "atomic-scm-ref/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, obj, offset;
      SCM *loc;
      UNPACK_8_8_8 (op, dst, obj, offset);
      loc = SCM_CELL_OBJECT_LOC (SP_REF (obj), offset);
      SP_SET (dst, scm_atomic_ref_scm (loc));
      NEXT (1);
    }

  VM_DEFINE_OP (84, atomic_set_scm_immediate, "atomic-scm-set!/immediate", OP1 (X8_S8_C8_S8))
    {
      uint8_t obj, offset, val;
      SCM *loc;
      UNPACK_8_8_8 (op, obj, offset, val);
      loc = SCM_CELL_OBJECT_LOC (SP_REF (obj), offset);
      scm_atomic_set_scm (loc, SP_REF (val));
      NEXT (1);
    }

  VM_DEFINE_OP (85, atomic_scm_swap_immediate, "atomic-scm-swap!/immediate", DOP3 (X8_S24, X8_S24, C8_S24))
    {
      uint32_t dst, obj, val;
      uint8_t offset;
      SCM *loc;
      UNPACK_24 (op, dst);
      UNPACK_24 (ip[1], obj);
      UNPACK_8_24 (ip[2], offset, val);
      loc = SCM_CELL_OBJECT_LOC (SP_REF (obj), offset);
      SP_SET (dst, scm_atomic_swap_scm (loc, SP_REF (val)));
      NEXT (3);
    }

  VM_DEFINE_OP (86, atomic_scm_compare_and_swap_immediate, "atomic-scm-compare-and-swap!/immediate", DOP4 (X8_S24, X8_S24, C8_S24, X8_S24))
    {
      uint32_t dst, obj, expected, desired;
      uint8_t offset;
      SCM *loc;
      SCM scm_expected;
      UNPACK_24 (op, dst);
      UNPACK_24 (ip[1], obj);
      UNPACK_8_24 (ip[2], offset, expected);
      UNPACK_24 (ip[3], desired);
      loc = SCM_CELL_OBJECT_LOC (SP_REF (obj), offset);
      scm_expected = SP_REF (expected);
      scm_atomic_compare_and_swap_scm (loc, &scm_expected, SP_REF (desired));
      SP_SET (dst, scm_expected);
      NEXT (4);
    }

  VM_DEFINE_OP (87, call_thread_scm_scm, "call-thread-scm-scm", OP2 (X8_S12_S12, C32))
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

  VM_DEFINE_OP (88, call_thread, "call-thread", OP2 (X32, C32))
    {
      scm_t_thread_intrinsic intrinsic;

      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      intrinsic (thread);
      CACHE_SP ();

      NEXT (2);
    }

  VM_DEFINE_OP (89, call_scm_from_thread_scm, "call-scm<-thread-scm", DOP2 (X8_S12_S12, C32))
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

  VM_DEFINE_OP (90, call_thread_scm, "call-thread-scm", OP2 (X8_S24, C32))
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

  VM_DEFINE_OP (91, call_scm_from_scm_u64, "call-scm<-scm-u64", DOP2 (X8_S8_S8_S8, C32))
    {
      uint8_t dst, a, b;
      SCM res;
      scm_t_scm_from_scm_u64_intrinsic intrinsic;

      UNPACK_8_8_8 (op, dst, a, b);
      intrinsic = intrinsics[ip[1]];

      SYNC_IP ();
      res = intrinsic (SP_REF (a), SP_REF_U64 (b));
      CACHE_SP ();

      SP_SET (dst, res);

      NEXT (2);
    }

  VM_DEFINE_OP (92, call_scm_from_thread, "call-scm<-thread", DOP2 (X8_S24, C32))
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

  VM_DEFINE_OP (93, unused_93, NULL, NOP)
  VM_DEFINE_OP (94, unused_94, NULL, NOP)
  VM_DEFINE_OP (95, unused_95, NULL, NOP)
  VM_DEFINE_OP (96, unused_96, NULL, NOP)
  VM_DEFINE_OP (97, unused_97, NULL, NOP)
  VM_DEFINE_OP (98, unused_98, NULL, NOP)
  VM_DEFINE_OP (99, unused_99, NULL, NOP)
  VM_DEFINE_OP (100, unused_100, NULL, NOP)
  VM_DEFINE_OP (101, unused_101, NULL, NOP)
  VM_DEFINE_OP (102, unused_102, NULL, NOP)
  VM_DEFINE_OP (103, unused_103, NULL, NOP)
  VM_DEFINE_OP (104, unused_104, NULL, NOP)
  VM_DEFINE_OP (105, unused_105, NULL, NOP)
  VM_DEFINE_OP (106, unused_106, NULL, NOP)
  VM_DEFINE_OP (107, unused_107, NULL, NOP)
  VM_DEFINE_OP (108, unused_108, NULL, NOP)
  VM_DEFINE_OP (109, unused_109, NULL, NOP)
  VM_DEFINE_OP (110, unused_110, NULL, NOP)
  VM_DEFINE_OP (111, unused_111, NULL, NOP)
  VM_DEFINE_OP (112, unused_112, NULL, NOP)
  VM_DEFINE_OP (113, unused_113, NULL, NOP)
  VM_DEFINE_OP (114, unused_114, NULL, NOP)
  VM_DEFINE_OP (115, unused_115, NULL, NOP)
  VM_DEFINE_OP (116, unused_116, NULL, NOP)
  VM_DEFINE_OP (117, unused_117, NULL, NOP)
  VM_DEFINE_OP (118, unused_118, NULL, NOP)
  VM_DEFINE_OP (119, unused_119, NULL, NOP)
  VM_DEFINE_OP (120, unused_120, NULL, NOP)
  VM_DEFINE_OP (121, unused_121, NULL, NOP)
  VM_DEFINE_OP (122, unused_122, NULL, NOP)
  VM_DEFINE_OP (123, unused_123, NULL, NOP)
  VM_DEFINE_OP (124, unused_124, NULL, NOP)
  VM_DEFINE_OP (125, unused_125, NULL, NOP)
  VM_DEFINE_OP (126, unused_126, NULL, NOP)
  VM_DEFINE_OP (127, unused_127, NULL, NOP)
  VM_DEFINE_OP (128, unused_128, NULL, NOP)
  VM_DEFINE_OP (129, unused_129, NULL, NOP)
  VM_DEFINE_OP (130, unused_130, NULL, NOP)
  VM_DEFINE_OP (131, unused_131, NULL, NOP)
  VM_DEFINE_OP (132, unused_132, NULL, NOP)
  VM_DEFINE_OP (133, unused_133, NULL, NOP)
  VM_DEFINE_OP (134, unused_134, NULL, NOP)
  VM_DEFINE_OP (135, unused_135, NULL, NOP)
  VM_DEFINE_OP (136, unused_136, NULL, NOP)
  VM_DEFINE_OP (137, unused_137, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* fadd dst:8 a:8 b:8
   *
   * Add A to B, and place the result in DST.  The operands and the
   * result are unboxed double-precision floating-point numbers.
   */
  VM_DEFINE_OP (138, fadd, "fadd", DOP1 (X8_S8_S8_S8))
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
  VM_DEFINE_OP (139, fsub, "fsub", DOP1 (X8_S8_S8_S8))
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
  VM_DEFINE_OP (140, fmul, "fmul", DOP1 (X8_S8_S8_S8))
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
  VM_DEFINE_OP (141, fdiv, "fdiv", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;
      UNPACK_8_8_8 (op, dst, a, b);
      SP_SET_F64 (dst, SP_REF_F64 (a) / SP_REF_F64 (b));
      NEXT (1);
    }

  VM_DEFINE_OP (142, unused_142, NULL, NOP)
  VM_DEFINE_OP (143, unused_143, NULL, NOP)
  VM_DEFINE_OP (144, unused_144, NULL, NOP)
  VM_DEFINE_OP (145, unused_145, NULL, NOP)
  VM_DEFINE_OP (146, unused_146, NULL, NOP)
  VM_DEFINE_OP (147, unused_147, NULL, NOP)
  VM_DEFINE_OP (148, unused_148, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* uadd dst:8 a:8 b:8
   *
   * Add A to B, and place the result in DST.  The operands and the
   * result are unboxed unsigned 64-bit integers.  Overflow will wrap
   * around.
   */
  VM_DEFINE_OP (149, uadd, "uadd", DOP1 (X8_S8_S8_S8))
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
  VM_DEFINE_OP (150, usub, "usub", DOP1 (X8_S8_S8_S8))
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
  VM_DEFINE_OP (151, umul, "umul", DOP1 (X8_S8_S8_S8))
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
  VM_DEFINE_OP (152, uadd_immediate, "uadd/immediate", DOP1 (X8_S8_S8_C8))
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
  VM_DEFINE_OP (153, usub_immediate, "usub/immediate", DOP1 (X8_S8_S8_C8))
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
  VM_DEFINE_OP (154, umul_immediate, "umul/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, src, imm;
      uint64_t x;

      UNPACK_8_8_8 (op, dst, src, imm);
      x = SP_REF_U64 (src);
      SP_SET_U64 (dst, x * (uint64_t) imm);
      NEXT (1);
    }

  /* load-f64 dst:24 high-bits:32 low-bits:32
   *
   * Make a double-precision floating-point value with HIGH-BITS and
   * LOW-BITS.
   */
  VM_DEFINE_OP (155, load_f64, "load-f64", DOP3 (X8_S24, AF32, BF32))
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
  VM_DEFINE_OP (156, load_u64, "load-u64", DOP3 (X8_S24, AU32, BU32))
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

  VM_DEFINE_OP (157, unused_157, NULL, NOP)
  VM_DEFINE_OP (158, unused_158, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* load-s64 dst:24 high-bits:32 low-bits:32
   *
   * Make an unsigned 64-bit integer with HIGH-BITS and LOW-BITS.
   */
  VM_DEFINE_OP (159, load_s64, "load-s64", DOP3 (X8_S24, AS32, BS32))
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
  VM_DEFINE_OP (160, current_thread, "current-thread", DOP1 (X8_S24))
    {
      uint32_t dst;

      UNPACK_24 (op, dst);
      SP_SET (dst, thread->handle);

      NEXT (1);
    }

  VM_DEFINE_OP (161, unused_161, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* ulogand dst:8 a:8 b:8
   *
   * Place the bitwise AND of the u64 values in A and B into DST.
   */
  VM_DEFINE_OP (162, ulogand, "ulogand", DOP1 (X8_S8_S8_S8))
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
  VM_DEFINE_OP (163, ulogior, "ulogior", DOP1 (X8_S8_S8_S8))
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
  VM_DEFINE_OP (164, ulogsub, "ulogsub", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) & ~SP_REF_U64 (b));

      NEXT (1);
    }

  /* ursh dst:8 a:8 b:8
   *
   * Shift the u64 value in A right by B bits, and place the result in
   * DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (165, ursh, "ursh", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) >> (SP_REF_U64 (b) & 63));

      NEXT (1);
    }

  /* ulsh dst:8 a:8 b:8
   *
   * Shift the u64 value in A left by B bits, and place the result in
   * DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (166, ulsh, "ulsh", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) << (SP_REF_U64 (b) & 63));

      NEXT (1);
    }

  VM_DEFINE_OP (167, unused_167, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* ursh/immediate dst:8 a:8 b:8
   *
   * Shift the u64 value in A right by the immediate B bits, and place
   * the result in DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (168, ursh_immediate, "ursh/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) >> (b & 63));

      NEXT (1);
    }

  /* ulsh/immediate dst:8 a:8 b:8
   *
   * Shift the u64 value in A left by the immediate B bits, and place
   * the result in DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (169, ulsh_immediate, "ulsh/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) << (b & 63));

      NEXT (1);
    }

  VM_DEFINE_OP (170, unused_170, NULL, NOP)
  VM_DEFINE_OP (171, unused_171, NULL, NOP)
  VM_DEFINE_OP (172, unused_172, NULL, NOP)
  VM_DEFINE_OP (173, unused_173, NULL, NOP)
  VM_DEFINE_OP (174, unused_174, NULL, NOP)
  VM_DEFINE_OP (175, unused_175, NULL, NOP)
  VM_DEFINE_OP (176, unused_176, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* ulogxor dst:8 a:8 b:8
   *
   * Place the bitwise exclusive OR of the u64 values in A and B into
   * DST.
   */
  VM_DEFINE_OP (177, ulogxor, "ulogxor", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_U64 (dst, SP_REF_U64 (a) ^ SP_REF_U64 (b));

      NEXT (1);
    }

  VM_DEFINE_OP (178, unused_178, NULL, NOP)
  VM_DEFINE_OP (179, unused_179, NULL, NOP)
  VM_DEFINE_OP (180, unused_180, NULL, NOP)
  VM_DEFINE_OP (181, unused_181, NULL, NOP)
  VM_DEFINE_OP (182, unused_182, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  /* handle-interrupts _:24
   *
   * Handle pending interrupts.
   */
  VM_DEFINE_OP (183, handle_interrupts, "handle-interrupts", OP1 (X32))
    {
      if (SCM_LIKELY (scm_is_null
                      (scm_atomic_ref_scm (&thread->pending_asyncs))))
        NEXT (1);

      if (thread->block_asyncs > 0)
        NEXT (1);

      SYNC_IP ();
      CALL_INTRINSIC (push_interrupt_frame, (thread, 0));
      CACHE_SP ();
      ip = (uint32_t *) vm_handle_interrupt_code;
      APPLY_HOOK ();
      NEXT (0);
    }

  /* return-from-interrupt _:24
   *
   * Return from handling an interrupt, discarding any return values and
   * stripping away the interrupt frame.
   */
  VM_DEFINE_OP (184, return_from_interrupt, "return-from-interrupt", OP1 (X32))
    {
      VP->sp = sp = SCM_FRAME_PREVIOUS_SP (VP->fp);
      ip = SCM_FRAME_VIRTUAL_RETURN_ADDRESS (VP->fp);
      VP->fp = SCM_FRAME_DYNAMIC_LINK (VP->fp);

      NEXT (0);
    }

  VM_DEFINE_OP (185, unused_185, NULL, NOP)
  VM_DEFINE_OP (186, unused_186, NULL, NOP)
  VM_DEFINE_OP (187, unused_187, NULL, NOP)
  VM_DEFINE_OP (188, unused_188, NULL, NOP)
  VM_DEFINE_OP (189, unused_189, NULL, NOP)
  VM_DEFINE_OP (190, unused_190, NULL, NOP)
  VM_DEFINE_OP (191, unused_191, NULL, NOP)
  VM_DEFINE_OP (192, unused_192, NULL, NOP)
    {
      vm_error_bad_instruction (op);
      abort (); /* never reached */
    }

  VM_DEFINE_OP (193, u64_numerically_equal, "u64=?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      uint64_t x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF_U64 (a);
      y = SP_REF_U64 (b);

      VP->compare_result = x == y ? SCM_F_COMPARE_EQUAL : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  VM_DEFINE_OP (194, u64_less, "u64<?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      uint64_t x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF_U64 (a);
      y = SP_REF_U64 (b);

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  VM_DEFINE_OP (195, s64_numerically_equal, "s64=?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      int64_t x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF_S64 (a);
      y = SP_REF_S64 (b);

      VP->compare_result = x == y ? SCM_F_COMPARE_EQUAL : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  VM_DEFINE_OP (196, s64_less, "s64<?", OP1 (X8_S12_S12))
    {
      uint16_t a, b;
      int64_t x, y;

      UNPACK_12_12 (op, a, b);
      x = SP_REF_S64 (a);
      y = SP_REF_S64 (b);

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  VM_DEFINE_OP (197, f64_numerically_equal, "f64=?", OP1 (X8_S12_S12))
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

  VM_DEFINE_OP (198, f64_less, "f64<?", OP1 (X8_S12_S12))
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

  VM_DEFINE_OP (199, numerically_equal, "=?", OP1 (X8_S12_S12))
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

  VM_DEFINE_OP (200, less, "<?", OP1 (X8_S12_S12))
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

  VM_DEFINE_OP (201, check_arguments, "arguments<=?", OP1 (X8_C24))
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

  VM_DEFINE_OP (202, check_positional_arguments, "positional-arguments<=?", OP2 (X8_C24, X8_C24))
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

  VM_DEFINE_OP (203, immediate_tag_equals, "immediate-tag=?", OP2 (X8_S24, C16_C16))
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

  VM_DEFINE_OP (204, heap_tag_equals, "heap-tag=?", OP2 (X8_S24, C16_C16))
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

  VM_DEFINE_OP (205, eq, "eq?", OP1 (X8_S12_S12))
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
  VM_DEFINE_OP (206, j, "j", OP1 (X8_L24))
    {
      int32_t offset = op;
      offset >>= 8; /* Sign-extending shift. */
      NEXT (offset);
    }

  /* jl offset:24
   *
   * If the flags register is equal to SCM_F_COMPARE_LESS_THAN, add
   * OFFSET, a signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (207, jl, "jl", OP1 (X8_L24))
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
   * If the flags register is equal to SCM_F_COMPARE_EQUAL, add
   * OFFSET, a signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (208, je, "je", OP1 (X8_L24))
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
   * If the flags register is not equal to SCM_F_COMPARE_LESS_THAN, add
   * OFFSET, a signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (209, jnl, "jnl", OP1 (X8_L24))
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
   * If the flags register is not equal to SCM_F_COMPARE_EQUAL, add
   * OFFSET, a signed 24-bit number, to the current instruction pointer.
   */
  VM_DEFINE_OP (210, jne, "jne", OP1 (X8_L24))
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
   * If the flags register is equal to SCM_F_COMPARE_NONE, add OFFSET, a
   * signed 24-bit number, to the current instruction pointer.  This is
   * intended for use after a "<?" comparison, and is different from
   * "jnl" in the way it handles not-a-number (NaN) values: "<?" sets
   * SCM_F_COMPARE_UNORDERED instead of SCM_F_COMPARE_NONE if either
   * value is a NaN.  For exact numbers, "jge" is the same as "jnl".
   */
  VM_DEFINE_OP (211, jge, "jge", OP1 (X8_L24))
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
   * If the flags register is not equal to SCM_F_COMPARE_NONE, add
   * OFFSET, a signed 24-bit number, to the current instruction pointer.
   * This is intended for use after a "<?" comparison, and is different
   * from "jl" in the way it handles not-a-number (NaN) values: "<?"
   * sets SCM_F_COMPARE_UNORDERED instead of SCM_F_COMPARE_NONE if
   * either value is a NaN.  For exact numbers, "jnge" is the same as
   * "jl".
   */
  VM_DEFINE_OP (212, jnge, "jnge", OP1 (X8_L24))
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

  VM_DEFINE_OP (213, heap_numbers_equal, "heap-numbers-equal?", OP1 (X8_S12_S12))
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

  VM_DEFINE_OP (214, untag_fixnum, "untag-fixnum", DOP1 (X8_S12_S12))
    {
      uint16_t dst, src;

      UNPACK_12_12 (op, dst, src);

      SP_SET_S64 (dst, SCM_I_INUM (SP_REF (src)));

      NEXT (1);
    }

  VM_DEFINE_OP (215, tag_fixnum, "tag-fixnum", DOP1 (X8_S12_S12))
    {
      uint16_t dst, src;

      UNPACK_12_12 (op, dst, src);

      SP_SET (dst, SCM_I_MAKINUM (SP_REF_S64 (src)));

      NEXT (1);
    }

  /* srsh dst:8 a:8 b:8
   *
   * Shift the s64 value in A right by B bits, and place the result in
   * DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (216, srsh, "srsh", DOP1 (X8_S8_S8_S8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_S64 (dst, SCM_SRS (SP_REF_S64 (a), (SP_REF_U64 (b) & 63)));

      NEXT (1);
    }

  /* srsh/immediate dst:8 a:8 b:8
   *
   * Shift the s64 value in A right by the immediate B bits, and place
   * the result in DST.  Only the lower 6 bits of B are used.
   */
  VM_DEFINE_OP (217, srsh_immediate, "srsh/immediate", DOP1 (X8_S8_S8_C8))
    {
      uint8_t dst, a, b;

      UNPACK_8_8_8 (op, dst, a, b);

      SP_SET_S64 (dst, SCM_SRS (SP_REF_S64 (a), b & 63));

      NEXT (1);
    }

  VM_DEFINE_OP (218, s64_imm_numerically_equal, "s64-imm=?", OP1 (X8_S12_Z12))
    {
      uint16_t a;
      int64_t x, y;

      a = (op >> 8) & 0xfff;
      x = SP_REF_S64 (a);

      y = ((int32_t) op) >> 20; /* Sign extension.  */

      VP->compare_result = x == y ? SCM_F_COMPARE_EQUAL : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  VM_DEFINE_OP (219, u64_imm_less, "u64-imm<?", OP1 (X8_S12_C12))
    {
      uint16_t a;
      uint64_t x, y;

      UNPACK_12_12 (op, a, y);
      x = SP_REF_U64 (a);

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  VM_DEFINE_OP (220, imm_u64_less, "imm-u64<?", OP1 (X8_S12_C12))
    {
      uint16_t a;
      uint64_t x, y;

      UNPACK_12_12 (op, a, x);
      y = SP_REF_U64 (a);

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  VM_DEFINE_OP (221, s64_imm_less, "s64-imm<?", OP1 (X8_S12_Z12))
    {
      uint16_t a;
      int64_t x, y;

      a = (op >> 8) & 0xfff;
      x = SP_REF_S64 (a);

      y = ((int32_t) op) >> 20; /* Sign extension.  */

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

      NEXT (1);
    }

  VM_DEFINE_OP (222, imm_s64_less, "imm-s64<?", OP1 (X8_S12_Z12))
    {
      uint16_t a;
      int64_t x, y;

      a = (op >> 8) & 0xfff;
      y = SP_REF_S64 (a);

      x = ((int32_t) op) >> 20; /* Sign extension.  */

      VP->compare_result = x < y ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;

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

  VM_DEFINE_OP (223, u8_ref, "u8-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (uint8_t, U64);
  VM_DEFINE_OP (224, u16_ref, "u16-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (uint16_t, U64);
  VM_DEFINE_OP (225, u32_ref, "u32-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (uint32_t, U64);
  VM_DEFINE_OP (226, u64_ref, "u64-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (uint64_t, U64);

  VM_DEFINE_OP (227, u8_set, "u8-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (uint8_t, U64);
  VM_DEFINE_OP (228, u16_set, "u16-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (uint16_t, U64);
  VM_DEFINE_OP (229, u32_set, "u32-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (uint32_t, U64);
  VM_DEFINE_OP (230, u64_set, "u64-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (uint64_t, U64);

  VM_DEFINE_OP (231, s8_ref, "s8-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (int8_t, S64);
  VM_DEFINE_OP (232, s16_ref, "s16-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (int16_t, S64);
  VM_DEFINE_OP (233, s32_ref, "s32-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (int32_t, S64);
  VM_DEFINE_OP (234, s64_ref, "s64-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (int64_t, S64);

  VM_DEFINE_OP (235, s8_set, "s8-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (int8_t, S64);
  VM_DEFINE_OP (236, s16_set, "s16-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (int16_t, S64);
  VM_DEFINE_OP (237, s32_set, "s32-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (int32_t, S64);
  VM_DEFINE_OP (238, s64_set, "s64-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (int64_t, S64);

  VM_DEFINE_OP (239, f32_ref, "f32-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (float, F64);
  VM_DEFINE_OP (240, f64_ref, "f64-ref", DOP1 (X8_S8_S8_S8))
    PTR_REF (double, F64);

  VM_DEFINE_OP (241, f32_set, "f32-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (float, F64);
  VM_DEFINE_OP (242, f64_set, "f64-set!", OP1 (X8_S8_S8_S8))
    PTR_SET (double, F64);

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


#undef ABORT_CONTINUATION_HOOK
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
#undef POP_CONTINUATION_HOOK
#undef PUSH_CONTINUATION_HOOK
#undef RUN_HOOK
#undef RUN_HOOK0
#undef RUN_HOOK1
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
