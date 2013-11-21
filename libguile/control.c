/* Copyright (C) 2010, 2011, 2012, 2013  Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <alloca.h>

#include "libguile/_scm.h"
#include "libguile/control.h"
#include "libguile/programs.h"
#include "libguile/instructions.h"
#include "libguile/vm.h"



#define PROMPT_ESCAPE_P(p)                              \
  (SCM_DYNSTACK_TAG_FLAGS (SCM_DYNSTACK_TAG (p))        \
   & SCM_F_DYNSTACK_PROMPT_ESCAPE_ONLY)




/* Only to be called if the SCM_I_SETJMP returns 1 */
SCM
scm_i_prompt_pop_abort_args_x (struct scm_vm *vp)
{
  size_t i, n;
  SCM vals = SCM_EOL;

  n = scm_to_size_t (vp->sp[0]);
  for (i = 0; i < n; i++)
    vals = scm_cons (vp->sp[-(i + 1)], vals);

  /* The abort did reset the VM's registers, but then these values
     were pushed on; so we need to pop them ourselves. */
  vp->sp -= n + 1;
  /* FIXME NULLSTACK */

  return vals;
}


static const scm_t_uint32 compose_continuation_code[] =
  {
    SCM_PACK_OP_24 (compose_continuation, 0)
  };


static SCM
make_partial_continuation (SCM vm_cont)
{
  scm_t_bits nfree = 1;
  scm_t_bits flags = SCM_F_PROGRAM_IS_PARTIAL_CONTINUATION;
  SCM ret;

  ret = scm_words (scm_tc7_program | (nfree << 16) | flags, nfree + 2);
  SCM_SET_CELL_WORD_1 (ret, compose_continuation_code);
  SCM_PROGRAM_FREE_VARIABLE_SET (ret, 0, vm_cont);

  return ret;
}

static SCM
reify_partial_continuation (struct scm_vm *vp,
                            SCM *saved_fp,
                            SCM *saved_sp,
                            scm_t_uint32 *saved_ip,
                            scm_i_jmp_buf *saved_registers,
                            scm_t_dynstack *dynstack,
                            scm_i_jmp_buf *current_registers)
{
  SCM vm_cont;
  scm_t_uint32 flags;
  SCM *bottom_fp;

  flags = SCM_F_VM_CONT_PARTIAL;
  /* If we are aborting to a prompt that has the same registers as those
     of the abort, it means there are no intervening C frames on the
     stack, and so the continuation can be relocated elsewhere on the
     stack: it is rewindable.  */
  if (saved_registers && saved_registers == current_registers)
    flags |= SCM_F_VM_CONT_REWINDABLE;

  /* Walk the stack down until we find the first frame after saved_fp.
     We will save the stack down to that frame.  It used to be that we
     could determine the stack bottom in O(1) time, but that's no longer
     the case, since the thunk application doesn't occur where the
     prompt is saved.  */
  for (bottom_fp = vp->fp;
       SCM_FRAME_DYNAMIC_LINK (bottom_fp) > saved_fp;
       bottom_fp = SCM_FRAME_DYNAMIC_LINK (bottom_fp));

  if (SCM_FRAME_DYNAMIC_LINK (bottom_fp) != saved_fp)
    abort();

  /* Capture from the top of the thunk application frame up to the end. */
  vm_cont = scm_i_vm_capture_stack (&SCM_FRAME_LOCAL (bottom_fp, 0),
                                    vp->fp,
                                    vp->sp,
                                    vp->ip,
                                    dynstack,
                                    flags);

  return make_partial_continuation (vm_cont);
}

void
scm_c_abort (struct scm_vm *vp, SCM tag, size_t n, SCM *argv,
             scm_i_jmp_buf *current_registers)
{
  SCM cont;
  scm_t_dynstack *dynstack = &SCM_I_CURRENT_THREAD->dynstack;
  scm_t_bits *prompt;
  scm_t_dynstack_prompt_flags flags;
  scm_t_ptrdiff fp_offset, sp_offset;
  SCM *fp, *sp;
  scm_t_uint32 *ip;
  scm_i_jmp_buf *registers;
  size_t i;

  prompt = scm_dynstack_find_prompt (dynstack, tag,
                                     &flags, &fp_offset, &sp_offset, &ip,
                                     &registers);

  if (!prompt)
    scm_misc_error ("abort", "Abort to unknown prompt", scm_list_1 (tag));

  fp = vp->stack_base + fp_offset;
  sp = vp->stack_base + sp_offset;

  /* Only reify if the continuation referenced in the handler. */
  if (flags & SCM_F_DYNSTACK_PROMPT_ESCAPE_ONLY)
    cont = SCM_BOOL_F;
  else
    {
      scm_t_dynstack *captured;

      captured = scm_dynstack_capture (dynstack, SCM_DYNSTACK_NEXT (prompt));
      cont = reify_partial_continuation (vp, fp, sp, ip, registers, captured,
                                         current_registers);
    }

  /* Unwind.  */
  scm_dynstack_unwind (dynstack, prompt);

  /* Restore VM regs */
  vp->fp = fp;
  vp->sp = sp;
  vp->ip = ip;

  /* Since we're jumping down, we should always have enough space.  */
  if (vp->sp + n + 1 >= vp->stack_limit)
    abort ();

  /* Push vals */
  *(++(vp->sp)) = cont;
  for (i = 0; i < n; i++)
    *(++(vp->sp)) = argv[i];
  if (flags & SCM_F_DYNSTACK_PROMPT_PUSH_NARGS)
    *(++(vp->sp)) = scm_from_size_t (n+1); /* +1 for continuation */

  /* Jump! */
  SCM_I_LONGJMP (*registers, 1);

  /* Shouldn't get here */
  abort ();
}

SCM_DEFINE (scm_abort_to_prompt_star, "abort-to-prompt*", 2, 0, 0,
            (SCM tag, SCM args),
            "Abort to the nearest prompt with tag @var{tag}, yielding the\n"
            "values in the list, @var{args}.")
#define FUNC_NAME s_scm_abort_to_prompt_star
{
  SCM *argv;
  size_t i;
  long n;

  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG2, args, n);
  argv = alloca (sizeof (SCM)*n);
  for (i = 0; i < n; i++, args = scm_cdr (args))
    argv[i] = scm_car (args);

  scm_c_abort (scm_the_vm (), tag, n, argv, NULL);

  /* Oh, what, you're still here? The abort must have been reinstated. Actually,
     that's quite impossible, given that we're already in C-land here, so...
     abort! */

  abort ();
}
#undef FUNC_NAME

void
scm_init_control (void)
{
#include "libguile/control.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
