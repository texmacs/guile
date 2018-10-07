/* Copyright 2010-2013,2018
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <alloca.h>

#include "dynstack.h"
#include "extensions.h"
#include "frames.h"
#include "gsubr.h"
#include "instructions.h"
#include "jit.h"
#include "list.h"
#include "pairs.h"
#include "programs.h"
#include "threads.h"
#include "version.h"
#include "vm.h"

#include "control.h"



#define PROMPT_ESCAPE_P(p)                              \
  (SCM_DYNSTACK_TAG_FLAGS (SCM_DYNSTACK_TAG (p))        \
   & SCM_F_DYNSTACK_PROMPT_ESCAPE_ONLY)




/* Only to be called if the setjmp returns 1 */
SCM
scm_i_prompt_pop_abort_args_x (struct scm_vm *vp,
                               ptrdiff_t saved_stack_depth)
{
  size_t i, n;
  ptrdiff_t stack_depth;
  SCM vals = SCM_EOL;

  stack_depth = vp->stack_top - vp->sp;
  if (stack_depth < saved_stack_depth)
    abort ();
  n = stack_depth - saved_stack_depth;

  for (i = 0; i < n; i++)
    vals = scm_cons (vp->sp[i].as_scm, vals);

  vp->sp += n;

  return vals;
}


struct compose_continuation_code
{
  struct scm_jit_function_data data;
  uint32_t code[3];
};

struct compose_continuation_code compose_continuation_code = {
  {
    /* mcode = */ 0,
    /* counter = */ 0,
    /* start = */ sizeof (struct scm_jit_function_data),
    /* end = */ sizeof (struct scm_jit_function_data) + 12
  },
  {
    SCM_PACK_OP_24 (instrument_entry, 0),
    ((uint32_t) -(sizeof (struct scm_jit_function_data) / 4)),
    SCM_PACK_OP_24 (compose_continuation, 0),
  }
};

SCM
scm_i_make_composable_continuation (SCM vmcont)
{
  scm_t_bits nfree = 1;
  scm_t_bits flags = SCM_F_PROGRAM_IS_PARTIAL_CONTINUATION;
  SCM ret;

  ret = scm_words (scm_tc7_program | (nfree << 16) | flags, nfree + 2);
  SCM_SET_CELL_WORD_1 (ret, compose_continuation_code.code);
  SCM_PROGRAM_FREE_VARIABLE_SET (ret, 0, vmcont);

  return ret;
}

SCM_DEFINE (scm_abort_to_prompt_star, "abort-to-prompt*", 2, 0, 0,
            (SCM tag, SCM args),
            "Abort to the nearest prompt with tag @var{tag}, yielding the\n"
            "values in the list, @var{args}.")
#define FUNC_NAME s_scm_abort_to_prompt_star
{
  SCM *tag_and_argv;
  size_t i;
  long n;

  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG2, args, n);
  n = n + 1; /* Add space for the tag.  */
  tag_and_argv = alloca (sizeof (SCM)*(n+1));
  tag_and_argv[0] = tag;
  for (i = 1; i < n; i++, args = scm_cdr (args))
    tag_and_argv[i] = scm_car (args);

  scm_i_vm_abort (tag_and_argv, n);

  /* Oh, what, you're still here? The abort must have been reinstated. Actually,
     that's quite impossible, given that we're already in C-land here, so...
     abort! */

  abort ();
}
#undef FUNC_NAME

static SCM
scm_suspendable_continuation_p (SCM tag)
{
  scm_t_dynstack_prompt_flags flags;
  scm_thread *thread = SCM_I_CURRENT_THREAD;
  jmp_buf *registers;

  if (scm_dynstack_find_prompt (&thread->dynstack, tag, &flags,
                                NULL, NULL, NULL, NULL, &registers))
    return scm_from_bool (registers == thread->vm.registers);

  return SCM_BOOL_F;
}

static void
scm_init_ice_9_control (void *unused)
{
  scm_c_define_gsubr ("suspendable-continuation?", 1, 0, 0,
                      scm_suspendable_continuation_p);
}

void
scm_init_control (void)
{
#include "control.x"

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_ice_9_control", scm_init_ice_9_control,
			    NULL);
}
