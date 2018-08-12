#ifndef SCM_CONTINUATIONS_H
#define SCM_CONTINUATIONS_H

/* Copyright 1995-1996,2000-2001,2006,2008-2010,2012-2014,2018
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



#include <setjmp.h>

#include "libguile/programs.h"
#include "libguile/throw.h"



#define SCM_CONTINUATIONP(x) \
  (SCM_PROGRAM_P (x) && SCM_PROGRAM_IS_CONTINUATION (x))

/* a continuation SCM is a non-immediate pointing to a heap cell with:
   word 0: bits 0-15: smob type tag: scm_tc16_continuation.
           bits 16-31: unused.
   word 1: malloc block containing an scm_t_contregs structure with a
           tail array of SCM_STACKITEM.  the size of the array is stored
	   in the num_stack_items field of the structure.
*/

typedef struct 
{
  jmp_buf jmpbuf;
#if SCM_HAVE_AUXILIARY_STACK
  void *auxiliary_stack;
  unsigned long auxiliary_stack_size;
#endif
  size_t num_stack_items;   /* size of the saved stack.  */
  SCM root;                 /* continuation root identifier.  */
  SCM vm_cont;              /* vm's stack and regs */

  /* The offset from the live stack location to this copy.  This is
     used to adjust pointers from within the copied stack to the stack
     itself.

     Thus, when you read a pointer from the copied stack that points
     into the live stack, you need to add OFFSET so that it points
     into the copy.
  */
  ptrdiff_t offset;

  SCM_STACKITEM stack[1];    /* copied stack of size num_stack_items.  */ 
} scm_t_contregs;




SCM_INTERNAL SCM scm_i_make_continuation (scm_thread *thread, SCM vm_cont);
SCM_INTERNAL void scm_i_reinstate_continuation (SCM cont,
                                                uint8_t *mra) SCM_NORETURN;

SCM_INTERNAL int scm_i_continuation_to_frame (SCM cont,
                                              struct scm_frame *frame);

SCM_INTERNAL scm_t_contregs* scm_i_contregs (SCM contregs);

SCM_API void *scm_c_with_continuation_barrier (void *(*func)(void*), void *);
SCM_API SCM scm_with_continuation_barrier (SCM proc);

SCM_INTERNAL SCM
scm_i_with_continuation_barrier (scm_t_catch_body body,
				 void *body_data,
				 scm_t_catch_handler handler,
				 void *handler_data,
				 scm_t_catch_handler pre_unwind_handler,
				 void *pre_unwind_handler_data);

SCM_INTERNAL void scm_init_continuations (void);

#endif  /* SCM_CONTINUATIONS_H */
