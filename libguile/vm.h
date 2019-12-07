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

#ifndef _SCM_VM_H_
#define _SCM_VM_H_

#include <setjmp.h>

#include <libguile/gc.h>
#include <libguile/programs.h>

#define SCM_VM_REGULAR_ENGINE 0
#define SCM_VM_DEBUG_ENGINE 1
#define SCM_VM_NUM_ENGINES 2

enum scm_compare {
  SCM_F_COMPARE_NONE = 0x0,
  SCM_F_COMPARE_EQUAL = 0x1,
  SCM_F_COMPARE_LESS_THAN = 0x2,
  SCM_F_COMPARE_INVALID = 0x3
};

struct scm_vm {
  uint32_t *ip;		/* instruction pointer */
  union scm_vm_stack_element *sp; /* stack pointer */
  union scm_vm_stack_element *fp; /* frame pointer */
  union scm_vm_stack_element *stack_limit; /* stack limit address */
  uint8_t compare_result;       /* flags register: a value from scm_compare */
  uint8_t apply_hook_enabled;   /* if apply hook is enabled */
  uint8_t return_hook_enabled;  /* if return hook is enabled */
  uint8_t next_hook_enabled;    /* if next hook is enabled */
  uint8_t abort_hook_enabled;   /* if abort hook is enabled */
  uint8_t disable_mcode;        /* if mcode is disabled (because debugging) */
  uint8_t engine;               /* which vm engine we're using */
  uint8_t unused;               /* padding */
  size_t stack_size;		/* stack size */
  union scm_vm_stack_element *stack_bottom; /* lowest address in allocated stack */
  SCM apply_hook;               /* apply hook */
  SCM return_hook;              /* return hook */
  SCM next_hook;                /* next hook */
  SCM abort_hook;               /* abort hook */
  union scm_vm_stack_element *stack_top; /* highest address in allocated stack */
  SCM overflow_handler_stack;   /* alist of max-stack-size -> thunk */
  jmp_buf *registers;           /* registers captured at latest vm entry  */
  uint8_t *mra_after_abort;     /* mra to resume after nonlocal exit, or NULL */
  int trace_level;              /* traces enabled if trace_level > 0 */
};

SCM_API SCM scm_call_with_vm (SCM proc, SCM args);

SCM_API SCM scm_call_with_stack_overflow_handler (SCM limit, SCM thunk,
                                                  SCM handler);

SCM_INTERNAL SCM scm_vm_add_apply_hook_x (SCM);
SCM_INTERNAL SCM scm_vm_add_return_hook_x (SCM);
SCM_INTERNAL SCM scm_vm_add_abort_hook_x (SCM);
SCM_INTERNAL SCM scm_vm_add_next_hook_x (SCM);
SCM_INTERNAL SCM scm_vm_remove_apply_hook_x (SCM);
SCM_INTERNAL SCM scm_vm_remove_return_hook_x (SCM);
SCM_INTERNAL SCM scm_vm_remove_abort_hook_x (SCM);
SCM_INTERNAL SCM scm_vm_remove_next_hook_x (SCM);
SCM_API SCM scm_vm_trace_level (void);
SCM_API SCM scm_set_vm_trace_level_x (SCM level);
SCM_API SCM scm_vm_engine (void);
SCM_API SCM scm_set_vm_engine_x (SCM engine);
SCM_API SCM scm_set_default_vm_engine_x (SCM engine);
SCM_API void scm_c_set_vm_engine_x (int engine);
SCM_API void scm_c_set_default_vm_engine_x (int engine);

SCM_INTERNAL void scm_i_vm_prepare_stack (struct scm_vm *vp);
struct GC_ms_entry;
SCM_INTERNAL struct GC_ms_entry * scm_i_vm_mark_stack (struct scm_vm *,
                                                       struct GC_ms_entry *,
                                                       struct GC_ms_entry *);
SCM_INTERNAL void scm_i_vm_free_stack (struct scm_vm *vp);

#define SCM_F_VM_CONT_PARTIAL 0x1
#define SCM_F_VM_CONT_REWINDABLE 0x2

struct scm_vm_cont {
  /* IP of newest frame.  */
  uint32_t *vra;
  /* Machine code corresponding to IP.  */
  uint8_t *mra;
  /* Offset of FP of newest frame, relative to stack top.  */
  ptrdiff_t fp_offset;
  /* Besides being the stack size, this is also the offset of the SP of
     the newest frame.  */
  ptrdiff_t stack_size;
  /* Stack bottom, which also keeps saved stack alive for GC.  */
  union scm_vm_stack_element *stack_bottom;
  /* Saved dynamic stack, with prompts relocated to record saved SP/FP
     offsets from the stack top of this scm_vm_cont.  */
  scm_t_dynstack *dynstack;
  /* See the continuation is partial and/or rewindable.  */
  uint32_t flags;
};

#define SCM_VM_CONT_P(OBJ)	(SCM_HAS_TYP7 (OBJ, scm_tc7_vm_cont))
#define SCM_VM_CONT_DATA(CONT)	((struct scm_vm_cont *) SCM_CELL_WORD_1 (CONT))
#define SCM_VM_CONT_PARTIAL_P(CONT) (SCM_VM_CONT_DATA (CONT)->flags & SCM_F_VM_CONT_PARTIAL)
#define SCM_VM_CONT_REWINDABLE_P(CONT) (SCM_VM_CONT_DATA (CONT)->flags & SCM_F_VM_CONT_REWINDABLE)

SCM_API SCM scm_load_compiled_with_vm (SCM file);

SCM_INTERNAL SCM scm_i_call_with_current_continuation (SCM proc);
SCM_INTERNAL SCM scm_i_capture_current_stack (void);
SCM_INTERNAL void scm_i_vm_abort (SCM *tag_and_argv, size_t n) SCM_NORETURN;
SCM_INTERNAL void scm_i_vm_emergency_abort (SCM *tag_and_argv, size_t n) SCM_NORETURN;
SCM_INTERNAL int scm_i_vm_cont_to_frame (SCM cont, struct scm_frame *frame);
SCM_INTERNAL void scm_i_vm_cont_print (SCM x, SCM port,
                                       scm_print_state *pstate);
SCM_INTERNAL int scm_i_vm_is_boot_continuation_code (uint32_t *ip);
SCM_INTERNAL void scm_bootstrap_vm (void);
SCM_INTERNAL void scm_init_vm (void);

#endif /* _SCM_VM_H_ */
