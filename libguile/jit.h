#ifndef SCM_JIT_H
#define SCM_JIT_H

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



#include "libguile/scm.h"



struct scm_jit_function_data;
struct scm_jit_state;

#ifdef BUILDING_LIBGUILE
struct scm_jit_function_data
{
  uint8_t *mcode;
  uint32_t counter;
  int32_t start;
  int32_t end;
#if SCM_SIZEOF_UINTPTR_T == 4
#elif SCM_SIZEOF_UINTPTR_T == 8
  uint32_t pad;
#else
#error unhandled sizeof(uintptr_t)
#endif
};

/* These values should be even, so that a function's counter is never
   0xffffffff, so that setting the JIT threshold to 0xffffffff always
   disables compilation.  */
enum scm_jit_counter_value
{
  SCM_JIT_COUNTER_ENTRY_INCREMENT = 30,
  SCM_JIT_COUNTER_LOOP_INCREMENT = 2,
};
#endif

SCM_INTERNAL uint32_t scm_jit_counter_threshold;

SCM_INTERNAL const uint8_t *scm_jit_compute_mcode (scm_thread *thread,
                                                   struct scm_jit_function_data *data);
SCM_INTERNAL void scm_jit_enter_mcode (scm_thread *thread,
                                       const uint8_t *mcode);
SCM_INTERNAL void scm_jit_state_free (struct scm_jit_state *j);

SCM_INTERNAL void *scm_jit_return_to_interpreter_trampoline;
SCM_INTERNAL void scm_jit_clear_mcode_return_addresses (scm_thread *thread);

SCM_INTERNAL void scm_init_jit (void);

#endif  /* SCM_JIT_H */
