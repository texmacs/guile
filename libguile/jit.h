#ifndef SCM_JIT_H
#define SCM_JIT_H

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



#include "libguile/scm.h"



struct scm_jit_function_data;

#ifdef BUILDING_LIBGUILE
struct scm_jit_function_data
{
  uint8_t *mcode;
  uint32_t counter;
  uint32_t start;
  uint32_t end;
#if SCM_SIZEOF_UINTPTR_T == 4
#elif SCM_SIZEOF_UINTPTR_T == 8
  uint32_t pad;
#else
#error unhandled sizeof(uintptr_t)
#endif
};

enum scm_jit_counter_value
{
  SCM_JIT_COUNTER_ENTRY_INCREMENT = 15,
  SCM_JIT_COUNTER_LOOP_INCREMENT = 1,
  SCM_JIT_COUNTER_THRESHOLD = 50
};
#endif

SCM_INTERNAL const uint8_t *scm_jit_compute_mcode (scm_thread *thread,
                                                   struct scm_jit_function_data *data);
SCM_INTERNAL void scm_jit_enter_mcode (scm_thread *thread, const uint8_t *mcode);

SCM_INTERNAL void scm_init_jit (void);

#endif  /* SCM_JIT_H */
