#ifndef SCM_ATOMICS_INTERNAL_H
#define SCM_ATOMICS_INTERNAL_H

/* Copyright (C) 2016
 * Free Software Foundation, Inc.
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




#include <stdint.h>




#define HAVE_C11_ATOMICS (__STDC_VERSION__ >= 201112L && !defined(__STDC_NO_ATOMICS__))

#if HAVE_C11_ATOMICS

#include <stdatomic.h>
static inline uint32_t
scm_atomic_subtract_uint32 (uint32_t *obj, uint32_t arg)
{
  return atomic_fetch_sub (obj, arg);
}
static inline _Bool
scm_atomic_compare_and_swap_uint32 (uint32_t *obj, uint32_t *expected,
                                    uint32_t desired)
{
  return atomic_compare_exchange_weak (obj, expected, desired);
}

#else /* HAVE_C11_ATOMICS */

/* Fallback implementation using locks.  */
#include "libguile/threads.h"
static scm_i_pthread_mutex_t atomics_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;
static inline uint32_t
scm_atomic_subtract_uint32 (uint32_t *obj, uint32_t arg)
{
  uint32_t ret;
  scm_i_pthread_mutex_lock (&atomics_lock);
  ret = *obj;
  *obj -= arg;
  scm_i_pthread_mutex_unlock (&atomics_lock);
  return ret;
}
static inline int
scm_atomic_compare_and_swap_uint32 (uint32_t *obj, uint32_t *expected,
                                    uint32_t desired)
{
  int ret;
  scm_i_pthread_mutex_lock (&atomics_lock);
  if (*obj == *expected)
    {
      *obj = desired;
      ret = 1;
    }
  else
    {
      *expected = *obj;
      ret = 0;
    }
  scm_i_pthread_mutex_unlock (&atomics_lock);
  return ret;
}

#endif /* HAVE_C11_ATOMICS */

#endif /* SCM_ATOMICS_INTERNAL_H */
