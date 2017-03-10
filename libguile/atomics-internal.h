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




#ifdef HAVE_STDATOMIC_H

#include <stdatomic.h>

static inline uint32_t
scm_atomic_subtract_uint32 (uint32_t *loc, uint32_t arg)
{
  atomic_uint_least32_t *a_loc = (atomic_uint_least32_t *) loc;
  return atomic_fetch_sub (a_loc, arg);
}
static inline _Bool
scm_atomic_compare_and_swap_uint32 (uint32_t *loc, uint32_t *expected,
                                    uint32_t desired)
{
  atomic_uint_least32_t *a_loc = (atomic_uint_least32_t *) loc;
  return atomic_compare_exchange_weak (a_loc, expected, desired);
}
static inline void
scm_atomic_set_pointer (void **loc, void *val)
{
  atomic_uintptr_t *a_loc = (atomic_uintptr_t *) loc;
  atomic_store (a_loc, (uintptr_t) val);
}
static inline void *
scm_atomic_ref_pointer (void **loc)
{
  atomic_uintptr_t *a_loc = (atomic_uintptr_t *) loc;
  return (void *) atomic_load (a_loc);
}
static inline void
scm_atomic_set_scm (SCM *loc, SCM val)
{
  atomic_uintptr_t *a_loc = (atomic_uintptr_t *) loc;
  atomic_store (a_loc, SCM_UNPACK (val));
}
static inline SCM
scm_atomic_ref_scm (SCM *loc)
{
  atomic_uintptr_t *a_loc = (atomic_uintptr_t *) loc;
  return SCM_PACK (atomic_load (a_loc));
}
static inline SCM
scm_atomic_swap_scm (SCM *loc, SCM val)
{
  atomic_uintptr_t *a_loc = (atomic_uintptr_t *) loc;
  return SCM_PACK (atomic_exchange (a_loc, SCM_UNPACK (val)));
}
static inline _Bool
scm_atomic_compare_and_swap_scm (SCM *loc, SCM *expected, SCM desired)
{
  atomic_uintptr_t *a_loc = (atomic_uintptr_t *) loc;
  return atomic_compare_exchange_weak (a_loc,
                                       (uintptr_t *) expected,
                                       SCM_UNPACK (desired));
}
#else /* HAVE_STDATOMIC_H */

/* Fallback implementation using locks.  */
#include "libguile/threads.h"
static scm_i_pthread_mutex_t atomics_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;
static inline uint32_t
scm_atomic_subtract_uint32 (uint32_t *loc, uint32_t arg)
{
  uint32_t ret;
  scm_i_pthread_mutex_lock (&atomics_lock);
  ret = *loc;
  *loc -= arg;
  scm_i_pthread_mutex_unlock (&atomics_lock);
  return ret;
}
static inline int
scm_atomic_compare_and_swap_uint32 (uint32_t *loc, uint32_t *expected,
                                    uint32_t desired)
{
  int ret;
  scm_i_pthread_mutex_lock (&atomics_lock);
  if (*loc == *expected)
    {
      *loc = desired;
      ret = 1;
    }
  else
    {
      *expected = *loc;
      ret = 0;
    }
  scm_i_pthread_mutex_unlock (&atomics_lock);
  return ret;
}

static inline void
scm_atomic_set_pointer (void **loc, void *val)
{
  scm_i_pthread_mutex_lock (&atomics_lock);
  *loc = val;
  scm_i_pthread_mutex_unlock (&atomics_lock);
}
static inline void *
scm_atomic_ref_pointer (void **loc)
{
  void *ret;
  scm_i_pthread_mutex_lock (&atomics_lock);
  ret = *loc;
  scm_i_pthread_mutex_unlock (&atomics_lock);
  return ret;
}

static inline void
scm_atomic_set_scm (SCM *loc, SCM val)
{
  scm_i_pthread_mutex_lock (&atomics_lock);
  *loc = val;
  scm_i_pthread_mutex_unlock (&atomics_lock);
}
static inline SCM
scm_atomic_ref_scm (SCM *loc)
{
  SCM ret;
  scm_i_pthread_mutex_lock (&atomics_lock);
  ret = *loc;
  scm_i_pthread_mutex_unlock (&atomics_lock);
  return ret;
}
static inline SCM
scm_atomic_swap_scm (SCM *loc, SCM val)
{
  SCM ret;
  scm_i_pthread_mutex_lock (&atomics_lock);
  ret = *loc;
  *loc = val;
  scm_i_pthread_mutex_unlock (&atomics_lock);
  return ret;
}
static inline int
scm_atomic_compare_and_swap_scm (SCM *loc, SCM *expected, SCM desired)
{
  int ret;
  scm_i_pthread_mutex_lock (&atomics_lock);
  if (*loc == *expected)
    {
      *loc = desired;
      ret = 1;
    }
  else
    {
      *expected = *loc;
      ret = 0;
    }
  scm_i_pthread_mutex_unlock (&atomics_lock);
  return ret;
}

#endif /* HAVE_STDATOMIC_H */

#endif /* SCM_ATOMICS_INTERNAL_H */
