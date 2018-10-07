#ifndef SCM_DEPRECATED_H
#define SCM_DEPRECATED_H

/* Copyright 2003-2007,2009-2018
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

#include "libguile/snarf.h"

#if (SCM_ENABLE_DEPRECATED == 1)

/* Deprecated declarations go here.  */

/* Return true (non-zero) if GCC version MAJ.MIN or later is being used
 * (macro taken from glibc.)  */
#if defined __GNUC__ && defined __GNUC_MINOR__
# define SCM_GNUC_PREREQ(maj, min) \
	((__GNUC__ << 16) + __GNUC_MINOR__ >= ((maj) << 16) + (min))
#else
# define SCM_GNUC_PREREQ(maj, min) 0
#endif

#define scm_i_jmp_buf scm_i_jmp_buf_GONE__USE_JMP_BUF_INSTEAD

#define SCM_VALIDATE_VECTOR_OR_DVECTOR(pos, v) \
  do { \
    SCM_ASSERT (scm_is_vector (v) || scm_is_true (scm_f64vector_p (v)), \
                v, pos, FUNC_NAME); \
  } while (0)

#ifdef SCM_SUPPORT_STATIC_ALLOCATION
#define SCM_STATIC_DOUBLE_CELL(c_name, car, cbr, ccr, cdr)		\
  static SCM_ALIGNED (8) SCM_UNUSED scm_t_cell                          \
  c_name ## _raw_cell [2] =						\
    {									\
      { SCM_PACK (car), SCM_PACK (cbr) },				\
      { SCM_PACK (ccr), SCM_PACK (cdr) }				\
    };									\
  static SCM_UNUSED SCM c_name = SCM_PACK (& c_name ## _raw_cell)
#endif /* SCM_SUPPORT_STATIC_ALLOCATION */

#define scm_gc_running_p  0

#define SCM_I_UTYPE_MAX(type)      ((type)-1)
#define SCM_I_TYPE_MAX(type,umax)  ((type)((umax)/2))
#define SCM_I_TYPE_MIN(type,umax)  (-((type)((umax)/2))-1)

#define SCM_T_UINT8_MAX   UINT8_MAX
#define SCM_T_INT8_MIN    INT8_MIN
#define SCM_T_INT8_MAX    INT8_MAX

#define SCM_T_UINT16_MAX  UINT16_MAX
#define SCM_T_INT16_MIN   INT16_MIN
#define SCM_T_INT16_MAX   INT16_MAX

#define SCM_T_UINT32_MAX  UINT32_MAX
#define SCM_T_INT32_MIN   INT32_MIN
#define SCM_T_INT32_MAX   INT32_MAX

#define SCM_T_UINT64_MAX  UINT64_MAX
#define SCM_T_INT64_MIN   INT64_MIN
#define SCM_T_INT64_MAX   INT64_MAX

#define SCM_T_UINTMAX_MAX UINTMAX_MAX
#define SCM_T_INTMAX_MIN  INTMAX_MIN
#define SCM_T_INTMAX_MAX  INTMAX_MAX

#define SCM_T_UINTPTR_MAX UINTPTR_MAX
#define SCM_T_INTPTR_MIN  INTPTR_MIN
#define SCM_T_INTPTR_MAX  INTPTR_MAX

#define SCM_HAVE_T_INT64 1 /* 0 or 1 */
#define SCM_HAVE_T_UINT64 1 /* 0 or 1 */

#define SCM_HAVE_ARRAYS 1 /* always true now */

#ifdef __GNUC__
#define SCM_DEPRECATED_TYPE __attribute__((__deprecated__))
#else
#define SCM_DEPRECATED_TYPE /*deprecated*/
#endif
typedef int8_t scm_t_int8 SCM_DEPRECATED_TYPE;
typedef uint8_t scm_t_uint8 SCM_DEPRECATED_TYPE;
typedef int16_t scm_t_int16 SCM_DEPRECATED_TYPE;
typedef uint16_t scm_t_uint16 SCM_DEPRECATED_TYPE;
typedef int32_t scm_t_int32 SCM_DEPRECATED_TYPE;
typedef uint32_t scm_t_uint32 SCM_DEPRECATED_TYPE;
typedef intmax_t scm_t_intmax SCM_DEPRECATED_TYPE;
typedef uintmax_t scm_t_uintmax SCM_DEPRECATED_TYPE;
typedef intptr_t scm_t_intptr SCM_DEPRECATED_TYPE;
typedef uintptr_t scm_t_uintptr SCM_DEPRECATED_TYPE;
typedef int64_t scm_t_int64 SCM_DEPRECATED_TYPE;
typedef uint64_t scm_t_uint64 SCM_DEPRECATED_TYPE;
typedef ptrdiff_t scm_t_ptrdiff SCM_DEPRECATED_TYPE;

typedef struct scm_thread scm_i_thread SCM_DEPRECATED_TYPE;
#undef SCM_DEPRECATED_TYPE

SCM_DEPRECATED char* scm_find_executable (const char *name);

void scm_i_init_deprecated (void);

#endif

#endif /* SCM_DEPRECATED_H */
