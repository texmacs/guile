/* srfi-4.c --- Homogeneous numeric vector datatypes.
 *
 * 	Copyright (C) 2001 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives
 * permission for additional uses of the text contained in its release
 * of GUILE.
 *
 * The exception is that, if you link the GUILE library with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public
 * License.  Your use of that executable is in no way restricted on
 * account of linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public
 * License.
 *
 * This exception applies only to the code released by the Free
 * Software Foundation under the name GUILE.  If you copy code from
 * other Free Software Foundation releases into a copy of GUILE, as
 * the General Public License permits, the exception does not apply to
 * the code that you add in this way.  To avoid misleading anyone as
 * to the status of such modified files, you must delete this
 * exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

#include <libguile.h>

#include "srfi-4.h"

#include <string.h>
#include <stdio.h>

/* For brevity and maintainability, we define our own types for the
   various integer and floating point types.  */
typedef unsigned char int_u8;
typedef signed char int_s8;
typedef unsigned short int_u16;
typedef signed short int_s16;
typedef unsigned int int_u32;
typedef signed int int_s32;

#if SIZEOF_LONG == 8
typedef unsigned long int_u64;
typedef signed long int_s64;
#define HAVE_UVEC_64 1
#elif SIZEOF_LONG_LONG == 8
typedef unsigned long long int_u64;
typedef signed long long int_s64;
#define HAVE_UVEC_64 1
#else
#define HAVE_UVEC_64 0
#endif /* no 64-bit integer support */

typedef float float_f32;
typedef double float_f64;


/* Smob type code for homogeneous numeric vectors.  */
int scm_tc16_uvec = 0;


/* Accessor macros for the three components of a homogeneous numeric
   vector:
   - The type tag (one of the symbolic constants below).
   - The vector's length (counted in elements).
   - The address of the data area (holding the elements of the
     vector). */
#define SCM_UVEC_TYPE(u)   (SCM_CELL_WORD_1(u))
#define SCM_UVEC_LENGTH(u) (SCM_CELL_WORD_2(u))
#define SCM_UVEC_BASE(u)   (SCM_CELL_OBJECT_3(u))


/* Symbolic constants encoding the various types of homogeneous
   numeric vectors.  */
#define SCM_UVEC_U8  	0
#define SCM_UVEC_S8  	1
#define SCM_UVEC_U16 	2
#define SCM_UVEC_S16 	3
#define SCM_UVEC_U32 	4
#define SCM_UVEC_S32 	5
#define SCM_UVEC_U64 	6
#define SCM_UVEC_S64 	7
#define SCM_UVEC_F32 	8
#define SCM_UVEC_F64 	9

#define VALIDATE_UVEC(pos, obj, type) \
  do { \
    SCM_VALIDATE_SMOB (pos, obj, uvec); \
    if (SCM_UVEC_TYPE (obj) != type) \
      scm_wrong_type_arg (FUNC_NAME, 1, obj); \
  } while (0)

#define RANGE_CHECK_AND_COPY_UVEC_INDEX(pos, uvec, i, cindex) \
  do { \
    cindex = scm_num2size (i, pos, FUNC_NAME); \
    if (cindex < 0 || cindex >= SCM_UVEC_LENGTH (uvec)) \
      scm_out_of_range_pos (FUNC_NAME, i, SCM_MAKINUM (pos)); \
  } while (0)


/* This array maps type tags to the size of the elements.  */
static const int uvec_sizes[] = {1, 1, 2, 2, 4, 4, 8, 8, 4, 8};
/* This array maps type tags to the bit shifts related to the sizes. */
static const int uvec_shifts[] = {0, 0, 1, 1, 2, 2, 3, 3, 2, 3};

#if HAVE_UVEC_64

/* This is a modified version of scm_iint2str and should go away once
   we have a public scm_print_integer or similar. */

static void
print_int64 (int_s64 num, SCM port)
{
  char num_buf[SCM_INTBUFLEN];
  char *p = num_buf;
  const int rad = 10;
  size_t num_chars = 1;
  size_t i;
  int_u64 n = (num < 0) ? -num : num;

  for (n /= rad; n > 0; n /= rad)
    num_chars++;

  i = num_chars;
  if (num < 0)
    {
      *p++ = '-';
      num_chars++;
      n = -num;
    }
  else
    n = num;
  while (i--)
    {
      int d = n % rad;

      n /= rad;
      p[i] = d + ((d < 10) ? '0' : 'a' - 10);
    }

  scm_lfwrite (num_buf, num_chars, port);
}

/* This is a modified version of scm_iint2str and should go away once
   we have a public scm_print_integer or similar. */

static void
print_uint64 (int_u64 num, SCM port)
{
  char num_buf[SCM_INTBUFLEN];
  char *p = num_buf;
  const int rad = 10;
  size_t num_chars = 1;
  size_t i;
  int_u64 n = num;

  for (n /= rad; n > 0; n /= rad)
    num_chars++;

  i = num_chars;
  n = num;
  while (i--)
    {
      int d = n % rad;

      n /= rad;
      p[i] = d + ((d < 10) ? '0' : 'a' - 10);
    }

  scm_lfwrite (num_buf, num_chars, port);
}


#endif /* HAVE_UVEC_64 */

static void
print_uint32 (int_u32 num, SCM port)
{
  char num_buf[SCM_INTBUFLEN];
  char *p = num_buf;
  const int rad = 10;
  size_t num_chars = 1;
  size_t i;
  int_u32 n = num;

  for (n /= rad; n > 0; n /= rad)
    num_chars++;

  i = num_chars;
  n = num;
  while (i--)
    {
      int d = n % rad;

      n /= rad;
      p[i] = d + ((d < 10) ? '0' : 'a' - 10);
    }

  scm_lfwrite (num_buf, num_chars, port);
}

/* ================================================================ */
/* SMOB procedures.                                                 */
/* ================================================================ */


/* Smob print hook for homogeneous vectors.  */
static int
uvec_print (SCM uvec, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  union {
    int_u8 *u8;
    int_s8 *s8;
    int_u16 *u16;
    int_s16 *s16;
    int_u32 *u32;
    int_s32 *s32;
#if HAVE_UVEC_64
    int_u64 *u64;
    int_s64 *s64;
#endif
    float_f32 *f32;
    float_f64 *f64;
  } np;

  size_t i = 0; /* since SCM_UVEC_LENGTH will return something this size. */
  const size_t uvlen = SCM_UVEC_LENGTH (uvec);
  char *tagstr;
  void *uptr = SCM_UVEC_BASE (uvec);

  switch (SCM_UVEC_TYPE (uvec))
  {
    case SCM_UVEC_U8: tagstr = "u8"; np.u8 = (int_u8 *) uptr; break;
    case SCM_UVEC_S8: tagstr = "s8"; np.s8 = (int_s8 *) uptr; break;
    case SCM_UVEC_U16: tagstr = "u16"; np.u16 = (int_u16 *) uptr; break;
    case SCM_UVEC_S16: tagstr = "s16"; np.s16 = (int_s16 *) uptr; break;
    case SCM_UVEC_U32: tagstr = "u32"; np.u32 = (int_u32 *) uptr; break;
    case SCM_UVEC_S32: tagstr = "s32"; np.s32 = (int_s32 *) uptr; break;
#if HAVE_UVEC_64
    case SCM_UVEC_U64: tagstr = "u64"; np.u64 = (int_u64 *) uptr; break;
    case SCM_UVEC_S64: tagstr = "s64"; np.s64 = (int_s64 *) uptr; break;
#endif
    case SCM_UVEC_F32: tagstr = "f32"; np.f32 = (float_f32 *) uptr; break;
    case SCM_UVEC_F64: tagstr = "f64"; np.f64 = (float_f64 *) uptr; break;
    default:
      abort ();			/* Sanity check.  */
      break;
  }

  scm_putc ('#', port);
  scm_puts (tagstr, port);
  scm_putc ('(', port);

  while (i < uvlen)
  {
    if (i != 0) scm_puts (" ", port);
    switch (SCM_UVEC_TYPE (uvec))
    {
      case SCM_UVEC_U8: scm_intprint (*np.u8, 10, port); np.u8++; break;
      case SCM_UVEC_S8: scm_intprint (*np.s8, 10, port); np.s8++; break;
      case SCM_UVEC_U16: scm_intprint (*np.u16, 10, port); np.u16++; break;
      case SCM_UVEC_S16: scm_intprint (*np.s16, 10, port); np.s16++; break;
      case SCM_UVEC_U32: print_uint32 (*np.u32, port); np.u32++; break;
      case SCM_UVEC_S32: scm_intprint (*np.s32, 10, port); np.s32++; break;
#if HAVE_UVEC_64
      case SCM_UVEC_U64: print_uint64(*np.u64, port); np.u64++; break;
      case SCM_UVEC_S64: print_int64(*np.s64, port); np.s64++; break;
#endif
      case SCM_UVEC_F32: scm_iprin1 (scm_make_real (*np.f32), port, pstate);
        np.f32++;
        break;
      case SCM_UVEC_F64: scm_iprin1 (scm_make_real (*np.f64), port, pstate);
        np.f64++;
        break;
      default:
        abort ();			/* Sanity check.  */
        break;
    }
    i++;
  }
  scm_remember_upto_here_1 (uvec);
  scm_puts (")", port);
  return 1;
}

static SCM
uvec_equalp (SCM a, SCM b)
{
  SCM result = SCM_BOOL_T;
  if(SCM_UVEC_TYPE (a) != SCM_UVEC_TYPE (b))
    result = SCM_BOOL_F;
  else if(SCM_UVEC_LENGTH (a) != SCM_UVEC_LENGTH (b))
    result = SCM_BOOL_F;
  else if(memcmp(SCM_UVEC_BASE (a), SCM_UVEC_BASE (b), SCM_UVEC_LENGTH (a))
          != 0)
    result = SCM_BOOL_F;

  scm_remember_upto_here_2 (a, b);
  return result;
}

static size_t
uvec_free (SCM uvec)
{
  scm_must_free (SCM_UVEC_BASE (uvec));
  return SCM_UVEC_LENGTH (uvec) * uvec_sizes[SCM_UVEC_TYPE (uvec)];
}


/* ================================================================ */
/* Utility procedures.                                              */
/* ================================================================ */


/* Create a new, uninitialized homogeneous numeric vector of type TYPE
   with space for LEN elements.  */
inline static SCM
make_uvec (const int pos, const char * func_name, int type, size_t len)
{
  if (len > ((size_t) SIZE_MAX >> uvec_shifts[type]))
  {
    if(pos < 0)
      scm_out_of_range (func_name, scm_size2num (len)); 
    else
      scm_out_of_range_pos (func_name, scm_size2num (len), SCM_MAKINUM (pos)); 
  }
  else
  {
    void *p = scm_must_malloc (len * uvec_sizes[type], func_name);
    SCM_RETURN_NEWSMOB3 (scm_tc16_uvec, type, len, p);
  }
}

inline static SCM
uvec_length (SCM uvec, const int required_type, const char *FUNC_NAME)
{
  SCM result;
  VALIDATE_UVEC (1, uvec, required_type);
  result = scm_size2num (SCM_UVEC_LENGTH (uvec));
  scm_remember_upto_here_1 (uvec);
  return result;
}


/* ================================================================ */
/* U8 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_u8vector_p, "u8vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type u8,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_u8vector_p
{
  SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj)
                         && (SCM_UVEC_TYPE (obj) == SCM_UVEC_U8));
  scm_remember_upto_here_1 (obj);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_u8vector, "make-u8vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_u8vector
{
  SCM uvec;
  int_u8 f;
  const size_t count = scm_num2size (n, 1, s_scm_make_u8vector);
  uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_U8, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
  {
    unsigned int s = scm_num2uint (fill, 2, FUNC_NAME);
    f = s;
    if ((unsigned int) f != s)
      scm_out_of_range_pos (FUNC_NAME, fill, SCM_MAKINUM (2));
  }
  memset(SCM_UVEC_BASE (uvec), f, count);
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector, "u8vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_u8vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_u8vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector_length, "u8vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_u8vector_length
{
  return uvec_length (uvec, SCM_UVEC_U8, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector_ref, "u8vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_u8vector_ref
{
  SCM result;
  size_t idx;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U8);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_short2num (((int_u8 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (uvec);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector_set_x, "u8vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_u8vector_set_x
{
  size_t idx;
  int_u8 f;
  unsigned int s;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U8);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  s = scm_num2uint (value, 3, FUNC_NAME);
  f = s;
  if ((unsigned int) f != s)
    scm_out_of_range_pos (FUNC_NAME, value, SCM_MAKINUM (3));

  ((int_u8 *) SCM_UVEC_BASE (uvec))[idx] = f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u8vector_to_list, "u8vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_u8vector_to_list
{
  size_t idx;
  int_u8 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U8);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_u8 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (SCM_MAKINUM (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_u8vector, "list->u8vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "8-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_u8vector
{
  SCM uvec;
  SCM tmp;
  int_u8 * p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_U8, n);
  p = (int_u8 *) SCM_UVEC_BASE (uvec);
  tmp = l;
  while (SCM_CONSP (tmp))
  {
    int_u8 f;
    unsigned int s = scm_num2uint (SCM_CAR (tmp), 2, FUNC_NAME);
    f = s;
    if ((unsigned int) f != s)
      scm_out_of_range (FUNC_NAME, SCM_CAR (tmp));
    *p++ = f;
    tmp = SCM_CDR (tmp);
  }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* S8 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_s8vector_p, "s8vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type s8,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_s8vector_p
{
  SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj)
                         && (SCM_UVEC_TYPE (obj) == SCM_UVEC_S8));
  scm_remember_upto_here_1 (obj);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_s8vector, "make-s8vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_s8vector
{
  SCM uvec;
  int_s8 f;
  size_t count = scm_num2size (n, 1, s_scm_make_s8vector);
  uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_S8, count);
  if (SCM_UNBNDP (fill))
    f = 0;
  else
  {
    int s = scm_num2int (fill, 2, FUNC_NAME);
    f = s;
    if ((signed int) f != s)
      scm_out_of_range_pos (FUNC_NAME, fill, SCM_MAKINUM (2));
  }
  memset(SCM_UVEC_BASE (uvec), f, count);
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector, "s8vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_s8vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_s8vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector_length, "s8vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_s8vector_length
{
  return uvec_length (uvec, SCM_UVEC_S8, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector_ref, "s8vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_s8vector_ref
{
  SCM result;
  size_t idx;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S8);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_short2num (((int_s8 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (uvec);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector_set_x, "s8vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_s8vector_set_x
{
  size_t idx;
  int_s8 f;
  signed int s;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S8);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  s = scm_num2int (value, 3, FUNC_NAME);
  f = s;
  if ((signed int) f != s)
    scm_out_of_range_pos (FUNC_NAME, value, SCM_MAKINUM (3));

  ((int_s8 *) SCM_UVEC_BASE (uvec))[idx] = f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s8vector_to_list, "s8vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_s8vector_to_list
{
  size_t idx;
  int_s8 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S8);
  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_s8 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (SCM_MAKINUM (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_s8vector, "list->s8vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "8-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_s8vector
{
  SCM uvec;
  SCM tmp;
  int_s8 * p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_S8, n);
  p = (int_s8 *) SCM_UVEC_BASE (uvec);
  tmp = l;
  while (SCM_CONSP (tmp))
  {
    int_s8 f;
    signed int s;
    
    s = scm_num2int (SCM_CAR (tmp), 2, FUNC_NAME);
    f = s;
    if ((signed int) f != s)
      scm_out_of_range (FUNC_NAME, SCM_CAR (tmp));
    *p++ = f;
    tmp = SCM_CDR (tmp);
  }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* U16 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_u16vector_p, "u16vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type u16,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_u16vector_p
{
  SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj)
                         && SCM_UVEC_TYPE (obj) == SCM_UVEC_U16);
  scm_remember_upto_here_1 (obj);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_u16vector, "make-u16vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_u16vector
{
  size_t count = scm_num2size (n, 1, s_scm_make_u16vector);
  SCM uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_U16, count);

  if (SCM_UNBNDP (fill))
    memset (SCM_UVEC_BASE (uvec), 0, count * sizeof (int_u16));
  else
  {
    /* if we had SIZEOF_SHORT we could be more efficient here */
    int_u16 * p;
    const unsigned int f = scm_num2uint (fill, 2, FUNC_NAME);

#if SIZEOF_INT > 2
    SCM_ASSERT_RANGE (2, fill, (f <= (int_u16) 65535));
#endif

    p = (int_u16 *) SCM_UVEC_BASE (uvec);
    while (count-- > 0)
      *p++ = f;
  }
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector, "u16vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_u16vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_u16vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector_length, "u16vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_u16vector_length
{
  return uvec_length (uvec, SCM_UVEC_U16, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector_ref, "u16vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_u16vector_ref
{
  SCM result;
  size_t idx;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U16);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_ushort2num (((int_u16 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (result);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector_set_x, "u16vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_u16vector_set_x
{
  size_t idx;
  unsigned int f;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U16);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);

  /* if we had SIZEOF_SHORT we could be more efficient here */
  f = scm_num2uint (value, 3, FUNC_NAME);
#if SIZEOF_INT > 2
  SCM_ASSERT_RANGE (2, value, (f <= (int_u16) 65535));
#endif

  ((int_u16 *) SCM_UVEC_BASE (uvec))[idx] = (int_u16) f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u16vector_to_list, "u16vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_u16vector_to_list
{
  size_t idx;
  int_u16 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U16);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_u16 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (SCM_MAKINUM (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_u16vector, "list->u16vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "16-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_u16vector
{
  SCM uvec;
  int_u16 * p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_U16, n);
  p = (int_u16 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
  {
    int_u16 f = scm_num2ushort (SCM_CAR (l), 2, FUNC_NAME);
    *p++ = f;
    l = SCM_CDR (l);
  }
  return uvec;
}
#undef FUNC_NAME

/* ================================================================ */
/* S16 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_s16vector_p, "s16vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type s16,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_s16vector_p
{
  const SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj)
                               && SCM_UVEC_TYPE (obj) == SCM_UVEC_S16);
  scm_remember_upto_here_1 (obj);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_s16vector, "make-s16vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_s16vector
{
  SCM uvec;
  size_t count = scm_num2size (n, 1, s_scm_make_s16vector);
  uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_S16, count);

  if (SCM_UNBNDP (fill))
    memset (SCM_UVEC_BASE (uvec), 0, count * sizeof (int_s16));
  else
  {
    /* if we had SIZEOF_SHORT we could be more efficient here */
    int_s16 * p;
    int f = scm_num2int (fill, 2, FUNC_NAME);

#if SIZEOF_INT > 2
    SCM_ASSERT_RANGE (2, fill,
                      (f >= (int_s16) -32768) && (f <= (int_s16) 32767));
#endif
    
    p = (int_s16 *) SCM_UVEC_BASE (uvec);
    while (count-- > 0)
      *p++ = f;
  }
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector, "s16vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_s16vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_s16vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector_length, "s16vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_s16vector_length
{
  return uvec_length (uvec, SCM_UVEC_S16, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector_ref, "s16vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_s16vector_ref
{
  SCM result;
  size_t idx;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S16);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_short2num (((int_s16 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (uvec);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector_set_x, "s16vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_s16vector_set_x
{
  size_t idx;
  int f;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S16);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);

  /* if we had SIZEOF_SHORT we could be more efficient here */
  f = scm_num2int (value, 3, FUNC_NAME);
#if SIZEOF_INT > 2
  SCM_ASSERT_RANGE (2, value,
                    (f >= (int_s16) -32768) && (f <= (int_s16) 32767));
#endif

  ((int_s16 *) SCM_UVEC_BASE (uvec))[idx] = (int_s16) f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s16vector_to_list, "s16vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_s16vector_to_list
{
  size_t idx;
  int_s16 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S16);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_s16 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (SCM_MAKINUM (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_s16vector, "list->s16vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "16-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_s16vector
{
  SCM uvec;
  SCM tmp;
  int_s16 * p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_S16, n);
  p = (int_s16 *) SCM_UVEC_BASE (uvec);
  tmp = l;
  while (SCM_CONSP (tmp))
  {
    int_s16 f = scm_num2short (SCM_CAR (tmp), 2, FUNC_NAME);
    *p++ = f;
    tmp = SCM_CDR (tmp);
  }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* U32 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_u32vector_p, "u32vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type u32,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_u32vector_p
{
  const SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj)
                               && SCM_UVEC_TYPE (obj) == SCM_UVEC_U32);
  scm_remember_upto_here_1 (obj);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_u32vector, "make-u32vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_u32vector
{
  size_t count = scm_num2size (n, 1, s_scm_make_u32vector);
  SCM uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_U32, count);

  if (SCM_UNBNDP (fill))
    memset (SCM_UVEC_BASE (uvec), 0, count * sizeof (int_u32));
  else
  {
    int_u32 * p;
    const unsigned long f = scm_num2ulong (fill, 2, FUNC_NAME);

#if SIZEOF_LONG > 4
    SCM_ASSERT_RANGE (2, fill, (f <= (int_u32) 0xFFFFFFFFUL));
#endif
    
    p = (int_u32 *) SCM_UVEC_BASE (uvec);
    while (count-- > 0)
      *p++ = f;
  }

  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector, "u32vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_u32vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_u32vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector_length, "u32vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_u32vector_length
{
  return uvec_length (uvec, SCM_UVEC_U32, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector_ref, "u32vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_u32vector_ref
{
  SCM result;
  size_t idx;
  VALIDATE_UVEC (1, uvec, SCM_UVEC_U32);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_ulong2num (((int_u32 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (uvec);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector_set_x, "u32vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_u32vector_set_x
{
  size_t idx;
  unsigned long f;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U32);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);

  f = scm_num2ulong (value, 3, FUNC_NAME);
#if SIZEOF_LONG > 4
  SCM_ASSERT_RANGE (2, fill, (f <= (int_u32) 0xFFFFFFFFUL));
#endif

  ((int_u32 *) SCM_UVEC_BASE (uvec))[idx] = (int_u32) f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u32vector_to_list, "u32vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_u32vector_to_list
{
  size_t idx;
  int_u32 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U32);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_u32 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (scm_ulong2num (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_u32vector, "list->u32vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "32-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_u32vector
{
  SCM uvec;
  int_u32 *p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_U32, n);
  p = (int_u32 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
  {
    int_u32 f = scm_num2ulong (SCM_CAR (l), 2, FUNC_NAME);
    *p++ = f;
    l = SCM_CDR (l);
  }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* S32 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_s32vector_p, "s32vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type s32,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_s32vector_p
{
  SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
                         SCM_UVEC_TYPE (obj) == SCM_UVEC_S32);
  scm_remember_upto_here_1 (result);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_s32vector, "make-s32vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_s32vector
{
  size_t count = scm_num2size (n, 1, s_scm_make_s32vector);
  SCM uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_S32, count);

  if (SCM_UNBNDP (fill))
    memset (SCM_UVEC_BASE (uvec), 0, count * sizeof (int_s32));
  else
  {
    int_s32 * p;
    const long f = scm_num2long (fill, 2, FUNC_NAME);

#if SIZEOF_LONG > 4
    SCM_ASSERT_RANGE (2, fill,
                      (f >= (int_s32) -2147483648)
                      && (f <= (int_s32) 2147483647));
#endif
    
    p = (int_s32 *) SCM_UVEC_BASE (uvec);
    while (count-- > 0)
      *p++ = f;
  }
  
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector, "s32vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_s32vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_s32vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector_length, "s32vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_s32vector_length
{
  return uvec_length (uvec, SCM_UVEC_S32, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector_ref, "s32vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_s32vector_ref
{
  SCM result;
  size_t idx;
  VALIDATE_UVEC (1, uvec, SCM_UVEC_S32);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_long2num (((int_s32 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (uvec);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector_set_x, "s32vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_s32vector_set_x
{
  size_t idx;
  long f;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S32);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);

  f = scm_num2long (value, 3, FUNC_NAME);
#if SIZEOF_LONG > 4
  SCM_ASSERT_RANGE (2, value,
                    (f >= (int_s32) -2147483648)
                    && (f <= (int_s32) 2147483647));
#endif

  ((int_s32 *) SCM_UVEC_BASE (uvec))[idx] = (int_s32) f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s32vector_to_list, "s32vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_s32vector_to_list
{
  size_t idx;
  int_s32 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S32);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_s32 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (scm_long2num (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_s32vector, "list->s32vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "32-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_s32vector
{
  SCM uvec;
  int_s32 *p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_S32, n);
  p = (int_s32 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
  {
    int_s32 f = scm_num2long (SCM_CAR (l), 2, FUNC_NAME);
    *p++ = f;
    l = SCM_CDR (l);
  }
  return uvec;
}
#undef FUNC_NAME


#if HAVE_UVEC_64

/* ================================================================ */
/* U64 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_u64vector_p, "u64vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type u64,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_u64vector_p
{
  SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
                         SCM_UVEC_TYPE (obj) == SCM_UVEC_U64);
  scm_remember_upto_here_1 (obj);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_u64vector, "make-u64vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_u64vector
{
  size_t count = scm_num2size (n, 1, s_scm_make_u64vector);
  SCM uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_U64, count);

  if (SCM_UNBNDP (fill))
    memset (SCM_UVEC_BASE (uvec), 0, count * sizeof (int_u64));
  else
  {
    int_u64 * p;
    const unsigned long long f = scm_num2ulong_long (fill, 2, FUNC_NAME);

#if SIZEOF_LONG_LONG > 8
    SCM_ASSERT_RANGE (2, fill, (f <= (int_u64) 0xFFFFFFFFFFFFFFFFUL));
#endif
    
    p = (int_u64 *) SCM_UVEC_BASE (uvec);
    while (count-- > 0)
      *p++ = f;
  }

  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector, "u64vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_u64vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_u64vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector_length, "u64vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_u64vector_length
{
  return uvec_length (uvec, SCM_UVEC_U64, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector_ref, "u64vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_u64vector_ref
{
  SCM result;
  size_t idx;
  VALIDATE_UVEC (1, uvec, SCM_UVEC_U64);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_ulong_long2num (((int_u64 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (uvec);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector_set_x, "u64vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_u64vector_set_x
{
  size_t idx;
  unsigned long long f;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U64);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);

  f = scm_num2ulong_long (value, 3, FUNC_NAME);
#if SIZEOF_LONG_LONG > 8
  SCM_ASSERT_RANGE (2, fill, (f <= (int_u64) 0xFFFFFFFFFFFFFFFFUL));
#endif

  ((int_u64 *) SCM_UVEC_BASE (uvec))[idx] = (int_u64) f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_u64vector_to_list, "u64vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_u64vector_to_list
{
  size_t idx;
  int_u64 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_U64);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_u64 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (scm_ulong_long2num (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_u64vector, "list->u64vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "64-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_u64vector
{
  SCM uvec;
  int_u64 *p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_U64, n);
  p = (int_u64 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
  {
    int_u64 f = scm_num2ulong_long (SCM_CAR (l), 2, FUNC_NAME);
    *p++ = f;
    l = SCM_CDR (l);
  }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* S64 procedures.                                                   */
/* ================================================================ */

SCM_DEFINE (scm_s64vector_p, "s64vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type s64,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_s64vector_p
{
  const SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
                               SCM_UVEC_TYPE (obj) == SCM_UVEC_S64);
  scm_remember_upto_here_1 (obj);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_s64vector, "make-s64vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_s64vector
{
  size_t count = scm_num2size (n, 1, s_scm_make_s64vector);
  SCM uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_S64, count);

  if (SCM_UNBNDP (fill))
    memset (SCM_UVEC_BASE (uvec), 0, count * sizeof (int_s64));
  else
  {
    int_s64 * p;
    const long long f = scm_num2long_long (fill, 2, FUNC_NAME);

#if SIZEOF_LONG_LONG > 8
    SCM_ASSERT_RANGE (2, fill,
                      (f >= (int_s64) -9223372036854775808)
                      && (f <= (int_s64) 9223372036854775807));
#endif
    
    p = (int_s64 *) SCM_UVEC_BASE (uvec);
    while (count-- > 0)
      *p++ = f;
  }
  
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector, "s64vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_s64vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_s64vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector_length, "s64vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_s64vector_length
{
  return uvec_length (uvec, SCM_UVEC_S64, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector_ref, "s64vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_s64vector_ref
{
  SCM result;
  size_t idx;
  VALIDATE_UVEC (1, uvec, SCM_UVEC_S64);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_long_long2num (((int_s64 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (uvec);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector_set_x, "s64vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_s64vector_set_x
{
  size_t idx;
  long long f;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S64);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);

  f = scm_num2long_long (value, 3, FUNC_NAME);
#if SIZEOF_LONG_LONG > 8
    SCM_ASSERT_RANGE (2, value,
                      (f >= (int_s64) -9223372036854775808)
                      && (f <= (int_s64) 9223372036854775807));
#endif

  ((int_s64 *) SCM_UVEC_BASE (uvec))[idx] = (int_s64) f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_s64vector_to_list, "s64vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_s64vector_to_list
{
  size_t idx;
  int_s64 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_S64);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (int_s64 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (scm_long_long2num (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_s64vector, "list->s64vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "64-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_s64vector
{
  SCM uvec;
  int_s64 *p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_S64, n);
  p = (int_s64 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
  {
    int_s64 f = scm_num2long_long (SCM_CAR (l), 2, FUNC_NAME);
    *p++ = f;
    l = SCM_CDR (l);
  }
  return uvec;
}
#undef FUNC_NAME

#endif /* HAVE_UVEC_64 */


/* ================================================================ */
/* F32 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_f32vector_p, "f32vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type f32,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_f32vector_p
{
  SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
                         SCM_UVEC_TYPE (obj) == SCM_UVEC_F32);
  scm_remember_upto_here_1 (obj);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_f32vector, "make-f32vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_f32vector
{
  float_f32 f;
  float_f32 *p;
  size_t count = scm_num2size (n, 1, s_scm_make_f32vector);
  SCM uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_F32, count);

  if (SCM_UNBNDP (fill))
    f = 0;
  else
    f = scm_num2float (fill, 2, FUNC_NAME);

  p = (float_f32 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector, "f32vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_f32vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_f32vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector_length, "f32vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_f32vector_length
{
  return uvec_length (uvec, SCM_UVEC_F32, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector_ref, "f32vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_f32vector_ref
{
  SCM result;
  size_t idx;
  VALIDATE_UVEC (1, uvec, SCM_UVEC_F32);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_make_real (((float_f32 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (uvec);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector_set_x, "f32vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_f32vector_set_x
{
  size_t idx;
  float f;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_F32);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);

  f = scm_num2float (value, 3, FUNC_NAME);
  ((float_f32 *) SCM_UVEC_BASE (uvec))[idx] = f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f32vector_to_list, "f32vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_f32vector_to_list
{
  size_t idx;
  float_f32 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_F32);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (float_f32 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (scm_float2num (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_f32vector, "list->f32vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain unsigned\n"
	    "8-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_f32vector
{
  SCM uvec;
  float_f32 *p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_F32, n);
  p = (float_f32 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
  {
    float_f32 f = scm_num2float (SCM_CAR (l), 2, FUNC_NAME);
    *p++ = f;
    l = SCM_CDR (l);
  }
  return uvec;
}
#undef FUNC_NAME


/* ================================================================ */
/* F64 procedures.                                                   */
/* ================================================================ */


SCM_DEFINE (scm_f64vector_p, "f64vector?", 1, 0, 0,
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a vector of type f64,\n"
	    "@code{#f} otherwise.")
#define FUNC_NAME s_scm_f64vector_p
{
  SCM result = SCM_BOOL (SCM_SMOB_PREDICATE (scm_tc16_uvec, obj) &&
                         SCM_UVEC_TYPE (obj) == SCM_UVEC_F64);
  scm_remember_upto_here_1 (obj);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_f64vector, "make-f64vector", 1, 1, 0,
            (SCM n, SCM fill),
	    "Create a newly allocated homogeneous numeric vector which can\n"
	    "hold @var{len} elements.  If @var{fill} is given, it is used to\n"
	    "initialize the elements, otherwise the contents of the vector\n"
	    "is unspecified.")
#define FUNC_NAME s_scm_make_f64vector
{
  float_f64 f;
  float_f64 *p;
  size_t count = scm_num2size (n, 1, s_scm_make_f64vector);
  SCM uvec = make_uvec (1, FUNC_NAME, SCM_UVEC_F64, count);

  if (SCM_UNBNDP (fill))
    f = 0;
  else
    f = scm_num2double (fill, 2, FUNC_NAME);

  p = (float_f64 *) SCM_UVEC_BASE (uvec);
  while (count-- > 0)
    *p++ = f;
  
  return uvec;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector, "f64vector", 0, 0, 1,
            (SCM l),
	    "Create a newly allocated homogeneous numeric vector containing\n"
	    "all argument values.")
#define FUNC_NAME s_scm_f64vector
{
  SCM_VALIDATE_REST_ARGUMENT (l);
  return scm_list_to_f64vector (l);
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector_length, "f64vector-length", 1, 0, 0,
            (SCM uvec),
	    "Return the number of elements in the homogeneous numeric vector\n"
	    "@var{uvec}.")
#define FUNC_NAME s_scm_f64vector_length
{
  return uvec_length (uvec, SCM_UVEC_F64, FUNC_NAME);
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector_ref, "f64vector-ref", 2, 0, 0,
            (SCM uvec, SCM index),
	    "Return the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec}.")
#define FUNC_NAME s_scm_f64vector_ref
{
  SCM result;
  size_t idx;
  VALIDATE_UVEC (1, uvec, SCM_UVEC_F64);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);
  result = scm_make_real (((float_f64 *) SCM_UVEC_BASE (uvec))[idx]);
  scm_remember_upto_here_1 (uvec);
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector_set_x, "f64vector-set!", 3, 0, 0,
            (SCM uvec, SCM index, SCM value),
	    "Set the element at @var{index} in the homogeneous numeric\n"
	    "vector @var{uvec} to @var{value}.  The return value is not\n"
	    "specified.")
#define FUNC_NAME s_scm_f64vector_set_x
{
  size_t idx;
  float f;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_F64);
  RANGE_CHECK_AND_COPY_UVEC_INDEX (2, uvec, index, idx);

  f = scm_num2double (value, 3, FUNC_NAME);
  ((float_f64 *) SCM_UVEC_BASE (uvec))[idx] = f;
  scm_remember_upto_here_1 (uvec);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (scm_f64vector_to_list, "f64vector->list", 1, 0, 0,
            (SCM uvec),
	    "Convert the homogeneous numeric vector @var{uvec} to a list.")
#define FUNC_NAME s_scm_f64vector_to_list
{
  size_t idx;
  float_f64 * p;
  SCM res = SCM_EOL;

  VALIDATE_UVEC (1, uvec, SCM_UVEC_F64);

  idx = SCM_UVEC_LENGTH (uvec);
  p = (float_f64 *) SCM_UVEC_BASE (uvec) + idx;
  while (idx-- > 0)
  {
    p--;
    res = scm_cons (scm_double2num (*p), res);
  }
  scm_remember_upto_here_1 (uvec);
  return res;
}
#undef FUNC_NAME


SCM_DEFINE (scm_list_to_f64vector, "list->f64vector", 1, 0, 0,
            (SCM l),
	    "Convert the list @var{l}, which must only contain signed\n"
	    "8-bit values, to a numeric homogeneous vector.")
#define FUNC_NAME s_scm_list_to_f64vector
{
  SCM uvec;
  float_f64 *p;
  long n; /* to match COPYLEN */

  SCM_VALIDATE_LIST_COPYLEN (1, l, n);

  uvec = make_uvec (-1, FUNC_NAME, SCM_UVEC_F64, n);
  p = (float_f64 *) SCM_UVEC_BASE (uvec);
  while (SCM_CONSP (l))
  {
    float_f64 f = scm_num2double (SCM_CAR (l), 2, FUNC_NAME);
    *p++ = f;
    l = SCM_CDR (l);
  }
  return uvec;
}
#undef FUNC_NAME


/* Create the smob type for homogeneous numeric vectors and install
   the primitives.  */
void
scm_init_srfi_4 (void)
{
  /* this will be a compilation-time test in future versions of guile */
  if(sizeof(size_t) > sizeof(scm_t_bits))
  {
    fprintf(stderr, "fatal: size_t will not fit in a cell (srfi-4)\n");
    abort();
  }

  scm_tc16_uvec = scm_make_smob_type ("uvec", 0);
  scm_set_smob_equalp (scm_tc16_uvec, uvec_equalp);
  scm_set_smob_free (scm_tc16_uvec, uvec_free);
  scm_set_smob_print (scm_tc16_uvec, uvec_print);
#include "srfi/srfi-4.x"
}
