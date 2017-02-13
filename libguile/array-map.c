/* Copyright (C) 1996, 1998, 2000, 2001, 2004, 2005, 2006, 2008, 2009,
 *   2010, 2011, 2012, 2013, 2014, 2015 Free Software Foundation, Inc.
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






#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/strings.h"
#include "libguile/arrays.h"
#include "libguile/smob.h"
#include "libguile/chars.h"
#include "libguile/eq.h"
#include "libguile/eval.h"
#include "libguile/feature.h"
#include "libguile/vectors.h"
#include "libguile/bitvectors.h"
#include "libguile/srfi-4.h"
#include "libguile/generalized-arrays.h"

#include "libguile/validate.h"
#include "libguile/array-map.h"
#include <assert.h>

/* The WHAT argument for `scm_gc_malloc ()' et al.  */
static const char vi_gc_hint[] = "array-indices";

static SCM
make1array (SCM v, ssize_t inc)
{
  SCM a = scm_i_make_array (1);
  SCM_I_ARRAY_SET_BASE (a, 0);
  SCM_I_ARRAY_DIMS (a)->lbnd = 0;
  SCM_I_ARRAY_DIMS (a)->ubnd = scm_c_array_length (v) - 1;
  SCM_I_ARRAY_DIMS (a)->inc = inc;
  SCM_I_ARRAY_SET_V (a, v);
  return a;
}

/* Linear index of not-unrolled index set. */
static size_t
cindk (SCM ra, ssize_t *ve, int kend)
{
  if (SCM_I_ARRAYP (ra))
    {
      int k;
      size_t i = SCM_I_ARRAY_BASE (ra);
      for (k = 0; k < kend; ++k)
        i += (ve[k] - SCM_I_ARRAY_DIMS (ra)[k].lbnd) * SCM_I_ARRAY_DIMS (ra)[k].inc;
      return i;
    }
  else
    return 0; /* this is BASE */
}

/* array mapper: apply cproc to each dimension of the given arrays?.
     int (*cproc) ();   procedure to call on unrolled arrays?
			   cproc (dest, source list) or
			   cproc (dest, data, source list).
     SCM data;          data to give to cproc or unbound.
     SCM ra0;           destination array.
     SCM lra;           list of source arrays.
     const char *what;  caller, for error reporting. */

#define LBND(ra, k) SCM_I_ARRAY_DIMS (ra)[k].lbnd
#define UBND(ra, k) SCM_I_ARRAY_DIMS (ra)[k].ubnd


/* scm_ramapc() always calls cproc with rank-1 arrays created by
   make1array. cproc (rafe, ramap, rafill, racp) can assume that the
   dims[0].lbnd of these arrays is always 0. */
int
scm_ramapc (void *cproc_ptr, SCM data, SCM ra0, SCM lra, const char *what)
{
  int (*cproc) () = cproc_ptr;
  SCM z, va0, lva, *plva;
  int k, kmax, kroll;
  ssize_t *vi, inc;
  size_t len;

  /* Prepare reference argument. */
  if (SCM_I_ARRAYP (ra0))
    {
      kmax = SCM_I_ARRAY_NDIM (ra0)-1;
      inc = kmax < 0 ?  0 : SCM_I_ARRAY_DIMS (ra0)[kmax].inc;
      va0 = make1array (SCM_I_ARRAY_V (ra0), inc);

      /* Find unroll depth */
      for (kroll = max(0, kmax); kroll > 0; --kroll)
        {
          inc *= (UBND (ra0, kroll) - LBND (ra0, kroll) + 1);
          if (inc != SCM_I_ARRAY_DIMS (ra0)[kroll-1].inc)
            break;
        }
    }
  else
    {
      kroll = kmax = 0;
      va0 = ra0 = make1array (ra0, 1);
    }

  /* Prepare rest arguments. */
  lva = SCM_EOL;
  plva = &lva;
  for (z = lra; !scm_is_null (z); z = SCM_CDR (z))
    {
      SCM va1, ra1 = SCM_CAR (z);
      if (SCM_I_ARRAYP (ra1))
        {
          if (kmax != SCM_I_ARRAY_NDIM (ra1) - 1)
            scm_misc_error (what, "array shape mismatch: ~S", scm_list_1 (ra0));
          inc = kmax < 0 ? 0 : SCM_I_ARRAY_DIMS (ra1)[kmax].inc;
          va1 = make1array (SCM_I_ARRAY_V (ra1), inc);

          /* Check unroll depth. */
          for (k = kmax; k > kroll; --k)
            {
              ssize_t l0 = LBND (ra0, k), u0 = UBND (ra0, k);
              if (l0 < LBND (ra1, k) || u0 > UBND (ra1, k))
                scm_misc_error (what, "array shape mismatch: ~S", scm_list_1 (ra0));
              inc *= (u0 - l0 + 1);
              if (inc != SCM_I_ARRAY_DIMS (ra1)[k-1].inc)
                {
                  kroll = k;
                  break;
                }
            }

          /* Check matching of not-unrolled axes. */
          for (; k>=0; --k)
            if (LBND (ra0, k) < LBND (ra1, k) || UBND (ra0, k) > UBND (ra1, k))
              scm_misc_error (what, "array shape mismatch: ~S", scm_list_1 (ra0));
        }
      else
        {
          if (kmax != 0)
            scm_misc_error (what, "array shape mismatch: ~S", scm_list_1 (ra0));
          va1 = make1array (ra1, 1);

          if (LBND (ra0, 0) < 0 /* LBND (va1, 0) */ || UBND (ra0, 0) > UBND (va1, 0))
            scm_misc_error (what, "array shape mismatch: ~S", scm_list_1 (ra0));
        }
      *plva = scm_cons (va1, SCM_EOL);
      plva = SCM_CDRLOC (*plva);
    }

  /* Check emptiness of not-unrolled axes. */
  for (k = 0; k < kroll; ++k)
    if (0 == (UBND (ra0, k) - LBND (ra0, k) + 1))
      return 1;

  /* Set unrolled size. */
  for (len = 1; k <= kmax; ++k)
    len *= (UBND (ra0, k) - LBND (ra0, k) + 1);
  UBND (va0, 0) = len - 1;
  for (z = lva; !scm_is_null (z); z = SCM_CDR (z))
    UBND (SCM_CAR (z), 0) = len - 1;

  /* Set starting indices and go. */
  vi = scm_gc_malloc_pointerless (sizeof(ssize_t) * kroll, vi_gc_hint);
  for (k = 0; k < kroll; ++k)
    vi[k] = LBND (ra0, k);
  do
    {
      if (k == kroll)
        {
          SCM y = lra;
          SCM_I_ARRAY_SET_BASE (va0, cindk (ra0, vi, kroll));
          for (z = lva; !scm_is_null (z); z = SCM_CDR (z), y = SCM_CDR (y))
            SCM_I_ARRAY_SET_BASE (SCM_CAR (z), cindk (SCM_CAR (y), vi, kroll));
          if (! (SCM_UNBNDP (data) ? cproc (va0, lva) : cproc (va0, data, lva)))
            return 0;
          --k;
        }
      else if (vi[k] < UBND (ra0, k))
        {
          ++vi[k];
          ++k;
        }
      else
        {
          vi[k] = LBND (ra0, k) - 1;
          --k;
        }
    }
  while (k >= 0);

  return 1;
}

#undef UBND
#undef LBND

static int
rafill (SCM dst, SCM fill)
{
  size_t n = SCM_I_ARRAY_DIMS (dst)->ubnd + 1;
  size_t i = SCM_I_ARRAY_BASE (dst);
  ssize_t inc = SCM_I_ARRAY_DIMS (dst)->inc;
  scm_t_array_handle h;
  dst = SCM_I_ARRAY_V (dst);
  scm_array_get_handle (dst, &h);

  for (; n-- > 0; i += inc)
    h.vset (h.vector, i, fill);

  scm_array_handle_release (&h);
  return 1;
}

SCM_DEFINE (scm_array_fill_x, "array-fill!", 2, 0, 0,
	    (SCM ra, SCM fill),
	    "Store @var{fill} in every element of array @var{ra}.  The value\n"
	    "returned is unspecified.")
#define FUNC_NAME s_scm_array_fill_x
{
  scm_ramapc (rafill, fill, ra, SCM_EOL, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static int
racp (SCM src, SCM dst)
{
  size_t i_s, i_d, n;
  ssize_t inc_s, inc_d;
  scm_t_array_handle h_s, h_d;
  dst = SCM_CAR (dst);
  i_s = SCM_I_ARRAY_BASE (src);
  i_d = SCM_I_ARRAY_BASE (dst);
  n = (SCM_I_ARRAY_DIMS (src)->ubnd + 1);
  inc_s = SCM_I_ARRAY_DIMS (src)->inc;
  inc_d = SCM_I_ARRAY_DIMS (dst)->inc;
  src = SCM_I_ARRAY_V (src);
  dst = SCM_I_ARRAY_V (dst);
  scm_array_get_handle (src, &h_s);
  scm_array_get_handle (dst, &h_d);

  if (h_s.element_type == SCM_ARRAY_ELEMENT_TYPE_SCM
      && h_d.element_type == SCM_ARRAY_ELEMENT_TYPE_SCM)
    {
      SCM const * el_s = h_s.elements;
      SCM * el_d = h_d.writable_elements;
      if (!el_d && n>0)
        scm_wrong_type_arg_msg ("array-copy!", SCM_ARG2, dst, "mutable array");
      for (; n-- > 0; i_s += inc_s, i_d += inc_d)
        el_d[i_d] = el_s[i_s];
    }
  else
    for (; n-- > 0; i_s += inc_s, i_d += inc_d)
      h_d.vset (h_d.vector, i_d, h_s.vref (h_s.vector, i_s));

  scm_array_handle_release (&h_d);
  scm_array_handle_release (&h_s);

  return 1;
}

SCM_REGISTER_PROC(s_array_copy_in_order_x, "array-copy-in-order!", 2, 0, 0, scm_array_copy_x);


SCM_DEFINE (scm_array_copy_x, "array-copy!", 2, 0, 0,
	    (SCM src, SCM dst),
	    "@deffnx {Scheme Procedure} array-copy-in-order! src dst\n"
	    "Copy every element from vector or array @var{src} to the\n"
	    "corresponding element of @var{dst}.  @var{dst} must have the\n"
	    "same rank as @var{src}, and be at least as large in each\n"
	    "dimension.  The order is unspecified.")
#define FUNC_NAME s_scm_array_copy_x
{
  scm_ramapc (racp, SCM_UNDEFINED, src, scm_cons (dst, SCM_EOL), FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static int
ramap (SCM ra0, SCM proc, SCM ras)
{
  size_t i0 = SCM_I_ARRAY_BASE (ra0);
  ssize_t inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  size_t n = SCM_I_ARRAY_DIMS (ra0)->ubnd + 1;
  scm_t_array_handle h0;
  ra0 = SCM_I_ARRAY_V (ra0);
  scm_array_get_handle (ra0, &h0);

  if (scm_is_null (ras))
    for (; n--; i0 += inc0)
      h0.vset (h0.vector, i0, scm_call_0 (proc));
  else
    {
      SCM ra1 = SCM_CAR (ras);
      size_t i1 = SCM_I_ARRAY_BASE (ra1);
      ssize_t inc1 = SCM_I_ARRAY_DIMS (ra1)->inc;
      scm_t_array_handle h1;
      ra1 = SCM_I_ARRAY_V (ra1);
      scm_array_get_handle (ra1, &h1);
      ras = SCM_CDR (ras);
      if (scm_is_null (ras))
        for (; n--; i0 += inc0, i1 += inc1)
          h0.vset (h0.vector, i0, scm_call_1 (proc, h1.vref (h1.vector, i1)));
      else
        {
          SCM ra2 = SCM_CAR (ras);
          size_t i2 = SCM_I_ARRAY_BASE (ra2);
          ssize_t inc2 = SCM_I_ARRAY_DIMS (ra2)->inc;
          scm_t_array_handle h2;
          ra2 = SCM_I_ARRAY_V (ra2);
          scm_array_get_handle (ra2, &h2);
          ras = SCM_CDR (ras);
          if (scm_is_null (ras))
            for (; n--; i0 += inc0, i1 += inc1, i2 += inc2)
              h0.vset (h0.vector, i0, scm_call_2 (proc, h1.vref (h1.vector, i1), h2.vref (h2.vector, i2)));
          else
            {
              scm_t_array_handle *hs;
              size_t restn = scm_ilength (ras);
              SCM args = SCM_EOL;
              SCM *p = &args;
              SCM **sa = scm_gc_malloc (sizeof(SCM *) * restn, vi_gc_hint);
              size_t k;
              ssize_t i;
              
              for (k = 0; k < restn; ++k)
                {
                  *p = scm_cons (SCM_UNSPECIFIED, SCM_EOL);
                  sa[k] = SCM_CARLOC (*p);
                  p = SCM_CDRLOC (*p);
                }

              hs = scm_gc_malloc (sizeof(scm_t_array_handle) * restn, vi_gc_hint);
              for (k = 0; k < restn; ++k, ras = scm_cdr (ras))
                scm_array_get_handle (scm_car (ras), hs+k);

              for (i = 0; n--; i0 += inc0, i1 += inc1, i2 += inc2, ++i)
                {
                  for (k = 0; k < restn; ++k)
                    *(sa[k]) = scm_array_handle_ref (hs+k, i*hs[k].dims[0].inc);
                  h0.vset (h0.vector, i0, scm_apply_2 (proc, h1.vref (h1.vector, i1), h2.vref (h2.vector, i2), args));
                }

              for (k = 0; k < restn; ++k)
                scm_array_handle_release (hs+k);
            }
          scm_array_handle_release (&h2);
        }
      scm_array_handle_release (&h1);
    }
  scm_array_handle_release (&h0);
  return 1;
}


SCM_REGISTER_PROC(s_array_map_in_order_x, "array-map-in-order!", 2, 0, 1, scm_array_map_x);

SCM_SYMBOL (sym_b, "b");

SCM_DEFINE (scm_array_map_x, "array-map!", 2, 0, 1,
	    (SCM ra0, SCM proc, SCM lra),
	    "@deffnx {Scheme Procedure} array-map-in-order! ra0 proc . lra\n"
	    "@var{array1}, @dots{} must have the same number of dimensions\n"
	    "as @var{ra0} and have a range for each index which includes the\n"
	    "range for the corresponding index in @var{ra0}.  @var{proc} is\n"
	    "applied to each tuple of elements of @var{array1}, @dots{} and\n"
	    "the result is stored as the corresponding element in @var{ra0}.\n"
	    "The value returned is unspecified.  The order of application is\n"
	    "unspecified.")
#define FUNC_NAME s_scm_array_map_x
{
  SCM_VALIDATE_PROC (2, proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);

  scm_ramapc (ramap, proc, ra0, lra, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static int
rafe (SCM ra0, SCM proc, SCM ras)
{
  size_t i0 = SCM_I_ARRAY_BASE (ra0);
  ssize_t inc0 = SCM_I_ARRAY_DIMS (ra0)->inc;
  size_t n = SCM_I_ARRAY_DIMS (ra0)->ubnd + 1;
  scm_t_array_handle h0;
  ra0 = SCM_I_ARRAY_V (ra0);
  scm_array_get_handle (ra0, &h0);

  if (scm_is_null (ras))
    for (; n--; i0 += inc0)
      scm_call_1 (proc, h0.vref (h0.vector, i0));
  else
    {
      scm_t_array_handle *hs;      
      size_t restn = scm_ilength (ras);

      SCM args = SCM_EOL;
      SCM *p = &args;
      SCM **sa = scm_gc_malloc (sizeof(SCM *) * restn, vi_gc_hint);
      for (size_t k = 0; k < restn; ++k)
        {
          *p = scm_cons (SCM_UNSPECIFIED, SCM_EOL);
          sa[k] = SCM_CARLOC (*p);
          p = SCM_CDRLOC (*p);
        }

      hs = scm_gc_malloc (sizeof(scm_t_array_handle) * restn, vi_gc_hint);
      for (size_t k = 0; k < restn; ++k, ras = scm_cdr (ras))
        scm_array_get_handle (scm_car (ras), hs+k);

      for (ssize_t i = 0; n--; i0 += inc0, ++i)
        {
          for (size_t k = 0; k < restn; ++k)
            *(sa[k]) = scm_array_handle_ref (hs+k, i*hs[k].dims[0].inc);
          scm_apply_1 (proc, h0.vref (h0.vector, i0), args);
        }

      for (size_t k = 0; k < restn; ++k)
        scm_array_handle_release (hs+k);
    }
  scm_array_handle_release (&h0);
  return 1;
}

SCM_DEFINE (scm_array_for_each, "array-for-each", 2, 0, 1,
	    (SCM proc, SCM ra0, SCM lra),
	    "Apply @var{proc} to each tuple of elements of @var{ra0} @dots{}\n"
	    "in row-major order.  The value returned is unspecified.")
#define FUNC_NAME s_scm_array_for_each
{
  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_REST_ARGUMENT (lra);
  scm_ramapc (rafe, proc, ra0, lra, FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void
array_index_map_1 (SCM ra, SCM proc)
{
  scm_t_array_handle h;
  ssize_t i, inc;
  size_t p;
  scm_array_get_handle (ra, &h);
  inc = h.dims[0].inc;
  for (i = h.dims[0].lbnd, p = h.base; i <= h.dims[0].ubnd; ++i, p += inc)
    h.vset (h.vector, p, scm_call_1 (proc, scm_from_ssize_t (i)));
  scm_array_handle_release (&h);
}

/* Here we assume that the array is a scm_tc7_array, as that is the only
   kind of array in Guile that supports rank > 1.  */
static void
array_index_map_n (SCM ra, SCM proc)
{
  scm_t_array_handle h;
  int k, kmax = SCM_I_ARRAY_NDIM (ra) - 1;
  SCM args = SCM_EOL;
  SCM *p = &args;

  ssize_t *vi = scm_gc_malloc_pointerless (sizeof(ssize_t) * (kmax + 1), vi_gc_hint);
  SCM **si = scm_gc_malloc_pointerless (sizeof(SCM *) * (kmax + 1), vi_gc_hint);

  for (k = 0; k <= kmax; k++)
    {
      vi[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd;
      if (vi[k] > SCM_I_ARRAY_DIMS (ra)[k].ubnd)
        return;
      *p = scm_cons (scm_from_ssize_t (vi[k]), SCM_EOL);
      si[k] = SCM_CARLOC (*p);
      p = SCM_CDRLOC (*p);
    }

  scm_array_get_handle (ra, &h);
  k = kmax;
  do
    {
      if (k == kmax)
        {
          size_t i;
          vi[kmax] = SCM_I_ARRAY_DIMS (ra)[kmax].lbnd;
          i = cindk (ra, vi, kmax+1);
          for (; vi[kmax] <= SCM_I_ARRAY_DIMS (ra)[kmax].ubnd; ++vi[kmax])
            {
              *(si[kmax]) = scm_from_ssize_t (vi[kmax]);
              h.vset (h.vector, i, scm_apply_0 (proc, args));
              i += SCM_I_ARRAY_DIMS (ra)[kmax].inc;
            }
          k--;
        }
      else if (vi[k] < SCM_I_ARRAY_DIMS (ra)[k].ubnd)
        {
          *(si[k]) = scm_from_ssize_t (++vi[k]);
          k++;
        }
      else
        {
          vi[k] = SCM_I_ARRAY_DIMS (ra)[k].lbnd - 1;
          k--;
        }
    }
  while (k >= 0);
  scm_array_handle_release (&h);
}

SCM_DEFINE (scm_array_index_map_x, "array-index-map!", 2, 0, 0,
	    (SCM ra, SCM proc),
	    "Apply @var{proc} to the indices of each element of @var{ra} in\n"
	    "turn, storing the result in the corresponding element.  The value\n"
	    "returned and the order of application are unspecified.\n\n"
	    "One can implement @var{array-indexes} as\n"
	    "@lisp\n"
	    "(define (array-indexes array)\n"
	    "    (let ((ra (apply make-array #f (array-shape array))))\n"
	    "      (array-index-map! ra (lambda x x))\n"
	    "      ra))\n"
	    "@end lisp\n"
	    "Another example:\n"
	    "@lisp\n"
	    "(define (apl:index-generator n)\n"
	    "    (let ((v (make-uniform-vector n 1)))\n"
	    "      (array-index-map! v (lambda (i) i))\n"
	    "      v))\n"
	    "@end lisp")
#define FUNC_NAME s_scm_array_index_map_x
{
  SCM_VALIDATE_PROC (2, proc);

  switch (scm_c_array_rank (ra))
    {
    case 0:
      scm_array_set_x (ra, scm_call_0 (proc), SCM_EOL);
      break;
    case 1:
      array_index_map_1 (ra, proc);
      break;
    default:
      array_index_map_n (ra, proc);
      break;
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


static int
array_compare (scm_t_array_handle *hx, scm_t_array_handle *hy,
               size_t dim, unsigned long posx, unsigned long posy)
{
  if (dim == scm_array_handle_rank (hx))
    return scm_is_true (scm_equal_p (scm_array_handle_ref (hx, posx),
                                     scm_array_handle_ref (hy, posy)));
  else
    {
      long incx, incy;
      size_t i;

      if (hx->dims[dim].lbnd != hy->dims[dim].lbnd
          || hx->dims[dim].ubnd != hy->dims[dim].ubnd)
        return 0;

      i = hx->dims[dim].ubnd - hx->dims[dim].lbnd + 1;

      incx = hx->dims[dim].inc;
      incy = hy->dims[dim].inc;
      posx += (i - 1) * incx;
      posy += (i - 1) * incy;

      for (; i > 0; i--, posx -= incx, posy -= incy)
        if (!array_compare (hx, hy, dim + 1, posx, posy))
          return 0;
      return 1;
    }
}

SCM
scm_array_equal_p (SCM x, SCM y)
{
  scm_t_array_handle hx, hy;
  SCM res;

  scm_array_get_handle (x, &hx);
  scm_array_get_handle (y, &hy);

  res = scm_from_bool (hx.ndims == hy.ndims
                       && hx.element_type == hy.element_type);

  if (scm_is_true (res))
    res = scm_from_bool (array_compare (&hx, &hy, 0, 0, 0));

  scm_array_handle_release (&hy);
  scm_array_handle_release (&hx);

  return res;
}

static SCM scm_i_array_equal_p (SCM, SCM, SCM);
SCM_DEFINE (scm_i_array_equal_p, "array-equal?", 0, 2, 1,
            (SCM ra0, SCM ra1, SCM rest),
	    "Return @code{#t} iff all arguments are arrays with the same\n"
	    "shape, the same type, and have corresponding elements which are\n"
	    "either @code{equal?}  or @code{array-equal?}.  This function\n"
	    "differs from @code{equal?} in that all arguments must be arrays.")
#define FUNC_NAME s_scm_i_array_equal_p
{
  if (SCM_UNBNDP (ra0) || SCM_UNBNDP (ra1))
    return SCM_BOOL_T;

  while (!scm_is_null (rest))
    {
      if (scm_is_false (scm_array_equal_p (ra0, ra1)))
        return SCM_BOOL_F;
      ra0 = ra1;
      ra1 = scm_car (rest);
      rest = scm_cdr (rest);
    }
  return scm_array_equal_p (ra0, ra1);
}
#undef FUNC_NAME


/* Copy array descriptor with different base. */
SCM
scm_i_array_rebase (SCM a, size_t base)
{
    size_t ndim = SCM_I_ARRAY_NDIM (a);
    SCM b = scm_words (((scm_t_bits) ndim << 17) + scm_tc7_array, 3 + ndim*3);
    SCM_I_ARRAY_SET_V (b, SCM_I_ARRAY_V (a));
/* FIXME do check base */
    SCM_I_ARRAY_SET_BASE (b, base);
    memcpy (SCM_I_ARRAY_DIMS (b), SCM_I_ARRAY_DIMS (a), sizeof (scm_t_array_dim)*ndim);
    return b;
}

static inline size_t padtoptr(size_t d) { return (d + (sizeof (void *) - 1)) & ~(sizeof (void *) - 1); }

SCM_DEFINE (scm_array_slice_for_each, "array-slice-for-each", 2, 0, 1,
            (SCM frame_rank, SCM op, SCM args),
            "Apply @var{op} to each of the cells of rank rank(@var{arg})-@var{frame_rank}\n"
            "of the arrays @var{args}, in unspecified order. The first\n"
            "@var{frame_rank} dimensions of each @var{arg} must match.\n"
            "Rank-0 cells are passed as rank-0 arrays.\n\n"
            "The value returned is unspecified.\n\n"
            "For example:\n"
            "@lisp\n"
            ";; Sort the rows of rank-2 array A.\n\n"
            "(array-slice-for-each 1 (lambda (x) (sort! x <)) a)\n"
            "\n"
            ";; Compute the arguments of the (x y) vectors in the rows of rank-2\n"
            ";; array XYS and store them in rank-1 array ANGLES. Inside OP,\n"
            ";; XY is a rank-1 (2-1) array, and ANGLE is a rank-0 (1-1) array.\n\n"
            "(array-slice-for-each 1 \n"
            "  (lambda (xy angle)\n"
            "    (array-set! angle (atan (array-ref xy 1) (array-ref xy 0))))\n"
            "  xys angles)\n"
            "@end lisp")
#define FUNC_NAME s_scm_array_slice_for_each
{
  SCM xargs = args;
  int const N = scm_ilength (args);
  int const frank = scm_to_int (frame_rank);
  int ocd;
  ssize_t step;
  SCM dargs_ = SCM_EOL;
  char const * msg;
  scm_t_array_dim * ais;
  int n, k;
  ssize_t z;

  /* to be allocated inside the pool */
  scm_t_array_handle * ah;
  SCM * args_;
  scm_t_array_dim ** as;
  int * rank;

  ssize_t * s;
  SCM * ai;
  SCM ** dargs;
  ssize_t * i;

  int * order;
  size_t * base;

  /* size the pool */
  char * pool;
  char * pool0;
  size_t pool_size = 0;
  pool_size += padtoptr(N*sizeof (scm_t_array_handle));
  pool_size += padtoptr(N*sizeof (SCM));
  pool_size += padtoptr(N*sizeof (scm_t_array_dim *));
  pool_size += padtoptr(N*sizeof (int));

  pool_size += padtoptr(frank*sizeof (ssize_t));
  pool_size += padtoptr(N*sizeof (SCM));
  pool_size += padtoptr(N*sizeof (SCM *));
  pool_size += padtoptr(frank*sizeof (ssize_t));

  pool_size += padtoptr(frank*sizeof (int));
  pool_size += padtoptr(N*sizeof (size_t));
  pool = scm_gc_malloc (pool_size, "pool");

  /* place the items in the pool */
#define AFIC_ALLOC_ADVANCE(pool, count, type, name)    \
  name = (void *)pool;                                 \
  pool += padtoptr(count*sizeof (type));

  pool0 = pool;
  AFIC_ALLOC_ADVANCE (pool, N, scm_t_array_handle, ah);
  AFIC_ALLOC_ADVANCE (pool, N, SCM, args_);
  AFIC_ALLOC_ADVANCE (pool, N, scm_t_array_dim *, as);
  AFIC_ALLOC_ADVANCE (pool, N, int, rank);

  AFIC_ALLOC_ADVANCE (pool, frank, ssize_t, s);
  AFIC_ALLOC_ADVANCE (pool, N, SCM, ai);
  AFIC_ALLOC_ADVANCE (pool, N, SCM *, dargs);
  AFIC_ALLOC_ADVANCE (pool, frank, ssize_t, i);

  AFIC_ALLOC_ADVANCE (pool, frank, int, order);
  AFIC_ALLOC_ADVANCE (pool, N, size_t, base);
  assert((pool0+pool_size==pool) && "internal error");
#undef AFIC_ALLOC_ADVANCE

  for (n=0, xargs=args; scm_is_pair(xargs); xargs=scm_cdr(xargs), ++n)
    {
      args_[n] = scm_car(xargs);
      scm_array_get_handle(args_[n], ah+n);
      as[n] = scm_array_handle_dims(ah+n);
      rank[n] = scm_array_handle_rank(ah+n);
    }
  /* checks */
  msg = NULL;
  if (frank<0)
    msg = "bad frame rank ~S, ~S";
  else
    {
      for (n=0; n!=N; ++n)
        {
          if (rank[n]<frank)
            {
              msg = "frame too large for arguments: ~S, ~S";
              goto check_msg;
            }
          for (k=0; k!=frank; ++k)
            {
              if (as[0][k].lbnd!=as[n][k].lbnd || as[0][k].ubnd!=as[n][k].ubnd)
                {
                  msg = "mismatched frames: ~S, ~S";
                  goto check_msg;
                }
              s[k] = as[n][k].ubnd - as[n][k].lbnd + 1;

              /* this check is needed if the array cannot be entirely */
              /* unrolled, because the unrolled subloop will be run before */
              /* checking the dimensions of the frame. */
              if (s[k]==0)
                goto end;
            }
        }
    }
 check_msg: ;
  if (msg!=NULL)
    {
      for (n=0; n!=N; ++n)
        scm_array_handle_release(ah+n);
      scm_misc_error("array-slice-for-each", msg, scm_cons(frame_rank, args));
    }
  /* prepare moving cells. */
  for (n=0; n!=N; ++n)
    {
      ai[n] = scm_i_make_array(rank[n]-frank);
      SCM_I_ARRAY_SET_V (ai[n], scm_shared_array_root(args_[n]));
      /* FIXME scm_array_handle_base (ah+n) should be in Guile */
      SCM_I_ARRAY_SET_BASE (ai[n], ah[n].base);
      ais = SCM_I_ARRAY_DIMS(ai[n]);
      for (k=frank; k!=rank[n]; ++k)
        {
          ais[k-frank] = as[n][k];
        }
    }
  /* prepare rest list for callee. */
  {
    SCM *p = &dargs_;
    for (n=0; n<N; ++n)
      {
        *p = scm_cons (SCM_UNSPECIFIED, SCM_EOL);
        dargs[n] = SCM_CARLOC (*p);
        p = SCM_CDRLOC (*p);
      }
  }
  /* special case for rank 0. */
  if (frank==0)
    {
      for (n=0; n<N; ++n)
        *dargs[n] = ai[n];
      scm_apply_0(op, dargs_);
      for (n=0; n<N; ++n)
        scm_array_handle_release(ah+n);
      return SCM_UNSPECIFIED;
    }
  /* FIXME determine best looping order. */
  for (k=0; k!=frank; ++k)
    {
      i[k] = 0;
      order[k] = frank-1-k;
    }
  /* find outermost compact dim. */
  step = s[order[0]];
  ocd = 1;
  for (; ocd<frank; step *= s[order[ocd]], ++ocd)
    for (n=0; n!=N; ++n)
      if (step*as[n][order[0]].inc!=as[n][order[ocd]].inc)
        goto ocd_reached;
 ocd_reached: ;
  /* rank loop. */
  for (n=0; n!=N; ++n)
    base[n] = SCM_I_ARRAY_BASE(ai[n]);
  for (;;)
    {
      /* unrolled loop. */
      for (z=0; z!=step; ++z)
        {
          /* we are forced to create fresh array descriptors for each */
          /* call since we don't know whether the callee will keep them, */
          /* and Guile offers no way to copy the descriptor (since */
          /* descriptors are immutable). Yet another reason why this */
          /* should be in Scheme. */
          for (n=0; n<N; ++n)
            {
              *dargs[n] = scm_i_array_rebase(ai[n], base[n]);
              base[n] += as[n][order[0]].inc;
            }
          scm_apply_0(op, dargs_);
        }
      for (n=0; n<N; ++n)
        base[n] -= step*as[n][order[0]].inc;
      for (k=ocd; ; ++k)
        {
          if (k==frank)
            goto end;
          else if (i[order[k]]<s[order[k]]-1)
            {
              ++i[order[k]];
              for (n=0; n<N; ++n)
                base[n] += as[n][order[k]].inc;
              break;
            }
          else
            {
              i[order[k]] = 0;
              for (n=0; n<N; ++n)
                base[n] += as[n][order[k]].inc*(1-s[order[k]]);
            }
        }
    }
 end:;
  for (n=0; n<N; ++n)
    scm_array_handle_release(ah+n);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_array_slice_for_each_in_order, "array-slice-for-each-in-order", 2, 0, 1,
            (SCM frank, SCM op, SCM a),
            "Same as array-slice-for-each, but visit the cells sequentially\n"
            "and in row-major order.\n")
#define FUNC_NAME s_scm_array_slice_for_each_in_order
{
  return scm_array_slice_for_each (frank, op, a);
}
#undef FUNC_NAME


void
scm_init_array_map (void)
{
#include "libguile/array-map.x"
  scm_add_feature (s_scm_array_for_each);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
