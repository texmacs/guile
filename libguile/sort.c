/* Copyright (C) 1999, 2000, 2001, 2002, 2004, 2006, 2007, 2008, 2009,
 *   2010, 2011, 2012, 2014 Free Software Foundation, Inc.
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



/* Written in December 1998 by Roland Orre <orre@nada.kth.se>
 * This implements the same sort interface as slib/sort.scm
 * for lists and vectors where slib defines:
 * sorted?, merge, merge!, sort, sort!
 * For scsh compatibility sort-list and sort-list! are also defined.
 * In cases where a stable-sort is required use stable-sort or
 * stable-sort!.  An additional feature is
 * (restricted-vector-sort! vector less? startpos endpos)
 * which allows you to sort part of a vector.
 * Thanks to Aubrey Jaffer for the slib/sort.scm library.
 * Thanks to Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
 * for the merge sort inspiration.
 * Thanks to Douglas C. Schmidt (schmidt@ics.uci.edu) for the
 * quicksort code.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/arrays.h"
#include "libguile/array-map.h"
#include "libguile/feature.h"
#include "libguile/vectors.h"
#include "libguile/async.h"
#include "libguile/dynwind.h"

#include "libguile/validate.h"
#include "libguile/sort.h"

/* We have two quicksort variants: one for SCM (#t) arrays and one for
   typed arrays.
*/

#define NAME        quicksort
#define INC_PARAM   ssize_t inc,
#define VEC_PARAM   SCM * ra,
#define GET(i)      ra[(i)*inc]
#define SET(i, val) ra[(i)*inc] = val
#include "libguile/quicksort.i.c"

#define NAME        quicksorta
#define INC_PARAM
#define VEC_PARAM   scm_t_array_handle * const ra,
#define GET(i)      scm_array_handle_ref (ra, scm_array_handle_pos_1 (ra, i))
#define SET(i, val) scm_array_handle_set (ra, scm_array_handle_pos_1 (ra, i), val)
#include "libguile/quicksort.i.c"

SCM_DEFINE (scm_restricted_vector_sort_x, "restricted-vector-sort!", 4, 0, 0,
            (SCM vec, SCM less, SCM startpos, SCM endpos),
	    "Sort the vector @var{vec}, using @var{less} for comparing\n"
	    "the vector elements.  @var{startpos} (inclusively) and\n"
	    "@var{endpos} (exclusively) delimit\n"
	    "the range of the vector which gets sorted.  The return value\n"
	    "is not specified.")
#define FUNC_NAME s_scm_restricted_vector_sort_x
{
  ssize_t spos = scm_to_ssize_t (startpos);
  ssize_t epos = scm_to_ssize_t (endpos)-1;

  scm_t_array_handle handle;
  scm_t_array_dim const * dims;
  scm_array_get_handle (vec, &handle);
  dims = scm_array_handle_dims (&handle);

  if (scm_array_handle_rank(&handle) != 1)
    {
      scm_array_handle_release (&handle);
      scm_misc_error (FUNC_NAME, "rank must be 1", scm_list_1 (vec));
    }
  if (spos < dims[0].lbnd)
    {
      scm_array_handle_release (&handle);
      scm_error (scm_out_of_range_key, FUNC_NAME, "startpos ~s out of range of ~s",
                 scm_list_2 (startpos, vec), scm_list_1 (startpos));
    }
  if (epos > dims[0].ubnd)
    {
      scm_array_handle_release (&handle);
      scm_error (scm_out_of_range_key, FUNC_NAME, "endpos ~s out of range of ~s",
                 scm_list_2 (endpos, vec), scm_list_1 (endpos));
    }
  if (handle.element_type == SCM_ARRAY_ELEMENT_TYPE_SCM)
    quicksort (scm_array_handle_writable_elements (&handle) - dims[0].lbnd * dims[0].inc,
               spos, epos, dims[0].inc, less);
  else
    quicksorta (&handle, spos, epos, less);

  scm_array_handle_release (&handle);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* (sorted? sequence less?)
 * is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
 * such that for all 1 <= i <= m,
 * (not (less? (list-ref list i) (list-ref list (- i 1)))). */
SCM_DEFINE (scm_sorted_p, "sorted?", 2, 0, 0,
            (SCM items, SCM less),
	    "Return @code{#t} iff @var{items} is a list or vector such that, "
	    "for each element @var{x} and the next element @var{y} of "
	    "@var{items}, @code{(@var{less} @var{y} @var{x})} returns "
	    "@code{#f}.")
#define FUNC_NAME s_scm_sorted_p
{
  long len, j;			/* list/vector length, temp j */
  SCM item, rest;		/* rest of items loop variable */

  if (SCM_NULL_OR_NIL_P (items))
    return SCM_BOOL_T;

  if (scm_is_pair (items))
    {
      len = scm_ilength (items); /* also checks that it's a pure list */
      SCM_ASSERT_RANGE (1, items, len >= 0);
      if (len <= 1)
	return SCM_BOOL_T;

      item = SCM_CAR (items);
      rest = SCM_CDR (items);
      j = len - 1;
      while (j > 0)
	{
	  if (scm_is_true (scm_call_2 (less, SCM_CAR (rest), item)))
	    return SCM_BOOL_F;
	  else
	    {
	      item = SCM_CAR (rest);
	      rest = SCM_CDR (rest);
	      j--;
	    }
	}
      return SCM_BOOL_T;
    }
  else
    {
      SCM result = SCM_BOOL_T;
      ssize_t i, end;
      scm_t_array_handle handle;
      scm_t_array_dim const * dims;
      scm_array_get_handle (items, &handle);
      dims = scm_array_handle_dims (&handle);

      if (scm_array_handle_rank(&handle) != 1)
        {
          scm_array_handle_release (&handle);
          scm_error (scm_misc_error_key, FUNC_NAME, "rank must be 1", items, SCM_EOL);
        }

      if (handle.element_type == SCM_ARRAY_ELEMENT_TYPE_SCM)
        {
          ssize_t inc = dims[0].inc;
          const SCM *elts = scm_array_handle_elements (&handle);
          for (i = dims[0].lbnd+1, end = dims[0].ubnd+1; i < end; ++i, elts += inc)
            {
              if (scm_is_true (scm_call_2 (less, elts[inc], elts[0])))
                {
                  result = SCM_BOOL_F;
                  break;
                }
            }
        }
      else
        {
          for (i = 1, end = dims[0].ubnd-dims[0].lbnd+1; i < end; ++i)
            {
              if (scm_is_true (scm_call_2 (less,
                                           scm_array_handle_ref (&handle, i*dims[0].inc),
                                           scm_array_handle_ref (&handle, (i-1)*dims[0].inc))))
                {
                  result = SCM_BOOL_F;
                  break;
                }
            }
        }

      scm_array_handle_release (&handle);
      return result;
    }
}
#undef FUNC_NAME


/* (merge a b less?)
   takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
   and returns a new list in which the elements of a and b have been stably
   interleaved so that (sorted? (merge a b less?) less?).
   Note:  this does _not_ accept vectors. */
SCM_DEFINE (scm_merge, "merge", 3, 0, 0,
            (SCM alist, SCM blist, SCM less),
	    "Merge two already sorted lists into one.\n"
	    "Given two lists @var{alist} and @var{blist}, such that\n"
	    "@code{(sorted? alist less?)} and @code{(sorted? blist less?)},\n"
	    "return a new list in which the elements of @var{alist} and\n"
	    "@var{blist} have been stably interleaved so that\n"
	    "@code{(sorted? (merge alist blist less?) less?)}.\n"
	    "Note:  this does _not_ accept vectors.")
#define FUNC_NAME s_scm_merge
{
  SCM build;

  if (SCM_NULL_OR_NIL_P (alist))
    return blist;
  else if (SCM_NULL_OR_NIL_P (blist))
    return alist;
  else
    {
      long alen, blen;		/* list lengths */
      SCM last;

      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (1, alist, alen);
      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (2, blist, blen);
      if (scm_is_true (scm_call_2 (less, SCM_CAR (blist), SCM_CAR (alist))))
	{
	  build = scm_cons (SCM_CAR (blist), SCM_EOL);
	  blist = SCM_CDR (blist);
	  blen--;
	}
      else
	{
	  build = scm_cons (SCM_CAR (alist), SCM_EOL);
	  alist = SCM_CDR (alist);
	  alen--;
	}
      last = build;
      while ((alen > 0) && (blen > 0))
	{
	  SCM_TICK;
	  if (scm_is_true (scm_call_2 (less, SCM_CAR (blist), SCM_CAR (alist))))
	    {
	      SCM_SETCDR (last, scm_cons (SCM_CAR (blist), SCM_EOL));
	      blist = SCM_CDR (blist);
	      blen--;
	    }
	  else
	    {
	      SCM_SETCDR (last, scm_cons (SCM_CAR (alist), SCM_EOL));
	      alist = SCM_CDR (alist);
	      alen--;
	    }
	  last = SCM_CDR (last);
	}
      if ((alen > 0) && (blen == 0))
	SCM_SETCDR (last, alist);
      else if ((alen == 0) && (blen > 0))
	SCM_SETCDR (last, blist);
    }
  return build;
}
#undef FUNC_NAME


static SCM
scm_merge_list_x (SCM alist, SCM blist,
		  long alen, long blen,
		  SCM less)
{
  SCM build, last;

  if (SCM_NULL_OR_NIL_P (alist))
    return blist;
  else if (SCM_NULL_OR_NIL_P (blist))
    return alist;
  else
    {
      if (scm_is_true (scm_call_2 (less, SCM_CAR (blist), SCM_CAR (alist))))
	{
	  build = blist;
	  blist = SCM_CDR (blist);
	  blen--;
	}
      else
	{
	  build = alist;
	  alist = SCM_CDR (alist);
	  alen--;
	}
      last = build;
      while ((alen > 0) && (blen > 0))
	{
	  SCM_TICK;
	  if (scm_is_true (scm_call_2 (less, SCM_CAR (blist), SCM_CAR (alist))))
	    {
	      scm_set_cdr_x (last, blist);
	      blist = SCM_CDR (blist);
	      blen--;
	    }
	  else
	    {
	      scm_set_cdr_x (last, alist);
	      alist = SCM_CDR (alist);
	      alen--;
	    }
	  last = SCM_CDR (last);
	}
      if ((alen > 0) && (blen == 0))
	scm_set_cdr_x (last, alist);
      else if ((alen == 0) && (blen > 0))
	scm_set_cdr_x (last, blist);
    }
  return build;
}				/* scm_merge_list_x */


SCM_DEFINE (scm_merge_x, "merge!", 3, 0, 0,
            (SCM alist, SCM blist, SCM less),
	    "Takes two lists @var{alist} and @var{blist} such that\n"
	    "@code{(sorted? alist less?)} and @code{(sorted? blist less?)} and\n"
	    "returns a new list in which the elements of @var{alist} and\n"
	    "@var{blist} have been stably interleaved so that\n"
	    " @code{(sorted? (merge alist blist less?) less?)}.\n"
	    "This is the destructive variant of @code{merge}\n"
	    "Note:  this does _not_ accept vectors.")
#define FUNC_NAME s_scm_merge_x
{
  if (SCM_NULL_OR_NIL_P (alist))
    return blist;
  else if (SCM_NULL_OR_NIL_P (blist))
    return alist;
  else
    {
      long alen, blen;		/* list lengths */
      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (1, alist, alen);
      SCM_VALIDATE_NONEMPTYLIST_COPYLEN (2, blist, blen);
      return scm_merge_list_x (alist, blist, alen, blen, less);
    }
}
#undef FUNC_NAME


/* This merge sort algorithm is same as slib's by Richard A. O'Keefe.
   The algorithm is stable. We also tried to use the algorithm used by
   scsh's merge-sort but that algorithm showed to not be stable, even
   though it claimed to be.
*/
static SCM
scm_merge_list_step (SCM * seq, SCM less, long n)
{
  SCM a, b;

  if (n > 2)
    {
      long mid = n / 2;
      SCM_TICK;
      a = scm_merge_list_step (seq, less, mid);
      b = scm_merge_list_step (seq, less, n - mid);
      return scm_merge_list_x (a, b, mid, n - mid, less);
    }
  else if (n == 2)
    {
      SCM p = *seq;
      SCM rest = SCM_CDR (*seq);
      SCM x = SCM_CAR (*seq);
      SCM y = SCM_CAR (SCM_CDR (*seq));
      *seq = SCM_CDR (rest);
      SCM_SETCDR (rest, SCM_EOL);
      if (scm_is_true (scm_call_2 (less, y, x)))
	{
	  SCM_SETCAR (p, y);
	  SCM_SETCAR (rest, x);
	}
      return p;
    }
  else if (n == 1)
    {
      SCM p = *seq;
      *seq = SCM_CDR (p);
      SCM_SETCDR (p, SCM_EOL);
      return p;
    }
  else
    return SCM_EOL;
}				/* scm_merge_list_step */


#define SCM_VALIDATE_MUTABLE_LIST(pos, lst)                             \
  do {                                                                  \
    SCM walk;                                                           \
    for (walk = lst; !scm_is_null_or_nil (walk); walk = SCM_CDR (walk)) \
      SCM_VALIDATE_MUTABLE_PAIR (pos, walk);                            \
  } while (0)


SCM_DEFINE (scm_sort_x, "sort!", 2, 0, 0,
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector.  @var{less} is used for comparing the sequence\n"
	    "elements.  The sorting is destructive, that means that the\n"
	    "input sequence is modified to produce the sorted result.\n"
	    "This is not a stable sort.")
#define FUNC_NAME s_scm_sort_x
{
  long len;			/* list/vector length */
  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (scm_is_pair (items))
    {
      SCM_VALIDATE_LIST_COPYLEN (1, items, len);
      SCM_VALIDATE_MUTABLE_LIST (1, items);
      return scm_merge_list_step (&items, less, len);
    }
  else if (scm_is_array (items) && scm_c_array_rank (items) == 1)
    {
      scm_t_array_handle handle;
      scm_t_array_dim const * dims;
      scm_array_get_handle (items, &handle);
      dims = scm_array_handle_dims (&handle);

      if (scm_array_handle_rank (&handle) != 1)
        {
          scm_array_handle_release (&handle);
          scm_misc_error (FUNC_NAME, "rank must be 1", scm_list_1 (items));
        }

      scm_restricted_vector_sort_x (items,
				    less,
				    scm_from_ssize_t (dims[0].lbnd),
                                    scm_from_ssize_t (dims[0].ubnd+1));

      scm_array_handle_release (&handle);
      return items;
    }
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


SCM_DEFINE (scm_sort, "sort", 2, 0, 0,
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector.  @var{less} is used for comparing the sequence\n"
	    "elements.  This is not a stable sort.")
#define FUNC_NAME s_scm_sort
{
  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (scm_is_pair (items))
    return scm_sort_x (scm_list_copy (items), less);
  else if (scm_is_array (items) && scm_c_array_rank (items) == 1)
    {
      SCM copy;
      if (scm_c_array_rank (items) != 1)
        scm_error (scm_misc_error_key, FUNC_NAME, "rank must be 1", items, SCM_EOL);
      copy = scm_make_typed_array (scm_array_type (items), SCM_UNSPECIFIED, scm_array_dimensions (items));
      scm_array_copy_x (items, copy);
      return scm_sort_x (copy, less);
    }
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


static void
scm_merge_vector_x (SCM *vec,
		    SCM *temp,
		    SCM less,
		    size_t low,
		    size_t mid,
		    size_t high,
		    ssize_t inc)
{
  size_t it;	     	/* Index for temp vector */
  size_t i1 = low;      /* Index for lower vector segment */
  size_t i2 = mid + 1; 	/* Index for upper vector segment */

#define VEC(i) vec[(i)*inc]

  /* Copy while both segments contain more characters */
  for (it = low; (i1 <= mid) && (i2 <= high); ++it)
    {
      if (scm_is_true (scm_call_2 (less, VEC(i2), VEC(i1))))
	temp[it] = VEC(i2++);
      else
	temp[it] = VEC(i1++);
    }

  {
    /* Copy while first segment contains more characters */
    while (i1 <= mid)
      temp[it++] = VEC(i1++);

    /* Copy while second segment contains more characters */
    while (i2 <= high)
      temp[it++] = VEC(i2++);

    /* Copy back from temp to vp */
    for (it = low; it <= high; it++)
      VEC(it) = temp[it];
  }
} 	        		/* scm_merge_vector_x */


static void
scm_merge_vector_step (SCM *vec,
		       SCM *temp,
		       SCM less,
		       size_t low,
		       size_t high,
		       ssize_t inc)
{
  if (high > low)
    {
      size_t mid = (low + high) / 2;
      SCM_TICK;
      scm_merge_vector_step (vec, temp, less, low, mid, inc);
      scm_merge_vector_step (vec, temp, less, mid+1, high, inc);
      scm_merge_vector_x (vec, temp, less, low, mid, high, inc);
    }
}				/* scm_merge_vector_step */


SCM_DEFINE (scm_stable_sort_x, "stable-sort!", 2, 0, 0,
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector. @var{less} is used for comparing the sequence elements.\n"
	    "The sorting is destructive, that means that the input sequence\n"
	    "is modified to produce the sorted result.\n"
	    "This is a stable sort.")
#define FUNC_NAME s_scm_stable_sort_x
{
  long len;			/* list/vector length */

  if (SCM_NULL_OR_NIL_P (items))
    return items;

  if (scm_is_pair (items))
    {
      SCM_VALIDATE_LIST_COPYLEN (1, items, len);
      SCM_VALIDATE_MUTABLE_LIST (1, items);
      return scm_merge_list_step (&items, less, len);
    }
  else if (scm_is_array (items) && 1 == scm_c_array_rank (items))
    {
      scm_t_array_handle temp_handle, vec_handle;
      SCM temp, *temp_elts, *vec_elts;
      size_t len;
      ssize_t inc;

      vec_elts = scm_vector_writable_elements (items, &vec_handle,
					       &len, &inc);
      if (len == 0)
        {
          scm_array_handle_release (&vec_handle);
          return items;
        }

      temp = scm_c_make_vector (len, SCM_UNDEFINED);
      temp_elts = scm_vector_writable_elements (temp, &temp_handle,
						NULL, NULL);

      scm_merge_vector_step (vec_elts, temp_elts, less, 0, len-1, inc);

      scm_array_handle_release (&temp_handle);
      scm_array_handle_release (&vec_handle);

      return items;
    }
  else
    SCM_WRONG_TYPE_ARG (1, items);
}
#undef FUNC_NAME


SCM_DEFINE (scm_stable_sort, "stable-sort", 2, 0, 0,
            (SCM items, SCM less),
	    "Sort the sequence @var{items}, which may be a list or a\n"
	    "vector. @var{less} is used for comparing the sequence elements.\n"
	    "This is a stable sort.")
#define FUNC_NAME s_scm_stable_sort
{
  if (SCM_NULL_OR_NIL_P (items))
    return SCM_EOL;

  if (scm_is_pair (items))
    return scm_stable_sort_x (scm_list_copy (items), less);
  else
    return scm_stable_sort_x (scm_vector_copy (items), less);
}
#undef FUNC_NAME


SCM_DEFINE (scm_sort_list_x, "sort-list!", 2, 0, 0,
            (SCM items, SCM less),
	    "Sort the list @var{items}, using @var{less} for comparing the\n"
	    "list elements. The sorting is destructive, that means that the\n"
	    "input list is modified to produce the sorted result.\n"
	    "This is a stable sort.")
#define FUNC_NAME s_scm_sort_list_x
{
  long len;

  SCM_VALIDATE_LIST_COPYLEN (1, items, len);
  SCM_VALIDATE_MUTABLE_LIST (1, items);

  return scm_merge_list_step (&items, less, len);
}
#undef FUNC_NAME


SCM_DEFINE (scm_sort_list, "sort-list", 2, 0, 0,
	    (SCM items, SCM less),
	    "Sort the list @var{items}, using @var{less} for comparing the\n"
	    "list elements. This is a stable sort.")
#define FUNC_NAME s_scm_sort_list
{
  long len;

  SCM_VALIDATE_LIST_COPYLEN (1, items, len);
  items = scm_list_copy (items);
  return scm_merge_list_step (&items, less, len);
}
#undef FUNC_NAME


void
scm_init_sort ()
{
#include "libguile/sort.x"

  scm_add_feature ("sort");
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
