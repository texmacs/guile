/* Copyright 2000-2001,2006,2008-2009,2011-2013,2016-2019
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "feature.h"
#include "gc.h"
#include "gsubr.h"
#include "list.h"
#include "numbers.h"
#include "pairs.h"

#include "values.h"


/* OBJ must be a values object containing exactly two values.
   scm_i_extract_values_2 puts those two values into *p1 and *p2.  */
void
scm_i_extract_values_2 (SCM obj, SCM *p1, SCM *p2)
{
  SCM_ASSERT_TYPE (scm_is_values (obj), obj, SCM_ARG1,
		   "scm_i_extract_values_2", "values");
  if (scm_i_nvalues (obj) != 2)
    scm_wrong_type_arg_msg
      ("scm_i_extract_values_2", SCM_ARG1, obj,
       "a values object containing exactly two values");

  *p1 = scm_i_value_ref (obj, 0);
  *p2 = scm_i_value_ref (obj, 1);
}

size_t
scm_c_nvalues (SCM obj)
{
  if (SCM_LIKELY (scm_is_values (obj)))
    return scm_i_nvalues (obj);
  else
    return 1;
}

SCM
scm_c_value_ref (SCM obj, size_t idx)
{
  if (scm_is_values (obj))
    {
      if (idx < scm_i_nvalues (obj))
        return scm_i_value_ref (obj, idx);
    }
  else
    {
      if (idx == 0)
        return obj;
    }

  scm_error (scm_out_of_range_key,
	     "scm_c_value_ref",
	     "Too few values in ~S to access index ~S",
             scm_list_2 (obj, scm_from_size_t (idx)),
             scm_list_1 (scm_from_size_t (idx)));
}

SCM_DEFINE (scm_values, "values", 0, 0, 1,
	    (SCM args),
	    "Delivers all of its arguments to its continuation.  Except for\n"
	    "continuations created by the @code{call-with-values} procedure,\n"
	    "all continuations take exactly one value.  The effect of\n"
	    "passing no value or more than one value to continuations that\n"
	    "were not created by @code{call-with-values} is unspecified.")
#define FUNC_NAME s_scm_values
{
  long n;
  SCM result;

  SCM_VALIDATE_LIST_COPYLEN (1, args, n);
  if (n == 1)
    result = SCM_CAR (args);
  else
    {
      size_t i;

      if ((size_t) n > (size_t) (UINTPTR_MAX >> 8))
        scm_error (scm_out_of_range_key, FUNC_NAME, "Too many values",
                   SCM_EOL, SCM_EOL);

      result = scm_words ((((scm_t_bits) n) << 8) | scm_tc7_values, n + 1);
      for (i = 0; i < n; i++, args = SCM_CDR (args))
        SCM_SET_CELL_OBJECT (result, i + 1, SCM_CAR (args));
    }

  return result;
}
#undef FUNC_NAME

SCM
scm_c_values (SCM *base, size_t nvalues)
{
  SCM ret;
  size_t i;

  if (nvalues == 1)
    return *base;

  if ((uintptr_t) nvalues > (UINTPTR_MAX >> 8))
    scm_error (scm_out_of_range_key, "scm_c_values", "Too many values",
               SCM_EOL, SCM_EOL);

  ret = scm_words ((((scm_t_bits) nvalues) << 8) | scm_tc7_values, nvalues + 1);

  for (i = 0; i < nvalues; i++)
    SCM_SET_CELL_OBJECT (ret, i + 1, base[i]);

  return ret;
}

SCM
scm_values_2 (SCM a, SCM b)
{
  SCM ret;

  ret = scm_words ((2 << 8) | scm_tc7_values, 3);
  SCM_SET_CELL_OBJECT_1 (ret, a);
  SCM_SET_CELL_OBJECT_2 (ret, b);

  return ret;
}

SCM
scm_values_3 (SCM a, SCM b, SCM c)
{
  SCM ret;

  ret = scm_words ((3 << 8) | scm_tc7_values, 4);
  SCM_SET_CELL_OBJECT_1 (ret, a);
  SCM_SET_CELL_OBJECT_2 (ret, b);
  SCM_SET_CELL_OBJECT_3 (ret, c);

  return ret;
}

void
scm_init_values (void)
{
  scm_add_feature ("values");

#include "values.x"
}
