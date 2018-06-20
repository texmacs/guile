/* Copyright 1995-1998,2000-2006,2009-2014,2018
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
#  include <config.h>
#endif

#include "error.h"
#include "gsubr.h"

#include "generalized-vectors.h"


struct scm_t_vector_ctor
{
  SCM tag;
  SCM (*ctor)(SCM, SCM);
};

#define VECTOR_CTORS_N_STATIC_ALLOC 20
static struct scm_t_vector_ctor vector_ctors[VECTOR_CTORS_N_STATIC_ALLOC];
static int num_vector_ctors_registered = 0;

void
scm_i_register_vector_constructor (SCM type, SCM (*ctor)(SCM, SCM))
{
  if (num_vector_ctors_registered >= VECTOR_CTORS_N_STATIC_ALLOC)
    /* need to increase VECTOR_CTORS_N_STATIC_ALLOC, buster */
    abort ();
  else
    { 
      vector_ctors[num_vector_ctors_registered].tag = type;
      vector_ctors[num_vector_ctors_registered].ctor = ctor;
      num_vector_ctors_registered++;
    }
}

SCM_DEFINE (scm_make_generalized_vector, "make-generalized-vector", 2, 1, 0,
            (SCM type, SCM len, SCM fill),
            "Make a generalized vector")
#define FUNC_NAME s_scm_make_generalized_vector
{
  int i;
  for (i = 0; i < num_vector_ctors_registered; i++)
    if (scm_is_eq (vector_ctors[i].tag, type))
      return vector_ctors[i].ctor(len, fill);
  scm_wrong_type_arg_msg (FUNC_NAME, SCM_ARG1, type, "array type");
}
#undef FUNC_NAME

void
scm_init_generalized_vectors ()
{
#include "generalized-vectors.x"
}
