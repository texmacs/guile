/* Copyright 1995-1998,2000-2006,2009-2010,2013-2014,2018
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

#include "uniform.h"


const size_t scm_i_array_element_type_sizes[SCM_ARRAY_ELEMENT_TYPE_LAST + 1] = {
  0,
  0,
  1,
  8,
  8, 8,
  16, 16,
  32, 32,
  64, 64,
  32, 64,
  64, 128
};

size_t
scm_array_handle_uniform_element_size (scm_t_array_handle *h)
{
  size_t ret = scm_i_array_element_type_sizes[h->element_type];
  if (ret && ret % 8 == 0)
    return ret / 8;
  else if (ret)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "byte-aligned uniform array");
  else
    scm_wrong_type_arg_msg (NULL, 0, h->array, "uniform array");
}

size_t
scm_array_handle_uniform_element_bit_size (scm_t_array_handle *h)
{
  size_t ret = scm_i_array_element_type_sizes[h->element_type];
  if (ret)
    return ret;
  else
    scm_wrong_type_arg_msg (NULL, 0, h->array, "uniform array");
}

const void *
scm_array_handle_uniform_elements (scm_t_array_handle *h)
{
  size_t esize;
  const uint8_t *ret;

  esize = scm_array_handle_uniform_element_size (h);
  ret = ((const uint8_t *) h->elements) + h->base * esize;
  return ret;
}

void *
scm_array_handle_uniform_writable_elements (scm_t_array_handle *h)
{
  if (h->writable_elements != h->elements)
    scm_wrong_type_arg_msg (NULL, 0, h->array, "mutable array");

  return (void *) scm_array_handle_uniform_elements (h);
}

void
scm_init_uniform (void)
{
#include "uniform.x"
}
