/* Copyright 2009,2018
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

/* Make sure `scm_take_u8vector ()' returns a u8vector that actually uses the
   provided storage.  */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

#include <stdlib.h>


static void *
do_test (void *result)
{
#define LEN 123
  SCM u8v;
  uint8_t *data;
  scm_t_array_handle handle;

  data = scm_malloc (LEN);
  u8v = scm_take_u8vector (data, LEN);

  scm_array_get_handle (u8v, &handle);

  if (scm_array_handle_u8_writable_elements (&handle) == data
      && scm_array_handle_u8_elements (&handle) == data)
    * (int *) result = EXIT_SUCCESS;
  else
    * (int *) result = EXIT_FAILURE;

  scm_array_handle_release (&handle);

  return NULL;
#undef LEN
}

int
main (int argc, char *argv[])
{
  int result;

  scm_with_guile (do_test, &result);

  return result;
}
