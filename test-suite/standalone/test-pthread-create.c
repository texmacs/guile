/* Copyright 2011,2018
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

/* Test whether threads created with `pthread_create' work (bug #32436)
   when then main thread is the one that initializes Guile.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <pthread.h>
#include <stdlib.h>
#include <libguile.h>

static void *
do_something (void *arg)
{
  scm_list_copy (scm_make_list (scm_from_int (1234), SCM_BOOL_T));
  scm_gc ();
  return NULL;
}

static void *
thread (void *arg)
{
  scm_with_guile (do_something, NULL);
  return NULL;
}

static void *
inner_main (void *data)
{
  int i;
  pthread_t thr;

  do_something (NULL);

  for (i = 0; i < 77; i++)
    {
      pthread_create (&thr, NULL, thread, NULL);
      pthread_join (thr, NULL);
    }

  return NULL;
}


int
main (int argc, char *argv[])
{
  scm_with_guile (inner_main, NULL);

  return EXIT_SUCCESS;
}
