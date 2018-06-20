/* Copyright 1999-2001,2003,2006,2008,2018
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

#include <libguile.h>

SCM init2_count;

void libtest_extensions_init2 (void);
void libtest_extensions_init (void);

void
libtest_extensions_init2 (void)
{
  scm_variable_set_x (init2_count,
                      scm_from_int (scm_to_int (scm_variable_ref (init2_count)) + 1));
}

void
libtest_extensions_init (void)
{
  scm_c_define ("init2-count", scm_from_int (0));
  init2_count = scm_permanent_object (scm_c_lookup ("init2-count"));
  scm_c_register_extension ("libtest-extensions", "libtest_extensions_init2",
                            (scm_t_extension_init_func)libtest_extensions_init2, NULL);
}
