#ifndef SCM_SCRIPT_H
#define SCM_SCRIPT_H

/* Copyright 1997-1998,2000,2006,2008,2011,2018
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



#include "libguile/scm.h"


SCM_API char **scm_get_meta_args (int argc, char **argv);
SCM_API int scm_count_argv (char **argv);
SCM_API void scm_shell_usage (int fatal, char *message);
SCM_API SCM scm_compile_shell_switches (int argc, char **argv);
SCM_API void scm_shell (int argc, char **argv);
SCM_API char *scm_usage_name;
SCM_INTERNAL void scm_i_set_boot_program_arguments (int argc, char *argv[]);
SCM_INTERNAL void scm_init_script (void);

#endif  /* SCM_SCRIPT_H */
