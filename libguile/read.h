#ifndef SCM_READ_H
#define SCM_READ_H

/* Copyright 1995-1996,2000,2006,2008-2009,2018
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



#include "libguile/options.h"


/* SCM_LINE_INCREMENTORS are the characters which cause the line count to
 * be incremented for the purposes of error reporting.  This feature
 * is only used for scheme code loaded from files.
 *
 * SCM_WHITE_SPACES are other characters which should be treated like spaces
 * in programs.
 */

#define SCM_LINE_INCREMENTORS  '\n'

#ifdef MSDOS
# define SCM_SINGLE_SPACES  ' ':case '\r':case '\f': case 26
#else
# define SCM_SINGLE_SPACES  ' ':case '\r':case '\f'
#endif

#define SCM_WHITE_SPACES  SCM_SINGLE_SPACES: case '\t'





SCM_API SCM scm_sym_dot;

SCM_API SCM scm_read_options (SCM setting);
SCM_API SCM scm_read (SCM port);
SCM_API SCM scm_read_hash_extend (SCM chr, SCM proc);
SCM_INTERNAL char *scm_i_scan_for_encoding (SCM port);
SCM_API SCM scm_file_encoding (SCM port);

SCM_INTERNAL void scm_i_input_error (const char *func, SCM port,
				     const char *message, SCM arg)
  SCM_NORETURN;

SCM_INTERNAL void scm_init_read (void);

#endif  /* SCM_READ_H */
