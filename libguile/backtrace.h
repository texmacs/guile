#ifndef SCM_BACKTRACE_H
#define SCM_BACKTRACE_H

/* Copyright 1996,1998-2001,2004,2006,2008,2010-2011,2018
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

SCM_API SCM scm_print_exception (SCM port, SCM frame, SCM key, SCM args);

SCM_API void scm_display_error_message (SCM message, SCM args, SCM port);
SCM_INTERNAL void scm_i_display_error (SCM frame, SCM port, SCM subr,
				       SCM message, SCM args, SCM rest);
SCM_API SCM scm_display_error (SCM frame, SCM port, SCM subr, SCM message, SCM args, SCM rest);
SCM_API SCM scm_display_application (SCM frame, SCM port, SCM indent);
SCM_API SCM scm_display_backtrace (SCM stack, SCM port, SCM first, SCM depth);
SCM_API SCM scm_display_backtrace_with_highlights (SCM stack, SCM port, SCM first, SCM depth, SCM highlights);
SCM_API SCM scm_backtrace (void);
SCM_API SCM scm_backtrace_with_highlights (SCM highlights);

SCM_INTERNAL void scm_init_backtrace (void);

#endif  /* SCM_BACKTRACE_H */
