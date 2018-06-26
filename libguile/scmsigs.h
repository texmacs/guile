#ifndef SCM_SCMSIGS_H
#define SCM_SCMSIGS_H

/* Copyright 1995-1998,2000,2002,2006-2008,2018
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



SCM_API SCM scm_sigaction (SCM signum, SCM handler, SCM flags);
SCM_API SCM scm_sigaction_for_thread (SCM signum, SCM handler, SCM flags,
				      SCM thread);
SCM_API SCM scm_restore_signals (void);
SCM_API SCM scm_alarm (SCM i);
SCM_API SCM scm_setitimer (SCM which_timer,
			   SCM interval_seconds, SCM interval_microseconds,
			   SCM value_seconds, SCM value_microseconds);
SCM_API SCM scm_getitimer (SCM which_timer);
SCM_API SCM scm_pause (void);
SCM_API SCM scm_sleep (SCM i);
SCM_API SCM scm_usleep (SCM i);
SCM_API SCM scm_raise (SCM sig);
SCM_INTERNAL void scm_init_scmsigs (void);

SCM_INTERNAL void scm_i_close_signal_pipe (void);
SCM_INTERNAL void scm_i_ensure_signal_delivery_thread (void);

SCM_INTERNAL scm_thread *scm_i_signal_delivery_thread;

#endif  /* SCM_SCMSIGS_H */
