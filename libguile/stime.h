#ifndef SCM_STIME_H
#define SCM_STIME_H

/* Copyright 1995-1998,2000,2003,2006,2008,2011,2018
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



SCM_API long scm_c_time_units_per_second;
#define SCM_TIME_UNITS_PER_SECOND scm_c_time_units_per_second


SCM_API long scm_c_get_internal_run_time (void);
SCM_API SCM scm_get_internal_real_time (void);
SCM_API SCM scm_get_internal_run_time (void);
SCM_API SCM scm_current_time (void);
SCM_API SCM scm_gettimeofday (void);
SCM_API SCM scm_localtime (SCM time, SCM zone);
SCM_API SCM scm_gmtime (SCM time);
SCM_API SCM scm_mktime (SCM sbd_time, SCM zone);
SCM_API SCM scm_tzset (void);
SCM_API SCM scm_times (void);
SCM_API SCM scm_strftime (SCM format, SCM stime);
SCM_API SCM scm_strptime (SCM format, SCM string);
SCM_INTERNAL void scm_init_stime (void);

#endif  /* SCM_STIME_H */
