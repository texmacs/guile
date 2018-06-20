#ifndef SCM_NET_DB_H
#define SCM_NET_DB_H

/* Copyright 1995,2000-2001,2006,2008,2010,2018
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



SCM_API SCM scm_gethost (SCM host);
SCM_API SCM scm_getnet (SCM name);
SCM_API SCM scm_getproto (SCM name);
SCM_API SCM scm_getserv (SCM name, SCM proto);
SCM_API SCM scm_sethost (SCM arg);
SCM_API SCM scm_setnet (SCM arg);
SCM_API SCM scm_setproto (SCM arg);
SCM_API SCM scm_setserv (SCM arg);
SCM_API SCM scm_getaddrinfo (SCM, SCM, SCM, SCM, SCM, SCM);
SCM_API SCM scm_gai_strerror (SCM);
SCM_INTERNAL void scm_init_net_db (void);

#endif  /* SCM_NET_DB_H */
