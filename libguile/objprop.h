#ifndef SCM_OBJPROP_H
#define SCM_OBJPROP_H

/* Copyright 1995,2000-2001,2006,2008,2018
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



SCM_API SCM scm_object_properties (SCM obj);
SCM_API SCM scm_set_object_properties_x (SCM obj, SCM plist);
SCM_API SCM scm_object_property (SCM obj, SCM key);
SCM_API SCM scm_set_object_property_x (SCM obj, SCM key, SCM val);
SCM_INTERNAL void scm_init_objprop (void);

#endif  /* SCM_OBJPROP_H */
