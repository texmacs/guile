/* Copyright 2003-2004,2006,2008-2018
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

#define SCM_BUILDING_DEPRECATED_CODE

#include "deprecation.h"

#include "deprecated.h"

#if (SCM_ENABLE_DEPRECATED == 1)



/* Newly deprecated code goes here.  */



void
scm_i_init_deprecated ()
{
#include "deprecated.x"
}

#endif /* SCM_ENABLE_DEPRECATD == 1 */
