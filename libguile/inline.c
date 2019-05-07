/* Copyright 2001,2006,2008,2011-2013,2018-2019
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

#define SCM_IMPLEMENT_INLINES 1
#define SCM_INLINE_C_IMPLEMENTING_INLINES 1

#include "array-handle.h"
#include "chars.h"
#include "gc.h"
#include "pairs.h"
#include "ports.h"
#include "smob.h"
#include "strings.h"
