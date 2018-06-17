/* classes: h_files */

#ifndef SCM__SCM_H
#define SCM__SCM_H

/* Copyright (C) 1995-1996,2000-2002,2006,2008-2011,2013-2014,2018
 *   Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



/**********************************************************************
 This file is Guile's central private header.

 When included by other files, this file should preceed any include
 other than __scm.h.  See __scm.h for details regarding the purpose of
 and differences between _scm.h and __scm.h.
 **********************************************************************/

#include <alignof.h>
#include "libguile/__scm.h"

/* Include headers for those files central to the implementation.  The
   rest should be explicitly #included in the C files themselves.  */
#include "libguile/error.h"	/* Everyone signals errors.  */
#include "libguile/print.h"	/* Everyone needs to print.  */
#include "libguile/pairs.h"	/* Everyone conses.  */
#include "libguile/list.h"	/* Everyone makes lists.  */
#include "libguile/gc.h"	/* Everyone allocates.  */
#include "libguile/gsubr.h"	/* Everyone defines global functions.  */
#include "libguile/procs.h"	/* Same.  */
#include "libguile/numbers.h"	/* Everyone deals with fixnums.  */
#include "libguile/symbols.h"	/* For length, chars, values, miscellany.  */
#include "libguile/boolean.h"	/* Everyone wonders about the truth.  */
#include "libguile/threads.h"	/* You are not alone. */
#include "libguile/snarf.h"	/* Everyone snarfs. */
#include "libguile/foreign.h"	/* Snarfing needs the foreign data structures. */
#include "libguile/programs.h"	/* ... and program.h. */
#include "libguile/variable.h"
#include "libguile/modules.h"
#include "libguile/inline.h"
#include "libguile/strings.h"


#endif  /* SCM__SCM_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
