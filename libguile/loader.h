/* Copyright 2001,2009-2015,2018
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

#ifndef _SCM_LOADER_H_
#define _SCM_LOADER_H_

#include <libguile/scm.h>

#ifdef BUILDING_LIBGUILE

/* The endianness marker in objcode.  */
#ifdef WORDS_BIGENDIAN
# define SCM_OBJCODE_ENDIANNESS "BE"
#else
# define SCM_OBJCODE_ENDIANNESS "LE"
#endif

#define _SCM_CPP_STRINGIFY(x)  # x
#define SCM_CPP_STRINGIFY(x)   _SCM_CPP_STRINGIFY (x)

/* The word size marker in objcode.  */
#define SCM_OBJCODE_WORD_SIZE  SCM_CPP_STRINGIFY (SIZEOF_VOID_P)

/* Major and minor versions must be single characters. */
#define SCM_OBJCODE_MAJOR_VERSION 4
#define SCM_OBJCODE_MINIMUM_MINOR_VERSION 1
#define SCM_OBJCODE_MINOR_VERSION 1
#define SCM_OBJCODE_MAJOR_VERSION_STRING        \
  SCM_CPP_STRINGIFY(SCM_OBJCODE_MAJOR_VERSION)
#define SCM_OBJCODE_MINOR_VERSION_STRING        \
  SCM_CPP_STRINGIFY(SCM_OBJCODE_MINOR_VERSION)
#define SCM_OBJCODE_VERSION_STRING                                      \
  SCM_OBJCODE_MAJOR_VERSION_STRING "." SCM_OBJCODE_MINOR_VERSION_STRING
#define SCM_OBJCODE_MACHINE_VERSION_STRING                              \
  SCM_OBJCODE_ENDIANNESS "-" SCM_OBJCODE_WORD_SIZE "-" SCM_OBJCODE_VERSION_STRING

#endif

SCM_API SCM scm_load_thunk_from_file (SCM filename);
SCM_API SCM scm_load_thunk_from_memory (SCM bv);

SCM_INTERNAL const uint8_t *
scm_find_slot_map_unlocked (const uint32_t *ip);

SCM_INTERNAL void scm_bootstrap_loader (void);
SCM_INTERNAL void scm_init_loader (void);

#endif /* _SCM_LOADER_H_ */
