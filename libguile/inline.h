#ifndef SCM_INLINE_H
#define SCM_INLINE_H

/* Copyright 2001-2004,2006,2008-2013,2018
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

/* This file is for inline functions.  On platforms that don't support
   inlining functions, they are turned into ordinary functions.  On
   platforms that do support inline functions, the definitions are still
   compiled into the library, once, in inline.c.  */

#include "libguile/scm.h"

/* Define SCM_C_INLINE_KEYWORD so that it can be used as a replacement
   for the "inline" keyword, expanding to nothing when "inline" is not
   available.
*/

#ifdef SCM_C_INLINE
#define SCM_C_INLINE_KEYWORD SCM_C_INLINE
#else
#define SCM_C_INLINE_KEYWORD
#endif

/* We would like gnu89 extern inline semantics, not C99 extern inline
   semantics, so that we can be sure to avoid reifying definitions of
   inline functions in all compilation units, which is a possibility at
   low optimization levels, or if a user takes the address of an inline
   function.

   Hence the `__gnu_inline__' attribute, in accordance with:
   http://gcc.gnu.org/gcc-4.3/porting_to.html .

   With GCC 4.2, `__GNUC_STDC_INLINE__' is never defined (because C99 inline
   semantics are not supported), but a warning is issued in C99 mode if
   `__gnu_inline__' is not used.

   Apple's GCC build >5400 (since Xcode 3.0) doesn't support GNU inline in
   C99 mode and doesn't define `__GNUC_STDC_INLINE__'.  Fall back to "static
   inline" in that case.  */

# if (defined __GNUC__) && (!(((defined __APPLE_CC__) && (__APPLE_CC__ > 5400)) && __STDC_VERSION__ >= 199901L))
#  if (defined __GNUC_STDC_INLINE__) || (__GNUC__ == 4 && __GNUC_MINOR__ == 2)
#   define SCM_C_EXTERN_INLINE					\
           extern __inline__ __attribute__ ((__gnu_inline__))
#  else
#   define SCM_C_EXTERN_INLINE extern __inline__
#  endif
# endif

/* SCM_INLINE is a macro prepended to all public inline function
   declarations.  Implementations of those functions should also be in
   the header file, prefixed by SCM_INLINE_IMPLEMENTATION, and protected
   by SCM_CAN_INLINE.  Non-inline definitions will be reified into
   inline.c.  See strings.h for an example usage, for scm_is_string.  */

#if defined SCM_IMPLEMENT_INLINES
/* Reifying functions to a file, whether or not inlining is available.  */
# define SCM_CAN_INLINE 0
# define SCM_INLINE SCM_API
# define SCM_INLINE_IMPLEMENTATION
#elif defined SCM_C_INLINE
/* Declarations when inlining is available.  */
# define SCM_CAN_INLINE 1
# ifdef SCM_C_EXTERN_INLINE
#  define SCM_INLINE SCM_C_EXTERN_INLINE
# else
/* Fall back to static inline if GNU "extern inline" is unavailable.  */
#  define SCM_INLINE static SCM_C_INLINE
# endif
# define SCM_INLINE_IMPLEMENTATION SCM_INLINE
#else
/* Declarations when inlining is not available.  */
# define SCM_CAN_INLINE 0
# define SCM_INLINE SCM_API
/* Don't define SCM_INLINE_IMPLEMENTATION; it should never be seen in
   this case.  */
#endif

#endif /* SCM_INLINE_H */
