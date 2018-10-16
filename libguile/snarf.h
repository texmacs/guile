#ifndef SCM_SNARF_H
#define SCM_SNARF_H

/* Copyright 1995-2004,2006,2009-2011,2013-2014,2017-2019
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




#include <libguile/scm.h>


/* Macros for snarfing initialization actions from C source. */

#ifdef SCM_ALIGNED
/* We support static allocation of some `SCM' objects.  */
# define SCM_SUPPORT_STATIC_ALLOCATION
#endif

/* C preprocessor token concatenation.  */
#define scm_i_paste(x, y)      x ## y
#define scm_i_paste3(a, b, c)  a ## b ## c



/* Generic macros to be used in user macro definitions.
 *
 * For example, in order to define a macro which creates ints and
 * initializes them to the result of foo (), do:
 *
 *   #define SCM_FOO(NAME) \
 *     SCM_SNARF_HERE (int NAME) \
 *     SCM_SNARF_INIT (NAME = foo ())
 *
 * The SCM_SNARF_INIT text goes into the corresponding .x file
 * up through the first occurrence of SCM_SNARF_DOC_START on that
 * line, if any.
 *
 * Some debugging options can cause the preprocessor to echo #define
 * directives to its output. Keeping the snarfing markers on separate
 * lines prevents guile-snarf from inadvertently snarfing the definition
 * of SCM_SNARF_INIT if those options are in effect.
 */

#ifdef SCM_MAGIC_SNARF_INITS
# define SCM_SNARF_HERE(X)
# define SCM_SNARF_INIT_PREFIX ^^
# define SCM_SNARF_INIT(X) SCM_SNARF_INIT_PREFIX X ^:^
# define SCM_SNARF_DOCS(TYPE, CNAME, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)
#else
# ifdef SCM_MAGIC_SNARF_DOCS
#  define SCM_SNARF_HERE(X)
#  define SCM_SNARF_INIT(X)
#  define SCM_SNARF_DOCS(TYPE, CNAME, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING) \
^^ { \
cname CNAME ^^ \
fname FNAME ^^ \
type TYPE ^^ \
location __FILE__ __LINE__ ^^ \
arglist ARGLIST ^^ \
argsig REQ OPT VAR ^^ \
DOCSTRING ^^ }
# else
#  define SCM_SNARF_HERE(X) X
#  define SCM_SNARF_INIT(X)
#  define SCM_SNARF_DOCS(TYPE, CNAME, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)
# endif
#endif


/* Low-level snarfing for static memory allocation.  */

#ifdef SCM_SUPPORT_STATIC_ALLOCATION

#define SCM_IMMUTABLE_CELL(c_name, car, cdr)		\
  static SCM_ALIGNED (8) const SCM c_name ## _raw [2] =			\
    { SCM_PACK (car), SCM_PACK (cdr) };					\
  static SCM_UNUSED const SCM c_name = SCM_PACK (& c_name ## _raw)

#define SCM_IMMUTABLE_DOUBLE_CELL(c_name, car, cbr, ccr, cdr)		\
  static SCM_ALIGNED (8) const SCM c_name ## _raw [4] =			\
    { SCM_PACK (car), SCM_PACK (cbr), SCM_PACK (ccr), SCM_PACK (cdr) };	\
  static SCM_UNUSED const SCM c_name = SCM_PACK (& c_name ## _raw)
#endif /* SCM_SUPPORT_STATIC_ALLOCATION */



/* Documentation.  */

#ifdef SCM_MAGIC_SNARF_DOCS
#undef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr) ^^ argpos _arg _pos __LINE__ ^^
#endif /* SCM_MAGIC_SNARF_DOCS */

#endif  /* SCM_SNARF_H */
