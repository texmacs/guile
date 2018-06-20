/* classes: h_files */

#ifndef SCM_SNARF_H
#define SCM_SNARF_H

/* Copyright (C) 1995-2004,2006,2009-2011,2013-2014,2018
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




#include <libguile/gc.h>


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

#define SCM_SMOB(tag, scheme_name, size) \
SCM_SNARF_HERE(static scm_t_bits tag) \
SCM_SNARF_INIT((tag)=scm_make_smob_type((scheme_name), (size));)

#define SCM_GLOBAL_SMOB(tag, scheme_name, size) \
SCM_SNARF_HERE(scm_t_bits tag) \
SCM_SNARF_INIT((tag)=scm_make_smob_type((scheme_name), (size));)

#define SCM_SMOB_MARK(tag, c_name, arg) \
SCM_SNARF_HERE(static SCM c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_mark((tag), (c_name));)

#define SCM_GLOBAL_SMOB_MARK(tag, c_name, arg) \
SCM_SNARF_HERE(SCM c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_mark((tag), (c_name));)

#define SCM_SMOB_FREE(tag, c_name, arg) \
SCM_SNARF_HERE(static size_t c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_free((tag), (c_name));)

#define SCM_GLOBAL_SMOB_FREE(tag, c_name, arg) \
SCM_SNARF_HERE(size_t c_name(SCM arg)) \
SCM_SNARF_INIT(scm_set_smob_free((tag), (c_name));)

#define SCM_SMOB_PRINT(tag, c_name, obj, port, pstate) \
SCM_SNARF_HERE(static int c_name(SCM obj, SCM port, scm_print_state* pstate)) \
SCM_SNARF_INIT(scm_set_smob_print((tag), (c_name));)

#define SCM_GLOBAL_SMOB_PRINT(tag, c_name, obj, port, pstate) \
SCM_SNARF_HERE(int c_name(SCM obj, SCM port, scm_print_state* pstate)) \
SCM_SNARF_INIT(scm_set_smob_print((tag), (c_name));)

#define SCM_SMOB_EQUALP(tag, c_name, obj1, obj2) \
SCM_SNARF_HERE(static SCM c_name(SCM obj1, SCM obj2)) \
SCM_SNARF_INIT(scm_set_smob_equalp((tag), (c_name));)

#define SCM_GLOBAL_SMOB_EQUALP(tag, c_name, obj1, obj2) \
SCM_SNARF_HERE(SCM c_name(SCM obj1, SCM obj2)) \
SCM_SNARF_INIT(scm_set_smob_equalp((tag), (c_name));)

#define SCM_SMOB_APPLY(tag, c_name, req, opt, rest, arglist) \
SCM_SNARF_HERE(static SCM c_name arglist) \
SCM_SNARF_INIT(scm_set_smob_apply((tag), (c_name), (req), (opt), (rest));)

#define SCM_GLOBAL_SMOB_APPLY(tag, c_name, req, opt, rest, arglist) \
SCM_SNARF_HERE(SCM c_name arglist) \
SCM_SNARF_INIT(scm_set_smob_apply((tag), (c_name), (req), (opt), (rest));)


/* Low-level snarfing for static memory allocation.  */

#ifdef SCM_SUPPORT_STATIC_ALLOCATION

#define SCM_IMMUTABLE_CELL(c_name, car, cdr)		\
  static SCM_ALIGNED (8) SCM_UNUSED const scm_t_cell			\
       c_name ## _raw_scell =						\
  {                                                                     \
    SCM_PACK (car),                                                     \
    SCM_PACK (cdr)                                                      \
  };                                                                    \
  static SCM_UNUSED const SCM c_name = SCM_PACK (& c_name ## _raw_scell)

#define SCM_IMMUTABLE_DOUBLE_CELL(c_name, car, cbr, ccr, cdr)		\
  static SCM_ALIGNED (8) SCM_UNUSED const scm_t_cell			\
  c_name ## _raw_cell [2] =						\
    {									\
      { SCM_PACK (car), SCM_PACK (cbr) },				\
      { SCM_PACK (ccr), SCM_PACK (cdr) }				\
    };									\
  static SCM_UNUSED const SCM c_name = SCM_PACK (& c_name ## _raw_cell)
#endif /* SCM_SUPPORT_STATIC_ALLOCATION */



/* Documentation.  */

#ifdef SCM_MAGIC_SNARF_DOCS
#undef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr) ^^ argpos _arg _pos __LINE__ ^^
#endif /* SCM_MAGIC_SNARF_DOCS */

#endif  /* SCM_SNARF_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
