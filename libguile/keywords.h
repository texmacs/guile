#ifndef SCM_KEYWORDS_H
#define SCM_KEYWORDS_H

/* Copyright 1995-1996,1999-2001,2006,2008,2015,2018-2019
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



#include <libguile/error.h>
#include <libguile/snarf.h>



SCM_API SCM scm_keyword_p (SCM obj);
SCM_API SCM scm_symbol_to_keyword (SCM symbol);
SCM_API SCM scm_keyword_to_symbol (SCM keyword);

SCM_API int scm_is_keyword (SCM val);
SCM_API SCM scm_from_locale_keyword (const char *name);
SCM_API SCM scm_from_locale_keywordn (const char *name, size_t len);
SCM_API SCM scm_from_latin1_keyword (const char *name);
SCM_API SCM scm_from_utf8_keyword (const char *name);

#define SCM_VALIDATE_KEYWORD(pos, v) \
  SCM_MAKE_VALIDATE_MSG (pos, v, KEYWORDP, "keyword")

#define SCM_KEYWORD(c_name, scheme_name) \
SCM_SNARF_HERE(static SCM c_name) \
SCM_SNARF_INIT(c_name = scm_from_utf8_keyword (scheme_name))

#define SCM_GLOBAL_KEYWORD(c_name, scheme_name) \
SCM_SNARF_HERE(SCM c_name) \
SCM_SNARF_INIT(c_name = scm_from_utf8_keyword (scheme_name))

enum scm_keyword_arguments_flags
{
  SCM_ALLOW_OTHER_KEYS            = (1U << 0),
  SCM_ALLOW_NON_KEYWORD_ARGUMENTS = (1U << 1)
};

typedef enum scm_keyword_arguments_flags scm_t_keyword_arguments_flags;

SCM_API void
scm_c_bind_keyword_arguments (const char *subr, SCM rest,
                              scm_t_keyword_arguments_flags flags, ...);

SCM_INTERNAL void scm_init_keywords (void);

#endif  /* SCM_KEYWORDS_H */
