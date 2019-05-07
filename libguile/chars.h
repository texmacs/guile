#ifndef SCM_CHARS_H
#define SCM_CHARS_H

/* Copyright 1995-1996,2000-2001,2004,2006,2008-2009,2018-2019
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



#include "libguile/error.h"
#include "libguile/inline.h"


/* Immediate Characters
 */
#define SCM_CHARP(x) (SCM_ITAG8(x) == scm_tc8_char)
#define SCM_CHAR(x) ((scm_t_wchar)SCM_ITAG8_DATA(x))

/* SCM_MAKE_CHAR maps signed chars (-128 to 127) and unsigned chars
   (0 to 255) to Latin-1 codepoints (0 to 255) while allowing higher
   codepoints (256 to 1114111) to pass through unchanged.

   This macro evaluates X twice, which may lead to side effects if used
   incorrectly.  It's also likely to be inefficient if X calls a
   procedure.  Use 'scm_c_make_char' in those cases.  */
#define SCM_MAKE_CHAR(x)                                                \
  ((x) <= 1                                                             \
   ? SCM_MAKE_ITAG8 ((scm_t_bits) (unsigned char) (x), scm_tc8_char)    \
   : SCM_MAKE_ITAG8 ((scm_t_bits) (x), scm_tc8_char))

#define SCM_CODEPOINT_DOTTED_CIRCLE (0x25cc)
#define SCM_CODEPOINT_SURROGATE_START (0xd800)
#define SCM_CODEPOINT_SURROGATE_END (0xdfff)
#define SCM_CODEPOINT_MAX (0x10ffff)
#define SCM_IS_UNICODE_CHAR(c)                                          \
  (((scm_t_wchar) (c) >= 0                                              \
    && (scm_t_wchar) (c) < SCM_CODEPOINT_SURROGATE_START)               \
   || ((scm_t_wchar) (c) > SCM_CODEPOINT_SURROGATE_END                  \
       && (scm_t_wchar) (c) <= SCM_CODEPOINT_MAX))

#define SCM_VALIDATE_CHAR(pos, scm) SCM_MAKE_VALIDATE_MSG (pos, scm, CHARP, "character")

#define SCM_VALIDATE_CHAR_COPY(pos, scm, cvar) \
  do { \
    SCM_ASSERT (SCM_CHARP (scm), scm, pos, FUNC_NAME); \
    cvar = SCM_CHAR (scm); \
  } while (0)



SCM_API SCM scm_char_p (SCM x);
SCM_API SCM scm_char_eq_p (SCM x, SCM y);
SCM_API SCM scm_char_less_p (SCM x, SCM y);
SCM_API SCM scm_char_leq_p (SCM x, SCM y);
SCM_API SCM scm_char_gr_p (SCM x, SCM y);
SCM_API SCM scm_char_geq_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_eq_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_less_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_leq_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_gr_p (SCM x, SCM y);
SCM_API SCM scm_char_ci_geq_p (SCM x, SCM y);
SCM_API SCM scm_char_alphabetic_p (SCM chr);
SCM_API SCM scm_char_numeric_p (SCM chr);
SCM_API SCM scm_char_whitespace_p (SCM chr);
SCM_API SCM scm_char_upper_case_p (SCM chr);
SCM_API SCM scm_char_lower_case_p (SCM chr);
SCM_API SCM scm_char_is_both_p (SCM chr);
SCM_API SCM scm_char_to_integer (SCM chr);
SCM_API SCM scm_integer_to_char (SCM n);
SCM_API SCM scm_char_upcase (SCM chr);
SCM_API SCM scm_char_downcase (SCM chr);
SCM_API SCM scm_char_titlecase (SCM chr);
SCM_API SCM scm_char_general_category (SCM chr);

SCM_INLINE SCM scm_c_make_char (scm_t_wchar c);
SCM_API scm_t_wchar scm_c_upcase (scm_t_wchar c);
SCM_API scm_t_wchar scm_c_downcase (scm_t_wchar c);
SCM_API scm_t_wchar scm_c_titlecase (scm_t_wchar c);

SCM_INTERNAL const char *scm_i_charname (SCM chr);
SCM_INTERNAL SCM scm_i_charname_to_char (const char *charname, 
                                         size_t charname_len);
SCM_INTERNAL void scm_init_chars (void);

#if SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES
SCM_INLINE_IMPLEMENTATION SCM
scm_c_make_char (scm_t_wchar c)
{
  return SCM_MAKE_CHAR(c);
}
#endif

#endif  /* SCM_CHARS_H */
