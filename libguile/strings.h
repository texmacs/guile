#ifndef SCM_STRINGS_H
#define SCM_STRINGS_H

/* Copyright 1995-1998,2000-2001,2004-2006,2008-2011,2013,2015-2019
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



#include <libguile/gc.h>
#include <libguile/error.h>
#include "libguile/inline.h"
#include <libguile/snarf.h>



/* String representation.

   A string is a piece of a stringbuf.  A stringbuf can be used by
   more than one string.  When a string is written to and the
   stringbuf of that string is used by more than one string, a new
   stringbuf is created.  That is, strings are copy-on-write.  This
   behavior can be used to make the substring operation quite
   efficient.

   The implementation is tuned so that mutating a string is costly,
   but just reading it is cheap and lock-free.

   There are also mutation-sharing strings.  They refer to a part of
   an ordinary string.  Writing to a mutation-sharing string just
   writes to the ordinary string.


   Internal, low level interface to the character arrays

   - Use scm_i_is_narrow_string to determine is the string is narrow or
     wide.

   - Use scm_i_string_chars or scm_i_string_wide_chars to get a
     pointer to the byte or scm_t_wchar array of a string for reading.
     Use scm_i_string_length to get the number of characters in that
     array.  The array is not null-terminated.

   - The array is valid as long as the corresponding SCM object is
     protected but only until the next SCM_TICK.  During such a 'safe
     point', strings might change their representation.

   - Use scm_i_string_start_writing to get a version of the string
     ready for reading and writing.  This is a potentially costly
     operation since it implements the copy-on-write behavior.  When
     done with the writing, call scm_i_string_stop_writing.  You must
     do this before the next SCM_TICK.  (This means, before calling
     almost any other scm_ function and you can't allow throws, of
     course.)

   - New strings can be created with scm_i_make_string or
     scm_i_make_wide_string.  This gives access to a writable pointer
     that remains valid as long as nobody else makes a copy-on-write
     substring of the string.  Do not call scm_i_string_stop_writing
     for this pointer.

   - Alternately, scm_i_string_ref and scm_i_string_set_x can be used
     to read and write strings without worrying about whether the
     string is narrow or wide.  scm_i_string_set_x still needs to be
     bracketed by scm_i_string_start_writing and
     scm_i_string_stop_writing.

   Legacy interface

   - SCM_STRINGP is just scm_is_string.

   - SCM_STRING_CHARS uses scm_i_string_writable_chars and immediately
     calls scm_i_stop_writing, hoping for the best.  SCM_STRING_LENGTH
     is the same as scm_i_string_length.  SCM_STRING_CHARS will throw
     an error for strings that are not null-terminated.  There is
     no wide version of this interface.
*/

/* A type indicating what strategy to take when string locale
   conversion is unsuccessful.  */
typedef enum
{
  SCM_FAILED_CONVERSION_ERROR = SCM_ICONVEH_ERROR,
  SCM_FAILED_CONVERSION_QUESTION_MARK = SCM_ICONVEH_QUESTION_MARK,
  SCM_FAILED_CONVERSION_ESCAPE_SEQUENCE = SCM_ICONVEH_ESCAPE_SEQUENCE
} scm_t_string_failed_conversion_handler;

SCM_INTERNAL SCM scm_nullstr;

SCM_INTERNAL scm_t_string_failed_conversion_handler
scm_i_default_string_failed_conversion_handler (void);

SCM_INLINE int scm_is_string (SCM x);
SCM_API SCM scm_string_p (SCM x);
SCM_API SCM scm_string (SCM chrs);
SCM_API SCM scm_make_string (SCM k, SCM chr);
SCM_API SCM scm_string_length (SCM str);
SCM_API SCM scm_string_utf8_length (SCM str);
SCM_API SCM scm_string_bytes_per_char (SCM str);
SCM_API SCM scm_string_ref (SCM str, SCM k);
SCM_API SCM scm_string_set_x (SCM str, SCM k, SCM chr);
SCM_API SCM scm_substring (SCM str, SCM start, SCM end);
SCM_API SCM scm_substring_read_only (SCM str, SCM start, SCM end);
SCM_API SCM scm_substring_shared (SCM str, SCM start, SCM end);
SCM_API SCM scm_substring_copy (SCM str, SCM start, SCM end);
SCM_API SCM scm_string_append (SCM args);

SCM_API SCM scm_from_stringn (const char *str, size_t len, const char *encoding,
                              scm_t_string_failed_conversion_handler handler);
SCM_API SCM scm_c_make_string (size_t len, SCM chr);
SCM_API size_t scm_c_string_length (SCM str);
SCM_API size_t scm_c_string_utf8_length (SCM str);
SCM_API size_t scm_c_symbol_length (SCM sym);
SCM_API SCM scm_c_string_ref (SCM str, size_t pos);
SCM_API void scm_c_string_set_x (SCM str, size_t pos, SCM chr);
SCM_API SCM scm_c_substring (SCM str, size_t start, size_t end);
SCM_API SCM scm_c_substring_read_only (SCM str, size_t start, size_t end);
SCM_API SCM scm_c_substring_shared (SCM str, size_t start, size_t end);
SCM_API SCM scm_c_substring_copy (SCM str, size_t start, size_t end);

/* Use locale encoding for user input, user output, or interacting with
   the C library.  Use latin1 for ASCII, and for literals in source
   code.  Use utf8 for interaction with modern libraries which deal in
   UTF-8.  Otherwise use scm_to_stringn or scm_from_stringn with a
   specific encoding. */

SCM_API SCM scm_from_locale_string (const char *str);
SCM_API SCM scm_from_locale_stringn (const char *str, size_t len);
SCM_API SCM scm_take_locale_string (char *str);
SCM_API SCM scm_take_locale_stringn (char *str, size_t len);
SCM_API char *scm_to_locale_string (SCM str);
SCM_API char *scm_to_locale_stringn (SCM str, size_t *lenp);

SCM_API SCM scm_from_latin1_string (const char *str);
SCM_API SCM scm_from_latin1_stringn (const char *str, size_t len);
SCM_API char *scm_to_latin1_string (SCM str);
SCM_API char *scm_to_latin1_stringn (SCM str, size_t *lenp);

SCM_API char *scm_to_utf8_string (SCM str);
SCM_API char *scm_to_utf8_stringn (SCM str, size_t *lenp);
SCM_API SCM scm_from_utf8_string (const char *str);
SCM_API SCM scm_from_utf8_stringn (const char *str, size_t len);

SCM_API scm_t_wchar *scm_to_utf32_string (SCM str);
SCM_API scm_t_wchar *scm_to_utf32_stringn (SCM str, size_t *lenp);
SCM_API SCM scm_from_utf32_string (const scm_t_wchar *str);
SCM_API SCM scm_from_utf32_stringn (const scm_t_wchar *str, size_t len);

SCM_API char *scm_to_port_string (SCM str, SCM port);
SCM_API char *scm_to_port_stringn (SCM str, size_t *lenp, SCM port);
SCM_API SCM scm_from_port_string (const char *str, SCM port);
SCM_API SCM scm_from_port_stringn (const char *str, size_t len, SCM port);

SCM_API char *scm_to_stringn (SCM str, size_t *lenp, const char *encoding,
                              scm_t_string_failed_conversion_handler handler);
SCM_API size_t scm_to_locale_stringbuf (SCM str, char *buf, size_t max_len);

SCM_API SCM scm_string_normalize_nfd (SCM str);
SCM_API SCM scm_string_normalize_nfkd (SCM str);
SCM_API SCM scm_string_normalize_nfc (SCM str);
SCM_API SCM scm_string_normalize_nfkc (SCM str);

SCM_API SCM scm_makfromstrs (int argc, char **argv);




/* Snarfing support.  See snarf.h.  */

#ifdef SCM_SUPPORT_STATIC_ALLOCATION
#define SCM_IMMUTABLE_STRINGBUF(c_name, contents)	\
  static SCM_UNUSED const				\
  struct						\
  {							\
    scm_t_bits word_0;					\
    scm_t_bits word_1;					\
    const char buffer[sizeof (contents)];		\
  }							\
  c_name =						\
    {							\
      scm_tc7_stringbuf,                                \
      sizeof (contents) - 1,				\
      contents						\
    }

#define SCM_IMMUTABLE_STRING(c_name, contents)				\
  SCM_IMMUTABLE_STRINGBUF (scm_i_paste (c_name, _stringbuf), contents);	\
  SCM_IMMUTABLE_DOUBLE_CELL (c_name,					\
			     scm_tc7_ro_string,				\
			     (scm_t_bits) &scm_i_paste (c_name,		\
							_stringbuf),	\
			     (scm_t_bits) 0,				\
                             (scm_t_bits) (sizeof (contents) - 1))

#endif /* SCM_SUPPORT_STATIC_ALLOCATION */



/* internal constants */

/* Type tag for read-only strings.  */
#define scm_tc7_ro_string             (scm_tc7_string + 0x200)

/* Flags for shared and wide strings.  */
#define SCM_I_STRINGBUF_F_WIDE        0x400
#define SCM_I_STRINGBUF_F_MUTABLE     0x800

SCM_INTERNAL void scm_i_print_stringbuf (SCM exp, SCM port,
                                         scm_print_state *pstate);

/* internal accessor functions.  Arguments must be valid. */

SCM_INTERNAL SCM scm_i_make_string (size_t len, char **datap,
				    int read_only_p);
SCM_INTERNAL SCM scm_i_make_wide_string (size_t len, scm_t_wchar **datap,
					 int read_only_p);
SCM_INTERNAL SCM scm_i_substring (SCM str, size_t start, size_t end);
SCM_INTERNAL SCM scm_i_substring_read_only (SCM str, size_t start, size_t end);
SCM_INTERNAL SCM scm_i_substring_shared (SCM str, size_t start, size_t end);
SCM_INTERNAL SCM scm_i_substring_copy (SCM str, size_t start, size_t end);
SCM_INTERNAL size_t scm_i_string_length (SCM str);
SCM_INTERNAL int scm_i_string_is_mutable (SCM str);
SCM_API /* FIXME: not internal */ const char *scm_i_string_chars (SCM str);
SCM_API /* FIXME: not internal */ char *scm_i_string_writable_chars (SCM str);
SCM_INTERNAL const scm_t_wchar *scm_i_string_wide_chars (SCM str);
SCM_INTERNAL const void *scm_i_string_data (SCM str);

SCM_INTERNAL SCM scm_i_string_start_writing (SCM str);
SCM_INTERNAL void scm_i_string_stop_writing (void);
SCM_INTERNAL int scm_i_is_narrow_string (SCM str);
SCM_INTERNAL scm_t_wchar scm_i_string_ref (SCM str, size_t x);
SCM_INTERNAL int scm_i_string_contains_char (SCM str, char c);
SCM_INTERNAL int scm_i_string_strcmp (SCM sstr, size_t start_x, const char *cstr);
SCM_INTERNAL void scm_i_string_set_x (SCM str, size_t p, scm_t_wchar chr);
/* internal functions related to symbols. */

SCM_INTERNAL SCM scm_i_make_symbol (SCM name, scm_t_bits flags,
				    unsigned long hash, SCM props);
SCM_INTERNAL SCM
scm_i_c_make_symbol (const char *name, size_t len,
		     scm_t_bits flags, unsigned long hash, SCM props);
SCM_INTERNAL const char *scm_i_symbol_chars (SCM sym);
SCM_INTERNAL const scm_t_wchar *scm_i_symbol_wide_chars (SCM sym);
SCM_INTERNAL size_t scm_i_symbol_length (SCM sym);
SCM_INTERNAL int scm_i_is_narrow_symbol (SCM str);
SCM_INTERNAL int scm_i_try_narrow_string (SCM str);
SCM_INTERNAL SCM scm_i_symbol_substring (SCM sym, size_t start, size_t end);
SCM_INTERNAL scm_t_wchar scm_i_symbol_ref (SCM sym, size_t x);
SCM_INTERNAL void scm_encoding_error (const char *subr, int err,
				      const char *message, SCM port, SCM chr);
SCM_INTERNAL void scm_decoding_error (const char *subr, int err,
				      const char *message, SCM port);

/* internal utility functions. */

SCM_INTERNAL char **scm_i_allocate_string_pointers (SCM list);
SCM_INTERNAL void scm_i_get_substring_spec (size_t len,
					    SCM start, size_t *cstart,
					    SCM end, size_t *cend);

/* Debugging functions */

SCM_API SCM scm_sys_string_dump (SCM);
SCM_API SCM scm_sys_symbol_dump (SCM);
#ifdef SCM_STRING_LENGTH_HISTOGRAM
SCM_API SCM scm_sys_stringbuf_hist (void);
#endif




#if SCM_CAN_INLINE || defined SCM_INLINE_C_IMPLEMENTING_INLINES
/* Either inlining, or being included from inline.c.  */

SCM_INLINE_IMPLEMENTATION int
scm_is_string (SCM x)
{
  return SCM_HAS_TYP7 (x, scm_tc7_string);
}

#endif




#define SCM_VALIDATE_STRING(pos, str) \
  do { \
    SCM_ASSERT_TYPE (scm_is_string (str), str, pos, FUNC_NAME, "string"); \
  } while (0)




SCM_INTERNAL void scm_init_strings (void);

#endif  /* SCM_STRINGS_H */
