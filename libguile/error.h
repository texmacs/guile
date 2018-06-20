#ifndef SCM_ERROR_H
#define SCM_ERROR_H

/* Copyright 1995-1998,2000-2002,2006,2008,2011,2014,2018
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



#include "libguile/scm.h"


SCM_API SCM scm_system_error_key;
SCM_API SCM scm_num_overflow_key;
SCM_API SCM scm_out_of_range_key;
SCM_API SCM scm_args_number_key;
SCM_API SCM scm_arg_type_key;
SCM_API SCM scm_misc_error_key;



/* Snarfing for docs may override SCM_ASSERT; see snarf.h.  */
#ifndef SCM_ASSERT
#define SCM_ASSERT(_cond, _arg, _pos, _subr)			\
  do { if (SCM_UNLIKELY (!(_cond)))                             \
      scm_wrong_type_arg (_subr, _pos, _arg); } while (0)
#endif
#define SCM_ASSERT_TYPE(_cond, _arg, _pos, _subr, _msg)			\
  do { if (SCM_UNLIKELY (!(_cond)))                                     \
      scm_wrong_type_arg_msg(_subr, _pos, _arg, _msg);  } while (0)




SCM_API void scm_error (SCM key, const char *subr, const char *message,
			SCM args, SCM rest) SCM_NORETURN;
SCM_API SCM scm_error_scm (SCM key, SCM subr, SCM message,
			   SCM args, SCM rest) SCM_NORETURN;
SCM_API SCM scm_strerror (SCM err);
SCM_API void scm_syserror (const char *subr) SCM_NORETURN;
SCM_API void scm_syserror_msg (const char *subr, const char *message,
			       SCM args, int eno) SCM_NORETURN;
SCM_API void scm_num_overflow (const char *subr) SCM_NORETURN;
SCM_API void scm_out_of_range (const char *subr, SCM bad_value)
     SCM_NORETURN;
SCM_API void scm_out_of_range_pos (const char *subr, SCM bad_value, SCM pos)
     SCM_NORETURN;
SCM_API void scm_wrong_num_args (SCM proc) SCM_NORETURN;
SCM_API void scm_error_num_args_subr (const char* subr) SCM_NORETURN;
SCM_API void scm_wrong_type_arg (const char *subr, int pos,
				 SCM bad_value) SCM_NORETURN;
SCM_INTERNAL void scm_i_wrong_type_arg_symbol (SCM symbol, int pos,
					       SCM bad_value) SCM_NORETURN;
SCM_API void scm_wrong_type_arg_msg (const char *subr, int pos,
				     SCM bad_value, const char *sz) SCM_NORETURN;
SCM_API void scm_misc_error (const char *subr, const char *message,
			     SCM args) SCM_NORETURN;
SCM_INTERNAL void scm_init_error (void);



#ifndef SCM_MAGIC_SNARFER
/* Let these macros pass through if
   we are snarfing;  thus we can tell the
   difference between the use of an actual
   number vs. the use of one of these macros --
   actual numbers in SCM_VALIDATE_* and SCM_ASSERT
   constructs must match the formal argument name,
   but using SCM_ARG* avoids the test */

#define SCM_ARGn 		0
#define SCM_ARG1 		1
#define SCM_ARG2 		2
#define SCM_ARG3 		3
#define SCM_ARG4 		4
#define SCM_ARG5 		5
#define SCM_ARG6 		6
#define SCM_ARG7 		7

#endif /* SCM_MAGIC_SNARFER */



#define SCM_MAKE_VALIDATE(pos, var, pred) \
  do { \
    SCM_ASSERT_TYPE (SCM_ ## pred (var), var, pos, FUNC_NAME, #pred); \
  } while (0)

#define SCM_I_MAKE_VALIDATE_MSG2(pos, var, pred, msg) \
  do { \
    SCM_ASSERT_TYPE (pred (var), var, pos, FUNC_NAME, msg); \
  } while (0)

#define SCM_MAKE_VALIDATE_MSG(pos, var, pred, msg) \
  SCM_I_MAKE_VALIDATE_MSG2 (pos, var, SCM_ ## pred, msg)

#define SCM_SYSERROR do { scm_syserror (FUNC_NAME); } while (0)

#define SCM_MEMORY_ERROR do { scm_memory_error (FUNC_NAME); } while (0)

#define SCM_SYSERROR_MSG(str, args, val) \
  do { scm_syserror_msg (FUNC_NAME, (str), (args), (val)); } while (0)

#define SCM_MISC_ERROR(str, args) \
  do { scm_misc_error (FUNC_NAME, str, args); } while (0)

#define SCM_WRONG_NUM_ARGS() \
  do { scm_error_num_args_subr (FUNC_NAME); } while (0)

#define SCM_WRONG_TYPE_ARG(pos, obj) \
  do { scm_wrong_type_arg (FUNC_NAME, pos, obj); } while (0)


#endif  /* SCM_ERROR_H */
