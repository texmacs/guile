#ifndef SCM_SRFI_4_H
#define SCM_SRFI_4_H
/* srfi-4.c --- Homogeneous numeric vector datatypes.

   Copyright 2001,2004,2006,2008-2011,2014,2018
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


#include "libguile/array-handle.h"

SCM_API SCM scm_make_srfi_4_vector (SCM type, SCM len, SCM fill);


/* Specific procedures.
 */

SCM_API SCM scm_u8vector_p (SCM obj);
SCM_API SCM scm_make_u8vector (SCM n, SCM fill);
SCM_API SCM scm_take_u8vector (uint8_t *data, size_t n);
SCM_API SCM scm_u8vector (SCM l);
SCM_API SCM scm_u8vector_length (SCM uvec);
SCM_API SCM scm_u8vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u8vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u8vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u8vector (SCM l);
SCM_API SCM scm_any_to_u8vector (SCM obj);
SCM_API const uint8_t *scm_array_handle_u8_elements (scm_t_array_handle *h);
SCM_API uint8_t *scm_array_handle_u8_writable_elements (scm_t_array_handle *h);
SCM_API const uint8_t *scm_u8vector_elements (SCM uvec, 
						  scm_t_array_handle *h,
						  size_t *lenp, ssize_t *incp);
SCM_API uint8_t *scm_u8vector_writable_elements (SCM uvec, 
						     scm_t_array_handle *h,
						     size_t *lenp,
						     ssize_t *incp);

SCM_API SCM scm_s8vector_p (SCM obj);
SCM_API SCM scm_make_s8vector (SCM n, SCM fill);
SCM_API SCM scm_take_s8vector (int8_t *data, size_t n);
SCM_API SCM scm_s8vector (SCM l);
SCM_API SCM scm_s8vector_length (SCM uvec);
SCM_API SCM scm_s8vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s8vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s8vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s8vector (SCM l);
SCM_API SCM scm_any_to_s8vector (SCM obj);
SCM_API const int8_t *scm_array_handle_s8_elements (scm_t_array_handle *h);
SCM_API int8_t *scm_array_handle_s8_writable_elements (scm_t_array_handle *h);
SCM_API const int8_t *scm_s8vector_elements (SCM uvec, 
						 scm_t_array_handle *h,
						 size_t *lenp, ssize_t *incp);
SCM_API int8_t *scm_s8vector_writable_elements (SCM uvec, 
						    scm_t_array_handle *h,
						    size_t *lenp,
						    ssize_t *incp);

SCM_API SCM scm_u16vector_p (SCM obj);
SCM_API SCM scm_make_u16vector (SCM n, SCM fill);
SCM_API SCM scm_take_u16vector (uint16_t *data, size_t n);
SCM_API SCM scm_u16vector (SCM l);
SCM_API SCM scm_u16vector_length (SCM uvec);
SCM_API SCM scm_u16vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u16vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u16vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u16vector (SCM l);
SCM_API SCM scm_any_to_u16vector (SCM obj);
SCM_API const uint16_t *scm_array_handle_u16_elements (scm_t_array_handle *h);
SCM_API uint16_t *scm_array_handle_u16_writable_elements (scm_t_array_handle *h);
SCM_API const uint16_t *scm_u16vector_elements (SCM uvec, 
						    scm_t_array_handle *h,
						    size_t *lenp,
						    ssize_t *incp);
SCM_API uint16_t *scm_u16vector_writable_elements (SCM uvec, 
						       scm_t_array_handle *h,
						       size_t *lenp,
						       ssize_t *incp);

SCM_API SCM scm_s16vector_p (SCM obj);
SCM_API SCM scm_make_s16vector (SCM n, SCM fill);
SCM_API SCM scm_take_s16vector (int16_t *data, size_t n);
SCM_API SCM scm_s16vector (SCM l);
SCM_API SCM scm_s16vector_length (SCM uvec);
SCM_API SCM scm_s16vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s16vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s16vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s16vector (SCM l);
SCM_API SCM scm_any_to_s16vector (SCM obj);
SCM_API const int16_t *scm_array_handle_s16_elements (scm_t_array_handle *h);
SCM_API int16_t *scm_array_handle_s16_writable_elements (scm_t_array_handle *h);
SCM_API const int16_t *scm_s16vector_elements (SCM uvec, 
						   scm_t_array_handle *h,
						   size_t *lenp, ssize_t *incp);
SCM_API int16_t *scm_s16vector_writable_elements (SCM uvec, 
						      scm_t_array_handle *h,
						      size_t *lenp,
						      ssize_t *incp);

SCM_API SCM scm_u32vector_p (SCM obj);
SCM_API SCM scm_make_u32vector (SCM n, SCM fill);
SCM_API SCM scm_take_u32vector (uint32_t *data, size_t n);
SCM_API SCM scm_u32vector (SCM l);
SCM_API SCM scm_u32vector_length (SCM uvec);
SCM_API SCM scm_u32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u32vector (SCM l);
SCM_API SCM scm_any_to_u32vector (SCM obj);
SCM_API const uint32_t *scm_array_handle_u32_elements (scm_t_array_handle *h);
SCM_API uint32_t *scm_array_handle_u32_writable_elements (scm_t_array_handle *h);
SCM_API const uint32_t *scm_u32vector_elements (SCM uvec, 
						    scm_t_array_handle *h,
						    size_t *lenp,
						    ssize_t *incp);
SCM_API uint32_t *scm_u32vector_writable_elements (SCM uvec, 
						       scm_t_array_handle *h,
						       size_t *lenp,
						       ssize_t *incp);

SCM_API SCM scm_s32vector_p (SCM obj);
SCM_API SCM scm_make_s32vector (SCM n, SCM fill);
SCM_API SCM scm_take_s32vector (int32_t *data, size_t n);
SCM_API SCM scm_s32vector (SCM l);
SCM_API SCM scm_s32vector_length (SCM uvec);
SCM_API SCM scm_s32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s32vector (SCM l);
SCM_API SCM scm_any_to_s32vector (SCM obj);
SCM_API const int32_t *scm_array_handle_s32_elements (scm_t_array_handle *h);
SCM_API int32_t *scm_array_handle_s32_writable_elements (scm_t_array_handle *h);
SCM_API const int32_t *scm_s32vector_elements (SCM uvec, 
						   scm_t_array_handle *h,
						   size_t *lenp, ssize_t *incp);
SCM_API int32_t *scm_s32vector_writable_elements (SCM uvec, 
						      scm_t_array_handle *h,
						      size_t *lenp,
						      ssize_t *incp);

SCM_API SCM scm_u64vector_p (SCM obj);
SCM_API SCM scm_make_u64vector (SCM n, SCM fill);
SCM_API SCM scm_u64vector (SCM l);
SCM_API SCM scm_u64vector_length (SCM uvec);
SCM_API SCM scm_u64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_u64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_u64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_u64vector (SCM l);
SCM_API SCM scm_any_to_u64vector (SCM obj);

SCM_API SCM scm_take_u64vector (uint64_t *data, size_t n);
SCM_API const uint64_t *scm_array_handle_u64_elements (scm_t_array_handle *h);
SCM_API uint64_t *scm_array_handle_u64_writable_elements (scm_t_array_handle *h);
SCM_API const uint64_t *scm_u64vector_elements (SCM uvec, 
						    scm_t_array_handle *h,
						    size_t *lenp,
						    ssize_t *incp);
SCM_API uint64_t *scm_u64vector_writable_elements (SCM uvec, 
						       scm_t_array_handle *h,
						       size_t *lenp,
						       ssize_t *incp);

SCM_API SCM scm_s64vector_p (SCM obj);
SCM_API SCM scm_make_s64vector (SCM n, SCM fill);
SCM_API SCM scm_s64vector (SCM l);
SCM_API SCM scm_s64vector_length (SCM uvec);
SCM_API SCM scm_s64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_s64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_s64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_s64vector (SCM l);
SCM_API SCM scm_any_to_s64vector (SCM obj);

SCM_API SCM scm_take_s64vector (int64_t *data, size_t n);
SCM_API const int64_t *scm_array_handle_s64_elements (scm_t_array_handle *h);
SCM_API int64_t *scm_array_handle_s64_writable_elements (scm_t_array_handle *h);
SCM_API const int64_t *scm_s64vector_elements (SCM uvec, 
						   scm_t_array_handle *h,
						   size_t *lenp, ssize_t *incp);
SCM_API int64_t *scm_s64vector_writable_elements (SCM uvec, 
						      scm_t_array_handle *h,
						      size_t *lenp,
						      ssize_t *incp);

SCM_API SCM scm_f32vector_p (SCM obj);
SCM_API SCM scm_make_f32vector (SCM n, SCM fill);
SCM_API SCM scm_take_f32vector (float *data, size_t n);
SCM_API SCM scm_f32vector (SCM l);
SCM_API SCM scm_f32vector_length (SCM uvec);
SCM_API SCM scm_f32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_f32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_f32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_f32vector (SCM l);
SCM_API SCM scm_any_to_f32vector (SCM obj);
SCM_API const float *scm_array_handle_f32_elements (scm_t_array_handle *h);
SCM_API float *scm_array_handle_f32_writable_elements (scm_t_array_handle *h);
SCM_API const float *scm_f32vector_elements (SCM uvec, 
					    scm_t_array_handle *h,
					    size_t *lenp, ssize_t *incp);
SCM_API float *scm_f32vector_writable_elements (SCM uvec, 
						scm_t_array_handle *h,
						size_t *lenp,
						ssize_t *incp);

SCM_API SCM scm_f64vector_p (SCM obj);
SCM_API SCM scm_make_f64vector (SCM n, SCM fill);
SCM_API SCM scm_take_f64vector (double *data, size_t n);
SCM_API SCM scm_f64vector (SCM l);
SCM_API SCM scm_f64vector_length (SCM uvec);
SCM_API SCM scm_f64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_f64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_f64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_f64vector (SCM l);
SCM_API SCM scm_any_to_f64vector (SCM obj);
SCM_API const double *scm_array_handle_f64_elements (scm_t_array_handle *h);
SCM_API double *scm_array_handle_f64_writable_elements (scm_t_array_handle *h);
SCM_API const double *scm_f64vector_elements (SCM uvec, 
					      scm_t_array_handle *h,
					      size_t *lenp, ssize_t *incp);
SCM_API double *scm_f64vector_writable_elements (SCM uvec, 
						 scm_t_array_handle *h,
						 size_t *lenp,
						 ssize_t *incp);

SCM_API SCM scm_c32vector_p (SCM obj);
SCM_API SCM scm_make_c32vector (SCM n, SCM fill);
SCM_API SCM scm_take_c32vector (float *data, size_t n);
SCM_API SCM scm_c32vector (SCM l);
SCM_API SCM scm_c32vector_length (SCM uvec);
SCM_API SCM scm_c32vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_c32vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_c32vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_c32vector (SCM l);
SCM_API SCM scm_any_to_c32vector (SCM obj);
SCM_API const float *scm_array_handle_c32_elements (scm_t_array_handle *h);
SCM_API float *scm_array_handle_c32_writable_elements (scm_t_array_handle *h);
SCM_API const float *scm_c32vector_elements (SCM uvec, 
					     scm_t_array_handle *h,
					     size_t *lenp, ssize_t *incp);
SCM_API float *scm_c32vector_writable_elements (SCM uvec, 
						scm_t_array_handle *h,
						size_t *lenp,
						ssize_t *incp);

SCM_API SCM scm_c64vector_p (SCM obj);
SCM_API SCM scm_make_c64vector (SCM n, SCM fill);
SCM_API SCM scm_take_c64vector (double *data, size_t n);
SCM_API SCM scm_c64vector (SCM l);
SCM_API SCM scm_c64vector_length (SCM uvec);
SCM_API SCM scm_c64vector_ref (SCM uvec, SCM index);
SCM_API SCM scm_c64vector_set_x (SCM uvec, SCM index, SCM value);
SCM_API SCM scm_c64vector_to_list (SCM uvec);
SCM_API SCM scm_list_to_c64vector (SCM l);
SCM_API SCM scm_any_to_c64vector (SCM obj);
SCM_API const double *scm_array_handle_c64_elements (scm_t_array_handle *h);
SCM_API double *scm_array_handle_c64_writable_elements (scm_t_array_handle *h);
SCM_API const double *scm_c64vector_elements (SCM uvec, 
					      scm_t_array_handle *h,
					      size_t *lenp, ssize_t *incp);
SCM_API double *scm_c64vector_writable_elements (SCM uvec, 
						 scm_t_array_handle *h,
						 size_t *lenp,
						 ssize_t *incp);

SCM_INTERNAL void scm_init_srfi_4 (void);

#endif /* SCM_SRFI_4_H */
