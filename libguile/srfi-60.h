/* srfi-60.h --- SRFI-60 procedures for Guile

   Copyright 2005-2006,2010,2018
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


#ifndef SCM_SRFI_60_H
#define SCM_SRFI_60_H

#include "libguile/scm.h"

SCM_INTERNAL SCM scm_srfi60_log2_binary_factors (SCM n);
SCM_INTERNAL SCM scm_srfi60_copy_bit (SCM index, SCM n, SCM bit);
SCM_INTERNAL SCM scm_srfi60_rotate_bit_field (SCM n, SCM count, SCM start, SCM end);
SCM_INTERNAL SCM scm_srfi60_reverse_bit_field (SCM n, SCM start, SCM end);
SCM_INTERNAL SCM scm_srfi60_integer_to_list (SCM n, SCM len);
SCM_INTERNAL SCM scm_srfi60_list_to_integer (SCM lst);

SCM_INTERNAL void scm_register_srfi_60 (void);
SCM_INTERNAL void scm_init_srfi_60 (void);

#endif /* SCM_SRFI_60_H */
