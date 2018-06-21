#ifndef SCM_STRUCT_H
#define SCM_STRUCT_H

/* Copyright 1995,1997,1999-2001,2006-2013,2015,2017-2018
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



#include "libguile/boolean.h"
#include <libguile/error.h>
#include <libguile/gc.h>
#include "libguile/print.h"



/* Structs are sequences of words where the first word points to the
   struct's vtable, and the rest are its slots.  The vtable indicates
   how many words are in the struct among other meta-information.  A
   vtable is itself a struct and as such has a vtable, and so on until
   you get to a root struct that is its own vtable.

     .--------+----------------- -
     | vtable | slot0 | slot1 |
     `--------+----------------- -
         |
         |
     .---v----+----------------- -
     | vtable | slot0 | slot1 |
     `--------+----------------- -
         |
        ...
         |
     .---v----+----------------- -
   .-| vtable | slot0 | slot1 |
   | `--------+----------------- -
   |     ^
   `-----'
 */

/* All vtables have the following fields. */
#define SCM_VTABLE_BASE_LAYOUT                                          \
  "pw" /* layout */                                                     \
  "uh" /* flags */							\
  "uh" /* finalizer */                                                  \
  "pw" /* printer */                                                    \
  "ph" /* name (hidden from make-struct for back-compat reasons) */     \
  "uh" /* size */							\
  "uh" /* unboxed fields */						\
  "uh" /* reserved */

#define scm_vtable_index_layout            0 /* A symbol describing the physical arrangement of this type. */
#define scm_vtable_index_flags	           1 /* Class flags */
#define scm_vtable_index_instance_finalize 2 /* Finalizer for instances of this struct type. */
#define scm_vtable_index_instance_printer  3 /* A printer for this struct type. */
#define scm_vtable_index_name              4 /* Name of this vtable. */
#define scm_vtable_index_size              5 /* Number of fields, for simple structs.  */
#define scm_vtable_index_unboxed_fields    6 /* Raw uint32_t* bitmask indicating unboxed fields.  */
#define scm_vtable_index_reserved_7        7
#define scm_vtable_offset_user             8 /* Where do user fields start in the vtable? */

/* All applicable structs have the following fields. */
#define SCM_APPLICABLE_BASE_LAYOUT              \
  "pw" /* procedure */
#define SCM_APPLICABLE_WITH_SETTER_BASE_LAYOUT  \
  "pw" /* procedure */                          \
  "pw" /* setter */
#define scm_applicable_struct_index_procedure 0 /* The procedure of an applicable
                                                   struct. Only valid if the
                                                   struct's vtable has the
                                                   applicable flag set. */
#define scm_applicable_struct_index_setter    1 /* The setter of an applicable
                                                   struct. Only valid if the
                                                   struct's vtable has the
                                                   setter flag set. */

#define SCM_VTABLE_FLAG_VALIDATED (1L << 0) /* the layout of this vtable been validated? */
#define SCM_VTABLE_FLAG_VTABLE (1L << 1) /* instances of this vtable are themselves vtables? */
#define SCM_VTABLE_FLAG_APPLICABLE_VTABLE (1L << 2) /* instances of this vtable are applicable vtables? */
#define SCM_VTABLE_FLAG_APPLICABLE (1L << 3) /* instances of this vtable are applicable? */
#define SCM_VTABLE_FLAG_SETTER_VTABLE (1L << 4) /* instances of this vtable are applicable-with-setter vtables? */
#define SCM_VTABLE_FLAG_SETTER (1L << 5) /* instances of this vtable are applicable-with-setters? */
#define SCM_VTABLE_FLAG_RESERVED_0 (1L << 6)
#define SCM_VTABLE_FLAG_RESERVED_1 (1L << 7)
#define SCM_VTABLE_FLAG_SMOB_0 (1L << 8)
#define SCM_VTABLE_FLAG_GOOPS_0 (1L << 9)
#define SCM_VTABLE_FLAG_GOOPS_1 (1L << 10)
#define SCM_VTABLE_FLAG_GOOPS_2 (1L << 11)
#define SCM_VTABLE_FLAG_GOOPS_3 (1L << 12)
#define SCM_VTABLE_FLAG_GOOPS_4 (1L << 13)
#define SCM_VTABLE_FLAG_RESERVED_2 (1L << 14)
#define SCM_VTABLE_FLAG_RESERVED_3 (1L << 15)
#define SCM_VTABLE_USER_FLAG_SHIFT 16

typedef void (*scm_t_struct_finalize) (SCM obj);

#define SCM_STRUCTP(X)  		(!SCM_IMP(X) && (SCM_TYP3(X) == scm_tc3_struct))
#define SCM_STRUCT_SLOTS(X) 		(SCM_CELL_OBJECT_LOC(X, 1))
#define SCM_STRUCT_SLOT_REF(X,I) 	(SCM_STRUCT_SLOTS (X)[(I)])
#define SCM_STRUCT_SLOT_SET(X,I,V) 	SCM_STRUCT_SLOTS (X)[(I)]=(V)
#define SCM_STRUCT_DATA(X) 		((scm_t_bits*)SCM_STRUCT_SLOTS (X))
#define SCM_STRUCT_DATA_REF(X,I) 	(SCM_STRUCT_DATA (X)[(I)])
#define SCM_STRUCT_DATA_SET(X,I,V) 	SCM_STRUCT_DATA (X)[(I)]=(V)

#define SCM_VALIDATE_STRUCT(pos, v) \
  SCM_MAKE_VALIDATE_MSG (pos, v, STRUCTP, "struct")
#define SCM_VALIDATE_VTABLE(pos, v) \
  do { \
    SCM_ASSERT (scm_is_true (scm_struct_vtable_p (v)), v, pos, FUNC_NAME); \
  } while (0)

/* The SCM_VTABLE_* macros assume that you're passing them a struct which is a
   valid vtable. */
#define SCM_VTABLE_LAYOUT(X)            (SCM_STRUCT_SLOT_REF ((X), scm_vtable_index_layout))
#define SCM_SET_VTABLE_LAYOUT(X,L)      (SCM_STRUCT_SLOT_SET ((X), scm_vtable_index_layout, L))
#define SCM_VTABLE_FLAGS(X)             (SCM_STRUCT_DATA_REF (X, scm_vtable_index_flags))
#define SCM_SET_VTABLE_FLAGS(X,F)       (SCM_STRUCT_DATA_REF (X, scm_vtable_index_flags) |= (F))
#define SCM_CLEAR_VTABLE_FLAGS(X,F)     (SCM_STRUCT_DATA_REF (X, scm_vtable_index_flags) &= (~(F)))
#define SCM_VTABLE_FLAG_IS_SET(X,F)     (SCM_STRUCT_DATA_REF (X, scm_vtable_index_flags) & (F))
#define SCM_VTABLE_INSTANCE_FINALIZER(X) ((scm_t_struct_finalize)SCM_STRUCT_DATA_REF (X, scm_vtable_index_instance_finalize))
#define SCM_SET_VTABLE_INSTANCE_FINALIZER(X,P) (SCM_STRUCT_DATA_SET (X, scm_vtable_index_instance_finalize, (scm_t_bits)(P)))
#define SCM_VTABLE_INSTANCE_PRINTER(X)  (SCM_STRUCT_SLOT_REF (X, scm_vtable_index_instance_printer))
#define SCM_SET_VTABLE_INSTANCE_PRINTER(X,P) (SCM_STRUCT_SLOT_SET (X, scm_vtable_index_instance_printer, (P)))
#define SCM_VTABLE_NAME(X)              (SCM_STRUCT_SLOT_REF (X, scm_vtable_index_name))
#define SCM_SET_VTABLE_NAME(X,V)        (SCM_STRUCT_SLOT_SET (X, scm_vtable_index_name, V))
#define SCM_VTABLE_SIZE(X)              (SCM_STRUCT_DATA_REF (X, scm_vtable_index_size))
#define SCM_VTABLE_UNBOXED_FIELDS(X)    ((uint32_t*) SCM_STRUCT_DATA_REF (X, scm_vtable_index_unboxed_fields))
#define SCM_VTABLE_FIELD_IS_UNBOXED(X,F) (SCM_VTABLE_UNBOXED_FIELDS (X)[(F)>>5]&(1U<<((F)&31)))

#define SCM_STRUCT_VTABLE(X)            (SCM_PACK (SCM_CELL_WORD_0 (X) - scm_tc3_struct))
#define SCM_STRUCT_LAYOUT(X) 	        (SCM_VTABLE_LAYOUT (SCM_STRUCT_VTABLE (X)))
#define SCM_STRUCT_SIZE(X) 	        (SCM_VTABLE_SIZE (SCM_STRUCT_VTABLE (X)))
#define SCM_STRUCT_PRINTER(X) 	        (SCM_VTABLE_INSTANCE_PRINTER (SCM_STRUCT_VTABLE (X)))
#define SCM_STRUCT_FINALIZER(X)         (SCM_VTABLE_INSTANCE_FINALIZER (SCM_STRUCT_VTABLE (X)))
#define SCM_STRUCT_VTABLE_FLAGS(X)      (SCM_VTABLE_FLAGS (SCM_STRUCT_VTABLE (X)))
#define SCM_STRUCT_VTABLE_FLAG_IS_SET(X,F) (SCM_VTABLE_FLAG_IS_SET (SCM_STRUCT_VTABLE (X), (F)))
#define SCM_STRUCT_FIELD_IS_UNBOXED(X,F) (SCM_VTABLE_FIELD_IS_UNBOXED (SCM_STRUCT_VTABLE (X), (F)))

#define SCM_STRUCT_APPLICABLE_P(X) 	(SCM_STRUCT_VTABLE_FLAG_IS_SET ((X), SCM_VTABLE_FLAG_APPLICABLE))
#define SCM_STRUCT_SETTER_P(X) 	        (SCM_STRUCT_VTABLE_FLAG_IS_SET ((X), SCM_VTABLE_FLAG_SETTER))
#define SCM_STRUCT_PROCEDURE(X) 	(SCM_STRUCT_SLOT_REF (X, scm_applicable_struct_index_procedure))
#define SCM_SET_STRUCT_PROCEDURE(X,P) 	(SCM_STRUCT_SLOT_SET (X, scm_applicable_struct_index_procedure, P))
#define SCM_STRUCT_SETTER(X)            (SCM_STRUCT_SLOT_REF (X, scm_applicable_struct_index_setter))
#define SCM_SET_STRUCT_SETTER(X,P) 	(SCM_STRUCT_SLOT_SET (X, scm_applicable_struct_index_setter, P))

SCM_API SCM scm_standard_vtable_vtable;
SCM_API SCM scm_applicable_struct_vtable_vtable;
SCM_API SCM scm_applicable_struct_with_setter_vtable_vtable;



SCM_API SCM scm_make_struct_layout (SCM fields);
SCM_API SCM scm_struct_p (SCM x);
SCM_API SCM scm_struct_vtable_p (SCM x);
SCM_INTERNAL SCM scm_allocate_struct (SCM vtable, SCM n_words);
SCM_INTERNAL SCM scm_make_struct_simple (SCM vtable, SCM init);
SCM_API SCM scm_make_struct_no_tail (SCM vtable, SCM init);
SCM_API SCM scm_c_make_struct (SCM vtable, size_t n_tail, size_t n_inits,
                               scm_t_bits init, ...);
SCM_API SCM scm_c_make_structv (SCM vtable, size_t n_tail, size_t n_inits,
                                scm_t_bits init[]);
SCM_API SCM scm_make_vtable (SCM fields, SCM printer);
SCM_INTERNAL SCM scm_i_make_vtable_vtable (SCM fields);
SCM_API SCM scm_struct_ref (SCM handle, SCM pos);
SCM_API SCM scm_struct_set_x (SCM handle, SCM pos, SCM val);
SCM_API SCM scm_struct_ref_unboxed (SCM handle, SCM pos);
SCM_API SCM scm_struct_set_x_unboxed (SCM handle, SCM pos, SCM val);
SCM_API SCM scm_struct_vtable (SCM handle);
SCM_API SCM scm_struct_vtable_name (SCM vtable);
SCM_API SCM scm_set_struct_vtable_name_x (SCM vtable, SCM name);
SCM_API void scm_print_struct (SCM exp, SCM port, scm_print_state *);

SCM_INTERNAL SCM scm_i_struct_equalp (SCM s1, SCM s2);
SCM_INTERNAL unsigned long scm_struct_ihashq (SCM, unsigned long, void *);
SCM_INTERNAL void scm_i_struct_inherit_vtable_magic (SCM vtable, SCM obj);
SCM_INTERNAL void scm_init_struct (void);

#endif  /* SCM_STRUCT_H */
