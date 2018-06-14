/* Copyright (C) 1996-2001, 2003-2004, 2006-2013, 2015,
 *               2017-2018 Free Software Foundation, Inc.
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


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <alloca.h>
#include <assert.h>

#define SCM_BUILDING_DEPRECATED_CODE

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/chars.h"
#include "libguile/deprecation.h"
#include "libguile/eval.h"
#include "libguile/finalizers.h"
#include "libguile/goops.h"
#include "libguile/alist.h"
#include "libguile/hashtab.h"
#include "libguile/ports.h"
#include "libguile/strings.h"
#include "libguile/srfi-13.h"

#include "libguile/validate.h"
#include "libguile/struct.h"

#include "libguile/eq.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libguile/bdw-gc.h"



static SCM required_vtable_fields = SCM_BOOL_F;
static SCM required_applicable_fields = SCM_BOOL_F;
static SCM required_applicable_with_setter_fields = SCM_BOOL_F;
SCM scm_applicable_struct_vtable_vtable;
SCM scm_applicable_struct_with_setter_vtable_vtable;
SCM scm_standard_vtable_vtable;



SCM_DEFINE (scm_make_struct_layout, "make-struct-layout", 1, 0, 0, 
            (SCM fields),
	    "Return a new structure layout object.\n\n"
	    "@var{fields} must be a string made up of pairs of characters\n"
	    "strung together.  The first character of each pair describes a field\n"
	    "type, the second a field protection.  Allowed types are 'p' for\n"
	    "GC-protected Scheme data, 'u' for unprotected binary data.  \n"
            "Allowed protections are 'w' for normal fields or 'h' for \n"
            "hidden fields.\n\n"
            "Hidden fields are writable, but they will not consume an initializer arg\n"
            "passed to @code{make-struct}. They are useful to add slots to a struct\n"
            "in a way that preserves backward-compatibility with existing calls to\n"
            "@code{make-struct}, especially for derived vtables.")
#define FUNC_NAME s_scm_make_struct_layout
{
  SCM new_sym;
  scm_t_wchar c;

  SCM_VALIDATE_STRING (1, fields);

  { /* scope */
    size_t len;
    int x;

    len = scm_i_string_length (fields);
    if (len % 2 == 1)
      SCM_MISC_ERROR ("odd length field specification: ~S", 
		      scm_list_1 (fields));

    for (x = 0; x < len; x += 2)
      {
	switch (c = scm_i_string_ref (fields, x))
	  {
	  case 'u':
	  case 'p':
	    break;
	  default:
	    SCM_MISC_ERROR ("unrecognized field type: ~S", 
			    scm_list_1 (SCM_MAKE_CHAR (c)));
	  }

	switch (c = scm_i_string_ref (fields, x + 1))
	  {
	  case 'w':
	  case 'h':
	  case 'r':
	    break;
	  default:
	    SCM_MISC_ERROR ("unrecognized ref specification: ~S",
			    scm_list_1 (SCM_MAKE_CHAR (c)));
	  }
      }
    new_sym = scm_string_to_symbol (fields);
  }
  scm_remember_upto_here_1 (fields);
  return new_sym;
}
#undef FUNC_NAME


static void
set_vtable_access_fields (SCM vtable)
{
  size_t len, nfields, bitmask_size, field;
  SCM layout;
  const char *c_layout;
  scm_t_uint32 *unboxed_fields;

  layout = SCM_VTABLE_LAYOUT (vtable);
  c_layout = scm_i_symbol_chars (layout);
  len = scm_i_symbol_length (layout);

  assert (len % 2 == 0);
  nfields = len / 2;

  bitmask_size = (nfields + 31U) / 32U;
  unboxed_fields = scm_gc_malloc_pointerless (bitmask_size, "unboxed fields");
  memset (unboxed_fields, 0, bitmask_size * sizeof(*unboxed_fields));

  /* Update FLAGS according to LAYOUT.  */
  for (field = 0; field < nfields; field++)
    if (c_layout[field*2] == 'u')
      unboxed_fields[field/32U] |= 1U << (field%32U);

  /* Record computed size of vtable's instances.  */
  SCM_SET_VTABLE_FLAGS (vtable, 0);
  SCM_STRUCT_DATA_SET (vtable, scm_vtable_index_size, len / 2);
  SCM_STRUCT_DATA_SET (vtable, scm_vtable_index_unboxed_fields,
                       (scm_t_uintptr) unboxed_fields);
}

static int
scm_is_valid_vtable_layout (SCM layout)
{
  size_t len, n;
  const char *c_layout;

  c_layout = scm_i_symbol_chars (layout);
  len = scm_i_symbol_length (layout);

  if (len % 2)
    return 0;
  
  for (n = 0; n < len; n += 2)
    switch (c_layout[n])
      {
      case 'u':
      case 'p':
        switch (c_layout[n+1])
          {
          case 'w':
          case 'h':
            break;
          case 'r':
            scm_c_issue_deprecation_warning
              ("Read-only struct fields are deprecated.  Implement access "
               "control at a higher level instead, as structs no longer "
               "enforce field permissions.");
            break;
          default:
            return 0;
          }
        break;
      default:        
        return 0;
      }
  return 1;
}

/* Have OBJ, a newly created vtable, inherit flags from VTABLE.  VTABLE is a
   vtable-vtable and OBJ is an instance of VTABLE.  */
void
scm_i_struct_inherit_vtable_magic (SCM vtable, SCM obj)
#define FUNC_NAME "%inherit-vtable-magic"
{
  /* Verily, what is the deal here, you ask? Basically, we need to know a couple
     of properties of structures at runtime. For example, "is this structure a
     vtable of vtables (a metaclass)?"; also, "is this structure applicable?".
     Both of these questions also imply a certain layout of the structure. So
     instead of checking the layout at runtime, what we do is pre-verify the
     layout -- so that at runtime we can just check the applicable flag and
     dispatch directly to the Scheme procedure in slot 0.  */
  SCM olayout;

  /* Verify that OBJ is a valid vtable.  */
  if (! scm_is_valid_vtable_layout (SCM_VTABLE_LAYOUT (obj)))
    SCM_MISC_ERROR ("invalid layout for new vtable: ~a",
                    scm_list_1 (SCM_VTABLE_LAYOUT (obj)));

  set_vtable_access_fields (obj);

  /* If OBJ's vtable is compatible with the required vtable (class) layout, it
     is a metaclass.  */
  olayout = scm_symbol_to_string (SCM_VTABLE_LAYOUT (obj));
  if (scm_is_true (scm_leq_p (scm_string_length (required_vtable_fields),
                              scm_string_length (olayout)))
      && scm_is_true (scm_string_eq (olayout, required_vtable_fields,
                                     scm_from_size_t (0), 
                                     scm_string_length (required_vtable_fields),
                                     scm_from_size_t (0),
                                     scm_string_length (required_vtable_fields))))
    SCM_SET_VTABLE_FLAGS (obj, SCM_VTABLE_FLAG_VTABLE);

  /* Finally, if OBJ is an applicable class, verify that its vtable is
     compatible with the required applicable layout.  */
  if (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_SETTER_VTABLE))
    {
      if (scm_is_false (scm_string_eq (olayout, required_applicable_with_setter_fields,
                                       scm_from_size_t (0), 
                                       scm_from_size_t (4), 
                                       scm_from_size_t (0),
                                       scm_from_size_t (4))))
        SCM_MISC_ERROR ("invalid applicable-with-setter struct layout",
                        scm_list_1 (olayout));
      SCM_SET_VTABLE_FLAGS (obj, SCM_VTABLE_FLAG_APPLICABLE | SCM_VTABLE_FLAG_SETTER);
    }
  else if (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_APPLICABLE_VTABLE))
    {
      if (scm_is_false (scm_string_eq (olayout, required_applicable_fields,
                                       scm_from_size_t (0), 
                                       scm_from_size_t (2), 
                                       scm_from_size_t (0),
                                       scm_from_size_t (2))))
        SCM_MISC_ERROR ("invalid applicable struct layout",
                        scm_list_1 (olayout));
      SCM_SET_VTABLE_FLAGS (obj, SCM_VTABLE_FLAG_APPLICABLE);
    }

  SCM_SET_VTABLE_FLAGS (obj, SCM_VTABLE_FLAG_VALIDATED);
}
#undef FUNC_NAME


static void
scm_struct_init (SCM handle, SCM layout, size_t n_inits, scm_t_bits *inits)
{
  size_t n, n_fields, inits_idx = 0;

  n_fields = SCM_STRUCT_SIZE (handle);

  for (n = 0; n < n_fields; n++)
    {
      if (inits_idx == n_inits || scm_i_symbol_ref (layout, n*2+1) == 'h')
        {
          if (SCM_STRUCT_FIELD_IS_UNBOXED (handle, n))
            SCM_STRUCT_DATA_SET (handle, n, 0);
          else
            SCM_STRUCT_SLOT_SET (handle, n, SCM_BOOL_F);
        }
      else
        {
          SCM_STRUCT_DATA_SET (handle, n,
                               SCM_STRUCT_FIELD_IS_UNBOXED (handle, n)
                               ? scm_to_uintptr_t (SCM_PACK (inits[inits_idx]))
                               : inits[inits_idx]);
          inits_idx++;
        }
    }
}


SCM_DEFINE (scm_struct_p, "struct?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} iff @var{x} is a structure object, else\n"
	    "@code{#f}.")
#define FUNC_NAME s_scm_struct_p
{
  return scm_from_bool(SCM_STRUCTP (x));
}
#undef FUNC_NAME

SCM_DEFINE (scm_struct_vtable_p, "struct-vtable?", 1, 0, 0, 
            (SCM x),
	    "Return @code{#t} iff @var{x} is a vtable structure.")
#define FUNC_NAME s_scm_struct_vtable_p
{
  if (!SCM_STRUCTP (x)
      || !SCM_STRUCT_VTABLE_FLAG_IS_SET (x, SCM_VTABLE_FLAG_VTABLE))
    return SCM_BOOL_F;
  if (!SCM_VTABLE_FLAG_IS_SET (x, SCM_VTABLE_FLAG_VALIDATED))
    SCM_MISC_ERROR ("vtable has invalid layout: ~A",
                    scm_list_1 (SCM_VTABLE_LAYOUT (x)));
  return SCM_BOOL_T;
}
#undef FUNC_NAME


/* Finalization: invoke the finalizer of the struct pointed to by PTR.  */
static void
struct_finalizer_trampoline (void *ptr, void *unused_data)
{
  SCM obj = PTR2SCM (ptr);
  scm_t_struct_finalize finalize = SCM_STRUCT_FINALIZER (obj);

  if (finalize)
    finalize (obj);
}

/* A struct is a sequence of words preceded by a pointer to the struct's
   vtable.  The vtable reference is tagged with the struct tc3.  */
static SCM
scm_i_alloc_struct (scm_t_bits vtable_bits, int n_words)
{
  SCM ret;

  ret = scm_words (vtable_bits | scm_tc3_struct, n_words + 1);

  /* vtable_bits can be 0 when making a vtable vtable */
  if (vtable_bits && SCM_VTABLE_INSTANCE_FINALIZER (SCM_PACK (vtable_bits)))
    /* Register a finalizer for the newly created instance.  */
    scm_i_set_finalizer (SCM2PTR (ret), struct_finalizer_trampoline, NULL);

  return ret;
}


SCM
scm_c_make_structv (SCM vtable, size_t n_tail, size_t n_init, scm_t_bits *init)
#define FUNC_NAME "make-struct"
{
  size_t basic_size;
  SCM obj;

  SCM_VALIDATE_VTABLE (1, vtable);

  basic_size = SCM_VTABLE_SIZE (vtable);

  SCM_ASSERT (n_tail == 0, scm_from_size_t (n_tail), 2, FUNC_NAME);

  obj = scm_i_alloc_struct (SCM_UNPACK (vtable), basic_size);
  scm_struct_init (obj, SCM_VTABLE_LAYOUT (vtable), n_init, init);

  /* If we're making a vtable, validate its layout and inherit
     flags. However we allow for separation of allocation and
     initialization, to humor GOOPS, so only validate if the layout was
     passed as an initarg. */
  if (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_VTABLE)
      && scm_is_true (SCM_VTABLE_LAYOUT (obj)))
    scm_i_struct_inherit_vtable_magic (vtable, obj);

  return obj;
}
#undef FUNC_NAME

SCM
scm_c_make_struct (SCM vtable, size_t n_tail, size_t n_init, scm_t_bits init, ...)
{
  va_list foo;
  scm_t_bits *v;
  size_t i;

  SCM_ASSERT (n_tail == 0, scm_from_size_t (n_tail), 2, "scm_c_make_struct");

  v = alloca (sizeof (scm_t_bits) * n_init);

  va_start (foo, init);
  for (i = 0; i < n_init; i++)
    {
      v[i] = init;
      init = va_arg (foo, scm_t_bits);
    }
  va_end (foo);

  return scm_c_make_structv (vtable, 0, n_init, v);
}

SCM_DEFINE (scm_allocate_struct, "allocate-struct", 2, 0, 0,
            (SCM vtable, SCM nfields),
	    "Allocate a new structure with space for @var{nfields} fields.\n\n"
	    "@var{vtable} must be a vtable structure (@pxref{Vtables}).\n\n"
	    "@var{nfields} must be a non-negative integer.  Strictly speaking\n"
	    "@var{nfields} is redundant, as the vtable carries the size\n"
            "for its instances.  However passing it is useful as a sanity\n"
            "check, given that one module can inline a constructor in\n"
            "another.\n\n"
	    "Fields will be initialized with their default values.")
#define FUNC_NAME s_scm_allocate_struct
{
  SCM ret;
  size_t c_nfields;

  SCM_VALIDATE_VTABLE (1, vtable);
  c_nfields = scm_to_size_t (nfields);

  SCM_ASSERT (SCM_VTABLE_SIZE (vtable) == c_nfields, nfields, 2, FUNC_NAME);

  ret = scm_i_alloc_struct (SCM_UNPACK (vtable), c_nfields);
  scm_struct_init (ret, SCM_VTABLE_LAYOUT (vtable), 0, NULL);

  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_struct_simple, "make-struct/simple", 1, 0, 1,
            (SCM vtable, SCM init),
	    "Create a new structure.\n\n"
	    "@var{vtable} must be a vtable structure (@pxref{Vtables}).\n\n"
	    "The @var{init1}, @dots{} arguments supply the initial values\n"
	    "for the structure's fields\n.\n"
	    "This is a restricted variant of @code{make-struct/no-tail}\n"
            "which applies only if the structure has no unboxed fields.\n"
            "@code{make-struct/simple} must be called with as many\n"
            "@var{init} values as the struct has fields.  No finalizer is set\n"
            "on the instance, even if the vtable has a non-zero finalizer\n"
            "field.  No magical vtable fields are inherited.\n\n"
            "The advantage of using @code{make-struct/simple} is that the\n"
            "compiler can inline it, so it is faster.  When in doubt though,\n"
            "use @code{make-struct/no-tail}.")
#define FUNC_NAME s_scm_make_struct_simple
{
  long i, n_init;
  SCM ret;

  SCM_VALIDATE_VTABLE (1, vtable);
  n_init = scm_ilength (init);
  if (n_init != SCM_VTABLE_SIZE (vtable))
    SCM_MISC_ERROR ("Wrong number of initializers.", SCM_EOL);

  ret = scm_words (SCM_UNPACK (vtable) | scm_tc3_struct, n_init + 1);

  for (i = 0; i < n_init; i++, init = scm_cdr (init))
    {
      SCM_ASSERT (!SCM_VTABLE_FIELD_IS_UNBOXED (vtable, i),
                  vtable, 1, FUNC_NAME);
      SCM_STRUCT_SLOT_SET (ret, i, scm_car (init));
    }

  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_struct_no_tail, "make-struct/no-tail", 1, 0, 1, 
            (SCM vtable, SCM init),
	    "Create a new structure.\n\n"
	    "@var{vtable} must be a vtable structure (@pxref{Vtables}).\n\n"
	    "The @var{init1}, @dots{} are optional arguments describing how\n"
	    "successive fields of the structure should be initialized.\n"
            "Note that hidden fields (those with protection 'h') have to be\n"
            "manually set.\n\n"
	    "If fewer optional arguments than initializable fields are supplied,\n"
	    "fields of type 'p' get default value #f while fields of type 'u' are\n"
	    "initialized to 0.")
#define FUNC_NAME s_scm_make_struct_no_tail
{
  size_t i, n_init;
  long ilen;
  scm_t_bits *v;

  SCM_VALIDATE_VTABLE (1, vtable);
  ilen = scm_ilength (init);
  if (ilen < 0)
    SCM_MISC_ERROR ("Rest arguments do not form a proper list.", SCM_EOL);
  
  n_init = (size_t)ilen;

  /* best to use alloca, but init could be big, so hack to avoid a possible
     stack overflow */
  if (n_init < 64)
    v = alloca (n_init * sizeof(scm_t_bits));
  else
    v = scm_gc_malloc (n_init * sizeof(scm_t_bits), "struct");

  for (i = 0; i < n_init; i++, init = SCM_CDR (init))
    v[i] = SCM_UNPACK (SCM_CAR (init));

  return scm_c_make_structv (vtable, 0, n_init, v);
}
#undef FUNC_NAME

SCM
scm_i_make_vtable_vtable (SCM fields)
#define FUNC_NAME "make-vtable-vtable"
{
  SCM layout, obj;
  size_t n, nfields;

  SCM_VALIDATE_STRING (1, fields);

  layout = scm_make_struct_layout (fields);
  if (!scm_is_valid_vtable_layout (layout))
    SCM_MISC_ERROR ("invalid user fields", scm_list_1 (fields));

  nfields = scm_i_symbol_length (layout) / 2;

  obj = scm_i_alloc_struct (0, nfields);
  /* Make it so that the vtable of OBJ is itself.  */
  SCM_SET_CELL_WORD_0 (obj, SCM_UNPACK (obj) | scm_tc3_struct);
  /* Manually initialize fields.  */
  SCM_STRUCT_SLOT_SET (obj, scm_vtable_index_layout, layout);
  set_vtable_access_fields (obj);
  SCM_SET_VTABLE_FLAGS (obj, SCM_VTABLE_FLAG_VTABLE | SCM_VTABLE_FLAG_VALIDATED);
  SCM_STRUCT_DATA_SET (obj, scm_vtable_index_instance_finalize, 0);
  SCM_STRUCT_SLOT_SET (obj, scm_vtable_index_instance_printer, SCM_BOOL_F);
  SCM_STRUCT_SLOT_SET (obj, scm_vtable_index_name, SCM_BOOL_F);
  SCM_STRUCT_DATA_SET (obj, scm_vtable_index_reserved_7, 0);

  for (n = scm_vtable_offset_user; n < nfields; n++)
    if (SCM_STRUCT_FIELD_IS_UNBOXED (obj, n))
      SCM_STRUCT_DATA_SET (obj, n, 0);
    else
      SCM_STRUCT_SLOT_SET (obj, n, SCM_BOOL_F);

  return obj;
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_vtable, "make-vtable", 1, 1, 0,
            (SCM fields, SCM printer),
	    "Create a vtable, for creating structures with the given\n"
	    "@var{fields}.\n"
	    "\n"
	    "The optional @var{printer} argument is a function to be called\n"
	    "@code{(@var{printer} struct port)} on the structures created.\n"
	    "It should look at @var{struct} and write to @var{port}.")
#define FUNC_NAME s_scm_make_vtable
{
  if (SCM_UNBNDP (printer))
    printer = SCM_BOOL_F;

  return scm_c_make_struct (scm_standard_vtable_vtable, 0, 2,
                            SCM_UNPACK (scm_make_struct_layout (fields)),
                            SCM_UNPACK (printer));
}
#undef FUNC_NAME


/* Return true if S1 and S2 are equal structures, i.e., if their vtable and
   contents are the same.  */
SCM
scm_i_struct_equalp (SCM s1, SCM s2)
#define FUNC_NAME "scm_i_struct_equalp"
{
  size_t struct_size, field_num;

  SCM_VALIDATE_STRUCT (1, s1);
  SCM_VALIDATE_STRUCT (2, s2);

  if (!scm_is_eq (SCM_STRUCT_VTABLE (s1), SCM_STRUCT_VTABLE (s2)))
    return SCM_BOOL_F;

  struct_size = SCM_STRUCT_SIZE (s1);

  for (field_num = 0; field_num < struct_size; field_num++)
    {
      scm_t_bits field1, field2;

      field1 = SCM_STRUCT_DATA_REF (s1, field_num);
      field2 = SCM_STRUCT_DATA_REF (s2, field_num);

      if (field1 != field2) {
        if (SCM_STRUCT_FIELD_IS_UNBOXED (s1, field_num))
          return SCM_BOOL_F;

        /* Having a normal field point to the object itself is a bit
           bonkers, but R6RS enums do it, so here we have a horrible
           hack.  */
        if (field1 != SCM_UNPACK (s1) && field2 != SCM_UNPACK (s2))
          {
            if (scm_is_false
                (scm_equal_p (SCM_PACK (field1), SCM_PACK (field2))))
              return SCM_BOOL_F;
          }
      }
    }

  return SCM_BOOL_T;
}
#undef FUNC_NAME





SCM_DEFINE (scm_struct_ref, "struct-ref", 2, 0, 0,
            (SCM handle, SCM pos),
	    "Access the @var{pos}th field of struct associated with\n"
	    "@var{handle}.\n"
	    "\n"
	    "If the field is of type 'p', then it can be set to an arbitrary\n"
	    "value.\n"
	    "\n"
	    "If the field is of type 'u', then it can only be set to a\n"
	    "non-negative integer value small enough to fit in one machine\n"
	    "word.")
#define FUNC_NAME s_scm_struct_ref
{
  size_t nfields, p;

  SCM_VALIDATE_STRUCT (1, handle);

  nfields = SCM_STRUCT_SIZE (handle);
  p = scm_to_size_t (pos);

  SCM_ASSERT_RANGE (2, pos, p < nfields);

  SCM_ASSERT (!SCM_STRUCT_FIELD_IS_UNBOXED (handle, p), pos, 2, FUNC_NAME);

  return SCM_STRUCT_SLOT_REF (handle, p);
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_set_x, "struct-set!", 3, 0, 0,
            (SCM handle, SCM pos, SCM val),
	    "Set the slot of the structure @var{handle} with index @var{pos}\n"
	    "to @var{val}.  Signal an error if the slot can not be written\n"
	    "to.")
#define FUNC_NAME s_scm_struct_set_x
{
  size_t nfields, p;

  SCM_VALIDATE_STRUCT (1, handle);

  nfields = SCM_STRUCT_SIZE (handle);
  p = scm_to_size_t (pos);

  SCM_ASSERT_RANGE (2, pos, p < nfields);

  SCM_ASSERT (!SCM_STRUCT_FIELD_IS_UNBOXED (handle, p), pos, 2, FUNC_NAME);

  SCM_STRUCT_SLOT_SET (handle, p, val);

  return val;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_ref_unboxed, "struct-ref/unboxed", 2, 0, 0,
            (SCM handle, SCM pos),
	    "Access the @var{pos}th field of struct associated with\n"
	    "@var{handle}.  The field must be of type 'u'.")
#define FUNC_NAME s_scm_struct_ref_unboxed
{
  size_t nfields, p;

  SCM_VALIDATE_STRUCT (1, handle);

  nfields = SCM_STRUCT_SIZE (handle);
  p = scm_to_size_t (pos);

  SCM_ASSERT_RANGE (2, pos, p < nfields);

  SCM_ASSERT (SCM_STRUCT_FIELD_IS_UNBOXED (handle, p), pos, 2, FUNC_NAME);

  return scm_from_uintptr_t (SCM_STRUCT_DATA_REF (handle, p));
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_set_x_unboxed, "struct-set!/unboxed", 3, 0, 0,
            (SCM handle, SCM pos, SCM val),
	    "Set the slot of the structure @var{handle} with index @var{pos}\n"
	    "to @var{val}.  Signal an error if the slot can not be written\n"
	    "to.")
#define FUNC_NAME s_scm_struct_set_x_unboxed
{
  size_t nfields, p;

  SCM_VALIDATE_STRUCT (1, handle);

  nfields = SCM_STRUCT_SIZE (handle);
  p = scm_to_size_t (pos);

  SCM_ASSERT_RANGE (2, pos, p < nfields);

  SCM_ASSERT (SCM_STRUCT_FIELD_IS_UNBOXED (handle, p), pos, 2, FUNC_NAME);

  SCM_STRUCT_DATA_SET (handle, p, scm_to_uintptr_t (val));

  return val;
}
#undef FUNC_NAME


SCM_DEFINE (scm_struct_vtable, "struct-vtable", 1, 0, 0, 
            (SCM handle),
	    "Return the vtable structure that describes the type of struct\n"
	    "associated with @var{handle}.")
#define FUNC_NAME s_scm_struct_vtable
{
  SCM_VALIDATE_STRUCT (1, handle);
  return SCM_STRUCT_VTABLE (handle);
}
#undef FUNC_NAME


/* {Associating names and classes with vtables}
 *
 * The name of a vtable should probably be stored as a slot.  This is
 * a backward compatible solution until agreement has been achieved on
 * how to associate names with vtables.
 */

unsigned long
scm_struct_ihashq (SCM obj, unsigned long n, void *closure)
{
  /* The length of the hash table should be a relative prime it's not
     necessary to shift down the address.  */
  return SCM_UNPACK (obj) % n;
}

SCM_DEFINE (scm_struct_vtable_name, "struct-vtable-name", 1, 0, 0, 
            (SCM vtable),
	    "Return the name of the vtable @var{vtable}.")
#define FUNC_NAME s_scm_struct_vtable_name
{
  SCM_VALIDATE_VTABLE (1, vtable);
  return SCM_VTABLE_NAME (vtable);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_struct_vtable_name_x, "set-struct-vtable-name!", 2, 0, 0, 
            (SCM vtable, SCM name),
	    "Set the name of the vtable @var{vtable} to @var{name}.")
#define FUNC_NAME s_scm_set_struct_vtable_name_x
{
  SCM_VALIDATE_VTABLE (1, vtable);
  SCM_VALIDATE_SYMBOL (2, name);
  SCM_SET_VTABLE_NAME (vtable, name);
  /* FIXME: remove this, and implement proper struct classes instead.
     (Vtables *are* classes.)  */
  scm_i_define_class_for_vtable (vtable);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




void
scm_print_struct (SCM exp, SCM port, scm_print_state *pstate)
{
  if (scm_is_true (scm_procedure_p (SCM_STRUCT_PRINTER (exp))))
    scm_printer_apply (SCM_STRUCT_PRINTER (exp), exp, port, pstate);
  else
    {
      SCM vtable = SCM_STRUCT_VTABLE (exp);
      SCM name = scm_struct_vtable_name (vtable);
      scm_puts ("#<", port);
      if (scm_is_true (name))
	{
          scm_display (name, port);
          scm_putc (' ', port);
        }
      else
	{
          if (SCM_VTABLE_FLAG_IS_SET (vtable, SCM_VTABLE_FLAG_VTABLE))
            scm_puts ("vtable:", port);
          else
            scm_puts ("struct:", port);
          scm_uintprint (SCM_UNPACK (vtable), 16, port);
          scm_putc (' ', port);
          scm_write (SCM_VTABLE_LAYOUT (vtable), port);
          scm_putc (' ', port);
        }
      scm_uintprint (SCM_UNPACK (exp), 16, port);
      /* hackety hack */
      if (SCM_STRUCT_APPLICABLE_P (exp))
        {
          if (scm_is_true (SCM_STRUCT_PROCEDURE (exp)))
            {
              scm_puts (" proc: ", port);
              if (scm_is_true (scm_procedure_p (SCM_STRUCT_PROCEDURE (exp))))
                scm_write (SCM_STRUCT_PROCEDURE (exp), port);
              else
                scm_puts ("(not a procedure?)", port);
            }
          if (SCM_STRUCT_SETTER_P (exp))
            {
              scm_puts (" setter: ", port);
              scm_write (SCM_STRUCT_SETTER (exp), port);
            }
        }
      scm_putc ('>', port);
    }
}

void
scm_init_struct ()
{
  SCM name;

  required_vtable_fields = scm_from_latin1_string (SCM_VTABLE_BASE_LAYOUT);
  scm_c_define ("standard-vtable-fields", required_vtable_fields);
  required_applicable_fields = scm_from_latin1_string (SCM_APPLICABLE_BASE_LAYOUT);
  required_applicable_with_setter_fields = scm_from_latin1_string (SCM_APPLICABLE_WITH_SETTER_BASE_LAYOUT);

  scm_standard_vtable_vtable =
    scm_i_make_vtable_vtable (required_vtable_fields);
  name = scm_from_utf8_symbol ("<standard-vtable>");
  scm_set_struct_vtable_name_x (scm_standard_vtable_vtable, name);
  scm_define (name, scm_standard_vtable_vtable);

  scm_applicable_struct_vtable_vtable =
    scm_c_make_struct (scm_standard_vtable_vtable, 0, 1,
                       SCM_UNPACK (scm_make_struct_layout (required_vtable_fields)));
  name = scm_from_utf8_symbol ("<applicable-struct-vtable>");
  SCM_SET_VTABLE_FLAGS (scm_applicable_struct_vtable_vtable,
                        SCM_VTABLE_FLAG_APPLICABLE_VTABLE);
  scm_set_struct_vtable_name_x (scm_applicable_struct_vtable_vtable, name);
  scm_define (name, scm_applicable_struct_vtable_vtable);

  scm_applicable_struct_with_setter_vtable_vtable =
    scm_c_make_struct (scm_standard_vtable_vtable, 0, 1,
                       SCM_UNPACK (scm_make_struct_layout (required_vtable_fields)));
  name = scm_from_utf8_symbol ("<applicable-struct-with-setter-vtable>");
  scm_set_struct_vtable_name_x (scm_applicable_struct_with_setter_vtable_vtable, name);
  SCM_SET_VTABLE_FLAGS (scm_applicable_struct_with_setter_vtable_vtable,
                        SCM_VTABLE_FLAG_APPLICABLE_VTABLE | SCM_VTABLE_FLAG_SETTER_VTABLE);
  scm_define (name, scm_applicable_struct_with_setter_vtable_vtable);

  scm_c_define ("vtable-index-layout", scm_from_int (scm_vtable_index_layout));
  scm_c_define ("vtable-index-printer",
		scm_from_int (scm_vtable_index_instance_printer));
  scm_c_define ("vtable-offset-user", scm_from_int (scm_vtable_offset_user));
#include "libguile/struct.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
