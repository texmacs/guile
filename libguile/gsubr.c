/* Copyright 1995-2001,2006,2008-2011,2013,2015,2018-2019
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



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <flexmember.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "foreign.h"
#include "frames.h"
#include "instructions.h"
#include "jit.h"
#include "modules.h"
#include "numbers.h"
#include "private-options.h"
#include "programs.h"
#include "srfi-4.h"
#include "symbols.h"
#include "threads.h"

#include "gsubr.h"



/*
 * gsubr.c
 * Provide `gsubrs' -- subrs taking a prescribed number of required, optional,
 * and rest arguments.
 */

/* In July 2018 there were 1140 subrs defined in stock Guile.  */
static const size_t expected_subr_count = 1500;

static scm_i_pthread_mutex_t admin_mutex = SCM_I_PTHREAD_MUTEX_INITIALIZER;

static void **subrs = NULL;
static uint32_t next_subr_idx = 0;
static uint32_t subrs_array_size = 0;

static uint32_t
alloc_subr_idx (void *subr)
{
  uint32_t idx;

  scm_i_pthread_mutex_lock (&admin_mutex);

  idx = next_subr_idx++;

  if (idx > 0xffffff) abort ();

  if (idx >= subrs_array_size)
    {
      void **new_subrs;

      if (subrs_array_size)
        subrs_array_size *= 2;
      else
        subrs_array_size = expected_subr_count;

      /* Leak this allocation, as code lives as long as the program
         does.  In the likely case, we only make one malloc for the
         program; in the general case it's still O(n) in number of subrs
         because of the geometric factor.  Use malloc instead of GC
         allocations because it's not traceable and not collectable.  */
      new_subrs = malloc (subrs_array_size * sizeof (void*));
      memcpy (new_subrs, subrs, idx * sizeof (void*));
      subrs = new_subrs;
    }

  subrs[idx] = subr;

  scm_i_pthread_mutex_unlock (&admin_mutex);

  return idx;
}



static SCM *names = NULL;
static uint32_t names_array_size = 0;

static void
record_subr_name (uint32_t idx, SCM name)
{
  scm_i_pthread_mutex_lock (&admin_mutex);

  if (idx >= names_array_size)
    {
      SCM *new_names;
      uint32_t new_size;

      if (names_array_size)
        new_size = names_array_size * 2;
      else
        new_size = expected_subr_count;

      new_names = SCM_GC_MALLOC (new_size * sizeof (SCM));
      memcpy (new_names, names, names_array_size * sizeof (SCM));
      while (names_array_size < new_size)
        new_names[names_array_size++] = SCM_BOOL_F;
      names = new_names;
      names_array_size = new_size;
    }

  names[idx] = name;

  scm_i_pthread_mutex_unlock (&admin_mutex);
}



struct code_arena {
  struct code_arena *next;
  size_t size;
  size_t used;
  char data[FLEXIBLE_ARRAY_MEMBER];
};

static struct code_arena *code_arena = NULL;

static size_t
round_up_power_of_two (size_t n, size_t m)
{
  return (n + (m-1)) & ~(m-1);
}

static struct code_arena *
alloc_chunk (size_t size, struct code_arena *next)
{
  /* Leak the allocation, as we currently don't allow code to be
     collected.  */
  struct code_arena *ret = malloc (FLEXSIZEOF (struct code_arena, data, size));
  if (!ret) abort ();
  ret->next = next;
  ret->size = size;
  ret->used = 0;
  return ret;
}

static char *
alloc (size_t byte_size)
{
  char *ret;

  byte_size = round_up_power_of_two (byte_size, sizeof (void *));

  scm_i_pthread_mutex_lock (&admin_mutex);

  if (code_arena == NULL || code_arena->size - code_arena->used < byte_size)
    {
      size_t chunk_size;
      size_t avg_code_size = 6 * sizeof(uint32_t);
      avg_code_size += sizeof (struct scm_jit_function_data);

      chunk_size = expected_subr_count * avg_code_size;
      if (chunk_size < byte_size)
        chunk_size = byte_size;

      code_arena = alloc_chunk (chunk_size, code_arena);
    }

  ret = &code_arena->data[code_arena->used];
  code_arena->used += byte_size;

  scm_i_pthread_mutex_unlock (&admin_mutex);

  memset (ret, 0, byte_size);

  return ret;
}

uint32_t *
scm_i_alloc_primitive_code_with_instrumentation (size_t uint32_count,
                                                 uint32_t **write_ptr)
{
  char *ptr;
  uint32_t *ret;
  struct scm_jit_function_data *data;
  size_t byte_size, padded_byte_size;

  /* Reserve space for instrument-entry.  */
  byte_size = (2 + uint32_count) * sizeof (uint32_t);
  padded_byte_size = round_up_power_of_two (byte_size, sizeof (void *));
  /* Reserve space for jit data.  */
  ptr = alloc (padded_byte_size + sizeof (struct scm_jit_function_data));
  ret = (uint32_t *) ptr;
  data = (struct scm_jit_function_data*) (ptr + padded_byte_size);

  ret[0] = SCM_PACK_OP_24 (instrument_entry, 0);
  ret[1] = padded_byte_size / 4;

  *write_ptr = ret + 2;

  data->mcode = NULL;
  data->counter = 0;
  data->start = -padded_byte_size;
  data->end = -(padded_byte_size - byte_size);

  return ret;
}

static int
is_primitive_code (const void *ptr)
{
  const char *cptr = ptr;
  int ret = 0;
  struct code_arena *arena;

  scm_i_pthread_mutex_lock (&admin_mutex);
  for (arena = code_arena; arena; arena = arena->next)
    if (&arena->data[0] <= cptr && cptr < &arena->data[arena->used])
      {
        ret = 1;
        break;
      }
  scm_i_pthread_mutex_unlock (&admin_mutex);

  return ret;
}

static uint32_t *
alloc_subr_code (uint32_t subr_idx, uint32_t code[], size_t code_size)
{
  uint32_t post[3] = { SCM_PACK_OP_24 (subr_call, subr_idx),
                       SCM_PACK_OP_24 (handle_interrupts, 0),
                       SCM_PACK_OP_24 (return_values, 0) };
  uint32_t *ret, *write;

  ret = scm_i_alloc_primitive_code_with_instrumentation (code_size + 3, &write);

  memcpy (write, code, code_size * sizeof (uint32_t));
  write += code_size;
  memcpy (write, post, 3 * sizeof (uint32_t));

  return ret;
}

enum arity_kind {
  NULLARY = 0,
  REQ = 1,
  OPT = 2,
  REST = 4,
  REQ_OPT = REQ + OPT,
  REQ_REST = REQ + REST,
  OPT_REST = OPT + REST,
  REQ_OPT_REST = REQ + OPT + REST
};

static uint32_t*
get_subr_stub_code (uint32_t subr_idx,
                    unsigned int nreq, unsigned int nopt, unsigned int rest)
{
  enum arity_kind kind = NULLARY;

  if (SCM_UNLIKELY (rest > 1 || nreq + nopt + rest > 10))
    scm_out_of_range ("make-subr", scm_from_uint (nreq + nopt + rest));
      
  if (nreq) kind += REQ;
  if (nopt) kind += OPT;
  if (rest) kind += REST;

  switch (kind)
    {
    case NULLARY:
    case REQ:
      {
        uint32_t code[1] = { SCM_PACK_OP_24 (assert_nargs_ee, nreq + 1) };
        return alloc_subr_code (subr_idx, code, 1);
      }
    case OPT:
      {
        uint32_t code[2] = { SCM_PACK_OP_24 (assert_nargs_le, nopt + 1),
                             SCM_PACK_OP_24 (bind_optionals, nopt + 1) };
        return alloc_subr_code (subr_idx, code, 2);
      }
    case REST:
      {
        uint32_t code[1] = { SCM_PACK_OP_24 (bind_rest, 1) };
        return alloc_subr_code (subr_idx, code, 1);
      }
    case REQ_OPT:
      {
        uint32_t code[3] = { SCM_PACK_OP_24 (assert_nargs_ge, nreq + 1),
                             SCM_PACK_OP_24 (assert_nargs_le, nreq + nopt + 1),
                             SCM_PACK_OP_24 (bind_optionals, nreq + nopt + 1) };
        return alloc_subr_code (subr_idx, code, 3);
      }
    case REQ_REST:
      {
        uint32_t code[2] = { SCM_PACK_OP_24 (assert_nargs_ge, nreq + 1),
                             SCM_PACK_OP_24 (bind_rest, nreq + 1) };
        return alloc_subr_code (subr_idx, code, 2);
      }
    case OPT_REST:
      {
        uint32_t code[2] = { SCM_PACK_OP_24 (bind_optionals, nopt + 1),
                             SCM_PACK_OP_24 (bind_rest, nopt + 1) };
        return alloc_subr_code (subr_idx, code, 2);
      }
    case REQ_OPT_REST:
      {
        uint32_t code[3] = { SCM_PACK_OP_24 (assert_nargs_ge, nreq + 1),
                             SCM_PACK_OP_24 (bind_optionals, nreq + nopt + 1),
                             SCM_PACK_OP_24 (bind_rest, nreq + nopt + 1) };
        return alloc_subr_code (subr_idx, code, 3);
      }
    default:
      abort ();
    }
}

static SCM
create_subr (int define, const char *name,
             unsigned int nreq, unsigned int nopt, unsigned int rest,
             void *fcn, SCM *generic_loc)
{
  SCM ret, sname;
  uint32_t idx;
  scm_t_bits flags;
  scm_t_bits nfree = generic_loc ? 1 : 0;

  idx = alloc_subr_idx (fcn);
  sname = scm_from_utf8_symbol (name);

  flags = SCM_F_PROGRAM_IS_PRIMITIVE;
  flags |= generic_loc ? SCM_F_PROGRAM_IS_PRIMITIVE_GENERIC : 0;

  ret = scm_words (scm_tc7_program | (nfree << 16) | flags, nfree + 2);
  SCM_SET_CELL_WORD_1 (ret, get_subr_stub_code (idx, nreq, nopt, rest));
  record_subr_name (idx, sname);
  if (generic_loc)
    SCM_PROGRAM_FREE_VARIABLE_SET (ret, 0,
                                   scm_from_pointer (generic_loc, NULL));

  if (define)
    scm_define (sname, ret);

  return ret;
}

int
scm_i_primitive_code_p (const uint32_t *code)
{
  return is_primitive_code (code);
}

static uintptr_t
primitive_call_ip (const uint32_t *code)
{
  int direction = 0;
  while (1)
    {
      switch (code[0] & 0xff)
        {
        case scm_op_instrument_entry:
          if (direction < 0) abort ();
          direction = 1;
          code += 2;
          break;
        case scm_op_assert_nargs_ee:
        case scm_op_assert_nargs_le:
        case scm_op_assert_nargs_ge:
        case scm_op_bind_optionals:
        case scm_op_bind_rest:
        case scm_op_alloc_frame:
          if (direction < 0) abort ();
          direction = 1;
          code += 1;
          break;
        case scm_op_subr_call:
        case scm_op_foreign_call:
          return (uintptr_t) code;
        case scm_op_return_values:
        case scm_op_handle_interrupts:
          /* Going back isn't possible for instruction streams where we
             don't know the length of the preceding instruction, but for
             the code we emit, these particular opcodes are only ever
             preceded by 4-byte instructions.  */
          if (direction > 0) abort ();
          direction = -1;
          code -= 1;
          break;
        default:
          return 0;
        }
    }
}

static const uint32_t NOT_A_SUBR_CALL = 0xffffffff;

static uint32_t
primitive_subr_idx (const uint32_t *code)
{
  uint32_t word;
  uintptr_t call_ip = primitive_call_ip (code);
  if (call_ip == 0)
    return NOT_A_SUBR_CALL;
  word = ((uint32_t *) call_ip)[0];
  if ((word & 0xff) == scm_op_subr_call)
    {
      uint32_t idx = word >> 8;
      if (idx >= next_subr_idx) abort();
      return idx;
    }
  else
    return NOT_A_SUBR_CALL;
}

uintptr_t
scm_i_primitive_call_ip (SCM subr)
{
  return primitive_call_ip (SCM_PROGRAM_CODE (subr));
}

SCM
scm_i_primitive_name (const uint32_t *code)
{
  uint32_t idx = primitive_subr_idx (code);
  if (idx == NOT_A_SUBR_CALL)
    return SCM_BOOL_F;
  return names[idx];
}

scm_t_subr
scm_subr_function_by_index (uint32_t idx)
{
  if (idx == NOT_A_SUBR_CALL)
    abort ();
  return subrs[idx];
}  

scm_t_subr
scm_subr_function (SCM subr)
{
  uint32_t idx = primitive_subr_idx (SCM_PROGRAM_CODE (subr));
  return scm_subr_function_by_index (idx);
}

SCM
scm_subr_name (SCM subr)
{
  return scm_i_primitive_name (SCM_PROGRAM_CODE (subr));
}

SCM
scm_apply_subr (union scm_vm_stack_element *sp, uint32_t idx, ptrdiff_t nslots)
{
  SCM (*subr)() = subrs[idx];

#define ARG(i) (sp[i].as_scm)
  switch (nslots - 1)
    {
    case 0:
      return subr ();
    case 1:
      return subr (ARG (0));
    case 2:
      return subr (ARG (1), ARG (0));
    case 3:
      return subr (ARG (2), ARG (1), ARG (0));
    case 4:
      return subr (ARG (3), ARG (2), ARG (1), ARG (0));
    case 5:
      return subr (ARG (4), ARG (3), ARG (2), ARG (1), ARG (0));
    case 6:
      return subr (ARG (5), ARG (4), ARG (3), ARG (2), ARG (1),
                   ARG (0));
    case 7:
      return subr (ARG (6), ARG (5), ARG (4), ARG (3), ARG (2),
                   ARG (1), ARG (0));
    case 8:
      return subr (ARG (7), ARG (6), ARG (5), ARG (4), ARG (3),
                   ARG (2), ARG (1), ARG (0));
    case 9:
      return subr (ARG (8), ARG (7), ARG (6), ARG (5), ARG (4),
                   ARG (3), ARG (2), ARG (1), ARG (0));
    case 10:
      return subr (ARG (9), ARG (8), ARG (7), ARG (6), ARG (5),
                   ARG (4), ARG (3), ARG (2), ARG (1), ARG (0));
    default:
      abort ();
    }
#undef ARG
}

SCM
scm_c_make_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  return create_subr (0, name, req, opt, rst, fcn, NULL);
}

SCM
scm_c_define_gsubr (const char *name, int req, int opt, int rst, SCM (*fcn)())
{
  return create_subr (1, name, req, opt, rst, fcn, NULL);
}

SCM
scm_c_make_gsubr_with_generic (const char *name,
			       int req,
			       int opt,
			       int rst,
			       SCM (*fcn)(),
			       SCM *gf)
{
  return create_subr (0, name, req, opt, rst, fcn, gf);
}

SCM
scm_c_define_gsubr_with_generic (const char *name,
				 int req,
				 int opt,
				 int rst,
				 SCM (*fcn)(),
				 SCM *gf)
{
  return create_subr (1, name, req, opt, rst, fcn, gf);
}

void
scm_init_gsubr()
{
#include "gsubr.x"
}
