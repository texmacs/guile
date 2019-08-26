/* Copyright 2018-2019
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

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <math.h>

#include "alist.h"
#include "atomics-internal.h"
#include "boolean.h"
#include "cache-internal.h"
#include "extensions.h"
#include "fluids.h"
#include "frames.h"
#include "gc-inline.h"
#include "goops.h"
#include "gsubr.h"
#include "keywords.h"
#include "modules.h"
#include "numbers.h"
#include "symbols.h"
#include "threads.h"
#include "version.h"

#include "intrinsics.h"


struct scm_vm_intrinsics scm_vm_intrinsics;

SCM_DEFINE (scm_intrinsic_list, "intrinsic-list", 0, 0, 0,
	    (void),
	    "")
#define FUNC_NAME s_scm_intrinsic_list
{
  SCM list = SCM_EOL;

#define ADD_INTRINSIC(type, id, name, ID)                   \
  if (name)                                                 \
    list = scm_acons (scm_from_latin1_symbol (name),        \
                      scm_from_int (SCM_VM_INTRINSIC_##ID), \
                      list);
  SCM_FOR_ALL_VM_INTRINSICS (ADD_INTRINSIC);
#undef ADD_INTRINSIC

  return list;
}
#undef FUNC_NAME

static SCM
add_immediate (SCM a, uint8_t b)
{
  if (SCM_LIKELY (SCM_I_INUMP (a)))
    {
      scm_t_signed_bits sum = SCM_I_INUM (a) + b;

      if (SCM_LIKELY (SCM_POSFIXABLE (sum)))
        return SCM_I_MAKINUM (sum);
    }

  return scm_sum (a, scm_from_uint8 (b));
}

static SCM
sub_immediate (SCM a, uint8_t b)
{
  if (SCM_LIKELY (SCM_I_INUMP (a)))
    {
      scm_t_signed_bits diff = SCM_I_INUM (a) - b;

      if (SCM_LIKELY (SCM_NEGFIXABLE (diff)))
        return SCM_I_MAKINUM (diff);
    }

  return scm_difference (a, scm_from_uint8 (b));
}

static void
string_set_x (SCM str, size_t idx, uint32_t ch)
{
  str = scm_i_string_start_writing (str);
  scm_i_string_set_x (str, idx, ch);
  scm_i_string_stop_writing ();
}

static SCM
string_to_number (SCM str)
{
  return scm_string_to_number (str, SCM_UNDEFINED /* radix = 10 */);
}

static uint64_t
scm_to_uint64_truncate (SCM x)
{
  if (SCM_LIKELY (SCM_I_INUMP (x)))
    return (uint64_t) SCM_I_INUM (x);
  else
    return scm_to_uint64 (scm_logand (x, scm_from_uint64 ((uint64_t) -1)));
}

#if INDIRECT_INT64_INTRINSICS
static void
indirect_scm_to_int64 (int64_t *dst, SCM x)
{
  *dst = scm_to_int64 (x);
}
static void
indirect_scm_to_uint64 (uint64_t *dst, SCM x)
{
  *dst = scm_to_uint64 (x);
}
static void
indirect_scm_to_uint64_truncate (uint64_t *dst, SCM x)
{
  *dst = scm_to_uint64_truncate (x);
}
static SCM
indirect_scm_from_int64 (int64_t *src)
{
  return scm_from_int64 (*src);
}
static SCM
indirect_scm_from_uint64 (uint64_t *src)
{
  return scm_from_uint64 (*src);
}
#endif

static SCM
logsub (SCM x, SCM y)
{
  if (SCM_I_INUMP (x) && SCM_I_INUMP (y))
    {
      scm_t_signed_bits a, b;

      a = SCM_I_INUM (x);
      b = SCM_I_INUM (y);

      return SCM_I_MAKINUM (a & ~b);
    }

  return scm_logand (x, scm_lognot (y));
}

static void
wind (scm_thread *thread, SCM winder, SCM unwinder)
{
  scm_dynstack_push_dynwind (&thread->dynstack, winder, unwinder);
}

static void
unwind (scm_thread *thread)
{
  scm_dynstack_pop (&thread->dynstack);
}

static void
push_fluid (scm_thread *thread, SCM fluid, SCM value)
{
  scm_dynstack_push_fluid (&thread->dynstack, fluid, value,
                           thread->dynamic_state);
}

static void
pop_fluid (scm_thread *thread)
{
  scm_dynstack_unwind_fluid (&thread->dynstack, thread->dynamic_state);
}

static SCM
fluid_ref (scm_thread *thread, SCM fluid)
{
  struct scm_cache_entry *entry;

  /* If we find FLUID in the cache, then it is indeed a fluid.  */
  entry = scm_cache_lookup (&thread->dynamic_state->cache, fluid);
  if (SCM_LIKELY (scm_is_eq (SCM_PACK (entry->key), fluid)
                  && !SCM_UNBNDP (SCM_PACK (entry->value))))
    return SCM_PACK (entry->value);

  return scm_fluid_ref (fluid);
}

static void
fluid_set_x (scm_thread *thread, SCM fluid, SCM value)
{
  struct scm_cache_entry *entry;

  /* If we find FLUID in the cache, then it is indeed a fluid.  */
  entry = scm_cache_lookup (&thread->dynamic_state->cache, fluid);
  if (SCM_LIKELY (scm_is_eq (SCM_PACK (entry->key), fluid)))
    entry->value = SCM_UNPACK (value);
  else
    scm_fluid_set_x (fluid, value);
}

static void
push_dynamic_state (scm_thread *thread, SCM state)
{
  scm_dynstack_push_dynamic_state (&thread->dynstack, state,
                                   thread->dynamic_state);
}

static void
pop_dynamic_state (scm_thread *thread)
{
  scm_dynstack_unwind_dynamic_state (&thread->dynstack,
                                     thread->dynamic_state);
}

static SCM
lsh (SCM a, uint64_t b)
{
  if (SCM_LIKELY (SCM_I_INUMP (a))
      && b < (uint64_t) (SCM_I_FIXNUM_BIT - 1)
      && ((scm_t_bits)
          (SCM_SRS (SCM_I_INUM (a), (SCM_I_FIXNUM_BIT-1 - b)) + 1)
          <= 1))
    {
      scm_t_signed_bits nn = SCM_I_INUM (a);
      return SCM_I_MAKINUM (nn < 0 ? -(-nn << b) : (nn << b));
    }
  else
    return scm_ash (a, scm_from_uint64 (b));
}

static SCM
rsh (SCM a, uint64_t b)
{
  if (SCM_LIKELY (SCM_I_INUMP (a)))
    {
      if (b > (uint64_t) (SCM_I_FIXNUM_BIT - 1))
        b = SCM_I_FIXNUM_BIT - 1;
      return SCM_I_MAKINUM (SCM_SRS (SCM_I_INUM (a), b));
    }
  else
    return scm_ash (a, scm_difference (SCM_INUM0, scm_from_uint64 (b)));
}

#if INDIRECT_INT64_INTRINSICS
static SCM
indirect_lsh (SCM a, uint64_t *b)
{
  return lsh (a, *b);
}
static SCM
indirect_rsh (SCM a, uint64_t *b)
{
  return rsh (a, *b);
}
#endif

static SCM
lsh_immediate (SCM a, uint8_t b)
{
  return lsh (a, b);
}

static SCM
rsh_immediate (SCM a, uint8_t b)
{
  return rsh (a, b);
}

static enum scm_compare
less_p (SCM a, SCM b)
{
  if (SCM_LIKELY (SCM_I_INUMP (a) && SCM_I_INUMP (b)))
    {
      scm_t_signed_bits a_bits = SCM_UNPACK (a);
      scm_t_signed_bits b_bits = SCM_UNPACK (b);
      return a_bits < b_bits ? SCM_F_COMPARE_LESS_THAN : SCM_F_COMPARE_NONE;
    }

  if (scm_is_true (scm_nan_p (a)) || scm_is_true (scm_nan_p (b)))
    return SCM_F_COMPARE_INVALID;
  else if (scm_is_true (scm_less_p (a, b)))
    return SCM_F_COMPARE_LESS_THAN;
  else
    return SCM_F_COMPARE_NONE;
}

static int
numerically_equal_p (SCM a, SCM b)
{
  if (SCM_LIKELY (SCM_I_INUMP (a) && SCM_I_INUMP (b)))
    return scm_is_eq (a, b);

  return scm_is_true (scm_num_eq_p (a, b));
}

static SCM
resolve_module (SCM name, uint8_t public_p)
{
  SCM mod;

  if (!scm_module_system_booted_p)
    return SCM_BOOL_F;

  mod = scm_maybe_resolve_module (name);
  if (scm_is_false (mod))
    scm_misc_error (NULL, "Module named ~s does not exist",
                    scm_list_1 (name));

  if (public_p)
    {
      mod = scm_module_public_interface (mod);

      if (scm_is_false (mod))
        scm_misc_error (NULL, "Module named ~s has no public interface",
                        scm_list_1 (name));
    }

  return mod;
}

static SCM
lookup (SCM module, SCM name)
{
  /* If MODULE was captured before modules were booted, use the root
     module.  Not so nice, but hey...  */
  if (scm_is_false (module))
    module = scm_the_root_module ();

  return scm_module_variable (module, name);
}

static void throw_ (SCM key, SCM args) SCM_NORETURN;
static void throw_with_value (SCM val, SCM key_subr_and_message) SCM_NORETURN;
static void throw_with_value_and_data (SCM val, SCM key_subr_and_message) SCM_NORETURN;

static void
throw_ (SCM key, SCM args)
{
  scm_throw (key, args);
  abort(); /* not reached */
}

static void
throw_with_value (SCM val, SCM key_subr_and_message)
{
  SCM key, subr, message, args, data;

  key = SCM_SIMPLE_VECTOR_REF (key_subr_and_message, 0);
  subr = SCM_SIMPLE_VECTOR_REF (key_subr_and_message, 1);
  message = SCM_SIMPLE_VECTOR_REF (key_subr_and_message, 2);
  args = scm_list_1 (val);
  data = SCM_BOOL_F;

  throw_ (key, scm_list_4 (subr, message, args, data));
}

static void
throw_with_value_and_data (SCM val, SCM key_subr_and_message)
{
  SCM key, subr, message, args, data;

  key = SCM_SIMPLE_VECTOR_REF (key_subr_and_message, 0);
  subr = SCM_SIMPLE_VECTOR_REF (key_subr_and_message, 1);
  message = SCM_SIMPLE_VECTOR_REF (key_subr_and_message, 2);
  args = scm_list_1 (val);
  data = args;

  throw_ (key, scm_list_4 (subr, message, args, data));
}

static void error_wrong_num_args (scm_thread *) SCM_NORETURN;
static void error_no_values (void) SCM_NORETURN;
static void error_not_enough_values (void) SCM_NORETURN;
static void error_wrong_number_of_values (uint32_t expected) SCM_NORETURN;

static void
error_wrong_num_args (scm_thread *thread)
{
  SCM callee = SCM_FRAME_LOCAL (thread->vm.fp, 0);
  scm_wrong_num_args (callee);
}

static void
error_no_values (void)
{
  scm_misc_error (NULL, "Zero values returned to single-valued continuation",
                  SCM_EOL);
}

static void
error_not_enough_values (void)
{
  scm_misc_error (NULL, "Too few values returned to continuation", SCM_EOL);
}

static void
error_wrong_number_of_values (uint32_t expected)
{
  scm_misc_error (NULL,
                  "Wrong number of values returned to continuation (expected ~a)",
                  scm_list_1 (scm_from_uint32 (expected)));
}

static SCM
allocate_words (scm_thread *thread, size_t n)
{
  return SCM_PACK_POINTER (scm_inline_gc_malloc_words (thread, n));
}

static SCM
allocate_words_with_freelist (scm_thread *thread, size_t freelist_idx)
{
  return SCM_PACK_POINTER
    (scm_inline_gc_alloc (&thread->freelists[freelist_idx],
                          freelist_idx,
                          SCM_INLINE_GC_KIND_NORMAL));
}

static SCM
allocate_pointerless_words (scm_thread *thread, size_t n)
{
  return SCM_PACK_POINTER (scm_inline_gc_malloc_pointerless_words (thread, n));
}

static SCM
allocate_pointerless_words_with_freelist (scm_thread *thread, size_t freelist_idx)
{
  return SCM_PACK_POINTER
    (scm_inline_gc_alloc (&thread->pointerless_freelists[freelist_idx],
                          freelist_idx,
                          SCM_INLINE_GC_KIND_POINTERLESS));
}

static SCM
current_module (scm_thread *thread)
{
  return scm_i_current_module (thread);
}

static void
push_prompt (scm_thread *thread, uint8_t escape_only_p,
             SCM tag, const union scm_vm_stack_element *sp, uint32_t *vra,
             uint8_t *mra)
{
  struct scm_vm *vp = &thread->vm;
  scm_t_dynstack_prompt_flags flags;

  flags = escape_only_p ? SCM_F_DYNSTACK_PROMPT_ESCAPE_ONLY : 0;
  scm_dynstack_push_prompt (&thread->dynstack, flags, tag,
                            vp->stack_top - vp->fp, vp->stack_top - sp,
                            vra, mra, thread->vm.registers);
}

static SCM
scm_atan1 (SCM x)
{
  return scm_atan (x, SCM_UNDEFINED);
}

void
scm_bootstrap_intrinsics (void)
{
  scm_vm_intrinsics.add = scm_sum;
  scm_vm_intrinsics.add_immediate = add_immediate;
  scm_vm_intrinsics.sub = scm_difference;
  scm_vm_intrinsics.sub_immediate = sub_immediate;
  scm_vm_intrinsics.mul = scm_product;
  scm_vm_intrinsics.div = scm_divide;
  scm_vm_intrinsics.quo = scm_quotient;
  scm_vm_intrinsics.rem = scm_remainder;
  scm_vm_intrinsics.mod = scm_modulo;
  scm_vm_intrinsics.logand = scm_logand;
  scm_vm_intrinsics.logior = scm_logior;
  scm_vm_intrinsics.logxor = scm_logxor;
  scm_vm_intrinsics.string_set_x = string_set_x;
  scm_vm_intrinsics.string_to_number = string_to_number;
  scm_vm_intrinsics.string_to_symbol = scm_string_to_symbol;
  scm_vm_intrinsics.symbol_to_keyword = scm_symbol_to_keyword;
  scm_vm_intrinsics.class_of = scm_class_of;
  scm_vm_intrinsics.scm_to_f64 = scm_to_double;
#if INDIRECT_INT64_INTRINSICS
  scm_vm_intrinsics.scm_to_u64 = indirect_scm_to_uint64;
  scm_vm_intrinsics.scm_to_u64_truncate = indirect_scm_to_uint64_truncate;
  scm_vm_intrinsics.scm_to_s64 = indirect_scm_to_int64;
  scm_vm_intrinsics.u64_to_scm = indirect_scm_from_uint64;
  scm_vm_intrinsics.s64_to_scm = indirect_scm_from_int64;
#else
  scm_vm_intrinsics.scm_to_u64 = scm_to_uint64;
  scm_vm_intrinsics.scm_to_u64_truncate = scm_to_uint64_truncate;
  scm_vm_intrinsics.scm_to_s64 = scm_to_int64;
  scm_vm_intrinsics.u64_to_scm = scm_from_uint64;
  scm_vm_intrinsics.s64_to_scm = scm_from_int64;
#endif
  scm_vm_intrinsics.logsub = logsub;
  scm_vm_intrinsics.wind = wind;
  scm_vm_intrinsics.unwind = unwind;
  scm_vm_intrinsics.push_fluid = push_fluid;
  scm_vm_intrinsics.pop_fluid = pop_fluid;
  scm_vm_intrinsics.fluid_ref = fluid_ref;
  scm_vm_intrinsics.fluid_set_x = fluid_set_x;
  scm_vm_intrinsics.push_dynamic_state = push_dynamic_state;
  scm_vm_intrinsics.pop_dynamic_state = pop_dynamic_state;
#if INDIRECT_INT64_INTRINSICS
  scm_vm_intrinsics.lsh = indirect_lsh;
  scm_vm_intrinsics.rsh = indirect_rsh;
#else
  scm_vm_intrinsics.lsh = lsh;
  scm_vm_intrinsics.rsh = rsh;
#endif
  scm_vm_intrinsics.lsh_immediate = lsh_immediate;
  scm_vm_intrinsics.rsh_immediate = rsh_immediate;
  scm_vm_intrinsics.heap_numbers_equal_p = scm_i_heap_numbers_equal_p;
  scm_vm_intrinsics.less_p = less_p;
  scm_vm_intrinsics.numerically_equal_p = numerically_equal_p;
  scm_vm_intrinsics.resolve_module = resolve_module;
  scm_vm_intrinsics.lookup = lookup;
  scm_vm_intrinsics.define_x = scm_module_ensure_local_variable;
  scm_vm_intrinsics.throw_ = throw_;
  scm_vm_intrinsics.throw_with_value = throw_with_value;
  scm_vm_intrinsics.throw_with_value_and_data = throw_with_value_and_data;
  scm_vm_intrinsics.error_wrong_num_args = error_wrong_num_args;
  scm_vm_intrinsics.error_no_values = error_no_values;
  scm_vm_intrinsics.error_not_enough_values = error_not_enough_values;
  scm_vm_intrinsics.error_wrong_number_of_values = error_wrong_number_of_values;
  scm_vm_intrinsics.allocate_words = allocate_words;
  scm_vm_intrinsics.current_module = current_module;
  scm_vm_intrinsics.push_prompt = push_prompt;
  scm_vm_intrinsics.allocate_words_with_freelist = allocate_words_with_freelist;
  scm_vm_intrinsics.abs = scm_abs;
  scm_vm_intrinsics.sqrt = scm_sqrt;
  scm_vm_intrinsics.fabs = fabs;
  scm_vm_intrinsics.fsqrt = sqrt;
  scm_vm_intrinsics.floor = scm_floor;
  scm_vm_intrinsics.ceiling = scm_ceiling;
  scm_vm_intrinsics.sin = scm_sin;
  scm_vm_intrinsics.cos = scm_cos;
  scm_vm_intrinsics.tan = scm_tan;
  scm_vm_intrinsics.asin = scm_asin;
  scm_vm_intrinsics.acos = scm_acos;
  scm_vm_intrinsics.atan = scm_atan1;
  scm_vm_intrinsics.atan2 = scm_atan;
  scm_vm_intrinsics.ffloor = floor;
  scm_vm_intrinsics.fceiling = ceil;
  scm_vm_intrinsics.fsin = sin;
  scm_vm_intrinsics.fcos = cos;
  scm_vm_intrinsics.ftan = tan;
  scm_vm_intrinsics.fasin = asin;
  scm_vm_intrinsics.facos = acos;
  scm_vm_intrinsics.fatan = atan;
  scm_vm_intrinsics.fatan2 = atan2;
  scm_vm_intrinsics.allocate_pointerless_words = allocate_pointerless_words;
  scm_vm_intrinsics.allocate_pointerless_words_with_freelist =
    allocate_pointerless_words_with_freelist;

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_intrinsics",
                            (scm_t_extension_init_func)scm_init_intrinsics,
                            NULL);
}

void
scm_init_intrinsics (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "intrinsics.x"
#endif
}
