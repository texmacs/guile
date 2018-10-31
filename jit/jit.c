/*
 * Copyright (C) 2012-2018  Free Software Foundation, Inc.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GNU lightning is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 */

#include <sys/mman.h>
#if defined(__sgi)
#  include <fcntl.h>
#endif

#include "../jit.h"
#include "private.h"

#ifndef MAP_ANON
#  define MAP_ANON			MAP_ANONYMOUS
#  ifndef MAP_ANONYMOUS
#    define MAP_ANONYMOUS		0
#  endif
#endif

#if !defined(__sgi)
#define  mmap_fd			-1
#endif

#define jit_regload_reload		0	/* convert to reload */
#define jit_regload_delete		1	/* just remove node */
#define jit_regload_isdead		2	/* delete and unset live bit */


/*
 * Implementation
 */
void
init_jit(void)
{
    jit_get_cpu();
}

jit_state_t *
jit_new_state(void)
{
    jit_state_t		*_jit;

    _jit = malloc (sizeof (*_jit));
    if (!_jit)
      abort ();

    memset(_jit, 0, sizeof (*_jit));

    jit_init (_jit);

    return _jit;
}

void
jit_destroy_state(jit_state_t *_jit)
{
  free (_jit);
}

jit_pointer_t
jit_address(jit_state_t *_jit)
{
  /* TODO: FIXME */
  abort ();
}

void
jit_begin(jit_state_t *_jit, jit_addr_t addr, size_t length)
{
  ASSERT (!_jit->start);

  _jit->start = addr;
  _jit->limit = _jit->start + length;
  jit_reset(_jit);
}

void
jit_reset(jit_state_t *_jit)
{
  ASSERT (_jit->start);
  _jit->pc.uc = _jit->start = _jit->limit = NULL;
}

jit_addr_t
jit_end(jit_state_t *_jit, size_t *length)
{
  uint8_t *code = _jit->start;
  uint8_t *end = _jit->pc.uc;

  ASSERT (code);
  ASSERT (end > code);
  ASSERT (end <= _jit->limit);

  jit_flush (code, end);

  if (length) {
    *length = end - code;
  }

  jit_reset (_jit);

  return code;
}

static int
is_power_of_two (unsigned x)
{
  return x && !(x & (x-1));
}

void
jit_align(jit_state_t *_jit, unsigned align)
{
  ASSERT (is_power_of_two (align));
  uintptr_t here = _jit->pc.w;
  uintptr_t there = (here + align - 1) & ~(align - 1);
  if (there - here)
    jit_nop(_jit, there - here);
}

void
jit_patch_here(jit_state_t *_jit, jit_reloc_t *reloc)
{
  jit_patch_there (_jit, reloc, jit_address (_jit));
}

void
jit_patch_there(jit_state_t* _jit, jit_reloc_t *reloc, jit_pointer_t *addr)
{
  if (reloc == _jit->last_instruction)
    jit_patch_last (_jit, reloc, addr);
  else
    jit_patch (_jit, reloc, addr);
}

#if defined(__i386__) || defined(__x86_64__)
#  include "x86.c"
#elif defined(__mips__)
#  include "mips.c"
#elif defined(__arm__)
#  include "arm.c"
#elif defined(__ppc__) || defined(__powerpc__)
#  include "ppc.c"
#elif defined(__sparc__)
#  include "sparc.c"
#elif defined(__ia64__)
#  include "ia64.c"
#elif defined(__hppa__)
#  include "hppa.c"
#elif defined(__aarch64__)
#  include "aarch64.c"
#elif defined(__s390__) || defined(__s390x__)
#  include "s390.c"
#elif defined(__alpha__)
#  include "alpha.c"
#endif
