/* Copyright 1995-2004,2006,2008-2014,2018
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
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "arrays.h"
#include "async.h"
#ifdef GUILE_DEBUG_MALLOC
#include "debug-malloc.h"
#endif
#include "deprecation.h"
#include "eval.h"
#include "hashtab.h"
#include "ports.h"
#include "smob.h"
#include "stackchk.h"
#include "stime.h"
#include "strings.h"
#include "struct.h"
#include "vectors.h"

#include "gc.h"


/*
  INIT_MALLOC_LIMIT is the initial amount of malloc usage which will
  trigger a GC.
  
  After startup (at the guile> prompt), we have approximately 100k of
  alloced memory, which won't go away on GC. Let's set the init such
  that we get a nice yield on the next allocation:
*/
#define SCM_DEFAULT_INIT_MALLOC_LIMIT 200*1024
#define SCM_DEFAULT_MALLOC_MINYIELD 40

/* #define DEBUGINFO */




static void*
do_realloc (void *from, size_t new_size)
{
  scm_gc_register_allocation (new_size);
  return realloc (from, new_size);
}

static void*
do_calloc (size_t n, size_t size)
{
  scm_gc_register_allocation (size);
  return calloc (n, size);
}

static void*
do_gc_malloc (size_t size, const char *what)
{
  /* Ensure nonzero size to be compatible with always-nonzero return of
     glibc malloc.  */
  return GC_MALLOC (size ? size : sizeof (void *));
}

static void*
do_gc_malloc_atomic (size_t size, const char *what)
{
  return GC_MALLOC_ATOMIC (size ? size : sizeof (void *));
}

static void*
do_gc_realloc (void *from, size_t size, const char *what)
{
  return GC_REALLOC (from, size ? size : sizeof (void *));
}

static void
do_gc_free (void *ptr)
{
  GC_FREE (ptr);
}



/* Function for non-cell memory management.
 */

void *
scm_realloc (void *mem, size_t size)
{
  void *ptr;

  ptr = do_realloc (mem, size);

  if (ptr || size == 0)
    return ptr;

  /* Time is hard: trigger a full, ``stop-the-world'' GC, and try again.  */
  GC_gcollect_and_unmap ();

  ptr = do_realloc (mem, size);
  if (ptr)
    return ptr;

  scm_report_out_of_memory ();

  /* Not reached.  */
  return NULL;
}

void *
scm_malloc (size_t sz)
{
  return scm_realloc (NULL, sz);
}

/*
  Hmm. Should we use the C convention for arguments (i.e. N_ELTS,
  SIZEOF_ELT)? --hwn
 */
void *
scm_calloc (size_t sz)
{
  void * ptr;

  /*
    By default, try to use calloc, as it is likely more efficient than
    calling memset by hand.
   */
  ptr = do_calloc (sz, 1);
  if (ptr || sz == 0)
    return ptr;

  ptr = scm_realloc (NULL, sz);
  memset (ptr, 0x0, sz);
  return ptr;
}


char *
scm_strndup (const char *str, size_t n)
{
  char *dst = scm_malloc (n + 1);
  memcpy (dst, str, n);
  dst[n] = 0;
  return dst;
}

char *
scm_strdup (const char *str)
{
  return scm_strndup (str, strlen (str));
}



void
scm_gc_register_collectable_memory (void *mem, size_t size, const char *what)
{
  scm_gc_register_allocation (size);

#ifdef GUILE_DEBUG_MALLOC
  if (mem)
    scm_malloc_register (mem, what);
#endif
}


void
scm_gc_unregister_collectable_memory (void *mem, size_t size, const char *what)
{
  /* Nothing to do.  */
#ifdef GUILE_DEBUG_MALLOC
  if (mem)
    scm_malloc_unregister (mem);
#endif
}

/* Allocate SIZE bytes of memory whose contents should not be scanned
   for pointers (useful, e.g., for strings).  Note though that this
   memory is *not* cleared; be sure to initialize it to prevent
   information leaks.  */
void *
scm_gc_malloc_pointerless (size_t size, const char *what)
{
  return do_gc_malloc_atomic (size, what);
}

void *
scm_gc_malloc (size_t size, const char *what)
{
  return do_gc_malloc (size, what);
}

void *
scm_gc_calloc (size_t size, const char *what)
{
  /* `GC_MALLOC ()' always returns a zeroed buffer.  */
  return do_gc_malloc (size, what);
}

void *
scm_gc_realloc (void *mem, size_t old_size, size_t new_size, const char *what)
{
  return do_gc_realloc (mem, new_size, what);
}

void
scm_gc_free (void *mem, size_t size, const char *what)
{
  do_gc_free (mem);
}

char *
scm_gc_strndup (const char *str, size_t n, const char *what)
{
  char *dst = do_gc_malloc_atomic (n + 1, what);
  memcpy (dst, str, n);
  dst[n] = 0;
  return dst;
}

char *
scm_gc_strdup (const char *str, const char *what)
{
  return scm_gc_strndup (str, strlen (str), what);
}
