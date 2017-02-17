#ifndef SCM_CACHE_INTERNAL_H
#define SCM_CACHE_INTERNAL_H

/* Copyright (C) 2016
 * Free Software Foundation, Inc.
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




#include <string.h>

#include "libguile/__scm.h"
#include "libguile/gc.h"
#include "libguile/hash.h"
#include "libguile/threads.h"


/* A simple cache with 8 entries.  The cache entries are stored in a
   sorted vector.  */
struct scm_cache_entry
{
  scm_t_bits key;
  scm_t_bits value;
};

#define SCM_CACHE_SIZE 16

struct scm_cache
{
  scm_t_bits eviction_cookie;
  struct scm_cache_entry entries[SCM_CACHE_SIZE];
};

static inline struct scm_cache*
scm_make_cache (void)
{
  struct scm_cache *ret = scm_gc_typed_calloc (struct scm_cache);
  ret->eviction_cookie = (scm_t_bits) ret;
  return ret;
}

static inline int
scm_cache_full_p (struct scm_cache *cache)
{
  return cache->entries[0].key != 0;
}

static inline void
scm_cache_evict_1 (struct scm_cache *cache, struct scm_cache_entry *evicted)
{
  size_t idx;
  cache->eviction_cookie = scm_ihashq (SCM_PACK (cache->eviction_cookie), -1);
  idx = cache->eviction_cookie & (SCM_CACHE_SIZE - 1);
  memcpy (evicted, cache->entries + idx, sizeof (*evicted));
  memmove (cache->entries + 1,
           cache->entries,
           sizeof (cache->entries[0]) * idx);
  cache->entries[0].key = 0;
  cache->entries[0].value = 0;
}

static inline struct scm_cache_entry*
scm_cache_lookup (struct scm_cache *cache, SCM k)
{
  scm_t_bits k_bits = SCM_UNPACK (k);
  struct scm_cache_entry *entry = cache->entries;
  /* Unrolled binary search, compiled to branchless cmp + cmov chain.  */
  if (entry[8].key <= k_bits) entry += 8;
  if (entry[4].key <= k_bits) entry += 4;
  if (entry[2].key <= k_bits) entry += 2;
  if (entry[1].key <= k_bits) entry += 1;
  return entry;
}

static inline void
scm_cache_insert (struct scm_cache *cache, SCM k, SCM v,
                  struct scm_cache_entry *evicted)
{
  struct scm_cache_entry *entry;

  if (scm_cache_full_p (cache))
    scm_cache_evict_1 (cache, evicted);
  entry = scm_cache_lookup (cache, k);
  if (entry->key == SCM_UNPACK (k))
    {
      entry->value = SCM_UNPACK (v);
      return;
    }
  memmove (cache->entries,
           cache->entries + 1,
           (entry - cache->entries) * sizeof (*entry));
  entry->key = SCM_UNPACK (k);
  entry->value = SCM_UNPACK (v);
}

#endif /* SCM_CACHE_INTERNAL_H */
