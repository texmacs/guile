/* Copyright 1995-1996,1998-2001,2003,2006,2008-2009,2011,2018
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

#include <stdio.h>

#include "gc.h"

#include "chooks.h"



/* C level hooks
 *
 */

/* Hint for `scm_gc_malloc ()' and friends.  */
static const char hook_entry_gc_hint[] = "hook entry";

void
scm_c_hook_init (scm_t_c_hook *hook, void *hook_data, scm_t_c_hook_type type)
{
  hook->first = 0;
  hook->type = type;
  hook->data = hook_data;
}

void
scm_c_hook_add (scm_t_c_hook *hook,
		scm_t_c_hook_function func,
		void *fn_data, 
		int appendp)
{
  scm_t_c_hook_entry *entry;
  scm_t_c_hook_entry **loc = &hook->first;

  entry = scm_gc_malloc (sizeof (scm_t_c_hook_entry), hook_entry_gc_hint);
  if (appendp)
    while (*loc)
      loc = &(*loc)->next;
  entry->next = *loc;
  entry->func = func;
  entry->data = fn_data;
  *loc = entry;
}

void
scm_c_hook_remove (scm_t_c_hook *hook,
		   scm_t_c_hook_function func,
		   void *fn_data)
{
  scm_t_c_hook_entry **loc = &hook->first;
  while (*loc)
    {
      if ((*loc)->func == func && (*loc)->data == fn_data)
	{
	  *loc = (*loc)->next;
	  return;
	}
      loc = &(*loc)->next;
    }
  fprintf (stderr, "Attempt to remove non-existent hook function\n");
  abort ();
}

void *
scm_c_hook_run (scm_t_c_hook *hook, void *data)
{
  scm_t_c_hook_entry *entry = hook->first;
  scm_t_c_hook_type type = hook->type;
  void *res = 0;
  while (entry)
    {
      res = (entry->func) (hook->data, entry->data, data);
      if (res)
	{
	  if (type == SCM_C_HOOK_OR)
	    break;
	}
      else
	{
	  if (type == SCM_C_HOOK_AND)
	    break;
	}
      entry = entry->next;
    }
  return res;
}
