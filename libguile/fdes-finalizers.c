/* Copyright (C) 2016  Free Software Foundation, Inc.
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
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/hashtab.h"
#include "libguile/numbers.h"
#include "libguile/fdes-finalizers.h"



/* Table of fdes finalizers and associated lock.  */
static scm_i_pthread_mutex_t fdes_finalizers_lock =
  SCM_I_PTHREAD_MUTEX_INITIALIZER;
static SCM fdes_finalizers;

SCM_DEFINE (scm_add_fdes_finalizer_x, "add-fdes-finalizer!", 2, 0, 0,
            (SCM fd, SCM finalizer),
	    "Add a finalizer that will be called when @var{fd} is closed.")
#define FUNC_NAME s_scm_add_fdes_finalizer_x
{
  SCM h;

  /* Check type.  */
  scm_to_uint (fd);

  scm_i_pthread_mutex_lock (&fdes_finalizers_lock);
  h = scm_hashv_create_handle_x (fdes_finalizers, fd, SCM_EOL);
  scm_set_cdr_x (h, scm_cons (finalizer, scm_cdr (h)));
  scm_i_pthread_mutex_unlock (&fdes_finalizers_lock);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_remove_fdes_finalizer_x, "remove-fdes-finalizer!", 2, 0, 0,
            (SCM fd, SCM finalizer),
	    "Remove a finalizer that was previously added to the file\n"
            "descriptor @var{fd}.")
#define FUNC_NAME s_scm_remove_fdes_finalizer_x
{
  SCM h;

  /* Check type.  */
  scm_to_uint (fd);

  scm_i_pthread_mutex_lock (&fdes_finalizers_lock);
  h = scm_hashv_get_handle (fdes_finalizers, fd);
  if (scm_is_true (h))
    scm_set_cdr_x (h, scm_delq1_x (finalizer, scm_cdr (h)));
  scm_i_pthread_mutex_unlock (&fdes_finalizers_lock);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

struct fdes_finalizer_data
{
  SCM finalizer;
  SCM fd;
};

static SCM
do_run_finalizer (void *data)
{
  struct fdes_finalizer_data *fdata = data;
  return scm_call_1 (fdata->finalizer, fdata->fd);
}

void
scm_run_fdes_finalizers (int fd)
{
  SCM finalizers;
  struct fdes_finalizer_data data;

  data.fd = scm_from_int (fd);

  scm_i_pthread_mutex_lock (&fdes_finalizers_lock);
  finalizers = scm_hashv_ref (fdes_finalizers, data.fd, SCM_EOL);
  if (!scm_is_null (finalizers))
    scm_hashv_remove_x (fdes_finalizers, data.fd);
  scm_i_pthread_mutex_unlock (&fdes_finalizers_lock);

  for (; !scm_is_null (finalizers); finalizers = scm_cdr (finalizers))
    {
      data.finalizer = scm_car (finalizers);
      scm_internal_catch (SCM_BOOL_T, do_run_finalizer, &data,
                          scm_handle_by_message_noexit, NULL);
    }
}




static void
scm_init_fdes_finalizers (void)
{
#include "libguile/fdes-finalizers.x"
}

void
scm_register_fdes_finalizers ()
{
  fdes_finalizers = scm_c_make_hash_table (0);

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_fdes_finalizers",
                            (scm_t_extension_init_func) scm_init_fdes_finalizers,
                            NULL);
}
