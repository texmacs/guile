#ifndef SCM_RANDOM_H
#define SCM_RANDOM_H

/* Copyright 1999-2001,2006,2008,2010,2018
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



#include <libguile/error.h>


/*
 * A plugin interface for RNGs
 *
 * Using this interface, it is possible for the application to tell
 * libguile to use a different RNG.  This is desirable if it is
 * necessary to use the same RNG everywhere in the application in
 * order to prevent interference, if the application uses RNG
 * hardware, or if the application has special demands on the RNG.
 *
 * Look how the default generator is "plugged in" in scm_init_random().
 */

typedef struct scm_t_rstate {
  struct scm_t_rng *rng;
  double normal_next; /* For scm_c_normal01 */
  /* Custom fields follow here */
} scm_t_rstate;

typedef struct scm_t_rng {
  size_t rstate_size;				    /* size of random state */
  uint32_t (*random_bits) (scm_t_rstate *state); /* gives 32 random bits */
  void (*init_rstate) (scm_t_rstate *state, const char *seed, int n);
  scm_t_rstate *(*copy_rstate) (scm_t_rstate *state);
  void (*from_datum) (scm_t_rstate *state, SCM datum);
  SCM (*to_datum) (scm_t_rstate *state);
} scm_t_rng;

SCM_API scm_t_rng scm_the_rng;


/*
 * Random number library functions
 */
SCM_API scm_t_rstate *scm_c_make_rstate (const char *, int);
SCM_API scm_t_rstate *scm_c_rstate_from_datum (SCM datum);
SCM_API scm_t_rstate *scm_c_default_rstate (void);
#define scm_c_uniform32(RSTATE) ((RSTATE)->rng->random_bits (RSTATE))
SCM_API double scm_c_uniform01 (scm_t_rstate *);
SCM_API double scm_c_normal01 (scm_t_rstate *);
SCM_API double scm_c_exp1 (scm_t_rstate *);
SCM_API uint32_t scm_c_random (scm_t_rstate *, uint32_t m);
SCM_API uint64_t scm_c_random64 (scm_t_rstate *state, uint64_t m);
SCM_API SCM scm_c_random_bignum (scm_t_rstate *, SCM m);


/*
 * Scheme level interface
 */
SCM_API scm_t_bits scm_tc16_rstate;
#define SCM_RSTATEP(obj) SCM_SMOB_PREDICATE (scm_tc16_rstate, obj)
#define SCM_RSTATE(obj)  ((scm_t_rstate *) SCM_SMOB_DATA (obj))

#define SCM_VALIDATE_RSTATE(pos, v) \
  SCM_MAKE_VALIDATE_MSG (pos, v, RSTATEP, "random-generator-state")

SCM_API unsigned char scm_masktab[256];

SCM_API SCM scm_var_random_state;
SCM_API SCM scm_random (SCM n, SCM state);
SCM_API SCM scm_copy_random_state (SCM state);
SCM_API SCM scm_seed_to_random_state (SCM seed);
SCM_API SCM scm_datum_to_random_state (SCM datum);
SCM_API SCM scm_random_state_to_datum (SCM state);
SCM_API SCM scm_random_state_from_platform (void);
SCM_API SCM scm_random_uniform (SCM state);
SCM_API SCM scm_random_solid_sphere_x (SCM v, SCM state);
SCM_API SCM scm_random_hollow_sphere_x (SCM v, SCM state);
SCM_API SCM scm_random_normal (SCM state);
SCM_API SCM scm_random_normal_vector_x (SCM v, SCM state);
SCM_API SCM scm_random_exp (SCM state);
SCM_INTERNAL void scm_init_random (void);

SCM_INTERNAL void scm_i_random_bytes_from_platform (unsigned char *buf, size_t len);

#endif  /* SCM_RANDOM_H */
