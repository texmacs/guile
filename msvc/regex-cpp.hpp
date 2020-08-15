//
// Created by pikachu on 8/12/2020.
//

#ifndef GUILE_REGEX_CPP_H
#define GUILE_REGEX_CPP_H

#ifdef __cplusplus
extern "C" {
#endif
#include "libguile/__scm.h"
extern scm_t_bits scm_tc16_regex;
#define SCM_RGX(X)	((regex_t *) SCM_CELL_WORD_1 (X))
#define SCM_RGXP(X)	(SCM_NIMP (X) && (SCM_CELL_TYPE (X) == scm_tc16_regex))

extern SCM scm_make_regexp (SCM pat, SCM flags);
SCM scm_regexp_p (SCM x);
extern SCM scm_regexp_exec (SCM rx, SCM str, SCM start, SCM flags);
extern void scm_init_regex_posix (void);
#ifdef __cplusplus
}
#endif
#endif //GUILE_REGEX_CPP_H
