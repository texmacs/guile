#include <stdio.h>
#include "libguile.h"
int main() {
    SCM func;
    scm_init_guile();
    scm_c_primitive_load("script.scm");
    func = scm_variable_ref(scm_c_lookup("simple-script"));
    scm_call_0(func);
    return 0;
}