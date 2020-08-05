#include <stdio.h>
#include "libguile.h"
SCM c_square(SCM arg) {
    int c_arg = scm_num2int(arg, 0, NULL);
    return scm_from_int(c_arg * c_arg);
}
int main() {
    SCM func;
    scm_init_guile();
    scm_c_define_gsubr("c_square", 1, 0, 0, c_square);
    scm_c_primitive_load("script.scm");
    func = scm_variable_ref(scm_c_lookup("main-script"));
    scm_call_0(func);
    return 0;
}