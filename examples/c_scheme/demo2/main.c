#include <stdio.h>
#include "libguile.h"
int main() {
    SCM func;
    SCM ret_val;
    int sqr_result;
    scm_init_guile();
    scm_c_primitive_load("script.scm");
    func = scm_variable_ref(scm_c_lookup("square"));
    ret_val = scm_call_1(func, scm_int2num(7));
    sqr_result = scm_num2int(ret_val, 0, NULL);
    printf("result of square is %d\n", sqr_result);
    scm_c_define("sc_arg", scm_int2num(9));
    func = scm_variable_ref(scm_c_lookup("square2"));
    ret_val = scm_call_0(func);
    sqr_result = scm_num2int(ret_val, 0, NULL);
    printf("result of square2 is %d\n", sqr_result);
    return 0;
}