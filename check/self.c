#include <lightning.h>
#include <stdio.h>
#include <assert.h>

int
main(int argc, char *argv[])
{
    /* Same JIT_XY are not constants */
    init_jit(argv[0]);

    assert(JIT_R0 == jit_r(0));
    assert(JIT_R1 == jit_r(1));
    assert(JIT_R2 == jit_r(2));
#if defined(JIT_R3)
    assert(JIT_R3 == jit_r(3));
#  if defined(JIT_R4)
    assert(JIT_R4 == jit_r(4));
#    if defined(JIT_R5)
    assert(JIT_R5 == jit_r(5));
#      if defined(JIT_R6)
    assert(JIT_R6 == jit_r(6));
#        if defined(JIT_R7)
    assert(JIT_R7 == jit_r(7));
#        endif
#      endif
#    endif
#  endif
#endif
    assert(JIT_V0 == jit_v(0));
    assert(JIT_V1 == jit_v(1));
    assert(JIT_V2 == jit_v(2));
#if defined(JIT_V3)
    assert(JIT_V3 == jit_v(3));
#  if defined(JIT_V4)
    assert(JIT_V4 == jit_v(4));
#    if defined(JIT_V5)
    assert(JIT_V5 == jit_v(5));
#      if defined(JIT_V6)
    assert(JIT_V6 == jit_v(6));
#        if defined(JIT_V7)
    assert(JIT_V7 == jit_v(7));
#          if defined(JIT_V8)
    assert(JIT_V8 == jit_v(8));
#            if defined(JIT_V9)
    assert(JIT_V9 == jit_v(9));
#            endif
#          endif
#        endif
#      endif
#    endif
#  endif
#endif
    assert(JIT_F0 == jit_f(0));
    assert(JIT_F1 == jit_f(1));
    assert(JIT_F2 == jit_f(2));
    assert(JIT_F3 == jit_f(3));
    assert(JIT_F4 == jit_f(4));
    assert(JIT_F5 == jit_f(5));
#if defined(JIT_F6)
    assert(JIT_F6 == jit_f(6));
#  if defined(JIT_F7)
    assert(JIT_F7 == jit_f(7));
#  endif
#endif

    finish_jit();

    return (0);
}
