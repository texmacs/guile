#include "test.h"

static void
maybe_save(jit_state_t *j, jit_gpr_t reg)
{
  if (jit_gpr_is_callee_save (j, reg))
    jit_pushr(j, reg);
}    

static void
maybe_restore(jit_state_t *j, jit_gpr_t reg)
{
  if (jit_gpr_is_callee_save (j, reg))
    jit_popr(j, reg);
}    

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = {
    JIT_ARG_ABI_POINTER, JIT_ARG_ABI_POINTER,
    JIT_ARG_ABI_INTMAX, JIT_ARG_ABI_INTMAX
  };
  jit_arg_t args[4];
  const jit_anyreg_t regs[] = {
    { .gpr=JIT_R0 }, { .gpr=JIT_R1 },
    { .gpr=JIT_R2 }, { .gpr=JIT_V0 }
  };

  maybe_save(j, JIT_V0);
  maybe_save(j, JIT_V1);
  maybe_save(j, JIT_V2);

  jit_receive(j, 4, abi, args);
  jit_load_args(j, 4, abi, args, regs);

  jit_qmulr_u(j, JIT_V1, JIT_V2, JIT_R2, JIT_V0);
  jit_str(j, JIT_R0, JIT_V1);
  jit_str(j, JIT_R1, JIT_V2);

  maybe_restore(j, JIT_V2);
  maybe_restore(j, JIT_V1);
  maybe_restore(j, JIT_V0);

  jit_ret(j);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  void (*f)(intmax_t*, intmax_t*, intmax_t, intmax_t) = ret;

#define UQMUL(a, b, c, d) \
  do { \
    intmax_t C = 0, D = 0; f(&C, &D, a, b); ASSERT(C == c); ASSERT(D == d); \
  } while (0)
  
#if __WORDSIZE == 32
  UQMUL(0xffffff, 0xffffff, 0xfe000001, 0xffff);
#else
  UQMUL(0xffffffffff, 0xffffffffff, 0xfffffe0000000001, 0xffff);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
