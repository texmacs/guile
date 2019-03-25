#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  jit_arg_abi_t abi[] = { JIT_ARG_ABI_DOUBLE, JIT_ARG_ABI_DOUBLE };
  jit_arg_t args[2];
  jit_receive(j, 2, abi, args);
  ASSERT(args[0].kind == JIT_ARG_LOC_FPR);
  ASSERT(args[1].kind == JIT_ARG_LOC_FPR);
  jit_addr_d(j, JIT_F0, args[0].loc.fpr, args[1].loc.fpr);
  jit_retr_d(j, JIT_F0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  double (*f)(double, double) = ret;
  ASSERT(f(42., 69.) == 111.);
  ASSERT(f(42.5, 69.5) == 112.);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
