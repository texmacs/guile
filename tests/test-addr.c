#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  jit_arg_abi_t abi[] = { JIT_ARG_ABI_INT32, JIT_ARG_ABI_INT32 };
  jit_arg_t args[2];
  jit_receive(j, 2, abi, args);
  ASSERT(args[0].kind == JIT_ARG_LOC_GPR);
  ASSERT(args[1].kind == JIT_ARG_LOC_GPR);
  jit_addr(j, JIT_R0, args[0].loc.gpr, args[1].loc.gpr);
  jit_retr(j, JIT_R0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  int (*f)(int, int) = ret;
  ASSERT(f(42, 69) == 111);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
