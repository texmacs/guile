#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  jit_arg_abi_t abi[] = { JIT_ARG_ABI_INT32 };
  jit_arg_t args[1];
  jit_receive(j, 1, abi, args);
  ASSERT(args[0].kind == JIT_ARG_LOC_GPR);
  jit_addi(j, JIT_R0, args[0].loc.gpr, 69);
  jit_retr(j, JIT_R0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  int (*f)(int) = ret;
  ASSERT(f(42) == 111);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
