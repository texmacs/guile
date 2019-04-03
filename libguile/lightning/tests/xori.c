#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_INTMAX };
  jit_arg_t args[1];
  const jit_anyreg_t regs[] = { { .gpr=JIT_R0 } };

  jit_receive(j, 1, abi, args);
  jit_load_args(j, 1, abi, args, regs);

  jit_xori(j, JIT_R0, JIT_R0, 1);
  jit_retr(j, JIT_R0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  intmax_t (*f)(intmax_t) = ret;

  ASSERT(f(0x7fffffff) == 0x7ffffffe);
  ASSERT(f(0x80000000) == 0x80000001);
#if __WORDSIZE == 64
  ASSERT(f(0x7fffffffffffffff) == 0x7ffffffffffffffe);
  ASSERT(f(0x8000000000000000) == 0x8000000000000001);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
