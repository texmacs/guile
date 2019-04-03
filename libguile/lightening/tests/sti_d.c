#include "test.h"

static double data[] = { -1.0, 0.0, 0.5 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_DOUBLE };
  jit_arg_t args[1];
  const jit_anyreg_t regs[] = { { .fpr=JIT_F0 } };

  jit_receive(j, 1, abi, args);
  jit_load_args(j, 1, abi, args, regs);

  jit_sti_d(j, &data[1], JIT_F0);
  jit_ret(j);

  void (*f)(double) = jit_end(j, NULL);

  ASSERT(data[0] == -1.0);
  ASSERT(data[1] == 0.0);
  ASSERT(data[2] == 0.5);
  f(42.5);
  ASSERT(data[0] == -1.0);
  ASSERT(data[1] == 42.5);
  ASSERT(data[2] == 0.5);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
