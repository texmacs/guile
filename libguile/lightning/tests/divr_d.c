#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_DOUBLE, JIT_ARG_ABI_DOUBLE };
  jit_arg_t args[2];
  const jit_anyreg_t regs[] = { { .fpr=JIT_F0 }, { .fpr=JIT_F1 }};

  jit_receive(j, 2, abi, args);
  jit_load_args(j, 2, abi, args, regs);

  jit_divr_d(j, JIT_F0, JIT_F0, JIT_F1);
  jit_retr_d(j, JIT_F0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  double (*f)(double, double) = ret;
  ASSERT(f(-0.5f, 0.5f) == -1.0f);
  ASSERT(f(1.25f, 0.5f) == 2.5f);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
