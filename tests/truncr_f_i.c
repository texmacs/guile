#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_FLOAT };
  jit_arg_t args[1];
  const jit_anyreg_t regs[] = { { .fpr=JIT_F0 } };

  jit_receive(j, 1, abi, args);
  jit_load_args(j, 1, abi, args, regs);

  jit_truncr_f_i(j, JIT_R0, JIT_F0);
  jit_retr(j, JIT_R0);

  int (*f)(float) = jit_end(j, NULL);

  ASSERT(f(0.0) == 0);
  ASSERT(f(-0.0) == 0);
  ASSERT(f(0.5) == 0);
  ASSERT(f(-0.5) == 0);
  ASSERT(f(1.5) == 1);
  ASSERT(f(-1.5) == -1);
  ASSERT(f(2.5) == 2);
  ASSERT(f(-2.5) == -2);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
