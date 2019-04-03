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

  jit_negr_f(j, JIT_F0, JIT_F0);
  jit_retr_f(j, JIT_F0);

  float (*f)(float) = jit_end(j, NULL);

  ASSERT(f(0.0f) == -0.0f);
  ASSERT(f(0.5f) == -0.5f);
  ASSERT(f(1.0f / 0.0f) == -1.0f / 0.0f);
  ASSERT(f(-1.25f) == 1.25f);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
