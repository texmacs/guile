#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  jit_load_args_1(j, jit_operand_fpr (JIT_OPERAND_ABI_DOUBLE, JIT_F0));

  jit_extr_d_f(j, JIT_F0, JIT_F0);
  jit_retr_f(j, JIT_F0);

  float (*f)(double) = jit_end(j, NULL);

  ASSERT(f(0.0) == 0.0f);
  ASSERT(f(0.5) == 0.5f);
  ASSERT(f(1.0 / 0.0) == 1.0f / 0.0f);
  ASSERT(f(1.25) == 1.25f);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
