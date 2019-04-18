#include "test.h"

static double data[] = { -1.0, 0.0, 0.5 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  jit_load_args_2(j, jit_operand_gpr (JIT_OPERAND_ABI_INTMAX, JIT_R2),
                  jit_operand_fpr (JIT_OPERAND_ABI_DOUBLE, JIT_F0));

  jit_stxi_d(j, (uintptr_t)data, JIT_R2, JIT_F0);
  jit_ret(j);

  void (*f)(intmax_t, double) = jit_end(j, NULL);

  ASSERT(data[0] == -1.0);
  ASSERT(data[1] == 0.0);
  ASSERT(data[2] == 0.5);
  f(8, 42.5);
  ASSERT(data[0] == -1.0);
  ASSERT(data[1] == 42.5);
  ASSERT(data[2] == 0.5);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
