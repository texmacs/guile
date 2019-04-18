#include "test.h"

static uint16_t data[] = { 0xffff, 0x0000, 0x4242 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_INTMAX, JIT_R0));

  jit_ldxi_s(j, JIT_R0, JIT_R0, (uintptr_t)data);
  jit_retr(j, JIT_R0);

  uintmax_t (*f)(uintmax_t) = jit_end(j, NULL);

  ASSERT(f(0) == -1);
  ASSERT(f(2) == 0);
  ASSERT(f(4) == 0x4242);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
