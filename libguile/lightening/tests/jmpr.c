#include "test.h"

static int tail(void) { return 42; }

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0));
  jit_leave_jit_abi(j, 0, 0, align);

  jit_jmpr(j, JIT_R0);

  int (*f)(void*) = jit_end(j, NULL);
  ASSERT(f(tail) == 42);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
