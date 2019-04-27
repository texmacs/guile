#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R0));

  jit_addi(j, JIT_R0, JIT_R0, 69);
  jit_leave_jit_abi(j, 0, 0, align);
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
