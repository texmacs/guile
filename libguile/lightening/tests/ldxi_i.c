#include "test.h"

static uint32_t data[] = { 0xffffffff, 0x00000000, 0x42424242 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0));

  jit_ldxi_i(j, JIT_R0, JIT_R0, (uintptr_t)data);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);

  jit_uword_t (*f)(jit_uword_t) = jit_end(j, NULL);

  ASSERT(f(0) == -1);
  ASSERT(f(4) == 0);
  ASSERT(f(8) == 0x42424242);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
