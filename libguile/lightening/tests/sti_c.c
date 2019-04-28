#include "test.h"

static uint8_t data[] = { 0x12, 0x00, 0x34 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_INT8, JIT_R1));

  jit_sti_c(j, &data[1], JIT_R1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_ret(j);

  void (*f)(int8_t) = jit_end(j, NULL);

  ASSERT(data[0] == 0x12);
  ASSERT(data[1] == 0x00);
  ASSERT(data[2] == 0x34);
  f(-1);
  ASSERT(data[0] == 0x12);
  ASSERT(data[1] == 0xff);
  ASSERT(data[2] == 0x34);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
