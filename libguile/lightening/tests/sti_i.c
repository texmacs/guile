#include "test.h"

static uint32_t data[] = { 0x12121212, 0x00000000, 0x34343434 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_INT32, JIT_R1));

  jit_sti_i(j, &data[1], JIT_R1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_ret(j);

  void (*f)(int32_t) = jit_end(j, NULL);

  ASSERT(data[0] == 0x12121212);
  ASSERT(data[1] == 0x00);
  ASSERT(data[2] == 0x34343434);
  f(-1);
  ASSERT(data[0] == 0x12121212);
  ASSERT(data[1] == 0xffffffff);
  ASSERT(data[2] == 0x34343434);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
