#include "test.h"

static uint16_t data[] = { 0x1212, 0x0000, 0x3434 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_2(j, jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0),
                  jit_operand_gpr (JIT_OPERAND_ABI_INT16, JIT_R1));

  jit_str_s(j, JIT_R0, JIT_R1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_ret(j);

  void (*f)(void*, int16_t) = jit_end(j, NULL);

  ASSERT(data[0] == 0x1212);
  ASSERT(data[1] == 0);
  ASSERT(data[2] == 0x3434);
  f(&data[1], -1);
  ASSERT(data[0] == 0x1212);
  ASSERT(data[1] == 0xffff);
  ASSERT(data[2] == 0x3434);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
