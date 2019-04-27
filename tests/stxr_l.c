#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
#if __WORDSIZE > 32
  static uint64_t data[] = { 0x1212121212121212, 0, 0x3434343434343434 };

  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_3(j, jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0),
                  jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R2),
                  jit_operand_gpr (JIT_OPERAND_ABI_INT64, JIT_R1));

  jit_stxr_l(j, JIT_R0, JIT_R2, JIT_R1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_ret(j);

  void (*f)(void*, jit_word_t, int64_t) = jit_end(j, NULL);

  ASSERT(data[0] == 0x1212121212121212);
  ASSERT(data[1] == 0x00);
  ASSERT(data[2] == 0x3434343434343434);
  f(data, 8, -1);
  ASSERT(data[0] == 0x1212121212121212);
  ASSERT(data[1] == 0xffffffffffffffff);
  ASSERT(data[2] == 0x3434343434343434);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
