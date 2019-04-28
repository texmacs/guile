#include "test.h"

static float data[] = { -1.0, 0.0, 0.5 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_2(j, jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0),
                  jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R1));

  jit_ldxr_f(j, JIT_F0, JIT_R0, JIT_R1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr_f(j, JIT_F0);

  float (*f)(void*, jit_uword_t) = jit_end(j, NULL);

  ASSERT(f(data, 0) == data[0]);
  ASSERT(f(data, 4) == data[1]);
  ASSERT(f(data, 8) == data[2]);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
