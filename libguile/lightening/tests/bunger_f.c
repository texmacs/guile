#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_2(j, jit_operand_fpr (JIT_OPERAND_ABI_FLOAT, JIT_F0),
                  jit_operand_fpr (JIT_OPERAND_ABI_FLOAT, JIT_F1));

  jit_reloc_t r = jit_bunger_f(j, JIT_F0, JIT_F1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_reti(j, 0);
  jit_patch_here(j, r);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_reti(j, 1);

  jit_word_t (*f)(float, float) = jit_end(j, NULL);

  ASSERT(f(0, 0) == 1);
  ASSERT(f(0, 1) == 0);
  ASSERT(f(1, 0) == 1);
  ASSERT(f(-1, 0) == 0);
  ASSERT(f(0, -1) == 1);

  ASSERT(f(0, 0.0/0.0) == 1);
  ASSERT(f(0.0/0.0, 0) == 1);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
