#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  const jit_gpr_t gpr[] = { JIT_R0, JIT_R1, JIT_R2, JIT_V0, JIT_V1, JIT_V2 };
  const jit_fpr_t fpr[] = { JIT_F0, JIT_F1, JIT_F2 };

  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 3, 0, 0);

  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R0));

  jit_pushr(j, JIT_R0);

  // Stomple registers.
  for (int i=0; i<6; i++)
    jit_movi(j, gpr[i], 0xcabba9e5);
  for (int i=0; i<3; i++)
    jit_extr_d(j, fpr[i], gpr[i]);

  jit_popr(j, JIT_R0);

  jit_leave_jit_abi(j, 3, 0, align);
  jit_retr(j, JIT_R0);

  jit_word_t (*f)(jit_word_t) = jit_end(j, NULL);
  ASSERT(f(42) == 42);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
