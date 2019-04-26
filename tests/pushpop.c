#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  const jit_gpr_t gpr[] = { JIT_R0, JIT_R1, JIT_R2, JIT_V0, JIT_V1, JIT_V2 };
  const jit_fpr_t fpr[] = { JIT_F0, JIT_F1, JIT_F2 };

  jit_begin(j, arena_base, arena_size);

  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R0));

  for (int i=0; i<6; i++)
    jit_pushr(j, gpr[i]);
  for (int i=0; i<3; i++)
    jit_pushr_d(j, fpr[i]);

  // Stomple registers.
  for (int i=0; i<6; i++)
    jit_movi(j, gpr[i], 0xcabba9e5);
  for (int i=0; i<3; i++)
    jit_extr_d(j, fpr[i], gpr[i]);

  for (int i=2; i>=0; i--)
    jit_popr_d(j, fpr[i]);
  for (int i=5; i>=0; i--)
    jit_popr(j, gpr[i]);

  jit_retr(j, JIT_R0);

  jit_word_t (*f)(jit_word_t) = jit_end(j, NULL);
  ASSERT(f(42) == 42);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
