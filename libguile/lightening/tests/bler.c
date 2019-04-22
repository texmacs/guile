#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  jit_load_args_2(j, jit_operand_gpr (JIT_OPERAND_ABI_INTMAX, JIT_R0),
                  jit_operand_gpr (JIT_OPERAND_ABI_INTMAX, JIT_R1));

  jit_reloc_t r = jit_bler(j, JIT_R0, JIT_R1);
  jit_reti(j, 0);
  jit_patch_here(j, r);
  jit_reti(j, 1);

  intmax_t (*f)(intmax_t, intmax_t) = jit_end(j, NULL);

  ASSERT(f(0, 0) == 1);
  ASSERT(f(0, 1) == 1);
  ASSERT(f(1, 0) == 0);
  ASSERT(f(-1, 0) == 1);
  ASSERT(f(0, -1) == 0);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
