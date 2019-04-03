#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_DOUBLE, JIT_ARG_ABI_DOUBLE };
  jit_arg_t args[2];
  const jit_anyreg_t regs[] = { { .fpr=JIT_F0 }, { .fpr=JIT_F1 } };

  jit_receive(j, 2, abi, args);
  jit_load_args(j, 2, abi, args, regs);

  jit_reloc_t r = jit_bgtr_d(j, JIT_F0, JIT_F1);
  jit_reti(j, 0);
  jit_patch_here(j, r);
  jit_reti(j, 1);

  intmax_t (*f)(double, double) = jit_end(j, NULL);

  ASSERT(f(0, 0) == 0);
  ASSERT(f(0, 1) == 0);
  ASSERT(f(1, 0) == 1);
  ASSERT(f(-1, 0) == 0);
  ASSERT(f(0, -1) == 1);

  ASSERT(f(0, 0.0/0.0) == 0);
  ASSERT(f(0.0/0.0, 0) == 0);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
