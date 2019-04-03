#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_INTMAX };
  jit_arg_t args[1];
  const jit_anyreg_t regs[] = { { .gpr=JIT_R0 } };

  jit_receive(j, 1, abi, args);
  jit_load_args(j, 1, abi, args, regs);

  jit_reloc_t r = jit_bnei(j, JIT_R0, 0);
  jit_reti(j, 0);
  jit_patch_here(j, r);
  jit_reti(j, 1);

  intmax_t (*f)(intmax_t) = jit_end(j, NULL);

  ASSERT(f(0) == 0);
  ASSERT(f(1) == 1);
  ASSERT(f(-1) == 1);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
