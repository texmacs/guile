#include "test.h"

static const intmax_t overflowed = 0xcabba9e5;

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_INTMAX };
  jit_arg_t args[1];
  const jit_anyreg_t regs[] = { { .gpr=JIT_R0 } };

  jit_receive(j, 1, abi, args);
  jit_load_args(j, 1, abi, args, regs);
  jit_reloc_t r = jit_bxaddi(j, JIT_R0, 1);
  jit_movi(j, JIT_R0, overflowed);
  jit_patch_here(j, r);
  jit_retr(j, JIT_R0);

  intmax_t (*f)(intmax_t) = jit_end(j, NULL);

  ASSERT(f(-1) == 0);
  ASSERT(f(0) == 1);
  ASSERT(f(1) == 2);

#if __WORDSIZE == 32
  ASSERT(f(0x7fffffff) == overflowed);
  ASSERT(f(0x80000000) == 0x80000001);
  ASSERT(f(0xffffffff) == 0);
#else
  ASSERT(f(0x7fffffffffffffff) == overflowed);
  ASSERT(f(0x8000000000000000) == 0x8000000000000001);
  ASSERT(f(0xffffffffffffffff) == 0);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
