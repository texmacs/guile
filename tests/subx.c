#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_INTMAX, JIT_ARG_ABI_INTMAX };
  jit_arg_t args[2];
  const jit_anyreg_t regs[] = { { .gpr=JIT_R0 }, { .gpr=JIT_R1 }};

  jit_receive(j, 2, abi, args);
  jit_load_args(j, 2, abi, args, regs);
  jit_movi(j, JIT_R2, 0);
  jit_subcr(j, JIT_R0, JIT_R0, JIT_R1);
  jit_subxi(j, JIT_R2, JIT_R2, 0);
  jit_retr(j, JIT_R2);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  intmax_t (*f)(intmax_t, intmax_t) = ret;

  ASSERT(f(0, 0) == 0);

#if __WORDSIZE == 32
  /* carry */
  ASSERT(f(0x7fffffff, 0xffffffff) == 0xffffffff);
  /* overflow */
  ASSERT(f(0x80000000, 1) == 0);
  /* carry */
  ASSERT(f(0x7fffffff, 0x80000000) == 0xffffffff);
  /* overflow */
  ASSERT(f(0x80000000, 0x7fffffff) == 0);
  /* carry+overflow */
  ASSERT(f(1, 0x80000000) == 0xffffffff);
#else
  /* carry */
  ASSERT(f(0x7fffffff, 0xffffffff) == -1);
  /* nothing */
  ASSERT(f(0x80000000, 1) == 0);
  /* carry */
  ASSERT(f(0x7fffffff, 0x80000000) == -1);
  /* nothing */
  ASSERT(f(0x80000000, 0x7fffffff) == 0);
  /* carry */
  ASSERT(f(1, 0x80000000) == -1);
  /* carry */
  ASSERT(f(0x7fffffffffffffff, 0xffffffffffffffff) == -1);
  /* overflow */
  ASSERT(f(0x8000000000000000, 1) == 0);
  /* carry */
  ASSERT(f(0x7fffffffffffffff, 0x8000000000000000) == -1);
  /* overflow */
  ASSERT(f(0x8000000000000000, 0x7fffffffffffffff) == 0);
  /* carry+overflow */
  ASSERT(f(1, 0x8000000000000000) == -1);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
