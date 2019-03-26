#include "test.h"

static uint64_t data[] = { 0x1212121212121212, 0, 0x3434343434343434 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
#if __WORDSIZE > 32
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] =
    { JIT_ARG_ABI_INTMAX, JIT_ARG_ABI_INT64 };
  jit_arg_t args[2];
  const jit_anyreg_t regs[] =
    { { .gpr=JIT_R2 }, { .gpr=JIT_R1 } };

  jit_receive(j, 2, abi, args);
  jit_load_args(j, 2, abi, args, regs);

  jit_stxi_l(j, (uintptr_t)data, JIT_R2, JIT_R1);
  jit_ret(j);

  void (*f)(intmax_t, int64_t) = jit_end(j, NULL);

  ASSERT(data[0] == 0x1212121212121212);
  ASSERT(data[1] == 0x00);
  ASSERT(data[2] == 0x3434343434343434);
  f(8, -1);
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
