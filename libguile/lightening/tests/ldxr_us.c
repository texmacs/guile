#include "test.h"

static uint16_t data[] = { 0xffff, 0x0000, 0x4242 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_INTMAX };
  jit_arg_t args[2];
  const jit_anyreg_t regs[] = { { .gpr=JIT_R0 }, { .gpr=JIT_R1 } };

  jit_receive(j, 2, abi, args);
  jit_load_args(j, 2, abi, args, regs);

  jit_ldxr_us(j, JIT_R0, JIT_R0, JIT_R1);
  jit_retr(j, JIT_R0);

  uintmax_t (*f)(void*, uintmax_t) = jit_end(j, NULL);

  ASSERT(f(data, 0) == data[0]);
  ASSERT(f(data, 2) == data[1]);
  ASSERT(f(data, 4) == data[2]);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
