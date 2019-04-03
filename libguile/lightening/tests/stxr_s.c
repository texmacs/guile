#include "test.h"

static uint16_t data[] = { 0x1212, 0x0000, 0x3434 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] =
    { JIT_ARG_ABI_POINTER, JIT_ARG_ABI_INTMAX, JIT_ARG_ABI_INT16 };
  jit_arg_t args[3];
  const jit_anyreg_t regs[] =
    { { .gpr=JIT_R0 }, { .gpr=JIT_R2 }, { .gpr=JIT_R1 } };

  jit_receive(j, 3, abi, args);
  jit_load_args(j, 3, abi, args, regs);

  jit_stxr_s(j, JIT_R0, JIT_R2, JIT_R1);
  jit_ret(j);

  void (*f)(void*, intmax_t, int16_t) = jit_end(j, NULL);

  ASSERT(data[0] == 0x1212);
  ASSERT(data[1] == 0);
  ASSERT(data[2] == 0x3434);
  f(data, 2, -1);
  ASSERT(data[0] == 0x1212);
  ASSERT(data[1] == 0xffff);
  ASSERT(data[2] == 0x3434);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
