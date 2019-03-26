#include "test.h"

static uint16_t data[] = { 0x1212, 0x0000, 0x3434 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_INT16 };
  jit_arg_t args[1];
  const jit_anyreg_t regs[] = { { .gpr=JIT_R1 } };

  jit_receive(j, 1, abi, args);
  jit_load_args(j, 1, abi, args, regs);

  jit_sti_s(j, &data[1], JIT_R1);
  jit_ret(j);

  void (*f)(int16_t) = jit_end(j, NULL);

  ASSERT(data[0] == 0x1212);
  ASSERT(data[1] == 0);
  ASSERT(data[2] == 0x3434);
  f(-1);
  ASSERT(data[0] == 0x1212);
  ASSERT(data[1] == 0xffff);
  ASSERT(data[2] == 0x3434);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
