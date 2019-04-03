#include "test.h"

static uint8_t data[] = { 0xff, 0x00, 0x42 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER };
  jit_arg_t args[1];
  const jit_anyreg_t regs[] = { { .gpr=JIT_R1 } };

  jit_receive(j, 1, abi, args);
  jit_load_args(j, 1, abi, args, regs);

  jit_ldr_c(j, JIT_R0, JIT_R1);
  jit_retr(j, JIT_R0);

  uintmax_t (*f)(void*) = jit_end(j, NULL);

  ASSERT(f(&data[0]) == -1);
  ASSERT(f(&data[1]) == 0);
  ASSERT(f(&data[2]) == 0x42);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
