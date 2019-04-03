#include "test.h"

static float data[] = { -1.0, 0.0, 0.5 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_POINTER };
  jit_arg_t args[1];
  const jit_anyreg_t regs[] = { { .gpr=JIT_R1 } };

  jit_receive(j, 1, abi, args);
  jit_load_args(j, 1, abi, args, regs);

  jit_ldr_f(j, JIT_F0, JIT_R1);
  jit_retr_f(j, JIT_F0);

  float (*f)(void*) = jit_end(j, NULL);

  ASSERT(f(&data[0]) == data[0]);
  ASSERT(f(&data[1]) == data[1]);
  ASSERT(f(&data[2]) == data[2]);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
