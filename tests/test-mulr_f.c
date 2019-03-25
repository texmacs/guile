#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_FLOAT, JIT_ARG_ABI_FLOAT };
  jit_arg_t args[2];
  const jit_anyreg_t regs[] = { { .gpr=JIT_F0 }, { .gpr=JIT_F1 }};

  jit_receive(j, 2, abi, args);
  jit_load_args(j, 2, abi, args, regs);

  jit_mulr_f(j, JIT_F0, JIT_F0, JIT_F1);
  jit_retr_f(j, JIT_F0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  float (*f)(float, float) = ret;
  ASSERT(f(-0.5f, 0.5f) == -0.25f);
  ASSERT(f(0.25f, 0.75f) == 0.1875f);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
