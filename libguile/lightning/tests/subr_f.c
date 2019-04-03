#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  const jit_arg_abi_t abi[] = { JIT_ARG_ABI_FLOAT, JIT_ARG_ABI_FLOAT };
  jit_arg_t args[2];
  const jit_anyreg_t regs[] = { { .fpr=JIT_F0 }, { .fpr=JIT_F1 }};

  jit_receive(j, 2, abi, args);
  jit_load_args(j, 2, abi, args, regs);

  jit_subr_f(j, JIT_F0, JIT_F0, JIT_F1);
  jit_retr_f(j, JIT_F0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  float (*f)(float, float) = ret;
  ASSERT(f(42.f, 69.f) == -27.f);
  ASSERT(f(42.0f, 69.5f) == -27.5f);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
