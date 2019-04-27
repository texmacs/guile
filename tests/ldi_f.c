#include "test.h"

static float data = -1.5f;

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);

  jit_ldi_f(j, JIT_F0, &data);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr_f(j, JIT_F0);

  float (*f)(void) = jit_end(j, NULL);

  ASSERT(f() == data);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
