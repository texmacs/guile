#include "test.h"

static uint32_t data = 0xffffffff;

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  jit_ldi_i(j, JIT_R0, &data);
  jit_retr(j, JIT_R0);

  uintmax_t (*f)(void) = jit_end(j, NULL);

  ASSERT(f() == -1);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
