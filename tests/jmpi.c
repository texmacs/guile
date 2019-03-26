#include "test.h"

static int tail(void) { return 42; }

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  jit_jmpi(j, tail);

  int (*f)(void) = jit_end(j, NULL);

  ASSERT(f() == 42);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
