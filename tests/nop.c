#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);

  size_t total = 0;
  char *start = jit_address(j);
  for (size_t i = 1; i < 10; total += i, i++)
    jit_nop(j, i);
  char *end = jit_address(j);
  ASSERT(end - start == total);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_reti(j, 42);

  jit_word_t (*f)(void) = jit_end(j, NULL);
  ASSERT(f() == 42);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
