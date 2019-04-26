#include "test.h"

static uint64_t data = 0xffffffffffffffff;

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
#if __WORDSIZE > 32
  jit_begin(j, arena_base, arena_size);

  jit_ldi_l(j, JIT_R0, &data);
  jit_retr(j, JIT_R0);

  jit_uword_t (*f)(void) = jit_end(j, NULL);

  ASSERT(f() == -1);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
