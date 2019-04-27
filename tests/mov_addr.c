#include "test.h"

static uint64_t thing = 0x123456789abcdef0;

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);

  jit_patch_there(j, jit_mov_addr(j, JIT_R0), &thing);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);

  void* (*f)(void) = jit_end(j, NULL);

  ASSERT(f() == &thing);
  ASSERT(*(uint64_t*)f() == thing);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
