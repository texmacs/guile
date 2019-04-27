#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_2(j, jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R0),
                  jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R1));

  jit_remr(j, JIT_R0, JIT_R0, JIT_R1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  jit_word_t (*f)(jit_word_t, jit_word_t) = ret;

  ASSERT(f(0x7fffffff, 1) == 0);
  ASSERT(f(1, 0x7fffffff) == 1);
  ASSERT(f(0x80000000, 1) == 0);
  ASSERT(f(1, 0x80000000) == 1);
  ASSERT(f(0x7fffffff, 2) == 1);
  ASSERT(f(2, 0x7fffffff) == 2);
  ASSERT(f(0x80000000, 2) == 0);
  ASSERT(f(2, 0x80000000) == 2);
  ASSERT(f(0x7fffffff, 0x80000000) == 0x7fffffff);
  ASSERT(f(0, 0x7fffffff) == 0);
  ASSERT(f(0xffffffff, 0xffffffff) == 0);

#if __WORDSIZE == 32
  ASSERT(f(0x80000000, 0x7fffffff) == 0xffffffff);
  ASSERT(f(0x7fffffff, 0xffffffff) == 0);
  ASSERT(f(0xffffffff, 0x7fffffff) == 0xffffffff);
#else
  ASSERT(f(0x80000000, 0x7fffffff) == 1);
  ASSERT(f(0x7fffffff, 0xffffffff) == 0x7fffffff);
  ASSERT(f(0xffffffff, 0x7fffffff) == 1);
  ASSERT(f(0x7fffffffffffffff, 1) == 0);
  ASSERT(f(1, 0x7fffffffffffffff) == 1);
  ASSERT(f(0x8000000000000000, 1) == 0);
  ASSERT(f(1, 0x8000000000000000) == 1);
  ASSERT(f(0x7fffffffffffffff, 2) == 1);
  ASSERT(f(2, 0x7fffffffffffffff) == 2);
  ASSERT(f(0x8000000000000000, 2) == 0);
  ASSERT(f(2, 0x8000000000000000) == 2);
  ASSERT(f(0x7fffffffffffffff, 0x8000000000000000) == 0x7fffffffffffffff);
  ASSERT(f(0x8000000000000000, 0x7fffffffffffffff) == 0xffffffffffffffff);
  ASSERT(f(0x7fffffffffffffff, 0xffffffffffffffff) == 0);
  ASSERT(f(0xffffffffffffffff, 0x7fffffffffffffff) == 0xffffffffffffffff);
  ASSERT(f(0xffffffffffffffff, 0xffffffffffffffff) == 0);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
