#include "test.h"

static const jit_word_t overflowed = 0xcabba9e5;

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_2(j, jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R0),
                  jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R1));

  jit_reloc_t r = jit_boaddr_u(j, JIT_R0, JIT_R1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);
  jit_patch_here(j, r);
  jit_movi(j, JIT_R0, overflowed);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);

  jit_word_t (*f)(jit_word_t, jit_word_t) = jit_end(j, NULL);

  ASSERT(f(0, 0) == 0);
  ASSERT(f(1, 1) == 2);

#if __WORDSIZE == 32
  ASSERT(f(0xffffffff, 0xffffffff) == overflowed);
  ASSERT(f(0x7fffffff, 0) == 0x7fffffff);
  ASSERT(f(0x7fffffff, 1) == 0x80000000);
  ASSERT(f(0x7fffffff, 0x7fffffff) == 0x7fffffffu + 0x7fffffffu);
  ASSERT(f(0x7fffffff, 0x80000000) == 0xffffffff);
  ASSERT(f(0x80000000, 0x80000000) == overflowed);
#else
  ASSERT(f(0xffffffff, 0xffffffff) == 0xffffffffull + 0xffffffffull);
  ASSERT(f(0x7fffffff, 1) == 0x80000000);
  ASSERT(f(0x7fffffff, 0x7fffffff) == 0x7fffffffull + 0x7fffffffull);
  ASSERT(f(0x7fffffff, 0x80000000) == 0xffffffff);
  ASSERT(f(0x80000000, 0x80000000) == 0x100000000);
  ASSERT(f(0xffffffffffffffff, 0xffffffffffffffff) == overflowed);
  ASSERT(f(0x7fffffffffffffff, 1) == 0x8000000000000000);
  ASSERT(f(0x7fffffffffffffff, 0x7fffffffffffffff) == -2);
  ASSERT(f(0x7fffffffffffffff, 0x8000000000000000) == -1);
  ASSERT(f(0x8000000000000000, 0x8000000000000000) == overflowed);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
