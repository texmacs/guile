#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_2(j, jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R0),
                  jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R1));

  jit_rshr_u(j, JIT_R0, JIT_R0, JIT_R1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  jit_word_t (*f)(jit_word_t, jit_word_t) = ret;

  ASSERT(f(0xfe, 1) == 0x7f);
  ASSERT(f(0x1fffc, 2) == 0x7fff);
  ASSERT(f(0x80000000, 31) == 1);
  ASSERT(f(0x40000000, 30) == 1);
  ASSERT(f(0x20000000, 29) == 1);
  ASSERT(f(0x10000000, 28) == 1);
  ASSERT(f(0x810000, 16) == 0x81);
  ASSERT(f(0x20000, 17) == 1);
  ASSERT(f(0x40000, 18) == 1);
  ASSERT(f(0x7f8000, 15) == 0xff);
  ASSERT(f(0x1000000, 24) == 1);
  ASSERT(f(0xffffff00, 8) == 0xffffff);
  ASSERT(f(0x7fffffff, 0) == 0x7fffffff);
#if __WORDSIZE == 32
  ASSERT(f(0xfffffff8, 3) == 0x1fffffff);
#else
  ASSERT(f(0x3fffffff8, 3) == 0x7fffffff);
  ASSERT(f(0xffffffc080000000, 31) == 0x1ffffff81);
  ASSERT(f(0xfe00000000, 33) == 0x7f);
  ASSERT(f(0x1ffffc00000000, 34) == 0x7ffff);
  ASSERT(f(0xfffffff800000000, 29) == 0x7ffffffc0);
  ASSERT(f(0x8000000000000000, 63) == 1);
  ASSERT(f(0x4000000000000000, 62) == 1);
  ASSERT(f(0x2000000000000000, 61) == 1);
  ASSERT(f(0x1000000000000000, 60) == 1);
  ASSERT(f(0x81000000000000, 48) == 0x81);
  ASSERT(f(0x2000000000000, 49) == 1);
  ASSERT(f(0x10000000000, 40) == 1);
  ASSERT(f(0x7f800000000000, 47) == 0xff);
  ASSERT(f(0x100000000000000, 56) == 1);
  ASSERT(f(0xffffff0000000000, 40) == 0xffffff);
  ASSERT(f(0xfffffffe00000000, 33) == 0x7fffffff);
  ASSERT(f(0x8000000000000001, 63) == 1);
  ASSERT(f(0x1000000000000, 48) == 1);
  ASSERT(f(0xffff800000000000, 47) == 0x1ffff);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
