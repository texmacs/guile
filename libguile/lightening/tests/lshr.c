#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_2(j, jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R0),
                  jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R1));

  jit_lshr(j, JIT_R0, JIT_R0, JIT_R1);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  jit_word_t (*f)(jit_word_t, jit_word_t) = ret;

  ASSERT(f(0x7f, 1) == 0xfe);
  ASSERT(f(0x7fff, 2) == 0x1fffc);
  ASSERT(f(0x81, 16) == 0x810000);
  ASSERT(f(0xff, 15) == 0x7f8000);
  ASSERT(f(0x7fffffff, 0) == 0x7fffffff);
#if __WORDSIZE == 32
  ASSERT(f(0xffffffff, 8) == 0xffffff00);
  ASSERT(f(0x7fffffff, 3) == 0xfffffff8);
  ASSERT(f(-0x7f, 31) == 0x80000000);
  ASSERT(f(-0x7fff, 30) == 0x40000000);
  ASSERT(f(-0x7fffffff, 29) == 0x20000000);
  ASSERT(f(0x80000001, 28) == 0x10000000);
  ASSERT(f(0x8001, 17) == 0x20000);
  ASSERT(f(0x80000001, 18) == 0x40000);
  ASSERT(f(-0xffff, 24) == 0x1000000);
#else
  ASSERT(f(0xffffffff, 8) == 0xffffffff00);
  ASSERT(f(0x7fffffff, 3) == 0x3fffffff8);
  ASSERT(f(-0x7f, 31) == 0xffffffc080000000);
  ASSERT(f(-0x7fff, 30) == 0xffffe00040000000);
  ASSERT(f(-0x7fffffff, 29) == 0xf000000020000000);
  ASSERT(f(0x80000001, 28) == 0x800000010000000);
  ASSERT(f(0x8001, 17) == 0x100020000);
  ASSERT(f(0x80000001, 18) == 0x2000000040000);
  ASSERT(f(-0xffff, 24) == 0xffffff0001000000);
  ASSERT(f(0x7f, 33) == 0xfe00000000);
  ASSERT(f(0x7ffff, 34) == 0x1ffffc00000000);
  ASSERT(f(0x7fffffff, 35) == 0xfffffff800000000);
  ASSERT(f(-0x7f, 63) == 0x8000000000000000);
  ASSERT(f(-0x7fff, 62) == 0x4000000000000000);
  ASSERT(f(-0x7fffffff, 61) == 0x2000000000000000);
  ASSERT(f(0x80000001, 60) == 0x1000000000000000);
  ASSERT(f(0x81, 48) == 0x81000000000000);
  ASSERT(f(0x8001, 49) == 0x2000000000000);
  ASSERT(f(0x80000001, 40) == 0x10000000000);
  ASSERT(f(0xff, 47) == 0x7f800000000000);
  ASSERT(f(0xffff0001, 56) == 0x100000000000000);
  ASSERT(f(0xffffffff, 40) == 0xffffff0000000000);
  ASSERT(f(0x7fffffffff, 33) == 0xfffffffe00000000);
  ASSERT(f(-0x7fffffffff, 63) == 0x8000000000000000);
  ASSERT(f(0x8000000001, 48) == 0x1000000000000);
  ASSERT(f(0xffffffffff, 47) == 0xffff800000000000);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
