#include "test.h"

static int32_t f(int32_t a, int32_t b, int32_t c, int32_t d, int32_t e,
                 int32_t f, int32_t g, int32_t h, int32_t i, int32_t j) {
  ASSERT(a == 0);
  ASSERT(b == 1);
  ASSERT(c == 2);
  ASSERT(d == 3);
  ASSERT(e == 4);
  ASSERT(f == 5);
  ASSERT(g == 6);
  ASSERT(h == 7);
  ASSERT(i == 8);
  ASSERT(j == 9);
  return 42;
}

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0));

  jit_operand_t args[10] = {
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 0 * sizeof(int32_t)),
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 1 * sizeof(int32_t)),
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 2 * sizeof(int32_t)),
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 3 * sizeof(int32_t)),
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 4 * sizeof(int32_t)),
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 5 * sizeof(int32_t)),
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 6 * sizeof(int32_t)),
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 7 * sizeof(int32_t)),
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 8 * sizeof(int32_t)),
    jit_operand_mem(JIT_OPERAND_ABI_INT32, JIT_R0, 9 * sizeof(int32_t))
  };
  jit_calli(j, f, 10, args);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_ret(j);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  int32_t (*f)(int32_t*) = ret;

  int32_t iargs[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  ASSERT(f(iargs) == 42);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
