#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 3, 0, 0);

  jit_operand_t args[] =
    { jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0),
      jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R1),
      jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R2),
      jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_V0) };
  jit_load_args(j, 4, args);

  jit_qdivr(j, JIT_V1, JIT_V2, JIT_R2, JIT_V0);
  jit_str(j, JIT_R0, JIT_V1);
  jit_str(j, JIT_R1, JIT_V2);

  jit_leave_jit_abi(j, 3, 0, align);

  jit_ret(j);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  void (*f)(jit_word_t*, jit_word_t*, jit_word_t, jit_word_t) = ret;

#define QDIV(a, b, c, d) \
  do { \
    jit_word_t C = 0, D = 0; f(&C, &D, a, b); ASSERT(C == c); ASSERT(D == d); \
  } while (0)
  
  QDIV(10, 3, 3, 1);
  QDIV(-33, 9, -3, -6);
  QDIV(-41, -7, 5, -6);
  QDIV(65536, 4096, 16, 0);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
