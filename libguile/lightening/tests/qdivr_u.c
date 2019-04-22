#include "test.h"

static void
maybe_save(jit_state_t *j, jit_gpr_t reg)
{
  if (jit_gpr_is_callee_save (j, reg))
    jit_pushr(j, reg);
}    

static void
maybe_restore(jit_state_t *j, jit_gpr_t reg)
{
  if (jit_gpr_is_callee_save (j, reg))
    jit_popr(j, reg);
}    

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);

  maybe_save(j, JIT_V0);
  maybe_save(j, JIT_V1);
  maybe_save(j, JIT_V2);

  jit_operand_t args[] =
    { jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R0),
      jit_operand_gpr (JIT_OPERAND_ABI_POINTER, JIT_R1),
      jit_operand_gpr (JIT_OPERAND_ABI_INTMAX, JIT_R2),
      jit_operand_gpr (JIT_OPERAND_ABI_INTMAX, JIT_V0) };
  jit_load_args(j, 4, args);

  jit_qdivr_u(j, JIT_V1, JIT_V2, JIT_R2, JIT_V0);
  jit_str(j, JIT_R0, JIT_V1);
  jit_str(j, JIT_R1, JIT_V2);

  maybe_restore(j, JIT_V2);
  maybe_restore(j, JIT_V1);
  maybe_restore(j, JIT_V0);

  jit_ret(j);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  void (*f)(intmax_t*, intmax_t*, intmax_t, intmax_t) = ret;

#define QDIV(a, b, c, d) \
  do { \
    intmax_t C = 0, D = 0; f(&C, &D, a, b); ASSERT(C == c); ASSERT(D == d); \
  } while (0)
  
  QDIV(-1, -2, 1, 1);
  QDIV(-2, -5, 1, 3);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
