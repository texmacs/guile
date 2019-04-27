#include "test.h"

struct args
{
  int8_t a;
  int16_t b;
  int32_t c;
  jit_word_t d;
  uint16_t e;
  float f;
  double g;
  float h;
};

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 3, 0, 0);

  jit_operand_t args[9] = {
    jit_operand_gpr(JIT_OPERAND_ABI_POINTER, JIT_R0),
    jit_operand_gpr(JIT_OPERAND_ABI_INT8, JIT_R1),
    jit_operand_gpr(JIT_OPERAND_ABI_INT16, JIT_R2),
    jit_operand_gpr(JIT_OPERAND_ABI_INT32, JIT_V0),
    jit_operand_gpr(JIT_OPERAND_ABI_WORD, JIT_V1),
    jit_operand_gpr(JIT_OPERAND_ABI_UINT16, JIT_V2),
    jit_operand_fpr(JIT_OPERAND_ABI_FLOAT, JIT_F0),
    jit_operand_fpr(JIT_OPERAND_ABI_DOUBLE, JIT_F1),
    jit_operand_fpr(JIT_OPERAND_ABI_FLOAT, JIT_F2),
  };
  jit_load_args(j, 9, args);
  jit_stxi_c(j, offsetof(struct args, a), JIT_R0, JIT_R1); // a
  jit_stxi_s(j, offsetof(struct args, b), JIT_R0, JIT_R2); // b
  jit_stxi_i(j, offsetof(struct args, c), JIT_R0, JIT_V0); // c
  jit_stxi(j,   offsetof(struct args, d), JIT_R0, JIT_V1); // d
  jit_stxi_s(j, offsetof(struct args, e), JIT_R0, JIT_V2); // e
  jit_stxi_f(j, offsetof(struct args, f), JIT_R0, JIT_F0); // f
  jit_stxi_d(j, offsetof(struct args, g), JIT_R0, JIT_F1); // g
  jit_stxi_f(j, offsetof(struct args, h), JIT_R0, JIT_F2); // h

  jit_leave_jit_abi(j, 3, 0, align);
  jit_retr(j, JIT_R0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  struct args* (*f)(struct args*, int8_t, int16_t, int32_t, jit_word_t,
                    uint16_t, float, double, float) = ret;

  struct args in = { 0, 1, 2, 3, 4, 5, 6, 7 };
  struct args out;
  ASSERT(f(&out, in.a, in.b, in.c, in.d, in.e, in.f, in.g, in.h) == &out);
  ASSERT(in.a == out.a);
  ASSERT(in.b == out.b);
  ASSERT(in.c == out.c);
  ASSERT(in.d == out.d);
  ASSERT(in.e == out.e);
  ASSERT(in.f == out.f);
  ASSERT(in.g == out.g);
  ASSERT(in.h == out.h);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
