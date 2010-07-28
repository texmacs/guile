/* Written by Paulo Cesar Pereira de Andrade, pcpa@mandriva.com.br */

#include <stdio.h>
#include <stddef.h>
#include <lightning.h>
#include <sys/types.h>

#define CHECK_FLOAT		1

typedef struct types {
    char		 c;
    unsigned char	 uc;
    short		 s;
    unsigned short	 us;
    int			 i;
    unsigned int	 ui;
    long		 l;
    unsigned long	 ul;
    float		 f;
    double		 d;
    void		*p;
} types_t;

typedef void (*pvv_t)(void);

pvv_t			 pvv;
types_t			 t0, t1;
jit_insn		 code[4096];

#define warn(format, ...)						\
    fprintf(stderr, "%s:%d: " format "\n", __func__, (int) __LINE__ ,#__VA_ARGS__)

#if __WORDSIZE == 64
#  define LONG_VALUE			0x7fffffffffffffff
#else
#  define LONG_VALUE			0x7fffffff
#endif
void
check1(void)
{
    if (t0.c  != t1.c)			warn("0x%x 0x%x",  t0.c,  t1.c);
    if (t0.uc != t1.uc)			warn("0x%x 0x%x",  t0.uc, t1.uc);
    if (t0.s  != t1.s)			warn("0x%x 0x%x",  t0.s,  t1.s);
    if (t0.us != t1.us)			warn("0x%x 0x%x",  t0.us, t1.us);
    if (t0.i  != t1.i)			warn("0x%x 0x%x",  t0.i,  t1.i);
    if (t0.ui != t1.ui)			warn("0x%x 0x%x",  t0.ui, t1.ui);
    if (t0.l  != t1.l)			warn("0x%lx 0x%lx",  t0.l,  t1.l);
    if (t0.ul != t1.ul)			warn("0x%lx 0x%lx",  t0.ul, t1.ul);
#if CHECK_FLOAT
    if (t0.f  != t1.f)			warn("%f %f",      t0.f,  t1.f);
    if (t0.d  != t1.d)			warn("%f %f",      t0.d,  t1.d);
#endif
    if (t0.p  != t1.p)			warn("0x%p 0x%p",  t0.p,  t1.p);
}
check0(void)
{
    if (t0.c != 0x7f)			warn("0x%x",  t0.c);
    if (t0.uc != 0x7f)			warn("0x%x",  t0.uc);
    if (t0.s != 0x7fff)			warn("0x%x",  t0.s);
    if (t0.us != 0x7fff)		warn("0x%x",  t0.us);
    if (t0.i != 0x7fffffff)		warn("0x%x",  t0.i);
    if (t0.ui != 0x7fffffff)		warn("0x%x",  t0.ui);
    if (t0.l != LONG_VALUE)		warn("0x%lx", t0.l);
    if (t0.ul != LONG_VALUE)		warn("0x%lx", t0.ul);
#if CHECK_FLOAT
    if (t0.f != 0.5f)			warn("%f",    t0.f);
    if (t0.d != 1.5)			warn("%f",    t0.d);

#endif
    if (t0.p != (void*)0xdeadbeef)	warn("0x%p",  t0.p);
}
void
test(V0, V1, R0, F0)
{
    jit_movi_p (			V0, &t0);
    jit_movi_p (			V1, &t1);
    jit_movi_i (			R0, 0x7f);
    jit_stxi_c (offsetof(types_t, c),	V0, R0);
    jit_stxi_uc(offsetof(types_t, uc),	V0, R0);
    jit_movi_i (			R0, 0x7fff);
    jit_stxi_s(offsetof(types_t, s),	V0, R0);
    jit_stxi_us(offsetof(types_t, us),	V0, R0);
    jit_movi_i (			R0, 0x7fffffff);
    jit_stxi_i (offsetof(types_t, i),	V0, R0);
    jit_stxi_ui(offsetof(types_t, ui),	V0, R0);
    jit_movi_l (			R0, LONG_VALUE);
    jit_stxi_l (offsetof(types_t, l),	V0, R0);
    jit_stxi_ul(offsetof(types_t, ul),	V0, R0);
#if CHECK_FLOAT
    jit_movi_f (			F0, 0.5f);
    jit_stxi_f (offsetof(types_t, f),	V0, F0);
    jit_movi_d (			F0, 1.5);
    jit_stxi_d (offsetof(types_t, d),	V0, F0);
#endif
    jit_movi_p (			R0, 0xdeadbeef);
    jit_stxi_p (offsetof(types_t, p),	V0, R0);
    jit_calli  (check0);

    jit_ldxi_c (			R0, V0, offsetof(types_t, c));
    jit_stxi_c (offsetof(types_t, c),	V1, R0);
    jit_ldxi_uc(			R0, V0, offsetof(types_t, uc));
    jit_stxi_uc(offsetof(types_t, uc),	V1, R0);
    jit_ldxi_s (			R0, V0, offsetof(types_t, s));
    jit_stxi_s (offsetof(types_t, s),	V1, R0);
    jit_ldxi_us(			R0, V0, offsetof(types_t, us));
    jit_stxi_us(offsetof(types_t, us),	V1, R0);
    jit_ldxi_i (			R0, V0, offsetof(types_t, i));
    jit_stxi_i (offsetof(types_t, i),	V1, R0);
    jit_ldxi_ui(			R0, V0, offsetof(types_t, ui));
    jit_stxi_ui(offsetof(types_t, ui),	V1, R0);
    jit_ldxi_l (			R0, V0, offsetof(types_t, l));
    jit_stxi_l (offsetof(types_t, l),	V1, R0);
    jit_ldxi_ul(			R0, V0, offsetof(types_t, ul));
    jit_stxi_ul(offsetof(types_t, ul),	V1, R0);
#if CHECK_FLOAT
    jit_ldxi_f (			F0, V0, offsetof(types_t, f));
    jit_stxi_f (offsetof(types_t, f),	V1, F0);
    jit_ldxi_d (			F0, V0, offsetof(types_t, d));
    jit_stxi_d (offsetof(types_t, d),	V1, F0);
#endif
    jit_ldxi_p (			R0, V0, offsetof(types_t, p));
    jit_stxi_p (offsetof(types_t, p),	V1, R0);
    jit_calli  (check1);
}

int
main(int argc, char *argv[])
{
    jit_set_ip(code);
    jit_prolog(0);
    test(JIT_V0, JIT_V1, JIT_R0, JIT_FPR0);
    jit_ret();

    jit_flush_code(code, jit_get_ip().ptr);

    pvv = (pvv_t)code;
    (*pvv)();

    return (0);
}
