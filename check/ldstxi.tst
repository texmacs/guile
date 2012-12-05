#if __WORDSIZE == 64
#  define L0		0x8000000000000001
#  define LL0		L0
#  define LC0		0xffffffffffffff81
#  define LS0		0xffffffffffff8001
#  define LI0		0xffffffff80000001
#  define L1		0x8000000000000000
#  define LL1		L1
#  define LC1		0xffffffffffffff80
#  define LS1		0xffffffffffff8000
#  define LI1		0xffffffff80000000
#  define L2		0x7fffffffffffffff
#  define LL2		L2
#  define LC2		0x000000000000007f
#  define LS2		0x0000000000007fff
#  define LI2		0x000000007fffffff
#  define L3		0xffffffffffffffff
#  define LL3		L3
#  define LC3		0xffffffffffffffff
#  define LS3		0xffffffffffffffff
#  define LI3		0xffffffffffffffff
#endif
#define I0		0x80000001
#define II0		I0
#define IC0		0xffffff81
#define IS0		0xffff8001
#define I1		0x80000000
#define II1		I1
#define IC1		0xffffff80
#define IS1		0xffff8000
#define I2		0x7fffffff
#define II2		I2
#define IC2		0x0000007f
#define IS2		0x00007fff
#define I3		0xffffffff
#define II3		I3
#define IC3		0xffffffff
#define IS3		0xffffffff
#define S0		0x8001
#define S1		0x8000
#define S2		0x7fff
#define S3		0xffff
#define C0		0x81
#define C1		0x80
#define C2		0x7f
#define C3		0xff
#define F0		 0.25
#define F1		 0.75
#define F2		-0.25
#define F3		-0.75
#define D0		 0.25
#define D1		 0.75
#define D2		-0.25
#define D3		-0.75

.data	512
ok:
.c	"ok\n"
t0:
c0:
.c	0
uc0:
.c	0
s0:
.s	0
us0:
.s	0
.align	4
i0:
.i	0
#if __WORDSIZE == 64
ui0:
.i	0
.align	8
l0:
.l	0
#endif
f0:
.f	0
.align	8
d0:
.d	0

.	$($offc  = c0  - t0)
.	$($offuc = uc0 - t0)
.	$($offs  = s0  - t0)
.	$($offus = us0 - t0)
.	$($offi  = i0  - t0)
#if __WORDSIZE == 64
.	$($offui = ui0 - t0)
.	$($offl  = l0  - t0)
#endif
.	$($offf  = f0  - t0)
.	$($offd  = d0  - t0)

#if __WORDSIZE == 64
#  define LDSTL(N, R0, R1)					\
	stxi_i $offui %R0 %R1					\
	movi %R1 L##N						\
	stxi_l $offl %R0 %R1

#  define SI(C, N, x, X, R0, R1)				\
	ldxi_##x %R1 %R0 $off##x				\
	beqi L##x##C %R1 L##X##N				\
	calli @abort						\
L##x##C:

#  define LDRL(C, N, R0, R1)					\
	UI(C, N, i, I, R0, R1)					\
	SI(C, N, l, L, R0, R1)
#else
#  define LDSTL(C, R0, R1)
#  define SI(C, N, x, X, R0, R1)				\
	ldxi_##x $off##x %R1 %R0				\
	beqi L##x##C %R1 I##X##N				\
	calli @abort						\
L##x##C:

#  define LDRL(C, N, R0, R1)

#endif

#define UI(C, N, x, X, R0, R1)					\
	ldxi_u##x %R1 %R0 $offu##x				\
	beqi Lu##x##C %R1 X##N					\
	calli @abort						\
Lu##x##C:

#define FF(C, N, x, X, R0, F0)					\
	ldxi_##x %F0 %R0 $off##x				\
	beqi_##x L##x##C %F0 X##N				\
L##x##C:

#define LDST1(X, N, R0, R1, F0)					\
	movi %R0 t0						\
	movi %R1 C##N						\
	stxi_c $offc %R0 %R1					\
	stxi_c $offuc %R0 %R1					\
	movi %R1 S##N						\
	stxi_s $offs %R0 %R1					\
	stxi_s $offus %R0 %R1					\
	movi %R1 I##N						\
	stxi_i $offi %R0 %R1					\
	LDSTL(N, R0, R1)					\
	movi_f %F0 F##N						\
	stxi_f $offf %R0 %F0					\
	movi_d %F0 D##N						\
	stxi_d $offd %R0 %F0					\
	SI(X, N, c, C, R0, R1)					\
	UI(X, N, c, C, R0, R1)					\
	SI(X, N, s, S, R0, R1)					\
	UI(X, N, s, S, R0, R1)					\
	SI(X, N, i, I, R0, R1)					\
	LDRL(X, N, R0, R1)					\
	FF(X, N, f, F, R0, F0)					\
	FF(X, N, d, D, R0, F0)

#define LDST0(R0, R1, F0)					\
	LDST1(0_##R0##_##R1##_##F0, 0, R0, R1, F0)		\
	LDST1(1_##R0##_##R1##_##F0, 1, R0, R1, F0)		\
	LDST1(2_##R0##_##R1##_##F0, 2, R0, R1, F0)		\
	LDST1(3_##R0##_##R1##_##F0, 3, R0, R1, F0)

#define LDST(V0, V1, V2, R0, R1, R2, F0, F1, F2, F3, F4, F5)	\
	LDST0(V0, V1, F0)					\
	LDST0(V0, V2, F1)					\
	LDST0(V0, R0, F3)					\
	LDST0(V0, R1, F4)					\
	LDST0(V0, R2, F5)					\
	LDST0(V1, V2, F0)					\
	LDST0(V1, R0, F1)					\
	LDST0(V1, R1, F2)					\
	LDST0(V1, R2, F3)					\
	LDST0(V2, R0, F4)					\
	LDST0(V2, R1, F5)					\
	LDST0(V2, R2, F0)

.code
	jmpi main

main:
	prolog

	/* Simple test to simplify validating encodings before
	 * brute force tests */
	movi %r0 t0
	movi %r1 0x81
	stxi_c $offc %r0 %r1
	stxi_c $offuc %r0 %r1
	movi %r1 0x8001
	stxi_s $offs %r0 %r1
	stxi_s $offus %r0 %r1
	movi %r1 0x80000001
	stxi_i $offi %r0 %r1
#if __WORDSIZE == 64
	stxi_i $offui %r0 %r1
	movi %r1 0x8000000000000001
	stxi_l $offl %r0 %r1
#endif
	movi_f %f0 0.5
	stxi_f $offf %r0 %f0
	movi_d %f0 0.25
	stxi_d $offd %r0 %f0
	ldxi_c %r1 %r0 $offc
#if __WORDSIZE == 32
	beqi Lc %r1 0xffffff81
#else
	beqi Lc %r1 0xffffffffffffff81
#endif
	calli @abort
Lc:
	ldxi_uc %r1 %r0 $offuc
	beqi Luc %r1 0x81
	calli @abort
Luc:
	ldxi_s %r1 %r0 $offs
#if __WORDSIZE == 32
	beqi Ls %r1 0xffff8001
#else
	beqi Ls %r1 0xffffffffffff8001
#endif
	calli @abort
Ls:
	ldxi_us %r1 %r0 $offus
	beqi Lus %r1 0x8001
	calli @abort
Lus:
	ldxi_i %r1 %r0 $offi
#if __WORDSIZE == 32
	beqi Li %r1 0x80000001
#else
	beqi Li %r1 0xffffffff80000001
#endif
	calli @abort
Li:
#if __WORDSIZE == 64
	ldxi_ui %r1 %r0 $offui
	beqi Lui %r1 0x80000001
	calli @abort
Lui:
	ldxi_l %r1 %r0 $offl
	beqi Ll %r1 0x8000000000000001
	calli @abort
Ll:
#endif
	ldxi_f %f0 %r0 $offf
	beqi_f Lf %f0 0.5
	calli @abort
Lf:
	ldxi_d %f0 %r0 $offd
	beqi_d Ld %f0 0.25
	calli @abort
Ld:

	LDST(v0, v1, v2, r0, r1, r2, f0, f1, f2, f3, f4, f5)
	// just to know did not abort
	prepare 1
		pushargi ok
	finishi @printf
	ret
	epilog
