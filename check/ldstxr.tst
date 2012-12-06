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
#  define LDSTL(N, R0, R1, R2)					\
	movi %R2 $offui						\
	stxr_i %R2 %R0 %R1					\
	movi %R1 L##N						\
	movi %R2 $offl						\
	stxr_l %R2 %R0 %R1

#  define SI(C, N, x, X, R0, R1, R2)				\
	movi %R2 $off##x					\
	ldxr_##x %R1 %R0 %R2					\
	beqi L##x##C %R1 L##X##N				\
	calli @abort						\
L##x##C:

#  define LDRL(C, N, R0, R1, R2)				\
	UI(C, N, i, I, R0, R1, R2)				\
	SI(C, N, l, L, R0, R1, R2)
#else
#  define LDSTL(C, R0, R1, R2)
#  define SI(C, N, x, X, R0, R1, R2)				\
	movi %R2 $off##x					\
	ldxr_##x %R1 %R0 %R2					\
	beqi L##x##C %R1 I##X##N				\
	calli @abort						\
L##x##C:

#  define LDRL(C, N, R0, R1, R2)

#endif

#define UI(C, N, x, X, R0, R1, R2)				\
	movi %R2 $offu##x					\
	ldxr_u##x %R1 %R0 %R2					\
	beqi Lu##x##C %R1 X##N					\
	calli @abort						\
Lu##x##C:

#define FF(C, N, x, X, R0, R1, F0)				\
	movi %R1 $off##x					\
	ldxr_##x %F0 %R0 %R1					\
	beqi_##x L##x##C %F0 X##N				\
L##x##C:

#define LDST1(X, N, R0, R1, R2, F0)				\
	movi %R0 t0						\
	movi %R1 C##N						\
	movi %R2 $offc						\
	stxr_c %R2 %R0 %R1					\
	movi %R2 $offuc						\
	stxr_c %R2 %R0 %R1					\
	movi %R1 S##N						\
	movi %R2 $offs						\
	stxr_s %R2 %R0 %R1					\
	movi %R2 $offus						\
	stxr_s %R2 %R0 %R1					\
	movi %R1 I##N						\
	movi %R2 $offi						\
	stxr_i %R2 %R0 %R1					\
	LDSTL(N, R0, R1, R2)					\
	movi_f %F0 F##N						\
	movi %R2 $offf						\
	stxr_f %R2 %R0 %F0					\
	movi_d %F0 D##N						\
	movi %R2 $offd						\
	stxr_d %R2 %R0 %F0					\
	SI(X, N, c, C, R0, R1, R2)				\
	UI(X, N, c, C, R0, R1, R2)				\
	SI(X, N, s, S, R0, R1, R2)				\
	UI(X, N, s, S, R0, R1, R2)				\
	SI(X, N, i, I, R0, R1, R2)				\
	LDRL(X, N, R0, R1, R2)					\
	FF(X, N, f, F, R0, R1, F0)				\
	FF(X, N, d, D, R0, R1, F0)

#define LDST0(R0, R1, R2, F0)					\
	LDST1(0_##R0##_##R1##_##R2##_##F0, 0, R0, R1, R2, F0)	\
	LDST1(1_##R0##_##R1##_##R2##_##F0, 1, R0, R1, R2, F0)	\
	LDST1(2_##R0##_##R1##_##R2##_##F0, 2, R0, R1, R2, F0)	\
	LDST1(3_##R0##_##R1##_##R2##_##F0, 3, R0, R1, R2, F0)

#define LDST(V0, V1, V2, R0, R1, R2, F0, F1, F2, F3, F4, F5)	\
	LDST0(V0, V1, R0, F0)					\
	LDST0(V0, V1, R1, F1)					\
	LDST0(V0, V1, R2, F2)					\
	LDST0(V0, V2, R0, F3)					\
	LDST0(V0, V2, R1, F4)					\
	LDST0(V0, V2, R2, F5)					\
	LDST0(V0, R0, V1, F0)					\
	LDST0(V0, R0, V2, F1)					\
	LDST0(V0, R0, R1, F2)					\
	LDST0(V0, R0, R2, F3)					\
	LDST0(V0, R0, V1, F4)					\
	LDST0(V0, R1, V1, F5)					\
	LDST0(V0, R1, V2, F0)					\
	LDST0(V0, R1, R0, F1)					\
	LDST0(V0, R1, R2, F2)					\
	LDST0(V0, V1, V2, F3)					\
	LDST0(V0, R1, R0, F4)					\
	LDST0(V0, R1, R2, F5)					\
	LDST0(R0, V1, V0, F0)					\
	LDST0(R0, V1, R1, F1)					\
	LDST0(R0, V1, R2, F2)					\
	LDST0(R0, V2, V0, F3)					\
	LDST0(R0, V2, R1, F4)					\
	LDST0(R0, V2, R2, F5)					\
	LDST0(R0, V0, V1, F0)					\
	LDST0(R0, V0, V2, F1)					\
	LDST0(R0, V0, R1, F2)					\
	LDST0(R0, V0, R2, F3)					\
	LDST0(R0, V0, V1, F4)					\
	LDST0(R0, R1, V1, F5)					\
	LDST0(R0, R1, V2, F0)					\
	LDST0(R0, R1, V0, F1)					\
	LDST0(R0, R1, R2, F2)					\
	LDST0(R0, V1, V2, F3)					\
	LDST0(R0, R1, V0, F4)					\
	LDST0(R0, R1, R2, F5)

.code
	jmpi main

main:
	prolog

	/* Simple test to simplify validating encodings before
	 * brute force tests */
	movi %r0 t0
	movi %r1 0x81
	movi %r2 $offc
	stxr_c %r2 %r0 %r1
	movi %r2 $offuc
	stxr_c %r2 %r0 %r1
	movi %r1 0x8001
	movi %r2 $offs
	stxr_s %r2 %r0 %r1
	movi %r2 $offus
	stxr_s %r2 %r0 %r1
	movi %r1 0x80000001
	movi %r2 $offi
	stxr_i %r2 %r0 %r1
#if __WORDSIZE == 64
	movi %r2 $offui
	stxr_i %r2 %r0 %r1
	movi %r1 0x8000000000000001
	movi %r2 $offl
	stxr_l %r2 %r0 %r1
#endif
	movi_f %f0 0.5
	movi %r2 $offf
	stxr_f %r2 %r0 %f0
	movi_d %f0 0.25
	movi %r2 $offd
	stxr_d %r2 %r0 %f0
	movi %r2 $offc
	ldxr_c %r1 %r0 %r2
#if __WORDSIZE == 32
	beqi Lc %r1 0xffffff81
#else
	beqi Lc %r1 0xffffffffffffff81
#endif
	calli @abort
Lc:
	movi %r2 $offuc
	ldxr_uc %r1 %r0 %r2
	beqi Luc %r1 0x81
	calli @abort
Luc:
	movi %r2 $offs
	ldxr_s %r1 %r0 %r2
#if __WORDSIZE == 32
	beqi Ls %r1 0xffff8001
#else
	beqi Ls %r1 0xffffffffffff8001
#endif
	calli @abort
Ls:
	movi %r2 $offus
	ldxr_us %r1 %r0 %r2
	beqi Lus %r1 0x8001
	calli @abort
Lus:
	movi %r2 $offi
	ldxr_i %r1 %r0 %r2
#if __WORDSIZE == 32
	beqi Li %r1 0x80000001
#else
	beqi Li %r1 0xffffffff80000001
#endif
	calli @abort
Li:
#if __WORDSIZE == 64
	movi %r2 $offui
	ldxr_ui %r1 %r0 %r2
	beqi Lui %r1 0x80000001
	calli @abort
Lui:
	movi %r2 $offl
	ldxr_l %r1 %r0 %r2
	beqi Ll %r1 0x8000000000000001
	calli @abort
Ll:
#endif
	movi %r2 $offf
	ldxr_f %f0 %r0 %r2
	beqi_f Lf %f0 0.5
	calli @abort
Lf:
	movi %r2 $offd
	ldxr_d %f0 %r0 %r2
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
