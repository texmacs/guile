#include "qalu.inc"

.code
	prolog
#define QMUL(N, I0, I1, LO, HI)		QALU(N, , qmul, I0, I1, LO, HI)
#define UQMUL(N, I0, I1, LO, HI)	QALU(N, _u, qmul, I0, I1, LO, HI)
	QMUL(0, -2, -1, 2, 0)
	QMUL(1, 0, -1, 0, 0)
	QMUL(2, -1, 0, 0, 0)
	QMUL(3, 1, -1, -1, -1)
#if __WORDSIZE == 32
	 QMUL(4, 0x7ffff, 0x7ffff, 0xfff00001, 0x3f)
	UQMUL(5, 0xffffff, 0xffffff, 0xfe000001, 0xffff)
	 QMUL(6, 0x80000000, -2, 0, 1)
#else
	 QMUL(4, 0x7ffffffff, 0x7ffffffff, 0xfffffff000000001, 0x3f)
	UQMUL(5, 0xffffffffff, 0xffffffffff, 0xfffffe0000000001, 0xffff)
	 QMUL(6, 0x8000000000000000, -2, 0, 1)
#endif
	prepare
		pushargi ok
		ellipsis
	finishi @printf
	ret
	epilog
