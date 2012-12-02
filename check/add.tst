.data	32
fmt:
.c	"%d + %d = %d\n"

.code
	jmpi main

test:
	prolog
	arg $i0
	arg $i1
	getarg %r0 $i0
	getarg %r1 $i1
	addr %ret %r0 %r1
	ret
	epilog

main:
	prolog
	prepare 0
		pushargi 5
		pushargi 4
	finishi test
	prepare 1
		pushargi fmt
		pushargi 5
		pushargi 4
		pushargr %r0
	finishi @printf
	ret
	epilog
