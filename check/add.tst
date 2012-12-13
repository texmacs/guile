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
	prepare
		pushargi 5
		pushargi 4
	finishi test
	retval %r0
	prepare
		pushargi fmt
		ellipsis
		pushargi 5
		pushargi 4
		pushargr %r0
	finishi @printf
	ret
	epilog
