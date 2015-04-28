.data	16
fmt:
.c	"%d %f\n"
.code
	jmpi main
varargs:
	prolog
	ellipsis
	va_start %v0
	va_arg %r0 %v0
	va_arg_d %f0 %v0
	va_end %v0
	prepare
		pushargi fmt
		ellipsis
		pushargr %r0
		pushargr_d %f0
	finishi @printf
	ret
	epilog

main:
	prolog
	prepare
		ellipsis
		pushargi 1
		pushargi_d 2
	finishi varargs
	ret
	epilog
