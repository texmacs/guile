.data	32
dfmt:
.c	"%1.0f\n"
ifmt:
.c	"%d\n"

.code
	jmpi main

#define def_test_double(a, b, c)		\
test_double_##a##_##b##_##c:			\
	prolog					\
	arg_d $d0				\
	arg_d $d1				\
	getarg_d %b $d0				\
	getarg_d %c $d1				\
	subr_d %a %b %c				\
	retr_d %a				\
	epilog
#define test_double(a, b, c, x, y)		\
	prepare 0				\
		pushargi_d x			\
		pushargi_d y			\
	finishi test_double_##a##_##b##_##c	\
	prepare 1				\
		pushargi dfmt			\
		pushargr_d %fret		\
	finishi @printf

#define def_test_int(a, b, c)			\
test_int_##a##_##b##_##c:			\
	prolog					\
	arg $i0					\
	arg $i1					\
	getarg %b $i0				\
	getarg %c $i1				\
	subr %a %b %c				\
	retr %a					\
	epilog
#define test_int(a, b, c, x, y)			\
	prepare 0				\
		pushargi x			\
		pushargi y			\
	finishi test_int_##a##_##b##_##c	\
	prepare 1				\
		pushargi ifmt			\
		pushargr %ret			\
	finishi @printf

def_test_double(f0, f0, f0)
def_test_double(f0, f0, f1)
def_test_double(f0, f1, f0)
def_test_double(f0, f1, f2)

def_test_double(f3, f3, f3)
def_test_double(f3, f3, f1)
def_test_double(f3, f1, f3)
def_test_double(f3, f1, f2)

def_test_double(f3, f0, f0)
def_test_double(f3, f0, f3)
def_test_double(f3, f3, f0)

def_test_int(r0, r0, r0)
def_test_int(r0, r0, r1)
def_test_int(r0, r1, r0)
def_test_int(r0, r1, r2)

def_test_int(v0, v0, v0)
def_test_int(v0, v0, r1)
def_test_int(v0, r1, v0)
def_test_int(v0, r1, r2)

def_test_int(v0, r0, r0)
def_test_int(v0, r0, v0)
def_test_int(v0, v0, r0)


main:
	prolog

	test_double(f0, f0, f0, 3.0, 2.0)
	test_double(f0, f0, f1, 3.0, 2.0)
	test_double(f0, f1, f0, 3.0, 2.0)
	test_double(f0, f1, f2, 3.0, 2.0)

	test_double(f3, f3, f3, 3.0, 2.0)
	test_double(f3, f3, f1, 3.0, 2.0)
	test_double(f3, f1, f3, 3.0, 2.0)
	test_double(f3, f1, f2, 3.0, 2.0)

	test_double(f3, f0, f0, 3.0, 2.0)
	test_double(f3, f0, f3, 3.0, 2.0)
	test_double(f3, f3, f0, 3.0, 2.0)

	test_int(r0, r0, r0, 3, 2)
	test_int(r0, r0, r1, 3, 2)
	test_int(r0, r1, r0, 3, 2)
	test_int(r0, r1, r2, 3, 2)

	test_int(v0, v0, v0, 3, 2)
	test_int(v0, v0, r1, 3, 2)
	test_int(v0, r1, v0, 3, 2)
	test_int(v0, r1, r2, 3, 2)

	test_int(v0, r0, r0, 3, 2)
	test_int(v0, r0, v0, 3, 2)
	test_int(v0, v0, r0, 3, 2)

	ret
	epilog
