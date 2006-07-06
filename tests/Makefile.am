AM_CPPFLAGS = -I$(top_builddir) -I$(top_srcdir)

noinst_PROGRAMS = fibit incr printf printf2 rpn fib fibdelay add bp testfp funcfp rpnfp modi
noinst_DATA = fibit.ok incr.ok printf.ok printf2.ok rpn.ok fib.ok fibdelay.ok testfp.ok funcfp.ok rpnfp.ok add.ok bp.ok modi.ok
EXTRA_DIST = $(noinst_DATA) run-test

if DISASS
LDADD = $(top_builddir)/opcode/libdisass.a
endif

if REGRESSION_TESTING
TESTS = fib fibit fibdelay incr printf printf2 rpn add bp testfp funcfp rpnfp modi
TESTS_ENVIRONMENT=$(srcdir)/run-test
endif
