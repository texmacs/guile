#
# Copyright 2012 Free Software Foundation, Inc.
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

AM_CFLAGS = -I$(top_srcdir)/include -D_GNU_SOURCE

check_PROGRAMS = lightning

lightning_LDADD = $(top_builddir)/lib/liblightning.la -lm -ldl

$(top_builddir)/lib/liblightning.la:
	cd $(top_builddir)/lib; $(MAKE) $(AM_MAKEFLAGS) liblightning.la

lightning_SOURCES =			\
	lightning.c

EXTRA_DIST =				\
	3to2.tst	3to2.ok		\
	add.tst		add.ok		\
	allocai.tst	allocai.ok	\
	bp.tst		bp.ok		\
	divi.tst	divi.ok		\
	fib.tst		fib.ok		\
	rpn.tst		rpn.ok		\
	ldst.inc			\
	ldstr.tst	ldstr.ok	\
	ldsti.tst	ldsti.ok	\
	ldstxr.tst	ldstxr.ok	\
	ldstxi.tst	ldstxi.ok	\
	ldstr-c.tst	ldstr-c.ok	\
	ldstxr-c.tst	ldstxr-c.ok	\
	ldstxi-c.tst	ldstxi-c.ok	\
	cvt.tst		cvt.ok		\
	check.sh	run-test	\
	all.tst

TESTS = 3to2 add allocai		\
	bp divi fib rpn			\
	ldstr ldsti			\
	ldstxr ldstxi			\
	ldstr-c ldstxr-c ldstxi-c	\
	cvt

CLEANFILES = $(TESTS)

# Not so pretty but good for a prototype
$(TESTS):	check.sh
	$(LN_S) $(srcdir)/check.sh $@

TESTS_ENVIRONMENT=$(srcdir)/run-test

debug:		$(check_PROGRAMS)
	$(LIBTOOL) --mode=execute gdb $(check_PROGRAMS)

