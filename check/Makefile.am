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
	branch.tst	branch.ok	\
	alu.inc				\
	alu_add.tst	alu_add.ok	\
	alux_add.tst	alux_add.ok	\
	alu_sub.tst	alu_sub.ok	\
	alux_sub.tst	alux_sub.ok	\
	alu_mul.tst	alu_mul.ok	\
	alu_div.tst	alu_div.ok	\
	alu_rem.tst	alu_rem.ok	\
	alu_and.tst	alu_and.ok	\
	alu_or.tst	alu_or.ok	\
	alu_xor.tst	alu_xor.ok	\
	alu_lsh.tst	alu_lsh.ok	\
	alu_rsh.tst	alu_rsh.ok	\
	alu_com.tst	alu_com.ok	\
	alu_neg.tst	alu_neg.ok	\
	check.sh	run-test	\
	all.tst

TESTS = 3to2 add allocai		\
	bp divi fib rpn			\
	ldstr ldsti			\
	ldstxr ldstxi			\
	ldstr-c ldstxr-c ldstxi-c	\
	cvt branch			\
	alu_add alux_add		\
	alu_sub alux_sub		\
	alu_mul alu_div alu_rem		\
	alu_and alu_or alu_xor		\
	alu_lsh alu_rsh			\
	alu_com alu_neg

CLEANFILES = $(TESTS)

# Not so pretty but good for a prototype
$(TESTS):	check.sh
	$(LN_S) $(srcdir)/check.sh $@

TESTS_ENVIRONMENT=$(srcdir)/run-test

debug:		$(check_PROGRAMS)
	$(LIBTOOL) --mode=execute gdb $(check_PROGRAMS)

