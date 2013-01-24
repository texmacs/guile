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

check_PROGRAMS = lightning ccall

lightning_LDADD = $(top_builddir)/lib/liblightning.la -lm -ldl
lightning_SOURCES = lightning.c

ccall_LDADD = $(top_builddir)/lib/liblightning.la -lm -ldl
ccall_SOURCES = ccall.c

$(top_builddir)/lib/liblightning.la:
	cd $(top_builddir)/lib; $(MAKE) $(AM_MAKEFLAGS) liblightning.la

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
	fop_abs.tst	fop_abs.ok	\
	fop_sqrt.tst	fop_sqrt.ok	\
	varargs.tst	varargs.ok	\
	stack.tst	stack.ok	\
	clobber.tst	clobber.ok	\
	carry.tst	carry.ok	\
	call.tst	call.ok		\
	float.tst	float.ok	\
	ccall.ok			\
	check.sh			\
	check.x87.sh			\
	check.arm.sh	check.swf.sh	\
	run-test	all.tst

base_TESTS =				\
	3to2 add allocai		\
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
	alu_com alu_neg			\
	fop_abs fop_sqrt		\
	varargs stack			\
	clobber carry call		\
	float

$(base_TESTS):	check.sh
	$(LN_S) $(srcdir)/check.sh $@

TESTS = $(base_TESTS)

if test_x86_x87
#x87_TESTS = $(addsuffix .x87, $(base_TESTS))
x87_TESTS =					\
	3to2.x87 add.x87 allocai.x87		\
	bp.x87 divi.x87 fib.x87 rpn.x87		\
	ldstr.x87 ldsti.x87			\
	ldstxr.x87 ldstxi.x87			\
	ldstr-c.x87 ldstxr-c.x87 ldstxi-c.x87	\
	cvt.x87 branch.x87			\
	alu_add.x87 alux_add.x87		\
	alu_sub.x87 alux_sub.x87		\
	alu_mul.x87 alu_div.x87 alu_rem.x87	\
	alu_and.x87 alu_or.x87 alu_xor.x87	\
	alu_lsh.x87 alu_rsh.x87			\
	alu_com.x87 alu_neg.x87			\
	fop_abs.x87 fop_sqrt.x87		\
	varargs.x87 stack.x87			\
	clobber.x87 carry.x87 call.x87		\
	float.x87
$(x87_TESTS):	check.x87.sh
	$(LN_S) $(srcdir)/check.x87.sh $@
TESTS += $(x87_TESTS)
endif

if test_arm_arm
#arm_TESTS = $(addsuffix .arm, $(base_TESTS))
arm_TESTS =					\
	3to2.arm add.arm allocai.arm		\
	bp.arm divi.arm fib.arm rpn.arm		\
	ldstr.arm ldsti.arm			\
	ldstxr.arm ldstxi.arm			\
	ldstr-c.arm ldstxr-c.arm ldstxi-c.arm	\
	cvt.arm branch.arm			\
	alu_add.arm alux_add.arm		\
	alu_sub.arm alux_sub.arm		\
	alu_mul.arm alu_div.arm alu_rem.arm	\
	alu_and.arm alu_or.arm alu_xor.arm	\
	alu_lsh.arm alu_rsh.arm			\
	alu_com.arm alu_neg.arm			\
	fop_abs.arm fop_sqrt.arm		\
	varargs.arm stack.arm			\
	clobber.arm carry.arm call.arm		\
	float.arm
$(arm_TESTS):	check.arm.sh
	$(LN_S) $(srcdir)/check.arm.sh $@
TESTS += $(arm_TESTS)
endif

if test_arm_swf
#swf_TESTS = $(addsuffix .swf, $(base_TESTS))
swf_TESTS =					\
	3to2.swf add.swf allocai.swf		\
	bp.swf divi.swf fib.swf rpn.swf		\
	ldstr.swf ldsti.swf			\
	ldstxr.swf ldstxi.swf			\
	ldstr-c.swf ldstxr-c.swf ldstxi-c.swf	\
	cvt.swf branch.swf			\
	alu_add.swf alux_add.swf		\
	alu_sub.swf alux_sub.swf		\
	alu_mul.swf alu_div.swf alu_rem.swf	\
	alu_and.swf alu_or.swf alu_xor.swf	\
	alu_lsh.swf alu_rsh.swf			\
	alu_com.swf alu_neg.swf			\
	fop_abs.swf fop_sqrt.swf		\
	varargs.swf stack.swf			\
	clobber.swf carry.swf call.swf		\
	float.swf
$(swf_TESTS):	check.swf.sh
	$(LN_S) $(srcdir)/check.swf.sh $@
TESTS += $(swf_TESTS)
endif

TESTS += ccall
CLEANFILES = $(TESTS)

#TESTS_ENVIRONMENT=$(srcdir)/run-test;

debug:		lightning
	$(LIBTOOL) --mode=execute gdb lightning

