#
# Copyright 2000, 2001, 2002, 2012 Free Software Foundation, Inc.
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

AM_CFLAGS = -I$(top_srcdir)/include -D_GNU_SOURCE $(LIGHTNING_CFLAGS)
liblightning_LTLIBRARIES = liblightning.la

liblightningdir = $(libdir)
liblightning_la_SOURCES =	\
	jit_disasm.c		\
	jit_print.c		\
	lightning.c

EXTRA_DIST =			\
	jit_arm.c		\
	jit_arm-cpu.c		\
	jit_arm-swf.c		\
	jit_arm-vfp.c		\
	jit_mips.c		\
	jit_mips-cpu.c		\
	jit_mips-fpu.c		\
	jit_ppc.c		\
	jit_ppc-cpu.c		\
	jit_ppc-fpu.c		\
	jit_x86.c		\
	jit_x86-cpu.c		\
	jit_x86-sse.c		\
	jit_x86-x87.c
