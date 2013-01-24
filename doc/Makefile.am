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

info_TEXINFOS = lightning.texi 
MOSTLYCLEANFILES = lightning.tmp

lightning_TEXINFOS = body.texi version.texi

noinst_PROGRAMS = incr printf rpn rfib ifib

$(top_builddir)/lib/liblightning.la:
	cd $(top_builddir)/lib; $(MAKE) $(AM_MAKEFLAGS) liblightning.la

incr_LDADD = $(top_builddir)/lib/liblightning.la -lm -ldl
incr_SOURCES = incr.c

printf_LDADD = $(top_builddir)/lib/liblightning.la -lm -ldl
printf_SOURCES = printf.c

rpn_LDADD = $(top_builddir)/lib/liblightning.la -lm -ldl
rpn_SOURCES = rpn.c

rfib_LDADD = $(top_builddir)/lib/liblightning.la -lm -ldl
rfib_SOURCES = rfib.c

ifib_LDADD = $(top_builddir)/lib/liblightning.la -lm -ldl
ifib_SOURCES = ifib.c
