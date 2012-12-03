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

lightning_includedir = $(includedir)/lightning

EXTRA_DIST =		\
	jit_private.h

if cpu_arm
lightning_include_HEADERS = 	\
	jit_arm.h
endif
if cpu_mips
lightning_include_HEADERS = 	\
	jit_mips.h
endif
if cpu_ppc
lightning_include_HEADERS = 	\
	jit_ppc.h
endif
if cpu_x86
lightning_include_HEADERS = 	\
	jit_x86.h
endif
