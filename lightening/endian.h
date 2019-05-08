/*
 * Copyright (C) 2012-2019  Free Software Foundation, Inc.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GNU lightning is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 *	Andy Wingo
 */

#ifndef _jit_endian_h
#define _jit_endian_h

#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stddef.h>

#ifndef __WORDSIZE
#  if defined(WORDSIZE)				/* ppc darwin */
#    define __WORDSIZE		WORDSIZE
#  elif defined(__SIZEOF_POINTER__)		/* ppc aix */
#    define __WORDSIZE		(__SIZEOF_POINTER__ << 3)
#  elif defined(_MIPS_SZPTR)			/* mips irix */
#    if _MIPS_SZPTR == 32
#      define __WORDSIZE	32
#    else
#      define __WORDSIZE	64
#    endif
#  else						/* From FreeBSD 9.1 stdint.h */
#    if defined(UINTPTR_MAX) && defined(UINT64_MAX) && \
	(UINTPTR_MAX == UINT64_MAX)
#      define __WORDSIZE	64
#    else
#      define __WORDSIZE	32
#    endif
#  endif
#endif
#ifndef __LITTLE_ENDIAN
#  if defined(LITTLE_ENDIAN)			/* ppc darwin */
#    define __LITTLE_ENDIAN	LITTLE_ENDIAN
#  elif defined(__ORDER_LITTLE_ENDIAN__)	/* ppc aix */
#    define __LITTLE_ENDIAN	__ORDER_LITTLE_ENDIAN__
#  else
#    define __LITTLE_ENDIAN	1234
#  endif
#endif
#ifndef __BIG_ENDIAN
#  if defined(BIG_ENDIAN)			/* ppc darwin */
#    define __BIG_ENDIAN	BIG_ENDIAN
#  elif defined(__ORDER_BIG_ENDIAN__)		/* ppc aix */
#    define __BIG_ENDIAN	__ORDER_BIG_ENDIAN__
#  else
#    define __BIG_ENDIAN	4321
#  endif
#endif
#ifndef __BYTE_ORDER
#  if defined(BYTE_ORDER)			/* ppc darwin */
#    define __BYTE_ORDER	BYTE_ORDER
#  elif defined(__BYTE_ORDER__)			/* ppc aix */
#    define __BYTE_ORDER	__BYTE_ORDER__
#  elif defined(__i386__)			/* 32 bit x86 solaris */
#    define __BYTE_ORDER	__LITTLE_ENDIAN
#  elif defined(__x86_64__)			/* 64 bit x86 solaris */
#    define __BYTE_ORDER	__LITTLE_ENDIAN
#  elif defined(__MIPSEB)			/* mips irix */
#    define __BYTE_ORDER	__BIG_ENDIAN
#  else
#    error cannot figure __BYTE_ORDER
#  endif
#endif

#if __WORDSIZE == 32
#define CHOOSE_32_64(x, y) x
#elif __WORDSIZE == 64
#define CHOOSE_32_64(x, y) y
#else
#error unhandled __WORDSIZE
#endif

#define WHEN_64(x) CHOOSE_32_64(/**/, x)


#endif /* _jit_endian_h */
