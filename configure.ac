dnl
dnl Copyright 2000, 2001, 2002, 2012 Free Software Foundation, Inc.
dnl
dnl This is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl This software is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl

AC_PREREQ(2.57)
AC_INIT([GNU lightning], 2.0, pcpa@gnu.org, lightning)
AC_CANONICAL_TARGET
AC_CONFIG_SRCDIR([Makefile.am])
AM_INIT_AUTOMAKE([dist-bzip2])
AC_CONFIG_MACRO_DIR(m4)

AM_CONFIG_HEADER(config.h)

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LIBTOOL

AC_CHECK_LIB(gmp, __gmpz_init, ,
	[AC_MSG_ERROR([GNU MP not found, see http://gmplib.org/])])

AC_ARG_ENABLE(disassembler,
	      AS_HELP_STRING([--enable-disassembler],
			     [Enable jit disassembler using binutils]),
	      [DISASSEMBLER=$enableval], [DISASSEMBLER=auto])
if test "x$DISASSEMBLER" != "xno"; then
    # FIXME need to check for libiberty first or will fail to link
    AC_CHECK_LIB(iberty, htab_try_create, ,
		 [HAVE_IBERTY="no"])
    AC_CHECK_LIB(bfd, bfd_init, ,
		 [HAVE_BFD="no"])
    AC_CHECK_LIB(opcodes, init_disassemble_info, ,
		 [HAVE_OPCODES="no"])
    if test "x$HAVE_IBERTY"  = "xno" -o \
	    "x$HAVE_BFD"     = "xno" -o \
	    "x$HAVE_OPCODES" = "xno"; then
	if test "x$DISASSEMBLER" != "xauto"; then
	    AC_MSG_ERROR([binutils not found, see http://www.gnu.org/software/binutils/])
	else
	    AC_MSG_WARN([binutils not found, see http://www.gnu.org/software/binutils/])
	    DISASSEMBLER="no"
	fi
    fi
fi
AM_CONDITIONAL(with_disassembler, [test "x$DISASSEMBLER" != "xno"])
if test "x$DISASSEMBLER" != "xno"; then
    LIGHTNING_CFLAGS="$LIGHTNING_CFLAGS -DDISASSEMBLER=1"
fi

cpu=
case "$target_cpu" in
    i?x86|x86_64)	cpu=x86		;;
    *arm*)		cpu=arm		;;
    *mips*)		cpu=mips	;;
    *ppc*)		cpu=ppc		;;
    *)					;;
esac
if test x$cpu = x; then
    AC_MSG_ERROR([cpu $target_cpu not supported])
fi
AM_CONDITIONAL(cpu_arm,  [test cpu-$cpu = cpu-arm])
AM_CONDITIONAL(cpu_mips, [test cpu-$cpu = cpu-mips])
AM_CONDITIONAL(cpu_ppc,  [test cpu-$cpu = cpu-ppc])
AM_CONDITIONAL(cpu_x86,  [test cpu-$cpu = cpu-x86])
if test $cpu = arm; then
    AC_CHECK_LIB(m, sqrtf, ,
	[AC_MSG_ERROR([sqrtf required but not available])])
fi

AC_SUBST([LIGHTNING_CFLAGS])

AC_OUTPUT([Makefile
	   doc/Makefile
	   include/Makefile
	   include/lightning/Makefile
	   lib/Makefile
	   check/Makefile])
