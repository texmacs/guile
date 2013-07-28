dnl
dnl Copyright 2000, 2001, 2002, 2012, 2013 Free Software Foundation, Inc.
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
AC_INIT([GNU lightning], 1.99a, pcpa@gnu.org, lightning)
AC_CANONICAL_TARGET
AC_CONFIG_SRCDIR([Makefile.am])
AM_INIT_AUTOMAKE([dist-bzip2])
AC_CONFIG_MACRO_DIR(m4)

AC_CONFIG_HEADERS(config.h)

case "$target_cpu" in
    ia64)
	case "$host_os" in
	    # Only supported mode
	    *hpux*)	CFLAGS="$CFLAGS -mlp64"	;;
	    *)					;;
	esac					;;
    *mips*)
	case "$host_os" in
	    # Extra flags to build with gcc 3.x
	    *irix*)	CFLAGS="$CFLAGS -D__c99 -Drestrict="	;;
	    *)					;;
	esac					;;
    *)						;;
esac

AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LIBTOOL

AC_CHECK_FUNCS(mremap ffsl getopt_long_only,,)

AC_CHECK_HEADERS([getopt.h],,,)

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

AC_ARG_ENABLE(assertions,
	      AS_HELP_STRING([--enable-assertions],
			     [Enable runtime code generation assertions]),
	      [DEBUG=$enableval], [DEBUG=auto])
if test "x$DEBUG" = xyes; then
    LIGHTNING_CFLAGS="$LIGHTNING_CFLAGS -DDEBUG=1"
else
    LIGHTNING_CFLAGS="$LIGHTNING_CFLAGS -DNDEBUG"
    DEBUG=no
fi

case "$host_os" in
    *hpux*)		SHLIB="-ldld"	;;
    *)			SHLIB="-ldl"	;;
esac
AC_SUBST(SHLIB)

cpu=
case "$target_cpu" in
    i?86|x86_64)	cpu=x86		;;
    *arm*)		cpu=arm		;;
    *mips*)		cpu=mips	;;
    *powerpc*)		cpu=ppc		;;
    *sparc*)		cpu=sparc	;;
    ia64)		cpu=ia64	;;
    hppa*)		cpu=hppa	;;
    aarch64)		cpu=aarch64	;;
    s390x)		cpu=s390x	;;
    *)					;;
esac
AM_CONDITIONAL(cpu_arm,     [test cpu-$cpu = cpu-arm])
AM_CONDITIONAL(cpu_mips,    [test cpu-$cpu = cpu-mips])
AM_CONDITIONAL(cpu_ppc,     [test cpu-$cpu = cpu-ppc])
AM_CONDITIONAL(cpu_sparc,   [test cpu-$cpu = cpu-sparc])
AM_CONDITIONAL(cpu_x86,     [test cpu-$cpu = cpu-x86])
AM_CONDITIONAL(cpu_ia64,    [test cpu-$cpu = cpu-ia64])
AM_CONDITIONAL(cpu_hppa,    [test cpu-$cpu = cpu-hppa])
AM_CONDITIONAL(cpu_aarch64, [test cpu-$cpu = cpu-aarch64])
AM_CONDITIONAL(cpu_s390x,   [test cpu-$cpu = cpu-s390x])

# Test x87 if both, x87 and sse2 available
ac_cv_test_x86_x87=
# Test arm instruction set if thumb instruction set available
ac_cv_test_arm_arm=
# Test sofware float if vfp available and not using hard float abi
ac_cv_test_arm_swf=

save_CFLAGS=$CFLAGS
CFLAGS="-I$PWD/include -D_GNU_SOURCE"
if test x$cpu = x; then
    AC_MSG_ERROR([cpu $target_cpu not supported])
elif test $cpu = x86; then
    AC_RUN_IFELSE([AC_LANG_SOURCE([[
    #include <lightning.h>
    int main(void) {
	int		    ac, flags;
	unsigned int	    eax, ebx, ecx, edx;
	if (__WORDSIZE == 64)
	    return 1;
	__asm__ volatile ("pushfl;\n\t"
			  "popl %0;\n\t"
			  "movl \$0x240000, %1;\n\t"
			  "xorl %0, %1;\n\t"
			  "pushl %1;\n\t"
			  "popfl;\n\t"
			  "pushfl;\n\t"
			  "popl %1;\n\t"
			  "xorl %0, %1;\n\t"
			  "pushl %0;\n\t"
			  "popfl"
			  : "=r" (flags), "=r" (ac));
	if ((ac & (1 << 21)) == 0)
	    return 1;
	__asm__ volatile ("xchgl %%ebx, %1; cpuid; xchgl %%ebx, %1"
			  : "=a" (eax), "=r" (ebx),
			  "=c" (ecx), "=d" (edx)
			  : "0" (1));
	return (edx & 1 << 26) ? 0 : 1;
    }
    ]])],[ac_cv_test_x86_x87=yes],[][])
elif test $cpu = arm; then
    AC_RUN_IFELSE([AC_LANG_SOURCE([[
    #include <stdio.h>
    int main(void) {
    #if defined(__linux__)
	FILE	*fp;
	char	 buf[128];
	if ((fp = fopen("/proc/cpuinfo", "r")) == NULL)
	    return 1;
	while (fgets(buf, sizeof(buf), fp)) {
	    if (strncmp(buf, "Features\t:", 10) == 0 &&
		strstr(buf + 10, "thumb")) {
		fclose(fp);
		return 0;
	    }
	}
	fclose(fp);
    #endif
	return 1;
    }
    ]])],[ac_cv_test_arm_arm=yes],[][])
    AC_RUN_IFELSE([AC_LANG_SOURCE([[
    #include <stdio.h>
    int main(void) {
    #if defined(__linux__)
	FILE	*fp;
	char	 buf[128];
    #  if !defined(__ARM_PCS_VFP)
	if ((fp = fopen("/proc/cpuinfo", "r")) == NULL)
	    return 1;
	while (fgets(buf, sizeof(buf), fp)) {
	    if (strncmp(buf, "Features\t:", 10) == 0 &&
		strstr(buf + 10, "vfp")) {
		fclose(fp);
		return 0;
	    }
	}
	fclose(fp);
    #  endif
    #endif
	return 1;
    }
    ]])],[ac_cv_test_arm_swf=yes],[][])
fi
CFLAGS=$save_CFLAGS

AM_CONDITIONAL(test_x86_x87, [test x$ac_cv_test_x86_x87 = xyes])
AM_CONDITIONAL(test_arm_arm, [test x$ac_cv_test_arm_arm = xyes])
AM_CONDITIONAL(test_arm_swf, [test x$ac_cv_test_arm_swf = xyes])

if test $cpu=arm; then
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
