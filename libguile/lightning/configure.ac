LIGHTNING_CFLAGS=

AC_CHECK_FUNCS(mremap ffsl isnan isinf,,)

cpu=
case "$target_cpu" in
    i?86|x86_64|amd64)	cpu=x86		;;
    *arm*)		cpu=arm		;;
    *mips*)		cpu=mips	;;
    *powerpc*)		cpu=ppc		;;
    *sparc*)		cpu=sparc	;;
    ia64)		cpu=ia64	;;
    hppa*)		cpu=hppa	;;
    aarch64)		cpu=aarch64	;;
    s390*)		cpu=s390	;;
    alpha*)		cpu=alpha	;;
    *)					;;
esac

if test $cpu = arm; then
     AC_CHECK_LIB(m, sqrtf, ,
        [AC_MSG_ERROR([sqrtf required but not available])])
fi
AC_SUBST(cpu)

AC_SUBST([LIGHTNING_CFLAGS])
