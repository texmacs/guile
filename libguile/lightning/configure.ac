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
AM_CONDITIONAL(cpu_arm,     [test cpu-$cpu = cpu-arm])
AM_CONDITIONAL(cpu_mips,    [test cpu-$cpu = cpu-mips])
AM_CONDITIONAL(cpu_ppc,     [test cpu-$cpu = cpu-ppc])
AM_CONDITIONAL(cpu_sparc,   [test cpu-$cpu = cpu-sparc])
AM_CONDITIONAL(cpu_x86,     [test cpu-$cpu = cpu-x86])
AM_CONDITIONAL(cpu_ia64,    [test cpu-$cpu = cpu-ia64])
AM_CONDITIONAL(cpu_hppa,    [test cpu-$cpu = cpu-hppa])
AM_CONDITIONAL(cpu_aarch64, [test cpu-$cpu = cpu-aarch64])
AM_CONDITIONAL(cpu_s390,    [test cpu-$cpu = cpu-s390])
AM_CONDITIONAL(cpu_alpha,   [test cpu-$cpu = cpu-alpha])

if test $cpu = arm; then
     AC_CHECK_LIB(m, sqrtf, ,
        [AC_MSG_ERROR([sqrtf required but not available])])
fi
AC_SUBST(cpu)

AC_SUBST([LIGHTNING_CFLAGS])
