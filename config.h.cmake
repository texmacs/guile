/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
systems. This function is required for `alloca.c' support on those systems.
*/
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Define to 1 if translation of program messages to the user's native
language is requested. */
#define ENABLE_NLS 1

/* Define to the type of elements in the array set by `getgroups'. Usually
this is either `int' or `gid_t'. */
#define GETGROUPS_T gid_t

/* Define this if you want to debug scm_must_malloc/realloc/free calls. */
/* #undef GUILE_DEBUG_MALLOC */

/* The imaginary unit (positive square root of -1). */
//#define GUILE_I _Complex_I

/* Define to 1 in order to try to use "64" versions of system and library
calls. */
//#define GUILE_USE_64_CALLS 1

/* Define to 1 if you have the `acosh' function. */
#cmakedefine HAVE_ACOSH 1

/* Define to 1 if you have `alloca', as a function or macro. */
#cmakedefine HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
*/
#cmakedefine HAVE_ALLOCA_H 1

/* Define to 1 if you have the `asinh' function. */
#cmakedefine HAVE_ASINH 1

/* Define to 1 if you have the <assert.h> header file. */
#cmakedefine HAVE_ASSERT_H 1

/* Define to 1 if you have the `atanh' function. */
#cmakedefine HAVE_ATANH 1

/* Define to 1 if you have the `atexit' function. */
#cmakedefine HAVE_ATEXIT 1

/* Define to 1 if you have the `bcopy' function. */
#cmakedefine HAVE_BCOPY 1

/* Define to 1 if you have the `cexp' function. */
#cmakedefine HAVE_CEXP 1

/* Define to 1 if you have the Mac OS X function CFLocaleCopyCurrent in the
CoreFoundation framework. */
/* #undef HAVE_CFLOCALECOPYCURRENT */

/* Define to 1 if you have the Mac OS X function CFPreferencesCopyAppValue in
the CoreFoundation framework. */
/* #undef HAVE_CFPREFERENCESCOPYAPPVALUE */


/* Define to 1 if you have the `chown' function. */
#cmakedefine HAVE_CHOWN 1

/* Define to 1 if you have the `chroot' function. */
#cmakedefine HAVE_CHROOT 1

/* Define to 1 if you have the `chsize' function. */
/* #undef HAVE_CHSIZE */

/* Define to 1 if you have the `clog' function. */
#cmakedefine HAVE_CLOG 1

/* Define to 1 if you have the `clog10' function. */
#cmakedefine HAVE_CLOG10 1

/* Define to 1 if the system has the type `complex double'. */
#cmakedefine HAVE_COMPLEX_DOUBLE 1

/* Define to 1 if you have the <complex.h> header file. */
#cmakedefine HAVE_COMPLEX_H 1

/* Define to 1 if you have the `connect' function. */
#cmakedefine HAVE_CONNECT 1

/* Define to 1 if you have the `copysign' function. */
#cmakedefine HAVE_COPYSIGN 1


/* Define to 1 if you have the <crt_externs.h> header file. */
#cmakedefine HAVE_CRT_EXTERNS_H 1

/* Define to 1 if you have the `crypt' function. */
#cmakedefine HAVE_CRYPT 1

/* Define to 1 if you have the <crypt.h> header file. */
#cmakedefine HAVE_CRYPT_H 1

/* Define to 1 if you have the `ctermid' function. */
#cmakedefine HAVE_CTERMID 1

/* Define to 1 if you have the `cuserid' function. */
// #cmakedefine HAVE_CUSERID 1

/* Define if the GNU dcgettext() function is already present or preinstalled.
*/
#cmakedefine HAVE_DCGETTEXT 1

/* Define to 1 if you have the declaration of `cuserid', and to 0 if you
don't. */
#define HAVE_DECL_CUSERID 1

/* Define to 1 if you have the declaration of `flock', and to 0 if you don't.
*/
#define HAVE_DECL_FLOCK 0

/* Define to 1 if you have the declaration of `hstrerror', and to 0 if you
don't. */
#define HAVE_DECL_HSTRERROR 0

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define this to control the default warning level for deprecated features.
*/
#define SCM_WARN_DEPRECATED_DEFAULT "summary"

/* The size of `char', as computed by sizeof. */
#define SIZEOF_CHAR @SIZEOF_CHAR@

/* The size of `float', as computed by sizeof. */
#define SIZEOF_FLOAT @SIZEOF_FLOAT@

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT @SIZEOF_INT@

/* The size of `intmax_t', as computed by sizeof. */
#define SIZEOF_INTMAX_T @SIZEOF_INTMAX_T@

/* The size of `intptr_t', as computed by sizeof. */
#define SIZEOF_INTPTR_T @SIZEOF_INTPTR_T@

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG @SIZEOF_LONG@

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG @SIZEOF_LONG_LONG@

/* The size of `off_t', as computed by sizeof. */
#define SIZEOF_OFF_T @SIZEOF_OFF_T@

/* The size of `ptrdiff_t', as computed by sizeof. */
#define SIZEOF_PTRDIFF_T @SIZEOF_PTRDIFF_T@

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT @SIZEOF_SHORT@

/* The size of `size_t', as computed by sizeof. */
#define SIZEOF_SIZE_T @SIZEOF_SIZE_T@

/* The size of `uintptr_t', as computed by sizeof. */
#define SIZEOF_UINTPTR_T @SIZEOF_UINTPTR_T@

/* The size of `unsigned char', as computed by sizeof. */
#define SIZEOF_UNSIGNED_CHAR @SIZEOF_UNSIGNED_CHAR@

/* The size of `unsigned int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_INT @SIZEOF_UNSIGNED_INT@

/* The size of `unsigned long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG @SIZEOF_UNSIGNED_LONG@

/* The size of `unsigned long long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG_LONG @SIZEOF_UNSIGNED_LONG_LONG@

/* The size of `unsigned short', as computed by sizeof. */
#define SIZEOF_UNSIGNED_SHORT @SIZEOF_UNSIGNED_SHORT@

/* The size of `unsigned __int64', as computed by sizeof. */
#define SIZEOF_UNSIGNED___INT64 @SIZEOF_UNSIGNED___INT64@

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P @SIZEOF_VOID_P@

/* The size of `__int64', as computed by sizeof. */
#define SIZEOF___INT64 @SIZEOF___INT64@

#define HAVE_DIRENT_H 1

#cmakedefine HAVE_UTIME_H 1


/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#cmakedefine HAVE_STDLIB_H 1

/* Define to 1 if you have the `strchr' function. */
#cmakedefine HAVE_STRCHR 1

/* Define to 1 if you have the `strcmp' function. */
#cmakedefine HAVE_STRCMP 1

/* Define to 1 if you have the `strdup' function. */
#cmakedefine HAVE_STRDUP 1

/* Define to 1 if you have the `strerror' function. */
#cmakedefine HAVE_STRERROR 1

/* Define to 1 if you have the `strftime' function. */
#cmakedefine HAVE_STRFTIME 1

/* Define to 1 if you have the <strings.h> header file. */
#cmakedefine HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#cmakedefine HAVE_STRING_H 1

/* Define to 1 if you have the `strncasecmp' function. */
#cmakedefine HAVE_STRNCASECMP 1

/* Define to 1 if you have the `strptime' function. */
#cmakedefine HAVE_STRPTIME 1


// TODO: replace
/* This is included as part of a workaround for a autoheader bug. */
#define HAVE_REGCOMP 1

/* Define to 1 if you have the <regex.h> header file. */
#cmakedefine HAVE_REGEX_H 1

/* Define to 1 if you have the `rename' function. */
#cmakedefine HAVE_RENAME 1

/* Define to 1 if you have the `rindex' function. */
#cmakedefine HAVE_RINDEX 1

/* Define to 1 if you have the `rmdir' function. */
#cmakedefine HAVE_RMDIR 1


/* Define to 1 if you have the `select' function. */
#cmakedefine HAVE_SELECT 1

/* Define to 1 if you have the `setegid' function. */
#cmakedefine HAVE_SETEGID 1

/* Define to 1 if you have the `seteuid' function. */
#cmakedefine HAVE_SETEUID 1

/* Define to 1 if you have the `setgroups' function. */
#cmakedefine HAVE_SETGROUPS 1

/* Define to 1 if you have the `sethostent' function. */
#cmakedefine HAVE_SETHOSTENT 1

/* Define to 1 if you have the `sethostname' function. */
#cmakedefine HAVE_SETHOSTNAME 1

/* Define to 1 if you have the `setitimer' function. */
#cmakedefine HAVE_SETITIMER 1

/* Define to 1 if you have the `setlocale' function. */
#cmakedefine HAVE_SETLOCALE 1

/* Define to 1 if you have the `setnetent' function. */
#cmakedefine HAVE_SETNETENT 1

/* Define to 1 if you have the `setpgid' function. */
#cmakedefine HAVE_SETPGID 1

/* Define to 1 if you have the `setpriority' function. */
#cmakedefine HAVE_SETPRIORITY 1

/* Define to 1 if you have the `setprotoent' function. */
#cmakedefine HAVE_SETPROTOENT 1

/* Define to 1 if you have the `setpwent' function. */
#cmakedefine HAVE_SETPWENT 1

/* Define to 1 if you have the `setservent' function. */
#cmakedefine HAVE_SETSERVENT 1

/* Define to 1 if you have the `setsid' function. */
#cmakedefine HAVE_SETSID 1

/* Define to 1 if you have the `sigaction' function. */
#cmakedefine HAVE_SIGACTION 1

/* Define to 1 if you have the `siginterrupt' function. */
#cmakedefine HAVE_SIGINTERRUPT 1

// TODO: Check
/* Define this if your IPv6 has sin6_scope_id in sockaddr_in6 struct. */
#define HAVE_SIN6_SCOPE_ID 1

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine HAVE_UNISTD_H 1

/* Define to 1 if you have the declaration of `sethostname', and to 0 if you
don't. */
#define HAVE_DECL_SETHOSTNAME 1


/* Define to 1 if you have the <sys/dir.h> header file. */
#cmakedefine HAVE_SYS_DIR_H 1

/* Define to 1 if you have the <sys/file.h> header file. */
#cmakedefine HAVE_SYS_FILE_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#cmakedefine HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
*/
#cmakedefine HAVE_SYS_NDIR_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
#cmakedefine HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/resource.h> header file. */
#cmakedefine HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#cmakedefine HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#cmakedefine HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/stdtypes.h> header file. */
#cmakedefine HAVE_SYS_STDTYPES_H 1

/* Define to 1 if you have the <sys/timeb.h> header file. */
#cmakedefine HAVE_SYS_TIMEB_H 1

/* Define to 1 if you have the <sys/times.h> header file. */
#cmakedefine HAVE_SYS_TIMES_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#cmakedefine HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#cmakedefine HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/utime.h> header file. */
#cmakedefine HAVE_SYS_UTIME_H 1

/* Define to 1 if you have the <sys/utsname.h> header file. */
#cmakedefine HAVE_SYS_UTSNAME_H 1

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#cmakedefine HAVE_SYS_WAIT_H 1

/* Define if you have the <winsock2.h> header file. */
#cmakedefine HAVE_WINSOCK2_H