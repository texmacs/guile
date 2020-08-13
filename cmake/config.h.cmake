/* config.h.in.  Generated from configure.in by autoheader.  */

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
#cmakedefine CRAY_STACKSEG_END 1

/* Define to 1 if using `alloca.c'. */
#cmakedefine C_ALLOCA 1

/* Define if you want support for debugging Scheme programs. */
#cmakedefine DEBUG_EXTENSIONS 1

/* Define if you want support for dynamic linking. */
#cmakedefine DYNAMIC_LINKING 1

/* Define to the type of elements in the array set by `getgroups'. Usually
   this is either `int' or `gid_t'. */
#define GETGROUPS_T @GETGROUPS_T@

/* Define this to include various undocumented functions used to debug. */
#cmakedefine GUILE_DEBUG 1

/* Define this if you want to debug the free list (helps w/ GC bugs). */
#cmakedefine GUILE_DEBUG_FREELIST 1

/* Define this if you want to debug scm_must_malloc/realloc/free calls. */
#cmakedefine GUILE_DEBUG_MALLOC 1

/* Define to implement scm_internal_select. */
#cmakedefine GUILE_ISELECT 1

/* Define to enable workaround for COOP-linuxthreads compatibility. */
#cmakedefine GUILE_PTHREAD_COMPAT 1

/* Define to 1 if you have `alloca', as a function or macro. */
#cmakedefine HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#cmakedefine HAVE_ALLOCA_H 1

/* Define to 1 if you have the `argz_append' function. */
#cmakedefine HAVE_ARGZ_APPEND 1

/* Define to 1 if you have the `argz_create_sep' function. */
#cmakedefine HAVE_ARGZ_CREATE_SEP 1

/* Define to 1 if you have the <argz.h> header file. */
#cmakedefine HAVE_ARGZ_H 1

/* Define to 1 if you have the `argz_insert' function. */
#cmakedefine HAVE_ARGZ_INSERT 1

/* Define to 1 if you have the `argz_next' function. */
#cmakedefine HAVE_ARGZ_NEXT 1

/* Define to 1 if you have the `argz_stringify' function. */
#cmakedefine HAVE_ARGZ_STRINGIFY 1

/* Define this if you want support for arrays and uniform arrays. */
#cmakedefine HAVE_ARRAYS 1

/* Define to 1 if you have the <assert.h> header file. */
#cmakedefine HAVE_ASSERT_H 1

/* Define to 1 if you have the `atexit' function. */
#cmakedefine HAVE_ATEXIT 1

/* Define to 1 if you have the `bcopy' function. */
#cmakedefine HAVE_BCOPY 1

/* Define to 1 if you have the `chown' function. */
#cmakedefine HAVE_CHOWN 1

/* Define to 1 if you have the `chroot' function. */
#cmakedefine HAVE_CHROOT 1

/* Define to 1 if you have the `closedir' function. */
#cmakedefine HAVE_CLOSEDIR 1

/* Define to 1 if you have the `connect' function. */
#cmakedefine HAVE_CONNECT 1

/* Define to 1 if you have the <crt_externs.h> header file. */
#cmakedefine HAVE_CRT_EXTERNS_H 1

/* Define to 1 if you have the `crypt' function. */
#cmakedefine HAVE_CRYPT 1

/* Define to 1 if you have the <crypt.h> header file. */
#cmakedefine HAVE_CRYPT_H 1

/* Define to 1 if you have the `ctermid' function. */
#cmakedefine HAVE_CTERMID 1

/* Define to 1 if you have the <ctype.h> header file. */
#cmakedefine HAVE_CTYPE_H 1

/* Define to 1 if you have the `cuserid' function. */
#cmakedefine HAVE_CUSERID 1

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#cmakedefine HAVE_DIRENT_H 1

/* Define if you have the GNU dld library. */
#cmakedefine HAVE_DLD 1

/* Define to 1 if you have the <dld.h> header file. */
#cmakedefine HAVE_DLD_H 1

/* Define to 1 if you have the `dlerror' function. */
#cmakedefine HAVE_DLERROR 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#cmakedefine HAVE_DLFCN_H 1

/* Define to 1 if you have the <dl.h> header file. */
#cmakedefine HAVE_DL_H 1

/* Define if you have the _dyld_func_lookup function. */
#cmakedefine HAVE_DYLD 1

/* Define to 1 if you have the `endhostent' function. */
#cmakedefine HAVE_ENDHOSTENT 1

/* Define to 1 if you have the `endnetent' function. */
#cmakedefine HAVE_ENDNETENT 1

/* Define to 1 if you have the `endprotoent' function. */
#cmakedefine HAVE_ENDPROTOENT 1

/* Define to 1 if you have the `endservent' function. */
#cmakedefine HAVE_ENDSERVENT 1

/* Define to 1 if you have the <errno.h> header file. */
#cmakedefine HAVE_ERRNO_H 1

/* Define to 1 if the system has the type `error_t'. */
#cmakedefine HAVE_ERROR_T 1

/* Define to 1 if you have the `fchown' function. */
#cmakedefine HAVE_FCHOWN 1

/* Define to 1 if you have the `fcntl' function. */
#cmakedefine HAVE_FCNTL 1

/* Define to 1 if you have the `flock' function. */
#cmakedefine HAVE_FLOCK 1

/* Define to 1 if you have the `fork' function. */
#cmakedefine HAVE_FORK 1

/* Define to 1 if you have the `ftime' function. */
#cmakedefine HAVE_FTIME 1

/* Define to 1 if you have the `getcwd' function. */
#cmakedefine HAVE_GETCWD 1

/* Define to 1 if you have the `geteuid' function. */
#cmakedefine HAVE_GETEUID 1

/* Define to 1 if you have the `getgrent' function. */
#cmakedefine HAVE_GETGRENT 1

/* Define to 1 if you have the `getgroups' function. */
#cmakedefine HAVE_GETGROUPS 1

/* Define to 1 if you have the `gethostbyname' function. */
#cmakedefine HAVE_GETHOSTBYNAME 1

/* Define to 1 if you have the `gethostent' function. */
#cmakedefine HAVE_GETHOSTENT 1

/* Define to 1 if you have the `gethostname' function. */
#cmakedefine HAVE_GETHOSTNAME 1

/* Define to 1 if you have the `getitimer' function. */
#cmakedefine HAVE_GETITIMER 1

/* Define to 1 if you have the `getlogin' function. */
#cmakedefine HAVE_GETLOGIN 1

/* Define to 1 if you have the `getnetbyaddr' function. */
#cmakedefine HAVE_GETNETBYADDR 1

/* Define to 1 if you have the `getnetbyname' function. */
#cmakedefine HAVE_GETNETBYNAME 1

/* Define to 1 if you have the `getnetent' function. */
#cmakedefine HAVE_GETNETENT 1

/* Define to 1 if you have the `getpass' function. */
#cmakedefine HAVE_GETPASS 1

/* Define to 1 if you have the `getpgrp' function. */
#cmakedefine HAVE_GETPGRP 1

/* Define to 1 if you have the `getppid' function. */
#cmakedefine HAVE_GETPPID 1

/* Define to 1 if you have the `getpriority' function. */
#cmakedefine HAVE_GETPRIORITY 1

/* Define to 1 if you have the `getprotoent' function. */
#cmakedefine HAVE_GETPROTOENT 1

/* Define to 1 if you have the `getpwent' function. */
#cmakedefine HAVE_GETPWENT 1

/* Define to 1 if you have the `getservent' function. */
#cmakedefine HAVE_GETSERVENT 1

/* Define to 1 if you have the `gettimeofday' function. */
#cmakedefine HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the <grp.h> header file. */
#cmakedefine HAVE_GRP_H 1

/* Define to 1 if you have the `hstrerror' function. */
#cmakedefine HAVE_HSTRERROR 1

/* Define if h_errno is declared in netdb.h. */
#cmakedefine HAVE_H_ERRNO 1

/* Define to 1 if you have the `index' function. */
#cmakedefine HAVE_INDEX 1

/* Define to 1 if you have the `inet_aton' function. */
#cmakedefine HAVE_INET_ATON 1

/* Define to 1 if you have the `inet_lnaof' function. */
#cmakedefine HAVE_INET_LNAOF 1

/* Define to 1 if you have the `inet_makeaddr' function. */
#cmakedefine HAVE_INET_MAKEADDR 1

/* Define to 1 if you have the `inet_netof' function. */
#cmakedefine HAVE_INET_NETOF 1

/* Define to 1 if you have the `inet_ntop' function. */
#cmakedefine HAVE_INET_NTOP 1

/* Define to 1 if you have the `inet_pton' function. */
#cmakedefine HAVE_INET_PTON 1

/* Define to 1 if you have the <inttypes.h> header file. */
#cmakedefine HAVE_INTTYPES_H 1

/* Define to 1 if you have the <io.h> header file. */
#cmakedefine HAVE_IO_H 1

/* Define if you want support for IPv6. */
#cmakedefine HAVE_IPV6 1

/* Define to 1 if you have the `kill' function. */
#cmakedefine HAVE_KILL 1

/* Define to 1 if you have the <libc.h> header file. */
#cmakedefine HAVE_LIBC_H 1

/* Define if you have the __libc_stack_end variable. */
#cmakedefine HAVE_LIBC_STACK_END 1

/* Define if you have the libdl library or equivalent. */
#cmakedefine HAVE_LIBDL 1

/* Define to 1 if you have the `m' library (-lm). */
#cmakedefine HAVE_LIBM 1

/* Define to 1 if you have the `nsl' library (-lnsl). */
#cmakedefine HAVE_LIBNSL 1

/* Define to 1 if you have the `pthread' library (-lpthread). */
#cmakedefine HAVE_LIBPTHREAD 1

/* Define to 1 if you have the `rx' library (-lrx). */
#cmakedefine HAVE_LIBRX 1

/* Define to 1 if you have the `socket' library (-lsocket). */
#cmakedefine HAVE_LIBSOCKET 1

/* Define to 1 if you have the <limits.h> header file. */
#cmakedefine HAVE_LIMITS_H 1

/* Define to 1 if you have the `link' function. */
#cmakedefine HAVE_LINK 1

/* Define if the compiler supports long longs. */
#cmakedefine HAVE_LONG_LONGS 1

/* Define to 1 if you have the `lstat' function. */
#cmakedefine HAVE_LSTAT 1

/* Define to 1 if you have the <mach-o/dyld.h> header file. */
#cmakedefine HAVE_MACH_O_DYLD_H 1

/* Define to 1 if you have the <malloc.h> header file. */
#cmakedefine HAVE_MALLOC_H 1

/* Define to 1 if you have the `memcpy' function. */
#cmakedefine HAVE_MEMCPY 1

/* Define to 1 if you have the `memmove' function. */
#cmakedefine HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#cmakedefine HAVE_MEMORY_H 1

/* Define to 1 if you have the `mkdir' function. */
#cmakedefine HAVE_MKDIR 1

/* Define to 1 if you have the `mknod' function. */
#cmakedefine HAVE_MKNOD 1

/* Define to 1 if you have the `mkstemp' function. */
#cmakedefine HAVE_MKSTEMP 1

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
#cmakedefine HAVE_NDIR_H 1

/* Define this if you want support for networking in Guile. */
#cmakedefine HAVE_NETWORKING 1

/* Define to 1 if you have the `nice' function. */
#cmakedefine HAVE_NICE 1

/* Define to 1 if you have the `on_exit' function. */
#cmakedefine HAVE_ON_EXIT 1

/* Define to 1 if you have the `opendir' function. */
#cmakedefine HAVE_OPENDIR 1

/* Define to 1 if you have the `pause' function. */
#cmakedefine HAVE_PAUSE 1

/* Define this if you want support for POSIX system calls in Guile. */
#cmakedefine HAVE_POSIX 1

/* Define if libtool can extract symbol lists from object files. */
#cmakedefine HAVE_PRELOADED_SYMBOLS 1

/* Define to 1 if the system has the type `ptrdiff_t'. */
#cmakedefine HAVE_PTRDIFF_T 1

/* Define to 1 if you have the `putenv' function. */
#cmakedefine HAVE_PUTENV 1

/* Define to 1 if you have the <pwd.h> header file. */
#cmakedefine HAVE_PWD_H 1

/* Define to 1 if you have the `readdir' function. */
#cmakedefine HAVE_READDIR 1

/* Define to 1 if you have the `readlink' function. */
#cmakedefine HAVE_READLINK 1

/* This is included as part of a workaround for a autoheader bug. */
#cmakedefine HAVE_REGCOMP 1

/* Define to 1 if you have the <regex.h> header file. */
#cmakedefine HAVE_REGEX_H 1

/* Define to 1 if you have the `rename' function. */
#cmakedefine HAVE_RENAME 1

/* Define to 1 if system calls automatically restart after interruption by a
   signal. */
#cmakedefine HAVE_RESTARTABLE_SYSCALLS 1

/* Define to 1 if you have the `rindex' function. */
#cmakedefine HAVE_RINDEX 1

/* Define to 1 if you have the `rmdir' function. */
#cmakedefine HAVE_RMDIR 1

/* Define to 1 if you have the <rxposix.h> header file. */
#cmakedefine HAVE_RXPOSIX_H 1

/* Define to 1 if you have the <rx/rxposix.h> header file. */
#cmakedefine HAVE_RX_RXPOSIX_H 1

/* Define to 1 if you have the `select' function. */
#cmakedefine HAVE_SELECT 1

/* Define to 1 if you have the `setegid' function. */
#cmakedefine HAVE_SETEGID 1

/* Define to 1 if you have the `seteuid' function. */
#cmakedefine HAVE_SETEUID 1

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

/* Define if you have the shl_load function. */
#cmakedefine HAVE_SHL_LOAD 1

/* Define to 1 if you have the `sigaction' function. */
#cmakedefine HAVE_SIGACTION 1

/* Define to 1 if you have the `siginterrupt' function. */
#cmakedefine HAVE_SIGINTERRUPT 1

/* Define this if your IPv6 has sin6_scope_id in sockaddr_in6 struct. */
#cmakedefine HAVE_SIN6_SCOPE_ID 1

/* Define to 1 if you have the `socketpair' function. */
#cmakedefine HAVE_SOCKETPAIR 1

/* Define to 1 if you have the <stdint.h> header file. */
#cmakedefine HAVE_STDINT_H 1

/* Define to 1 if you have the <stdio.h> header file. */
#cmakedefine HAVE_STDIO_H 1

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

/* Define to 1 if you have the `strptime' function. */
#cmakedefine HAVE_STRPTIME 1

/* Define to 1 if you have the `strrchr' function. */
#cmakedefine HAVE_STRRCHR 1

/* Define this if your system defines struct linger, for use with the
   getsockopt and setsockopt system calls. */
#cmakedefine HAVE_STRUCT_LINGER 1

/* Define to 1 if `sin6_len' is member of `struct sockaddr_in6'. */
#cmakedefine HAVE_STRUCT_SOCKADDR_IN6_SIN6_LEN 1

/* Define to 1 if `sin_len' is member of `struct sockaddr'. */
#cmakedefine HAVE_STRUCT_SOCKADDR_SIN_LEN 1

/* Define to 1 if `st_blksize' is member of `struct stat'. */
#cmakedefine HAVE_STRUCT_STAT_ST_BLKSIZE 1

/* Define to 1 if `st_blocks' is member of `struct stat'. */
#cmakedefine HAVE_STRUCT_STAT_ST_BLOCKS 1

/* Define to 1 if `st_rdev' is member of `struct stat'. */
#cmakedefine HAVE_STRUCT_STAT_ST_RDEV 1

/* Define this if your system defines struct timespec via <time.h>. */
#cmakedefine HAVE_STRUCT_TIMESPEC 1

/* Define to 1 if `tm_zone' is member of `struct tm'. */
#cmakedefine HAVE_STRUCT_TM_TM_ZONE 1

/* Define to 1 if your `struct stat' has `st_blocks'. Deprecated, use
   `HAVE_STRUCT_STAT_ST_BLOCKS' instead. */
#cmakedefine HAVE_ST_BLOCKS 1

/* Define to 1 if you have the `symlink' function. */
#cmakedefine HAVE_SYMLINK 1

/* Define to 1 if you have the `sync' function. */
#cmakedefine HAVE_SYNC 1

/* Define to 1 if you have the `system' function. */
#cmakedefine HAVE_SYSTEM 1

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
#cmakedefine HAVE_SYS_DIR_H 1

/* Define to 1 if you have the <sys/dl.h> header file. */
#cmakedefine HAVE_SYS_DL_H 1

/* Define to 1 if you have the <sys/file.h> header file. */
#cmakedefine HAVE_SYS_FILE_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#cmakedefine HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
#cmakedefine HAVE_SYS_NDIR_H 1

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

/* Define this if your system defines S_ISLNK in sys/stat.h. */
#cmakedefine HAVE_S_ISLNK 1

/* Define to 1 if you have the `tcgetpgrp' function. */
#cmakedefine HAVE_TCGETPGRP 1

/* Define to 1 if you have the `tcsetpgrp' function. */
#cmakedefine HAVE_TCSETPGRP 1

/* Define to 1 if you have the `times' function. */
#cmakedefine HAVE_TIMES 1

/* Define to 1 if you have the <time.h> header file. */
#cmakedefine HAVE_TIME_H 1

/* Define to 1 if your `struct tm' has `tm_zone'. Deprecated, use
   `HAVE_STRUCT_TM_TM_ZONE' instead. */
#cmakedefine HAVE_TM_ZONE 1

/* Define to 1 if you have the `ttyname' function. */
#cmakedefine HAVE_TTYNAME 1

/* Define to 1 if you don't have `tm_zone' but do have the external array
   `tzname'. */
#cmakedefine HAVE_TZNAME 1

/* Define to 1 if you have the `tzset' function. */
#cmakedefine HAVE_TZSET 1

/* Define if uint32_t typedef is defined when netdb.h is include. */
#cmakedefine HAVE_UINT32_T 1

/* Define to 1 if the system has the type `uintptr_t'. */
#cmakedefine HAVE_UINTPTR_T 1

/* Define to 1 if you have the `uname' function. */
#cmakedefine HAVE_UNAME 1

/* Define to 1 if you have the <unistd.h> header file. */
#cmakedefine HAVE_UNISTD_H 1

/* Define if the system supports Unix-domain (file-domain) sockets. */
#cmakedefine HAVE_UNIX_DOMAIN_SOCKETS 1

/* Define to 1 if you have the `unsetenv' function. */
#cmakedefine HAVE_UNSETENV 1

/* Define to 1 if you have the `usleep' function. */
#cmakedefine HAVE_USLEEP 1

/* Define to 1 if you have the <utime.h> header file. */
#cmakedefine HAVE_UTIME_H 1

/* Define to 1 if you have the `waitpid' function. */
#cmakedefine HAVE_WAITPID 1

/* Define to 1 if you have the <winsock2.h> header file. */
#cmakedefine HAVE_WINSOCK2_H 1

/* Define to 1 if you have the `_NSGetEnviron' function. */
#cmakedefine HAVE__NSGETENVIRON 1

/* Define this if we should include <libc.h> when we've already included
   <unistd.h>. On some systems, they conflict, and libc.h should be omitted.
   See GUILE_HEADER_LIBC_WITH_UNISTD in aclocal.m4. */
#cmakedefine LIBC_H_WITH_UNISTD_H 1

/* Define if localtime caches the TZ setting. */
#cmakedefine LOCALTIME_CACHE 1

/* Define if the OS needs help to load dependent libraries for dlopen(). */
#cmakedefine LTDL_DLOPEN_DEPLIBS 1

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LTDL_OBJDIR "@LTDL_OBJDIR@"

/* Define to the name of the environment variable that determines the dynamic
   library search path. */
#define LTDL_SHLIBPATH_VAR "@LTDL_SHLIBPATH_VAR@"

/* Define to the extension used for shared libraries, say, ".so". */
#define LTDL_SHLIB_EXT "@LTDL_SHLIB_EXT@"

/* Define to the system default library search path. */
#define LTDL_SYSSEARCHPATH "@LTDL_SYSSEARCHPATH@"

/* Define if the operating system supplies sleep without declaring it. */
#cmakedefine MISSING_SLEEP_DECL 1

/* Define if the operating system supplies strptime without declaring it. */
#cmakedefine MISSING_STRPTIME_DECL 1

/* Define if the operating system supplies usleep without declaring it. */
#cmakedefine MISSING_USLEEP_DECL 1

/* Define if dlsym() requires a leading underscore in symbol names. */
#cmakedefine NEED_USCORE 1

/* Define to the address where bug reports for this package should be sent. */
// #cmakedefine PACKAGE_BUGREPORT 1

/* Define to the full name of this package. */
// #cmakedefine PACKAGE_NAME 1

/* Define to the full name and version of this package. */
// #cmakedefine PACKAGE_STRING 1

/* Define to the one symbol short name of this package. */
// #cmakedefine PACKAGE_TARNAME 1

/* Define to the version of this package. */
// #cmakedefine PACKAGE_VERSION 1

/* Define if you want support for debugging Scheme programs. */
#cmakedefine READER_EXTENSIONS 1

/* Define as the return type of signal handlers (`int' or `void'). */
#cmakedefine RETSIGTYPE @RETSIGTYPE@

/* Define this if you want to exclude deprecated features. */
#define SCM_DEBUG_DEPRECATED @SCM_DEBUG_DEPRECATED@

/* Define this if floats are the same size as longs. */
#cmakedefine SCM_SINGLES 1

/* Define this if a callee's stack frame has a higher address than the
   caller's stack frame. On most machines, this is not the case. */
#cmakedefine SCM_STACK_GROWS_UP 1

/* Define this to control the default warning level for deprecated features.
   */
#define SCM_WARN_DEPRECATED_DEFAULT "summary"

/* The size of a `int', as computed by sizeof. */
#define SIZEOF_INT @SIZEOF_INT@

/* The size of a `long', as computed by sizeof. */
#define SIZEOF_LONG @SIZEOF_LONG@

/* The size of a `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG @SIZEOF_LONG_LONG@

/* The size of a `void *', as computed by sizeof. */
#define SIZEOF_VOID_P @SIZEOF_VOID_P@

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
#cmakedefine STACK_DIRECTION 1

/* Define to 1 if you have the ANSI C header files. */
#cmakedefine STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#cmakedefine TIME_WITH_SYS_TIME 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
#cmakedefine TM_IN_SYS_TIME 1

/* Define if using cooperative multithreading. */
#cmakedefine USE_COOP_THREADS 1

/* Define if using any sort of threads. */
#cmakedefine USE_THREADS 1

/* Define if the system headers declare usleep to return void. */
#cmakedefine USLEEP_RETURNS_VOID 1

/* Define this if <utime.h> doesn't define struct utimbuf unless _POSIX_SOURCE
   is defined. See GUILE_STRUCT_UTIMBUF in aclocal.m4. */
#cmakedefine UTIMBUF_NEEDS_POSIX 1

/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
#cmakedefine WORDS_BIGENDIAN 1

/* Define to 1 if on AIX 3.
   System headers sometimes define this.
   We just want to avoid a redefinition error message.  */
#ifndef _ALL_SOURCE
// #undef _ALL_SOURCE
#endif

/* Define to 1 if on MINIX. */
#cmakedefine _MINIX 1

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
#cmakedefine _POSIX_1_SOURCE 1

/* Define to 1 if you need to in order for `stat' and other things to work. */
#cmakedefine _POSIX_SOURCE 1

/* Define to empty if `const' does not conform to ANSI C. */
#cmakedefine const 1

/* Define to a type to use for `error_t' if it is not otherwise available. */
#cmakedefine error_t 1

/* Define to `int' if <sys/types.h> doesn't define. */
#cmakedefine gid_t 1

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
#cmakedefine inline 1
#endif

/* Define to `int' if <sys/types.h> does not define. */
#cmakedefine mode_t 1

/* Define to `int' if <sys/types.h> doesn't define. */
#cmakedefine uid_t 1
