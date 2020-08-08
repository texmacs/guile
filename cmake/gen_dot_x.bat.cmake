@echo off
set files=alist arbiters async backtrace boolean chars continuations debug deprecation deprecated discouraged dynl dynwind environments eq error eval evalext extensions feature fluids fports futures gc gc-mark gc-segment gc-malloc gc-card goops gsubr guardians hash hashtab hooks i18n init ioext keywords lang list load macros mallocs modules numbers objects objprop options pairs ports print procprop procs properties random rdelim read root rw scmsigs script simpos smob sort srcprop stackchk stacks stime strings srfi-4 srfi-13 srfi-14 strorder strports struct symbols threads throw values variable vectors version vports weaks ramap unif dynl filesys posix net_db socket regex-posix


set srcdir=%1
set dstdir=%2
echo dstdir: %dstdir%
set guile_snarf=%dstdir%/libguile/guile-snarf
for %%I in (%files%) do (
echo handle %%I.c
@BASH_PATH@ %guile_snarf% -o %dstdir%/libguile/%%I.x %srcdir%/libguile/%%I.c -DHAVE_CONFIG_H -I%srcdir% -g -O2 -Wall -Wmissing-prototypes -I%dstdir%
)