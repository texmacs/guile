import os
import os.path as osp

def _s():
    files = os.listdir('libguile')
    files = [ file for file in files if file[-2:]=='.c']
    # print(files)
    for file in files:
        print(f'libguile/{file}')

def _h():
    with open('config.h.in') as f:
        for o_line in f.readlines():
            line = o_line.strip()
            if line.startswith('#undef'):
                # print(line)
                arr = [e for e in line.split(' ') if len(e) > 0]
                print(f'#cmakedefine {arr[1]} 1')
            else:
                print(o_line.rstrip())

srcs="""
        libguile/gh_io.c
        libguile/rdelim.c
        libguile/values.c
        libguile/pairs.c
        libguile/socket.c
        libguile/smob.c
        libguile/arbiters.c
        libguile/print.c
        libguile/backtrace.c
        libguile/procprop.c
        libguile/objects.c
        libguile/filesys.c
        libguile/load.c
        libguile/gh_eval.c
        libguile/continuations.c
        libguile/hooks.c
        libguile/memmove.c
        libguile/macros.c
        libguile/eq.c
        libguile/symbols.c
        libguile/options.c
        libguile/gh_data.c
        libguile/ports.c
        libguile/gh_init.c
        libguile/evalext.c
        libguile/mallocs.c
        libguile/strerror.c
        libguile/numbers.c
        libguile/keywords.c
        libguile/rw.c
        libguile/gdbint.c
        libguile/vectors.c
        libguile/simpos.c
        libguile/net_db.c
        libguile/gc.c
        libguile/hashtab.c
        libguile/mkstemp.c
        libguile/hash.c
        libguile/list.c
        libguile/async.c
        libguile/regex-posix.c
        libguile/properties.c
        libguile/strop.c
        libguile/srcprop.c
        libguile/modules.c
        libguile/unif.c
        #libguile/putenv.c
        libguile/cpp_err_symbols.c
        #libguile/threads.c
        libguile/cpp_signal.c
        libguile/strings.c
        libguile/sort.c
        libguile/lang.c
        #libguile/num2integral.i.c
        libguile/boolean.c
        libguile/version.c
        libguile/strports.c
        libguile/deprecation.c
        libguile/guardians.c
        libguile/stackchk.c
        libguile/gh_list.c
        libguile/posix.c
        libguile/dynl.c
        libguile/throw.c
        libguile/strorder.c
        #libguile/coop-threads.c
        #libguile/coop.c
        #libguile/debug-malloc.c
        libguile/inet_aton.c
        libguile/environments.c
        libguile/fluids.c
        libguile/scmsigs.c
        libguile/dynwind.c
        libguile/cpp_errno.c
        libguile/iselect.c
        libguile/gc_os_dep.c
        libguile/eval.c
        libguile/script.c
        libguile/vports.c
        libguile/extensions.c
        libguile/alist.c
        libguile/symbols-deprecated.c
        libguile/procs.c
        libguile/weaks.c
        libguile/gh_funcs.c
        libguile/init.c
        libguile/cpp_sig_symbols.c
        libguile/struct.c
        libguile/stime.c
        libguile/random.c
        libguile/root.c
        libguile/variable.c
        libguile/ioext.c
        libguile/debug.c
        libguile/fports.c
        libguile/stacks.c
        libguile/gh_predicates.c
        #libguile/num2float.i.c
        libguile/alloca.c
        libguile/chars.c
        libguile/feature.c
        libguile/objprop.c
        libguile/read.c
        libguile/ramap.c
        libguile/c-tokenize.c
        libguile/gsubr.c
        libguile/error.c
        libguile/goops.c
        libguile-ltdl/guile-ltdl.c
"""
for line in srcs.split('\n'):
    line = line.strip()
    if len(line) == 0:
        continue
    if line.startswith("#"):
        continue
    print(f"{line[:-2]}.x")