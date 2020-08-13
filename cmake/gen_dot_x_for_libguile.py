import sys
sys.path.append('.')
import argparse
# from gen_dot_x import gen_dox_x
import subprocess
import re
import os
import os.path as osp

def gen_dox_x(input_c_source_file, output_x_file):
    output_path = osp.dirname(output_x_file)
    if not osp.exists(output_path):
        print(f'mkdir {output_path}')
        os.makedirs(output_path)
    with open(output_x_file, 'w') as f:
        print('// make sure file exist. fix xxx.x: No such file or directory when run cpp', file=f)
    if args.msvc:
        cmd = f'/E {input_c_source_file} /DSCM_MAGIC_SNARF_INITS /DSCM_MAGIC_SNARFER {args.other} /I{args.srcdir}/libguile-ltdl'
        os.environ['INCLUDE'] = args.include
    else:
        cmd = f'-E -DSCM_MAGIC_SNARF_INITS -DSCM_MAGIC_SNARFER {input_c_source_file} {args.other}'
    # remove redundant space
    cmd = [osp.basename(args.cc)] + [_cmd for _cmd in cmd.split(' ') if len(_cmd) > 0]
    # print(' '.join(cmd))
    if args.msvc:
        cpp_out_bytes = subprocess.check_output(cmd, shell=True, cwd=osp.dirname(args.cc))
    else:
        cpp_out_bytes = subprocess.check_output(cmd)
    cpp_out = str(cpp_out_bytes, encoding="utf8")
    cpp_out_lines = cpp_out.split(os.linesep)
    cpp_out_lines = [ line.strip() for line in cpp_out_lines if len(line.strip()) > 0]
    need_modify_lines = []

    # grep "^ *\^ *\^" $temp
    for line in cpp_out_lines:
        # print(line)
        if re.match('^ *\^ *\^', line) is not None:
            need_modify_lines.append(line)

    # sed -e "s/^ *\^ *\^//" -e "s/\^\ *:\ *\^.*/;/"
    pattern_begin = re.compile(r'^ *\^ *\^')
    pattern_end = re.compile('\^ *: *\^.*')
    with open(output_x_file, 'w') as f:
        for line in need_modify_lines:
            line = re.sub(pattern_begin, '', line)
            line = re.sub(pattern_end, ';', line)
            print(line, file=f)


def main():
    input_c_source_file_dir = args.srcdir
    output_x_file_dir = args.dstdir
    dot_x_files = """
libguile/gh_io.x    
libguile/rdelim.x
libguile/values.x
libguile/pairs.x
libguile/socket.x
libguile/smob.x
libguile/arbiters.x
libguile/print.x
libguile/backtrace.x
libguile/procprop.x
libguile/objects.x
libguile/filesys.x
libguile/load.x
libguile/gh_eval.x
libguile/continuations.x
libguile/hooks.x
libguile/memmove.x
libguile/macros.x
libguile/eq.x
libguile/symbols.x
libguile/options.x
libguile/gh_data.x
libguile/ports.x
libguile/gh_init.x
libguile/evalext.x
libguile/mallocs.x
libguile/strerror.x
libguile/numbers.x
libguile/keywords.x
libguile/rw.x
libguile/gdbint.x
libguile/vectors.x
libguile/simpos.x
libguile/net_db.x
libguile/gc.x
libguile/hashtab.x
libguile/mkstemp.x
libguile/hash.x
libguile/list.x
libguile/async.x
libguile/regex-posix.x
libguile/properties.x
libguile/strop.x
libguile/srcprop.x
libguile/modules.x
libguile/unif.x
libguile/cpp_err_symbols.x
libguile/cpp_signal.x
libguile/strings.x
libguile/sort.x
libguile/lang.x
libguile/boolean.x
libguile/version.x
libguile/strports.x
libguile/deprecation.x
libguile/guardians.x
libguile/stackchk.x
libguile/gh_list.x
libguile/posix.x
libguile/dynl.x
libguile/throw.x
libguile/strorder.x
libguile/inet_aton.x
libguile/environments.x
libguile/fluids.x
libguile/scmsigs.x
libguile/dynwind.x
libguile/cpp_errno.x
libguile/iselect.x
libguile/gc_os_dep.x
libguile/eval.x
libguile/script.x
libguile/vports.x
libguile/extensions.x
libguile/alist.x
libguile/symbols-deprecated.x
libguile/procs.x
libguile/weaks.x
libguile/gh_funcs.x
libguile/init.x
libguile/cpp_sig_symbols.x
libguile/struct.x
libguile/stime.x
libguile/random.x
libguile/root.x
libguile/variable.x
libguile/ioext.x
libguile/debug.x
libguile/fports.x
libguile/stacks.x
libguile/gh_predicates.x
libguile/alloca.x
libguile/chars.x
libguile/feature.x
libguile/objprop.x
libguile/read.x
libguile/ramap.x
libguile/c-tokenize.x
libguile/gsubr.x
libguile/error.x
libguile/goops.x
"""
    dot_x_files = [it.strip() for row in dot_x_files.split('\n') for it in row.split(' ') if len(row) > 0 if len(it.strip()) > 0]
    # print(dot_x_files)
    for file_x in dot_x_files:
        # file_x = 'libguile/dynl.x'
        print(f'generate {file_x}')
        file = file_x[:-2]
        gen_dox_x(f'{input_c_source_file_dir}/{file}.c', f'{output_x_file_dir}/{file_x}')
        # break

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--cc')
    parser.add_argument('--srcdir')
    parser.add_argument('--dstdir')
    parser.add_argument('--msvc', action='store_true', default=False)
    parser.add_argument('--include')
    parser.add_argument('--other')
    args = parser.parse_args()
    print(args.include)
    main()
    # print(args)
    # cmd='/E C:/msys64/home/pikachu/guile-1.6.7/libguile/gh_io.c /DSCM_MAGIC_SNARF_INITS /DSCM_MAGIC_SNARFER /DHAVE_CONFIG_H /D__MSVC__ /D__USE_W32_SOCKETS#1'
    # cmds = cmd.split(' ')
    # cc = "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC/14.27.29110/bin/Hostx86/x86/cl.exe"
    # print(osp.basename(cc))
    # cwd = osp.dirname(cc)
    # print(cmds)
    # os.environ["INCLUDE"] = args.include
    # subprocess.run([f'cl'] + cmds, shell=True, cwd=cwd)