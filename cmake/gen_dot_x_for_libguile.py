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

def gen_dot_x_input_names():
    input_c_source_file_dir = args.srcdir
    all_files = os.listdir(input_c_source_file_dir)
    file_names = list(filter(lambda x: x.endswith(".c"), all_files))
    return list(map(lambda x: x.rstrip(".c"), file_names))

def main():
    input_c_source_file_dir = args.srcdir
    output_x_file_dir = args.dstdir

    dot_x_files = list(map(lambda name: f'libguile/{name}.x', gen_dot_x_input_names()))
    
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