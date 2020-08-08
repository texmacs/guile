# -*- coding: utf-8 -*-

import sys
import re
import subprocess


def main(argv):
    if len(argv) != 4:
        print("Usage: gen_dot_x.py input_c_source_file output_x_file other_options")
        exit(0)

    input_c_source_file = argv[1]
    output_x_file = argv[2]
    other_options = argv[3]
    with open(output_x_file, 'w') as f:
        print('// make sure file exist. fix xxx.x: No such file or directory when run cpp', file=f)
    cmd = f'gcc -E -DSCM_MAGIC_SNARF_INITS -DSCM_MAGIC_SNARFER {input_c_source_file} {other_options}'
    # remove redundant space
    cmd = [_cmd for _cmd in cmd.split(' ') if len(_cmd) > 0]
    cpp_out_bytes = subprocess.check_output(cmd)
    cpp_out = str(cpp_out_bytes, encoding="utf8")
    cpp_out_lines = cpp_out.split('\n')
    need_modify_lines = []

    # grep "^ *\^ *\^" $temp
    for line in cpp_out_lines:
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
    print('done!')


if __name__ == '__main__':
    main(sys.argv)
