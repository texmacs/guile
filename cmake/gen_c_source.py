import sys
import os
import os.path as osp
def main(argv):
    if len(argv) != 2:
        print('Usage: gen_c_source.py guile_srcdir')
    guile_srcdir = argv[1]
    files = ["cpp_err_symbols.in", "cpp_sig_symbols.in"]
    for file in files:
        filename = file[:-3]
        filepath = f'{guile_srcdir}/libguile/{filename}.in'
        output_path = f'{guile_srcdir}/libguile'
        if not osp.exists(output_path):
            print(f'mkdir {output_path}')
            os.makedirs(output_path)
        print(f"generate {filename}.c")
        with open(filepath, 'r') as fin, open(f'{output_path}/{filename}.c', 'w') as fout:
            for line in fin.readlines():
                line = line.strip()
                print(f"""
#ifdef {line}
scm_c_define (\"{line}\", SCM_MAKINUM ({line}));
#endif
                """, file=fout)
    print('done')

if __name__ == '__main__':
    main(sys.argv)