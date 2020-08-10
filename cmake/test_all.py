# -*- coding: utf-8 -*-

import os
import sys
import subprocess
import platform

test_ret = {
    'fail': 0,
    'pass': 0
}


def print_test_header(s: str):
    print('=' * len(s))
    print(s)


def print_test_footer(s: str):
    print(s)
    print('=' * len(s))


def print_dir(s: str):
    s = f'test {s}'
    padding = 10
    pad_space = ' ' * padding
    print('=' * (len(s) + (padding + 1) * 2))
    print(f'={pad_space}{s}{pad_space}=')
    print('=' * (len(s) + (padding + 1) * 2))


def run_cmd(cmd: str, work_dir: str):
    if platform.system() == 'Windows':
        if cmd.startswith("./"):
            cmd = cmd[2:]
    print(f'work dir: {work_dir}')
    print(cmd)
    cmd = [_cmd for _cmd in cmd.split(' ') if len(_cmd) > 0]

    # subprocess.run(cmd, shell=True, check=True, cwd=work_dir, env=os.environ)
    # cmd = ['cd', work_dir, '&&'] + cmd
    # print(cmd)
    # ret = subprocess.check_output(cmd, cwd=work_dir, shell=True)
    # print(ret)
    ret = subprocess.call(cmd, cwd=work_dir, shell=False)
    if ret != 0:
        print('FAIL')
        test_ret['fail'] += 1
    else:
        print('PASS')
        test_ret['pass'] += 1


def run_test(name: str, work_dir: str, cmd: str = None, fn=None):
    guile_load_path_save = os.getenv("GUILE_LOAD_PATH")
    os.environ["GUILE_LOAD_PATH"]=f'{work_dir}{sep}{guile_load_path_save}'
    print_test_header(name)
    if cmd is not None:
        run_cmd(cmd, work_dir)
    if fn is not None:
        fn(name, work_dir)
    print_test_footer(name + ' done')
    os.environ["GUILE_LOAD_PATH"]=guile_load_path_save


def print_test_report():
    total = test_ret['pass'] + test_ret['fail']
    print(f'total: {total}')
    print(f'pass: {test_ret["pass"]}')
    print(f'fail: {test_ret["fail"]}')


def main(argv):
    if len(argv) != 3:
        print('Usage: test_all.py srcdir dstdir')
        exit(0)
    guile_srcdir = argv[1]
    guile_dstdir = argv[2]
    guile_main = f'{guile_dstdir}/guile'
    os.environ['PATH'] = f'{guile_dstdir}{sep}{os.getenv("PATH")}'
    if platform.system() != 'Windows':
        os.environ['GUILE_LOAD_PATH'] = '.'
        os.environ['LD_LIBRARY_PATH'] = '.'
    os.environ['GUILE_LOAD_PATH'] = f'{guile_srcdir}{sep}{os.getenv("GUILE_LOAD_PATH")}'
    os.environ['LD_LIBRARY_PATH'] = f'{guile_dstdir}{sep}{os.getenv("LD_LIBRARY_PATH")}'
    print(os.getenv('GUILE_LOAD_PATH'))
    print_dir('example')
    run_test('example box', f'{guile_dstdir}/examples/box', './box script.scm')
    run_test('example box-dynamic', f'{guile_dstdir}/examples/box-dynamic', f'{guile_main} script.scm')

    def run_test_example_box_dynamic_module(name, work_dir):
        os.environ['GUILE_LOAD_PATH'] = f'{guile_srcdir}{sep}{work_dir}'
        for subtest in ['box-mixed', 'box-module']:
            print(f'> run {subtest}')
            cmd = f'{guile_main} {subtest}-script.scm'
            run_cmd(cmd, work_dir)

    run_test('example box-dynamic-module', f'{guile_dstdir}/examples/box-dynamic-module',
             fn=run_test_example_box_dynamic_module)
    run_test('example box-module', f'{guile_dstdir}/examples/box-module', './box script.scm')

    def run_test_example_c_scheme(name, work_dir):
        for loop in [1, 2, 3]:
            demo_name = f'demo{loop}'
            print(f'> run {demo_name}')
            run_cmd(f'./{demo_name} script.scm', f"{work_dir}/{demo_name}")

    run_test('example c_scheme', f'{guile_dstdir}/examples/c_scheme', fn=run_test_example_c_scheme)
    run_test('examples modules', f'{guile_srcdir}/examples/modules', f'{guile_main} main')
    def run_test_example_safe(name, work_dir):
        for loop in ['evil', 'untrusted']:
            print(f'> {loop}')
            run_cmd(f'{guile_main} -s safe {loop}.scm', work_dir)

    run_test('examples safe', f'{guile_srcdir}/examples/safe', fn=run_test_example_safe)

    def run_test_example_scripts(name, work_dir):
        print('> run fact 5')
        run_cmd(f'{guile_main} fact 5', work_dir)
        for loop in ['hello', 'simple-hello.scm']:
            print(f'> run {loop}')
            run_cmd(f'{guile_main} {loop}', work_dir)

    run_test('exmaple scripts', f'{guile_srcdir}/examples/scripts', fn=run_test_example_scripts)

    print_dir('test-suite')
    os.environ['builddir'] = f'{guile_dstdir}/test-suite/standalone'
    run_test('test-suite standalone test-asmobs', f'{guile_dstdir}/test-suite/standalone', f'{guile_main} test-asmobs')
    standalone_exe_list = 'test-conversion test-gh test-list test-num2integral test-round test-scm-c-read ' \
                          'test-scm-take-locale-symbol test-scm-with-guile test-unwind test-with-guile-module'
    for loop in standalone_exe_list.split(' '):
        run_test(f'test-suite standalone {loop}', f'{guile_dstdir}/test-suite/standalone', f'./{loop}')

    if platform.system() == 'Linux':
        run_test(f'test-suite standalone test-require-extension', f'{guile_srcdir}/test-suite/standalone',
                 f'sh test-require-extension')
    else:
        print('skip test-require-extension')

    run_test(f'test-suite standalone test-system-cmds', f'{guile_srcdir}/test-suite/standalone',
             f'{guile_main} test-system-cmds')

    guile_test = f'{guile_srcdir}/test-suite/guile-test'
    run_test('test-suite tests', f'{guile_srcdir}/test-suite', f'{guile_main} -e main -s guile-test --test-suite tests')
    print_test_report()
    print('all done!!!')


if __name__ == '__main__':
    if platform.system() == 'Windows':
        sep = ';'
        os.environ['PATH'] = f'C:\\msys64\\mingw32\\bin;{os.getenv("PATH")}'
    else:
        sep = ':'

    main(sys.argv)
    # run_cmd('box script.scm', f"{sys.argv[2]}\\examples\\box")
