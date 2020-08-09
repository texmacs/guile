import sys
sys.path.append('.')
from gen_dot_x import gen_dox_x
def main(argv):
    if len(argv) != 4:
        print("Usage: gen_dot_x_for_libguile.py input_c_source_file_dir output_x_file_dir other_options")
        exit(0)
    input_c_source_file_dir = argv[1]
    output_x_file_dir = argv[2]
    other_options = argv[3]
    dot_x_files = """
    alist.x arbiters.x async.x backtrace.x boolean.x chars.x	
    continuations.x debug.x deprecation.x deprecated.x discouraged.x	
    dynl.x dynwind.x environments.x eq.x error.x eval.x evalext.x	
    extensions.x feature.x fluids.x fports.x futures.x gc.x gc-mark.x	
    gc-segment.x gc-malloc.x gc-card.x goops.x gsubr.x guardians.x	
    hash.x hashtab.x hooks.x i18n.x init.x ioext.x keywords.x lang.x	
    list.x load.x macros.x mallocs.x modules.x numbers.x objects.x	
    objprop.x options.x pairs.x ports.x print.x procprop.x procs.x	
    properties.x random.x rdelim.x read.x root.x rw.x scmsigs.x		
    script.x simpos.x smob.x sort.x srcprop.x stackchk.x stacks.x	
    stime.x strings.x srfi-4.x srfi-13.x srfi-14.x strorder.x		
    strports.x struct.x symbols.x threads.x throw.x values.x		
    variable.x vectors.x version.x vports.x weaks.x ramap.x unif.x 
    dynl.x filesys.x posix.x net_db.x socket.x regex-posix.x
"""
    dot_x_files = [it.strip() for row in dot_x_files.split('\n') for it in row.split(' ') if len(row) > 0 if len(it.strip()) > 0]
    print(dot_x_files)
    for file_x in dot_x_files:
        print(f'generate {file_x}')
        file = file_x[:-2]
        gen_dox_x(f'{input_c_source_file_dir}/{file}.c', f'{output_x_file_dir}/{file_x}', other_options)

if __name__ == '__main__':
    main(sys.argv)