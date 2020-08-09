if (CMAKE_BUILD_TYPE STREQUAL Debug)
    add_library(guile)
else()
    add_library(guile SHARED)
endif ()

add_definitions(-DHAVE_CONFIG_H)
target_sources(guile
        PRIVATE
        alist.c
        alloca.c
        arbiters.c
        async.c
        backtrace.c
        boolean.c
        chars.c
        continuations.c
#        conv-integer.i.c
#        conv-uinteger.i.c
        convert.c
#        convert.i.c
#        coop-pthreads.c
#        coop.c
        cpp_err_symbols.c
        cpp_errno.c
        cpp_sig_symbols.c
        cpp_signal.c
#        debug-malloc.c
        debug.c
        deprecated.c
        deprecation.c
        discouraged.c
        dynl.c
        dynwind.c
        environments.c
        eq.c
        error.c
        eval.c
        evalext.c
        extensions.c
        feature.c
        filesys.c
        fluids.c
        fports.c
        futures.c
        gc-card.c
        gc-freelist.c
        gc-malloc.c
        gc-mark.c
        gc-segment.c
        gc.c
        gc_os_dep.c
        gdbint.c
        gh_data.c
        gh_eval.c
        gh_funcs.c
        gh_init.c
        gh_io.c
        gh_list.c
        gh_predicates.c
        goops.c
        gsubr.c
        guardians.c
        guile.c
        hash.c
        hashtab.c
        hooks.c
        i18n.c
        inet_aton.c
        init.c
        inline.c
        ioext.c
        keywords.c
        lang.c
        list.c
        load.c
        macros.c
        mallocs.c
#        memmove.c
        mkstemp.c
        modules.c
        net_db.c
        null-threads.c
        numbers.c
        objects.c
        objprop.c
        options.c
        pairs.c
        ports.c
        posix.c
        print.c
        procprop.c
        procs.c
        properties.c
#        putenv.c
#        quicksort.i.c
        ramap.c
        random.c
        rdelim.c
        read.c
        regex-posix.c
        root.c
        rw.c
        scmsigs.c
        script.c
        simpos.c
        smob.c
        socket.c
        sort.c
        srcprop.c
        srfi-13.c
        srfi-14.c
        srfi-4.c
#        srfi-4.i.c
        stackchk.c
        stacks.c
        stime.c
        strerror.c
        strings.c
        strorder.c
        strports.c
        struct.c
        symbols.c
        threads.c
        throw.c
        unif.c
        values.c
        variable.c
        vectors.c
        version.c
        vports.c
        weaks.c
        )
if (WIN32)
    target_sources(guile PRIVATE
#            win32-dirent.c
            win32-socket.c
            win32-uname.c
            )
endif ()
target_include_directories(guile PUBLIC ${guile_BINARY_DIR} ${guile_SOURCE_DIR})
if (WIN32)
    target_link_libraries(guile  m pthread gmp ltdl ws2_32 regex intl)
else()
    target_link_libraries(guile  m Threads::Threads gmp ltdl crypt)
endif ()

add_executable(gen-scmconfig gen-scmconfig.c)
target_include_directories(gen-scmconfig PRIVATE ${guile_BINARY_DIR})
set(common_options "-DHAVE_CONFIG_H -I${guile_SOURCE_DIR} -I${guile_BINARY_DIR}")
add_custom_command(
        TARGET gen-scmconfig
        COMMAND gen-scmconfig > ${CMAKE_CURRENT_BINARY_DIR}/scmconfig.h
        COMMAND ${PYTHON_PATH}  ${guile_SOURCE_DIR}/cmake/gen_dot_x_for_libguile.py ${guile_SOURCE_DIR}/libguile ${guile_BINARY_DIR}/libguile ${common_options}
        COMMENT "Now Generating libguile/scmconfig.h file"
)

add_dependencies(guile gen-scmconfig)