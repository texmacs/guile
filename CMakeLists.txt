cmake_minimum_required(VERSION 3.16)
project(guile VERSION 1.6.7 LANGUAGES C CXX)
set(GUILE_MAJOR_VERSION ${guile_VERSION_MAJOR})
set(GUILE_MINOR_VERSION ${guile_VERSION_MINOR})
set(GUILE_MICRO_VERSION ${guile_VERSION_PATCH})
set(CMAKE_C_STANDARD 90)
set(CMAKE_MODULE_PATH ${guile_SOURCE_DIR}/cmake)
find_program (PYTHON_PATH NAMES python3 python REQUIRED)
if(${PYTHON_PATH} STREQUAL PYTHON_PATH-NOTFOUND)
    message("PYTHON_PATH back to python3")
    set(PYTHON_PATH python3)
else()
    message("find python " ${PYTHON_PATH})
endif()
include(cmake/AutotoolCheck.cmake)
find_package(Math REQUIRED)
add_library(guile)
target_compile_definitions(guile PUBLIC -DHAVE_CONFIG_H)
target_include_directories(guile PUBLIC ${guile_SOURCE_DIR} ${guile_BINARY_DIR} ${guile_SOURCE_DIR}/libguile-ltdl)
set(DEBUG_EXTENSIONS 1)
set(HAVE_ARRAYS 1)
set(HAVE_RESTARTABLE_SYSCALLS 1)
set(HAVE_UINT32_T 1)
set(RETSIGTYPE void)
set(SCM_DEBUG_DEPRECATED 0)
set(SCM_SINGLES 1)
set(GETGROUPS_T gid_t)
set(LTDL_OBJDIR .libs/)
set(LTDL_SHLIBPATH_VAR LD_LIBRARY_PATH)
set(LTDL_SHLIB_EXT .so)
set(LTDL_SYSSEARCHPATH "")
set(SCM_DEBUG_DEPRECATED 0)
set(DYNAMIC_LINKING 1)
set(HAVE_ALLOCA 1)
set(HAVE_ARGZ_STRINGIFY 1)
set(HAVE_CRYPT 1)
set(HAVE_DLERROR 1)
set(HAVE_ERROR_T 1)
set(HAVE_LIBDL 1)
set(HAVE_PRELOADED_SYMBOLS 1)
set(HAVE_SIN6_SCOPE_ID 1)
set(HAVE_ST_BLOCKS 1)
set(MISSING_STRPTIME_DECL 1)
set(READER_EXTENSIONS 1)
if (WIN32)
    # fix: no struct sockaddr_in6
    unset(HAVE_IPV6)
    # fix: error: unknown type name 'error_t'
    # set(HAVE_ARGZ_APPEND 1)
    # set(HAVE_ARGZ_CREATE_SEP 1)
    # set(HAVE_ARGZ_INSERT 1)
    unset(HAVE_ARGZ_STRINGIFY)
    # undefined reference to `crypt'
    unset(HAVE_CRYPT)
    set(HAVE_SELECT 1)
elseif(MSVC)
    unset(HAVE_UNISTD_H)
endif ()
configure_file (${guile_SOURCE_DIR}/cmake/version.h.cmake ${guile_BINARY_DIR}/libguile/version.h)
configure_file (${guile_SOURCE_DIR}/cmake/libpath.h.cmake ${guile_BINARY_DIR}/libguile/libpath.h)

configure_file (${guile_SOURCE_DIR}/cmake/config.h.cmake ${guile_BINARY_DIR}/libguile/scmconfig.h)
configure_file (${guile_SOURCE_DIR}/cmake/config.h.cmake ${guile_BINARY_DIR}/config.h)

execute_process(
    COMMAND ${PYTHON_PATH} ${guile_SOURCE_DIR}/cmake/gen_c_source.py ${guile_SOURCE_DIR}
)

if (MSVC)
    set(common_options "/DHAVE_CONFIG_H /D__MSVC__ /D__USE_W32_SOCKETS#1")
    message(INFO $ENV{INCLUDE})
    set(MSVC_INCLUDE_PATH $ENV{INCLUDE})
    list(JOIN MSVC_INCLUDE_PATH "\\;" MSVC_INCLUDE_PATH_)
    set(MSVC_INCLUDE_PATH "${MSVC_INCLUDE_PATH_}\\;${guile_SOURCE_DIR}\\;${guile_BINARY_DIR}\\;${guile_SOURCE_DIR}/libguile-ltdl")

    execute_process(
            COMMAND ${PYTHON_PATH} ${guile_SOURCE_DIR}/cmake/gen_dot_x_for_libguile.py
            --cc ${CMAKE_C_COMPILER}
            --srcdir ${guile_SOURCE_DIR}
            --dstdir ${guile_BINARY_DIR}
            --msvc --include \"${MSVC_INCLUDE_PATH}\"
            --other ${common_options}
    )
else()
    set(common_options "-DHAVE_CONFIG_H -I${guile_SOURCE_DIR} -I${guile_BINARY_DIR} -I${guile_SOURCE_DIR}/libguile-ltdl")
    execute_process(
            COMMAND ${PYTHON_PATH} ${guile_SOURCE_DIR}/cmake/gen_dot_x_for_libguile.py
            --cc ${CMAKE_C_COMPILER}
            --srcdir ${guile_SOURCE_DIR}
            --dstdir ${guile_BINARY_DIR}
            --other ${common_options}
    )
endif ()
add_executable(guile-main libguile/guile.c)
target_link_libraries(guile-main guile)
if (NOT MSVC)
    target_link_libraries(guile-main dl)
endif ()
target_include_directories(guile-main PRIVATE ${guile_SOURCE_DIR} ${guile_BINARY_DIR} ${guile_SOURCE_DIR}/libguile-ltdl)
if (MSVC)
    message(INFO "MSVC")
    target_compile_definitions(guile PUBLIC -D__MSVC__)
else()
    target_link_libraries(guile-main Math::Math)
endif ()
if (WIN32)
    # undefined reference to `crypt'
    # undefined reference to `regfree'
    target_link_libraries(guile-main ws2_32)
    if (NOT MSVC)
        target_link_libraries(guile-main regex)
    endif ()
else()
    target_link_libraries(guile-main crypt)
endif ()
target_sources(
        guile PRIVATE
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
        #libguile/memmove.c
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
        #libguile/regex-posix.c
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
        #libguile/dynl.c
        libguile/throw.c
        libguile/strorder.c
        #libguile/coop-threads.c
        #libguile/coop.c
        #libguile/debug-malloc.c
        libguile/inet_aton.c
        libguile/environments.c
        libguile/fluids.c
        #libguile/scmsigs.c
        libguile/dynwind.c
        libguile/cpp_errno.c
        libguile/iselect.c
        libguile/gc_os_dep.c
        libguile/eval.c
        libguile/script.c
        libguile/vports.c
        #libguile/extensions.c
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
        #libguile-ltdl/guile-ltdl.c
)
if(MSVC)
    target_sources(guile PRIVATE
            msvc/dynl.c
            msvc/regex-cpp.cpp
            )
else (NOT MSVC)
    target_sources(
            guile PRIVATE
            libguile/regex-posix.c
            libguile/posix.c
            libguile/dynl.c
            libguile/scmsigs.c
            libguile/extensions.c
            libguile-ltdl/guile-ltdl.c
    )
endif ()