cmake_minimum_required(VERSION 3.16)
project(guile VERSION 1.8.8 LANGUAGES C)
set(GUILE_MAJOR_VERSION ${guile_VERSION_MAJOR})
set(GUILE_MINOR_VERSION ${guile_VERSION_MINOR})
set(GUILE_MICRO_VERSION ${guile_VERSION_PATCH})
set(CMAKE_C_STANDARD 90)
message("GUILE ${guile_SOURCE_DIR} ${guile_BINARY_DIR}")
find_program (PYTHON_PATH NAMES python3 python REQUIRED)
if(${PYTHON_PATH} STREQUAL PYTHON_PATH-NOTFOUND)
    message("PYTHON_PATH back to python3")
    set(PYTHON_PATH python3)
else()
    message("find python " ${PYTHON_PATH})
endif()

# Generate c source before cmake handle target
execute_process(
        COMMAND ${PYTHON_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake/gen_c_source.py ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/cmake
        COMMAND_ECHO STDOUT
)

set(GEN_DOT_X_PY ${CMAKE_CURRENT_SOURCE_DIR}/cmake/gen_dot_x.py)
if (UNIX)
    find_package(Threads REQUIRED)
endif ()
include(cmake/AutotoolCheck.cmake)
string(TIMESTAMP BUILDSTAMP "%Y-%m-%d %H:%M")
set(PREFIX "/usr/local")
include_directories(${CMAKE_BINARY_DIR})
include_directories(${CMAKE_SOURCE_DIR})
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/cmake/config.h.cmake ${CMAKE_CURRENT_BINARY_DIR}/config.h)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/cmake/libguile/gen-scmconfig.h.cmake ${CMAKE_CURRENT_BINARY_DIR}/libguile/gen-scmconfig.h)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/cmake/libguile/version.h.cmake ${CMAKE_CURRENT_BINARY_DIR}/libguile/version.h)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/cmake/libguile/libpath.h.cmake ${CMAKE_CURRENT_BINARY_DIR}/libguile/libpath.h)
set(guile_snarf_path ${CMAKE_BINARY_DIR}/libguile/guile-snarf)
if (CMAKE_BUILD_TYPE STREQUAL Debug)
    add_library(guile)
else()
    add_library(guile SHARED)
endif ()

add_definitions(-DHAVE_CONFIG_H)
target_sources(guile
        PRIVATE
        libguile/alist.c
        libguile/alloca.c
        libguile/arbiters.c
        libguile/async.c
        libguile/backtrace.c
        libguile/boolean.c
        libguile/chars.c
        libguile/continuations.c
#        conv-integer.i.c
#        conv-uinteger.i.c
        libguile/convert.c
#        convert.i.c
#        coop-pthreads.c
#        coop.c
        libguile/cpp_err_symbols.c
        libguile/cpp_errno.c
        libguile/cpp_sig_symbols.c
        libguile/cpp_signal.c
#        debug-malloc.c
        libguile/debug.c
        libguile/deprecated.c
        libguile/deprecation.c
        libguile/discouraged.c
        libguile/dynl.c
        libguile/dynwind.c
        libguile/environments.c
        libguile/eq.c
        libguile/error.c
        libguile/eval.c
        libguile/evalext.c
        libguile/extensions.c
        libguile/feature.c
        libguile/filesys.c
        libguile/fluids.c
        libguile/fports.c
        libguile/futures.c
        libguile/gc-card.c
        libguile/gc-freelist.c
        libguile/gc-malloc.c
        libguile/gc-mark.c
        libguile/gc-segment.c
        libguile/gc.c
        libguile/gc_os_dep.c
        libguile/gdbint.c
        libguile/gh_data.c
        libguile/gh_eval.c
        libguile/gh_funcs.c
        libguile/gh_init.c
        libguile/gh_io.c
        libguile/gh_list.c
        libguile/gh_predicates.c
        libguile/goops.c
        libguile/gsubr.c
        libguile/guardians.c
        libguile/guile.c
        libguile/hash.c
        libguile/hashtab.c
        libguile/hooks.c
        libguile/i18n.c
        libguile/inet_aton.c
        libguile/init.c
        libguile/inline.c
        libguile/ioext.c
        libguile/keywords.c
        libguile/lang.c
        libguile/list.c
        libguile/load.c
        libguile/macros.c
        libguile/mallocs.c
#        memmove.c
        libguile/mkstemp.c
        libguile/modules.c
        libguile/net_db.c
        libguile/null-threads.c
        libguile/numbers.c
        libguile/objects.c
        libguile/objprop.c
        libguile/options.c
        libguile/pairs.c
        libguile/ports.c
        libguile/posix.c
        libguile/print.c
        libguile/procprop.c
        libguile/procs.c
        libguile/properties.c
#        putenv.c
#        quicksort.i.c
        libguile/ramap.c
        libguile/random.c
        libguile/rdelim.c
        libguile/read.c
        libguile/regex-posix.c
        libguile/root.c
        libguile/rw.c
        libguile/scmsigs.c
        libguile/script.c
        libguile/simpos.c
        libguile/smob.c
        libguile/socket.c
        libguile/sort.c
        libguile/srcprop.c
        libguile/srfi-13.c
        libguile/srfi-14.c
        libguile/srfi-4.c
#        srfi-4.i.c
        libguile/stackchk.c
        libguile/stacks.c
        libguile/stime.c
        libguile/strerror.c
        libguile/strings.c
        libguile/strorder.c
        libguile/strports.c
        libguile/struct.c
        libguile/symbols.c
        libguile/threads.c
        libguile/throw.c
        libguile/unif.c
        libguile/values.c
        libguile/variable.c
        libguile/vectors.c
        libguile/version.c
        libguile/vports.c
        libguile/weaks.c
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

add_executable(gen-scmconfig libguile/gen-scmconfig.c)
target_include_directories(gen-scmconfig PRIVATE ${guile_BINARY_DIR})
set(common_options "-DHAVE_CONFIG_H -I${guile_SOURCE_DIR} -I${guile_BINARY_DIR}")
add_custom_command(
        TARGET gen-scmconfig
        COMMAND gen-scmconfig > ${guile_BINARY_DIR}/libguile/scmconfig.h
        COMMAND ${PYTHON_PATH}  ${guile_SOURCE_DIR}/cmake/gen_dot_x_for_libguile.py ${guile_SOURCE_DIR}/libguile ${guile_BINARY_DIR}/libguile ${common_options}
        COMMENT "Now Generating libguile/scmconfig.h file"
)

add_dependencies(guile gen-scmconfig)
#add_subdirectory(guile-readline)
#add_subdirectory(examples)
#add_subdirectory(test-suite/standalone)
add_executable(guile-main libguile/guile.c)
target_link_libraries(guile-main guile)
#target_include_directories(guile-main PRIVATE ${guile_SOURCE_DIR})
set_target_properties(guile-main PROPERTIES OUTPUT_NAME guile)


