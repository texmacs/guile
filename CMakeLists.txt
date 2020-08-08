cmake_minimum_required(VERSION 3.16)
project(guile VERSION 1.8.8 LANGUAGES C)
set(GUILE_MAJOR_VERSION ${guile_VERSION_MAJOR})
set(GUILE_MINOR_VERSION ${guile_VERSION_MINOR})
set(GUILE_MICRO_VERSION ${guile_VERSION_PATCH})
set(CMAKE_C_STANDARD 90)

# Generate c source before cmake handle target
if (WIN32)
    execute_process(
            COMMAND gen_c_source.bat ${CMAKE_BINARY_DIR}
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
            COMMAND_ECHO STDOUT
    )
else()
    execute_process(
            COMMAND sh gen_c_source.sh ${CMAKE_BINARY_DIR}
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
            COMMAND_ECHO STDOUT
    )
endif ()

find_program (AWK_PATH NAMES awk gawk awk.exe gawk.exe PATH_SUFFIXES /usr/bin REQUIRED)
if(${AWK_PATH} STREQUAL AWK_PATH-NOTFOUND)
    message("AWK_PATH back to awk")
    set(AWK_PATH awk)
endif()
message("find awk " ${AWK_PATH})
find_program (CPP_PATH NAMES cpp REQUIRED)
message("find cpp " ${CPP_PATH})
if (WIN32)
    find_program (BASH_PATH NAMES sh.exe REQUIRED)
    message("find bash " ${BASH_PATH})
    configure_file (${CMAKE_SOURCE_DIR}/gen_dot_x.bat.cmake ${CMAKE_BINARY_DIR}/gen_dot_x.bat)
endif ()

find_program (PYTHON_PATH NAMES python3 python REQUIRED)
message("find python " ${PYTHON_PATH})
set(GEN_DOT_X_PY ${CMAKE_CURRENT_SOURCE_DIR}/gen_dot_x.py)
if (UNIX)
    find_package(Threads REQUIRED)
endif ()
include(cmake/AutotoolCheck.cmake)
string(TIMESTAMP BUILDSTAMP "%Y-%m-%d %H:%M")
set(PREFIX "/usr/local")
include_directories(${CMAKE_BINARY_DIR})
include_directories(${CMAKE_SOURCE_DIR})
configure_file (${CMAKE_SOURCE_DIR}/cmake/config.h.cmake ${CMAKE_BINARY_DIR}/config.h)
configure_file (${CMAKE_SOURCE_DIR}/cmake/libguile/guile-snarf.in.cmake ${CMAKE_BINARY_DIR}/libguile/guile-snarf)
configure_file (${CMAKE_SOURCE_DIR}/cmake/libguile/gen-scmconfig.h.cmake ${CMAKE_BINARY_DIR}/libguile/gen-scmconfig.h)
configure_file (${CMAKE_SOURCE_DIR}/cmake/libguile/version.h.cmake ${CMAKE_BINARY_DIR}/libguile/version.h)
configure_file (${CMAKE_SOURCE_DIR}/cmake/libguile/libpath.h.cmake ${CMAKE_BINARY_DIR}/libguile/libpath.h)
set(guile_snarf_path ${CMAKE_BINARY_DIR}/libguile/guile-snarf)
add_subdirectory(libguile)
add_subdirectory(guile-readline)
add_subdirectory(examples)
add_subdirectory(test-suite/standalone)
add_executable(guile-main main.c)
target_link_libraries(guile-main guile)
set_target_properties(guile-main PROPERTIES OUTPUT_NAME guile)


