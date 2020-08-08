set(need_gen_dot_x "test-asmobs-lib")
set(shared_lib "test-asmobs-lib")
set(common_options "-DHAVE_CONFIG_H -g -O2 -Wall -Wmissing-prototypes -I${CMAKE_BINARY_DIR} -I${CMAKE_SOURCE_DIR} -I${CMAKE_CURRENT_BINARY_DIR} ")
file(GLOB c_scheme_subfiles RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/*)
foreach(subfile ${c_scheme_subfiles})
    if (${subfile} MATCHES ".c$")
        message("-- ${subfile}")
        string(REGEX REPLACE ".c$" "" test_target_name ${subfile} )
        if (${test_target_name} IN_LIST shared_lib)
            if (${test_target_name} STREQUAL test-asmobs-lib)
                set(test_target_name test-asmobs)
            endif ()
            add_library(${test_target_name} SHARED ${subfile})
        else()
            add_executable(${test_target_name} ${subfile})
        endif()
        target_link_libraries(${test_target_name} guile)
        target_include_directories(${test_target_name} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
    endif ()
endforeach()

foreach(test_target_name ${need_gen_dot_x})
    execute_process(
            COMMAND ${PYTHON_PATH} ${GEN_DOT_X_PY} ${test_target_name}.c ${CMAKE_CURRENT_BINARY_DIR}/${test_target_name}.x ${common_options}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            COMMAND_ECHO STDOUT
    )
endforeach()

add_custom_command(
        TARGET test-asmobs
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND cp test-asmobs ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "cp test-asmobs ${CMAKE_CURRENT_BINARY_DIR}"
)