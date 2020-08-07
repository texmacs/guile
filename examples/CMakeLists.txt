add_subdirectory(box)
add_subdirectory(box-dynamic)
add_subdirectory(box-dynamic-module)
add_subdirectory(box-module)
file(GLOB c_scheme_subdirs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/c_scheme ${CMAKE_CURRENT_SOURCE_DIR}/c_scheme/*)
foreach(subdir ${c_scheme_subdirs})
    add_executable(c_scheme_${subdir} c_scheme/${subdir}/main.c)
    set_target_properties(c_scheme_${subdir} PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/c_scheme/${subdir}
            OUTPUT_NAME ${subdir}
            )
    target_link_libraries(c_scheme_${subdir} guile)
    add_custom_command(
            TARGET c_scheme_${subdir}
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/c_scheme/${subdir}/script.scm ${CMAKE_CURRENT_BINARY_DIR}/c_scheme/${subdir}/script.scm
            COMMENT "cp ${CMAKE_CURRENT_SOURCE_DIR}/c_scheme/${subdir}/script.scm ${CMAKE_CURRENT_BINARY_DIR}/c_scheme/${subdir}/script.scm"
    )
endforeach()