add_subdirectory(box)
add_subdirectory(box-dynamic)
add_subdirectory(box-dynamic-module)
add_subdirectory(box-module)
file(GLOB c_scheme_subdirs RELATIVE ${CMAKE_SOURCE_DIR}/examples/c_scheme ${CMAKE_SOURCE_DIR}/examples/c_scheme/*)
foreach(subdir ${c_scheme_subdirs})
    add_executable(c_scheme_${subdir} c_scheme/${subdir}/main.c)
    target_link_libraries(c_scheme_${subdir} guile)
    add_custom_command(
            TARGET c_scheme_${subdir}
            COMMAND cp ${CMAKE_SOURCE_DIR}/examples/c_scheme/${subdir}/script.scm ${CMAKE_BINARY_DIR}/examples/script.scm
            COMMENT "cp ${CMAKE_SOURCE_DIR}/examples/c_scheme/${subdir}/script.scm ${CMAKE_BINARY_DIR}/examples/script.scm"
    )
endforeach()