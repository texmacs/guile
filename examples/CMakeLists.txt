include_directories(../)
add_executable(box box/box.c)
target_link_libraries(box guile)

add_executable(c_scheme c_scheme/main.c)
target_link_libraries(c_scheme guile)
add_custom_command(
        TARGET c_scheme
        COMMAND cp ${CMAKE_SOURCE_DIR}/examples/c_scheme/script.scm ${CMAKE_BINARY_DIR}/examples/script.scm
        COMMENT "cp ${CMAKE_SOURCE_DIR}/examples/c_scheme/script.scm ${CMAKE_BINARY_DIR}/examples/script.scm"
)

add_executable(box-module box-module/box.c)
target_link_libraries(box-module guile)