STRING( REGEX REPLACE ".*/(.*)" "\\1" CURRENT_FOLDER ${CMAKE_CURRENT_SOURCE_DIR} )
add_executable(${CURRENT_FOLDER} box.c )
target_link_libraries(${CURRENT_FOLDER} guile)
set_target_properties(${CURRENT_FOLDER} PROPERTIES OUTPUT_NAME box)
add_custom_command(
        TARGET ${CURRENT_FOLDER}
        COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/script.scm ${CMAKE_CURRENT_BINARY_DIR}/script.scm
        COMMENT "cp ${CMAKE_CURRENT_SOURCE_DIR}/script.scm ${CMAKE_CURRENT_BINARY_DIR}/script.scm"
)