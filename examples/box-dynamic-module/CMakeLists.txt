STRING( REGEX REPLACE ".*/(.*)" "\\1" CURRENT_FOLDER ${CMAKE_CURRENT_SOURCE_DIR} )
add_library(${CURRENT_FOLDER} SHARED box.c )
target_link_libraries(${CURRENT_FOLDER} guile)
set_target_properties(${CURRENT_FOLDER} PROPERTIES OUTPUT_NAME box-module)
add_custom_command(
        TARGET ${CURRENT_FOLDER}
        COMMAND cd ${CMAKE_CURRENT_SOURCE_DIR} && cp box-mixed.scm box-module.scm box-mixed-script.scm box-module-script.scm ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "cd ${CMAKE_CURRENT_SOURCE_DIR} && cp box-mixed.scm box-module.scm box-mixed-script.scm box-module-script.scm ${CMAKE_CURRENT_BINARY_DIR}"
)