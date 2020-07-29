include_directories(../)
add_executable(box box/box.c)
target_link_libraries(box guile)


add_executable(box-module box-module/box.c)
target_link_libraries(box-module guile)