include_directories(../)
add_executable(box box/box.c)
target_link_libraries(box guile)