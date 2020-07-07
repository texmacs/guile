cmake_minimum_required(VERSION 3.16)
project(guile C)

set(CMAKE_C_STANDARD 99)
add_subdirectory(libguile)
add_executable(guile_main_test main.c)
target_link_libraries(guile_main_test m pthread)