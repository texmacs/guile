cmake_minimum_required(VERSION 3.16)
project(guile C)

set(CMAKE_C_STANDARD 90)
set(CMAKE_INCLUDE_CURRENT_DIR ON)
add_subdirectory(libguile)
add_subdirectory(examples)
add_executable(guile-main main.c)
target_link_libraries(guile-main guile)