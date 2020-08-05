add_library(guile-readline SHARED)
target_sources(guile-readline PRIVATE readline.h readline.c)
target_include_directories(guile-readline PRIVATE ${CMAKE_BINARY_DIR}/guile-readline)