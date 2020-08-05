# TODO: How to check function in library
CHECK_LIBRARY_EXISTS (readline "" "" HAVE_LIBREADLINE)
check_function_exists (rl_cleanup_after_signal HAVE_RL_CLEANUP_AFTER_SIGNAL)
check_function_exists (rl_clear_signals HAVE_RL_CLEAR_SIGNALS)
check_function_exists (rl_filename_completion_function HAVE_RL_FILENAME_COMPLETION_FUNCTION)
check_function_exists (rl_getc_function HAVE_RL_GETC_FUNCTION)
check_function_exists (rl_get_keymap HAVE_RL_GET_KEYMAP)
check_function_exists (rl_get_keymap HAVE_RL_GET_KEYMAP)
configure_file (${CMAKE_SOURCE_DIR}/guile-readline/guile-readline-config.h.cmake
        ${CMAKE_BINARY_DIR}//guile-readline/guile-readline-config.h)
add_library(guile-readline SHARED)
target_sources(guile-readline PRIVATE readline.h readline.c)
target_include_directories(guile-readline PRIVATE ${CMAKE_BINARY_DIR}/guile-readline)
target_link_libraries(guile-readline readline)