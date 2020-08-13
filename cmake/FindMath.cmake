# https://android.googlesource.com/platform/external/eigen/+/master/cmake/FindStandardMathLibrary.cmake
# Look for the necessary library
# Look for the necessary header
find_path(MATH_INCLUDE_DIR NAMES math.h)
mark_as_advanced(MATH_INCLUDE_DIR)

# Look for the necessary library
find_library(MATH_LIBRARY NAMES m libm)
mark_as_advanced(MATH_LIBRARY)
include(CheckCSourceCompiles)
set(find_math_library_test_program
        "#include<math.h>
int main() { sin(0.0); log(0.0f); }")
set(CMAKE_REQUIRED_FLAGS "")
set(CMAKE_REQUIRED_LIBRARIES "m")
check_c_source_compiles(
        "${find_math_library_test_program}"
        math_library_linked_to_as_m)
if(math_library_linked_to_as_m)
    include(FindPackageHandleStandardArgs)
    find_package_handle_standard_args(Math REQUIRED_VARS MATH_LIBRARY)
endif()

# Create the imported target
if(Math_FOUND)
    set(MATH_LIBRARIES ${MATH_LIBRARY})
    if(NOT TARGET Math::Math)
        add_library(Math::Math UNKNOWN IMPORTED)
        set_target_properties(Math::Math PROPERTIES
                IMPORTED_LOCATION             "${MATH_LIBRARY}")
    endif()
endif()