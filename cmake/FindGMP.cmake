#[=======================================================================[.rst:
FindGMP
-----------

Find the GMP libraries

IMPORTED targets
^^^^^^^^^^^^^^^^

This module defines the following :prop_tgt:`IMPORTED` target:

``GMP::GMP``

Result variables
^^^^^^^^^^^^^^^^

This module will set the following variables if found:

``GMP_INCLUDE_DIRS``
  where to find gmp.h, etc.
``GMP_LIBRARIES``
  the libraries to link against to use GMP.
``GMP_VERSION``
  version of the GMP library found
``GMP_FOUND``
  TRUE if found

#]=======================================================================]

# Look for the necessary header
find_path(GMP_INCLUDE_DIR NAMES gmp.h)
mark_as_advanced(GMP_INCLUDE_DIR)

# Look for the necessary library
find_library(GMP_LIBRARY NAMES gmp libgmp)
mark_as_advanced(GMP_LIBRARY)

if(GMP_INCLUDE_DIR)
    file(STRINGS ${GMP_INCLUDE_DIR}/gmp.h __GNU_MP_VERSION_ver_line
            REGEX "^#define __GNU_MP_VERSION  *[0-9]"
            LIMIT_COUNT 1)
    file(STRINGS ${GMP_INCLUDE_DIR}/gmp.h __GNU_MP_VERSION_MINOR_ver_line
            REGEX "^#define __GNU_MP_VERSION_MINOR  *[0-9]"
            LIMIT_COUNT 1)
    file(STRINGS ${GMP_INCLUDE_DIR}/gmp.h __GNU_MP_VERSION_PATCHLEVEL_ver_line
            REGEX "^#define __GNU_MP_VERSION_PATCHLEVEL  *[0-9]"
            LIMIT_COUNT 1)
    string(REGEX MATCH "[0-9]" GMP_VERSION_MAJOR "${__GNU_MP_VERSION_ver_line}")
    string(REGEX MATCH "[0-9]" GMP_VERSION_MINOR "${__GNU_MP_VERSION_MINOR_ver_line}")
    string(REGEX MATCH "[0-9]" GMP_VERSION_PATCH "${__GNU_MP_VERSION_PATCHLEVEL_ver_line}")
    set(GMP_VERSION "${GMP_VERSION_MAJOR}.${GMP_VERSION_MINOR}.${GMP_VERSION_PATCH}")
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(GMP
        REQUIRED_VARS GMP_INCLUDE_DIR GMP_LIBRARY
        VERSION_VAR GMP_VERSION)

# Create the imported target
if(GMP_FOUND)
    set(GMP_INCLUDE_DIRS ${GMP_INCLUDE_DIR})
    set(GMP_LIBRARIES ${GMP_LIBRARY})
    if(NOT TARGET GMP::GMP)
        add_library(GMP::GMP UNKNOWN IMPORTED)
        set_target_properties(GMP::GMP PROPERTIES
                IMPORTED_LOCATION             "${GMP_LIBRARY}"
                INTERFACE_INCLUDE_DIRECTORIES "${GMP_INCLUDE_DIR}")
    endif()
endif()