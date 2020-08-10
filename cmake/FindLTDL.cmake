#[=======================================================================[.rst:
FindLTDL
-----------

Find the LTDL libraries

IMPORTED targets
^^^^^^^^^^^^^^^^

This module defines the following :prop_tgt:`IMPORTED` target:

``LTDL::LTDL``

Result variables
^^^^^^^^^^^^^^^^

This module will set the following variables if found:

``LTDL_INCLUDE_DIRS``
  where to find ltdl.h, etc.
``LTDL_LIBRARIES``
  the libraries to link against to use ltdl.
``LTDL_VERSION``
  version of the ltdl library found
``LTDL_FOUND``
  TRUE if found

#]=======================================================================]

# Look for the necessary header
find_path(LTDL_INCLUDE_DIR NAMES ltdl.h)
mark_as_advanced(LTDL_INCLUDE_DIR)

# Look for the necessary library
find_library(LTDL_LIBRARY NAMES ltdl)
mark_as_advanced(LTDL_LIBRARY)

find_program(LIBTOOL_EXECUTABLE
        NAMES glibtool libtool libtoolize
        )
if ( NOT LIBTOOL_EXECUTABLE STREQUAL "LIBTOOL_EXECUTABLE-NOTFOUND" )
    execute_process(
            COMMAND ${LIBTOOL_EXECUTABLE} --version
            RESULT_VARIABLE RESULT
            OUTPUT_VARIABLE LTDL_VAR_OUTPUT
            OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    if ( RESULT EQUAL 0 )
        string( REGEX REPLACE ".* ([0-9]+\\.[0-9]+\\.[0-9]+).*" "\\1"
                LTDL_VERSION ${LTDL_VAR_OUTPUT} )
    endif ()
endif ()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LTDL
        REQUIRED_VARS LTDL_INCLUDE_DIR LTDL_LIBRARY
        VERSION_VAR LTDL_VERSION)

# Create the imported target
if(LTDL_FOUND)
    set(LTDL_INCLUDE_DIRS ${LTDL_INCLUDE_DIR})
    set(LTDL_LIBRARIES ${LTDL_LIBRARY})
    if(NOT TARGET LTDL::LTDL)
        add_library(LTDL::LTDL UNKNOWN IMPORTED)
        set_target_properties(LTDL::LTDL PROPERTIES
                IMPORTED_LOCATION             "${LTDL_LIBRARY}"
                INTERFACE_INCLUDE_DIRECTORIES "${LTDL_INCLUDE_DIR}")
    endif()
endif()