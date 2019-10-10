## 0. Find all source files ...
file ( GLOB "FILES" RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.f90" "*.c" )

# Preprocess every source file
foreach ( "FILE" ${FILES} )
  configure_file ( ${FILE} "${PROJECT_BINARY_DIR}/src/${FILE}" )
  set ( SRC "${PROJECT_BINARY_DIR}/src/${FILE}" ${SRC} )

endforeach ()

# Include preprocessed source files
include_directories ( "${PROJECT_BINARY_DIR}/src" )


## 1. Compile ...
add_library ( ${CMAKE_PROJECT_NAME} SHARED ${SRC} )

# Where to put files after they are built, and what to name them
set_target_properties ( ${CMAKE_PROJECT_NAME}
  PROPERTIES
  OUTPUT_NAME ${CMAKE_PROJECT_NAME}
  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
  VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include"
)

# Add C Headers
target_include_directories ( ${CMAKE_PROJECT_NAME} PUBLIC "${CMAKE_SOURCE_DIR}/include" )

# Add some external definitions
if ( ${CMAKE_BUILD_TYPE} STREQUAL DEBUG )
	add_definitions ( -D_DEBUG=.true. )
endif ()

## 3. Install the compiled library ...
message ( STATUS "Install prefix: ${CMAKE_INSTALL_PREFIX}")
install ( TARGETS ${CMAKE_PROJECT_NAME} LIBRARY DESTINATION lib )

# Install FORTRAN modules and C headers files (in include directory)
install ( DIRECTORY "${CMAKE_BINARY_DIR}/include/"
	DESTINATION "${CMAKE_INSTALL_PREFIX}/include"
	FILES_MATCHING PATTERN "*.mod"
	)

# install ( DIRECTORY "${CMAKE_SOURCE_DIR}/include/"
# 	DESTINATION "${CMAKE_INSTALL_PREFIX}/include"
# 	FILES_MATCHING PATTERN "*.h"
# 	)
