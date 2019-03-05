# Add all files in src/
file ( GLOB SRC "*.f90" "*.F90" "*.c" )
message ( STATUS "SOURCE files: ${SRC}" )
message ( STATUS "Install prefix : ${CMAKE_INSTALL_PREFIX}")

# The main library to build
add_library ( ${CMAKE_PROJECT_NAME} SHARED ${SRC} )

# Add C Headers
target_include_directories ( ${CMAKE_PROJECT_NAME} PUBLIC "${CMAKE_SOURCE_DIR}/headers" )

# Add some external definitions
add_definitions(-D_VERSION="v${VERSION}")
if ( "${CMAKE_BUILD_TYPE} " STREQUAL "DEBUG " )
	add_definitions(-D_DEBUG=.true.)
endif ()

# Where to put files after they are built, and what to name them
set_target_properties ( ${CMAKE_PROJECT_NAME}
  PROPERTIES
  OUTPUT_NAME ${CMAKE_PROJECT_NAME}
  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
  VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules"
)

# Install the compiled library
install ( TARGETS ${CMAKE_PROJECT_NAME} LIBRARY DESTINATION lib )

# Install FORTRAN module and C header files (in include directory)
install ( DIRECTORY "${CMAKE_BINARY_DIR}/modules/"
	DESTINATION "${CMAKE_INSTALL_PREFIX}/include"
	FILES_MATCHING PATTERN "*.mod"  
	)
install ( DIRECTORY "${CMAKE_SOURCE_DIR}/headers/" 
	DESTINATION "${CMAKE_INSTALL_PREFIX}/include"
	FILES_MATCHING PATTERN "*.h" 
	)