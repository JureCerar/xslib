cmake_minimum_required ( VERSION 2.8.8 )
project ( xslib Fortran )

# Compile XDRFILE library
add_subdirectory ( include )

# Add /cmake subdirectory
list ( APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" )

# Set project info
include ( Info )

# Set package version
include ( SetVersion )

# Look for dependencies
# include ( Dependencies )

# Publicize installed location to other CMake projects and pkg-config
include ( Public )

# =================================

# Set compiler
include( SetCompilerFlags )

# Setup the LAPACK libraries.
# include( SetUpLAPACK.cmake )

# =================================

# Add all files in src/
file ( GLOB SRC "src/*.f90" "src/*.F90" )
message ( STATUS "SOURCE files: ${SRC}" )
message ( STATUS "Install prefix : ${CMAKE_INSTALL_PREFIX}")

# The main library to build
add_library ( ${CMAKE_PROJECT_NAME} SHARED ${SRC} )

# Link to xdrfile
target_link_libraries ( ${CMAKE_PROJECT_NAME} xdrfile )
target_include_directories ( ${CMAKE_PROJECT_NAME} PUBLIC "${CMAKE_BINARY_DIR}/include" )

# Add some external definitions
add_definitions(-D_VERSION="v${VERSION}")
if ( "${CMAKE_BUILD_TYPE} " STREQUAL "DEBUG " )
	add_definitions(-D_DEBUG=.true.)
endif ()

# Where to put files after they are built, and what to name them
set_target_properties ( ${CMAKE_PROJECT_NAME}
  PROPERTIES
  OUTPUT_NAME ${CMAKE_PROJECT_NAME}
#  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
  VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules"
)

# Install the compiled library
install ( TARGETS ${CMAKE_PROJECT_NAME} LIBRARY DESTINATION lib )

# Install the module files (in include directory)
install ( DIRECTORY "${CMAKE_BINARY_DIR}/modules/" DESTINATION "${CMAKE_INSTALL_PREFIX}/include" )

####################################################

# Tests
enable_testing ()
add_subdirectory ( tests )
