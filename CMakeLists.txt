cmake_minimum_required ( VERSION 2.8.8 )
project ( xslib C Fortran )

# Add /cmake subdirectory
list ( APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" )

# Set project Info, make Public, set Version and define Compiler flags
include ( SetVersion )
include ( Info )
include ( Public )
include ( SetCompilerFlags )

# Look for dependencies
# include ( Dependencies )

# Setup the LAPACK libraries.
# include( SetUpLAPACK.cmake )

# Main build script
add_subdirectory ( src )

# Tests
enable_testing ()
add_subdirectory ( examples )
