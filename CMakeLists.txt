cmake_minimum_required ( VERSION 2.8.8 )
project ( xslib C Fortran )

# Add /cmake subdirectory
list ( APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" )

# Set project find packages, define Compiler flagsInfo, set Version, and make Public.
include ( FindPackages )
include ( SetCompilerFlags )
include ( SetVersion )
include ( Info )
include ( Public )

# Main build script
add_subdirectory ( src )

# Tests
enable_testing ()
add_subdirectory ( examples )
