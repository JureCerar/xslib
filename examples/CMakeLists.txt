# Create custom target: 'make check' + add dependency to main build.
add_custom_target( check COMMAND ${CMAKE_CTEST_COMMAND} )
add_dependencies( check ${CMAKE_PROJECT_NAME}_static )

# Include library and module files from the build
link_directories ( "${CMAKE_BINARY_DIR}/lib" )
include_directories ( "${CMAKE_BINARY_DIR}/include" )

# Get source files for tests
file ( GLOB "TEST_SRC" RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.f90" "*.F90" )

# Compile tests as part of 'check'.
foreach ( "FILE" ${TEST_SRC} )
  # Get file name
  get_filename_component ( "TEST" ${FILE} NAME_WE )
  add_executable ( ${TEST} EXCLUDE_FROM_ALL ${FILE} )
  # Linking shared or static library
  target_link_libraries ( ${TEST} ${CMAKE_PROJECT_NAME}_static )
  add_dependencies ( check ${TEST} )

endforeach ()

# -----------------------------------------------------------
# Simple test to see if library runs correctly
add_test ( NAME "simple" COMMAND test_about )

# Error test expects failure.
add_test ( NAME "error" COMMAND test_error )
set_tests_properties ( error PROPERTIES WILL_FAIL TRUE )

# Test each library
add_test ( NAME "cstring" COMMAND test_cstring )
add_test ( NAME "vector" COMMAND test_vector )
add_test ( NAME "xmalloc" COMMAND test_xmalloc )

# Test each I/O library individually
add_test ( NAME "xyzio" COMMAND test_ioutil "files/conf.xyz" )
add_test ( NAME "groio" COMMAND test_ioutil "files/conf.gro" )
add_test ( NAME "pdbio" COMMAND test_ioutil "files/conf.pdb" )
add_test ( NAME "xtcio" COMMAND test_ioutil "files/conf.xtc" )
add_test ( NAME "trrio" COMMAND test_ioutil "files/conf.trr" )
add_test ( NAME "dcdio" COMMAND test_ioutil "files/conf.dcd" )
add_test ( NAME "cubio" COMMAND test_ioutil "files/grid.cub" )

# Test support files I/O
add_test ( NAME "ndxio" COMMAND test_ioutil "files/index.ndx" )
add_test ( NAME "tplio" COMMAND test_ioutil "files/temp.tpl" )

# Test data files I/O
add_test ( NAME "pdhio" COMMAND test_ioutil "files/data.pdh" )
add_test ( NAME "csvio" COMMAND test_ioutil "files/data.csv" )

# -----------------------------------------------------------
# Copy over files needed for running the tests
file ( COPY "${CMAKE_CURRENT_SOURCE_DIR}/files/" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/files/" )
