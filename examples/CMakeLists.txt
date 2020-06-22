# Create custom target: 'make check' + add dependency to main build.
add_custom_target( check COMMAND ${CMAKE_CTEST_COMMAND} )
add_dependencies( check ${CMAKE_PROJECT_NAME}_static )

# Include module files from the build
include_directories ( "${CMAKE_BINARY_DIR}/include" )

# Test list
set ( TESTS about cstring vector error xmalloc ioutil fileio list )

# Compile tests as part of 'check'.
foreach ( "TEST" ${TESTS} )
  add_executable ( ${TEST} EXCLUDE_FROM_ALL test_${TEST}.f90 )
  # Linking shared library does not work properly? --> keeps linking external xslib
  # target_link_libraries ( ${TEST} ${CMAKE_PROJECT_NAME}_shared )
  target_link_libraries ( ${TEST} ${CMAKE_PROJECT_NAME}_static )
  add_dependencies ( check ${TEST} )

endforeach ()

# -----------------------------------------------------------
# Simple test to see if library runs correctly
add_test ( NAME "simple" COMMAND about )

# Error test expects failure.
add_test ( NAME "error" COMMAND error )
set_tests_properties ( error PROPERTIES WILL_FAIL TRUE )

# Test each library
add_test ( NAME "cstring" COMMAND cstring )
add_test ( NAME "vector" COMMAND vector )
add_test ( NAME "xmalloc" COMMAND xmalloc )

# Test each I/O library individually
add_test ( NAME "xyzio" COMMAND ioutil "files/conf.xyz" )
add_test ( NAME "groio" COMMAND ioutil "files/conf.gro" )
add_test ( NAME "pdbio" COMMAND ioutil "files/conf.pdb" )
add_test ( NAME "xtcio" COMMAND ioutil "files/conf.xtc" )
add_test ( NAME "trrio" COMMAND ioutil "files/conf.trr" )
add_test ( NAME "dcdio" COMMAND ioutil "files/conf.dcd" )
add_test ( NAME "cubio" COMMAND ioutil "files/grid.cub" )

# Test each file with file_t and OpenMP.
add_test ( NAME "master" COMMAND fileio
  "files/conf.xyz"
  "files/conf.gro"
  "files/conf.pdb"
  "files/conf.xtc"
  "files/conf.trr"
  "files/conf.dcd"
)

# Test support files I/O
add_test ( NAME "ndxio" COMMAND ioutil "files/index.ndx" )
add_test ( NAME "tplio" COMMAND ioutil "files/temp.tpl" )

# Test data files I/O
add_test ( NAME "pdhio" COMMAND ioutil "files/data.pdh" )
add_test ( NAME "csvio" COMMAND ioutil "files/data.csv" )

# Linked list test
add_test ( NAME "list" COMMAND list )

# -----------------------------------------------------------
# Copy over files needed for running the tests
file ( COPY "${CMAKE_CURRENT_SOURCE_DIR}/files/" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/files/" )
