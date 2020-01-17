# Module files from the build
include_directories ( "${CMAKE_BINARY_DIR}/include" )

# -----------------------------------------------------------
# Create custom target: 'cmake check'
add_custom_target( check COMMAND ${CMAKE_CTEST_COMMAND} )

# Test list
set ( TESTS simple cstring vector error fileio list )
if ( OpenMP_FOUND )
  set ( TESTS ${TESTS} openmp )
endif ()

# Compile tests as par of 'check'.
foreach ( TEST ${TESTS} )
  add_executable ( ${TEST} EXCLUDE_FROM_ALL test_${TEST}.f90 )
  target_link_libraries ( ${TEST} ${CMAKE_PROJECT_NAME} )
  add_dependencies ( check ${TEST} )

endforeach ()

# -----------------------------------------------------------
# Utilities test
add_test ( NAME "simple" COMMAND simple )
add_test ( NAME "cstring" COMMAND cstring )
add_test ( NAME "vector" COMMAND vector )
add_test ( NAME "error" COMMAND error )

# Error test expects failure.
set_tests_properties ( error PROPERTIES WILL_FAIL TRUE )

# Test each molecular file type individually
add_test ( NAME "xyzio" COMMAND fileio "files/conf.xyz" )
add_test ( NAME "groio" COMMAND fileio "files/conf.gro" )
add_test ( NAME "pdbio" COMMAND fileio "files/conf.pdb" )
add_test ( NAME "xtcio" COMMAND fileio "files/conf.xtc" )
add_test ( NAME "dcdio" COMMAND fileio "files/conf.dcd" )
add_test ( NAME "cubio" COMMAND fileio "files/conf.cub" )

# Test file_t with OpenMP (works with every file type).
if ( OpenMP_FOUND )
  add_test ( NAME "openmp" COMMAND openmp "files/conf.xyz" )
endif()

# Test support files
add_test ( NAME "ndxio" COMMAND fileio "files/index.ndx" )
add_test ( NAME "tplio" COMMAND fileio "files/temp.tpl" )

# Test data files
add_test ( NAME "pdhio" COMMAND fileio "files/data.pdh" )
add_test ( NAME "csvio" COMMAND fileio "files/data.csv" )

# Linked list test
add_test ( NAME "list" COMMAND list )

# -----------------------------------------------------------
# Copy over files needed for running the tests
file ( COPY "${CMAKE_CURRENT_SOURCE_DIR}/files/" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/files/" )
