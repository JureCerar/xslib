# Module files from the build
include_directories( ${CMAKE_BINARY_DIR}/modules )

# List of all test
foreach ( TESTNAME test_simple test_utilities test_files )
    add_executable ( ${TESTNAME} ${TESTNAME}.f90 )
    target_link_libraries ( ${TESTNAME} xslib )
endforeach ( TESTNAME )

# Execute tests on by one
add_test ( "test_simple" test_simple )
add_test ( "test_utilities" test_utilities )
add_test ( "test_files" test_files )

# Copy resource files
file ( COPY resources/ DESTINATION ${CMAKE_BINARY_DIR}/tests/resources/ )
