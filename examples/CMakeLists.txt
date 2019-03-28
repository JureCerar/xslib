# Module files from the build
include_directories ( ${CMAKE_BINARY_DIR}/modules )

# List of all test
foreach ( TESTNAME simple utilities objects )
    add_executable ( ${TESTNAME} ${TESTNAME}.f90 )
    target_link_libraries ( ${TESTNAME} xslib )
endforeach ( TESTNAME )

# Execute tests on by one
add_test ( "simple" simple )
add_test ( "utilities" utilities )
add_test ( "objects" objects )

# Copy resource files
file ( COPY . DESTINATION ${CMAKE_BINARY_DIR}/examples )
