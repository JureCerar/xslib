# Module files from the build
include_directories ( "${CMAKE_BINARY_DIR}/include" )

# Compile all test
foreach ( TESTNAME simple utilities fileio )
  add_executable ( ${TESTNAME} ${TESTNAME}.f90 )
  target_link_libraries ( ${TESTNAME} ${CMAKE_PROJECT_NAME} )
  add_test ( NAME ${TESTNAME} COMMAND ${TESTNAME} )

endforeach ()

# Copy over files needed for running the tests
file ( COPY "${CMAKE_CURRENT_SOURCE_DIR}/src/" DESTINATION ${CMAKE_CURRENT_BINARY_DIR} )
