# Module files from the build
include_directories ( ${CMAKE_BINARY_DIR}/modules )

# Compile all test
foreach ( TESTNAME simple utilities objects )
  add_executable ( ${TESTNAME} ${TESTNAME}.f90 )
  target_link_libraries ( ${TESTNAME} xslib )

endforeach ( TESTNAME )

# Simple test if library is loaded correctly.
add_test ( NAME Simple
  COMMAND simple
)

# Test all utility functions
add_test ( NAME Utilities
  COMMAND utilities
)

# Test all file objects.
add_test ( NAME Objects
  COMMAND objects
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/examples/src"
)
