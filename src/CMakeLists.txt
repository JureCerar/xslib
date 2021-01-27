## 0. Preprocess all source files
file ( GLOB_RECURSE "FILES" RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.f90" "*.F90" "*.c" "*.h" )
foreach ( "FILE" ${FILES} )
  configure_file ( ${FILE} "${PROJECT_BINARY_DIR}/src/${FILE}" )
  set ( SRC "${PROJECT_BINARY_DIR}/src/${FILE}" ${SRC} )
endforeach ()

# -----------------------------------------------------------
## 1. Create object library

# Include preprocessed source files
include_directories ( "${PROJECT_BINARY_DIR}/src" )

# Create Object library
add_library ( "ObjectLib" OBJECT ${SRC} )
set_target_properties( "ObjectLib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE 1
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include"
)

# -----------------------------------------------------------
## 2. Compile library

# Create shared (dynamic) library
add_library ( ${CMAKE_PROJECT_NAME}_shared SHARED $<TARGET_OBJECTS:ObjectLib> )
set_target_properties ( ${CMAKE_PROJECT_NAME}_shared
  PROPERTIES
  OUTPUT_NAME ${CMAKE_PROJECT_NAME}
  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
  VERSION ${VERSION}
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include"
)

# Create static library
add_library ( ${CMAKE_PROJECT_NAME}_static STATIC $<TARGET_OBJECTS:ObjectLib> )
set_target_properties ( ${CMAKE_PROJECT_NAME}_static
  PROPERTIES
  OUTPUT_NAME ${CMAKE_PROJECT_NAME}
  PREFIX lib
  SOVERSION ${VERSION_MAJOR}.${VERSION_MINOR}
  VERSION ${VERSION}
  ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include"
)

# Add some external definitions
if ( CMAKE_BUILD_TYPE STREQUAL "DEBUG" )
	add_definitions ( -D_DEBUG=.true. )
endif ()

# -----------------------------------------------------------
## 3. Link libraries

target_link_libraries ( ${CMAKE_PROJECT_NAME}_shared
  ${OpenMP_Fortran_LIBRARIES}
  ${OpenMP_C_LIBRARIES}
)

target_link_libraries ( ${CMAKE_PROJECT_NAME}_static
  ${OpenMP_Fortran_LIBRARIES}
  ${OpenMP_C_LIBRARIES}
)

# -----------------------------------------------------------
## 4. Install library

message ( STATUS "Install prefix: ${CMAKE_INSTALL_PREFIX}")
install ( TARGETS ${CMAKE_PROJECT_NAME}_shared LIBRARY DESTINATION lib )
install ( TARGETS ${CMAKE_PROJECT_NAME}_static ARCHIVE DESTINATION lib )

# Install FORTRAN modules in include directory
install ( DIRECTORY "${CMAKE_BINARY_DIR}/include/"
	DESTINATION "${CMAKE_INSTALL_PREFIX}/include/${CMAKE_PROJECT_NAME}"
	FILES_MATCHING PATTERN "*.mod"
)
