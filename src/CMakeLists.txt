## 0. Find all source files ...
file ( GLOB "FILES" RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
  xslib.F90
  xslib_cstring.f90
  xslib_vector.f90
  xslib_time.f90
  xslib_error.f90
  xslib_list.f90
  xslib_fileio.f90
  xslib_groio.f90
  xslib_ndxio.f90
  xslib_pdbio.f90
  xslib_pdhio.f90
  xslib_tplio.f90
  xslib_xtcio.f90
  xslib_xyzio.f90
  xslib_dcdio.f90
  xslib_cubio.f90
  xslib_csvio.f90
  xdrfor.f90
  xdrfile.c
  xdrfile.h
  fileio.h
)

# Preprocess every source file
foreach ( "FILE" ${FILES} )
  configure_file ( ${FILE} "${PROJECT_BINARY_DIR}/src/${FILE}" )
  set ( SRC "${PROJECT_BINARY_DIR}/src/${FILE}" ${SRC} )
endforeach ()

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
## 1. Compile ...

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
## 2. Link libraries ...

target_link_libraries ( ${CMAKE_PROJECT_NAME}_shared
  ${OpenMP_Fortran_LIBRARIES}
  ${OpenMP_C_LIBRARIES}
)

target_link_libraries ( ${CMAKE_PROJECT_NAME}_static
  ${OpenMP_Fortran_LIBRARIES}
  ${OpenMP_C_LIBRARIES}
)

# -----------------------------------------------------------
## 3. Install the compiled library ...

message ( STATUS "Install prefix: ${CMAKE_INSTALL_PREFIX}")
install ( TARGETS ${CMAKE_PROJECT_NAME}_shared LIBRARY DESTINATION lib )
install ( TARGETS ${CMAKE_PROJECT_NAME}_static ARCHIVE DESTINATION lib )

# Install FORTRAN modules in include directory
install ( DIRECTORY "${CMAKE_BINARY_DIR}/include/"
	DESTINATION "${CMAKE_INSTALL_PREFIX}/include"
	FILES_MATCHING PATTERN "*.mod"
)
