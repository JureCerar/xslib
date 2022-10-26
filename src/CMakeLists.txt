# This file is part of xslib
# https://github.com/JureCerar/xslib
#
# Copyright (C) 2019-2022 Jure Cerar
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Preprocess all source files
file ( GLOB_RECURSE "FILES" RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.f90" "*.F90" "*.c" "*.h" )
foreach ( "FILE" ${FILES} )
  configure_file ( ${FILE} "${PROJECT_BINARY_DIR}/src/${FILE}" )
  set ( SRC "${PROJECT_BINARY_DIR}/src/${FILE}" ${SRC} )
endforeach ()
include_directories ( "${PROJECT_BINARY_DIR}/src" )

# Create Object library
add_library ( "ObjectLib" OBJECT ${SRC} )
set_target_properties( "ObjectLib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE 1
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/include"
)

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
	add_definitions ( -D_DEBUG=.True. )
endif ()

# Link libraries
target_link_libraries ( ${CMAKE_PROJECT_NAME}_shared
  ${OpenMP_Fortran_LIBRARIES}
  ${OpenMP_C_LIBRARIES}
)
target_link_libraries ( ${CMAKE_PROJECT_NAME}_static
  ${OpenMP_Fortran_LIBRARIES}
  ${OpenMP_C_LIBRARIES}
)

# Install library and include directory
message ( STATUS "Install prefix: ${CMAKE_INSTALL_PREFIX}")
install ( TARGETS ${CMAKE_PROJECT_NAME}_shared LIBRARY DESTINATION lib )
install ( TARGETS ${CMAKE_PROJECT_NAME}_static ARCHIVE DESTINATION lib )
install ( DIRECTORY "${CMAKE_BINARY_DIR}/include/"
	DESTINATION "${CMAKE_INSTALL_PREFIX}/include/${CMAKE_PROJECT_NAME}"
	FILES_MATCHING PATTERN "*.mod"
)
