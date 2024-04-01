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

# Include library and module files from the build
link_directories ( "${CMAKE_BINARY_DIR}/lib" )
include_directories ( "${CMAKE_BINARY_DIR}/include" )

# Get test source files and copy sample files.
file ( GLOB "TEST_SRC" RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.f90" "*.F90" )

# Make custom 'check' compile target.
add_custom_target ( check COMMAND ${CMAKE_CTEST_COMMAND} )
add_dependencies ( check ${CMAKE_PROJECT_NAME}_static )

# Compile tests as part of 'check'.
foreach ( "FILE" ${TEST_SRC} )
  get_filename_component ( "NAME" ${FILE} NAME_WE )
  add_executable ( ${NAME} EXCLUDE_FROM_ALL ${FILE} )
  target_link_libraries ( ${NAME} ${CMAKE_PROJECT_NAME}_static )
  add_dependencies ( check ${NAME} )
endforeach ()

# Add tests
add_test ( NAME "xslib" COMMAND xslib )
add_test ( NAME "array" COMMAND array )
add_test ( NAME "cstring" COMMAND cstring )
add_test ( NAME "dict" COMMAND dict )
add_test ( NAME "errorh" COMMAND errorh )
set_tests_properties ( "errorh" PROPERTIES WILL_FAIL TRUE )
add_test ( NAME "fitting" COMMAND fitting )
add_test ( NAME "list" COMMAND list )
add_test ( NAME "math" COMMAND math )
add_test ( NAME "pathlib" COMMAND pathlib )
add_test ( NAME "sort" COMMAND sort )
add_test ( NAME "stats" COMMAND stats )
add_test ( NAME "time" COMMAND time )
add_test ( NAME "vector" COMMAND vector )
add_test ( NAME "xmalloc" COMMAND xmalloc )
