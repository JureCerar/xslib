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

# Find OpenMP library 
find_package ( OpenMP )
if ( OpenMP_FOUND )
  list ( APPEND CMAKE_Fortran_FLAGS ${OpenMP_Fortran_FLAGS} )
  list ( APPEND CMAKE_C_FLAGS ${OpenMP_C_FLAGS} )
endif ()
