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

# Set `RELEASE` as default build type
if ( NOT CMAKE_BUILD_TYPE )
  set ( CMAKE_BUILD_TYPE "RELEASE" CACHE STRING "Build configuration 'RELEASE' or 'DEBUG'." FORCE )
  message ( STATUS "CMAKE_BUILD_TYPE not given, defaulting to RELEASE.")
else ()
  message ( STATUS "CMAKE_BUILD_TYPE is: ${CMAKE_BUILD_TYPE}")
endif ()

# Fortran compiler settings
if ( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" )
  set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -march=native" )
  set ( CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffast-math -flto -fexcess-precision=fast -funroll-loops" )
  set ( CMAKE_Fortran_FLAGS_DEBUG "-g -Og -Wall -fcheck=all -fbacktrace" )
else ()
  set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -march=native" )
  set ( CMAKE_Fortran_FLAGS_RELEASE "-O3" )
  set ( CMAKE_Fortran_FLAGS_DEBUG "-g -Og" )  
endif ()

# C compiler settings
if ( ${CMAKE_C_COMPILER_ID} STREQUAL "GNU" )
  set ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -march=native" )
  set ( CMAKE_C_FLAGS_RELEASE "-O3 -ffast-math -flto -fexcess-precision=fast -funroll-loops" )
  set ( CMAKE_C_FLAGS_DEBUG "-g -Og -Wall" )
else ()
  set ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -march=native" )
  set ( CMAKE_C_FLAGS_RELEASE "-O3" )
  set ( CMAKE_C_FLAGS_DEBUG "-g -Og" )
endif ()
