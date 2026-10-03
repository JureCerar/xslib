! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2026 Jure Cerar
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

#ifndef PROJECT_VERSION
#define PROJECT_VERSION "0.0.0"
#endif

#ifndef VERSION_MAJOR
#define VERSION_MAJOR 0
#endif

#ifndef VERSION_MINOR
#define VERSION_MINOR 0
#endif

#ifndef VERSION_PATCH
#define VERSION_PATCH 0
#endif

module xslib
  use xslib_ansi
  use xslib_array
  use xslib_cstring
  use xslib_dict
  use xslib_errorh
  use xslib_fileio
  use xslib_fitting
  use xslib_geometry
  use xslib_linalg
  use xslib_list
  use xslib_logical
  use xslib_math
  use xslib_memory
  use xslib_pathlib
  use xslib_signal
  use xslib_sort
  use xslib_stats
  use xslib_time
  implicit none
  
  character(*), parameter :: xslib_version = PROJECT_VERSION
  !! String representation of the library version

  integer, parameter :: xslib_version_major = VERSION_MAJOR
  !! Major version number of the library
  
  integer, parameter :: xslib_version_minor = VERSION_MINOR
  !! Minor version number of the library

  integer, parameter :: xslib_version_patch = VERSION_PATCH
  !! Patch version number of the library

end module xslib