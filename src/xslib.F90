! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2020  Jure Cerar
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

module xslib
  use xslib_cstring
  use xslib_vector
  use xslib_error
  use xslib_time
  use xslib_fileio
  use xslib_groio
  use xslib_pdbio
  use xslib_xyzio
  use xslib_xtcio
  use xslib_dcdio
  use xslib_cubio
  use xslib_ndxio
  use xslib_tplio
  use xslib_pdhio
  use xslib_csvio
  use xslib_list
  implicit none

  character(32), parameter :: xslib_version="v@PROJECT_VERSION@"
  character(32), parameter :: xslib_date=__DATE__//" "//__TIME__

contains

! Returns Xslib version and compile date.
character(:) function xslibInfo()
  implicit none
  allocatable :: xslibinfo
  xslibinfo = trim(xslib_version)//" -- "//trim(xslib_date)
  return
end function xslibInfo


! Custom fortran flags option
#ifdef NDEBUG
#define _Fortran_FLAGS "@CMAKE_Fortran_FLAGS_RELEASE@"
#else
#define _Fortran_FLAGS "@CMAKE_Fortran_FLAGS_DEBUG@"
#endif

! More extensive xslib infor and copyright.
subroutine xslibAbout()
  use, intrinsic :: iso_fortran_env
  implicit none
  write (*,*) "Name:             ", "xslib - Extra small library"
  write (*,*) "URL:              ", "@PROJECT_URL@"
  write (*,*) "Version:          ", "@PROJECT_VERSION@"
  write (*,*) "Build date:       ", __DATE__//" "//__TIME__
  write (*,*) "Compiler:         ", compiler_version()
  write (*,*) "Compiler flags:   ", _Fortran_FLAGS, &
  & "@CMAKE_Fortran_FLAGS@"
  ! write (*,*) "Compiler flags:   ", compiler_options() ! Retruns clusterfuck of flags
  write (*,*) ""
  write (*,*) "Copyright (C) 2019-2020 Jure Cerar"
  write (*,*) " This is free software; See the source for copying conditions. There is NO warranty; "
  write (*,*) " Not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
  return
end subroutine xslibAbout

end module xslib
