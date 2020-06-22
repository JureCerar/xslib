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

#ifndef _DEBUG
#define _DEBUG .false.
#endif

module xslib
  use xslib_cstring
  use xslib_vector
  use xslib_error
  use xslib_time
  use xslib_list
  use xslib_xmalloc
  use xslib_fileio
  use xslib_groio
  use xslib_pdbio
  use xslib_xyzio
  use xslib_xtcio
  use xslib_trrio
  use xslib_dcdio
  use xslib_cubio
  use xslib_ndxio
  use xslib_tplio
  use xslib_pdhio
  use xslib_csvio
  implicit none

  ! Version and compile date strings
  character(*), parameter :: xslib_version="v@PROJECT_VERSION@"
  character(*), parameter :: xslib_date=__DATE__//" "//__TIME__

contains

! Returns xslib version and compile date.
character(:) function xslibInfo()
  implicit none
  allocatable :: xslibinfo

  xslibinfo = trim(xslib_version)//" -- "//trim(xslib_date)

  return
end function xslibInfo

! More extensive xslib info and copyright.
subroutine xslibAbout()
  ! use, intrinsic :: iso_fortran_env
  implicit none
  character(:), allocatable :: FC_flags, FC_build_flags, FC_compiler_flags
  character(:), allocatable :: CC_flags, CC_build_flags, CC_compiler_flags

  ! Fortran compiler flags
  FC_flags = "@CMAKE_Fortran_FLAGS@"
  ! Fortran build flags (Release/Debug)
  FC_build_flags = smerge( &
    "@CMAKE_Fortran_FLAGS_DEBUG@", &
    "@CMAKE_Fortran_FLAGS_RELEASE@", &
    _DEBUG &
  )
  ! Fortran combined flags
  FC_compiler_flags = trim(adjustl(FC_build_flags)) // " " // trim(adjustl(FC_flags))


  ! C compiler flags
  CC_flags = "@CMAKE_C_FLAGS@"
  ! C build flags (Release/Debug)
  CC_build_flags = smerge( &
    "@CMAKE_C_FLAGS_DEBUG@", &
    "@CMAKE_C_FLAGS_RELEASE@", &
    _DEBUG &
  )
  ! C combined flags
  CC_compiler_flags = trim(adjustl(FC_build_flags)) // " " // trim(adjustl(FC_flags))

  ! About
  write (*,*) "Name:             ", "xslib - Extra-Small Library"
  write (*,*) "URL:              ", "@PROJECT_URL@"
  write (*,*) "Version:          ", "@PROJECT_VERSION@"
  write (*,*) "Build date:       ", __DATE__//" "//__TIME__
  write (*,*) "Fortran compiler: ", "@CMAKE_Fortran_COMPILER@ @CMAKE_Fortran_COMPILER_ID@ @CMAKE_Fortran_COMPILER_VERSION@"
  write (*,*) "Fortran flags:    ", trim(FC_compiler_flags)
  write (*,*) "C compiler:       ", "@CMAKE_C_COMPILER@ @CMAKE_C_COMPILER_ID@ @CMAKE_C_COMPILER_VERSION@"
  write (*,*) "C flags:          ", trim(CC_compiler_flags)
  write (*,*) ""
  write (*,*) "Copyright (C) 2019-2020 Jure Cerar"
  write (*,*) " This is free software; See the source for copying conditions. There is NO warranty; "
  write (*,*) " Not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."

  return
end subroutine xslibAbout

end module xslib
