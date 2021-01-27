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

! --------------------------------

! Warning and error macro override.

#define __THISFILE__ "error.F90"
#define error(x) error_( x, __THISFILE__, __LINE__ )
#define warning(x) warning_( x, __THISFILE__, __LINE__ )
#define assert(x) assert_( x, __THISFILE__, __LINE__ )

program main
  use xslib_error
  implicit none
  integer, parameter :: DIM = 3
  integer            :: i
  integer            :: value = 1, array(DIM) = 1, matrix(DIM,DIM) = 1

  ! Write version
  ! write (*,*) "xslib "//xslibInfo()
  ! write (*,*) "" ! Empty line

  ! Test all xslib error messages
  do i = 1, xslibNR
    write (*,100) i, xslibErrMsg( i )
    100 format( x, "ERROR ", i0, ":", x, a )
  end do

  ! Test correct assertion
  call assert( value == 1 )
  call assert( array(:) == [(1,i=1,DIM)] )
  call assert( matrix(:,:) == reshape( [(1,i=1,DIM*DIM)], SHAPE=shape(matrix) ) )

  ! Write warning and error to stderr.
  call warning( "This is warning message." )
  call error( "This is error message." )

  return
end program main
