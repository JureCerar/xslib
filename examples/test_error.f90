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
! #define error(x) error_(x,__FILE__,__LINE__)
! #define warning(x) warning_(x,__FILE__,__LINE__)

program main
  use xslib_error
  implicit none
  integer :: i

  ! Test all xslib error messages
  do i = 1, xslibNR
    write (*,100) i, xslibErrMsg( i )
    100 format( x, "ERROR ", i0, ":", x, a )
  end do

  ! Test correct assertion
  call assert( 1 == 1 )
  call assert( 1.0, 1.0, TOL=1.0e-3 )

  ! (Experimental) Try to get ENV colors.
  ! call set_error_colors()

  ! Write warning and error to stderr.
  call warning( "This is warning message." )
  call error( "This is error message." )

end program main
