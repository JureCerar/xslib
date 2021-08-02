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

module xslib_error
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  implicit none
  ! private
  ! public :: error, error_, warning, warning_, assert, assert_, xslibErrMsg

  ! Import error definitions
  include "fileio.h"

  ! Default error/warning colors (<ecc>[xx;xx;xxxm)
  character(*), parameter, private :: C1 = char(27)//"[1;91m" ! = bold, light_red
  character(*), parameter, private :: C2 = char(27)//"[1;95m" ! = bold, light_magenta
  character(*), parameter, private :: C0 = char(27)//"[m"     ! = color reset

  ! Keywords for error and warning
  character(*), parameter, private :: errorKeyword   = C1//"[Error]:"//C0
  character(*), parameter, private :: warningKeyword = C2//"[Warning]:"//C0

  ! If any of the argument expression is false, a message is written to the STDERR
  ! * and abort is called, terminating the program execution.
  interface assert
    procedure :: assert, assert_1d, assert_2d
  end interface assert

  ! Extended assertion for __FILE__ and __LINE__
  interface assert_
    procedure :: assert_, assert_1d_, assert_2d_
  end interface assert_

contains

! Write error message to errout and terminate the program
! * ERROR: <message>
subroutine error( message )
  implicit none
  character(*), intent(in) :: message
  write (ERROR_UNIT,100) errorKeyword, trim(message)
  100 format( 2(x,a) )
  call exit( 1 )
end subroutine error

! Write extended error message to errout and terminate the program
! * <file>:<line>: ERROR: <message>
! #define error(x) error_( x, __FILE__, __LINE__ )
subroutine error_( message, file, line )
  implicit none
  character(*), intent(in)  :: message, file
  integer, intent(in)       :: line
  write (ERROR_UNIT,100) trim(file), ":", line, ":", errorKeyword, trim(message)
  100 format( x,2a,i0,2a,x,a )
  call exit( 1 )
end subroutine error_

! Write warning message to errout
! * WARNING: <message>
subroutine warning( message )
  implicit none
  character(*), intent(in) :: message
  write (ERROR_UNIT,100) warningKeyword, trim(message)
  100 format( 2(x,a) )
  return
end subroutine warning

! Write extended warning message to errout.
! * <file>:<line>: WARNING: <message>
! #define warning(x) warning_( x, __FILE__, __LINE__ )
subroutine warning_( message, file, line )
  implicit none
  character(*), intent(in) :: message, file
  integer, intent(in)      :: line
  write (ERROR_UNIT,100) trim(file), ":", line, ":", warningKeyword, trim(message)
  100 format( x,2a,i0,2a,x,a )
  return
end subroutine warning_

! Function returns error message for xslib error code.
character(:) function xslibErrMsg( errnum )
  implicit none
  allocatable         :: xslibErrMsg
  integer, intent(in) :: errnum

  select case( errnum )
  case( xslibOK )
    xslibErrMsg = ""
  case( xslibHEADER )
    xslibErrMsg = "Cannot read/write file header."
  case( xslibSTRING )
    xslibErrMsg = "Cannot read/write variable of kind: character."
  case( xslibDOUBLE )
    xslibErrMsg = "Cannot read/write variable of kind: double."
  case( xslibINT )
    xslibErrMsg = "Cannot read/write variable of kind: integer."
  case( xslibFLOAT )
    xslibErrMsg = "Cannot read/write variable of kind: float."
  case( xslibUINT )
    xslibErrMsg = "Cannot read/write variable of kind: unsigned integer."
  case( xslib3DX )
    xslibErrMsg = "Cannot read/write coordinate data."
  case( xslibCLOSE )
    xslibErrMsg = "Cannot close file."
  case( xslibMAGIC )
    xslibErrMsg = "Cannot read/write magic number."
  case( xslibNOMEM )
    xslibErrMsg = "Memory allocation failure."
  case( xslibENDOFFILE )
    xslibErrMsg = "End of file."
  case( xslibFILENOTFOUND )
    xslibErrMsg = "File not found."
  case( xslibOPEN )
    xslibErrMsg = "File unit/pointer not assigned."
  case( xslibNATOMS )
    xslibErrMsg = "Wrong number of atoms."
  case default
    xslibErrMsg = "Unknown error."
  end select

  return
end function xslibErrMsg

! Error check for xslib I/O procedures
subroutine xslibCheck( stat )
  implicit none
  integer, intent(in) :: stat
  if ( stat /= xslibOK ) call error( xslibErrMsg(stat) )
  return
end subroutine xslibCheck

! Error check for xslib I/O procedures with extended error info
subroutine xslibCheck_( stat, file, line )
  implicit none
  integer, intent(in)      :: stat, line
  character(*), intent(in) :: file
  if ( stat /= xslibOK ) call error_( xslibErrMsg(stat), file, line )
  return
end subroutine xslibCheck_

! -------------------------------------------
! Assert

! TODO: Use assumed-rank arrays :: array(..)
! * select rank ( array )
! * ...
! * Should be supported in Fortran-2018 ( > gfotran-10 )

! Comment
subroutine assert( expression )
  implicit none
  logical, intent(in) :: expression

  if ( .not. expression ) then
    call error( "Assertion failed." )

  end if

  return
end subroutine assert

! Comment
subroutine assert_1d( expression )
  implicit none
  logical, intent(in) :: expression(:)
  integer             :: i
  character(128)      :: msg

  do i = 1, size(expression)
    if ( .not. expression(i) ) then
      write (msg,"(a,i0,a)") "Assertion failed at: [", i ,"]"
      call error( msg )

    end if
  end do

  return
end subroutine assert_1d

! Comment
subroutine assert_2d( expression )
  implicit none
  logical, intent(in) :: expression(:,:)
  integer             :: i, j
  character(128)      :: msg

  do i = 1, size(expression,DIM=1)
    do j = 1, size(expression,DIM=2)
      if ( .not. expression(j,i) ) then
        write (msg,"(2(a,i0),a)") "Assertion failed at: [", j, ",", i ,"]"
        call error( msg )

      end if
    end do ! for j
  end do ! for i

  return
end subroutine assert_2d

! Extended abort message
subroutine assert_( expression, file, line )
  implicit none
  logical, intent(in)       :: expression
  character(*), intent(in)  :: file
  integer, intent(in)       :: line

  if ( .not. expression ) then
    call error_( "Assertion failed.", file, line )

  end if

  return
end subroutine assert_

! Comment
subroutine assert_1d_( expression, file, line )
  implicit none
  logical, intent(in)       :: expression(:)
  character(*), intent(in)  :: file
  integer, intent(in)       :: line
  integer                   :: i
  character(128)            :: msg

  do i = 1, size(expression)
    if ( .not. expression(i) ) then
      write (msg,"(a,i0,a)") "Assertion failed at: [", i ,"]"
      call error_( msg, file, line )

    end if
  end do

  return
end subroutine assert_1d_

! Comment
subroutine assert_2d_( expression, file, line )
  implicit none
  logical, intent(in)       :: expression(:,:)
  character(*), intent(in)  :: file
  integer, intent(in)       :: line
  integer                   :: i, j
  character(128)            :: msg

  do i = 1, size(expression,DIM=1)
    do j = 1, size(expression,DIM=2)
      if ( .not. expression(j,i) ) then
        write (msg,"(2(a,i0),a)") "Assertion failed at: [", j, ",", i ,"]"
        call error_( msg, file, line )

      end if
    end do ! for j
  end do ! for i

  return
end subroutine assert_2d_

end module xslib_error
