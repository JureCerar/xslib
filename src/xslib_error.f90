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
  ! public :: error, error_, warning, warning_, xslibErrMsg, assert

  ! Import error definitions
  include "fileio.h"

  ! Default error/warning colors (<ecc>[xx;xx;xxxm)
  character(12), private :: errorColor = char(27)//"[1;91m" ! = bold, light_red
  character(12), private :: warningColor = char(27)//"[1;95m" ! = bold, light_magenta
  character(12), private :: noColor = char(27)//"[m" ! = color reset

  ! If any of the argument expression is false, a message is written to the STDERR
  ! * and abort is called, terminating the program execution.
  interface assert
    procedure :: assert, assert_1d, assert_2d
    procedure :: assert_ext, assert_1d_ext, assert_2d_ext ! extended for __FILE__ and __LINE__
    procedure :: assert_float, assert_float_1d, assert_float_2d ! Some basic support for floats
    procedure :: assert_double, assert_double_1d, assert_double_2d ! Some basic support for doubles
  end interface assert

contains

! Get error/warning color from environment variable "GCC_COLORS"
function get_env_colors( keyword )
  implicit none
  character(:), allocatable :: get_env_colors
  character(*), intent(in)  :: keyword
  character(512)            :: buffer
  integer                   :: stat, pos, end

  ! Example:
  ! * GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

  ! Get environment variable
  call get_environment_variable( "GCC_COLORS", buffer, STATUS=stat )
  if ( stat > 0 ) then
    ! No ENV variable set or cant get it. Return default
    select case( keyword )
    case( "error" )
      get_env_colors = errorColor
    case( "warning" )
      get_env_colors = warningColor
    case default
      get_env_colors = char(27)//"[1;91m" ! bold
    end select

  else
    ! Find keyword
    pos = index(buffer,keyword)
    if ( pos /= 0 ) then
      ! Modify position to account for '<keyword>='
      pos = pos+len_trim(keyword)+1
      ! Find end of string
      end = index( buffer(pos:), ":" )
      end = merge( end-1, len_trim(buffer), end /= 0 )
      ! Get string and add color escape sequance
      get_env_colors = char(27)//"["//trim(buffer(pos:pos+end-1))//"m"

    else
      ! Retrun default
      select case( keyword )
      case( "error" )
        get_env_colors = errorColor
      case( "warning" )
        get_env_colors = warningColor
      case default
        get_env_colors = char(27)//"[1;91m" ! bold
      end select

    end if
  end if

  return
end function get_env_colors

! Set error/warning colors from environment variable "GCC_COLORS"
! NOTE: experimental.
subroutine set_error_colors()
  implicit none
  errorColor = get_env_colors( "error" )
  warningColor = get_env_colors( "warning" )
  return
end subroutine set_error_colors

! Write error message to errout and terminate the program
! * ERROR: <message>
subroutine error( message )
  implicit none
  character(*), intent(in) :: message
  write (ERROR_UNIT,*) trim(errorColor)//"ERROR: "//trim(noColor)//trim(message)
  call exit( 1 )
end subroutine error

! Write extended error message to errout and terminate the program
! * <file>:<line>: ERROR: <message>
! #define error(x) error_( x, __FILE__, __LINE__ )
subroutine error_( message, file, line )
  implicit none
  character(*), intent(in)  :: message, file
  integer, intent(in)       :: line
  write (ERROR_UNIT,100) trim(file)//":", line, ":"//trim(errorColor)//" ERROR: "//trim(noColor)//trim(message)
  100 format( x,a,i0,a )
  call exit( 1 )
end subroutine error_

! Write warning message to errout
! * WARNING: <message>
subroutine warning( message )
  implicit none
  character(*), intent(in) :: message
  write (ERROR_UNIT,*) trim(warningColor)//"WARNING: "//trim(nocolor)//trim(message)
  return
end subroutine warning

! Write extended warning message to errout.
! * <file>:<line>: WARNING: <message>
! #define warning(x) warning_( x, __FILE__, __LINE__ )
subroutine warning_( message, file, line )
  implicit none
  character(*), intent(in) :: message, file
  integer, intent(in)      :: line
  write (ERROR_UNIT,100) trim(file)//":", line, ":"//trim(warningColor)//" WARNING: "//trim(noColor)//trim(message)
  100 format( x,a,i0,a )
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
    write (ERROR_UNIT,*) "Assertion failed!"
    call exit( 1 )
  end if

  return
end subroutine assert

! Comment
subroutine assert_1d( expression )
  implicit none
  logical, intent(in) :: expression(:)
  integer             :: i

  do i = 1, size(expression)
    if ( .not. expression(i) ) then
      write (ERROR_UNIT,100) "Assertion failed at [", i ,"]!"
      100 format( x, 2(a,i0) )
      call exit( 1 )
    end if
  end do

  return
end subroutine assert_1d

! Comment
subroutine assert_2d( expression )
  implicit none
  logical, intent(in) :: expression(:,:)
  integer             :: i, j

  do i = 1, size(expression,DIM=1)
    do j = 1, size(expression,DIM=2)
      if ( .not. expression(j,i) ) then
        write (ERROR_UNIT,100) "Assertion failed at [", j, ",", i ,"]!"
        100 format( x, 3(a,i0) )
        call exit( 1 )

      end if
    end do ! for j
  end do ! for i

  return
end subroutine assert_2d

! Extended abort message
subroutine assert_ext( expression, file, line )
  implicit none
  logical, intent(in)       :: expression
  character(*), intent(in)  :: file
  integer, intent(in)       :: line

  if ( .not. expression ) then
    write (ERROR_UNIT,100) trim(file)//":", line, ": Assertion failed!"
    100 format( x, 2(a,i0) )
    call exit( 1 )

  end if

  return
end subroutine assert_ext

! Comment
subroutine assert_1d_ext( expression, file, line )
  implicit none
  logical, intent(in)      :: expression(:)
  character(*), intent(in) :: file
  integer, intent(in)      :: line
  integer                  :: i

  do i = 1, size(expression)
    if ( .not. expression(i) ) then
      write (ERROR_UNIT,100) trim(file)//":", line, ": Assertion failed at [", i ,"]!"
      100 format( x, 3(a,i0) )
      call exit( 1 )

    end if
  end do

  return
end subroutine assert_1d_ext

! Comment
subroutine assert_2d_ext( expression, file, line )
  implicit none
  logical, intent(in)       :: expression(:,:)
  character(*), intent(in)  :: file
  integer, intent(in)       :: line
  integer                   :: i, j

  do i = 1, size(expression,DIM=1)
    do j = 1, size(expression,DIM=2)
      if ( .not. expression(j,i) ) then
        write (ERROR_UNIT,100) trim(file)//":", line,": Assertion failed at [", j, ",", i ,"]!"
        100 format( x, 4(a,i0) )
        call exit( 1 )

      end if
    end do ! for j
  end do ! for i

  return
end subroutine assert_2d_ext

! Basic support for floats and doubles
subroutine assert_float( actual, expected, tol, file, line )
  implicit none
  real, intent(in)                    :: actual, expected
  real, intent(in), optional          :: tol
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: line
  real                                :: delta

  delta = merge( tol, 1.0e-3, present(tol) )
  call assert( abs(actual-expected) < delta, file, line )

  return
end subroutine assert_float

! Comment
subroutine assert_float_1d( actual, expected, tol, file, line )
  implicit none
  real, intent(in)                    :: actual(:), expected(:)
  real, intent(in), optional          :: tol
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: line
  real                                :: delta

  ! if ( size(actual) /= size(expected) ) then
  !   write (ERROR_UNIT,*) "Assertion failed: different size!"
  !   call exit( 1 )
  ! end if

  delta = merge( tol, 1.0e-3, present(tol) )
  if ( size(actual) /= size(expected) ) call assert(.false.,file,line)
  call assert( abs(actual-expected) < delta, file, line )

  return
end subroutine assert_float_1d

! Comment
subroutine assert_float_2d( actual, expected, tol, file, line )
  implicit none
  real, intent(in)                    :: actual(:,:), expected(:,:)
  real, intent(in), optional          :: tol
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: line
  real                                :: delta

  delta = merge( tol, 1.0e-3, present(tol) )
  if ( size(actual) /= size(expected) ) call assert(.false.,file,line)
  call assert( abs(actual-expected) < delta, file, line )

  return
end subroutine assert_float_2d

! Comment
subroutine assert_double( actual, expected, tol, file, line )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(in)            :: actual, expected
  real(REAL64), intent(in), optional  :: tol
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: line
  real(REAL64)                        :: delta

  delta = merge( tol, 1.0d-3, present(tol) )
  call assert( abs(actual-expected) < delta, file, line )

  return
end subroutine assert_double

! Comment
subroutine assert_double_1d( actual, expected, tol, file, line )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(in)            :: actual(:), expected(:)
  real(REAL64), intent(in), optional  :: tol
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: line
  real(REAL64)                        :: delta

  delta = merge( tol, 1.0d-3, present(tol) )
  if ( size(actual) /= size(expected) ) call assert(.false.,file,line)
  call assert( abs(actual-expected) < delta, file, line )

  return
end subroutine assert_double_1d

! Comment
subroutine assert_double_2d( actual, expected, tol, file, line )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(in)            :: actual(:,:), expected(:,:)
  real(REAL64), intent(in), optional  :: tol
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: line
  real(REAL64)                        :: delta

  delta = merge( tol, 1.0d-3, present(tol) )
  if ( size(actual) /= size(expected) ) call assert(.false.,file,line)
  call assert( abs(actual-expected) < delta, file, line )

  return
end subroutine assert_double_2d

end module xslib_error
