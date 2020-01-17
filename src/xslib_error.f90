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
  implicit none
  public

  ! Import error definitions
  include "fileio.h"

  ! Default error/warning colors (<ecc>[xx;xx;xxxm)
  character(12), private :: errorColor = char(27)//"[1;91m" ! = bold, light_red
  character(12), private :: warningColor = char(27)//"[1m" ! = bold
  character(12), private :: noColor = char(27)//"[m" ! = color reset

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
subroutine set_env_colors()
  implicit none
  errorColor = get_env_colors( "error" )
  warningColor = get_env_colors( "warning" )
  return
end subroutine set_env_colors

! Write error message to errout and terminate the program
! * ERROR: <message>
subroutine error( message )
  use, intrinsic :: iso_fortran_env
  implicit none
  character(*), intent(in) :: message
  write (error_unit,*) trim(errorColor)//"ERROR: "//trim(noColor)//trim(message)
  call exit( 1 )
end subroutine error

! Write extended error message to errout and terminate the program
! * <file>:<line>: ERROR: <message>
! #define error(x) error_( x, __FILE__, __LINE__ )
subroutine error_( message, file, line )
  use, intrinsic :: iso_fortran_env
  implicit none
  character(*), intent(in)  :: message, file
  integer, intent(in)       :: line
  write (error_unit,100) trim(file)//":", line, ":"//trim(errorColor)//" ERROR: "//trim(noColor)//trim(message)
  100 format( x,a,i0,a )
  call exit( 1 )
end subroutine error_

! Write warning message to errout
! * WARNING: <message>
subroutine warning( message )
  use, intrinsic :: iso_fortran_env
  implicit none
  character(*), intent(in) :: message
  write (error_unit,*) trim(warningColor)//"WARNING: "//trim(nocolor)//trim(message)
  return
end subroutine warning

! Write extended warning message to errout.
! * <file>:<line>: WARNING: <message>
! #define warning(x) warning_( x, __FILE__, __LINE__ )
subroutine warning_( message, file, line )
  use, intrinsic :: iso_fortran_env
  implicit none
  character(*), intent(in) :: message, file
  integer, intent(in)      :: line
  write (error_unit,100) trim(file)//":", line, ":"//trim(warningColor)//" WARNING: "//trim(noColor)//trim(message)
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

end module xslib_error
