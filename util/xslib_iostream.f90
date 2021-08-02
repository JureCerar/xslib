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

module xslib_iostream
  implicit none
  integer, parameter :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2

  interface sfseek
    procedure :: sfseek_int, sfseek_long
  end interface

contains

! Open file as formatted stream
subroutine sfopen( unit, file, status, action, iostat )
  implicit none
  integer, intent(out)                :: unit
  character(*), intent(in)            :: file
  character(*), intent(in), optional  :: status, action
  integer, intent(out), optional      :: iostat
  integer                             :: iostat_
  character(32)                       :: status_, action_

  ! Defaults
  action_ = "readwrite"
  status_ = "unknown"

  ! Load optional arguments
  if (present(action)) action_ = action
  if (present(status)) status_ = status

  ! Open file as formatted stream
  open( NEWUNIT=unit, FILE=file, ACCESS="stream", FORM="formatted", STATUS=status_, ACTION=action_, IOSTAT=iostat_ )
  if (present(iostat)) iostat=iostat_

  return
end subroutine sfopen

! Close file
subroutine sfclose( unit, status, iostat )
  implicit none
  integer, intent(in)                 :: unit
  character(*), intent(in), optional  :: status
  integer, intent(out), optional      :: iostat
  integer                             :: iostat_
  character(32)                       :: status_

  ! Defaults
  status_ = "keep"
  if ( present(status) ) status_ = status

  close( UNIT=unit, STATUS=status_, IOSTAT=iostat_ )
  if ( present(iostat) ) iostat = iostat_

  return
end subroutine sfclose

! Returns the current value of the position indicator of the stream
integer(INT64) function sftell( unit )
  use iso_fortran_env, only: INT64
  implicit none
  integer, intent(in) :: unit
  integer             :: stat
  logical             :: opened

  inquire(UNIT=unit,OPENED=opened,POS=sftell,IOSTAT=stat)
  if ( stat /= 0 .or. .not.opened ) sftell = -1

  return
end function sftell

! Sets the position indicator associated with the stream to a new position.
! * SEEK_SET - Beginning of file
! * SEEK_CUR - Current position of the file pointer
! * SEEK_END - End of file
subroutine sfseek_int( unit, offset, whence, status)
  use iso_c_binding, only: c_sizeof
  implicit none
  integer, intent(in)             :: unit
  integer, intent(in)             :: offset
  integer, intent(in)             :: whence
  integer, intent(out), optional  :: status
  integer                         :: pos, size, stat
  logical                         :: opened
  character                       :: dummy

  ! Inquire file information
  inquire(UNIT=unit,OPENED=opened,POS=pos,SIZE=size,IOSTAT=stat)
  if ( stat /= 0 .or. .not.opened ) then
    if (present(status)) status = stat
    return
  end if

  ! Calculate new position based on "whence"
  select case ( whence )
  case ( SEEK_SET )
    pos = offset
  case ( SEEK_CUR )
    pos = pos+offset
  case ( SEEK_END )
    pos = size+offset
  case default
    if (present(status)) status = -1
    return
  end select

  ! Move to new position
  if ( pos < c_sizeof(dummy) ) then
    rewind (UNIT=unit,IOSTAT=stat)
  else
    read (unit,"(a)",POS=pos-c_sizeof(dummy),IOSTAT=stat) dummy
  end if
  if (present(status)) status = stat

  return
end subroutine sfseek_int

subroutine sfseek_long( unit, offset, whence, status)
  use iso_fortran_env, only: INT64
  use iso_c_binding, only: c_sizeof
  implicit none
  integer, intent(in)             :: unit
  integer(INT64), intent(in)      :: offset
  integer, intent(in)             :: whence
  integer, intent(out), optional  :: status
  integer(INT64)                  :: size, pos
  integer                         :: stat
  logical                         :: opened
  character                       :: dummy

  ! Inquire file information
  inquire(UNIT=unit,OPENED=opened,POS=pos,SIZE=size,IOSTAT=stat)
  if ( stat /= 0 .or. .not.opened ) then
    if (present(status)) status = stat
    return
  end if

  ! Calculate new position based on "whence"
  select case ( whence )
  case ( SEEK_SET )
    pos = offset
  case ( SEEK_CUR )
    pos = pos+offset
  case ( SEEK_END )
    pos = size+offset
  case default
    if (present(status)) status = -1
    return
  end select

  ! Move to new position
  if ( pos < c_sizeof(dummy) ) then
    rewind (UNIT=unit,IOSTAT=stat)
  else
    read (unit,"(a)",POS=pos-c_sizeof(dummy),IOSTAT=stat) dummy
  end if
  if (present(status)) status = stat

  return
end subroutine sfseek_long

end module xslib_iostream
