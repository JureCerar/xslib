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

module xslib_pdhio
  implicit none
  private
  public :: pdh_t

  ! Import error definitions
  include "fileio.h"

  ! Global constants
  integer, parameter :: TEXT_LEN = 80
  integer, parameter :: KEY_LEN = 4

  ! .PDH file format
  type pdh_t
    character(TEXT_LEN) :: text = ""              ! Title text
    character(KEY_LEN)  :: key_words(16) = ""     ! Key words
    integer             :: int_const(8) = 0       ! Integer constants
    real                :: real_const(10) = 0.    ! Real constants
    integer             :: num_points = 0         ! Num. of points 
    real, allocatable   :: x(:), y(:), y_error(:) ! x, y and error
  contains
    procedure :: allocate => pdh_allocate
    procedure :: assign => pdh_assign
    generic   :: assignment(=) => assign
    procedure :: read => pdh_read
    procedure :: write => pdh_write
  end type pdh_t

contains

! Deallocate and allocate .PDH file.
integer function pdh_allocate( this, np )
  implicit none
  class(pdh_t)        :: this
  integer, intent(in) :: np
  integer             :: stat

  ! Set number of points
  this%num_points = np

  ! Allocate data
  if ( allocated(this%x) ) deallocate( this%x, this%y, this%y_error, STAT=stat )
  allocate( this%x(this%num_points), this%y(this%num_points), this%y_error(this%num_points), STAT=stat )
  if ( stat /= 0 ) then
    pdh_allocate = xslibNOMEM
    return
  end if

  ! Return success
  pdh_allocate = xslibOK

  return
end function pdh_allocate

! Copy .PDH file
subroutine pdh_assign( this, other )
  implicit none
  class(pdh_t), intent(inout) :: this
  type(pdh_t), intent(in)     :: other
  integer                     :: stat

  ! Copy header
  this%text           = other%text
  this%key_words(:)   = other%key_words
  this%int_const(:)   = other%int_const
  this%real_const(:)  = other%real_const

  ! Copy data
  this%num_points = other%num_points
  if ( allocated(other%x) .and. other%num_points > 0 ) then
    stat = this%allocate( other%num_points )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%x(:)       = other%x
    this%y(:)       = other%y
    this%y_error(:) = other%y_error

  end if

  return
end subroutine pdh_assign

! Read .PDH file
integer function pdh_read( this, file )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  class(pdh_t)              :: this
  character(*), intent(in)  :: file
  integer                   :: i, unit, stat

  open( NEWUNIT=unit, FILE=trim(file), STATUS="old", ACTION="read", IOSTAT=stat )
  if ( stat /= 0 )  then
    pdh_read = xslibFILENOTFOUND
    return
  end if

  ! Text
  read (unit,"(a80)",IOSTAT=stat) this%text
  if ( stat /= 0 )  then
    pdh_read = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  ! Keywords
  read (unit,"(16(a4,1x))",IOSTAT=stat) this%key_words(:)
  if ( stat /= 0 )  then
    pdh_read = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  ! Integer parameters
  ! NOTE: Number of points is first int_constant
  read (unit,"(8(i9,1x))",IOSTAT=stat) this%num_points, this%int_const(2:8)
  if ( stat /= 0 )  then
    pdh_read = merge( xslibENDOFFILE, xslibINT, stat == IOSTAT_END )
    return
  end if

  ! 1st set of real parameters
  read (unit,"(5(e14.6,1x))",IOSTAT=stat) this%real_const(1:5)
  if ( stat /= 0 )  then
    pdh_read = merge( xslibENDOFFILE, xslibFLOAT, stat == IOSTAT_END )
    return
  end if

  ! 2nd set of real parameters
  read (unit,"(5(e14.6,1x))",IOSTAT=stat) this%real_const(6:10)
  if ( stat /= 0 )  then
    pdh_read = merge( xslibENDOFFILE, xslibFLOAT, stat == IOSTAT_END )
    return
  end if

  ! Allocate memory
  pdh_read = this%allocate( this%num_points )
  if ( pdh_read /= xslibOK ) return

  ! Read all data points
  do i = 1, this%num_points
    read (unit,"(3(1pe14.6,1x))",IOSTAT=stat) this%x(i), this%y(i), this%y_error(i)
    if ( stat /= 0 )  then
      pdh_read = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
      return
    end if

  end do ! for i

  ! Close file
  close( unit, IOSTAT=stat )
  if ( stat /= 0 )  then
    pdh_read = xslibCLOSE
    return
  end if

  ! Return success
  pdh_read = xslibOK

  return
end function pdh_read

! write .PDH to file or unit
integer function pdh_write( this, file, unit )
  use iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(pdh_t)                        :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: i, out, stat
  logical                             :: opened
  character(9)                        :: action

  ! Select output method. default is stdout
  if ( present(file) ) then
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      pdh_write = xslibFILENOTFOUND
      return
    end if
  else if ( present(unit) ) then
    inquire( UNIT=unit, OPENED=opened, ACTION=action )
    if ( .not. opened .or. index(action,"WRITE") == 0 ) then
      pdh_write = xslibOPEN
      return
    end if
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Check if data is allocated
  if ( .not. allocated(this%x) ) then
    pdh_write = xslibNOMEM
    return
  end if

  ! Title
  write (out,"(a80)") this%text

  ! Keywords
  write (out,"(16(a4,1x))") this%key_words(:)

  ! Integer parameters
  write (out,"(8(i9,1x))") this%num_points, this%int_const(2:8)

  ! Real constants
  write (out,"(5(e14.6,1x))") this%real_const(1:5)
  write (out,"(5(e14.6,1x))") this%real_const(6:10)

  ! Data points
  do i = 1, this%num_points
    write (out,"(3(1pe14.6,1x))") this%x(i), this%y(i), this%y_error(i)
  end do

  ! Close file
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      pdh_write = xslibCLOSE
      return
    end if
  end if

  ! Return success
  pdh_write = xslibOK

  return
end function pdh_write

end module xslib_pdhio
