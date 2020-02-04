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

module xslib_csvio
  implicit none
  private
  public :: csv_t

  ! Import error definitions
  include "fileio.h"

  type csv_t
    real, allocatable           :: data(:,:)
    character(128), allocatable :: header(:)
  contains
    procedure :: allocate => csv_allocate
    procedure :: read => csv_read
    procedure :: write => csv_write
  end type csv_t

contains

! Allocate and initialize data
integer function csv_allocate( this, cols, rows )
  implicit none
  class(csv_t)        :: this
  integer, intent(in) :: cols, rows

  if ( allocated(this%data) ) deallocate( this%data, STAT=csv_allocate )
  allocate( this%data(cols,rows), SOURCE=0.000, STAT=csv_allocate )
  if ( csv_allocate /= 0 ) then
    csv_allocate = xslibNOMEM
    return
  end if

  ! TODO: implement various character sizes
  if ( allocated(this%header) ) deallocate( this%header, STAT=csv_allocate )
  allocate( this%header(cols), STAT=csv_allocate )
  if ( csv_allocate /= 0 ) then
    csv_allocate = xslibNOMEM
    return
  end if
  this%header(:) = ""

  return
end function csv_allocate

! Reads csv file.
integer function csv_read( this, file, delim )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  class(csv_t)                        :: this
  character(*), intent(in)            :: file
  character(*), optional, intent(in)  :: delim
  integer                             :: i, unit, stat
  integer                             :: cols, rows
  character(512)                      :: buffer
  logical                             :: exists, hasHeader

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exists )
  if ( .not. exists ) then
    csv_read = xslibFILENOTFOUND
    return
  end if

  ! Open file
  open( NEWUNIT=unit, FILE=trim(file), ACTION="read", STATUS="old", IOSTAT=stat )
  if ( stat /= 0 ) then
    csv_read = xslibOPEN
    return
  end if

  ! -------------------
  ! Read first line and check if file has header (contains ASCII string)
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    csv_read = merge(xslibENDOFFILE,xslibHEADER,stat==IOSTAT_END)
    return
  end if
  hasHeader = isWord( buffer )

  ! if it has header read next line (with data) to buffer
  if ( hasHeader ) then
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat /= 0 ) then
      csv_read = merge(xslibENDOFFILE,xslibHEADER,stat==IOSTAT_END)
      return
    end if
  end if

  ! -------------------
  ! Count number of columns
  if ( present(delim) ) then
    cols = cnttok(buffer,delim)
  else
    cols = cnttok(buffer,",")
  end if

  ! Count number of rows
  rows = 1 ! One was already read
  do
    read (unit,*,IOSTAT=stat)
    if ( stat == IOSTAT_END ) exit
    rows = rows+1
  end do

  ! Rewind file
  rewind( unit, IOSTAT=stat )

  ! -------------------
  ! Allocate memory
  csv_read = this%allocate(cols,rows)
  if ( csv_read /= 0 ) return

  ! Read header
  if ( hasHeader ) then
    read (unit,*,IOSTAT=stat) this%header(1:cols)
    if ( stat /= 0 ) then
      csv_read = merge(xslibENDOFFILE,xslibHEADER,stat==IOSTAT_END)
      return

    end if
  end if

  ! Read actual data
  do i = 1, rows
    read (unit,*,IOSTAT=stat) this%data(:,i)
    if ( stat /= 0 ) then
      csv_read = merge(xslibENDOFFILE,xslib3DX,stat==IOSTAT_END)
      return

    end if
  end do ! for i

  ! Return success
  csv_read = xslibOK

  return
end function csv_read

! Write csv file to UNIT, FILE or STDOUT (if both absent).
! * Default DELIMITER is comma - ","
integer function csv_write( this, file, unit, delim )
  use iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(csv_t)              :: this
  character(*), optional    :: file, delim
  integer, optional         :: unit
  integer                   :: i, j, out, stat
  logical                   :: opened
  character(:), allocatable :: dl
  character(10)             :: action
  character(128)            :: buffer

  ! Select output method
  if ( present(file) ) then
    open( NEWUNIT=out, FILE=trim(file), ACTION="write", STATUS="unknown", IOSTAT=stat )
    if ( stat /= 0 ) then
      csv_write = xslibFILENOTFOUND
      return
    end if

  else if ( present(unit) ) then
    inquire( UNIT=unit, OPENED=opened, ACTION=action )
    if ( .not. opened .or. index(action,"WRITE") == 0 ) then
      csv_write = xslibOPEN
      return
    end if
    out = unit

  else
    out = OUTPUT_UNIT

  end if
  ! --------------------------------

  ! Set delimiter
  if ( present(delim) ) then
    dl = delim
  else
    dl = ","
  end if

  ! Write header if it contains data
  if ( any(this%header(:) /= "") ) then
    ! TODO: Add "" if header contains spaces and does not already have them

    ! Try writing one line
    write (out,"(a,$)",IOSTAT=stat) trim(this%header(1))
    if ( stat /= 0 ) then
      csv_write = xslibSTRING
      return
    end if

    ! Write remaining headers one-by-one
    do i = 2, size(this%header)
      write (out,"(2a,$)") dl, trim(this%header(i))
    end do

    ! New line
    write (out,*) ""

  end if

  ! Write data (use buffer)
  do i = 1, size(this%data,DIM=2)
    write (buffer,*,IOSTAT=stat) this%data(1,i)
    write (out,"(a,$)") trim(adjustl(buffer))

    ! Write remaining data one-by-one
    do j = 2, size(this%data,DIM=1)
      write (buffer,*,IOSTAT=stat) this%data(1,i)
      write (out,"(2a,$)") dl, trim(adjustl(buffer))

    end do

    ! New line
    write (out,*) ""

  end do

  ! Close file
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      csv_write = xslibCLOSE
      return
    end if
  end if

  ! Return success
  csv_write = xslibOK

  return
end function csv_write

! -------------------------------------------------
! Utilities

! Check if string contains any ASCII letters.
logical function isWord( string )
  implicit none
  character(*), intent(in)  :: string
  integer                   :: i

  isWord = .false.
  do i = 1, len_trim(string)
    isWord = ichar(string(i:i)) > 64 .and. ichar(string(i:i)) < 123
    if ( isWord ) return
  end do

  return
end function isWord

! Breaks string str into a series of tokens using the delimiter delim.
! * Works in same way as strtok in C, except instead of NULL feed char(0)
character(:) function strtok( string, delim )
  implicit none
  allocatable                     :: strtok
  character(*),intent(in)         :: string
  character(*),intent(in)         :: delim
  character(:), allocatable, save :: saved_string
  integer, save                   :: saved_start
  integer                         :: start, finish
  !$OMP THREADPRIVATE( saved_string, saved_start )

  ! SOURCE: http://fortranwiki.org/fortran/show/strtok

  ! initialize stored copy of input string and pointer into input string on first call
  if ( string(1:1) /= char(0) ) then
      saved_start = 1                 ! beginning of unprocessed data
      saved_string = trim(string)     ! save input string from first call in series
  endif

  ! Start from where we left
  start = saved_start

  ! Skip until next non-delimiter
  do while ( start <= len(saved_string) )
    if ( index(delim,saved_string(start:start)) /= 0 ) then
      start = start+1
    else
      exit
    end if
  end do

  ! If we reach end of string
  if ( start > len(saved_string) ) then
    strtok = char(0)
    return
  end if

  ! Find next delimiter
  finish = start
  do while ( finish <= len(saved_string) )
    if ( (index(delim,saved_string(finish:finish)) == 0) ) then
      finish = finish+1
    else
      exit
   end if
  end do

  ! Set result and update where we left
  strtok = saved_string(start:finish-1)
  saved_start = finish

  return
end function strtok

! Count number of tokens in string.
integer function cnttok( string, delim )
  implicit none
  character(*),intent(in)   :: string
  character(*),intent(in)   :: delim
  character(:), allocatable :: token
  ! Initialize
  cnttok = 0

  ! Get first token
  token = strtok( string, delim )

  ! Walk through other tokens
  do while ( token /= char(0) )
    cnttok = cnttok+1
    token = strtok( char(0), delim )
  end do

  return
end function cnttok

end module xslib_csvio
