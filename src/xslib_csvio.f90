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
  contains
    procedure, nopass :: read => csv_read
    procedure, nopass :: write => csv_write
  end type csv_t

contains

! Reads DATA and (optional) HEADER from csv file.
integer function csv_read( file, data, header, delim )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  character(*), intent(in)          :: file
  real, allocatable, intent(inout)  :: data(:,:)
  character(:), optional, allocatable, intent(inout) :: header(:) ! got any more of them attributes
  character(*), optional            :: delim
  integer                           :: i, unit, stat
  integer                           :: cols, rows
  character(512)                    :: buffer
  logical                           :: hasHeader, exists

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

  ! Data
  if ( allocated(data) ) deallocate( data, STAT=stat )
  allocate( data(cols,rows), SOURCE=0., STAT=stat )
  if ( stat /= 0 ) then
    csv_read = xslibNOMEM
    return
  end if

  ! Header
  if (present(header)) then
    if (allocated(header)) deallocate(header, STAT=stat)
    allocate ( character(128) :: header(cols), STAT=stat )
    if ( stat /= 0 ) then
      csv_read = xslibNOMEM
      return
    end if
    header(:)=""
  end if

  ! -------------------
  ! Read header
  if ( hasHeader ) then
    ! Is header provided
    if ( present(header) ) then
      read (unit,*,IOSTAT=stat) header(:)
    else
      ! Skip header line
      read (unit,*,IOSTAT=stat)
    end if

    ! Process error
    if ( stat /= 0 ) then
      csv_read = merge(xslibENDOFFILE,xslibHEADER,stat==IOSTAT_END)
      return
    end if

  end if

  ! Read actual data
  do i = 1, rows
    read (unit,*,IOSTAT=stat) data(:,i)
    if ( stat /= 0 ) then
      csv_read = merge(xslibENDOFFILE,xslib3DX,stat==IOSTAT_END)
      return
    end if
  end do ! for i

  ! Return success
  csv_read = xslibOK

  return
end function csv_read

! Write DATA and (optional) HEADER in csv format to UNIT, FILE or STDOUT (if both absent).
! Default DELIMITER is comma - ","
integer function csv_write( data, header, file, unit, delim )
  use iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  real, intent(in)          :: data(:,:)
  character(*), optional    :: header(:), file, delim
  integer, optional         :: unit
  integer                   :: i, j, out, stat
  logical                   :: opened
  character(:), allocatable :: dl
  character(64)             :: action, fmtHeader, fmtData

  if ( present(file) ) then
    open( UNIT=out, FILE=trim(file), ACTION="write", STATUS="unknown", IOSTAT=stat )
    if ( stat /= 0 ) then
      csv_write = xslibFILENOTFOUND
      return
    end if

  else if (present(file)) then
    inquire( UNIT=unit, OPENED=opened, ACTION=action )
    if ( .not. opened .or. index(action,"WRITE") == 0 ) then
      csv_write = xslibOPEN
      return
    end if
    out = unit

  else
    out = OUTPUT_UNIT

  end if

  ! ================================
  ! delimiter
  if ( present(delim) ) then
    dl = delim
  else
    dl = ","
  end if

  ! ================================
  ! Write header
  if ( present(header) ) then
    ! define custom format
    write (fmtHeader,100) size(header), dl
    100 format( "(", i0 ,"(a:'", a ,"'))"  )

    ! Add "" if contains spaces
    ! do i = 1, cols
    !   if (scan(trim(header(i)), " ")/=0) header(i)="'"//trim(header(i))//"'"
    ! end do

    write (out,fmtHeader,IOSTAT=stat) ( trim(header(i)), i=1,size(header) )
    if ( stat /= 0 ) then
      csv_write = xslibSTRING
      return
    end if

  end if

  ! ================================
  ! Write header

  ! Define custom format
  write (fmtData,200) size(data,DIM=1), dl
  200 format( "(", i0 ,"(f8.4:'", a ,"'))"  )

  do i = 1, size(data,DIM=2)
      write (out,fmtData,IOSTAT=stat) data(:,i)
      if ( stat /= 0 ) then
        csv_write = xslib3DX
        return
      end if
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
