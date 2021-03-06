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

module xslib_ndxio
  implicit none
  private
  public :: ndx_t, ndx_group

  ! Import error definitions
  include "fileio.h"

  type ndx_group
    character(128)        :: title = "" ! Title of the group
    real                  :: val = 0.00 ! Reserved value (not part of ndx)
    integer               :: natoms = 0 ! Size of index group
    integer, allocatable  :: loc(:)     ! Index location
  contains
    procedure :: allocate => ndx_group_allocate
    procedure :: assign => ndx_group_assign
    generic   :: assignment(=) => assign
    procedure :: read => ndx_group_read
    procedure :: write => ndx_group_write
  end type ndx_group

  type ndx_t
    integer                       :: ngroups = 0
    type(ndx_group), allocatable  :: group(:)
  contains
    procedure :: ndx_allocate, ndx_allocate_all
    generic   :: allocate => ndx_allocate, ndx_allocate_all
    procedure :: assign => ndx_assign
    generic   :: assignment(=) => assign
    procedure :: read => ndx_read
    procedure :: write => ndx_write
    procedure :: display => ndx_display
  end type ndx_t

contains

! -------------------------------------------------

! Allocate ndx group
integer function ndx_group_allocate( this, natoms )
  implicit none
  class(ndx_group)    :: this
  integer, intent(in) :: natoms

  ! Set number of atoms
  this%natoms = natoms

  ! Allocate data
  if ( allocated(this%loc) ) deallocate( this%loc, STAT=ndx_group_allocate )
  allocate( this%loc(this%natoms), STAT=ndx_group_allocate )
  if ( ndx_group_allocate /= 0 ) then
    ndx_group_allocate = xslibNOMEM
    return
  end if

  return
end function ndx_group_allocate

! Copy ndx group.
subroutine ndx_group_assign( this, other )
  implicit none
  class(ndx_group), intent(inout) :: this
  type(ndx_group), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%title = other%title
  this%val = other%val

  ! Copy data
  this%natoms = other%natoms
  if ( allocated(other%loc) ) then
    stat = this%allocate( other%natoms )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%loc(:) = other%loc

  end if

  return
end subroutine ndx_group_assign

! Read index group.
integer function ndx_group_read( this, unit )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  class(ndx_group)    :: this
  integer, intent(in) :: unit
  integer             :: ni, nj, stat
  character(128)      :: buffer
  character(9)        :: action
  logical             :: opened

  ! Check unit
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    ndx_group_read = xslibOPEN
    return
  end if

  ! Read buffer
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    ndx_group_read = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  ! Parse title from buffer
  ni = index(buffer,"[")
  nj = index(buffer,"]")
  if ( ni == 0 .or. nj == 0 ) then
    ndx_group_read = xslibSTRING
  end if
  this%title = buffer(ni+1:nj-1)
  this%title = trim(adjustl(this%title))

  ! Count number of indeces.
  this%natoms = indexCount( unit )

  ! If empty group(?) just finish
  if ( this%natoms == 0 ) then
    ndx_group_read = xslibOK
    return
  end if

  ! Allocate data
  ndx_group_read = this%allocate( this%natoms )
  if ( ndx_group_read /= xslibOK ) return

  ! Read data
  read (unit,*,IOSTAT=stat) this%loc(:)
  if ( stat /= 0 ) then
    ndx_group_read = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
    return
  end if

  ! Return success
  ndx_group_read = xslibOK

  return
end function ndx_group_read

! Write index group.
integer function ndx_group_write( this, unit )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  class(ndx_group)    :: this
  integer, intent(in) :: unit
  integer             :: stat
  character(9)        :: action
  logical             :: opened

  ! Check unit
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    ndx_group_write = xslibOPEN
    return
  end if

  ! Check if memory is allocated.
  if ( .not. allocated(this%loc) ) then
    ndx_group_write = xslibNOMEM
    return
  end if

  ! Title
  write (unit,100,IOSTAT=stat) trim(this%title)
  100 format( "[", x, a, x, "]" )
  if ( stat /= 0 ) then
    ndx_group_write = xslibSTRING
    return
  end if

  ! Write data
  ! * Default num. of cols is 15
  write (unit,200,IOSTAT=stat) this%loc(:)
  200 format( 15( x, i0 ) )
  if ( stat /= 0 ) then
    ndx_group_write = xslib3DX
    return
  end if

  ! Return success
  ndx_group_write = xslibOK

  return
end function ndx_group_write


! -------------------------------------------------

! Allocate ndx group
integer function ndx_allocate( this, ngroups )
  implicit none
  class(ndx_t)        :: this
  integer, intent(in) :: ngroups

  ! Set number of atoms
  this%ngroups = ngroups

  ! Allocate data
  if ( allocated(this%group) ) deallocate( this%group, STAT=ndx_allocate )
  allocate( this%group(this%ngroups), STAT=ndx_allocate )
  if ( ndx_allocate /= 0 ) then
    ndx_allocate = xslibNOMEM
    return
  end if

  ! Return success
  ndx_allocate = xslibOK

  return
end function ndx_allocate

! Allocate ndx group
integer function ndx_allocate_all( this, natoms )
  implicit none
  class(ndx_t)        :: this
  integer, intent(in) :: natoms(:)
  integer             :: i

  ! Set number of atoms
  this%ngroups = size(natoms)

  ! Allocate data
  if ( allocated(this%group) ) deallocate( this%group, STAT=ndx_allocate_all )
  allocate( this%group(this%ngroups), STAT=ndx_allocate_all )
  if ( ndx_allocate_all /= 0 ) then
    ndx_allocate_all = xslibNOMEM
    return
  end if

  ! Allocate each group
  do i = 1, this%ngroups
    ndx_allocate_all = this%group(i)%allocate( natoms(i) )
    if ( ndx_allocate_all /= xslibOK ) return
  end do

  ! Return success
  ndx_allocate_all = xslibOK

  return
end function ndx_allocate_all

! Copy index file.
subroutine ndx_assign( this, other )
  implicit none
  class(ndx_t), intent(inout) :: this
  type(ndx_t), intent(in)     :: other
  integer                     :: stat

  this%ngroups = other%ngroups
  if ( allocated(other%group) ) then
    stat = this%allocate( other%ngroups )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%group(:) = other%group

  end if

  return
end subroutine ndx_assign

! Read GROMACS index .ndx file
integer function ndx_read( this, file )
  use, intrinsic :: iso_fortran_env
  implicit none
  class(ndx_t)             :: this
  character(*), intent(in) :: file
  integer                  :: n, unit, stat
  character(8)             :: buffer

  ! Open file
  open( NEWUNIT=unit, FILE=trim(file), STATUS="old", ACTION="read", IOSTAT=stat )
  if ( stat /= 0 ) then
    ndx_read = xslibFILENOTFOUND
    return
  end if

  ! Is it index file?
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    ndx_read = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if
  if ( index(buffer(:),"[") == 0 ) then
    ndx_read = xslibHEADER ! Not an index file
    return
  end if

  ! Count number of index groups.
  this%ngroups = 1 ! One was already found.
  do while( stat == 0 )
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat /= 0 ) exit
    if ( index(buffer,"[") /= 0 ) this%ngroups = this%ngroups+1

  end do

  ! Go to beginning
  rewind( unit, IOSTAT=stat )

  ! ---------------------

  ! Allocate groups
  ndx_read = this%allocate( this%ngroups )
  if ( ndx_read /= xslibOK ) return

  ! For each index group
  do n = 1, this%ngroups
    ndx_read = this%group(n)%read( unit )
    if ( ndx_read /= xslibOK ) return

  end do ! for n

  ! Close file
  close( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    ndx_read = xslibCLOSE
    return
  end if

  ! Return success
  ndx_read = xslibOK

  return
end function ndx_read

! Write index file to file UNIT, FILE or STDOUT
integer function ndx_write( this, file, unit )
  use iso_fortran_env
  implicit none
  class(ndx_t)                        :: this
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: unit
  integer                             :: n, out, stat

  ! Select output method depending on what arguments are provided.
  if ( present(file) ) then
    ! Open file
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      ndx_write = xslibFILENOTFOUND
      return
    end if

  else if ( present(unit) ) then
    out = unit

  else
    out = OUTPUT_UNIT

  end if

  ! Check if data is allocated
  if ( .not. allocated(this%group) ) then
    ndx_write = xslibNOMEM
    return
  end if

  ! For each group
  do n = 1, this%ngroups
    ndx_write = this%group(n)%write( out )
    if ( ndx_write /= xslibOK ) return

  end do ! for n

  ! Close unit only for newly opened file
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      ndx_write = xslibCLOSE
    end if
  end if

  ! Return sucess
  ndx_write = xslibOK

  return
end function ndx_write

! Display ndx file information to stdout.
integer function ndx_display( this )
  implicit none
  class(ndx_t)  :: this
  integer       :: i

  ! Present static index groups:
  !  Group  X "System" (xxxxx atoms)
  !  ...

  write (*,*) "Present static index groups:"
  if ( allocated(this%group) ) then
    do i = 1, this%ngroups
      write (*,100) i, trim(this%group(i)%title), this%group(i)%natoms
      100 format ( 2x, "Group  ", i0, " '", a, "' ( ", i0, " atoms )" )
    end do

  else
    write (*,"(2x,a)") "NONE"

  end if

  ! Return success
  ndx_display = xslibOK

  return
end function ndx_display

! -------------------------------------------------
! Utilities

! Count number of atoms (indeces) in one index group
integer function indexCount( unit )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in) :: unit
  integer             :: i, nlines, stat
  character(128)      :: buffer

  ! Count number of atoms in group
  indexCount = 0
  nlines = 1
  do while ( .true. )
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat == IOSTAT_END ) exit

    ! Ops ... start of next group. Go back.
    if ( index( buffer, "[" ) /= 0) exit

    ! Number of atoms is the number of columns
    indexCount = indexCount+cnttok( buffer, " " )

    ! Increment number of lines read
    nlines = nlines+1

  end do ! while

  ! Rewind by number of lines
  do i = 1, nlines
    backspace( unit )
  end do

  return
end function indexCount

! Count number of indeces (atoms) in one index group
! NOTE to self: For some fucking reason fseek and ftell do not work.
integer function indexCount_( unit )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END, INT64
  implicit none
  integer, intent(in) :: unit
  integer             :: stat
  character(128)      :: buffer
  integer, parameter  :: SEEK_SET=0, SEEK_CUR=1, SEEK_END=2
  integer(INT64)      :: offset

  ! Current position
  ! read (unit,*)
  offset = ftell( unit )

  ! Count number of atoms in group
  indexCount_ = 0
  do while ( .true. )
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat /= 0 ) exit

    ! Ops ... start of next group. Go back.
    if ( index(buffer,"[") /= 0) exit
    indexCount_ = indexCount_+cnttok( buffer, " " )

  end do ! while

  ! Return to starting position
  call fseek( unit, offset, SEEK_SET )

  return
end function indexCount_

! Breaks string str into a series of tokens using the delimiter delim.
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

end module xslib_ndxio
