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

module xslib_xyzio
  implicit none
  private
  public :: xyz_t, xyz_frame

  ! Import error definitions
  include "fileio.h"

  type xyz_frame
    integer                   :: natoms = 0
    character(:), allocatable :: comment
    character(6), allocatable :: name(:)
    real, allocatable         :: coor(:,:)
    real                      :: box(3,3) = 0.000
  contains
    procedure :: allocate => xyz_frame_allocate
    procedure :: assign => xyz_frame_assign
    generic   :: assignment(=) => assign
    procedure :: write => xyz_frame_write
    procedure :: read => xyz_frame_read
  end type xyz_frame

  type xyz_t
    integer, private              :: unit = 0, natoms = 0, allframes = 0, remaining = 0
    real, private                 :: box(3,3) = 0.000
    integer                       :: nframes = 0
    type(xyz_frame), allocatable  :: frame(:)
  contains
    procedure :: xyz_allocate, xyz_allocate_all
    generic   :: allocate => xyz_allocate, xyz_allocate_all
    procedure :: assign => xyz_assign
    generic   :: assignment(=) => assign
    procedure :: open => xyz_open
    procedure :: close => xyz_close
    procedure :: read => xyz_read
    procedure :: read_next => xyz_read_next
    procedure :: skip_next => xyz_skip_next
    procedure :: write => xyz_write
    procedure :: getAllframes => xyz_getAllframes
    procedure :: getNatoms => xyz_getNatoms
    procedure :: getBox => xyz_getCubicBox
  end type xyz_t

contains

! -------------------------------------------------
! Class independant (low-level) routines

! Comment
integer function xyz_header( unit, natoms, comment )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)                     :: unit
  integer, intent(out)                    :: natoms
  character(:), allocatable, intent(out)  :: comment
  character(128)                          :: buffer
  integer                                 :: stat
  logical                                 :: opened

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened )
  if ( .not. opened ) then
    xyz_header = xslibOPEN
    return
  end if

  ! Read number of atoms
  read (unit,*,IOSTAT=stat) natoms
  if ( stat /= 0 ) then
    xyz_header = merge( xslibENDOFFILE, xslibINT, stat == IOSTAT_END )
    return
  end if

  ! Read title
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    xyz_header =  merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if
  comment = trim(buffer)

  ! TODO: Try to extract box information from comment.
  ! * <comment> box= 0.000 0.000 0.000

  ! Return success
  xyz_header = xslibOK

  return
end function xyz_header

! Comment
integer function xyz_coor( unit, natoms, box, name, coor )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit, natoms
  real, intent(out)         :: coor(3,natoms)
  character(6), intent(out) :: name(natoms)
  real, intent(out)         :: box(3,3)
  integer                   :: i, nx, stat
  character(128)            :: buffer
  logical                   :: opened

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened )
  if ( .not. opened ) then
    xyz_coor = xslibOPEN
    return
  end if

  ! Read line and return before evaluated line.
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat == IOSTAT_END ) then
    xyz_coor = xslibENDOFFILE
    return
  end if

  ! Count number of tokens (function defined below)
  nx = cnttok( buffer, " " )
  if ( nx < 3 ) then
    xyz_coor = xslib3DX
    return
  end if

  ! Rewind before procesed line.
  backspace( unit, IOSTAT=stat )

  ! Read data according to number of argument on the line.
  if ( nx > 3 ) then
    ! Read name and coordinates
    do i = 1, natoms
      read (unit,*,IOSTAT=stat) name(i), coor(:,i)
      if ( stat /= 0 ) then
        xyz_coor = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
        return
      end if

    end do ! for i
  else
    ! Read only coordinates
    do i = 1, natoms
      read (unit,*,IOSTAT=stat) coor(:,i)
      if ( stat /= 0 ) then
          xyz_coor = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
        return
      end if

    end do ! for i
    ! No names
    name(:) = ""

  end if

  ! NOTE: XYZ file does not contain any box size data. Return dummy data.
  box(:,:) = 0.000

  ! Return success
  xyz_coor = xslibOK

  return
end function xyz_coor

! Comment
integer function xyz_coor_skip( unit, natoms, box )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in) :: unit, natoms
  real, intent(out)   :: box(3,3)
  integer             :: i, stat
  logical             :: opened
  character(9)        :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    xyz_coor_skip = xslibOPEN
    return
  end if

  ! Skip all atoms. +1 is for comment
  do i = 1, natoms
    read (unit,*,IOSTAT=stat) ! dummy
    if ( stat /= 0 ) then
      xyz_coor_skip = xslibENDOFFILE
      return
    end if
  end do

  ! NOTE: XYZ file does not contain any box size data. Return dummy data.
  box(:,:) = 0.000

  ! Return success
  xyz_coor_skip = xslibOK

  return
end function xyz_coor_skip

! Comment
integer function xyz_output( unit, natoms, comment, box, name, coor )
  implicit none
  integer, intent(in)       :: unit, natoms
  character(*), intent(in)  :: comment
  real, intent(in)          :: box(3,3)
  character(6), intent(in)  :: name(natoms)
  real, intent(in)          :: coor(3,natoms)
  logical                   :: opened
  integer                   :: i
  character(9)              :: action

  ! Check file unit
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    xyz_output = xslibOPEN
    return
  end if

  ! Write natoms
  write (unit,"(i0)") natoms

  ! Write comment + box
  if ( index(comment,"box=") /= 0 ) then
    write (unit,"(a)") trim(comment)
  else
    write (unit,"(a, ' box= ', 3(f10.5,x))") trim(comment), box(1,1), box(2,2), box(3,3)
  end if

  ! Write all atoms and names
  do i = 1, natoms
    write (unit,*) trim(name(i)), coor(:,i)

  end do ! for i

  ! Return success
  xyz_output = xslibOK

  return
end function xyz_output

! -------------------------------------------------
! xyz_frame procedures

! Deallocate and allocate XYZ file
integer function xyz_frame_allocate( this, np )
  class(xyz_frame)    :: this
  integer, intent(in) :: np  ! Number of points to allocate
  integer             :: stat
  ! Set number of atoms
  this%natoms = np
  ! Allocate atom names
  if ( allocated(this%coor) ) deallocate( this%coor, this%name, STAT=stat )
  allocate( this%name(this%natoms), this%coor(3,this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    xyz_frame_allocate = xslibNOMEM
    return
  end if
  ! Return success
  xyz_frame_allocate = xslibOK
  return
end function xyz_frame_allocate

! Comment
subroutine xyz_frame_assign( this, other )
  class(xyz_frame), intent(inout) :: this
  type(xyz_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%box(:,:) = other%box
  if ( allocated(other%comment) ) then
     this%comment = other%comment
  end if

  ! Copy data
  ! TODO: Check all parameters
  if ( allocated(other%coor) ) then
    ! natoms is set by allocation.
    stat = this%allocate( other%natoms )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%coor(:,:) = other%coor
    this%name(:)   = other%name

  end if

  return
end subroutine xyz_frame_assign

! Comment
integer function xyz_frame_read( this, unit )
  implicit none
  class(xyz_frame)    :: this
  integer, intent(in) :: unit

  ! NOTE: File UNIT check is done by low-level routines.

  ! Read Header
  xyz_frame_read = xyz_header( unit, this%natoms, this%comment )
  if ( xyz_frame_read /= xslibOK ) return

  ! Allocated data
  xyz_frame_read = this%allocate( this%natoms )
  if ( xyz_frame_read /= xslibOK ) return

  ! Read data
  xyz_frame_read = xyz_coor( unit, this%natoms, this%box, this%name, this%coor  )
  if ( xyz_frame_read /= xslibOK ) return

  return
end function xyz_frame_read

! Comment
integer function xyz_frame_write( this, unit )
  implicit none
  class(xyz_frame)    :: this
  integer, intent(in) :: unit

  ! Check memory
  if ( .not. allocated(this%coor) ) then
    xyz_frame_write = xslibNOMEM
    return
  end if

  ! Write header and coor
  xyz_frame_write = xyz_output( unit, this%natoms, this%comment, this%box, this%name, this%coor )

  return
end function xyz_frame_write

! -------------------------------------------------
! xyz_t procedures

! Comment
integer function xyz_allocate( this, nframes )
  implicit none
  class(xyz_t)        :: this
  integer, intent(in) :: nframes
  integer             :: stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(nframes), STAT=stat )
  if ( stat /= 0 ) then
    xyz_allocate = xslibNOMEM
    return
  end if

  ! Return success
  xyz_allocate = xslibOK

  return
end function xyz_allocate

! Comment
integer function xyz_allocate_all( this, natoms, nframes )
  implicit none
  class(xyz_t)        :: this
  integer, intent(in) :: nframes, natoms
  integer             :: i, stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(nframes), STAT=stat )
  if ( stat /= 0 ) then
    xyz_allocate_all = xslibNOMEM
    return
  end if

  ! Allocate each frame
  do i = 1, this%nframes
    xyz_allocate_all = this%frame(i)%allocate( natoms )
    if ( xyz_allocate_all /= xslibOK ) return
  end do

  ! Return success
  xyz_allocate_all = xslibOK

  return
end function xyz_allocate_all

! Comment
subroutine xyz_assign( this, other )
  class(xyz_t), intent(inout) :: this
  type(xyz_t), intent(in)     :: other
  integer                     :: stat

  ! Copy private data
  this%unit = other%unit
  this%allframes = other%allframes
  this%box(:,:) = other%box

  ! Copy frames
  this%natoms = other%natoms
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%nframes )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine xyz_assign

! Comment
integer function xyz_open( this, file )
  implicit none
  class(xyz_t)              :: this
  character(*), intent(in)  :: file
  logical                   :: exist
  integer                   :: natoms, stat
  real                      :: box(3,3)
  character(:), allocatable :: comment

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    xyz_open = xslibFILENOTFOUND
    return
  end if

  ! Open and check file
  open( NEWUNIT=this%unit, FILE=trim(file), STATUS="old", ACTION="read", IOSTAT=stat )
  if ( stat /= 0 ) then
    xyz_open = xslibOPEN
    return
  end if

  ! Read XYZ header. Only save natoms.
  xyz_open = xyz_header( this%unit, this%natoms, comment )
  if ( xyz_open /= xslibOK ) return

  ! Read the rest of the frame. Save only box.
  xyz_open = xyz_coor_skip( this%unit, this%natoms, this%box )
  if ( xyz_open /= xslibOK ) return

  ! Count the rest of the frames.
  this%allframes = 1 ! One frame was already read.
  do while( xyz_open == xslibOK )
    ! Read hedaer; Stop if end-of-file
    xyz_open = xyz_header( this%unit, natoms, comment )
    if ( xyz_open == xslibENDOFFILE ) exit
    if ( xyz_open /= xslibOK ) return

    ! Read the rest of the frame
    xyz_open = xyz_coor_skip( this%unit, natoms, box )
    if ( xyz_open /= xslibOK ) return

    ! Copy higher number of atoms & box
    this%natoms = merge( this%natoms, natoms, this%natoms > natoms )
    this%box = merge( this%box, box, all(this%box >= box) )

    ! Another frame was read
    this%allframes = this%allframes+1

  end do

  ! Back to begining
  rewind( this%unit, IOSTAT=stat )

  ! All frames remain
  this%remaining = this%allframes

  ! Return success
  xyz_open = xslibOK

  return
end function xyz_open

! Comment
integer function xyz_read( this, file, first, last, stride )
  implicit none
  class(xyz_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: i, nfirst, nlast, nstride

  ! Open file.
  xyz_read = this%open( file )
  if ( xyz_read /= xslibOK ) return

  ! Optional parameters.
  nfirst = merge( first, 1, present(first) )
  nstride = merge( stride, 1, present(last) )
  nlast = merge( last, this%allframes, present(last) )

  ! Prevet doing stupid things.
  if ( nfirst > this%allframes ) nfirst = this%allframes
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Estimate number of frames an allocate data.
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  xyz_read = this%allocate( this%nframes )
  if ( xyz_read /= xslibOK ) return

  ! Skip all frames before first
  xyz_read = this%skip_next( nfirst-1 )
  if ( xyz_read /= xslibOK ) return

  ! ----------------
  ! Read frames
  do i = 1, this%nframes
    ! Read frame
    xyz_read = this%frame(i)%read( this%unit )
    if ( xyz_read /= xslibOK ) return

    ! If not last frame, skip until next frame.
    if ( i /= this%nframes) then
      xyz_read = this%skip_next( nstride-1 )
      if ( xyz_read /= xslibOK ) return

    end if
  end do ! for i

  ! No frames remain
  this%remaining = 0

  ! Close file
  xyz_read = this%close()
  if ( xyz_read /= xslibOK ) return

  ! Return success
  xyz_read = xslibOK

  return
end function xyz_read

! Comment
integer function xyz_read_next( this, nframes )
  implicit none
  class(xyz_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: i

  ! Allocate number of frames that will be read.
  this%nframes = min( merge( nframes, 1, present(nframes) ), this%remaining )
  xyz_read_next = this%allocate( this%nframes )
  if ( xyz_read_next /= xslibOK ) return

  do i = 1, this%nframes
    ! Read frame
    xyz_read_next = this%frame(i)%read( this%unit )
    if ( xyz_read_next /= xslibOK ) return

    ! One frame less to read
    this%remaining = this%remaining-1

  end do

  ! Return success
  xyz_read_next = xslibOK

  return
end function xyz_read_next

! Comment
integer function xyz_skip_next( this, nframes )
  implicit none
  class(xyz_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: natoms, i
  real                          :: box(3,3)
  character(:), allocatable     :: comment

  do i = 1, min( merge( nframes, 1, present(nframes) ), this%remaining )
    ! Read header
    xyz_skip_next = xyz_header( this%unit, natoms, comment )
    if ( xyz_skip_next /= xslibOK ) return

    ! Skip coordinates
    xyz_skip_next = xyz_coor_skip( this%unit, natoms, box )
    if ( xyz_skip_next /= xslibOK ) return

    ! One frame less to read
    this%remaining = this%remaining-1

  end do

  ! Return success
  xyz_skip_next = xslibOK

  return
end function xyz_skip_next

! Comment
integer function xyz_write( this, unit, file )
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(xyz_t)                        :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: i, out, stat

  ! Select output method depending on what arguments are provided.
  ! NOTE: Check for file first. For closing.
  if ( present(file) ) then
    ! Open a new file.
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      xyz_write = xslibFILENOTFOUND
      return
    end if

  else if ( present(unit) ) then
    ! Output to file unit. If opened is checked by low-level routines.
    out = unit

  else
    ! Output to stdout
    out = OUTPUT_UNIT

  end if

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    xyz_write = xslibNOMEM
    return
  end if

  ! Write each frame to output
  do i = 1, this%nframes
    xyz_write = this%frame(i)%write( out )
    if ( xyz_write /= xslibOK ) return

  end do ! for n

  ! Close unit only for newly opened file
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      xyz_write = xslibCLOSE
      return
    end if
  end if

  ! Return success
  xyz_write = xslibOK

  return
end function xyz_write

! Comment
integer function xyz_close( this )
  implicit none
  class(xyz_t)  :: this
  integer       :: stat

  close( this%unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    xyz_close = xslibCLOSE
    return
  end if

  ! Return success
  xyz_close = xslibOK

  return
end function xyz_close

! -------------------------------------------------
! Inquiry functions

! Comment
integer function xyz_getNatoms( this )
  implicit none
  class(xyz_t)  :: this

  xyz_getNatoms = this%natoms

  return
end function xyz_getNatoms

! Comment
integer function xyz_getAllframes( this )
  implicit none
  class(xyz_t)  :: this

  xyz_getAllframes = this%allframes

  return
end function xyz_getAllframes

! Comment
real function xyz_getCubicBox( this )
  implicit none
  class(xyz_t)  :: this
  dimension     :: xyz_getCubicBox(3)

  xyz_getCubicBox(:) = [this%box(1,1), this%box(2,2), this%box(3,3)]

  return
end function xyz_getCubicBox

! Comment
real function xyz_getBox( this )
  implicit none
  class(xyz_t)  :: this
  dimension     :: xyz_getBox(3,3)

  xyz_getBox(:,:) = this%box

  return
end function xyz_getBox

! -------------------------------------------------
! Utilities

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


end module xslib_xyzio
