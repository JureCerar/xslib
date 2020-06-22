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
  public :: xyz_open_file, xyz_close_file, xyz_read_header, xyz_read_data
  public :: xyz_read_coor, xyz_skip_data, xyz_write_data

  ! Import error definitions
  include "fileio.h"

  type xyz_frame
    integer                   :: natoms = 0
    character(128)            :: comment = ""
    character(6), allocatable :: name(:)
    real, allocatable         :: coor(:,:)
    real                      :: box(3,3) = 0.000
  contains
    procedure :: allocate => xyz_frame_allocate
    procedure :: assign => xyz_frame_assign
    generic   :: assignment(=) => assign
    procedure :: copy => xyz_frame_copy
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
! Class independant routines

! Open and check .xyz file
integer function xyz_open_file( unit, file, nframes, natoms, box )
  implicit none
  integer, intent(out)      :: unit
  character(*), intent(in)  :: file
  integer, intent(out)      :: nframes, natoms
  real, intent(out)         :: box(3,3)
  real                      :: ibox(3,3)
  integer                   :: inatoms, stat
  character(128)            :: comment
  logical                   :: exist

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    xyz_open_file = xslibFILENOTFOUND
    return
  end if

  ! Open file
  open ( NEWUNIT=unit, FILE=trim(file), STATUS="old", ACTION="read", IOSTAT=stat )
  if ( stat /= 0 ) then
    xyz_open_file = xslibOPEN
    return
  end if

  ! Count and check all frames
  nframes = 0
  box(:,:) = 0.000
  natoms = 0
  do while( .true. )
    ! Read header
    xyz_open_file = xyz_read_header( unit, inatoms, comment, ibox )
    if ( xyz_open_file == xslibENDOFFILE ) exit
    if ( xyz_open_file /= xslibOK ) return

    ! Skip the rest of the frame
    xyz_open_file = xyz_skip_data( unit, inatoms )
    if ( xyz_open_file /= xslibOK ) return

    ! Update data
    nframes = nframes+1
    box(:,:) = max( box, ibox )
    natoms = max( natoms, inatoms )

  end do ! while

  ! Rewind file
  rewind ( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    xyz_open_file = xslibOPEN
    return
  end if

  ! Return sucess
  xyz_open_file = xslibOK

  return
end function xyz_open_file

! Read .xyz header
integer function xyz_read_header( unit, natoms, comment, box )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit
  integer, intent(out)      :: natoms
  character(*), intent(out) :: comment
  real, intent(out)         :: box(3,3)
  integer                   :: pos, stat
  logical                   :: opened

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened )
  if ( .not. opened ) then
    xyz_read_header = xslibOPEN
    return
  end if

  ! Read number of atoms
  read (unit,*,IOSTAT=stat) natoms
  if ( stat /= 0 ) then
    xyz_read_header = merge( xslibENDOFFILE, xslibINT, stat == IOSTAT_END )
    return
  end if

  ! Read title
  read (unit,"(a)",IOSTAT=stat) comment
  if ( stat /= 0 ) then
    xyz_read_header =  merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  ! Try to extract box information from comment.
  ! * <comment> box= 0.000 0.000 0.000
  box(:,:) = 0.000
  pos = index( comment, "box=" )
  if ( pos > 0 ) then
    read (comment(pos+4:),*,IOSTAT=stat) box(1,1), box(2,2), box(3,3)
  end if

  ! Return success
  xyz_read_header = xslibOK

  return
end function xyz_read_header

! Read .xyz data
integer function xyz_read_data( unit, natoms, name, coor )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit, natoms
  real, intent(out)         :: coor(3,natoms)
  character(6), intent(out) :: name(natoms)
  integer                   :: i, nx, stat
  character(128)            :: buffer
  logical                   :: opened

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened )
  if ( .not. opened ) then
    xyz_read_data = xslibOPEN
    return
  end if

  ! Read line and return before evaluated line.
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat == IOSTAT_END ) then
    xyz_read_data = xslibENDOFFILE
    return
  end if

  ! Count number of tokens (function defined below)
  nx = cnttok( buffer, " " )
  if ( nx < 3 ) then
    xyz_read_data = xslibHEADER
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
        xyz_read_data = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
        return
      end if

    end do ! for i
  else
    ! Read only coordinates
    do i = 1, natoms
      read (unit,*,IOSTAT=stat) coor(:,i)
      if ( stat /= 0 ) then
          xyz_read_data = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
        return
      end if

    end do ! for i
    ! No names
    name(:) = ""

  end if

  ! Return success
  xyz_read_data = xslibOK

  return
end function xyz_read_data

! Read only .xyz coordinates
integer function xyz_read_coor( unit, natoms, coor )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in) :: unit, natoms
  real, intent(out)   :: coor(3,natoms)
  integer             :: i, nx, stat
  character(128)      :: buffer
  logical             :: opened

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened )
  if ( .not. opened ) then
    xyz_read_coor = xslibOPEN
    return
  end if

  ! Read line and return before evaluated line.
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat == IOSTAT_END ) then
    xyz_read_coor = xslibENDOFFILE
    return
  end if

  ! Count number of tokens (function defined below)
  nx = cnttok( buffer, " " )
  if ( nx < 3 ) then
    xyz_read_coor = xslibHEADER
    return
  end if

  ! Rewind before procesed line.
  backspace( unit, IOSTAT=stat )

  ! Read data according to number of argument on the line.
  if ( nx > 3 ) then
    ! Read name and coordinates
    do i = 1, natoms
      read (unit,*,IOSTAT=stat) buffer, coor(:,i)
      if ( stat /= 0 ) then
        xyz_read_coor = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
        return
      end if
    end do ! for i
  else
    ! Read only coordinates
    do i = 1, natoms
      read (unit,*,IOSTAT=stat) coor(:,i)
      if ( stat /= 0 ) then
          xyz_read_coor = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
        return
      end if
    end do ! for i
  end if

  ! Return success
  xyz_read_coor = xslibOK

  return
end function xyz_read_coor

! Skip .xyz data.
integer function xyz_skip_data( unit, natoms )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in) :: unit, natoms
  integer             :: i, stat
  logical             :: opened
  character(9)        :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    xyz_skip_data = xslibOPEN
    return
  end if

  ! Skip all atoms
  do i = 1, natoms
    read (unit,*,IOSTAT=stat) ! Dummy read
    if ( stat /= 0 ) then
      xyz_skip_data = xslibENDOFFILE
      return
    end if
  end do

  ! Return success
  xyz_skip_data = xslibOK

  return
end function xyz_skip_data

! Output data (header included) in .xyz format.
integer function xyz_write_data( unit, natoms, comment, box, name, coor )
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
    xyz_write_data = xslibOPEN
    return
  end if

  ! Write natoms
  write (unit,"(i0)") natoms

  ! Write comment + box
  if ( index(comment,"box=") /= 0 ) then
    write (unit,"(a)") trim(comment)
  else
    write (unit,"(a, '; box= ', 3(f10.5,x))") trim(comment), box(1,1), box(2,2), box(3,3)
  end if

  ! Write all atoms and names
  do i = 1, natoms
    write (unit,*) trim(name(i)), coor(:,i)
  end do ! for i

  ! Return success
  xyz_write_data = xslibOK

  return
end function xyz_write_data

! Close .xyz file handle.
integer function xyz_close_file( unit )
  implicit none
  integer, intent(in) :: unit
  integer             :: stat
  close ( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    xyz_close_file = xslibCLOSE
  else
    xyz_close_file = xslibOK
  end if
  return
end function xyz_close_file

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
  this%comment = other%comment

  ! Copy data
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

  ! Read Header
  xyz_frame_read = xyz_read_header( unit, this%natoms, this%comment, this%box )
  if ( xyz_frame_read /= xslibOK ) return

  ! Allocated data
  xyz_frame_read = this%allocate( this%natoms )
  if ( xyz_frame_read /= xslibOK ) return

  ! Read data
  xyz_frame_read = xyz_read_data( unit, this%natoms, this%name, this%coor  )
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
  xyz_frame_write = xyz_write_data( unit, this%natoms, this%comment, this%box, this%name, this%coor )

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
integer function xyz_frame_copy( this, obj, dest, src, num )
  implicit none
  class(xyz_frame)            :: this
  type(xyz_frame), intent(in) :: obj
  integer, intent(in)         :: dest, src, num

  ! Size check
  if ( (src+num-1) > obj%natoms .or. (dest+num-1) > this%natoms ) then
    xyz_frame_copy = xslibNOMEM
    return
  end if

  ! Copy data
  this%name(dest:dest+num-1)   = obj%name(src:src+num-1)
  this%coor(:,dest:dest+num-1) = obj%coor(:,src:src+num-1)

  ! Return sucess
  xyz_frame_copy = xslibOK

  return
end function xyz_frame_copy

! Comment
integer function xyz_open( this, file )
  implicit none
  class(xyz_t)              :: this
  character(*), intent(in)  :: file

  ! Open file
  xyz_open = xyz_open_file( this%unit, file, this%allframes, this%natoms, this%box )
  if ( xyz_open /= xslibOK ) return

  ! All frames remain
  this%remaining = this%allframes

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
  character(128)                :: comment

  do i = 1, min( merge( nframes, 1, present(nframes) ), this%remaining )
    ! Read header
    xyz_skip_next = xyz_read_header( this%unit, natoms, comment, box )
    if ( xyz_skip_next /= xslibOK ) return

    ! Skip coordinates
    xyz_skip_next = xyz_skip_data( this%unit, natoms)
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
  xyz_close = xyz_close_file( this%unit )
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
