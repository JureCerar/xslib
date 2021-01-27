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
  use iso_fortran_env, only: INT64
  implicit none
  private
  public :: xyz_t
  ! Class independent procedures (if you are feeling adventurous)
  public :: xyz_open_file, xyz_close_file, xyz_read_header, xyz_read_data, &
  & xyz_read_coor, xyz_skip_data, xyz_write_data, xyz_check_file

  ! Import error definitions
  include "fileio.h"

  ! Global constants
  integer, parameter      :: DIM = 3 ! Default dimension
  integer, parameter      :: XYZ_LEN = 6 ! Length of atomnm and resnm
  integer, parameter      :: XYZ_MAX_LEN = 128 ! Max buffer and comment length
  character(*), parameter :: BOX_KEYWORD = "box=" ! Box string keyword
  integer, parameter      :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2  ! Positioning constants

  type xyz_frame
    character(XYZ_MAX_LEN)          :: comment = ""       ! Comment 
    real                            :: box(DIM,DIM) = 0.  ! Box side 
    integer                         :: natoms = 0         ! Num. of atoms
    character(XYZ_LEN), allocatable :: name(:)            ! Atom name
    real, allocatable               :: coor(:,:)          ! Coordinate
  contains
    procedure :: allocate => xyz_frame_allocate
    procedure :: assign => xyz_frame_assign
    generic   :: assignment(=) => assign
    ! procedure :: copy => xyz_frame_copy
    procedure :: write => xyz_frame_write
    procedure :: read => xyz_frame_read
  end type xyz_frame

  type xyz_t
    integer, private                      :: unit = 0           ! File unit
    integer, private                      :: allframes = 0      ! Num. of frames in currently opened file
    integer, private                      :: natoms = 0         ! Num. of atoms (per frame) in currently opened file
    integer, private                      :: current = 0        ! Current frame in opened file
    real, private                         :: box(DIM,DIM) = 0.  ! Box side 
    integer(INT64), allocatable, private  :: offsets(:)         ! Frames offset table
    ! Public data --------------------------
    integer                               :: nframes            ! Num. of frames
    type(xyz_frame), allocatable          :: frame(:)           ! Frame object
  contains
    procedure :: xyz_allocate, xyz_allocate_all
    generic   :: allocate => xyz_allocate, xyz_allocate_all
    procedure :: xyz_assign
    generic   :: assignment(=) => xyz_assign
    procedure :: open => xyz_open
    procedure :: close => xyz_close
    procedure :: fseek => xyz_fseek
    procedure :: ftell => xyz_ftell
    procedure :: read_next => xyz_read_next
    procedure :: skip_next => xyz_skip_next
    procedure :: read_frame => xyz_read_frame
    procedure :: read => xyz_read
    procedure :: write => xyz_write
    procedure :: getNatoms => xyz_getNatoms
    procedure :: getNframes => xyz_getNframes
    procedure :: getBox => xyz_getBox
  end type xyz_t

contains

! -------------------------------------------------
! Class independent procedures

! Open .XYZ file
integer function xyz_open_file( unit, file, bRead )
  implicit none
  integer, intent(out)      :: unit
  character(*), intent(in)  :: file
  logical, intent(in)       :: bRead ! Open in read mode
  integer                   :: stat
  logical                   :: exist

  ! Open in read or write mode
  if ( bRead ) then
    ! For read mode file must already exist
    inquire( FILE=trim(file), EXIST=exist )
    if ( .not. exist ) then
      xyz_open_file = xslibFILENOTFOUND
      return
    end if

    ! Open file for reading
    open ( NEWUNIT=unit, FILE=trim(file), ACCESS="stream", FORM="formatted", STATUS="old", ACTION="read", IOSTAT=stat )
    if ( stat /= 0 ) then
      xyz_open_file = xslibOPEN
      return
    end if

  else
    ! Open file for writing
    open ( NEWUNIT=unit, FILE=trim(file), ACCESS="stream", FORM="formatted", STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      xyz_open_file = xslibOPEN
      return
    end if

  end if

  ! Return success
  xyz_open_file = xslibOK

  return
end function xyz_open_file

! Close .XYZ file
integer function xyz_close_file( unit )
  implicit none
  integer, intent(inout)  :: unit
  integer                 :: stat
  logical                 :: opened

  ! Close any previously opened files
  inquire( UNIT=unit, OPENED=opened )
  if ( opened ) then
    close( unit, IOSTAT=stat )
    if ( stat /= 0 ) then
      xyz_close_file = xslibCLOSE
      return
    end if
  end if

  ! Return success
  xyz_close_file = xslibOK

  return
end function xyz_close_file

! Read .XYZ file header
integer function xyz_read_header( unit, natoms, comment, box )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit
  integer, intent(out)      :: natoms
  character(*), intent(out) :: comment
  real, intent(out)         :: box(DIM,DIM)
  integer                   :: i, pos, stat
  logical                   :: opened
  character(9)              :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    xyz_read_header = xslibOPEN
    return
  end if

  ! Read number of atoms
  read (unit,*,IOSTAT=stat) natoms
  if ( stat /= 0 ) then
    xyz_read_header = merge( xslibENDOFFILE, xslibINT, stat == IOSTAT_END )
    return
  end if

  ! Read comment line
  read (unit,"(a)",IOSTAT=stat) comment
  if ( stat /= 0 ) then
    xyz_read_header =  merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  ! Try to extract box information from comment.
  ! * <comment> box= 0.000 0.000 0.000
  box(:,:) = 0.000
  pos = index( comment, BOX_KEYWORD )
  if ( pos > 0 ) then
    read (comment(pos+len(BOX_KEYWORD):),*,IOSTAT=stat) (box(i,i), i=1,DIM)
  end if

  ! Return success
  xyz_read_header = xslibOK

  return
end function xyz_read_header

! Read (all) .XYZ file data
integer function xyz_read_data( unit, natoms, name, coor )
  use iso_fortran_env, only: IOSTAT_END, INT64
  implicit none
  integer, intent(in)             :: unit, natoms
  real, intent(out)               :: coor(DIM,natoms)
  character(XYZ_LEN), intent(out) :: name(natoms)
  integer                         :: i, nx, stat
  character(XYZ_MAX_LEN)          :: buffer
  logical                         :: opened
  character(9)                    :: action
  integer(INT64)                  :: pos

  ! Check if unit is opened for reading and get current position
  inquire( UNIT=unit, OPENED=opened, ACTION=action, POS=pos )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    xyz_read_data = xslibOPEN
    return
  end if

  ! Read one line to the buffer
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat == IOSTAT_END ) then
    xyz_read_data = xslibENDOFFILE
    return
  end if

  ! Count number of tokens on line (see utilities below)
  nx = cnttok( buffer, " " )
  if ( nx < DIM ) then
    xyz_read_data = xslibHEADER
    return
  end if

  ! Return to starting position
  backspace( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    xyz_read_data = xslibNR
    return
  end if

  ! Read data according to number of argument on the line.
  if ( nx > DIM ) then
    ! Read names and coordinates
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

    ! Names are empty
    name(:) = ""

  end if

  ! Return success
  xyz_read_data = xslibOK

  return
end function xyz_read_data

! Read (only) .XYZ file coordinates
integer function xyz_read_coor( unit, natoms, coor )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)     :: unit, natoms
  real, intent(out)       :: coor(DIM,natoms)
  integer                 :: i, nx, stat
  character(XYZ_MAX_LEN)  :: buffer
  logical                 :: opened
  character(9)            :: action
  integer(INT64)          :: pos

  ! Check if unit is opened for reading and get current position
  inquire( UNIT=unit, OPENED=opened, ACTION=action, POS=pos )
  if ( .not. opened .and. index(action,"READ") == 0 ) then
    xyz_read_coor = xslibOPEN
    return
  end if

  ! Read one line to the buffer
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat == IOSTAT_END ) then
    xyz_read_coor = xslibENDOFFILE
    return
  end if

  ! Count number of tokens on line (see utilities below)
  nx = cnttok( buffer, " " )
  if ( nx < DIM ) then
    xyz_read_coor = xslibHEADER
    return
  end if

  ! Return to starting position
  backspace( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    xyz_read_coor = xslibNR
    return
  end if

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

! Skip .XYZ file data
integer function xyz_skip_data( unit, natoms )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in) :: unit, natoms
  integer             :: i, stat
  logical             :: opened
  character(9)        :: action

  ! Check if unit is opened for reading
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

  end do ! for i

  ! Return success
  xyz_skip_data = xslibOK

  return
end function xyz_skip_data

! Write data in .XYZ format (header included)
integer function xyz_write_data( unit, natoms, comment, box, name, coor )
  implicit none
  integer, intent(in)       :: unit, natoms
  character(*), intent(in)  :: comment
  real, intent(in)          :: box(DIM,DIM)
  character(*), intent(in)  :: name(natoms)
  real, intent(in)          :: coor(DIM,natoms)
  logical                   :: opened
  integer                   :: i, stat
  character(9)              :: action

  ! Check file unit
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    xyz_write_data = xslibOPEN
    return
  end if

  ! Write natoms
  write (unit,"(i0)") natoms

  ! Write comment and append box to comment
  if ( index(comment,BOX_KEYWORD) /= 0 ) then
    write (unit,"(a)") trim(comment)
  else
    write (unit,"(a,';',x,a,x,3(f10.5,x))") trim(comment), BOX_KEYWORD, (box(i,i), i=1,DIM)
  end if

  ! Write all atoms and names
  do i = 1, natoms
    write (unit,*,IOSTAT=stat) trim(name(i)), coor(:,i)
    if ( stat /= 0 ) then
      xyz_write_data = xslib3DX
      return
    end if

  end do ! for i

  ! Return success
  xyz_write_data = xslibOK

  return
end function xyz_write_data

! Check .XYZ file (check frames and construct offsets table)
integer function xyz_check_file( unit, natoms, nframes, offsets )
  use iso_fortran_env, only: INT64
  implicit none
  integer, intent(in)         :: unit
  integer, intent(out)        :: natoms, nframes
  integer(INT64), allocatable :: offsets(:)
  integer                     :: iatoms, stat
  character(XYZ_MAX_LEN)      :: comment
  real                        :: box(DIM,DIM)
  logical                     :: opened
  character(9)                :: action
  integer(INT64), allocatable :: temp(:)

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    xyz_check_file = xslibOPEN
    return
  end if

  ! Allocate frame offsets (start with size = 32)
  if ( allocated(offsets) ) deallocate( offsets, STAT=stat )
  allocate( offsets(32), SOURCE=0_INT64, STAT=stat )
  if ( stat /= xslibOK ) then
    xyz_check_file = xslibNOMEM
    return
  end if

  ! Initialize
  nframes = 0
  natoms  = 0

  ! Read until the EOF
  do while ( .true. )
    ! Check if offsets table is large enough. If NOT double the array size
    if ( nframes+1 > size(offsets) ) then
      allocate( temp(2*size(offsets)), SOURCE=0_INT64, STAT=stat )
      if ( stat /= 0 ) then
        xyz_check_file = xslibNOMEM
        return
      end if
      temp(:nframes) = offsets(:nframes)
      call move_alloc( temp, offsets )
    end if

    ! Store current position in file
    inquire( UNIT=unit, POS=offsets(nframes+1) )

    ! Read header (exit loop if EOF)
    xyz_check_file = xyz_read_header( unit, iatoms, comment, box )
    if ( xyz_check_file == xslibENDOFFILE ) exit
    if ( xyz_check_file /= xslibOK ) return

    ! Another frame exists
    nframes = nframes+1
    natoms  = max( iatoms, natoms )

    ! Skip the rest of the data
    xyz_check_file = xyz_skip_data( unit, iatoms )
    if ( xyz_check_file /= xslibOK ) return

  end do ! while

  ! Go back to beginning of file (this resets EOF flag)
  rewind( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    xyz_check_file = xslibOPEN
    return
  end if

  ! Return success
  xyz_check_file = xslibOK

  return
end function xyz_check_file

! -------------------------------------------------
! xyz_frame class procedures

! Allocate frame
integer function xyz_frame_allocate( this, natoms )
  class(xyz_frame)    :: this
  integer, intent(in) :: natoms  ! Number of atoms in frame
  integer             :: stat
  ! Set number of atoms
  this%natoms = natoms
  ! Allocate atom names
  if ( allocated(this%coor) ) deallocate( this%coor, this%name, STAT=stat )
  allocate( this%name(this%natoms), this%coor(DIM,this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    xyz_frame_allocate = xslibNOMEM
    return
  end if
  ! Return success
  xyz_frame_allocate = xslibOK
  return
end function xyz_frame_allocate

! Assigment(=) for xyz_frame class
subroutine xyz_frame_assign( this, other )
  class(xyz_frame), intent(inout) :: this
  type(xyz_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%box(:,:) = other%box
  this%comment  = other%comment
  this%natoms   = other%natoms

  ! Copy data
  ! TODO: check all parameters
  if ( allocated(other%coor) ) then
    stat = this%allocate( other%natoms )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%coor(:,:) = other%coor
    this%name(:)   = other%name

  end if

  return
end subroutine xyz_frame_assign

! Copy data from frame to frame
! integer function xyz_frame_copy( this, obj, dest, src, num )
!   implicit none
!   class(xyz_frame)            :: this
!   type(xyz_frame), intent(in) :: obj
!   integer, intent(in)         :: dest, src, num
!
!   ! Bound check
!   if ( (src+num-1) > obj%natoms .or. (dest+num-1) > this%natoms ) then
!     xyz_frame_copy = xslibNOMEM
!     return
!   end if
!
!   ! Copy data
!   this%name(dest:dest+num-1)   = obj%name(src:src+num-1)
!   this%coor(:,dest:dest+num-1) = obj%coor(:,src:src+num-1)
!
!   ! Return success
!   xyz_frame_copy = xslibOK
!
!   return
! end function xyz_frame_copy

! Read .XYZ frame
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

! Write .XYZ frame
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
! xyz_t class procedures

! Allocate NFRAMES of frames (only allocates frames)
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

! Allocate NFRAMES of frames each with NATOMS of atoms
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

! Assigment(=) for xyz_t class
subroutine xyz_assign( this, other )
  class(xyz_t), intent(inout) :: this
  type(xyz_t), intent(in)     :: other
  integer                     :: stat

  ! Copy private data
  this%unit      = other%unit
  this%allFrames = other%allFrames
  this%natoms    = other%natoms
  this%current   = other%current

  ! Copy frame offsets
  if ( allocated(other%offsets) ) then
    if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )
    allocate( this%offsets(other%allFrames+1), STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%offsets(:) = other%offsets(1:other%allFrames+1) ! +1 for EOF
  end if

  ! Copy all frames
  this%natoms = other%natoms
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%nframes )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine xyz_assign

! Open (& check) .XYZ file and construct frame offsets table
integer function xyz_open( this, file )
  use iso_fortran_env, only: INT64
  implicit none
  class(xyz_t)                :: this
  character(*), intent(in)    :: file

  ! Open file for reading
  xyz_open = xyz_open_file( this%unit, file, .true. )
  if ( xyz_open /= xslibOK ) return

  ! Check file and get frame offsets
  xyz_open = xyz_check_file( this%unit, this%natoms, this%allframes, this%offsets )
  if ( xyz_open /= xslibOK ) return

  ! Set current frame to 1
  this%current = 1

  ! Return success
  xyz_open = xslibOK

  return
end function xyz_open

! Close .XYZ file and clean-up
integer function xyz_close( this )
  implicit none
  class(xyz_t)  :: this
  integer       :: stat

  ! Close file
  xyz_close = xyz_close_file( this%unit )
  if ( xyz_close /= xslibOK ) return

  ! Reset private variables
  this%allFrames = 0
  this%natoms    = 0
  this%current   = 0

  ! Clean-up offsets table
  if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )

  ! Return success
  xyz_close = xslibOK

  return
end function xyz_close

! Moves UNIT to the specified FRAME.
! * If WHENCE is set to 0, the OFFSET is taken as an absolute value,
! * if set to 1, OFFSET is taken to be relative to the current position,
! * and if set to 2, relative to the end of the file.
integer function xyz_fseek( this, offset, whence )
  implicit none
  class(xyz_t)        :: this
  integer, intent(in) :: offset, whence
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
  logical             :: opened
  integer             :: frame, stat

  ! Check if unit is opened.
  inquire ( UNIT=this%unit, OPENED=opened )
  if ( .not. opened ) then
    xyz_fseek = xslibOPEN
    return
  end if

  ! Check if offsets table is allocated
  if ( .not. allocated(this%offsets) ) then
    xyz_fseek = xslibNOMEM
    return
  end if

  ! Calculate which frame to move to
  select case ( whence )
  case ( SEEK_SET )
    frame = offset
  case ( SEEK_CUR )
    frame = this%current + offset
  case ( SEEK_END )
    frame = this%allframes + offset
  case default
    xyz_fseek = xslibNR
    return
  end select

  ! Limit selection to bounds
  frame = max( frame, 1 )
  frame = min( frame, this%allframes+1 ) ! +1 for EOF

  ! Move to specified frame
  if ( this%offsets(frame) < 4 ) then
    rewind( this%unit, IOSTAT=stat )
  else
    ! Account 32bit/4byte offset for dummy read
    read (this%unit,*,POS=this%offsets(frame)-4,IOSTAT=stat) ! Dummy read
  end if
  if ( stat /= 0 ) then
    xyz_fseek = xslibENDOFFILE
    return
  end if

  ! Update current frame
  this%current = frame

  ! Return success
  xyz_fseek = xslibOK

  return
end function xyz_fseek

! Retrieves the current position within an open file.
integer function xyz_ftell( this )
  implicit none
  class(xyz_t)  :: this
  xyz_ftell = this%current
  return
end function xyz_ftell

! Read next <N> frames in .XYZ file
integer function xyz_read_next( this, nframes )
  implicit none
  class(xyz_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: frames, remaining, i

  ! Optional argument
  frames = merge( nframes, 1, present(nframes) )
  if ( frames < 0 ) frames = 1

  ! Calculate remaining frames
  remaining = this%allFrames-this%current+1

  ! Allocate frames
  this%nframes = min( frames, remaining )
  xyz_read_next = this%allocate( this%nframes )
  if ( xyz_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    xyz_read_next = this%frame(i)%read( this%unit )
    if ( xyz_read_next /= xslibOK ) return

    ! Increment current frame
    this%current = this%current+1

  end do

  ! Return success
  xyz_read_next = xslibOK

  return
end function xyz_read_next

! Skip next <N> frames in .XYZ file
integer function xyz_skip_next( this, nframes )
  implicit none
  class(xyz_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: np

  ! Move to selected frame aka. "skip frames"
  np = merge( nframes, 1, present(nframes) )
  xyz_skip_next = this%fseek( np, SEEK_CUR )

  return
end function xyz_skip_next

! Read selected frame in .XYZ file
integer function xyz_read_frame( this, frame )
  implicit none
  class(xyz_t)        :: this
  integer, intent(in) :: frame

  ! Move to selected frame
  if ( this%current /= frame ) then
    xyz_read_frame = this%fseek( frame, SEEK_SET )
    if ( xyz_read_frame /= 0 ) return
  end if

  ! Read frame
  xyz_read_frame = this%read_next( 1 )

  return
end function xyz_read_frame

! Read entire .XYZ file
integer function xyz_read( this, file, first, last, stride )
  implicit none
  class(xyz_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: nfirst, nlast, nstride
  integer                       :: i, frame

  ! Open file.
  xyz_read = this%open( file )
  if ( xyz_read /= xslibOK ) return

  ! Check optional arguments
  nfirst  = merge( first, 1, present(first) )
  nlast   = merge( last, -1, present(last) )
  nstride = merge( stride, 1, present(stride) )

  ! Prevet doing stupid things
  if ( nfirst < 0 ) nfirst = 1
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Calculate number of frames and allocate data
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  xyz_read = this%allocate( this%nframes )
  if ( xyz_read /= xslibOK ) return

  ! Read frames
  do i = 1, this%nframes
    ! Actual frame number
    frame = nfirst + (i-1)*nstride

    ! Move to selected frame
    if ( this%current /= frame ) then
      xyz_read = this%fseek( frame, SEEK_SET )
      if ( xyz_read /= 0 ) return
    end if

    ! Read i-th frame
    xyz_read = this%frame(i)%read( this%unit )
    if ( xyz_read /= xslibOK ) return

    ! Update current frame
    this%current = this%current+1

  end do ! for i

  ! Close file
  xyz_read = this%close()
  if ( xyz_read /= xslibOK ) return

  ! Return success
  xyz_read = xslibOK

  return
end function xyz_read

! Write data in .XYZ format to unit, file or stdout
integer function xyz_write( this, unit, file )
  use iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(xyz_t)                        :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: i, out

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    xyz_write = xslibNOMEM
    return
  end if

  ! Select output destination; Default is stdout.
  if ( present(file) ) then
    xyz_write = xyz_open_file( out, file, .false. )
    if ( xyz_write /= xslibOK ) return
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Write each frame to output
  do i = 1, this%nframes
    xyz_write = this%frame(i)%write( out )
    if ( xyz_write /= xslibOK ) return

  end do ! for n

  ! Close file if present
  if ( present(file) ) then
    xyz_write = xyz_close_file( out )
    if ( xyz_write /= xslibOK ) return
  end if

  ! Return success
  xyz_write = xslibOK

  return
end function xyz_write

! -------------------------------------------------
! gro_t class utilities

! Return max num. of atoms in currently opened file
integer function xyz_getNatoms( this )
  implicit none
  class(xyz_t)  :: this
  xyz_getNatoms = this%natoms
  return
end function xyz_getNatoms

! Return num. of frames in currently opened file
integer function xyz_getNframes( this )
  implicit none
  class(xyz_t)  :: this
  xyz_getNframes = this%allframes
  return
end function xyz_getNframes

! Return largest box in currently opened file
function xyz_getBox( this ) result( box )
  implicit none
  class(xyz_t)  :: this
  real          :: box(DIM,DIM)
  box(:,:) = this%box
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

  ! Initialize stored copy of input string and pointer into input string on first call
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
