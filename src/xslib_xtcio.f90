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

module xslib_xtcio
  use iso_fortran_env, only: INT64
  use xdrfor
  implicit none
  private
  public :: xtc_t
  ! Class independent procedures (if you are feeling adventurous)
  public :: xtc_open_file, xtc_close_file, xtc_header, xtc_data, xtc_skip, xtc_check

  ! Import error definitions
  include "fileio.h"

  ! Global constants
  integer, parameter  :: DIM = 3 ! Default dimension
  integer, parameter  :: MAGIC = 1995 ! Magic constant
  integer, parameter  :: XTC_SHORTHEADER_SIZE = (20+DIM*DIM*4) ! magic natoms step time DIM*DIM_box_vecs natoms
  integer, parameter  :: XTC_SHORT_BYPESPERATOM = 12 ! Short XTCs store each coordinate as a 32-bit float.
  integer, parameter  :: XTC_HEADER_SIZE = (DIM*DIM*4+DIM*2+46) ! Short XTCs store each coordinate as a 32-bit float.
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2 ! Positioning constants

  type xtc_frame
    real              :: prec = 0.000         ! Default precission
    real              :: time = 0.000         ! Simulation of frame
    integer           :: step = 0             ! Simulation step
    integer           :: natoms = 0           ! Num. of atoms in frame
    real, allocatable :: coor(:,:)            ! Coordinate data
    real              :: box(DIM,DIM) = 0.000 ! Simulation box size
  contains
    procedure :: allocate => xtc_frame_allocate
    procedure :: assign => xtc_frame_assign
    generic   :: assignment(=) => assign
    procedure :: read => xtc_frame_read
    procedure :: write => xtc_frame_write
    procedure :: dump => xtc_frame_dump
  end type xtc_frame

  type xtc_t
    type(xdrfile), pointer, private       :: xd => null()      ! XDR file pointer
    integer, private                      :: allFrames = 0     ! Num. of frames in currently opened file
    integer, private                      :: natoms = 0        ! Num. of atoms (per frame) in currently opened file
    integer, private                      :: current = 0       ! Current frame in opened file
    real, private                         :: box(DIM,DIM) = 0. ! Simulation box side
    integer(INT64), allocatable, private  :: offsets(:)        ! Frames offset table
    ! Public data --------------------------
    integer                               :: nframes = 0       ! Num. of frames
    type(xtc_frame), allocatable          :: frame(:)          ! Frame object
  contains
    procedure :: xtc_allocate, xtc_allocate_all
    generic   :: allocate => xtc_allocate, xtc_allocate_all
    procedure :: assign => xtc_assign
    generic   :: assignment(=) => assign
    procedure :: open => xtc_open
    procedure :: close => xtc_close
    procedure :: fseek => xtc_fseek
    procedure :: ftell => xtc_ftell
    procedure :: read_next => xtc_read_next
    procedure :: skip_next => xtc_skip_next
    procedure :: read_frame => xtc_read_frame
    procedure :: read => xtc_read
    procedure :: write => xtc_write
    procedure :: dump => xtc_dump
    procedure :: getNframes => xtc_getNframes
    procedure :: getNatoms => xtc_getNatoms
    procedure :: getBox => xtc_getBox
  end type xtc_t

contains

! -------------------------------------------------
! Class independent procedures
! NOTE: xrdfile read is same as write. He he he.

! Open .XTC file
integer function xtc_open_file( xd, file, bRead )
  use iso_c_binding, only: c_f_pointer, C_NULL_CHAR
  implicit none
  type(xdrfile), pointer, intent(out) :: xd
  character(*), intent(in)            :: file
  logical, intent(in)                 :: bRead ! Open in read mode
  logical                             :: exist

  ! Open file for reading or writing
  if ( bRead ) then
    ! Check if file exists
    inquire( FILE=file, EXIST=exist )
    if ( .not. exist ) then
      xtc_open_file = xslibFILENOTFOUND
      return
    end if
    call c_f_pointer( xdrfile_open( trim(file)//C_NULL_CHAR, 'r' ), xd )

  else
    call c_f_pointer( xdrfile_open( trim(file)//C_NULL_CHAR, 'w' ), xd )

  end if

  ! Check pointer association
  if ( .not. associated(xd) ) then
    xtc_open_file = xslibOPEN
    return
  end if

  ! Return success
  xtc_open_file = xslibOK

  return
end function xtc_open_file

! Close .XTC file
integer function xtc_close_file( xd )
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  xtc_close_file = xdrfile_close( xd )
  return
end function xtc_close_file

! Read/write .XTC file header
integer function xtc_header( xd, natoms, step, time )
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  integer, intent(inout)              :: natoms
  integer, intent(inout)              :: step
  real,  intent(inout)                :: time
  integer                             :: imagic

  ! Check pointer association
  if ( .not. associated(xd) ) then
    xtc_header = xslibOPEN
    return
  end if

  ! Read/write magic number
  imagic = MAGIC
  if ( xdrfile_read_int( imagic, 1, xd ) /= 1 ) then
    xtc_header = xslibENDOFFILE
    return
  end if

  ! Check magic number
  if ( imagic /= MAGIC ) then
    xtc_header = exdrMAGIC
    return
  end if

  ! Number of atoms
  if ( xdrfile_read_int( natoms, 1, xd ) /= 1 ) then
    xtc_header = exdrINT
    return
  end if

  ! Simulation step
  if ( xdrfile_read_int( step, 1, xd ) /= 1 ) then
    xtc_header = exdrINT
    return
  end if

  ! Simulation time
  if ( xdrfile_read_float( time, 1, xd ) /= 1 ) then
    xtc_header = exdrINT
    return
  end if

  ! Return success
  xtc_header = xslibOK

  return
end function xtc_header

! Read/write .XTC file data
integer function xtc_data( xd, natoms, box, coor, prec, bRead )
  implicit none
  type(xdrfile), intent(in), pointer  :: xd
  logical, intent(in)                 :: bRead
  integer, intent(in)                 :: natoms
  real, intent(inout)                 :: box(DIM,DIM)
  real, intent(inout)                 :: coor(DIM,natoms)
  real, intent(inout)                 :: prec

  ! Check pointer association
  if ( .not. associated(xd) ) then
    xtc_data = xslibOPEN
    return
  end if

  ! Box dimmension
  if ( xdrfile_read_float( box, DIM*DIM, xd ) /= size(box) ) then
    xtc_data = exdr3DX
    return
  end if

  ! Read coordinates
  if ( bRead ) then
    if ( xdrfile_decompress_coord_float( coor, natoms, prec, xd ) /= natoms ) then
      xtc_data = exdr3DX
      return
    end if
  else
    if ( xdrfile_compress_coord_float( coor, natoms, prec, xd ) /= natoms ) then
      xtc_data = exdr3DX
      return
    end if
  end if

  ! Return success
  xtc_data = xslibOK

  return
end function xtc_data

! Skip .xtc data
integer function xtc_skip( xd, natoms, box )
  use iso_fortran_env, only: INT64
  implicit none
  type(xdrfile), intent(in), pointer  :: xd
  integer, intent(in)                 :: natoms
  real, intent(out)                   :: box(DIM,DIM)
  integer                             :: framebytes

  ! Check pointer association
  if ( .not. associated(xd) ) then
    xtc_skip = xslibOPEN
    return
  end if

  ! If less than 10 atoms then the framesize is known
  if ( natoms < 10 ) then
    ! Read simulation box
    if ( xdrfile_read_float( box, DIM*DIM, xd ) /= DIM*DIM ) then
      xtc_skip = exdr3DX
      return
    end if

    ! Calculate frame size and skip
    framebytes = 4+XTC_SHORT_BYPESPERATOM*natoms
    xtc_skip = xdr_seek( xd, int(framebytes,INT64), SEEK_CUR )
    if ( xtc_skip /= xslibOK ) return

  else
    ! Read simulation box
    if ( xdrfile_read_float( box, DIM*DIM, xd ) /= DIM*DIM ) then
      xtc_skip = exdr3DX
      return
    end if

    ! Skip bullshit
    xtc_skip = xdr_seek( xd, int(36,INT64), SEEK_CUR )

    ! Read framebytes
    if ( xdrfile_read_int( framebytes, 1, xd ) == 0 ) then
      xtc_skip = exdrINT
      return
    end if

    ! Round to next 32-bit boundry.
    ! C source: int framebytes = ( framebytes + 3 ) & ~0x03
    framebytes = int(AND((framebytes+3),NOT(x'03')))
    xtc_skip = xdr_seek( xd, int(framebytes,INT64), SEEK_CUR )
    if ( xtc_skip /= xslibOK ) return

  end if

  ! Return success
  xtc_skip = xslibOK

  return
end function xtc_skip

! Check .XTC file (check frames and construct offsets table)
integer function xtc_check( xd, natoms, nframes, offsets )
  use iso_fortran_env, only: INT64
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  integer, intent(out)                :: natoms, nframes
  integer(INT64), allocatable         :: offsets(:)
  real                                :: time, ibox(DIM,DIM)
  integer                             :: iatoms, step, stat
  integer(INT64), allocatable         :: temp(:)

  ! Check pointer association
  if ( .not. associated(xd) ) then
    xtc_check = xslibOPEN
    return
  end if

  ! Allocate frame offsets (start with size = 32)
  if ( allocated(offsets) ) deallocate( offsets, STAT=stat )
  allocate( offsets(32), SOURCE=0_INT64, STAT=stat )
  if ( stat /= 0 ) then
    xtc_check = xslibNOMEM
    return
  end if

  ! Initialize
  nframes = 0
  natoms  = 0

  ! Read until the EOF
  do while ( .true. )
    ! Check if offsets table is large enough. If NOT double the array size
    if ( nframes+1 > size(offsets) ) then
      ! Reallocate data (double the array size)
      allocate( temp(2*size(offsets)), SOURCE=0_INT64, STAT=stat )
      if ( stat /= 0 ) then
        xtc_check = xslibNOMEM
        return
      end if
      temp(:nframes) = offsets(:nframes)
      call move_alloc( temp, offsets )
    end if

    ! Store current position in file
    offsets(nframes+1) = xdr_tell( xd )

    ! Read header (exit loop if EOF)
    xtc_check = xtc_header( xd, iatoms, step, time )
    if ( xtc_check == xslibENDOFFILE ) exit
    if ( xtc_check /= xslibOK ) return

    ! Another frame exists
    nframes = nframes+1

    ! Skip the rest of the frame
    xtc_check = xtc_skip( xd, iatoms, ibox )
    if ( xtc_check /= xslibOK ) return

    ! Keep largest values
    natoms   = max( iatoms, natoms )
    ! box(:,:) = max( ibox, box )

  end do ! while

  ! Rewind file
  xtc_check = xdr_seek( xd, 0_INT64, SEEK_SET )
  if ( xtc_check /= xslibOK ) return

  ! Return success
  xtc_check = xslibOK

  return
end function xtc_check

! -------------------------------------------------
! xtc_frame class procedures

! Allocate .XTC frame
integer function xtc_frame_allocate( this, natoms )
  implicit none
  class(xtc_frame)    :: this
  integer, intent(in) :: natoms
  integer             :: stat
  ! Set number of atoms
  this%natoms = natoms

  ! Allocate residue number
  if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
  allocate( this%coor(DIM,this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    xtc_frame_allocate = xslibNOMEM
    return
  end if

  ! Return success
  xtc_frame_allocate = xslibOK

  return
end function xtc_frame_allocate

! Assigment(=) for xtc_frame class
subroutine xtc_frame_assign( this, other )
  implicit none
  class(xtc_frame), intent(inout) :: this
  type(xtc_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%step     = other%step
  this%prec     = other%prec
  this%time     = other%time
  this%box(:,:) = other%box

  ! Copy data
  this%natoms = other%natoms
  if ( allocated(other%coor) ) then
    stat = this%allocate( other%natoms )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%coor(:,:) = other%coor
  end if

  return
end subroutine xtc_frame_assign

! Read .XTC frame
integer function xtc_frame_read( this, xd )
  implicit none
  class(xtc_frame)                    :: this
  type(xdrfile), pointer, intent(in)  :: xd

  ! Read xtc header
  xtc_frame_read = xtc_header( xd, this%natoms, this%step, this%time )
  if ( xtc_frame_read /= xslibOK ) return

  ! Allocate frame data
  xtc_frame_read = this%allocate( this%natoms )
  if ( xtc_frame_read /= xslibOK ) return

  ! Read xtc data
  xtc_frame_read = xtc_data( xd, this%natoms, this%box, this%coor, this%prec, .true. )
  if ( xtc_frame_read /= xslibOK ) return

  return
end function xtc_frame_read

! Write .XTC frame
integer function xtc_frame_write( this, xd )
  implicit none
  class(xtc_frame)                    :: this
  type(xdrfile), intent(in), pointer  :: xd

  ! Check if data is allocated
  if ( .not. allocated(this%coor) ) then
    xtc_frame_write = xslibNOMEM
    return
  end if

  ! Write xtc header
  xtc_frame_write = xtc_header( xd, this%natoms, this%step, this%time )
  if ( xtc_frame_write /= xslibOK ) return

  ! Write xtc data
  xtc_frame_write = xtc_data( xd, this%natoms, this%box, this%coor, this%prec, .false. )
  if ( xtc_frame_write /= xslibOK ) return

  return
end function xtc_frame_write

! Write .XTC frame in human readable format
integer function xtc_frame_dump( this, unit )
  implicit none
  class(xtc_frame)    :: this
  integer, intent(in) :: unit
  integer             :: i
  logical             :: opened
  character(9)        :: action

  ! Check if unit is opened for writing
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    xtc_frame_dump = xslibOPEN
    return
  end if

  ! Check if data is allocated.
  if ( .not. allocated(this%coor) ) then
    xtc_frame_dump = xslibNOMEM
    return
  end if

  ! Header
  ! * {natoms=         5  step=        10  time=0.0000000e+00  prec=        -1}
  write (unit,100) this%natoms, this%step, this%time, this%prec
  100 format( "NATOMS:", i9, 2x, "STEP:", i9, 2x, "TIME:", e15.7e3, 2x, "PREC:", e15.7e3  )

  ! Simulation box
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  do i = 1, DIM
    write (unit,200) this%box(:,i)
    200 format( 5x, 3( 1pe13.5e2: "," ) )
  end do ! for i

  ! Coordinate data
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  do i = 1, this%natoms
    write (unit,300) this%coor(:,i)
    300 format( 3( 1pe13.5e2: "," ) )
  end do ! for i

  ! Return success
  xtc_frame_dump = xslibOK

  return
end function xtc_frame_dump

! -------------------------------------------------
! xtc_t class procedures

! Allocate NFRAMES of frames (only allocates frames)
integer function xtc_allocate( this, nframes )
  implicit none
  class(xtc_t)        :: this
  integer, intent(in) :: nframes
  integer             :: stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(nframes), STAT=stat )
  if ( stat /= 0 ) then
    xtc_allocate = xslibNOMEM
    return
  end if

  ! Return success
  xtc_allocate = xslibOK

  return
end function xtc_allocate

! Allocate NFRAMES frames each with NATOMS of atoms
integer function xtc_allocate_all( this, natoms, nframes )
  implicit none
  class(xtc_t)        :: this
  integer, intent(in) :: natoms, nframes
  integer             :: i, stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(this%nframes), STAT=stat )
  if ( stat /= 0 ) then
    xtc_allocate_all = xslibNOMEM
    return
  end if

  ! Allocate each frame
  do i = 1, this%nframes
    xtc_allocate_all = this%frame(i)%allocate( natoms )
    if ( xtc_allocate_all /= xslibOK ) return

  end do

  ! Return success
  xtc_allocate_all = xslibOK

  return
end function xtc_allocate_all

! Assigment(=) for xtc_t class
subroutine xtc_assign( this, other )
  use iso_c_binding
  implicit none
  class(xtc_t), intent(inout) :: this
  type(xtc_t), intent(in)     :: other
  integer                     :: stat

  ! Copy pointer
  if ( associated(other%xd) ) then
    allocate( this%xd, SOURCE=other%xd, STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
  end if

  ! Copy header
  this%allframes = other%allFrames
  this%current   = other%current

  ! Copy frame offsets
  if ( allocated(other%offsets) ) then
    if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )
    allocate( this%offsets(other%allFrames+1), STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%offsets(:) = other%offsets(1:other%allFrames+1) ! +1 for EOF
  end if

  ! Copy data
  this%natoms  = other%natoms
  this%nframes = other%nframes
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%nframes )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine xtc_assign

! Open .XTC file
integer function xtc_open( this, file )
  implicit none
  class(xtc_t)                :: this
  character(*), intent(in)    :: file

  ! Open file for reading
  xtc_open = xtc_open_file( this%xd, file, .true. )
  if ( xtc_open /= xslibOK ) return

  ! Check file and get frame offsets
  xtc_open = xtc_check( this%xd, this%natoms, this%allframes, this%offsets )
  if ( xtc_open /= xslibOK ) return

  ! Set current frame to 1
  this%current = 1

  ! Return success
  xtc_open = xslibOK

  return
end function xtc_open

! Close .XTC file and clean-up
integer function xtc_close( this )
  implicit none
  class(xtc_t)  :: this
  integer       :: stat

  ! Close file
  if ( associated(this%xd) ) then
    xtc_close = xtc_close_file( this%xd )
    if ( xtc_close /= xslibOK ) return
  end if

  ! Reset private variables
  this%allFrames = 0
  this%natoms    = 0
  this%current   = 0

  ! Clean-up offsets table
  if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )

  ! Return success
  xtc_close = xslibOK

  return
end function xtc_close

! Moves UNIT to the specified FRAME.
! * If WHENCE is set to 0, the OFFSET is taken as an absolute value,
! * if set to 1, OFFSET is taken to be relative to the current position,
! * and if set to 2, relative to the end of the file.
integer function xtc_fseek( this, offset, whence )
  implicit none
  class(xtc_t)        :: this
  integer, intent(in) :: offset, whence
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
  integer             :: frame

  ! Check if pointer is associated
  if ( .not. associated(this%xd) ) then
    xtc_fseek = xslibOPEN
    return
  end if

  ! Check if offsets table is allocated
  if ( .not. allocated(this%offsets) ) then
    xtc_fseek = xslibNOMEM
    return
  end if

  ! Calculate which frame to move to
  select case ( whence )
  case ( SEEK_SET )
    frame = offset
  case ( SEEK_CUR )
    frame = this%current+offset
  case ( SEEK_END )
    frame = this%allframes+offset
  case default
    xtc_fseek = xslibNR
    return
  end select

  ! Limit selection to bounds
  frame = max( frame, 1 )
  frame = min( frame, this%allframes+1 ) ! "+1" is EOF

  ! Move to selected frame
  xtc_fseek = xdr_seek( this%xd, this%offsets(frame), SEEK_SET )
  if ( xtc_fseek /= 0 ) return

  ! Update current frame
  this%current = frame

  ! Return success
  xtc_fseek = xslibOK

  return
end function xtc_fseek

! Retrieves the current position within an open file.
integer function xtc_ftell( this )
  implicit none
  class(xtc_t)        :: this
  xtc_ftell = this%current
  return
end function xtc_ftell

! Read next <N> frames in .XTC file
integer function xtc_read_next( this, nframes )
  implicit none
  class(xtc_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: frames, remaining, i

  ! Optional argument
  frames = merge( nframes, 1, present(nframes) )
  if ( frames < 0 ) frames = 1

  ! Calculate remaining frames
  remaining = this%allFrames-this%current+1

  ! Allocate frames
  this%nframes = min( frames, remaining )
  xtc_read_next = this%allocate( this%nframes )
  if ( xtc_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    xtc_read_next = this%frame(i)%read( this%xd )
    if ( xtc_read_next /= xslibOK ) return

    ! Update current frame
    this%current = this%current+1

  end do ! for i

  ! Return success
  xtc_read_next = xslibOK

  return
end function xtc_read_next

! Skip next <N> frames in .XTC file
integer function xtc_skip_next( this, nframes )
  implicit none
  class(xtc_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: np

  ! Move to selected frame aka. "skip frames"
  np = merge( nframes, 1, present(nframes) )
  xtc_skip_next = this%fseek( np, SEEK_CUR )

  return
end function xtc_skip_next

! Read selected frame in .XTC file
integer function xtc_read_frame( this, frame )
  implicit none
  class(xtc_t) :: this
  integer, intent(in) :: frame

  ! Move to selected frame
  if ( this%current /= frame ) then
    xtc_read_frame = this%fseek( frame, SEEK_SET )
    if ( xtc_read_frame /= 0 ) return
  end if

  ! Read frame
  xtc_read_frame = this%read_next()

  return
end function xtc_read_frame

! Read entire .XTC file
integer function xtc_read( this, file, first, last, stride )
  implicit none
  class(xtc_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: nfirst, nlast, nstride
  integer                       :: i, frame

  ! Open file
  xtc_read = this%open( file )
  if ( xtc_read /= xslibOK ) return

  ! Optional parameters
  nfirst  = merge( first, 1, present(first) )
  nlast   = merge( last, -1, present(last) )
  nstride = merge( stride, 1, present(stride) )

  ! Prevet doing stupid things
  if ( nfirst < 0 ) nfirst = 1
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Calculate number of frames and allocate data
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  xtc_read = this%allocate( this%nframes )
  if ( xtc_read /= xslibOK ) return

  ! Read frames
  do i = 1, this%nframes
    ! Actual frame number
    frame = nfirst + (i-1)*nstride

    ! Move to selected frame
    if ( this%current /= frame ) then
      xtc_read = this%fseek( frame, SEEK_SET )
      if ( xtc_read /= xslibOK ) return
    end if

    ! Read i-th frame
    xtc_read = this%frame(i)%read( this%xd )
    if ( xtc_read /= xslibOK ) return

    ! Update current frame
    this%current = this%current+1

  end do ! for i

  ! Close file
  xtc_read = this%close()
  if ( xtc_read /= xslibOK ) return

  ! Return success
  xtc_read = xslibOK

  return
end function xtc_read

! Write data in .XTC format to unit
integer function xtc_write( this, file )
  implicit none
  class(xtc_t)              :: this
  character(*), intent(in)  :: file
  type(xdrfile), pointer    :: xd => null()
  integer                   :: i

  ! Check if object is allocated
  if ( .not. allocated(this%frame) ) then
    xtc_write = xslibNOMEM
    return
  end if

  ! Open file for writing
  xtc_write = xtc_open_file( xd, file, .false. )
  if ( xtc_write /= xslibOK ) return

  ! Write each frame to output
  do i = 1, this%nframes
    xtc_write = this%frame(i)%write( xd )
    if ( xtc_write /= xslibOK ) return

  end do ! for i

  ! Close file
  xtc_write = xtc_close_file( xd )
  if ( xtc_write /= xslibOK ) return

  ! Return success
  xtc_write = xslibOK

  return
end function xtc_write

! Write data in .XTC data to stdout, unit, or file in human-readable format.
integer function xtc_dump( this, file, unit )
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(xtc_t)                        :: this
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: unit
  integer                             :: i, out, stat

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    xtc_dump = xslibNOMEM
    return
  end if

  ! Select output method depending on what arguments are provided.
  if ( present(file) ) then
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      xtc_dump = xslibFILENOTFOUND
      return
    end if
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Write each frame to output
  do i = 1, this%nframes
    xtc_dump = this%frame(i)%dump( out )
    if ( xtc_dump /= xslibOK ) return
  end do ! for i

  ! Close file if present
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      xtc_dump = xslibCLOSE
      return
    end if
  end if

  ! Rerturn success
  xtc_dump = xslibOK

  return
end function xtc_dump

! -------------------------------------------------
! xtc_t class utilities

! Return max. num. of atoms in currently opened file
integer function xtc_getNatoms( this )
  implicit none
  class(xtc_t)  :: this
  xtc_getNatoms = this%natoms
  return
end function xtc_getNatoms

! Return num. of frames in currently opened file
integer function xtc_getNframes( this )
  implicit none
  class(xtc_t)  :: this
  xtc_getNframes = this%allframes
  return
end function xtc_getNframes

! Return largest box in currently opened file
function xtc_getBox( this ) result( box )
  implicit none
  class(xtc_t)  :: this
  real          :: box(DIM,DIM)
  box(:,:) = this%box
  return
end function xtc_getBox

! Return largest box in currently opened file
! function xtc_getBox( this ) result( box )
!   implicit none
!   class(xtc_t)  :: this
!   real          :: box(DIM,DIM), ibox(DIM,DIM)
!   integer       :: i, pos, stat
!
!   ! Initialize result
!   box(:,:) = 0.000
!
!   ! Check pointer association
!   if ( .not. associated(this%xd) ) return
!
!   ! Store starting position
!   pos = this%ftell()
!
!   ! loop over all frames
!   do i = 1, this%allframes
!     ! Move to frame
!     stat = this%fseek( i, SEEK_SET )
!     if ( stat /= xslibOK ) exit
!
!     ! Read box dimmension
!     if ( xdrfile_read_float( ibox, DIM*DIM, this%xd ) /= DIM*DIM ) exit
!
!     ! Keep the larger box
!     box(:,:) = max( box, ibox )
!
!   end do
!
!   ! Move back to starting position
!   stat = this%fseek( pos, SEEK_SET )
!
!   return
! end function xtc_getBox

! ! Skip .xtc data
! integer function xtc_skip( xd, natoms, box )
!   use iso_fortran_env, only: INT64
!   use iso_c_binding
!   implicit none
!   type(xdrfile), intent(in), pointer  :: xd
!   integer, intent(in)                 :: natoms
!   real, intent(out)                   :: box(DIM,DIM)
!   integer                             :: framebytes
!
!   ! Check pointer association
!   if ( .not. associated(xd) ) then
!     xtc_skip = xslibOPEN
!     return
!   end if
!
!   ! If less than 10 atoms then the framesize is known
!   if ( natoms < 10 ) then
!     ! Read box
!     if ( xdrfile_read_float( box, DIM*DIM, xd ) /= size(box) ) then
!       xtc_skip = exdr3DX
!       return
!     end if
!
!     ! Calculate frame size and skip
!     framebytes = 4+XTC_SHORT_BYPESPERATOM*natoms
!     xtc_skip = xdr_seek( xd, int(framebytes,INT64), SEEK_CUR )
!     if ( xtc_skip /= xslibOK ) return
!
!   else
!     ! Read box (36 bytes)
!     if ( xdrfile_read_float( box, size(box), xd ) /= size(box) ) then
!       xtc_skip = exdr3DX
!       return
!     end if
!
!     ! Skip bullshit
!     xtc_skip = xdr_seek( xd, int(36,INT64), SEEK_CUR )
!
!     ! Read framebytes
!     if ( xdrfile_read_int( framebytes, 1, xd ) == 0 ) then
!       xtc_skip = exdrINT
!       return
!     end if
!
!     ! Round to next 32-bit boundry.
!     ! C source:
!     ! > int framebytes = ( framebytes + 3 ) & ~0x03
!     framebytes = int(AND((framebytes+3),NOT(x'03')))
!     xtc_skip = xdr_seek( xd, int(framebytes,INT64), SEEK_CUR )
!     if ( xtc_skip /= xslibOK ) return
!
!
!   end if
!
!   ! Return success
!   xtc_skip = xslibOK
!
!   return
! end function xtc_skip

end module xslib_xtcio
