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
  use xdrfor
  implicit none
  private
  public :: xtc_t, xtc_frame
  public :: xtc_open_file, xtc_close_file, xtc_header, xtc_data, xtc_skip

  ! Import error definitions
  include "fileio.h"

  ! Magic constant
  integer, parameter :: MAGIC = 1995

  type xtc_frame
    integer           :: natoms = 0, step = 0
    real              :: prec = 0.000, time = 0.000, box(3,3) = 0.000
    real, allocatable :: coor(:,:)
  contains
    procedure :: allocate => xtc_frame_allocate
    procedure :: assign => xtc_frame_assign
    generic   :: assignment(=) => assign
    procedure :: read => xtc_frame_read
    procedure :: write => xtc_frame_write
    procedure :: dump => xtc_frame_dump
  end type xtc_frame

  type xtc_t
    type(xdrfile), pointer, private :: xd => null()
    integer, private                :: natoms = 0, allframes = 0, remaining = 0
    real, private                   :: box(3,3) = 0.000
    integer                         :: nframes = 0
    type(xtc_frame), allocatable    :: frame(:)
  contains
    procedure :: xtc_allocate, xtc_allocate_all
    generic   :: allocate => xtc_allocate, xtc_allocate_all
    procedure :: assign => xtc_assign
    generic   :: assignment(=) => assign
    procedure :: open => xtc_open
    procedure :: close => xtc_close
    procedure :: read => xtc_read
    procedure :: read_next => xtc_read_next
    procedure :: skip_next => xtc_skip_next
    procedure :: write => xtc_write
    procedure :: dump => xtc_dump
    procedure :: getAllframes => xtc_getAllframes
    procedure :: getNatoms => xtc_getNatoms
    procedure :: getBox => xtc_getCubicBox
  end type xtc_t

contains

! -------------------------------------------------
! NOTE: xrdfile read is same as write. He he he.

! Open and check .xtc file.
integer function xtc_open_file( xd, file, nframes, natoms, box )
  use, intrinsic :: iso_fortran_env, only: INT64
  use, intrinsic :: iso_c_binding, only: c_f_pointer, C_NULL_CHAR
  implicit none
  type(xdrfile), pointer, intent(out) :: xd
  character(*), intent(in)            :: file
  integer, intent(out)                :: nframes, natoms
  real, intent(out)                   :: box(3,3)
  real                                :: ibox(3,3), time
  integer                             :: inatoms, step
  logical                             :: exist

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    xtc_open_file = xslibFILENOTFOUND
    return
  end if

  ! Open file
  call c_f_pointer( xdrfile_open( trim(file)//C_NULL_CHAR, "r" ), xd )
  if ( .not. associated(xd) ) then
    xtc_open_file = xslibOPEN
    return
  end if

  ! Count and check all frames
  nframes = 0
  box(:,:) = 0.000
  natoms = 0
  do while( .true. )
    ! Count number of atoms in file
    xtc_open_file = xtc_header( xd, inatoms, step, time )
    if ( xtc_open_file == xslibENDOFFILE ) exit
    if ( xtc_open_file /= xslibOK ) return

    ! Skip the rest of the frame
    xtc_open_file = xtc_skip( xd, inatoms, ibox )
    if ( xtc_open_file /= xslibOK ) return

    ! Update data
    nframes = nframes+1
    box(:,:) = max( box, ibox )
    natoms = max( natoms, inatoms )

  end do ! while

  ! Rewind file
  xtc_open_file = xdr_seek( xd, 0_INT64, 0 )

  return
end function xtc_open_file

! Read/write .xtc header
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

  ! Read magic number
  imagic = MAGIC
  if ( xdrfile_read_int( imagic, 1, xd ) /= 1 ) then
    xtc_header = xslibENDOFFILE
    return
  else if ( imagic /= MAGIC ) then
    xtc_header = xslibMAGIC
    return
  end if

  ! Number of atoms
  if ( xdrfile_read_int( natoms, 1, xd ) /= 1 ) then
    xtc_header = exdrINT
    return
  end if

  ! Current step
  if ( xdrfile_read_int( step, 1, xd ) /= 1 ) then
    xtc_header = exdrINT
    return
  end if

  ! Time
  if ( xdrfile_read_float( time, 1, xd ) /= 1 ) then
    xtc_header = exdrINT
    return
  end if

  ! Return success
  xtc_header = xslibOK

  return
end function xtc_header

! Read/write .xtc data
integer function xtc_data( xd, natoms, box, x, prec, bRead )
  implicit none
  type(xdrfile), intent(in), pointer  :: xd
  logical, intent(in)                 :: bRead
  integer, intent(in)                 :: natoms
  real, intent(inout)                 :: box(3,3)
  real, intent(inout)                 :: x(3,natoms)
  real, intent(inout)                 :: prec

  ! Check pointer association
  if ( .not. associated(xd) ) then
    xtc_data = xslibOPEN
    return
  end if

  ! Box dimmension
  if ( xdrfile_read_float( box, size(box), xd ) /= size(box) ) then
    xtc_data = exdr3DX
    return
  end if

  ! Read coordinates
  if ( bRead ) then
    if ( xdrfile_decompress_coord_float( x, natoms, prec, xd ) /= natoms ) then
      xtc_data = exdr3DX
      return
    end if
  else
    if ( xdrfile_compress_coord_float( x, natoms, prec, xd ) /= natoms ) then
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
  use iso_c_binding
  implicit none
  type(xdrfile), intent(in), pointer  :: xd
  integer, intent(in)                 :: natoms
  real, intent(out)                   :: box(3,3)
  integer                             :: framebytes
  enum, bind(C)
    enumerator :: SEEK_SET, SEEK_CUR, SEEK_END
  end enum
  integer, parameter :: DIM = 3 ! dimension
  integer, parameter :: XTC_SHORTHEADER_SIZE = (20 + DIM*DIM*4) ! magic natoms step time DIM*DIM_box_vecs natoms
  integer, parameter :: XTC_SHORT_BYPESPERATOM = 12 ! Short XTCs store each coordinate as a 32-bit float.
  integer, parameter :: XTC_HEADER_SIZE = (DIM*DIM*4 + DIM*2 + 46) ! Short XTCs store each coordinate as a 32-bit float.

  ! Check pointer association
  if ( .not. associated(xd) ) then
    xtc_skip = xslibOPEN
    return
  end if

  ! If less than 10 atoms. Framesize is known
  if ( natoms < 10 ) then
    ! Read box
    if ( xdrfile_read_float( box, size(box), xd ) /= size(box) ) then
      xtc_skip = exdr3DX
      return
    end if

    ! Calculate frame size and skip
    framebytes = 4+XTC_SHORT_BYPESPERATOM*natoms
    xtc_skip = xdr_seek( xd, int(framebytes,INT64), SEEK_CUR )
    if ( xtc_skip /= xslibOK ) return

  else
    ! Read box (36 bytes)
    if ( xdrfile_read_float( box, size(box), xd ) /= size(box) ) then
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
    ! C source:
    ! > int framebytes = ( framebytes + 3 ) & ~0x03
    framebytes = int(AND((framebytes+3),NOT(x'03')))
    xtc_skip = xdr_seek( xd, int(framebytes,INT64), SEEK_CUR )
    if ( xtc_skip /= xslibOK ) return


  end if

  ! Return success
  xtc_skip = xslibOK

  return
end function xtc_skip

! Close .xtc file handle
integer function xtc_close_file( xd )
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  xtc_close_file = xdrfile_close( xd )
  return
end function xtc_close_file

! -------------------------------------------------

! Comment
integer function xtc_frame_allocate( this, np )
  implicit none
  class(xtc_frame)    :: this  ! .PDB data file
  integer, intent(in) :: np  ! Number of points to allocate
  integer             :: stat
  ! Set number of atoms
  this%natoms = np

  ! Allocate residue number
  if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
  allocate( this%coor(3,this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    xtc_frame_allocate = xslibNOMEM
    return
  end if

  ! Return success
  xtc_frame_allocate = xslibOK

  return
end function xtc_frame_allocate

! Comment
subroutine xtc_frame_assign( this, other )
  implicit none
  class(xtc_frame), intent(inout) :: this
  type(xtc_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%step = other%step
  this%prec = other%prec
  this%time = other%time
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

! Comment
integer function xtc_frame_read( this, xd )
  implicit none
  class(xtc_frame)                    :: this
  type(xdrfile), pointer, intent(in)  :: xd

  ! Read xtc header.
  xtc_frame_read = xtc_header( xd, this%natoms, this%step, this%time )
  if ( xtc_frame_read /= xslibOK ) return

  ! Allocate data frame.
  xtc_frame_read = this%allocate( this%natoms )
  if ( xtc_frame_read /= xslibOK ) return

  ! Read the rest of the frame.
  xtc_frame_read = xtc_data( xd, this%natoms, this%box, this%coor, this%prec, .true. )
  if ( xtc_frame_read /= xslibOK ) return

  return
end function xtc_frame_read

! Comment
integer function xtc_frame_write( this, xd )
  implicit none
  class(xtc_frame)                    :: this  ! .PDB data file
  type(xdrfile), intent(in), pointer  :: xd ! Number of points to allocate

  ! Check if allocated.
  if ( .not. allocated(this%coor) ) then
    xtc_frame_write = xslibNOMEM
    return
  end if

  ! Write xtc header.
  xtc_frame_write = xtc_header( xd, this%natoms, this%step, this%time )
  if ( xtc_frame_write /= xslibOK ) return

  ! Write data.
  xtc_frame_write = xtc_data( xd, this%natoms, this%box, this%coor, this%prec, .false. )
  if ( xtc_frame_write /= xslibOK ) return

  return
end function xtc_frame_write

! Write in human readable format.
integer function xtc_frame_dump( this, unit )
  implicit none
  class(xtc_frame)    :: this
  integer, intent(in) :: unit
  integer             :: i
  logical             :: opened
  character(9)        :: action

  ! Check if unit is opened
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    xtc_frame_dump = xslibOPEN
    return
  end if

  ! Check if allocated.
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
  do i = 1, 3
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

! Comment
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

! Comment
integer function xtc_allocate_all( this, natoms, nframes )
  implicit none
  class(xtc_t)        :: this
  integer, intent(in) :: natoms, nframes
  integer             :: i, stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(nframes), STAT=stat )
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

! Comment
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
  this%natoms = other%natoms
  this%allframes = other%allframes
  this%remaining = other%remaining
  this%box(:,:) = other%box

  ! Copy data
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%nframes )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine xtc_assign

! Open .xtc file
integer function xtc_open( this, file )
  implicit none
  class(xtc_t)              :: this
  character(*), intent(in)  :: file

  ! Open file
  xtc_open = xtc_open_file( this%xd, file, this%allframes, this%natoms, this%box )
  if ( xtc_open /= 0 ) return

  ! All frames are remaining
  this%remaining = this%allframes

  return
end function xtc_open

! Wrapper for xtc read.
integer function xtc_read( this, file, first, last, stride )
  implicit none
  class(xtc_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: i, nfirst, nlast, nstride

  ! Open file.
  xtc_read = this%open( file )
  if ( xtc_read /= xslibOK ) return

  ! Optional parameters.
  nfirst = merge( first, 1, present(first) )
  nstride = merge( stride, 1, present(last) )
  nlast = merge( last, this%allframes, present(last) )

  ! Prevet doing stupid things.
  if ( nfirst > this%allframes ) nfirst = this%allframes
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Estimate number of frames an allocate data.
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  xtc_read = this%allocate( this%nframes )
  if ( xtc_read /= xslibOK ) return

  ! Skip all frames before first
  xtc_read = this%skip_next( nfirst-1 )
  if ( xtc_read /= xslibOK ) return

  ! ----------------
  ! Read frames
  do i = 1, this%nframes
    ! Read frame
    xtc_read = this%frame(i)%read( this%xd )
    if ( xtc_read /= xslibOK ) return

    ! If not last frame, skip until next frame.
    if ( i /= this%nframes ) then
      xtc_read = this%skip_next( nstride-1 )
      if ( xtc_read /= xslibOK ) return

    end if
  end do ! for i

  ! No frames remain
  this%remaining = 0

  ! Close file
  xtc_read = this%close()
  if ( xtc_read /= xslibOK ) return

  ! Return success
  xtc_read = xslibOK

  return
end function xtc_read

! Comment
integer function xtc_read_next( this, nframes )
  implicit none
  class(xtc_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: i

  ! Allocate frames
  this%nframes = min( merge( nframes, 1, present(nframes) ), this%remaining )
  xtc_read_next = this%allocate( this%nframes )
  if ( xtc_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    xtc_read_next = this%frame(i)%read( this%xd )
    if ( xtc_read_next /= xslibOK ) return

    ! One less frame remaining
    this%remaining = this%remaining-1

  end do ! for n

  ! Return success
  xtc_read_next = xslibOK

  return
end function xtc_read_next

! Comment
integer function xtc_skip_next( this, nframes )
  implicit none
  class(xtc_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: i, natoms, step
  real                          :: time, box(3,3)

  ! Skip frame one by one
  do i = 1, min( merge( nframes, 1, present(nframes) ), this%remaining )
    ! Read header
    xtc_skip_next = xtc_header( this%xd, natoms, step, time )
    if ( xtc_skip_next /= xslibOK ) return

    ! Skip data
    xtc_skip_next = xtc_skip( this%xd, natoms, box )
    if ( xtc_skip_next /= xslibOK ) return

    ! One less frame remaining
    this%remaining = this%remaining-1

  end do ! for i

  ! Return success
  xtc_skip_next = xslibOK

  return
end function xtc_skip_next

! Comment
integer function xtc_write( this, file )
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR, c_f_pointer
  implicit none
  class(xtc_t)              :: this
  character(*), intent(in)  :: file
  type(xdrfile), pointer    :: xd => null()
  integer                   :: n

  ! Transform to Fortran pointer
  call c_f_pointer( xdrfile_open( trim(file)//C_NULL_CHAR, "w" ), xd )

  ! Check if opened
  if ( .not. associated(xd) ) then
    xtc_write = xslibOPEN
    return
  end if

  ! Check if object is allocated
  if ( .not. allocated(this%frame) ) then
    xtc_write = xslibNOMEM
    return
  end if

  do n = 1, this%nframes
    xtc_write = this%frame(n)%write( xd )
    if ( xtc_write /= xslibOK ) return

  end do ! for n

  ! Close file
  xtc_write = xdrfile_close( xd )
  if ( xtc_write /= xslibOK ) return

  ! Return success
  xtc_write = xslibOK

  return
end function xtc_write

! Write xtc file to stdout, unit, or file in human-readable format.
integer function xtc_dump( this, file, unit )
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(xtc_t)                        :: this
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: unit
  integer                             :: n, out, stat

  ! Select output method depending on what arguments are provided.
  if ( present(file) ) then
    ! Check if file can be opened.
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      xtc_dump = xslibFILENOTFOUND
      return
    end if

  else if ( present(unit) ) then
    out = unit

  else
    ! Output to stdout
    out = OUTPUT_UNIT

  end if

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    xtc_dump = xslibNOMEM
    return
  end if

  ! Write each frame to output
  do n = 1, this%nframes
    xtc_dump = this%frame(n)%dump( out )
    if ( xtc_dump /= xslibOK ) return

  end do ! for n

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

! Close xtc file.
integer function xtc_close( this )
  implicit none
  class(xtc_t)  :: this
  xtc_close = xtc_close_file( this%xd )
  return
end function xtc_close

! -------------------------------------------------

integer function xtc_getNatoms( this )
  implicit none
  class(xtc_t)  :: this

  xtc_getNatoms = this%natoms

  return
end function xtc_getNatoms

integer function xtc_getAllframes( this )
  implicit none
  class(xtc_t)  :: this

  xtc_getAllframes = this%allframes

  return
end function xtc_getAllframes

real function xtc_getCubicBox( this )
  implicit none
  class(xtc_t)  :: this
  dimension     :: xtc_getCubicBox(3)

  xtc_getCubicBox(:) = [this%box(1,1), this%box(2,2), this%box(3,3)]

  return
end function xtc_getCubicBox

real function xtc_getBox( this )
  implicit none
  class(xtc_t)  :: this
  dimension     :: xtc_getBox(3,3)

  xtc_getBox(:,:) = this%box

  return
end function xtc_getBox

end module xslib_xtcio
