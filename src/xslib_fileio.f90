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

module xslib_fileio
  use iso_fortran_env, only: INT64
  use xdrfor
  use xslib_groio
  use xslib_pdbio
  use xslib_xyzio
  use xslib_xtcio
  use xslib_trrio
  use xslib_dcdio
  implicit none
  private
  public :: file_t

  ! Import error definitions
  include "fileio.h"

  ! NOTE: Coordinate and box units are dependant on the file type (Default units of "file_t" are [nm])

  ! Global constants
  integer, parameter :: DIM = 3 ! Default dimension
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2 ! Positioning constants

  ! Supported file types
  enum, bind( C )
    enumerator :: NO_TYPE, XYZ_TYPE, GRO_TYPE, PDB_TYPE, DCD_TYPE, XTC_TYPE, TRR_TYPE
  end enum

  type file_frame
    integer           :: natoms        ! Num of. atoms 
    real              :: box(DIM,DIM)  ! Box side
    real, allocatable :: coor(:,:)     ! Coordinates
  contains
    procedure :: allocate => file_frame_allocate
    procedure :: assign => file_frame_assign
    generic   :: assignment(=) => assign
    procedure :: read => file_frame_read
    procedure :: dump => file_frame_dump
  end type file_frame

  type file_t
    type(xdrfile), pointer                :: xdr => null()      ! XDR file pointer
    integer                               :: unit               ! File unit
    integer, private                      :: allframes = 0      ! Num of. all frames in file
    integer, private                      :: natoms = 0         ! Num of. atoms
    integer, private                      :: current = 0        ! ID of current frame
    real, private                         :: box(DIM,DIM) = 0.  ! Box side
    integer, private                      :: type = NO_TYPE     ! Opened file type
    integer(INT64), allocatable, private  :: offsets(:)         ! Frame offset table
    ! Public data --------------------------
    integer                               :: nframes = 0        ! Num. of frames
    type(file_frame), allocatable         :: frame(:)           ! Frame object
  contains
    procedure :: file_allocate, file_allocate_all
    generic   :: allocate => file_allocate, file_allocate_all
    procedure :: assign => file_assign
    generic   :: assignment(=) => assign
    procedure :: open => file_open
    procedure :: close => file_close
    procedure :: fseek => file_fseek
    procedure :: ftell => file_ftell
    procedure :: read_next => file_read_next
    procedure :: skip_next => file_skip_next
    procedure :: read_frame => file_read_frame
    procedure :: read => file_read
    procedure :: dump => file_dump
    procedure :: getNframes => file_getNframes
    procedure :: getNatoms => file_getNatoms
    procedure :: getBox => file_getBox
  end type file_t

contains

! -------------------------------------------------
! file_frame class procedure

! Allocate frame
integer function file_frame_allocate( this, natoms )
  implicit none
  class(file_frame)   :: this   ! .GRO data file
  integer, intent(in) :: natoms ! Number of points to allocate
  integer             :: stat

  ! Set number of atoms
  this%natoms = natoms

  ! Allocate data
  if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
  allocate( this%coor(DIM,this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    file_frame_allocate = xslibNOMEM
    return
  end if

  ! Return success
  file_frame_allocate = xslibOK

  return
end function file_frame_allocate

! Assigment(=) for file_frame class
subroutine file_frame_assign( this, other )
  implicit none
  class(file_frame), intent(inout)  :: this
  type(file_frame), intent(in)      :: other
  integer                           :: stat

  ! Copy box
  this%box(:,:) = other%box

  ! Copy coor data
  this%natoms = other%natoms
  if ( allocated(other%coor) ) then
    stat = this%allocate( other%natoms )
    if ( stat /= 0 ) error stop "Segmentation fault - allocation failure"
    this%coor(:,:) = other%coor

  end if

  return
end subroutine file_frame_assign

! Read frame
integer function file_frame_read( this, unit, xdr, type, natoms )
  implicit none
  class(file_frame)       :: this
  integer, intent(in)     :: unit
  type(xdrfile), pointer, intent(in) :: xdr
  integer, intent(in)     :: type, natoms
  character(128)          :: string
  real                    :: float
  integer                 :: int
  type(trnheader)         :: sh
  logical                 :: opened

  ! Check if file is opened
  inquire ( UNIT=unit, OPENED=opened )
  if ( .not. ( opened .or. associated(xdr) ) ) then
    file_frame_read = xslibOPEN
    return
  end if

  ! Read header, allocate frames, and read data (transform to nm of necessary)
  select case ( type )
  case ( XYZ_TYPE )
    file_frame_read = xyz_read_header( unit, this%natoms, string, this%box )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = this%allocate( this%natoms )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = xyz_read_coor( unit, this%natoms, this%coor )
    if ( file_frame_read /= xslibOK ) return
    ! Transform to default units.
    this%coor(:,:) = this%coor * 0.1 ! to [nm].
    this%box(:,:)  = this%box * 0.1 ! to [nm].

  case ( GRO_TYPE )
    file_frame_read = gro_read_header( unit, this%natoms, string, float )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = this%allocate( this%natoms )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = gro_read_coor( unit, this%natoms, this%box, this%coor )
    if ( file_frame_read /= xslibOK ) return

  case ( PDB_TYPE )
    file_frame_read = pdb_count( unit, this%natoms )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = this%allocate( this%natoms )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = pdb_read_coor( unit, this%natoms, this%box, this%coor )
    if ( file_frame_read /= xslibOK ) return
    ! Transform to default units.
    this%coor(:,:) = this%coor * 0.1 ! to [nm].
    this%box(:,:)  = this%box * 0.1 ! to [nm].

  case ( XTC_TYPE )
    file_frame_read = xtc_header( xdr, this%natoms, int, float )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = this%allocate( this%natoms )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = xtc_data( xdr, this%natoms, this%box, this%coor, float, .true. )
    if ( file_frame_read /= xslibOK ) return

  case( TRR_TYPE )
    file_frame_read = trr_header( xdr, sh, .true. )
    if ( file_frame_read /= xslibOK ) return
    this%natoms = sh%natoms
    file_frame_read = this%allocate( this%natoms )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = trr_coor( xdr, sh, this%box, this%coor )
    if ( file_frame_read /= xslibOK ) return

  case( DCD_TYPE )
    this%natoms = natoms ! NOTE: There is only one header in .DCD file
    file_frame_read = this%allocate( this%natoms )
    if ( file_frame_read /= xslibOK ) return
    file_frame_read = dcd_read_data( unit, this%natoms, this%box, this%coor )
    if ( file_frame_read /= xslibOK ) return
    ! Transform to default units.
    this%coor(:,:) = this%coor * 0.1 ! to [nm].
    this%box(:,:)  = this%box * 0.1 ! to [nm].

  case default
    ! File is not opened or unsupported file type
    file_frame_read = xslibOPEN

  end select

  ! Return success
  file_frame_read = xslibOK

  return
end function file_frame_read

! Write frame in human readable format
integer function file_frame_dump( this, unit )
  implicit none
  class(file_frame)   :: this
  integer, intent(in) :: unit
  integer             :: i
  logical             :: opened
  character(9)        :: action

  ! Check if unit is opened for writing
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    file_frame_dump = xslibOPEN
    return
  end if

  ! Simulation box
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  ! * ...
  do i = 1, DIM
    write (unit,100) this%box(:,i)
    100 format( 5x, *( 1pe13.5e2: "," ) )
  end do ! for i

  ! Natoms
  ! * { natoms=         5 }
  write (unit,200) this%natoms
  200 format( "NATOMS:", i9  )

  ! Coordinates
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  ! * ...
  if ( allocated(this%coor) ) then
    do i = 1, this%natoms
      write (unit,300) this%coor(:,i)
      300 format( *( 1pe13.5e2: "," ) )

    end do ! for i
  end if

  ! Return success
  file_frame_dump = xslibOK

  return
end function file_frame_dump

! -------------------------------------------------
! ...

! Allocate NFRAMES of frames (only allocates frames)
integer function file_allocate( this, nframes )
  implicit none
  class(file_t)       :: this
  integer, intent(in) :: nframes
  integer             :: stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(nframes), STAT=stat )
  if ( stat /= 0 ) then
    file_allocate = xslibNOMEM
    return
  end if

  ! Return success
  file_allocate = xslibOK

  return
end function file_allocate

! Allocate NFRAMES of frames each with NATOMS of atoms
integer function file_allocate_all( this, natoms, nframes )
  implicit none
  class(file_t)       :: this
  integer, intent(in) :: natoms, nframes
  integer             :: i, stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(nframes), STAT=stat )
  if ( stat /= 0 ) then
    file_allocate_all = xslibNOMEM
    return
  end if

  ! Allocate each frame
  do i = 1, this%nframes
    file_allocate_all = this%frame(i)%allocate( natoms )
    if ( file_allocate_all /= xslibOK ) return

  end do

  ! Return success
  file_allocate_all = xslibOK

  return
end function file_allocate_all

! Assigment(=) for xyz_t class
subroutine file_assign( this, other )
  class(file_t), intent(inout)  :: this
  type(file_t), intent(in)      :: other
  integer                       :: stat

  ! Copy private data
  this%unit      = other%unit
  this%xdr      => other%xdr
  this%allFrames = other%allFrames
  this%natoms    = other%natoms
  this%current   = other%current

  ! Copy frame offsets
  if ( allocated(other%offsets) ) then
    if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )
    allocate( this%offsets(other%allFrames+1), STAT=stat ) ! +1 for EOF
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%offsets(:) = other%offsets(:other%allFrames+1)

  end if

  ! Copy all frames
  this%natoms = other%natoms
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%nframes )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine file_assign

! Open file
integer function file_open( this, file )
  implicit none
  class(file_t)             :: this
  character(*), intent(in)  :: file

  ! Extract file extesion and open file
  select case ( toUpper(extension(file)) )
  case ( "XYZ" )
    file_open = xyz_open_file( this%unit, file, .true. )
    if ( file_open /= xslibOK ) return
    file_open = xyz_check_file( this%unit, this%natoms, this%allframes, this%offsets )
    if ( file_open /= xslibOK ) return
    this%type = XYZ_TYPE

  case ( "GRO" )
    file_open = gro_open_file( this%unit, file, .true. )
    if ( file_open /= xslibOK ) return
    file_open = gro_check_file( this%unit, this%natoms, this%allframes, this%offsets )
    if ( file_open /= xslibOK ) return
    this%type = GRO_TYPE

  case ( "PDB" )
    file_open = pdb_open_file( this%unit, file, .true. )
    if ( file_open /= xslibOK ) return
    file_open = pdb_check_file( this%unit, this%natoms, this%allframes, this%offsets )
    if ( file_open /= xslibOK ) return
    this%type = PDB_TYPE

  case ( "DCD" )
    file_open = dcd_open_file( this%unit, file, .true. )
    if ( file_open /= xslibOK ) return
    file_open = dcd_check_file( this%unit, this%natoms, this%allframes, this%offsets )
    if ( file_open /= xslibOK ) return
    this%type = DCD_TYPE

  case ( "XTC" )
    file_open = xtc_open_file( this%xdr, file, .true. )
    if ( file_open /= xslibOK ) return
    file_open = xtc_check( this%xdr, this%natoms, this%allframes, this%offsets )
    if ( file_open /= xslibOK ) return
    this%type = XTC_TYPE

  case ( "TRR" )
    file_open = trr_open_file( this%xdr, file, .true. )
    if ( file_open /= xslibOK ) return
    file_open = trr_check_file( this%xdr, this%natoms, this%allframes, this%offsets )
    if ( file_open /= xslibOK ) return
    this%type = TRR_TYPE

  case default
    ! Unsupported file type
    file_open = xslibNR
    return

  end select

  ! Set current frame to 1
  this%current = 1

  ! Return success
  file_open = xslibOK

  return
end function file_open

! close file and clean up
integer function file_close( this )
  implicit none
  class(file_t) :: this

  ! Close file
  select case ( this%type )
  case ( XYZ_TYPE )
    file_close = xyz_close_file( this%unit )
  case ( GRO_TYPE )
    file_close = gro_close_file( this%unit )
  case ( PDB_TYPE )
    file_close = pdb_close_file( this%unit )
  case ( DCD_TYPE )
    file_close = dcd_close_file( this%unit )
  case ( XTC_TYPE )
    file_close = xtc_close_file( this%xdr )
  case ( TRR_TYPE )
    file_close = trr_close_file( this%xdr )
  end select

  ! Default values
  this%type      = NO_TYPE
  this%allframes = 0
  this%natoms    = 0
  this%current   = 0

  ! Return success
  file_close = xslibOK

  return
end function file_close

! Moves UNIT to the specified FRAME.
! * If WHENCE is set to 0, the OFFSET is taken as an absolute value,
! * if set to 1, OFFSET is taken to be relative to the current position,
! * and if set to 2, relative to the end of the file.
integer function file_fseek( this, offset, whence )
  implicit none
  class(file_t)       :: this
  integer, intent(in) :: offset, whence
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
  logical             :: opened
  integer             :: frame, stat, dummy

  ! Check if unit is opened or xdr pointer is associated.
  inquire ( UNIT=this%unit, OPENED=opened )
  if ( .not. ( opened .or. associated(this%xdr) ) ) then
    file_fseek = xslibOPEN
    return
  end if

  ! Check if offsets table is allocated
  if ( .not. allocated(this%offsets) ) then
    file_fseek = xslibNOMEM
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
    file_fseek = xslibNR
    return
  end select

  ! Limit selection to bounds
  frame = max( frame, 1 )
  frame = min( frame, this%allframes+1 ) ! +1 for EOF

  ! Move to selected frame
  select case ( this%type )
  case ( XYZ_TYPE, GRO_TYPE, PDB_TYPE )
    ! Rewind if start of file
    if ( this%offsets(frame) < 4 ) then
      rewind( this%unit, IOSTAT=stat )
    else
      read( this%unit,*,POS=this%offsets(frame)-4,IOSTAT=stat) ! Dummy read
    end if
    if ( stat /= 0 ) then
      file_fseek = xslibENDOFFILE
      return
    end if

  case ( DCD_TYPE )
    read( this%unit,POS=this%offsets(frame)-4,IOSTAT=stat) dummy
    if ( stat /= 0 ) then
      file_fseek = xslibENDOFFILE
      return
    end if

  case ( XTC_TYPE, TRR_TYPE )
    stat = xdr_seek( this%xdr, this%offsets(frame), SEEK_SET )
    if ( stat /= 0 ) then
      file_fseek = xslibENDOFFILE
      return
    end if

  case default
    file_fseek = xslibNR
    return

  end select

  ! Update current frame
  this%current = frame

  ! Return success
  file_fseek = xslibOK

  return
end function file_fseek

! Retrieves the current position within an open file.
integer function file_ftell( this )
  implicit none
  class(file_t) :: this
  file_ftell = this%current
  return
end function file_ftell

! Read next <N> frames in file
integer function file_read_next( this, nframes )
  implicit none
  class(file_t)                 :: this
  integer, intent(in), optional :: nframes
  integer                       :: frames, remaining, i

  ! Optional argument
  frames = merge( nframes, 1, present(nframes) )
  if ( frames < 0 ) frames = 1

  ! Calculate remaining frames
  remaining = this%allFrames-this%current+1

  ! Allocate frames
  this%nframes = min( frames, remaining )
  file_read_next = this%allocate( this%nframes )
  if ( file_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    file_read_next = this%frame(i)%read( this%unit, this%xdr, this%type, this%natoms )
    if ( file_read_next /= xslibOK ) return

    ! Increment current frame
    this%current = this%current+1

  end do

  ! Return success
  file_read_next = xslibOK

  return
end function file_read_next

! Skip next <N> frames in file
integer function file_skip_next( this, nframes )
  implicit none
  class(file_t)                 :: this
  integer, intent(in), optional :: nframes
  integer                       :: n

  ! Move to selected frame aka. "skip frames"
  n = merge( nframes, 1, present(nframes) )
  file_skip_next = this%fseek( n, SEEK_CUR )

  return
end function file_skip_next

! Read selected frame in file
integer function file_read_frame( this, frame )
  implicit none
  class(file_t)       :: this
  integer, intent(in) :: frame

  ! Move to selected frame
  if ( this%current /= frame ) then
    file_read_frame = this%fseek( frame, SEEK_SET )
    if ( file_read_frame /= 0 ) return
  end if

  ! Read frame
  file_read_frame = this%read_next()

  return
end function file_read_frame

! Read entire file
integer function file_read( this, file, first, last, stride )
  implicit none
  class(file_t)                 :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: nfirst, nlast, nstride
  integer                       :: i, frame

  ! Open file
  file_read = this%open( file )
  if ( file_read /= xslibOK ) return

  ! Check optional arguments
  nfirst  = merge( first, 1, present(first) )
  nlast   = merge( last, -1, present(last) )
  nstride = merge( stride, 1, present(stride) )

  ! Prevet doing stupid things
  if ( nfirst < 0 ) nfirst = 1
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Calculate number of frames and allocate data
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  file_read = this%allocate( this%nframes )
  if ( file_read /= xslibOK ) return

  ! Read frames
  do i = 1, this%nframes
    ! Actual frame number
    frame = nfirst + (i-1)*nstride

    ! Move to selected frame
    if ( this%current /= frame ) then
      file_read = this%fseek( frame, SEEK_SET )
      if ( file_read /= 0 ) return
    end if

    ! Read i-th frame
    file_read = this%frame(i)%read( this%unit, this%xdr, this%type, this%natoms )
    if ( file_read /= xslibOK ) return

    ! Update current frame
    this%current = frame+1

  end do ! for i

  ! Close file
  file_read = this%close()
  if ( file_read /= xslibOK ) return

  ! Return success
  file_read = xslibOK

  return
end function file_read

! Write data in human readable format to unit, file or stdout
integer function file_dump( this, unit, file )
  use iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(file_t)                       :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: i, out, stat

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    file_dump = xslibNOMEM
    return
  end if

  ! Select output destination; Default is stdout.
  if ( present(file) ) then
    open( NEWUNIT=out, FILE=file, ACTION="write", STATUS="unknown", IOSTAT=stat )
    if ( stat /= 0 ) then
      file_dump = xslibOPEN
      return
    end if
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Write each frame to output
  do i = 1, this%nframes
    file_dump = this%frame(i)%dump( out )
    if ( file_dump /= xslibOK ) return

  end do ! for i

  ! Close file if present
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      file_dump = xslibCLOSE
      return
    end if
  end if

  ! Return success
  file_dump = xslibOK

  return
end function file_dump

! -------------------------------------------------
! file_t class utilities

! Return max. num. of atoms in currently opened file
integer function file_getNatoms( this )
  implicit none
  class(file_t)  :: this
  file_getNatoms = this%natoms
  return
end function file_getNatoms

! Return num. of frames in currently opened file
integer function file_getNframes( this )
  implicit none
  class(file_t)  :: this
  file_getNframes = this%allframes
  return
end function file_getNframes

! Return largest box in currently opened file
function file_getBox( this ) result( box )
  implicit none
  class(file_t) :: this
  real          :: box(DIM,DIM)
  box(:,:) = this%box
  return
end function file_getBox

! ! -------------------------------------------------
! ! file_t class procedures
!
! ! Open and check file.
! integer function file_open( this, file )
!   use, intrinsic :: iso_fortran_env, only: INT64
!   use, intrinsic :: iso_c_binding, only: c_f_pointer, C_NULL_CHAR
!   implicit none
!   class(file_t)             :: this
!   character(*), intent(in)  :: file
!   logical                   :: exist
!
!   ! Open file
!   file_open = this%handle%open( file, this%ext )
!   if ( file_open /= xslibOK ) return
!
!
!   ! NOTE: Coordinate and box units are dependant on the file type.
!   ! * Default units of "file_t" are [nm].
!   ! .xyz = [A]
!   ! .gro = [nm]
!   ! .pdb = [A]
!   ! .xtc = [nm]
!   ! .trr = [nm]
!   ! .dcd = [A]
!   ! .cub = [bohr] or [A]
!
!   ! Open file
!   select case( this%ext )
!   case( "xyz" )
!     file_open = xyz_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
!     this%maxbox(:,:) = this%maxbox * 0.1
!
!   case( "gro" )
!     file_open = gro_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
!
!   case( "pdb" )
!     file_open = pdb_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
!     this%maxbox(:,:) = this%maxbox * 0.1
!
!   case( "xtc" )
!     file_open = xtc_open_file( this%xdr, file, this%nframes, this%maxatoms, this%maxbox )
!
!   case( "trr" )
!     file_open = trr_open_file( this%xdr, file, this%nframes, this%maxatoms, this%maxbox )
!
!   case( "dcd" )
!     file_open = dcd_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
!     this%maxbox(:,:) = this%maxbox * 0.1
!
!   case( "cub" )
!     error stop
!
!   case default
!     file_open = xslibNR !?
!
!   end select
!
!   return
! end function file_open
!
! ! Comment
! integer function file_allocate( this, natoms )
!   implicit none
!   class(file_t)       :: this
!   integer, intent(in) :: natoms
!   integer             :: stat
!
!   ! Set natoms
!   this%natoms = natoms
!
!   ! Allocate only if not allocated or different size.
!   if ( .not. allocated(this%coor) .or. size(this%coor,DIM=2) /= natoms ) then
!     if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
!     allocate( this%coor(3,this%natoms), STAT=stat )
!     if ( stat /= 0 ) then
!       file_allocate = xslibNOMEM
!       return
!     end if
!
!   end if
!
!   ! Return sucess
!   file_allocate = xslibOK
!
!   return
! end function file_allocate
!
! ! Open and check file.
! integer function file_open( this, file )
!   use, intrinsic :: iso_fortran_env, only: INT64
!   use, intrinsic :: iso_c_binding, only: c_f_pointer, C_NULL_CHAR
!   implicit none
!   class(file_t)             :: this
!   character(*), intent(in)  :: file
!   logical                   :: exist
!
!   ! Check if file exists
!   inquire( FILE=trim(file), EXIST=exist )
!   if ( .not. exist ) then
!     file_open = xslibFILENOTFOUND
!     return
!   end if
!
!   ! Extract and check file extension
!   this%ext = toLower( extension(file) )
!   if ( .not. any(trim(this%ext)==["xyz","gro","pdb","xtc","trr","dcd","cub"]) ) then
!     file_open = xslibOPEN ! Unsuported file type
!     return
!   end if
!
!   ! NOTE: Coordinate and box units are dependant on the file type.
!   ! * Default units of "file_t" are [nm].
!   ! .xyz = [A]
!   ! .gro = [nm]
!   ! .pdb = [A]
!   ! .xtc = [nm]
!   ! .trr = [nm]
!   ! .dcd = [A]
!   ! .cub = [bohr] or [A]
!
!   ! Open file
!   select case( this%ext )
!   case( "xyz" )
!     file_open = xyz_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
!     this%maxbox(:,:) = this%maxbox * 0.1
!
!   case( "gro" )
!     file_open = gro_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
!
!   case( "pdb" )
!     file_open = pdb_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
!     this%maxbox(:,:) = this%maxbox * 0.1
!
!   case( "xtc" )
!     file_open = xtc_open_file( this%xdr, file, this%nframes, this%maxatoms, this%maxbox )
!
!   case( "trr" )
!     file_open = trr_open_file( this%xdr, file, this%nframes, this%maxatoms, this%maxbox )
!
!   case( "dcd" )
!     file_open = dcd_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
!     this%maxbox(:,:) = this%maxbox * 0.1
!
!   case( "cub" )
!     error stop
!
!   case default
!     file_open = xslibNR !?
!
!   end select
!
!   return
! end function file_open
!
! ! Read one frame from file.
! integer function file_read_next( this )
!   implicit none
!   class(file_t)   :: this
!   type(trnheader) :: sh
!   character(32)   :: string
!   real            :: float
!   integer         :: int
!
!   ! NOTE: Coordinate and box units are dependant on the file type.
!   ! * Default units of "file_t" are [nm].
!   ! .xyz = [A]
!   ! .gro = [nm]
!   ! .pdb = [A]
!   ! .dcd = [A]
!   ! .xtc = [nm]
!   ! .trr = [nm]
!   ! .cub = [bohr] or [A] ! what the hell?
!
!   select case( this%ext )
!   case( "xyz" )
!     file_read_next = xyz_read_header( this%unit, this%natoms, string, this%box )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = this%allocate( this%natoms )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = xyz_read_coor( this%unit, this%natoms, this%coor )
!     if ( file_read_next /= xslibOK ) return
!     ! Transform to default units.
!     this%coor(:,:) = this%coor * 0.1 ! to [nm].
!     this%box(:,:) = this%box * 0.1 ! to [nm].
!
!   case( "gro" )
!     file_read_next = gro_read_header( this%unit, this%natoms, string, float )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = this%allocate( this%natoms )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = gro_read_coor( this%unit, this%natoms, this%box, this%coor )
!     if ( file_read_next /= xslibOK ) return
!
!   case( "pdb" )
!     file_read_next = pdb_count( this%unit, this%natoms )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = this%allocate( this%natoms )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = pdb_read_coor( this%unit, this%natoms, this%box, this%coor )
!     if ( file_read_next /= xslibOK ) return
!     ! Transform to default units.
!     this%coor(:,:) = this%coor * 0.1 ! to [nm].
!     this%box(:,:) = this%box * 0.1 ! to [nm].
!
!   case( "xtc" )
!     file_read_next = xtc_header( this%xdr, this%natoms, int, float )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = this%allocate( this%natoms )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = xtc_data( this%xdr, this%natoms, this%box, this%coor, float, .true. )
!     if ( file_read_next /= xslibOK ) return
!
!   case( "trr" )
!     file_read_next = trr_header( this%xdr, sh, .true. )
!     if ( file_read_next /= xslibOK ) return
!     this%natoms = sh%natoms
!     file_read_next = this%allocate( this%natoms )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = trr_coor( this%xdr, sh, this%box, this%coor )
!     if ( file_read_next /= xslibOK ) return
!
!   case( "dcd" )
!     ! Header was read uppon open.
!     file_read_next = this%allocate( this%maxatoms )
!     if ( file_read_next /= xslibOK ) return
!     file_read_next = dcd_read_data( this%unit, this%natoms, this%box, this%coor )
!     if ( file_read_next /= xslibOK ) return
!     ! Transform to default units.
!     this%coor(:,:) = this%coor * 0.1 ! to [nm].
!     this%box(:,:) = this%box * 0.1 ! to [nm].
!
!   case( "cub" )
!     error stop
!
!   case default
!     file_read_next = xslibOPEN ! No file is opened
!
!   end select
!
!   return
! end function file_read_next
!
! ! Skip one frame from file.
! integer function file_skip_next( this )
!   implicit none
!   class(file_t)   :: this
!   type(trnheader) :: sh
!   character(32)   :: string
!   real            :: float, box(3,3)
!   integer         :: natoms, int
!
!   ! Read header
!   select case( this%ext )
!   case( "xyz" )
!     file_skip_next = xyz_read_header( this%unit, natoms, string, box )
!     if ( file_skip_next /= xslibOK ) return
!     file_skip_next = xyz_skip_data( this%unit, natoms )
!     if ( file_skip_next /= xslibOK ) return
!
!   case( "gro" )
!     file_skip_next = gro_read_header( this%unit, natoms, string, float )
!     if ( file_skip_next /= xslibOK ) return
!     file_skip_next = gro_skip_data( this%unit, natoms, box )
!     if ( file_skip_next /= xslibOK ) return
!
!   case( "pdb" )
!     file_skip_next = pdb_count( this%unit, natoms )
!     if ( file_skip_next /= xslibOK ) return
!     file_skip_next = pdb_skip_data( this%unit, box )
!     if ( file_skip_next /= xslibOK ) return
!
!   case( "xtc" )
!     file_skip_next = xtc_header( this%xdr, natoms, int, float )
!     if ( file_skip_next /= xslibOK ) return
!     file_skip_next = xtc_skip( this%xdr, natoms, box )
!     if ( file_skip_next /= xslibOK ) return
!
!   case( "trr" )
!     file_skip_next = trr_header( this%xdr, sh, .true. )
!     if ( file_skip_next /= xslibOK ) return
!     file_skip_next = trr_skip( this%xdr, sh, box )
!     if ( file_skip_next /= xslibOK ) return
!
!   case( "dcd" )
!     file_skip_next = dcd_skip_data( this%unit, this%maxatoms, box )
!     if ( file_skip_next /= xslibOK ) return
!
!   case( "cub" )
!     error stop
!
!   case default
!     file_skip_next = xslibOPEN ! No file is opened
!
!   end select
!
!   return
! end function file_skip_next
!
! ! Dump contents to unit if present
! integer function file_dump( this, file, unit )
!   use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
!   implicit none
!   class(file_t)                       :: this
!   character(*), intent(in), optional  :: file
!   integer, intent(in), optional       :: unit
!   integer                             :: i, out, stat
!   logical                             :: opened
!   character(9)                        :: action
!
!   ! Select default output unit
!   if ( present(file) ) then
!     ! Check if file can be opened.
!     open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
!     if ( stat /= 0 ) then
!       file_dump = xslibFILENOTFOUND
!       return
!     end if
!
!   else if ( present(unit) ) then
!     ! Check if unit is opened
!     inquire( UNIT=unit, OPENED=opened, ACTION=action )
!     if ( .not. opened .or. index(action,"WRITE") == 0 ) then
!       file_dump = xslibOPEN
!       return
!     end if
!     out = unit
!
!   else
!     ! Output to stdout
!     out = OUTPUT_UNIT
!
!   end if
!
!   ! Dump simulation box
!   ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
!   do i = 1, 3
!     write (out,100) this%box(:,i)
!     100 format( 5x, 3( 1pe13.5e2: "," ) )
!   end do ! for i
!
!   ! Dump coordinate data if allocated
!   ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
!   if ( allocated(this%coor) ) then
!     do i = 1, this%natoms
!       write (out,200) this%coor(:,i)
!       200 format( 3( 1pe13.5e2: "," ) )
!     end do ! for i
!   end if
!
!   ! Close file if present
!   if ( present(file) ) then
!     close( out, IOSTAT=stat )
!     if ( stat /= 0 ) then
!       file_dump = xslibCLOSE
!       return
!     end if
!   end if
!
!   ! Return success
!   file_dump = xslibOK
!
!   return
! end function file_dump
!
! ! Close file.
! integer function file_close( this )
!   implicit none
!   class(file_t) :: this
!
!   select case( this%ext )
!   case( "xyz" )
!     file_close = xyz_close_file( this%unit )
!
!   case( "gro" )
!     file_close = gro_close_file( this%unit )
!
!   case( "pdb" )
!     file_close = pdb_close_file( this%unit )
!
!   case( "xtc" )
!     file_close = xtc_close_file( this%xdr )
!
!   case( "trr" )
!     file_close = trr_close_file( this%xdr )
!
!   case( "dcd" )
!     file_close = dcd_close_file( this%unit )
!
!   case( "cub" )
!     error stop
!
!   case default
!     file_close = xslibCLOSE
!
!   end select
!
!   ! Clean extension
!   this%ext = ""
!
!   return
! end function file_close
!
! ! -------------------------------------------------
!
! ! Returns the number of frames in file.
! integer function file_getAllframes( this )
!   implicit none
!   class(file_t) :: this
!   file_getAllframes = this%nframes
!   return
! end  function file_getAllframes
!
! ! Gets number of atoms in current frame (without changing position in file).
! integer function file_getNatoms( this )
!   implicit none
!   class(file_t) :: this
!   file_getNatoms = this%maxatoms
!   return
! end function file_getNatoms
!
! ! Get box size of current frame (without changing position in file).
! function file_getCubicBox( this )
!   implicit none
!   class(file_t) :: this
!   real          :: file_getCubicBox(3)
!   file_getCubicBox(:) = [this%maxbox(1,1),this%maxbox(2,2),this%maxbox(3,3)]
!   return
! end function file_getCubicBox

! -------------------------------------------------
! Utilities

! Returns string in all upper case.
character(:) function toUpper( string )
  implicit none
  allocatable               :: toUpper
  character(*), intent(in)  :: string
  integer, parameter        :: offset = ichar("a") - ichar("A") ! ASCII offset
  integer                   :: i
  toUpper = trim(string)
  do i = 1, len_trim(toUpper)
    select case ( toUpper(i:i) )
    case ( "a" : "z" )
      toUpper(i:i) = char(ichar(toUpper(i:i))-offset)
    end select
  end do
  return
end function toUpper

! Returns extension of file. Eg. "/path/to/file.txt" = "txt"
character(:) function extension( file )
  implicit none
  allocatable               :: extension
  character(*), intent(in)  :: file
  integer                   :: i

  i = index( file, ".", BACK=.true. )
  if ( i == 0 .or. i == len_trim(file) ) then
    extension = ""
  else
    extension = file(i+1:)
  end if

  return
end function extension

end module xslib_fileio
