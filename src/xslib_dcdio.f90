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

module xslib_dcdio
  use, intrinsic :: iso_fortran_env, only: INT64, REAL64
  implicit none
  private
  public :: dcd_t
  ! Class independent procedures (if you are feeling adventurous)
  public :: dcd_open_file, dcd_close_file, dcd_read_header, dcd_read_data, &
  & dcd_skip_data, dcd_write_header, dcd_write_data, dcd_check_file

  ! Import error definitions
  include "fileio.h"

  ! NOTE: .dcd file has only one header

  ! Global constants
  integer, parameter      :: DIM = 3 ! Default dimension
  integer, parameter      :: MAGIC_NUMBER = 84 ! Magic number
  integer, parameter      :: REMARK_LEN = 80 ! Length of remark string
  character(*), parameter :: MAGIC_STRING = "CORD" ! Magic string

  ! Positioning constants
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2

  type dcd_frame
    integer           :: natoms = 0
    real, allocatable :: coor(:,:)
    real              :: box(DIM,DIM) = 0.000
  contains
    procedure :: allocate => dcd_frame_allocate
    procedure :: assign => dcd_frame_assign
    generic   :: assignment(=) => assign
    procedure :: write => dcd_frame_write
    procedure :: read => dcd_frame_read
    procedure :: dump => dcd_frame_dump
  end type dcd_frame

  type dcd_t
    integer, private                      :: unit = 0, natoms = 0, allframes = 0, current = 0
    integer(INT64), private               :: nframes_pos = 0_INT64
    integer(INT64), allocatable, private  :: offsets(:)
    real, private                         :: box(DIM,DIM) = 0.000
    ! Header info
    integer                               :: start_time = 0, every_time = 0, end_time = 0
    real                                  :: timestep = 0.000
    character(REMARK_LEN), allocatable    :: remarks(:)
    ! Public data
    integer                               :: nframes = 0
    type(dcd_frame), allocatable          :: frame(:)
  contains
    procedure :: dcd_allocate, dcd_allocate_all
    generic   :: allocate => dcd_allocate, dcd_allocate_all
    procedure :: assign => dcd_assign
    generic   :: assignment(=) => assign
    procedure :: open => dcd_open
    procedure :: close => dcd_close
    procedure :: fseek => dcd_fseek
    procedure :: ftell => dcd_ftell
    procedure :: read_next => dcd_read_next
    procedure :: skip_next => dcd_skip_next
    procedure :: read_frame => dcd_read_frame
    procedure :: read => dcd_read
    procedure :: write => dcd_write
    procedure :: dump => dcd_dump
    procedure :: getNframes => dcd_getNframes
    procedure :: getNatoms => dcd_getNatoms
    procedure :: getBox => dcd_getBox
  end type dcd_t

contains

! -------------------------------------------------
! Class independent routines

! Open .DCD file (.DCD has some convoluted file check procedure)
integer function dcd_open_file( unit, file, bRead )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(out)      :: unit
  character(*), intent(in)  :: file
  logical, intent(in)       :: bRead ! Open file for reading
  integer                   :: stat
  integer                   :: charmm_version, has_extra_block, four_dimensions
  integer                   :: line1
  character(4)              :: line2
  logical                   :: exist

  if ( bRead ) then
    ! Check if file exists
    inquire( FILE=trim(file), EXIST=exist )
    if ( .not. exist ) then
      dcd_open_file = xslibFILENOTFOUND
      return
    end if

    ! Open file in native endinness
    open( NEWUNIT=unit, FILE=trim(file), FORM="unformatted", ACCESS="stream", STATUS="old", IOSTAT=stat )
    if ( stat /= 0 ) then
      dcd_open_file = xslibOPEN
      return
    end if

    ! Read magic number and magic string
    read (unit,POS=1,IOSTAT=stat) line1
    read (unit,IOSTAT=stat) line2
    if ( stat /= 0 ) then
      dcd_open_file = merge( xslibENDOFFILE, xslibHEADER, stat == IOSTAT_END)
      return
    end if

    ! Ensure the magic number and string are correct, if not we'll swap the endinness
    if ( line1 /= MAGIC_NUMBER .or. line2 /= MAGIC_STRING ) then
      ! Try converting to the reverse endianness
      close( unit, IOSTAT=stat )
      open( NEWUNIT=unit, FILE=trim(file), FORM="unformatted", ACCESS="stream", STATUS="old", IOSTAT=stat, CONVERT="swap" )

      ! Retry reading magic number and magic string
      read (unit,POS=1,IOSTAT=stat) line1
      read (unit,IOSTAT=stat) line2
      if ( stat /= 0 ) then
        dcd_open_file = merge( xslibENDOFFILE, xslibHEADER, stat == IOSTAT_END)
        return
      end if

      ! We tried both native and reverse endiness and failed
      if ( line1 /= MAGIC_NUMBER .or. line2 /= MAGIC_STRING ) then
        dcd_open_file = xslibMAGIC
        return
      end if

    end if

    ! Check if the file identifies as CHARMM (LAMMPS pretends to be CHARMM v24)
    read (unit,POS=85,IOSTAT=stat) charmm_version
    if ( stat /= 0 .or. charmm_version == 0 ) then
      dcd_open_file = xslibINT
      return
    end if

    ! We only support files with the extra unitcell block
    read (unit,POS=49,IOSTAT=stat) has_extra_block
    if ( stat /= 0 .or. has_extra_block /= 1 ) then
      dcd_open_file = xslibINT
      return
    end if

    ! We don't support files with four dimensions
    read (unit,IOSTAT=stat) four_dimensions
    if ( stat /= 0 .or. four_dimensions == 1 ) then
      dcd_open_file = xslib3DX
      return
    end if

  else
    ! Open a new file (action MUST be readwrite.)
    open( NEWUNIT=unit, FILE=trim(file), FORM="unformatted", ACCESS="stream", STATUS="unknown", ACTION="readwrite", IOSTAT=stat )
    if ( stat /= 0 ) then
      dcd_open_file = xslibFILENOTFOUND
      return
    end if

  end if ! bRead

  ! Return success
  dcd_open_file = xslibOK

  return
end function dcd_open_file

! Close .DCD file
integer function dcd_close_file( unit )
  implicit none
  integer, intent(inout)  :: unit
  integer                 :: stat
  logical                 :: opened

  ! Close any previously opened files
  inquire( UNIT=unit, OPENED=opened )
  if ( opened ) then
    close( unit, IOSTAT=stat )
    if ( stat /= 0 ) then
      dcd_close_file = xslibCLOSE
      return
    end if
  end if

  ! Return success
  dcd_close_file = xslibOK

  return
end function dcd_close_file

! Read .DCD header
! NOTE: Remarks are allocated within routine.
integer function dcd_read_header( unit, remarks, nframes, start_time, every_time, end_time, timestep, natoms )
  use iso_fortran_env, only: IOSTAT_END, INT64
  use iso_c_binding, only: C_NULL_CHAR
  implicit none
  integer, intent(in)                             :: unit
  character(REMARK_LEN), allocatable, intent(out) :: remarks(:)
  integer, intent(out)                            :: nframes, natoms, start_time, every_time, end_time
  real, intent(out)                               :: timestep
  integer(INT64)                                  :: filesize, framesize
  integer                                         :: i, nremarks, n, dummy, pos, stat
  integer(INT64)                                  :: nframes2
  logical                                         :: opened
  character(9)                                    :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    dcd_read_header = xslibOPEN
    return
  end if

  ! Read number of frames and simulation times
  read (unit,POS=9,IOSTAT=stat) nframes, start_time, every_time, end_time
  if ( stat /= 0 ) then
    dcd_read_header = merge( xslibENDOFFILE, xslibHEADER, stat == IOSTAT_END )
    return
  end if

  ! Read timestep
  read (unit,POS=45,IOSTAT=stat) timestep
  if ( stat /= 0 ) then
    dcd_read_header = xslibFLOAT
    return
  end if

  ! Number of remarks
  read (unit,POS=97,IOSTAT=stat) nremarks
  if ( stat /= 0 ) then
    dcd_read_header = xslibINT
    return
  end if

  ! Allocate remarks
  if ( allocated(remarks) ) deallocate( remarks, STAT=stat )
  allocate( remarks(nremarks), STAT=stat )
  if ( stat /= 0 ) then
    dcd_read_header = xslibNOMEM
    return
  end if

  ! Read remarks and trim C_NULL_CHAR
  do i = 1, nremarks
    read (unit,IOSTAT=stat) remarks(i)
    if ( stat /= 0 ) then
      dcd_read_header = xslibSTRING
      return
    end if
    n = index(remarks(i),C_NULL_CHAR)
    if ( n /= 0 ) remarks(i) = trim(remarks(i)(1:n-1))
  end do

  ! Dummy arguments (?)
  read (unit,IOSTAT=stat) dummy, dummy
  if ( stat /= 0 .or. dummy /= 4 ) then
    ! This DCD file format is not supported, or the file header is corrupt.
    dcd_read_header = xslibINT
    return
  end if

  ! Number of atoms in each snapshot
  read (unit,IOSTAT=stat) natoms, dummy
  if ( stat /= 0 .or. dummy /= 4 ) then
    ! "This DCD file format is not supported, or the file header is corrupt."
    dcd_read_header = xslibNATOMS
    return
  end if

  ! Current position and file size
  inquire( UNIT=unit, POS=pos, SIZE=filesize )
  pos = pos - 1

  ! Each frame has natoms*3 (4 bytes each) = natoms*12
  ! plus 6 box dimensions (8 bytes each) = 48
  ! Additionally there are 32 bytes of file information in each frame
  framesize = natoms*12 + 80

  ! Header is typically 276 bytes, but inquire gives us exact size
  nframes2 = (filesize-pos)/framesize
  if ( nframes2 /= nframes ) then
    ! Excpected nframes got nframes2
    ! NOTE: Technically not an error? Just let it slide???
    dcd_read_header = xslibNATOMS
    return
  end if

  ! Return success
  dcd_read_header = xslibOK

  return
end function dcd_read_header

! Read (all) .DCD file data
integer function dcd_read_data( unit, natoms, box, coor )
  use iso_fortran_env, only: IOSTAT_END, REAL32, REAL64
  use iso_c_binding, only: c_sizeof
  implicit none
  integer, intent(in) :: unit, natoms
  real, intent(out)   :: coor(DIM,natoms)
  real, intent(out)   :: box(DIM,DIM)
  integer             :: stat, dummy(6), nbytes
  real(REAL64)        :: ibox(6)
  logical             :: opened
  character(9)        :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    dcd_read_data = xslibOPEN
    return
  end if

  ! Expected size in bytes
  nbytes = natoms*4

  ! Read magic parameter (???)
  read (unit,IOSTAT=stat) dummy(1)
  if ( stat /= 0 .or. dummy(1) /= 48 ) then
    ! Problem reading in DCD snapshot.
    dcd_read_data = xslibINT
    return
  end if

  ! Simulation box size: XX,gamma,YY,beta,alpha,ZZ
  read (unit,IOSTAT=stat) ibox(1:6)
  if ( stat /= 0 ) then
    dcd_read_data = xslib3DX
    return
  end if

  ! NOTE to self: This is a problem, but we eill ignore it ...
  if ( ibox(1) < 0. .or. ibox(3) < 0. .or. ibox(6) < 0. ) then
    ! Problem reading in DCD snapshot box dimensions.
    dcd_read_data = xslib3DX
    return
  end if

  ! Transform box
  ! TODO: Implement real transformation (using gamma,beta,alpha)
  box(:,:) = 0.000
  box(1,1) = real(ibox(1))
  box(2,2) = real(ibox(3))
  box(3,3) = real(ibox(6))

  ! Coordinates
  ! Line: < 48, num. of bytes for x coordinates, x coordinates, ... > (repeat for y and z coordinates)
  read (unit,IOSTAT=stat) dummy(1:2), coor(1,:), dummy(3:4), coor(2,:), dummy(5:6), coor(3,:)
  if ( stat /= 0 .or. dummy(1) /= 48 ) then
    dcd_read_data = xslib3DX
    return
  end if

  if ( any(dummy(2:6) /= nbytes) ) then
    ! Number of bytes in DCD snapshot is incorrect for size of xyz array passed.
    dcd_read_data = xslib3DX
    return
  end if

  ! Last number is nbytes
  read (unit,IOSTAT=stat) dummy(1)
  if ( stat /= 0 .or. dummy(1) /= nbytes ) then
    ! Problem reading in DCD snapshot.
    dcd_read_data = xslib3DX
    return
  end if

  ! Return success
  dcd_read_data = xslibOK

  return
end function dcd_read_data

! Skip .DCD file data
integer function dcd_skip_data( unit, natoms, box )
  use iso_fortran_env, only: IOSTAT_END, REAL64, INT64
  implicit none
  integer, intent(in) :: unit, natoms
  real, intent(out)   :: box(DIM,DIM)
  integer             :: stat, dummy
  real(REAL64)        :: ibox(6)
  integer(INT64)      :: pos, newpos, framesize
  logical             :: opened
  character(9)        :: action

  ! Check if file is opened for reading. Also check where in file are we
  inquire( UNIT=unit, POS=pos, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    dcd_skip_data = xslibOPEN
    return
  end if

  ! Magic parameter (???)
  read (unit,IOSTAT=stat) dummy
  if ( stat /= 0 .or. dummy /= 48 ) then
    dcd_skip_data = merge( xslibENDOFFILE, xslibMAGIC, stat == IOSTAT_END )
    return
  end if

  ! Simulation box size: XX,gamma,YY,beta,alpha,ZZ
  ! TODO: Do the actual box transformation
  read (unit,IOSTAT=stat) ibox(1:6)
  if ( stat /= 0 ) then
    dcd_skip_data = xslib3DX
    return
  end if

  ! Transform box
  box(:,:) = 0.000
  box(1,1) = real(ibox(1))
  box(2,2) = real(ibox(3))
  box(3,3) = real(ibox(6))

  ! Each frame has natoms*DIM*(4 bytes each) = natoms*DIM*4
  ! plus 6 box dimensions*(8 bytes each) = 48
  ! Additionally there are 32 bytes of file information in each frame
  framesize = natoms*DIM*4 + 48 + 32

  ! We subtract 4 bytes so that the next read of the 4-byte integer will line things up properly for the next read
  newpos = pos + framesize - 4
  read (unit,POS=newpos,IOSTAT=stat) dummy
  if ( stat /= 0 ) then
    dcd_skip_data = xslib3DX
    return
  end if

  ! Return success
  dcd_skip_data = xslibOK

  return
end function dcd_skip_data

! Write .DCD header
integer function dcd_write_header( unit, remarks, start_time, every_time, end_time, timestep, natoms, nframes_pos )
  use iso_fortran_env, only: INT64
  use iso_c_binding, only: C_NULL_CHAR
  implicit none
  integer, intent(in)                   :: unit, natoms
  character(REMARK_LEN), intent(in)     :: remarks(:)
  real, intent(in)                      :: timestep
  integer, intent(in)                   :: start_time, every_time, end_time
  integer(INT64), intent(out)           :: nframes_pos
  integer                               :: i, nframes
  logical                               :: opened
  character(9)                          :: action

  ! Check if unit is assigned and opened for writing
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READWRITE") == 0 ) then
    dcd_write_header = xslibOPEN
    return
  end if

  ! Leave empty header; it will be updated each time frame is written.
  nframes = 0

  ! Magic number and magic string
  write (unit) MAGIC_NUMBER
  write (unit) MAGIC_STRING

  ! Number of snapshots in file
  inquire( UNIT=unit, POS=nframes_pos)
  write (unit) nframes

  ! Timestep of first, last snapshot, and snapshot stride.
  write (unit) start_time, every_time, end_time

  ! Write dummy arguments
  do i = 1, 5
    write (unit) 0
  end do

  ! Simulation timestep
  write (unit) timestep

  ! Has unit cell
  write (unit) 1
  do i = 1, 8
    write (unit) 0
  end do

  ! Pretend to be CHARMM version 24
  write (unit) 24, 84, 164

  ! Write remakrs and terminate them with C_NULL_CHAR
  write (unit) size(remarks)
  do i = 1, size(remarks)
    write (unit) remarks(i)(1:79)//C_NULL_CHAR
  end do

  write (unit) 164, 4

  ! Number of atoms in each snapshot
  write (unit) natoms
  write (unit) 4

  flush (unit)

  ! return success
  dcd_write_header = xslibOK

  return
end function dcd_write_header

! Write data in .DCD format
integer function dcd_write_data( unit, nframes_pos, natoms, box, coor )
  use iso_fortran_env, only: INT64, REAL64
  implicit none
  integer, intent(in)         :: unit, natoms
  integer(INT64), intent(in)  :: nframes_pos
  real, intent(in)            :: coor(3,natoms)
  real, intent(in)            :: box(3,3)
  real(REAL64)                :: ibox(6)
  integer(INT64)              :: current
  integer                     :: stat, coord_size
  integer                     :: nframes, istart, istride, iend
  logical                     :: opened
  character(9)                :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READWRITE") == 0 ) then
    dcd_write_data = xslibOPEN
    return
  end if

  ! Should be 48 (6 double precision floats)
  write (unit) 48

  ! Simulation box size: XX,gamma,YY,beta,alpha,ZZ
  ibox = [box(1,1),90.,box(2,2),90.,90.,box(3,3)]
  write (unit) ibox(1:6)

  coord_size = natoms*4

  ! X,Y, and Z coordinate data
  write (unit) 48
  write (unit) coord_size, coor(1,:), coord_size
  write (unit) coord_size, coor(2,:), coord_size
  write (unit) coord_size, coor(3,:), coord_size

  ! End of stream position
  inquire( UNIT=unit, POS=current)

  ! Update header (read->update->write)
  read (unit,POS=nframes_pos,IOSTAT=stat) nframes, istart, istride, iend
  if ( stat /= 0 ) then
    dcd_write_data = xslibHEADER
    return
  end if
  write (unit,POS=nframes_pos) nframes+1, istart, istride, iend+istride

  ! Go back to end of stream
  write (unit,POS=current)

  ! Flush stream
  flush( unit )

  ! Return sucess
  dcd_write_data = xslibOK

  return
end function dcd_write_data

! Check .DCD file (check frames and construct offsets table)
integer function dcd_check_file( unit, natoms, nframes, offsets )
  use iso_fortran_env, only: INT64
  implicit none
  integer, intent(in)         :: unit
  integer, intent(out)        :: natoms, nframes
  integer(INT64), allocatable :: offsets(:)
  logical                     :: opened
  real                        :: ibox(DIM,DIM)
  integer                     :: i, dummy, stat
  character(9)                :: action
  integer                     :: start_time, every_time, end_time
  real                        :: timestep
  character(REMARK_LEN), allocatable :: remarks(:)

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READWRITE") == 0 ) then
    dcd_check_file = xslibOPEN
    return
  end if

  ! Read header
  dcd_check_file = dcd_read_header( unit, remarks, nframes, start_time, every_time, end_time, timestep, natoms )
  if ( dcd_check_file /= xslibOK ) return

  ! Allocate frame offsets ("+1" for EOF)
  if ( allocated(offsets) ) deallocate( offsets, STAT=stat )
  allocate( offsets(nframes+1), SOURCE=0_INT64, STAT=stat )
  if ( stat /= 0 ) then
    dcd_check_file = xslibNOMEM
    return
  end if

  ! Loop over all frames
  do i = 1, nframes
    ! Store current position in file
    inquire( UNIT=unit, POS=offsets(i) )

    ! Skip frame
    dcd_check_file = dcd_skip_data( unit, natoms, ibox )
    if ( dcd_check_file == xslibENDOFFILE ) exit
    if ( dcd_check_file /= xslibOK ) return

    ! Keep largest values
    ! box(:,:) = max( ibox, box )

  end do

  ! Store EOF position
  inquire( UNIT=unit, POS=offsets(nframes+1) )

  ! Go back to first frame
  read (unit,POS=offsets(1)-4,IOSTAT=stat) dummy
  if ( stat /= 0 ) then
    dcd_check_file = xslibOPEN
    return
  end if

  ! Return success
  dcd_check_file = xslibOK

  return
end function dcd_check_file

! -------------------------------------------------
! dcd_frame class procedures

! Allocate frame
integer function dcd_frame_allocate( this, natoms )
  implicit none
  class(dcd_frame)    :: this
  integer, intent(in) :: natoms
  integer             :: stat

  ! Set number of atoms
  this%natoms = natoms

  ! Allocate atom names
  if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
  allocate( this%coor(DIM,this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    dcd_frame_allocate = xslibNOMEM
    return
  end if

  ! Return success
  dcd_frame_allocate = xslibOK

  return
end function dcd_frame_allocate

! Assigment(=) for dcd_frame class
subroutine dcd_frame_assign( this, other )
  implicit none
  class(dcd_frame), intent(inout) :: this
  type(dcd_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%box(:,:) = other%box

  ! Copy data
  this%natoms = other%natoms
  if ( allocated(other%coor) ) then
    stat = this%allocate( other%natoms )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%coor(:,:) = other%coor
  end if

  return
end subroutine dcd_frame_assign

! Read .DCD frame
integer function dcd_frame_read( this, unit, natoms )
  implicit none
  class(dcd_frame)    :: this
  integer, intent(in) :: unit, natoms

  ! Allocated data
  dcd_frame_read = this%allocate( natoms )
  if ( dcd_frame_read /= xslibOK ) return

  ! Read data
  dcd_frame_read = dcd_read_data( unit, this%natoms, this%box, this%coor )

  return
end function dcd_frame_read

! Write .DCD frame
integer function dcd_frame_write( this, unit, nframes_pos )
  use, intrinsic :: iso_fortran_env, only: INT64
  implicit none
  class(dcd_frame)            :: this
  integer, intent(in)         :: unit
  integer(INT64), intent(in)  :: nframes_pos

  ! Check if data is allocated
  if ( .not. allocated(this%coor) ) then
    dcd_frame_write = xslibNOMEM
    return
  end if

  ! Write header and coor
  dcd_frame_write = dcd_write_data( unit, nframes_pos, this%natoms, this%box, this%coor )

  return
end function dcd_frame_write

! Write .DCD frame in human readable format
integer function dcd_frame_dump( this, unit )
  implicit none
  class(dcd_frame)    :: this
  integer, intent(in) :: unit
  integer             :: i
  logical             :: opened
  character(9)        :: action

  ! Check if unit is opened for writing
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    dcd_frame_dump = xslibOPEN
    return
  end if

  ! Check if data is allocated.
  if ( .not. allocated(this%coor) ) then
    dcd_frame_dump = xslibNOMEM
    return
  end if

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
  dcd_frame_dump = xslibOK

  return
end function dcd_frame_dump

! -------------------------------------------------
! dcd_t class procedures

! Allocate NFRAMES of frames (only allocates frames)
integer function dcd_allocate( this, nframes )
  implicit none
  class(dcd_t)        :: this
  integer, intent(in) :: nframes
  integer             :: stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(nframes), STAT=stat )
  if ( stat /= 0 ) then
    dcd_allocate = xslibNOMEM
    return
  end if

  ! Return success
  dcd_allocate = xslibOK

  return
end function dcd_allocate

! Allocate NFRAMES of frames each with NATOMS of atoms
integer function dcd_allocate_all( this, natoms, nframes )
  implicit none
  class(dcd_t)         :: this
  integer, intent(in) :: nframes, natoms
  integer             :: i, stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(nframes), STAT=stat )
  if ( stat /= 0 ) then
    dcd_allocate_all = xslibNOMEM
    return
  end if

  ! Allocate each frame
  do i = 1, this%nframes
    dcd_allocate_all = this%frame(i)%allocate( natoms )
    if ( dcd_allocate_all /= xslibOK ) return
  end do

  ! Return success
  dcd_allocate_all = xslibOK

  return
end function dcd_allocate_all

! Assigment(=) for dcd_t class
subroutine dcd_assign( this, other )
  implicit none
  class(dcd_t), intent(inout) :: this
  type(dcd_t), intent(in)     :: other
  integer                     :: stat

  ! Copy header data
  this%unit        = other%unit
  this%allframes   = other%allframes
  this%current     = other%current
  this%natoms      = other%natoms
  this%nframes_pos = other%nframes_pos
  this%start_time  = other%start_time
  this%every_time  = other%every_time
  this%end_time    = other%end_time
  this%timestep    = other%timestep

  ! Copy frame offsets
  if ( allocated(other%offsets) ) then
    if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )
    allocate( this%offsets(other%allFrames+1), STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%offsets(:) = other%offsets(1:other%allFrames+1) ! +1 for EOF
  end if

  ! Copy remarks
  if ( allocated(other%remarks) ) then
    if ( allocated(this%remarks) ) deallocate( this%remarks, STAT=stat )
    allocate( this%remarks(size(other%remarks)), STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%remarks(:) = other%remarks
  end if

  ! Copy frames
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%nframes )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame
  end if

  return
end subroutine dcd_assign

! Open (& check) .DCD file and construct frame offsets table
integer function dcd_open( this, file )
  implicit none
  class(dcd_t)              :: this
  character(*), intent(in)  :: file

  ! Open file for reading
  dcd_open = dcd_open_file( this%unit, file, .true. )
  if ( dcd_open /= xslibOK ) return

  ! Check file and get frame offsets
  dcd_open = dcd_check_file( this%unit, this%natoms, this%allframes, this%offsets )
  if ( dcd_open /= xslibOK ) return


  ! Set current frame to 1
  this%current = 1

  ! Return success
  dcd_open = xslibOK

  return
end function dcd_open

! Close .DCD file and clean-up
integer function dcd_close( this )
  implicit none
  class(dcd_t)  :: this
  integer       :: stat
  logical       :: opened

  ! Close file
  inquire( UNIT=this%unit, OPENED=opened )
  if ( opened ) close( this%unit, IOSTAT=stat )

  ! Reset private variables
  this%allFrames = 0
  this%natoms    = 0
  this%current   = 0

  ! Clean-up offsets table
  if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )

  ! Return success
  dcd_close = xslibOK

  return
end function dcd_close

! Moves UNIT to the specified FRAME.
! * If WHENCE is set to 0, the OFFSET is taken as an absolute value,
! * if set to 1, OFFSET is taken to be relative to the current position,
! * and if set to 2, relative to the end of the file.
integer function dcd_fseek( this, offset, whence )
  implicit none
  class(dcd_t)        :: this
  integer, intent(in) :: offset, whence
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
  logical             :: opened
  integer             :: frame, dummy, stat

  ! Check if unit is opened.
  inquire ( UNIT=this%unit, OPENED=opened )
  if ( .not. opened ) then
    dcd_fseek = xslibOPEN
    return
  end if

  ! Check if offsets table is allocated
  if ( .not. allocated(this%offsets) ) then
    dcd_fseek = xslibNOMEM
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
    dcd_fseek = xslibNR
    return
  end select

  ! Limit selection to bounds
  frame = max( frame, 1 )
  frame = min( frame, this%allframes+1 ) ! "+1" is end of file

  ! Move to frame; NOTE: Fist frame is not at <offset=1> because of header
  read (this%unit,POS=this%offsets(frame)-4,IOSTAT=stat) dummy
  if ( stat /= 0 ) then
    dcd_fseek = xslibENDOFFILE
    return
  end if

  ! Update current frame
  this%current = frame

  ! Return success
  dcd_fseek = xslibOK

  return
end function dcd_fseek

! Retrieves the current position within an open file.
integer function dcd_ftell( this )
  implicit none
  class(dcd_t)  :: this
  dcd_ftell = this%current
  return
end function dcd_ftell

! Read next <N> frames in .DCD file
integer function dcd_read_next( this, nframes )
  implicit none
  class(dcd_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: frames, remaining, i

  ! Optional argument
  frames = merge( nframes, 1, present(nframes) )
  if ( frames < 0 ) frames = 1

  ! Calculate remaining frames
  remaining = this%allframes-this%current+1

  ! Allocate frames
  this%nframes = min( frames, remaining )
  dcd_read_next = this%allocate( this%nframes )
  if ( dcd_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    dcd_read_next = this%frame(i)%read( this%unit, this%natoms )
    if ( dcd_read_next /= xslibOK ) return

    ! Increment current frame
    this%current = this%current+1

  end do ! for i

  ! Return success
  dcd_read_next = xslibOK

  return
end function dcd_read_next

! Skip next <N> frames in .GRO file
integer function dcd_skip_next( this, nframes )
  implicit none
  class(dcd_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: newpos

  ! Move to selected frame aka. "skip frames"
  newpos = merge( nframes, 1, present(nframes) )
  dcd_skip_next = this%fseek( newpos, SEEK_CUR )

  ! ! Read frame
  ! dcd_skip_next = this%read_next()

  return
end function dcd_skip_next

! Read selected frame in .DCD file
integer function dcd_read_frame( this, frame )
  implicit none
  class(dcd_t)        :: this
  integer, intent(in) :: frame

  ! Move to selected frame
  if ( this%current /= frame ) then
    dcd_read_frame = this%fseek( frame, SEEK_SET )
    if ( dcd_read_frame /= 0 ) return
  end if

  ! Read frame
  dcd_read_frame = this%read_next()

  return
end function dcd_read_frame

! Read entire .DCD file
integer function dcd_read( this, file, first, last, stride )
  implicit none
  class(dcd_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: nfirst, nlast, nstride
  integer                       :: i, frame

  ! Open file
  dcd_read = this%open( file )
  if ( dcd_read /= xslibOK ) return

  ! Check optional arguments
  nfirst  = merge( first, 1, present(first) )
  nlast   = merge( last, -1, present(last) )
  nstride = merge( stride, 1, present(stride) )

  ! Prevet doing stupid things
  if ( nfirst < 0 ) nfirst = 1
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Calculate number of frames and allocate data
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  dcd_read = this%allocate( this%nframes )
  if ( dcd_read /= xslibOK ) return

  ! Read frames
  do i = 1, this%nframes
    ! Actual frame number
    frame = nfirst + (i-1)*nstride

    ! Move to selected frame
    if ( this%current /= frame ) then
      dcd_read = this%fseek( frame, SEEK_SET )
      if ( dcd_read /= xslibOK ) return
    end if

    ! Read i-th frame
    dcd_read = this%frame(i)%read( this%unit, this%natoms )
    if ( dcd_read /= xslibOK ) return

    ! Update current frame
    this%current = this%current+1

  end do ! for i

  ! Close file
  dcd_read = this%close()
  if ( dcd_read /= xslibOK ) return

  ! Return success
  dcd_read = xslibOK

  return
end function dcd_read

! Write data in .DCD format to unit
integer function dcd_write( this, file )
  implicit none
  class(dcd_t)              :: this
  character(*), intent(in)  :: file
  integer                   :: unit, i, stat

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    dcd_write = xslibNOMEM
    return
  end if

  ! Open file for writing
  dcd_write = dcd_open_file( unit, file, .false. )
  if ( dcd_write /= xslibOK ) return

  ! Write header
  dcd_write = dcd_write_header( this%unit, this%remarks, this%start_time, this%every_time, &
  &   this%end_time, this%timestep, this%natoms, this%nframes_pos )
  if ( dcd_write /= xslibOK ) return

  ! Write each frame to output
  do i = 1, this%nframes
    dcd_write = this%frame(i)%write( this%unit, this%nframes_pos )
    if ( dcd_write /= xslibOK ) return

  end do ! for i

  ! Close unit
  close( this%unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    dcd_write = xslibCLOSE
    return
  end if

  ! Return success
  dcd_write = xslibOK

  return
end function dcd_write

! Write data in .XTC data to stdout, unit, or file in human-readable format.
integer function dcd_dump( this, file, unit )
  use iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(dcd_t)                        :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: i, out, stat

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    dcd_dump = xslibNOMEM
    return
  end if

  ! Select output method depending on what arguments are provided.
  if ( present(file) ) then
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      dcd_dump = xslibFILENOTFOUND
      return
    end if
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Write header
  write (out,100) this%frame(1)%natoms, this%nframes
  100 format( "NATOMS:", i9, 2x, "NFRAMES:", i9  )

  write (out,200) this%start_time, this%every_time, this%end_time, this%timestep
  200 format( "START:", i9, 2x, "EVERY:", i9, 2x, "END:", i9, 2x, "TIMESTEP:", e15.7e3  )

  ! Write each frame to output
  do i = 1, this%nframes
    ! Frame info
    write (out,300) i
    300 format( 'FRAME:', x, i0 )

    ! Write frame
    dcd_dump = this%frame(i)%dump( out )
    if ( dcd_dump /= xslibOK ) return

  end do ! for i

  ! Close file if present
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      dcd_dump = xslibCLOSE
      return
    end if
  end if

  ! Rerturn success
  dcd_dump = xslibOK

  return
end function dcd_dump

! -------------------------------------------------
! dcd_t class utilities

! Return max. num. of atoms in currently opened file
integer function dcd_getNatoms( this )
  implicit none
  class(dcd_t)  :: this
  dcd_getNatoms = this%natoms
  return
end function dcd_getNatoms

! Return num. of frames in currently opened file
integer function dcd_getNframes( this )
  implicit none
  class(dcd_t)  :: this
  dcd_getNframes = this%allframes
  return
end function dcd_getNframes

! Return largest box in currently opened file
function dcd_getBox( this ) result( box )
  implicit none
  class(dcd_t)  :: this
  real          :: box(DIM,DIM)
  box(:,:) = this%box
  return
end function dcd_getBox


! -------------------------------------------------

! ! Return largest box in currently opened file
! function dcd_getCubicBox( this ) result( box )
!   use iso_fortran_env, only: REAL64
!   implicit none
!   class(dcd_t)  :: this
!   real          :: box(DIM), ibox(DIM)
!   integer       :: i, pos, dummy, stat
!   logical       :: opened
!   character(9)  :: action
!   real(REAL64)  :: buffer(6)
!
!   ! Initialize result
!   box(:) = 0.000
!
!   ! Check if unit is opened for reading
!   inquire( UNIT=this%unit, OPENED=opened, ACTION=action )
!   if ( .not. opened .or. index(action,"READ") == 0 ) return
!
!   ! Store starting position
!   pos = this%ftell()
!
!   ! loop over all frames
!   do i = 1, this%allframes
!     ! Move to i-th frame
!     stat = this%fseek( i, SEEK_SET )
!     if ( stat /= xslibOK ) exit
!
!     ! Read Magic parameter (?)
!     read (this%unit,IOSTAT=stat) dummy
!     if ( stat /= 0 .or. dummy /= 48 ) exit
!
!     ! Simulation box size: XX,gamma,YY,beta,alpha,ZZ
!     read (this%unit,IOSTAT=stat) buffer
!     if ( stat /= 0 ) exit
!
!     ! Transform box
!     ibox(1) = real( buffer(1) )
!     ibox(2) = real( buffer(3) )
!     ibox(3) = real( buffer(6) )
!
!     ! Keep the larger box
!     box(:) = max( box, ibox )
!
!   end do
!
!   ! Move back to starting position
!   stat = this%fseek( pos, SEEK_SET )
!
!   return
! end function dcd_getCubicBox


! -------------------------------------------------

! Open .dcd file
! * .dcd has some convoluted file check procedure

! integer function dcd_open_file( unit, file, nframes, natoms, box )
!   use, intrinsic :: iso_fortran_env, only: IOSTAT_END, INT64
!   implicit none
!   integer, intent(out)      :: unit
!   character(*), intent(in)  :: file
!   integer, intent(out)      :: nframes, natoms
!   real, intent(out)         :: box(3,3)
!   character(*), parameter   :: magic_string = "CORD"
!   integer, parameter        :: magic_number = 84
!   integer                   :: i, stat, line1, charmm_version, has_extra_block, four_dimensions
!   integer(INT64)            :: pos
!   real                      :: ibox(3,3)
!   character(4)              :: line2
!   logical                   :: exist
!
!   ! Does file exist? And grab file size.
!   inquire( FILE=trim(file), EXIST=exist )
!   if ( .not. exist ) then
!     dcd_open_file = xslibFILENOTFOUND
!     return
!   end if
!
!   ! Open file in native endinness
!   open ( NEWUNIT=unit, FILE=trim(file), FORM="unformatted", ACCESS="stream", STATUS="old", IOSTAT=stat )
!   if ( stat /= 0 ) then
!     dcd_open_file = xslibOPEN
!     return
!   end if
!
!   ! Read in magic number and magic string
!   read (unit,POS=1,IOSTAT=stat) line1
!   read (unit,IOSTAT=stat) line2
!   if ( stat /= 0 ) then
!     dcd_open_file = merge( xslibENDOFFILE, xslibHEADER, stat == IOSTAT_END)
!     return
!   end if
!
!   ! Ensure the magic number and string are correct, if not we'll swap the endinness
!   if ( line1 /= magic_number .or. line2 /= magic_string ) then
!     ! Try converting to the reverse endianness
!     close ( unit )
!     open ( NEWUNIT=unit, FILE=trim(file), FORM="unformatted", ACCESS="stream", STATUS="old", IOSTAT=stat, CONVERT="swap" )
!
!     ! Retry reading magic number and magic string
!     read (unit,POS=1,IOSTAT=stat) line1
!     read (unit,IOSTAT=stat) line2
!     if ( stat /= 0 ) then
!       dcd_open_file = merge( xslibENDOFFILE, xslibHEADER, stat == IOSTAT_END)
!       return
!     end if
!
!     ! We tried both native and reverse endiness and didn't have magic number or string
!     if ( line1 /= magic_number .or. line2 /= magic_string ) then
!       dcd_open_file = xslibMAGIC
!       return
!     end if
!
!   end if
!
!   ! Check if the file identifies as CHARMM (LAMMPS pretends to be CHARMM v. 24)
!   read (unit,POS=85,IOSTAT=stat) charmm_version
!   if ( stat /= 0 .or. charmm_version == 0 ) then
!     dcd_open_file = xslibINT
!     return
!   end if
!
!   ! We only support files with the extra unitcell block
!   read (unit,POS=49,IOSTAT=stat) has_extra_block
!   if ( stat /= 0 .or. has_extra_block /= 1 ) then
!     dcd_open_file = xslibINT
!     return
!   end if
!
!   ! We don't support files with four dimensions
!   read (unit,IOSTAT=stat) four_dimensions
!   if ( stat /= 0 .or. four_dimensions == 1 ) then
!     dcd_open_file = xslib3DX
!     return
!   end if
!
!   ! ---------------------------------------------------------
!   ! NOTE: File is now ready for reading header
!
!   ! Read full header
!   dcd_open_file = dcd_skip_header( unit, nframes, natoms )
!   if ( dcd_open_file /= xslibOK ) return
!
!   ! Current possiton
!   inquire ( UNIT=unit, POS=pos )
!
!   ! Check all frames for largest box
!   box(:,:) = 0.000
!   do i = 1, nframes
!     dcd_open_file = dcd_skip_data( unit, natoms, ibox )
!     if ( dcd_open_file /= xslibOK ) return
!     box(:,:) = max( box, ibox )
!
!   end do
!
!   ! Return to original position
!   read (unit,POS=pos)
!
!   ! Return success
!   dcd_open_file = xslibOK
!
!   return
! end function dcd_open_file

! Skip .DCD header (only read essentials)

! integer function dcd_skip_header( unit, nframes, natoms )
!   use, intrinsic :: iso_fortran_env, only: IOSTAT_END, INT64
!   use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
!   implicit none
!   integer, intent(in)   :: unit
!   integer, intent(out)  :: nframes, natoms
!   integer(INT64)        :: filesize, framesize
!   integer               :: i, ni, dummy, pos, stat
!   character(80)         :: remark
!   integer(INT64)        :: nframes2
!   logical               :: opened
!   character(9)          :: action
!
!   ! Check if unit is opened for reading
!   inquire( UNIT=unit, OPENED=opened, ACTION=action )
!   if ( .not. opened .or. index(action,"READ") == 0 ) then
!     dcd_skip_header = xslibOPEN
!     return
!   end if
!
!   ! Read number of frames
!   read (unit,POS=9,IOSTAT=stat) nframes
!   if ( stat /= 0 ) then
!     dcd_skip_header = merge( xslibENDOFFILE, xslibHEADER, stat == IOSTAT_END )
!     return
!   end if
!
!   ! Number of remarks
!   read (unit,POS=97,IOSTAT=stat) ni
!   if ( stat /= 0 ) then
!     dcd_skip_header = xslibINT
!     return
!   end if
!
!   ! Skip remarks
!   do i = 1, ni
!     read (unit,IOSTAT=stat) remark
!     if ( stat /= 0 ) then
!       dcd_skip_header = xslibSTRING
!       return
!     end if
!   end do
!
!   ! Dummy arguments (?)
!   read (unit,IOSTAT=stat) dummy, dummy
!   if ( stat /= 0 .or. dummy /= 4 ) then
!     ! This DCD file format is not supported, or the file header is corrupt.
!     dcd_skip_header = xslibINT
!     return
!   end if
!
!   ! Number of atoms in each snapshot
!   read (unit,IOSTAT=stat) natoms, dummy
!   if ( stat /= 0 .or. dummy /= 4 ) then
!     ! "This DCD file format is not supported, or the file header is corrupt."
!     dcd_skip_header = xslibNATOMS
!     return
!   end if
!
!   ! Current position and file size
!   inquire( UNIT=unit, POS=pos, SIZE=filesize )
!   pos = pos-1
!
!   ! Each frame has natoms*3 (4 bytes each) = natoms*12
!   ! plus 6 box dimensions (8 bytes each) = 48
!   ! Additionally there are 32 bytes of file information in each frame
!   framesize = natoms*12 + 80
!
!   ! Header is typically 276 bytes, but inquire gives us exact size
!   nframes2 = (filesize-pos)/framesize
!   if ( nframes2 /= nframes ) then
!     ! Excpected nframes got nframes2
!     ! NOTE: Technically not an error? Just let it slide???
!     dcd_skip_header = xslibNATOMS
!     return
!   end if
!
!   ! Return success
!   dcd_skip_header = xslibOK
!
!   return
! end function dcd_skip_header

! Skip .DCD data

! integer function dcd_skip_data( unit, natoms, box )
!   use, intrinsic :: iso_fortran_env, only: IOSTAT_END, REAL64, INT64
!   implicit none
!   integer, intent(in) :: unit, natoms
!   real, intent(out)   :: box(3,3)
!   integer             :: stat, dummy
!   real(REAL64)        :: ibox(6)
!   integer(INT64)      :: pos, newpos, framesize
!   logical             :: opened
!   character(9)        :: action
!
!   ! Where are we? Additionally, check if opened and ready for read.
!   inquire( UNIT=unit, POS=pos, OPENED=opened, ACTION=action )
!   if ( .not. opened .or. index(action,"READ") == 0 ) then
!     dcd_skip_data = xslibOPEN
!     return
!   end if
!
!   ! Magic parameter?
!   read (unit,IOSTAT=stat) dummy
!   if ( stat /= 0 .or. dummy /= 48 ) then
!     dcd_skip_data = merge( xslibENDOFFILE, xslibMAGIC, stat == IOSTAT_END )
!     return
!   end if
!
!   ! Simulation box size: XX,gamma,YY,beta,alpha,ZZ
!   read (unit,IOSTAT=stat) ibox(1:6)
!   if ( stat /= 0 ) then
!     dcd_skip_data = xslib3DX
!     return
!   end if
!
!   ! Transform box
!   box(:,:) = 0.000
!   box(1,1) = real(ibox(1))
!   box(2,2) = real(ibox(3))
!   box(3,3) = real(ibox(6))
!
!   ! Each frame has natoms*DIM*(4 bytes each) = natoms*12
!   ! plus 6 box dimensions*(8 bytes each) = 48
!   ! Additionally there are 32 bytes of file information in each frame
!   framesize = natoms*12 + 80
!
!   ! We subtract 4 bytes so that the next read of the 4-byte integer will line things up properly for the next read
!   newpos = pos + framesize - 4
!   read (unit,POS=newpos,IOSTAT=stat) dummy
!   if ( stat /= 0 ) then
!     dcd_skip_data = xslib3DX
!     return
!   end if
!
!   ! Return success
!   dcd_skip_data = xslibOK
!
!   return
! end function dcd_skip_data

end module xslib_dcdio
