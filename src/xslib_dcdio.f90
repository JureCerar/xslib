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
  public :: dcd_t, dcd_frame

  ! Import error definitions
  include "fileio.h"

  type dcd_frame
    integer           :: natoms = 0
    real, allocatable :: coor(:,:)
    real              :: box(3,3) = 0.000
  contains
    procedure :: allocate => dcd_frame_allocate
    procedure :: assign => dcd_frame_assign
    generic   :: assignment(=) => assign
    procedure :: write => dcd_frame_write
    procedure :: read => dcd_frame_read
    procedure :: dump => dcd_frame_dump
  end type dcd_frame

  type dcd_t
    integer, private              :: unit = 0, natoms = 0, allframes = 0, remaining = 0
    integer(INT64), private       :: nframes_pos = 0_INT64
    real, private                 :: box(3,3) = 0.000
    integer                       :: nframes = 0, start_time = 0, every_time = 0, end_time = 0
    real                          :: timestep = 0.000
    character(80), allocatable    :: remarks(:)
    type(dcd_frame), allocatable  :: frame(:)
  contains
    procedure :: dcd_allocate, dcd_allocate_all
    generic   :: allocate => dcd_allocate, dcd_allocate_all
    procedure :: assign => dcd_assign
    generic   :: assignment(=) => assign
    procedure :: open => dcd_open
    procedure :: close => dcd_close
    procedure :: read => dcd_read
    procedure :: read_next => dcd_read_next
    procedure :: skip_next => dcd_skip_next
    procedure :: write => dcd_write
    procedure :: dump => dcd_dump
    procedure :: getAllframes => dcd_getAllframes
    procedure :: getNatoms => dcd_getNatoms
    procedure :: getBox => dcd_getCubicBox
  end type dcd_t

contains

! -------------------------------------------------
! Class independant (low-level) routines

! Comment
integer function dcd_header_read( unit, remarks, nframes, start_time, every_time, end_time, timestep, natoms )
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
  ! use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)                     :: unit
  character(80), allocatable, intent(out) :: remarks(:)
  integer, intent(out)                    :: nframes, natoms, start_time, every_time, end_time
  real, intent(out)                       :: timestep
  ! --------
  integer(INT64)                          :: filesize, framesize
  integer                                 :: i, nremarks, n, dummy, pos, stat
  integer(INT64)                          :: nframes2
  logical                                 :: opened
  character(9)                            :: action

  ! Check inpit unit
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    dcd_header_read = xslibOPEN
    return
  end if

  read (unit,POS=9) nframes, start_time, every_time, end_time
  read (unit,POS=45) timestep

  ! Number of remarks
  read (unit,POS=97) nremarks

  ! Data allocation
  if ( allocated(remarks) ) deallocate( remarks, STAT=stat )
  allocate( remarks(nremarks), STAT=stat )
  if ( stat /= 0 ) then
    dcd_header_read = xslibNOMEM
    return
  end if

  ! Read remars and trim C_NULL_CHAR
  do i = 1, nremarks
      read (unit) remarks(i)
      n = index(remarks(i),C_NULL_CHAR)
      if ( n /= 0 ) remarks(i) = trim(remarks(i)(1:n-1))
  end do

  ! Dummy arguments (?)
  read (unit) dummy, dummy
  if ( dummy /= 4 ) then
    ! This DCD file format is not supported, or the file header is corrupt.
    dcd_header_read = xslibINT
    return
  end if

  ! Number of atoms in each snapshot
  read (unit) natoms, dummy
  if (dummy .ne. 4) then
    ! "This DCD file format is not supported, or the file header is corrupt."
    dcd_header_read = xslibNATOMS
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
    ! NOTE: Technically not an  an error?
    ! TODO: Just let it slide???
    dcd_header_read = xslibNATOMS
    return
  end if

  ! Return success
  dcd_header_read = xslibOK

  return
end function dcd_header_read

! Comment
integer function dcd_coor_read( unit, natoms, box, coor )
  use iso_fortran_env
  ! use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit, natoms
  real, intent(out)         :: coor(3,natoms)
  real, intent(out)         :: box(3,3)
  integer                   :: dummy(6), nbytes
  real(REAL64)              :: ibox(6)
  logical                   :: opened
  character(9)              :: action

  ! Check if unit is assigned and opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    dcd_coor_read = xslibOPEN
    return
  end if

  ! Expected size in bytes
  nbytes = natoms*4

  ! Magic parameter?
  read (unit) dummy(1)
  if ( dummy(1) /= 48 ) then
    ! Problem reading in DCD snapshot.
    dcd_coor_read = xslibINT
    return
  end if

  ! Simulation box size: XX,gamma,YY,beta,alpha,ZZ
  read (unit) ibox(1:6)
  ! if ( ibox(1) < 0. .or. ibox(3) < 0. .or. ibox(6) < 0. ) then
    ! Problem reading in DCD snapshot box dimensions.
    ! NOTE to self: this is a problem, but we eill ignore it ...
  ! end if
  ! Transform box
  box(:,:) = 0.000
  box(1,1) = real(ibox(1))
  box(2,2) = real(ibox(3))
  box(3,3) = real(ibox(6))

  ! Coordinates
  ! 48, then no. of bytes for x coordinates, x coordinates (repeat for y and z coordinates)
  read (unit) dummy(1:2), coor(1,:), dummy(3:4), coor(2,:), dummy(5:6), coor(3,:)
  if ( dummy(1) /= 48 ) then
    dcd_coor_read = xslib3DX
    return
  end if

  if ( any(dummy(2:6) /= nbytes) ) then
    ! Number of bytes in DCD snapshot is incorrect for size of xyz array passed.
    dcd_coor_read = xslib3DX
    return
  end if

  ! Last number is nbytes
  read (unit) dummy(1)
  if ( dummy(1) /= nbytes ) then
    ! Problem reading in DCD snapshot.
    dcd_coor_read = xslib3DX
    return
  end if

  ! Return success
  dcd_coor_read = xslibOK

  return
end function dcd_coor_read

! Comment
integer function dcd_coor_skip( unit, natoms, box )
  use iso_fortran_env
  implicit none
  integer, intent(in) :: unit, natoms
  real, intent(out)   :: box(3,3)
  integer             :: dummy
  real(REAL64)        :: ibox(6)
  integer(INT64)      :: pos, newpos, framesize
  logical             :: opened
  character(9)        :: action

  ! Where are we? Additionally, check if opened and ready for read.
  inquire( UNIT=unit, POS=pos, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    dcd_coor_skip = xslibOPEN
    return
  end if

  ! Magic parameter?
  read (unit) dummy
  if ( dummy /= 48 ) then
    dcd_coor_skip = xslibMAGIC
    return
  end if

  box(:,:) = 0.000

  ! Simulation box size: XX,gamma,YY,beta,alpha,ZZ
  read (unit) ibox(1:6)
  box(1,1) = real(ibox(1))
  box(2,2) = real(ibox(3))
  box(3,3) = real(ibox(6))

  ! Each frame has natoms*3 (4 bytes each) = natoms*12
  ! plus 6 box dimensions (8 bytes each) = 48
  ! Additionally there are 32 bytes of file information in each frame
  framesize = natoms*12 + 80

  ! We subtract 4 bytes so that the next read of the 4-byte integer will line things up properly for the next read
  newpos = pos + framesize - 4
  read (unit,POS=newpos) dummy

  ! Return success
  dcd_coor_skip = xslibOK

  return
end function dcd_coor_skip

! Comment
integer function dcd_header_write( unit, remarks, start_time, every_time, end_time, timestep, natoms, nframes_pos )
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
  implicit none
  integer, intent(in)           :: unit, natoms
  character(80), intent(in)     :: remarks(:)
  real, intent(in)              :: timestep
  integer, intent(in)           :: start_time, every_time, end_time
  integer(INT64), intent(out)   :: nframes_pos
  character(16)                 :: date, time
  character(80)                 :: remarks1, remarks2
  integer                       :: i, nframes

  ! Remarks
  call date_and_time(date=date,time=time)
  remarks1 = "Created by xslib_dcdio"
  remarks2 = "REMARK Created on "//date//" "//time

  ! Terminate wit C_NULL_CHAR
  ! do i = 1, size(remarks)
  !   remarks(i)(80:80) = C_NULL_CHAR
  ! end do

  ! Leave empty header; it will be updated each time frame is written.
  nframes = 0

  ! Magic number and magic string
  write (unit) 84
  write (unit) "CORD"

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

  ! TODO: write ALL remarks
  ! Write custom remakrs
  write (unit) 2
  write (unit) remarks1
  write (unit) remarks2

  write (unit) 164, 4

  ! Number of atoms in each snapshot
  write (unit) natoms
  write (unit) 4

  flush( unit )

  ! return success
  dcd_header_write = xslibOK

  return
end function dcd_header_write

! Comment
integer function dcd_coor_write( unit, nframes_pos, natoms, box, coor )
  use iso_fortran_env
  implicit none
  integer, intent(in)         :: unit, natoms
  integer(INT64), intent(in)  :: nframes_pos
  real, intent(in)            :: coor(3,natoms)
  real, intent(in)            :: box(3,3)
  ! -------
  real(REAL64)                :: ibox(6)
  integer(INT64)              :: current
  integer                     :: coord_size
  integer                     :: nframes, istart, istride, iend

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
  read (unit,POS=nframes_pos) nframes, istart, istride, iend
  write (unit,POS=nframes_pos) nframes+1, istart, istride, iend+istride

  ! Go back to end of stream
  write (unit,POS=current)

  ! Flush stream
  flush( unit )

  ! Return sucess
  dcd_coor_write = xslibOK

  return
end function dcd_coor_write


! -------------------------------------------------
! dcd_coor_read procedures

! Deallocate and allocate XYZ file
integer function dcd_frame_allocate( this, np )
  implicit none
  class(dcd_frame)    :: this
  integer, intent(in) :: np  ! Number of atoms to allocate
  integer             :: stat

  ! Set number of atoms
  this%natoms = np

  ! Allocate atom names
  if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
  allocate( this%coor(3,this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    dcd_frame_allocate = xslibNOMEM
    return
  end if

  ! Return success
  dcd_frame_allocate = xslibOK

  return
end function dcd_frame_allocate

! Comment
subroutine dcd_frame_assign( this, other )
  implicit none
  class(dcd_frame), intent(inout) :: this
  type(dcd_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%box(:,:) = other%box
  this%natoms = other%natoms

  ! Copy data
  if ( allocated(other%coor) ) then
    ! natoms is set by allocation.
    stat = this%allocate( other%natoms )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%coor(:,:) = other%coor
  end if

  return
end subroutine dcd_frame_assign

! Comment
integer function dcd_frame_read( this, unit, natoms )
  implicit none
  class(dcd_frame)    :: this
  integer, intent(in) :: unit, natoms

  ! NOTE: File UNIT check is done by low-level routines.

  ! Allocated data
  dcd_frame_read = this%allocate( natoms )
  if ( dcd_frame_read /= xslibOK ) return

  ! Read data
  dcd_frame_read = dcd_coor_read( unit, this%natoms, this%box, this%coor )

  return
end function dcd_frame_read

! Comment
integer function dcd_frame_write( this, unit, nframes_pos )
  implicit none
  class(dcd_frame)            :: this
  integer, intent(in)         :: unit
  integer(INT64), intent(in)  :: nframes_pos

  ! Check memory
  if ( .not. allocated(this%coor) ) then
    dcd_frame_write = xslibNOMEM
    return
  end if

  ! Write header and coor
  dcd_frame_write = dcd_coor_write( unit, nframes_pos, this%natoms, this%box, this%coor )

  return
end function dcd_frame_write

! Comment
integer function dcd_frame_dump( this, unit )
  implicit none
  class(dcd_frame)    :: this
  integer, intent(in) :: unit
  integer             :: i
  logical             :: opened
  character(9)        :: action

  ! Check if unit is opened
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    dcd_frame_dump = xslibOPEN
    return
  end if

  ! Check if allocated.
  if ( .not. allocated(this%coor) ) then
    dcd_frame_dump = xslibNOMEM
    return
  end if

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
  dcd_frame_dump = xslibOK

  return
end function dcd_frame_dump

! -------------------------------------------------
! dcd_t procedures

! Comment
integer function dcd_allocate( this, nframes )
  implicit none
  class(dcd_t)         :: this
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

! Comment
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

! Comment
subroutine dcd_assign( this, other )
  implicit none
  class(dcd_t), intent(inout)  :: this
  type(dcd_t), intent(in)      :: other
  integer                     :: stat

  ! Copy header data
  this%unit = other%unit
  this%allframes = other%allframes
  this%natoms = other%natoms
  this%nframes_pos = other%nframes_pos
  this%box(:,:) = other%box
  this%start_time = other%start_time
  this%every_time = other%every_time
  this%end_time = other%end_time
  this%timestep = other%timestep

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

! Comment
integer function dcd_open( this, file )
  implicit none
  class(dcd_t)                   :: this
  character(*), intent(in)      :: file
  character(*), parameter       :: magic_string = "CORD"
  integer, parameter            :: magic_number = 84
  integer                       :: i, pos, line1, stat, charmm_version, has_extra_block, four_dimensions, filesize
  character(4)                  :: line2
  real                          :: box(3,3)
  logical                       :: exist

  ! Does file exist? And grab file size.
  inquire( FILE=trim(file), EXIST=exist, SIZE=filesize )
  if ( .not. exist ) then
    dcd_open = xslibFILENOTFOUND
    return
  end if

  ! Open file in native endinness
  open ( NEWUNIT=this%unit, FILE=trim(file), FORM="unformatted", ACCESS="stream", STATUS="old", IOSTAT=stat )
  if ( stat /= 0 ) then
    dcd_open = xslibOPEN
    return
  end if

  ! Read in magic number and magic string
  read (this%unit,POS=1) line1
  read (this%unit) line2

  ! Ensure the magic number and string are correct, if not we'll swap the endinness
  if ( line1 /= magic_number .or. line2 /= magic_string ) then
    ! Try converting to the reverse endianness
    close( this%unit )
    open( NEWUNIT=this%unit, FILE=trim(file), FORM="unformatted", ACCESS="stream", STATUS="old", IOSTAT=stat, CONVERT="swap" )

    ! Retry reading magic number and magic string
    read (this%unit,POS=1) line2
    read (this%unit) line2

    ! We tried both native and reverse endiness and didn't have magic number or string
    if ( line1 /= magic_number .or. line2 /= magic_string ) then
      dcd_open = xslibHEADER
      return
    end if

  end if

  ! Check if the file identifies as CHARMM (LAMMPS pretends to be CHARMM v. 24)
  read (this%unit,POS=85) charmm_version
  if ( charmm_version == 0 ) then
    dcd_open = xslibINT
    return
  end if

  ! We only support files with the extra unitcell block
  read (this%unit,POS=49) has_extra_block
  if ( has_extra_block /= 1 ) then
    dcd_open = xslibINT
    return
  end if

  ! We don't support files with four dimensions
  read (this%unit) four_dimensions
  if ( four_dimensions .eq. 1 ) then
    dcd_open = xslib3DX
    return
  end if

  ! ---------------------------

  ! Read full header
  dcd_open = dcd_header_read( this%unit, this%remarks, this%allframes, this%start_time, &
  &   this%every_time, this%end_time, this%timestep, this%natoms )
  if ( dcd_open /= xslibOK ) return

  ! Current possiton
  inquire( UNIT=this%unit, POS=pos )

  ! Check all frames for largest box
  do i = 1, this%allframes
    dcd_open =  dcd_coor_skip( this%unit, this%natoms, box )
    this%box(:,:) = merge( this%box, box, all(this%box > box) )

  end do

  ! Return to original position
  read (this%unit,POS=pos)

  ! All frames remaining
  this%remaining = this%allframes

  ! Return success
  dcd_open = xslibOK

  return
end function dcd_open

! Comment
integer function dcd_read( this, file, first, last, stride )
  implicit none
  class(dcd_t)                   :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: i, nfirst, nlast, nstride

  ! Open file.
  dcd_read = this%open( file )
  if ( dcd_read /= xslibOK ) return

  ! Optional parameters.
  nfirst = merge( first, 1, present(first) )
  nstride = merge( stride, 1, present(last) )
  nlast = merge( last, this%allframes, present(last) )

  ! Prevet doing stupid things.
  if ( nfirst > this%allframes ) nfirst = this%allframes
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Estimate number of frames and allocate data.
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  dcd_read = this%allocate( this%nframes )
  if ( dcd_read /= xslibOK ) return

  ! Skip all frames before first
  dcd_read = this%skip_next( nfirst-1 )
  if ( dcd_read /= xslibOK ) return

  ! Read frames
  do i = 1, this%nframes
    ! Read frame
    dcd_read = this%frame(i)%read( this%unit, this%natoms )
    if ( dcd_read /= xslibOK ) return

    ! If not last frame, skip until next frame.
    if ( i /= this%nframes ) then
      dcd_read = this%skip_next( nstride-1 )
      if ( dcd_read /= xslibOK ) return

    end if
  end do ! for i

  ! No frames remaining
  this%remaining = 0

  ! Close file
  dcd_read = this%close()
  if ( dcd_read /= xslibOK ) return

  ! Return success
  dcd_read = xslibOK

  return
end function dcd_read

! Comment
integer function dcd_read_next( this, nframes )
  implicit none
  class(dcd_t)                   :: this
  integer, intent(in), optional :: nframes
  integer                       :: i

  ! Allocate frames
  this%nframes = min( merge( nframes, 1, present(nframes) ), this%remaining )
  dcd_read_next = this%allocate( this%nframes )
  if ( dcd_read_next /= xslibOK ) return

  ! Read data
  do i = 1, this%nframes
    ! Read frame
    dcd_read_next = this%frame(i)%read( this%unit, this%natoms )
    if ( dcd_read_next /= xslibOK ) return

    ! One frame less
    this%remaining = this%remaining-1

  end do

  ! Return success
  dcd_read_next = xslibOK

  return
end function dcd_read_next

! Comment
integer function dcd_skip_next( this, nframes )
  implicit none
  class(dcd_t)                   :: this
  integer, intent(in), optional :: nframes
  integer                       :: i
  real                          :: box(3,3)

  ! Skip selected number of frames
  do i = 1, min( merge( nframes, 1, present(nframes) ), this%remaining )
    dcd_skip_next = dcd_coor_skip( this%unit, this%natoms, box )
    if ( dcd_skip_next /= xslibOK ) return

    ! One less frame remaining
    this%remaining = this%remaining

  end do

  ! Return success
  dcd_skip_next = xslibOK

  return
end function dcd_skip_next

! Comment
integer function dcd_write( this, file )
  implicit none
  class(dcd_t)              :: this
  character(*), intent(in) :: file
  integer                  :: i, stat

  ! Open a new file.
  open( NEWUNIT=this%unit, FILE=trim(file), FORM="unformatted", ACCESS="stream", ACTION="readwrite", IOSTAT=stat )
  if ( stat /= 0 ) then
    dcd_write = xslibFILENOTFOUND
    return
  end if

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    dcd_write = xslibNOMEM
    return
  end if

  ! Write header
  ! dcd_header_write( unit, remarks, start_time, every_time, end_time, timestep, natoms, nframes_pos )
  dcd_write = dcd_header_write( this%unit, this%remarks, this%start_time, this%every_time, &
  &   this%end_time, this%timestep, this%natoms, this%nframes_pos )
  if ( dcd_write /= xslibOK ) return

  ! Write each frame to output
  do i = 1, this%nframes
    dcd_write = this%frame(i)%write( this%unit, this%nframes_pos )
    if ( dcd_write /= xslibOK ) return

  end do ! for n

  ! Close unit
  dcd_write = this%close()
  if ( dcd_write /= xslibOK ) return

  ! Return success
  dcd_write = xslibOK

  return
end function dcd_write

integer function dcd_dump( this, file, unit )
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(dcd_t)                        :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: n, out, stat

  ! Select output method depending on what arguments are provided.
  if ( present(file) ) then
    ! Check if file can be opened.
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

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    dcd_dump = xslibNOMEM
    return
  end if

  ! Write header
  write (out,100) this%natoms, this%nframes
  100 format( "NATOMS:", i9, 2x, "NFRAMES:", i9  )

  write (out,200) this%start_time, this%every_time, this%end_time, this%timestep
  200 format( "START:", i9, 2x, "EVERY:", i9, 2x, "END:", i9, 2x, "TIMESTEP:", e15.7e3  )

  ! Write each frame to output
  do n = 1, this%nframes
    ! Frame info
    write (out,300) n
    300 format( 'FRAME:', x, i0 )

    ! Data
    dcd_dump = this%frame(n)%dump( out )
    if ( dcd_dump /= xslibOK ) return

  end do ! for n

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

! Comment
integer function dcd_close( this )
  implicit none
  class(dcd_t) :: this
  integer     :: stat

  close( this%unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    dcd_close = xslibCLOSE
    return
  end if

  ! Return success
  dcd_close = xslibOK

  return
end function dcd_close

! -------------------------------------------------
! Inquiry functions

! Comment
integer function dcd_getNatoms( this )
  implicit none
  class(dcd_t) :: this

  dcd_getNatoms = this%natoms

  return
end function dcd_getNatoms

! Comment
integer function dcd_getAllframes( this )
  implicit none
  class(dcd_t) :: this

  dcd_getAllframes = this%allframes

  return
end function dcd_getAllframes

! Comment
real function dcd_getCubicBox( this )
  implicit none
  class(dcd_t) :: this
  dimension   :: dcd_getCubicBox(3)

  dcd_getCubicBox(:) = [this%box(1,1), this%box(2,2), this%box(3,3)]

  return
end function dcd_getCubicBox

! Comment
real function dcd_getBox( this )
  implicit none
  class(dcd_t) :: this
  dimension   :: dcd_getBox(3,3)

  dcd_getBox(:,:) = this%box

  return
end function dcd_getBox


end module xslib_dcdio
