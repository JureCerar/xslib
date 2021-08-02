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

module xslib_groio
  use iso_fortran_env, only: INT64
  implicit none
  private
  public :: gro_t
  ! Class independent procedures (if you are feeling adventurous)
  public :: gro_open_file, gro_close_file, gro_read_header, gro_read_data, &
  & gro_read_coor, gro_skip_data, gro_write_data, gro_check_file

  ! Import error definitions
  include "fileio.h"

  ! Global constants
  integer, parameter      :: DIM = 3 ! Default dimension
  integer, parameter      :: GRO_LEN = 6 ! Length of atomnm and resnm
  integer, parameter      :: GRO_MAX_LEN = 128 ! Max buffer and title length
  integer, parameter      :: GRO_MAX_NUM = 100*1000 ! Max. values of resn and atomn
  character(*), parameter :: GRO_TIME_KEYWORD = "t=" ! Keyword for time in title
  integer, parameter      :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2 ! Positioning constants

  type gro_frame
    character(GRO_MAX_LEN)          :: title = ""            ! Title card
    real                            :: time = 0.000          ! Simulation time
    integer                         :: natoms = 0            ! Number of atoms
    integer, allocatable            :: atomn(:), resn(:)     ! Atom and residue indeces
    character(GRO_LEN), allocatable :: atomnm(:), resnm(:)   ! Atom and residue names
    real, allocatable               :: coor(:,:), vel(:,:)   ! Coordinates and velocities
    real                            :: box(DIM,DIM) = 0.000  ! Coor box size
  contains
    procedure :: allocate => gro_frame_allocate
    procedure :: assign => gro_frame_assign
    generic   :: assignment(=) => assign
    ! procedure :: copy => gro_frame_copy
    procedure :: read => gro_frame_read
    procedure :: write => gro_frame_write
  end type gro_frame

  type gro_t
    integer, private                      :: unit = 0          ! File unit
    integer, private                      :: allFrames = 0     ! Num. of frames in currently opened file
    integer, private                      :: natoms = 0        ! Num. of atoms (per frame) in currently opened file
    integer, private                      :: current = 0       ! Current frame in opened file
    real, private                         :: box(DIM,DIM) = 0. ! Simulation box
    integer(INT64), allocatable, private  :: offsets(:)        ! Frames offset table
    ! Public data --------------------------
    integer                               :: nframes = 0       ! Num. of frames
    type(gro_frame), allocatable          :: frame(:)          ! Frame object
  contains
    procedure :: gro_allocate, gro_allocate_all
    generic   :: allocate => gro_allocate, gro_allocate_all
    procedure :: gro_assign
    generic   :: assignment(=) => gro_assign
    procedure :: open => gro_open
    procedure :: close => gro_close
    procedure :: fseek => gro_fseek
    procedure :: ftell => gro_ftell
    procedure :: read_next => gro_read_next
    procedure :: skip_next => gro_skip_next
    procedure :: read_frame => gro_read_frame
    procedure :: read => gro_read
    procedure :: write => gro_write
    procedure :: getNframes => gro_getNframes
    procedure :: getNatoms => gro_getNatoms
    procedure :: getBox => gro_getBox
  end type gro_t

contains

! -------------------------------------------------
! Class independent procedures

! Open .GRO file
integer function gro_open_file( unit, file, bRead )
  implicit none
  integer, intent(out)      :: unit
  character(*), intent(in)  :: file
  logical, intent(in)       :: bRead ! Open in read mode
  integer                   :: stat
  logical                   :: exist

  ! Open file in read or write mode
  if ( bRead ) then
    ! For read mode file must already exist
    inquire( FILE=trim(file), EXIST=exist )
    if ( .not. exist ) then
      gro_open_file = xslibFILENOTFOUND
      return
    end if

    ! Open file for reading
    open ( NEWUNIT=unit, FILE=trim(file), ACCESS="stream", FORM="formatted", STATUS="old", ACTION="read", IOSTAT=stat )
    if ( stat /= 0 ) then
      gro_open_file = xslibOPEN
      return
    end if

  else
    ! Open file for writing
    open ( NEWUNIT=unit, FILE=trim(file), ACCESS="stream", FORM="formatted", STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      gro_open_file = xslibOPEN
      return
    end if

  end if

  ! Return success
  gro_open_file = xslibOK

  return
end function gro_open_file

! Close .GRO file
integer function gro_close_file( unit )
  implicit none
  integer, intent(inout)  :: unit
  integer                 :: stat
  logical                 :: opened

  ! Close any previously opened files
  inquire( UNIT=unit, OPENED=opened )
  if ( opened ) then
    close( unit, IOSTAT=stat )
    if ( stat /= 0 ) then
      gro_close_file = xslibCLOSE
      return
    end if
  end if

  ! Return success
  gro_close_file = xslibOK

  return
end function gro_close_file

! Read .GRO file header
integer function gro_read_header( unit, natoms, title, time )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit
  integer, intent(out)      :: natoms
  character(*), intent(out) :: title
  real, intent(out)         :: time
  integer                   :: i, stat
  logical                   :: opened
  character(9)              :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    gro_read_header = xslibOPEN
    return
  end if

  ! Read title
  read (unit,"(a)",IOSTAT=stat) title
  if ( stat /= 0 ) then
    gro_read_header = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  ! Try to extract time from title
  time = 0.000
  i = index(title,GRO_TIME_KEYWORD)
  if ( i /= 0 ) read (title(i+len(GRO_TIME_KEYWORD):),*,IOSTAT=stat) time

  ! Read number of atoms
  read (unit,*,IOSTAT=stat) natoms
  if ( stat /= 0 ) then
    gro_read_header = merge( xslibENDOFFILE, xslibINT, stat == IOSTAT_END )
    return
  end if

  ! Return success
  gro_read_header = xslibOK

  return
end function gro_read_header

! Read (all) .GRO file data
integer function gro_read_data( unit, natoms, resn, resnm, atomn, atomnm, box, coor, vel )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)             :: unit, natoms
  real, intent(out)               :: coor(DIM,natoms), vel(DIM,natoms), box(DIM,DIM)
  character(GRO_LEN), intent(out) :: resnm(natoms), atomnm(natoms)
  integer, intent(out)            :: resn(natoms), atomn(natoms)
  character(GRO_MAX_LEN)          :: buffer
  integer                         :: i, stat
  logical                         :: opened
  character(9)                    :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    gro_read_data = xslibOPEN
    return
  end if

  ! Initialize velocities as they are optional in format.
  vel(:,:) = 0.000

  ! Read data
  do i = 1, natoms
    read (unit,100,IOSTAT=stat) resn(i), resnm(i), atomnm(i), coor(:,i), vel(:,i)
    100 format( i5, 2a5, 5x, 3f8.3: 3f8.4 )
    if ( stat /= 0 ) then
      gro_read_data = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
      return
    end if

    ! NOTE: Do not read atom numbers simply renumber all atoms
    atomn(i) = i

  end do

  ! Store box side to buffer
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    gro_read_data = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  ! Try to reading triclinic box dimensions from buffer
  box(:,:) = 0.000
  read (buffer,*,IOSTAT=stat) box(:,:)
  if ( stat /= 0 ) then
    ! Ok, no problem, try reading cubic box dimensions from buffer
    box(:,:) = 0.000
    read (buffer,*,IOSTAT=stat) (box(i,i), i=1,DIM)
    if ( stat /= 0 ) then
      ! Ok, now we do have a problem
      gro_read_data = xslib3DX
      return
    end if
  end if

  ! Return success
  gro_read_data = xslibOK

  return
end function gro_read_data

! Read only .GRO file coordinate data
integer function gro_read_coor( unit, natoms, box, coor )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)     :: unit, natoms
  real, intent(out)       :: box(DIM,DIM), coor(DIM,natoms)
  character(GRO_MAX_LEN)  :: buffer
  integer                 :: i, stat
  logical                 :: opened
  character(9)            :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .and. index(action,"READ") == 0 ) then
    gro_read_coor = xslibOPEN
    return
  end if

  ! Read only coor
  do i = 1, natoms
    read (unit,100,IOSTAT=stat) coor(:,i)
    100 format( 20x, 3f8.3 )
    if ( stat /= 0 ) then
      gro_read_coor = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
      return
    end if
  end do ! for i

  ! Store box side to buffer
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    gro_read_coor = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  ! Try to reading triclinic box dimensions from buffer
  box(:,:) = 0.000
  read (buffer,*,IOSTAT=stat) box(:,:)
  if ( stat /= 0 ) then
    ! Ok, no problem, try reading cubic box dimensions from buffer
    box(:,:) = 0.000
    read (buffer,*,IOSTAT=stat) (box(i,i), i=1,DIM)
    if ( stat /= 0 ) then
      ! Ok, now we do have a problem
      gro_read_coor = xslib3DX
      return
    end if
  end if

  ! Return success
  gro_read_coor = xslibOK

  return
end function gro_read_coor

! Skip .GRO file data
integer function gro_skip_data( unit, natoms, box  )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)     :: unit, natoms
  real, intent(out)       :: box(DIM,DIM)
  integer                 :: i, stat
  logical                 :: opened
  character(9)            :: action
  character(GRO_MAX_LEN)  :: buffer

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    gro_skip_data = xslibOPEN
    return
  end if

  ! Skip data
  do i = 1, natoms
    read (unit,*,IOSTAT=stat) ! Dummy read
    if ( stat /= 0 ) then
      gro_skip_data = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
      return
    end if

  end do

  ! Store box side to buffer
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    gro_skip_data = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  ! Try to reading triclinic box dimensions from buffer
  box(:,:) = 0.000
  read (buffer,*,IOSTAT=stat) box(:,:)
  if ( stat /= 0 ) then
    ! Ok, no problem, try reading cubic box dimensions from buffer
    box(:,:) = 0.000
    read (buffer,*,IOSTAT=stat) (box(i,i), i=1,DIM)
    if ( stat /= 0 ) then
      ! Ok, now we do have a problem
      gro_skip_data = xslib3DX
      return
    end if
  end if

  ! Return success
  gro_skip_data = xslibOK

  return
end function gro_skip_data

! Write data in .GRO format (header included)
integer function gro_write_data( unit, natoms, title, time, resn, resnm, atomn, atomnm, box, coor, vel )
  implicit none
  integer, intent(in)             :: unit, natoms
  character(*), intent(in)        :: title
  real, intent(in)                :: time
  real, intent(in)                :: coor(DIM,natoms), vel(DIM,natoms), box(DIM,DIM)
  character(GRO_LEN), intent(in)  :: resnm(natoms), atomnm(natoms)
  integer, intent(in)             :: resn(natoms), atomn(natoms)
  integer                         :: i
  logical                         :: opened
  character(9)                    :: action

  ! Check if unit is opened for writing
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .and. index(action,"WRITE") == 0 ) then
    gro_write_data = xslibOPEN
    return
  end if

  ! Append time to title
  if ( index(title,GRO_TIME_KEYWORD) == 0 ) then
    write (unit,"(2(a,x),f10.5)") trim(title), GRO_TIME_KEYWORD, time
  else
    write (unit,"(a)") trim(title)
  end if

  ! Write number of atoms
  write (unit,"(2x,i0)") natoms

  ! Write all atoms and names
  if ( any(vel(:,:) /= 0.000) ) then
    do i = 1, natoms
      write (unit,100) mod(resn(i),GRO_MAX_NUM), adjustl(resnm(i)), adjustr(trim(atomnm(i))), &
      & mod(atomn(i),GRO_MAX_NUM), coor(:,i), vel(:,i)

    end do ! for i
  else
    do i = 1, natoms
      write (unit,100) mod(resn(i),GRO_MAX_NUM), adjustl(resnm(i)), adjustr(trim(atomnm(i))), mod(atomn(i),GRO_MAX_NUM), coor(:,i)

    end do ! for i
  end if

  100 format( i5, 2a5, i5, 3f8.3: 3f8.4 )

  ! Write box-side
  if ( all(box(:,:)/=0.0) ) then
    write (unit,200) box(:,:)
  else
    write (unit,200) (box(i,i), i=1,DIM)
  end if
  200 format( 9(x,f9.5) )

  ! Return success
  gro_write_data = xslibOK

  return
end function gro_write_data

! Check .GRO file (check frames and construct offsets table)
integer function gro_check_file( unit, natoms, nframes, offsets )
  use iso_fortran_env, only: INT64
  implicit none
  integer, intent(in)         :: unit
  integer, intent(out)        :: natoms, nframes
  integer(INT64), allocatable :: offsets(:)
  integer                     :: iatoms, stat
  real                        :: time, ibox(DIM,DIM)
  character(GRO_MAX_LEN)      :: title
  logical                     :: opened
  character(9)                :: action
  integer(INT64), allocatable :: temp(:)

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    gro_check_file = xslibOPEN
    return
  end if

  ! Allocate frame offsets (start with size = 32)
  if ( allocated(offsets) ) deallocate( offsets, STAT=stat )
  allocate( offsets(32), SOURCE=0_INT64, STAT=stat )
  if ( stat /= 0 ) then
    gro_check_file = xslibNOMEM
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
        gro_check_file = xslibNOMEM
        return
      end if
      temp(:nframes) = offsets(:nframes)
      call move_alloc( temp, offsets )
    end if

    ! Store current position in file
    inquire( UNIT=unit, POS=offsets(nframes+1) )

    ! Read header (exit loop if EOF)
    gro_check_file = gro_read_header( unit, iatoms, title, time )
    if ( gro_check_file == xslibENDOFFILE ) exit
    if ( gro_check_file /= xslibOK ) return

    ! Another frame exists
    nframes = nframes+1

    ! Skip the rest of the frame
    gro_check_file = gro_skip_data( unit, iatoms, ibox )
    if ( gro_check_file /= xslibOK ) return

    ! Keep larger values
    natoms   = max( iatoms, natoms )
    ! box(:,:) = max( ibox, box )

  end do ! while

  ! Go back to beginning of file (this resets EOF flag)
  rewind( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    gro_check_file = xslibOPEN
    return
  end if

  ! Return success
  gro_check_file = xslibOK

  return
end function gro_check_file

! -------------------------------------------------
! gro_frame class procedures

! Allocate .GRO frame
integer function gro_frame_allocate( this, natoms )
  implicit none
  class(gro_frame)    :: this   ! .GRO data file
  integer, intent(in) :: natoms ! Number of points to allocate
  integer             :: stat

  ! Set number of atoms
  this%natoms = natoms

  ! Allocate all data
  if ( allocated(this%coor) ) deallocate( this%resn, this%atomn, this%resnm, this%atomnm, this%coor, this%vel, STAT=stat )
  allocate( this%resn(this%natoms), this%atomn(this%natoms), this%resnm(this%natoms), this%atomnm(this%natoms),  &
  &   this%coor(DIM,this%natoms), this%vel(DIM,this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    gro_frame_allocate = xslibNOMEM
    return
  end if

  ! Return success
  gro_frame_allocate = xslibOK

  return
end function gro_frame_allocate

! Assigment(=) for gro_frame class
subroutine gro_frame_assign( this, other )
  implicit none
  class(gro_frame), intent(inout) :: this
  type(gro_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%title    = other%title
  this%time     = other%time
  this%box(:,:) = other%box
  this%natoms   = other%natoms

  ! Copy data
  ! TODO: check all parameters
  if ( allocated(other%coor) ) then
    stat = this%allocate( other%natoms )
    if ( stat /= 0 ) error stop "Segmentation fault - allocation failure"
    this%resn(:)   = other%resn
    this%resnm(:)  = other%resnm
    this%atomn(:)  = other%atomn
    this%atomnm(:) = other%atomnm
    this%coor(:,:) = other%coor
    this%vel(:,:)  = other%vel

  end if

  return
end subroutine gro_frame_assign

! Copy frame data
! integer function gro_frame_copy( this, obj, dest, src, num )
!   implicit none
!   class(gro_frame)            :: this
!   type(gro_frame), intent(in) :: obj
!   integer, intent(in)         :: dest, src, num
!
!   ! Bound check
!   if ( (dest+num-1) > obj%natoms .or. (src+num-1) > this%natoms ) then
!     gro_frame_copy = xslibNOMEM
!     return
!   end if
!
!   ! Copy specified data
!   this%resn(dest:dest+num-1)   = obj%resn(src:src+num-1)
!   this%resnm(dest:dest+num-1)  = obj%resnm(src:src+num-1)
!   this%atomn(dest:dest+num-1)  = obj%atomn(src:src+num-1)
!   this%atomnm(dest:dest+num-1) = obj%atomnm(src:src+num-1)
!   this%coor(:,dest:dest+num-1) = obj%coor(:,src:src+num-1)
!   this%vel(:,dest:dest+num-1)  = obj%vel(:,src:src+num-1)
!
!   ! Return sucess
!   gro_frame_copy = xslibOK
!
!   return
! end function gro_frame_copy

! Read .GRO frame
integer function gro_frame_read( this, unit )
  implicit none
  class(gro_frame)    :: this
  integer, intent(in) :: unit

  ! Read header
  gro_frame_read = gro_read_header( unit, this%natoms, this%title, this%time )
  if ( gro_frame_read /= xslibOK ) return

  ! Allocate data
  gro_frame_read = this%allocate( this%natoms )
  if ( gro_frame_read /= xslibOK ) return

  ! Read data
  gro_frame_read = gro_read_data( unit, this%natoms, this%resn, this%resnm, this%atomn, this%atomnm, this%box, this%coor, this%vel )
  if ( gro_frame_read /= xslibOK ) return

  return
end function gro_frame_read

! Write .GRO frame
integer function gro_frame_write( this, unit )
  implicit none
  class(gro_frame)    :: this  ! .GRO data file
  integer, intent(in) :: unit

  ! Check if memory is allocated
  if ( .not. allocated(this%coor) ) then
    gro_frame_write = xslibNOMEM
    return
  end if

  ! Write data
  gro_frame_write =  gro_write_data( unit, this%natoms, this%title, this%time, this%resn, this%resnm,  &
  &   this%atomn, this%atomnm, this%box, this%coor, this%vel )

  return
end function gro_frame_write

! -------------------------------------------------
! gro_t class procedures

! Allocate NFRAMES of frames (only allocates frames)
integer function gro_allocate( this, nframes )
  implicit none
  class(gro_t)          :: this
  integer, intent(in)   :: nframes
  integer               :: stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(this%nframes), STAT=stat )
  if ( stat /= 0 ) then
    gro_allocate = xslibNOMEM
    return
  end if

  ! Return success
  gro_allocate = xslibOK

  return
end function gro_allocate

! Allocate NFRAMES of frames each with NATOMS of atoms
integer function gro_allocate_all( this, natoms, nframes )
  implicit none
  class(gro_t)          :: this
  integer, intent(in)   :: natoms, nframes
  integer               :: i, stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(this%nframes), STAT=stat )
  if ( stat /= 0 ) then
    gro_allocate_all = xslibNOMEM
    return
  end if

  ! Allocate each frame
  do i = 1, this%nframes
    gro_allocate_all = this%frame(i)%allocate( natoms )
    if ( gro_allocate_all /= xslibOK ) return

  end do

  ! Return success
  gro_allocate_all = xslibOK

  return
end function gro_allocate_all

! Assigment(=) for gro_t class
subroutine gro_assign( this, other )
  implicit none
  class(gro_t), intent(inout) :: this
  class(gro_t), intent(in)    :: other
  integer                     :: stat

  ! Copy header
  this%unit      = other%unit
  this%allFrames = other%allFrames
  this%current   = other%current

  ! Copy frame offsets
  if ( allocated(other%offsets) ) then
    if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )
    allocate( this%offsets(other%allFrames+1), STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%offsets(:) = other%offsets(1:other%allFrames+1) ! +1 for EOF
  end if

  ! Copy all frames
  this%natoms  = other%natoms
  this%nframes = other%nframes
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%natoms, other%nframes )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine gro_assign

! Open .GRO file
integer function gro_open( this, file )
  implicit none
  class(gro_t)                :: this
  character(*), intent(in)    :: file

  ! Open file for reading
  gro_open = gro_open_file( this%unit, file, .true. )
  if ( gro_open /= xslibOK ) return

  ! Check file and get frame offsets
  gro_open = gro_check_file( this%unit, this%natoms, this%allframes, this%offsets )
  if ( gro_open /= xslibOK ) return

  ! Set current frame to 1
  this%current = 1

  ! Return success
  gro_open = xslibOK

  return
end function gro_open

! Close .GRO file and clean-up
integer function gro_close( this )
  implicit none
  class(gro_t)  :: this
  integer       :: stat

  ! Close file
  gro_close = gro_close_file( this%unit )
  if ( gro_close /= xslibOK ) return

  ! Reset private variables
  this%allFrames = 0
  this%natoms    = 0
  this%current   = 0

  ! Clean-up offsets table
  if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )

  ! Return success
  gro_close = xslibOK

  return
end function gro_close

! Moves UNIT to the specified FRAME.
! * If WHENCE is set to 0, the OFFSET is taken as an absolute value,
! * if set to 1, OFFSET is taken to be relative to the current position,
! * and if set to 2, relative to the end of the file.
integer function gro_fseek( this, offset, whence )
  implicit none
  class(gro_t)        :: this
  integer, intent(in) :: offset, whence
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
  logical             :: opened
  integer             :: frame, stat

  ! Check if unit is opened
  inquire ( UNIT=this%unit, OPENED=opened )
  if ( .not. opened ) then
    gro_fseek = xslibOPEN
    return
  end if

  ! Check if offsets table is allocated
  if ( .not. allocated(this%offsets) ) then
    gro_fseek = xslibNOMEM
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
    gro_fseek = xslibNR
    return
  end select

  ! Limit selection to bounds
  frame = max( frame, 1 )
  frame = min( frame, this%allFrames+1 ) ! "+1" is end of file

  ! Move to selected frame
  if (  this%offsets(frame) < 4 ) then
    rewind( this%unit, IOSTAT=stat )
  else
    ! Account for 32bit/4byte offset for dummy read
    read (this%unit,*,POS=this%offsets(frame)-4,IOSTAT=stat) ! Dummy read
  end if
  if ( stat /= 0 ) then
    gro_fseek = xslibENDOFFILE
    return
  end if

  ! Update current frame
  this%current = frame

  ! Return success
  gro_fseek = xslibOK

  return
end function gro_fseek

! Retrieves the current position within an open file.
integer function gro_ftell( this )
  implicit none
  class(gro_t)        :: this
  gro_ftell = this%current
  return
end function gro_ftell

! Read next <N> frames in .GRO file
integer function gro_read_next( this, nframes )
  implicit none
  class(gro_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: frames, remaining, i

  ! Optional argument
  frames = merge( nframes, 1, present(nframes) )
  if ( frames < 0 ) frames = 1

  ! Calculate remaining frames
  remaining = this%allFrames-this%current+1

  ! Allocate frames
  this%nframes = min( frames, remaining )
  gro_read_next = this%allocate( this%nframes )
  if ( gro_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    gro_read_next = this%frame(i)%read( this%unit )
    if ( gro_read_next /= xslibOK ) return

    ! Increment current frame
    this%current = this%current+1

  end do ! for i

  ! Return success
  gro_read_next = xslibOK

  return
end function gro_read_next

! Skip next <N> frames in .GRO file
integer function gro_skip_next( this, nframes )
  implicit none
  class(gro_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: np

  ! Move to selected frame aka. "skip frames"
  np = merge( nframes, 1, present(nframes) )
  gro_skip_next = this%fseek( np, SEEK_CUR )

  return
end function gro_skip_next

! Read selected frame in .GRO file
integer function gro_read_frame( this, frame )
  implicit none
  class(gro_t)        :: this
  integer, intent(in) :: frame

  ! Move to selected frame
  if ( this%current /= frame ) then
    gro_read_frame = this%fseek( frame, SEEK_SET )
    if ( gro_read_frame /= 0 ) return
  end if

  ! Read frame
  gro_read_frame = this%read_next()

  return
end function gro_read_frame

! Read entire .GRO file
integer function gro_read( this, file, first, last, stride )
  implicit none
  class(gro_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: nfirst, nlast, nstride
  integer                       :: i, frame

  ! Open file
  gro_read = this%open( file )
  if ( gro_read /= xslibOK ) return

  ! Check optional arguments
  nfirst  = merge( first, 1, present(first) )
  nlast   = merge( last, -1, present(last) )
  nstride = merge( stride, 1, present(stride) )

  ! Prevet doing stupid things
  if ( nfirst < 0 ) nfirst = 1
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Calculate number of frames and allocate data
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  gro_read = this%allocate( this%nframes )
  if ( gro_read /= xslibOK ) return

  ! Read frames
  do i = 1, this%nframes
    ! Actual frame number
    frame = nfirst + (i-1)*nstride

    ! Move to selected frame
    if ( this%current /= frame ) then
      gro_read = this%fseek( frame, SEEK_SET )
      if ( gro_read /= 0 ) return
    end if

    ! Read i-th frame
    gro_read = this%frame(i)%read( this%unit )
    if ( gro_read /= xslibOK ) return

    ! Update current frame
    this%current = this%current+1

  end do ! for i

  ! Close file
  gro_read = this%close()
  if ( gro_read /= xslibOK ) return

  ! Return success
  gro_read = xslibOK

  return
end function gro_read

! Write data in .GRO format to unit, file or stdout
integer function gro_write( this, file, unit )
  use iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(gro_t)                        :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: i, out

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    gro_write = xslibNOMEM
    return
  end if

  ! Select output destination; Default is stdout.
  if ( present(file) ) then
    gro_write = gro_open_file( out, file, .false. )
    if ( gro_write /= xslibOK ) return
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Write each frame to output
  do i = 1, this%nframes
    gro_write = this%frame(i)%write( out )
    if ( gro_write /= xslibOK ) return
  end do ! for i

  ! Close file if present
  if ( present(file) ) then
    gro_write = gro_close_file( out )
    if ( gro_write /= xslibOK ) return
  end if

  ! Rerturn success
  gro_write = xslibOK

  return
end function gro_write

! -------------------------------------------------
! gro_t class utilities

! Return max. num. of atoms in currently opened file
integer function gro_getNatoms( this )
  implicit none
  class(gro_t)  :: this
  gro_getNatoms = this%natoms
  return
end function gro_getNatoms

! Return num. of frames in currently opened file
integer function gro_getNframes( this )
  implicit none
  class(gro_t)  :: this
  gro_getNframes = this%allframes
  return
end function gro_getNframes

! Return largest box in currently opened file
function gro_getBox( this ) result( box )
  implicit none
  class(gro_t)  :: this
  real          :: box(DIM,DIM)
  box(:,:) = this%box
  return
end function gro_getBox

! Return largest box in currently opened file
! function gro_getBox( this ) result( box )
!   implicit none
!   class(gro_t)            :: this
!   integer                 :: i, n, stat
!   real                    :: box(DIM,DIM), ibox(DIM,DIM)
!   character(GRO_MAX_LEN)  :: buffer
!   integer                 :: pos
!
!   ! Initialize result
!   box(:,:) = 0.000
!
!   ! Store starting position
!   pos = this%ftell()
!
!   ! Go to beginning of file
!   rewind( this%unit, IOSTAT=stat )
!   if ( stat /= 0 ) return
!
!   ! loop over all frames
!   do n = 1, this%allframes
!
!     ! Skip title, natoms and atom data.
!     do i = 1, this%natoms+2
!       read(this%unit,*,IOSTAT=stat) ! Dummy read
!       if ( stat /= 0 ) exit
!     end do
!
!     ! Store box side to buffer
!     read (this%unit,"(a)",IOSTAT=stat) buffer
!     if ( stat /= 0 ) exit
!
!     ! Try to reading triclinic box dimensions from buffer
!     ibox(:,:) = 0.000
!     read (buffer,*,IOSTAT=stat) ibox(:,:)
!     if ( stat /= 0 ) then
!       ! Ok, no problem, try reading cubic box dimensions from buffer
!       read (buffer,*,IOSTAT=stat) (ibox(i,i), i=1,DIM)
!       if ( stat /= 0 ) exit ! Ok, now we do  have a problem
!     end if
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
! end function gro_getBox

end module xslib_groio
