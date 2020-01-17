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
  implicit none
  private
  public :: gro_t, gro_frame

  ! Import error definitions
  include "fileio.h"

  ! .GRO file format
  type gro_frame
    character(:), allocatable  :: title
    real                       :: time = 0.0
    integer                    :: natoms = 0
    integer, allocatable       :: resn(:), atomn(:)
    character(6), allocatable  :: resnm(:), atomnm(:)
    real, allocatable          :: coor(:,:), vel(:,:)
    real                       :: box(3,3) = 0.0
  contains
    procedure :: allocate => gro_frame_allocate
    procedure :: assign => gro_frame_assign
    generic   :: assignment(=) => assign
    procedure :: read => gro_frame_read
    procedure :: write => gro_frame_write
  end type gro_frame


  type gro_t
    integer, private              :: unit = 0, natoms = 0, allframes = 0, remaining = 0
    real, private                 :: box(3,3) = 0.000
    integer                       :: nframes = 0
    type(gro_frame), allocatable  :: frame(:)
  contains
    procedure :: gro_allocate, gro_allocate_all
    generic   :: allocate => gro_allocate, gro_allocate_all
    procedure :: assign => gro_assign
    generic   :: assignment(=) => assign
    procedure :: open => gro_open
    procedure :: close => gro_close
    procedure :: read => gro_read
    procedure :: read_next => gro_read_next
    procedure :: skip_next => gro_skip_next
    procedure :: write => gro_write
    procedure :: getAllframes => gro_getAllframes
    procedure :: getNatoms => gro_getNatoms
    procedure :: getBox => gro_getCubicBox
  end type gro_t

contains

! -------------------------------------------------
! Class independat routines

integer function gro_header( unit, natoms, title, time )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)                     :: unit
  integer, intent(out)                    :: natoms
  character(:), allocatable, intent(out)  :: title
  real, intent(out)                       :: time
  character(128)                          :: buffer
  integer                                 :: i, stat
  logical                                 :: opened
  character(9)                            :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    gro_header = xslibOPEN
    return
  end if

  ! Read title
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    gro_header = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if
  title = trim(buffer)

  ! Try to extract time from comment
  time = 0.000
  i = index(title,"t=")
  if ( i /= 0 ) read (title(i+2:),*,IOSTAT=stat) time

  ! Read number of atoms
  read (unit,*,IOSTAT=stat) natoms
  if ( stat /= 0 ) then
    gro_header = merge( xslibENDOFFILE, xslibINT, stat == IOSTAT_END )
    return
  end if

  ! Return success
  gro_header = xslibOK

  return
end function gro_header

integer function gro_coor( unit, natoms, resn, resnm, atomn, atomnm, box, coor, vel )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)           :: unit, natoms
  real, intent(out)             :: coor(3,natoms), vel(3,natoms), box(3,3)
  character(6), intent(out)     :: resnm(natoms), atomnm(natoms)
  integer, intent(out)          :: resn(natoms), atomn(natoms)
  character(128)                :: buffer
  integer                       :: i, stat
  logical                       :: opened
  character(9)                  :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .and. index(action,"READ") == 0 ) then
    gro_coor = xslibOPEN
    return
  end if

  ! Initialize (velocity is optional)
  vel(:,:) = 0.000

  ! Read data
  do i = 1, natoms
    read (unit,100,IOSTAT=stat) resn(i), resnm(i), atomnm(i), coor(:,i), vel(:,i)
    if ( stat /= 0 ) then
      gro_coor = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
      return
    end if

    ! NOTE: renumber all atoms
    atomn(i) = i

    100 format( i5, 2a5, 5x, 3f8.3: 3f8.4 )

  end do

  ! Read line with simulation box side
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    gro_coor = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  box(:,:) = 0.000
  read (buffer,*,IOSTAT=stat) box(:,:)
  if ( stat /= 0 ) then
    ! Try reading cubic box dimmensions
    read (buffer,*,IOSTAT=stat) box(1,1), box(2,2), box(3,3)
    if ( stat /= 0 ) then
      gro_coor = xslib3DX
      return
    end if
  end if

  ! Return success
  gro_coor = xslibOK

  return
end function gro_coor

integer function gro_coor_skip( unit, natoms, box )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)           :: unit, natoms
  real, intent(out)             :: box(3,3)
  character(128)                :: buffer
  integer                       :: i, stat
  logical                       :: opened
  character(9)                  :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .and. index(action,"READ") == 0 ) then
    gro_coor_skip = xslibOPEN
    return
  end if

  ! Skip data data
  do i = 1, natoms
    read (unit,*,IOSTAT=stat)
    if ( stat /= 0 ) then
      gro_coor_skip = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
      return
    end if

  end do

  ! Read line with simulation box side
  read (unit,"(a)",IOSTAT=stat) buffer
  if ( stat /= 0 ) then
    gro_coor_skip = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
    return
  end if

  box(:,:) = 0.000
  read (buffer,*,IOSTAT=stat) box(:,:)
  if ( stat /= 0 ) then
    ! Try reading cubic box dimmensions
    read (buffer,*,IOSTAT=stat) box(1,1), box(2,2), box(3,3)
    if ( stat /= 0 ) then
      gro_coor_skip = xslib3DX
      return
    end if
  end if

  ! Return success
  gro_coor_skip = xslibOK

  return
end function gro_coor_skip

integer function gro_output( unit, natoms, title, time, resn, resnm, atomn, atomnm, box, coor, vel )
  implicit none
  integer, intent(in)       :: unit, natoms
  character(*), intent(in)  :: title
  real, intent(in)          :: time
  real, intent(in)          :: coor(3,natoms), vel(3,natoms), box(3,3)
  character(6), intent(in)  :: resnm(natoms), atomnm(natoms)
  integer, intent(in)       :: resn(natoms), atomn(natoms)
  integer, parameter        :: nmax = 100*1000 ! Max resn and atomn values.
  integer                   :: i
  logical                   :: opened
  character(9)              :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .and. index(action,"WRITE") == 0 ) then
    gro_output = xslibOPEN
    return
  end if

  ! Check if title contains time
  if ( index(title,"t=") == 0 ) then
    write (unit,"(a,' t= ',f10.5)") title, time
  else
    write (unit,"(a)") title
  end if

  ! Write number of atoms
  write (unit,"(2x,i0)") natoms

  ! Write all atoms and names
  if ( any(vel(:,:) /= 0.000) ) then
    do i = 1, natoms
      write (unit,100) mod(resn(i),nmax), adjustl(resnm(i)), adjustr(trim(atomnm(i))), mod(i,nmax), coor(:,i), vel(:,i)

    end do ! for i
  else
    do i = 1, natoms
      write (unit,100) mod(resn(i),nmax), adjustl(resnm(i)), adjustr(trim(atomnm(i))), mod(i,nmax), coor(:,i)

    end do ! for i
  end if

  100 format( i5, 2a5, i5, 3f8.3: 3f8.4 )

  ! Write box-side
  if ( all(box(:,:) /= 0.0) ) then
    write (unit,200) box(:,:)
  else
    write (unit,200) box(1,1), box(2,2), box(3,3)
  end if
  200 format( 9(x,f9.5) )

  ! Return success
  gro_output = xslibOK

  return
end function gro_output

! -------------------------------------------------

! Comment
integer function gro_frame_allocate( this, np )
  implicit none
  class(gro_frame)    :: this  ! .GRO data file
  integer, intent(in) :: np  ! Number of points to allocate
  integer             :: stat
  ! Set number of atoms
  this%natoms = np
  ! Allocate all data
  if ( allocated(this%coor) ) deallocate( this%resn, this%atomn,  &
  &   this%resnm, this%atomnm, this%coor, this%vel, STAT=stat )
  allocate( this%resn(this%natoms), this%atomn(this%natoms), this%resnm(this%natoms),  &
  &   this%atomnm(this%natoms), this%coor(3,this%natoms), this%vel(3,this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    gro_frame_allocate = xslibNOMEM
    return
  end if
  ! Return success
  gro_frame_allocate = xslibOK
  return
end function gro_frame_allocate

! Comment
subroutine gro_frame_assign( this, other )
  implicit none
  class(gro_frame), intent(inout) :: this
  type(gro_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%title = other%title
  this%time = other%time
  this%box(:,:) = other%box
  this%natoms = other%natoms

  ! Copy data
  ! TODO: check all parameters
  if ( allocated(other%coor) ) then
    stat = this%allocate( other%natoms )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%resn(:) = other%resn
    this%resnm(:) = other%resnm
    this%atomn(:) = other%atomn
    this%atomnm(:) = other%atomnm
    this%coor(:,:) = other%coor
    this%vel(:,:) = other%vel

  end if

  return
end subroutine gro_frame_assign

! Comment
integer function gro_frame_read( this, unit )
  implicit none
  class(gro_frame)    :: this
  integer, intent(in) :: unit

  ! Read header
  gro_frame_read = gro_header( unit, this%natoms, this%title, this%time )
  if ( gro_frame_read /= xslibOK ) return

  ! Allocate data
  gro_frame_read = this%allocate( this%natoms )
  if ( gro_frame_read /= xslibOK ) return

  ! Read data
  gro_frame_read = gro_coor( unit, this%natoms, this%resn, this%resnm, this%atomn, this%atomnm, this%box, this%coor, this%vel )
  if ( gro_frame_read /= xslibOK ) return

  return
end function gro_frame_read

! Comment
integer function gro_frame_write( this, unit )
  implicit none
  class(gro_frame)    :: this  ! .GRO data file
  integer, intent(in) :: unit

  ! Check if memory is allocated
  if ( .not. allocated(this%coor) ) then
    gro_frame_write = xslibNOMEM
    return
  end if

  gro_frame_write =  gro_output( unit, this%natoms, this%title, this%time, this%resn, this%resnm,  &
  &   this%atomn, this%atomnm, this%box, this%coor, this%vel )

  return
end function gro_frame_write

! -------------------------------------------------

! Comment
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

! Comment
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

! Comment
subroutine gro_assign( this, other )
  implicit none
  class(gro_t), intent(inout) :: this
  class(gro_t), intent(in)    :: other
  integer                     :: stat

  ! Copy header
  this%unit = other%unit
  this%natoms = other%natoms
  this%allframes = other%allframes
  this%box(:,:) = other%box

  ! Copy all frames
  this%nframes = other%nframes
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%nframes )
    if ( stat /= xslibOK ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine gro_assign

! Comment
integer function gro_open( this, file )
  implicit none
  class(gro_t)              :: this
  character(*), intent(in)  :: file
  logical                   :: exist
  integer                   :: natoms, stat
  real                      :: box(3,3), time
  character(:), allocatable :: title

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    gro_open = xslibFILENOTFOUND
    return
  end if

  ! Open and check file
  open( NEWUNIT=this%unit, FILE=trim(file), STATUS="old", ACTION="read", IOSTAT=stat )
  if ( stat /= 0 ) then
    gro_open = xslibOPEN
    return
  end if

  ! Read GRO header. Save only natoms.
  gro_open = gro_header( this%unit, this%natoms, title, time )
  if ( gro_open /= xslibOK ) return

  ! Read the rest of the frame. Save only box.
  gro_open = gro_coor_skip( this%unit, this%natoms, this%box )
  if ( gro_open /= xslibOK ) return

  ! Count the rest of the frames.
  this%allframes = 1 ! One frame already read.
  do while( gro_open == xslibOK )
    ! Read hedaer; if EOF is encountered stop.
    gro_open = gro_header( this%unit, natoms, title, time )
    if ( gro_open == xslibENDOFFILE ) exit
    if ( gro_open /= xslibOK ) return

    ! Skip the rest of the frame
    gro_open = gro_coor_skip( this%unit, natoms, box )
    if ( gro_open /= xslibOK ) return

    ! Save largest box and natoms
    this%natoms = merge( this%natoms, natoms, this%natoms > natoms )
    this%box(:,:) = merge( this%box, box, all(this%box >= box) )

    ! Another frame was read
    this%allframes = this%allframes+1

  end do

  ! All frames remain
  this%remaining = this%allframes

  ! Back to begining
  rewind( this%unit, IOSTAT=stat )

  ! Return success
  gro_open = xslibOK

  return
end function gro_open

! Comment
integer function gro_read( this, file, first, last, stride )
  implicit none
  class(gro_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: i, nfirst, nlast, nstride

  nfirst = merge( first, 1, present(first) )
  nlast = merge( last, -1, present(last) )
  nstride = merge( stride, 1, present(stride) )

  ! Open file
  gro_read = this%open( file )
  if ( gro_read /= xslibOK ) return

  ! Prevet doing stupid things.
  if ( nfirst > this%allframes ) nfirst = this%allframes
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Estimate number of frames an allocate data.
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  gro_read = this%allocate( this%nframes )

  ! Skip all frames before first
  gro_read = this%skip_next( nfirst-1 )
  if ( gro_read /= xslibOK ) return

  ! ----------------
  ! Read frames
  do i = 1, this%nframes
    ! Read frame
    gro_read = this%frame(i)%read( this%unit )
    if ( gro_read /= xslibOK ) return

    ! If not last frame, skip until next frame.
    if ( i /= this%nframes ) then
      gro_read = this%skip_next( nstride-1 )
      if ( gro_read /= xslibOK ) return

    end if
  end do ! for i

  ! No frames remaining
  this%remaining = 0

  ! Close file
  gro_read = this%close()
  if ( gro_read /= xslibOK ) return

  ! Return success
  gro_read = xslibOK

  return
end function gro_read

! Comment
integer function gro_read_next( this, nframes )
  implicit none
  class(gro_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: i

  ! Allocate frames
  this%nframes = min( merge( nframes, 1, present(nframes) ), this%remaining )
  gro_read_next = this%allocate( this%nframes )
  if ( gro_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    ! Read Header
    gro_read_next = this%frame(i)%read( this%unit )
    if ( gro_read_next /= xslibOK ) return

    ! One less frame remaining
    this%remaining = this%remaining-1

  end do ! for n

  ! Return success
  gro_read_next = xslibOK

  return
end function gro_read_next

! Comment
integer function gro_skip_next( this, nframes )
  implicit none
  class(gro_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: natoms, i
  real                          :: box(3,3), time
  character(:), allocatable     :: title

  do i = 1, min( merge( nframes, 1, present(nframes) ), this%remaining )
    ! Read header
    gro_skip_next = gro_header( this%unit, natoms, title, time )
    if ( gro_skip_next /= xslibOK ) return

    ! Skip coordinates
    gro_skip_next = gro_coor_skip( this%unit, natoms, box )
    if ( gro_skip_next /= xslibOK ) return

    ! One less frame remaining
    this%remaining = this%remaining-1

  end do

  ! Return success
  gro_skip_next = xslibOK

  return
end function gro_skip_next

! Comment
integer function gro_write( this, file, unit )
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(gro_t)                        :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: i, out, stat

  ! Select output method. Default is stdout.
  if ( present(file) ) then
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      gro_write = xslibOPEN
      return
    end if
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    gro_write = xslibNOMEM
    return
  end if

  ! Write each frame to output
  do i = 1, this%nframes
    gro_write = this%frame(i)%write( out )
    if ( gro_write /= xslibOK ) return

  end do ! for n

  ! Close file if present
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      gro_write = xslibCLOSE
      return
    end if
  end if

  ! Rerturn success
  gro_write = xslibOK

  return
end function gro_write

! Comment
integer function gro_close( this )
  implicit none
  class(gro_t)  :: this
  integer       :: stat

  close( this%unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    gro_close = xslibCLOSE
    return
  end if

  ! Return success
  gro_close = xslibOK

  return
end function gro_close

! -------------------------------------------------

integer function gro_getNatoms( this )
  implicit none
  class(gro_t)  :: this
  gro_getNatoms = this%natoms
  return
end function gro_getNatoms

integer function gro_getAllframes( this )
  implicit none
  class(gro_t)  :: this
  gro_getAllframes = this%allframes
  return
end function gro_getAllframes

real function gro_getCubicBox( this )
  implicit none
  class(gro_t)  :: this
  dimension     :: gro_getCubicBox(3)

  gro_getCubicBox(:) = [this%box(1,1), this%box(2,2), this%box(3,3)]

  return
end function gro_getCubicBox

real function gro_getBox( this )
  implicit none
  class(gro_t)  :: this
  dimension     :: gro_getBox(3,3)

  gro_getBox(:,:) = this%box

  return
end function gro_getBox

end module xslib_groio
