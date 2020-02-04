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
!
! NOTE: This is a fortran implementation of original GROMACS's code.
! https://github.com/gromacs/gromacs --> ttrio.cpp


module xslib_trrio
  use, intrinsic :: iso_fortran_env, only: REAL64
  use xdrfor
  implicit none
  private
  public :: trr_t, trr_frame

  ! Import error definitions
  include "fileio.h"

  ! Magic constant
  integer, parameter :: MAGIC = 1993

  ! All data in trnheader is hidden from user.
  type trnheader
    integer       :: ir_size = 0, e_size = 0, vir_size = 0, pres_size = 0 ! Backward compatibility
    integer       :: top_size = 0, sym_size = 0, nre = 0 ! Backward compatibility
    integer       :: box_size = 0, x_size = 0, v_size = 0, f_size = 0 ! Non-zero if box, x, v, f are present
    integer       :: bDouble = 0    ! Double precision?
    integer       :: natoms = 0, step = 0 ! The total number of atoms
    real          :: tf = 0.0, lambdaf = 0.0 ! Current time and value of lambda
    real(REAL64)  :: td = 0.0d0, lambdad = 0.0d0      ! Current time and value of lambda
  end type trnheader

  type trr_frame
    type(trnheader), private  :: sh
    integer                   :: natoms = 0, step = 0
    real                      :: lambda = 0.000, time = 0.000, box(3,3) = 0.000
    real, allocatable         :: coor(:,:), vel(:,:), force(:,:)
  contains
    procedure :: allocate => trr_frame_allocate
    procedure :: assign => trr_frame_assign
    generic   :: assignment(=) => assign
    procedure :: read => trr_frame_read
    procedure :: write => trr_frame_write
    procedure :: dump => trr_frame_dump
  end type trr_frame

  type trr_t
    type(xdrfile), pointer, private :: xd => null()
    integer, private                :: natoms = 0, allframes = 0, remaining = 0
    real, private                   :: box(3,3) = 0.000
    integer                         :: nframes = 0
    type(trr_frame), allocatable    :: frame(:)
  contains
    procedure :: trr_allocate, trr_allocate_all
    generic   :: allocate => trr_allocate, trr_allocate_all
    procedure :: assign => trr_assign
    generic   :: assignment(=) => assign
    procedure :: open => trr_open
    procedure :: close => trr_close
    procedure :: read => trr_read
    procedure :: read_next => trr_read_next
    procedure :: skip_next => trr_skip_next
    procedure :: write => trr_write
    procedure :: dump => trr_dump
    procedure :: getAllframes => trr_getAllframes
    procedure :: getNatoms => trr_getNatoms
    procedure :: getBox => trr_getCubicBox
  end type trr_t

contains

! -------------------------------------------------

! Calculates size of data from read header values.
integer function nFloatSize( sh, nflsize )
  use, intrinsic :: iso_c_binding, only: c_sizeof
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
  implicit none
  type(trnheader), intent(inout) :: sh
  integer, intent(out)            :: nflsize
  integer, parameter              :: DIM = 3

  ! Initialize
  nflsize = 0

  ! Calculate size depending on what we have
  if ( sh%box_size /= 0 ) then
    nflsize = sh%box_size/(DIM*DIM)
  else if ( sh%x_size /= 0 ) then
    nflsize = sh%x_size/(sh%natoms*DIM)
  else if ( sh%v_size /= 0 ) then
    nflsize = sh%v_size/(sh%natoms*DIM)
  else if ( sh%f_size /= 0 ) then
    nflsize = sh%f_size/(sh%natoms*DIM)
  else
    nFloatSize = xslibHEADER
    return
  end if

  ! Is calculated size correct?
  ! * sizeof(float) = REAL32
  ! * sizeof(double) = REAL64
  if ( nflsize /= c_sizeof(0.0_REAL32) .and. nflsize /= c_sizeof(0.0_REAL64) ) then
    nFloatSize = xslibHEADER
    return
  end if

  ! Return success
  nFloatSize = xslibOK

  return
end function nFloatSize

! Read/write trr header.
integer function trr_header( xd, sh, bRead )
  use, intrinsic :: iso_c_binding, only: c_sizeof
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  type(trnheader), intent(inout)      :: sh
  logical, intent(in)                 :: bRead
  integer                             :: imagic, slen, nflsize
  character(12)                       :: version = "GMX_trn_file"
  character(128)                      :: buffer

  ! Check pointer association
  if ( .not. associated(xd) ) then
    trr_header = xslibOPEN
    return
  end if

  ! Read magic number
  imagic = MAGIC
  if ( xdrfile_read_int( imagic, 1, xd ) /= 1 ) then
    trr_header = xslibENDOFFILE
    return
  else if ( imagic /= MAGIC ) then
    print *, imagic
    trr_header = xslibMAGIC
    return
  end if

  ! Read/write some header stuff ???
  if ( bRead ) then
    if ( xdrfile_read_int( slen, 1, xd ) /= 1 ) then
      trr_header = xslibINT
      return
    else if ( slen /= len(version)+1 ) then
      trr_header = xslibSTRING
      return
    end if

    if ( xdrfile_read_string( buffer, len(buffer), xd ) <= 0 ) then
      trr_header = xslibSTRING
      return
    end if

  else
    slen = len(version)+1
    if ( xdrfile_read_int( slen, 1, xd ) /= 1 ) then
      trr_header = xslibINT
      return
    end if

    if ( xdrfile_write_string( version, xd ) /= len(version)+1 ) then
      trr_header = xslibSTRING
      return
    end if

  end if

  ! Read/write data sizes
  if ( xdrfile_read_int( sh%ir_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%e_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%box_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%vir_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%pres_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%top_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%sym_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%x_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%v_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%f_size, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%natoms, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  ! Calculate header size
  trr_header = nFloatSize( sh, nflsize )
  if ( trr_header /= xslibOK ) return

  ! Are we double precision
  sh%bDouble = merge( 1, 0, nflsize == c_sizeof(0.0_REAL64) )

  ! Read yet more header information
  if ( xdrfile_read_int( sh%step, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( xdrfile_read_int( sh%nre, 1, xd ) /= 1 ) then
    trr_header = xslibINT
    return
  end if

  if ( sh%bDouble == 1 ) then
    if ( xdrfile_read_double( sh%td, 1, xd ) /= 1 ) then
      trr_header = xslibDOUBLE
      return
    end if
    sh%tf = real(sh%td)

    if ( xdrfile_read_double( sh%lambdad, 1, xd) /= 1 ) then
      trr_header = xslibDOUBLE
      return
    end if
    sh%lambdaf = real(sh%lambdad)

  else
    if ( xdrfile_read_float( sh%tf, 1, xd ) /= 1 ) then
      trr_header = xslibFLOAT
      return
    end if
    sh%td = sh%tf

    if ( xdrfile_read_float( sh%lambdaf, 1, xd) /= 1 ) then
      trr_header = xslibFLOAT
      return
    end if
    sh%lambdad = sh%lambdaf

  end if

  ! Return success
  trr_header = xslibOK

  return
end function trr_header

! Read/write trr data.
! NOTE: Memory allocation is done within routine.
integer function trr_data( xd, sh, box, x, v, f, bRead )
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  type(trnheader), intent(in)         :: sh
  real, intent(inout)                 :: box(3,3)
  real, allocatable, intent(inout)    :: x(:,:), v(:,:), f(:,:)
  logical, intent(in)                 :: bRead
  integer, parameter                  :: DIM = 3
  real(REAL64), allocatable           :: plcholder(:,:)
  real                                :: pvf(DIM,DIM)
  real(REAL64)                        :: pvd(DIM,DIM)
  integer                             :: stat

  ! Check xd pointer
  if ( .not. associated(xd) ) then
    trr_data = xslibOPEN
    return
  end if


  if ( sh%bDouble == 1 ) then

    ! NOTE: All .trr data types are single precission; if trajectory is
    ! *  double precision variables are still stored in single precision.

    ! NOTE: Use double pressicion placeholders for data transformations:
    ! * If read mode allocate data memory
    ! * else copy data to placeholder (float -> double)
    ! * Use xdrfile read/write with placeholder
    ! * Copy data from placeholder (double -> float)

    ! Box size double
    if ( sh%box_size /= 0 ) then
      if ( .not. bRead ) pvd(:,:) = real(box,REAL64)
      if ( xdrfile_read_double( pvd, size(pvd), xd ) /= DIM*DIM ) then
        trr_data = xslibDOUBLE
        return
      end if
      box(:,:) = real(pvd,REAL32)
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%vir_size /= 0 ) then
      if ( xdrfile_read_double( pvd, size(pvd), xd ) /= DIM*DIM ) then
        trr_data = xslibDOUBLE
        return
      end if
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%pres_size /= 0 ) then
      if ( xdrfile_read_double( pvd, size(pvd), xd ) /= DIM*DIM ) then
        trr_data = xslibDOUBLE
        return
      end if
    end if

    ! Allocate placeholder data
    if ( sh%x_size /= 0 .or. sh%v_size /= 0 .or. sh%f_size /= 0 ) then
      allocate( plcholder(DIM,sh%natoms), STAT=stat )
      if ( stat /= 0 ) then
        trr_data = xslibNOMEM
        return
      end if
    end if

    ! Coordinates
    if ( sh%x_size /= 0 ) then
      if ( bRead ) then
        ! Allocate memory
        if ( allocated(x) ) deallocate( x, STAT=stat )
        allocate( x(DIM,sh%natoms), STAT=stat )
        if ( stat /= 0 ) then
          trr_data = xslibNOMEM
          return
        end if
      else
        ! Copy to placeholder
        plcholder(:,:) = real(x,REAL64)
      end if

      ! Read data
      if ( xdrfile_read_double( plcholder, size(plcholder), xd ) /= sh%natoms*DIM ) then
        trr_data = xslib3DX
        return
      end if

      ! Copy from placeholder
      x(:,:) = real(plcholder,REAL32)

    end if

    ! Velocities
    if ( sh%v_size /= 0 ) then
      if ( bRead ) then
        ! Allocate data
        if ( allocated(v) ) deallocate( v, STAT=stat )
        allocate( v(DIM,sh%natoms), STAT=stat )
        if ( stat /= 0 ) then
          trr_data = xslibNOMEM
          return
        end if
      else
        ! Copy to placeholder
        plcholder(:,:) = real(v,REAL64)
      end if

      ! Read data
      if ( xdrfile_read_double( plcholder, size(plcholder), xd ) /= sh%natoms*DIM ) then
        trr_data = xslib3DX
        return
      end if

      ! Copy from placeholder
      v(:,:) = real(plcholder,REAL32)

    end if

    ! Forces
    if ( sh%f_size /= 0 ) then
      if ( bRead ) then
        ! Allocate memory
        if ( allocated(f) ) deallocate( f, STAT=stat )
        allocate( f(DIM,sh%natoms), STAT=stat )
        if ( stat /= 0 ) then
          trr_data = xslibNOMEM
          return
        end if
      else
        ! Copy to placeholder
        plcholder(:,:) = real(f,REAL64)
      end if

      ! Read data
      if ( xdrfile_read_double( plcholder, size(plcholder), xd ) /= sh%natoms*DIM ) then
        trr_data = xslib3DX
        return
      end if

      ! Copy from placeholder
      f(:,:) = real(plcholder,REAL32)

    end if

    ! Deallocate placeholder
    if ( sh%x_size /= 0 .or. sh%v_size /= 0 .or. sh%f_size /= 0 ) deallocate( plcholder )


  else ! float

    ! NOTE: No need for placeholder here, as .trr data are KIND float.

    ! Box size
    if ( sh%box_size /= 0 ) then
      if ( xdrfile_read_float( box, size(box), xd ) /= DIM*DIM ) then
        trr_data = xslibFLOAT
        return
      end if
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%vir_size /= 0 ) then
      if ( xdrfile_read_float( pvf, size(pvf), xd ) /= DIM*DIM ) then
        trr_data = xslibFLOAT
        return
      end if
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%pres_size /= 0 ) then
      if ( xdrfile_read_float( pvf, size(pvf), xd ) /= DIM*DIM ) then
        trr_data = xslibFLOAT
        return
      end if
    end if

    ! Coordinates
    if ( sh%x_size /= 0 ) then
      ! Allocate memory
      if ( bRead ) then
        if ( allocated(x) ) deallocate( x, STAT=stat )
        allocate( x(DIM,sh%natoms), STAT=stat )
        if ( stat /= 0 ) then
          trr_data = xslibNOMEM
          return
        end if
      end if
      ! Read data
      if ( xdrfile_read_float( x, size(x), xd ) /= sh%natoms*DIM ) then
        trr_data = xslib3DX
        return
      end if
    end if

    ! Velocities
    if ( sh%v_size /= 0 ) then
      ! Allocate memory
      if ( bRead ) then
        if ( allocated(v) ) deallocate( v, STAT=stat )
        allocate( v(DIM,sh%natoms), STAT=stat )
        if ( stat /= 0 ) then
          trr_data = xslibNOMEM
          return
        end if
      end if
      ! Read data
      if ( xdrfile_read_float( v, size(v), xd ) /= sh%natoms*DIM ) then
        trr_data = xslib3DX
        return
      end if
    end if

    ! Forces
    if ( sh%f_size /= 0 ) then
      ! Allocate memory
      if ( bRead ) then
        if ( allocated(f) ) deallocate( f, STAT=stat )
        allocate( f(DIM,sh%natoms), STAT=stat )
        if ( stat /= 0 ) then
          trr_data = xslibNOMEM
          return
        end if
      end if
      ! Read data
      if ( xdrfile_read_float( f, size(f), xd ) /= sh%natoms*DIM ) then
        trr_data = xslib3DX
        return
      end if
    end if

  end if

  ! Return sucess
  trr_data = xslibOK

  return
end function trr_data

! Skip trr data.
integer function trr_skip( xd, sh, box )
  use, intrinsic :: iso_c_binding, only: c_sizeof
  use, intrinsic :: iso_fortran_env, only: REAL32, INT64
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  type(trnheader), intent(in)         :: sh
  real, intent(inout)                 :: box(3,3)
  integer, parameter                  :: DIM = 3
  real(REAL64)                        :: double, pvd(DIM,DIM)
  real                                :: float
  integer(INT64)                      :: framebytes
  enum, bind(C)
    enumerator :: SEEK_SET, SEEK_CUR, SEEK_END
  end enum

  ! Check xd pointer
  if ( .not. associated(xd) ) then
    trr_skip = xslibOPEN
    return
  end if

  if ( sh%bDouble == 1 ) then

    ! Box size double
    if ( sh%box_size /= 0 ) then
      if ( xdrfile_read_double( pvd, size(pvd), xd ) /= DIM*DIM ) then
        trr_skip = xslibDOUBLE
        return
      end if
      box(:,:) = real(pvd,REAL32)
    end if

    ! Calculate storage size:
    framebytes = 0
    if ( sh%vir_size /= 0 ) framebytes = framebytes + DIM*DIM*sizeof(double)
    if ( sh%pres_size /= 0 ) framebytes = framebytes + DIM*DIM*sizeof(double)
    if ( sh%x_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*sizeof(double)
    if ( sh%v_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*sizeof(double)
    if ( sh%f_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*sizeof(double)

    ! Skip size
    trr_skip = xdr_seek( xd, framebytes, SEEK_CUR )
    if ( trr_skip /= xslibOK ) return

  else ! float

    ! Box size double
    if ( sh%box_size /= 0 ) then
      if ( xdrfile_read_float( box, size(box), xd ) /= DIM*DIM ) then
        trr_skip = xslibDOUBLE
        return
      end if
    end if

    ! Calculate storage size:
    framebytes = 0
    if ( sh%vir_size /= 0 ) framebytes = framebytes + DIM*DIM*sizeof(float)
    if ( sh%pres_size /= 0 ) framebytes = framebytes + DIM*DIM*sizeof(float)
    if ( sh%x_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*sizeof(float)
    if ( sh%v_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*sizeof(float)
    if ( sh%f_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*sizeof(float)

    ! Skip size
    trr_skip = xdr_seek( xd, framebytes, SEEK_CUR )
    if ( trr_skip /= xslibOK ) return

  end if

  ! Return success
  trr_skip = xslibOK

  return
end function trr_skip

! -------------------------------------------------

! Allocate trr frame data.
! NOTE: Use mode to allocate: x=Coordinates, v=Velocities, f=Forces
integer function trr_frame_allocate( this, natoms, mode )
  implicit none
  class(trr_frame)                    :: this
  integer, intent(in)                 :: natoms
  character(*), intent(in), optional  :: mode
  character(:), allocatable           :: imode
  integer                             :: i, stat

  ! Default allocation mode
  if ( present(mode) ) then
    imode = trim(mode)
  else
    imode = "x" ! Only coordinates
  end if

  ! Natoms
  this%natoms = natoms
  do i = 1, len_trim(imode)
    select case( imode(i:i) )
    case( "x", "X" )
      if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
      allocate( this%coor(3,natoms), STAT=stat )
      if ( stat /= 0 ) then
        trr_frame_allocate = xslibNOMEM
        return
      end if

    case( "v", "V" )
      if ( allocated(this%vel) ) deallocate( this%vel, STAT=stat )
      allocate( this%vel(3,natoms), STAT=stat )
      if ( stat /= 0 ) then
        trr_frame_allocate = xslibNOMEM
        return
      end if

    case( "f", "F" )
      if ( allocated(this%force) ) deallocate( this%force, STAT=stat )
      allocate( this%force(3,natoms), STAT=stat )
      if ( stat /= 0 ) then
        trr_frame_allocate = xslibNOMEM
        return
      end if

    case default
      ! Just let it roll
    end select
  end do

  ! Return success
  trr_frame_allocate = xslibOK

  return
end function trr_frame_allocate

! Comment
subroutine trr_frame_assign( this, other )
  implicit none
  class(trr_frame), intent(inout) :: this
  class(trr_frame), intent(in)    :: other
  integer                         :: stat

  ! Copy header data; No need to copy header itself.
  this%step = other%step
  this%lambda = other%lambda
  this%time = other%time
  this%box(:,:) = other%box

  ! Copy data
  this%natoms = other%natoms
  ! Coordinates
  if ( allocated(other%coor) ) then
    if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
    allocate( this%coor(3,this%natoms), SOURCE=other%coor, STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
  end if
  ! Velocities
  if ( allocated(other%vel) ) then
    if ( allocated(this%vel) ) deallocate( this%vel, STAT=stat )
    allocate( this%vel(3,this%natoms), SOURCE=other%vel, STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
  end if
  ! Forces
  if ( allocated(other%force) ) then
    if ( allocated(this%force) ) deallocate( this%force, STAT=stat )
    allocate( this%force(3,this%natoms), SOURCE=other%force, STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
  end if

  return
end subroutine trr_frame_assign

! Comment
integer function trr_frame_read( this, xd )
  implicit none
  class(trr_frame)                    :: this
  type(xdrfile), pointer, intent(in)  :: xd

  ! Read header
  trr_frame_read = trr_header( xd, this%sh, .true. )
  if ( trr_frame_read /= xslibOK ) return

  ! Read data
  trr_frame_read = trr_data( xd, this%sh, this%box, this%coor, this%vel, this%force, .true. )
  if ( trr_frame_read /= xslibOK ) return

  ! Copy data from trnheader
  this%natoms = this%sh%natoms
  this%step = this%sh%step
  this%time = this%sh%tf
  this%lambda = this%sh%lambdaf

  ! Return success
  trr_frame_read = xslibOK

  return
end function trr_frame_read

! Comment
integer function trr_frame_write( this, xd )
  use, intrinsic :: iso_c_binding, only: c_sizeof
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  class(trr_frame)                    :: this
  type(xdrfile), pointer, intent(in)  :: xd
  real                                :: float

  ! Edit trnheader
  if ( any(this%box /= 0.000) ) this%sh%box_size = int(size(this%box)*c_sizeof(float))
  if ( allocated(this%coor) ) this%sh%x_size = int(this%natoms*c_sizeof(float))
  if ( allocated(this%vel) ) this%sh%v_size = int(this%natoms*c_sizeof(float))
  if ( allocated(this%force) ) this%sh%f_size = int(this%natoms*c_sizeof(float))
  this%sh%natoms = this%natoms
  this%sh%step = this%step
  this%sh%tf = this%time
  this%sh%lambdaf = this%lambda
  this%sh%td = real(this%time,REAL64)
  this%sh%lambdad = real(this%lambda,REAL64)

  ! Write header
  trr_frame_write = trr_header( xd, this%sh, .false. )
  if ( trr_frame_write /= xslibOK ) return

  ! Write data
  trr_frame_write = trr_data( xd, this%sh, this%box, this%coor, this%vel, this%force, .false. )
  if ( trr_frame_write /= xslibOK ) return

  ! Return success
  trr_frame_write = xslibOK

  return
end function trr_frame_write

! Comment
integer function trr_frame_dump( this, unit )
  implicit none
  class(trr_frame)    :: this
  integer, intent(in) :: unit
  integer             :: i
  logical             :: opened
  character(9)        :: action

  ! Check if unit is opened
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    trr_frame_dump = xslibOPEN
    return
  end if

  ! Header
  ! * {natoms=         5  step=        10  time=0.0000000e+00  prec=        -1}
  write (unit,100) this%natoms, this%step, this%time, this%lambda
  100 format( "NATOMS:", i9, 2x, "STEP:", i9, 2x, "TIME:", e15.7e3, 2x, "LAMBDA:", e15.7e3  )

  ! Simulation box
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  do i = 1, 3
    write (unit,200) this%box(:,i)
    200 format( 5x, 3( 1pe13.5e2: "," ) )
  end do ! for i

  ! Coordinates
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  if ( allocated(this%coor) ) then
    write (unit,*) "x:", this%natoms
    do i = 1, this%natoms
      write (unit,300) this%coor(:,i)
      300 format( 3( 1pe13.5e2: "," ) )

    end do ! for i
  end if

  ! Velocities
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  if ( allocated(this%vel) ) then
    write (unit,*) "v:", this%natoms
    do i = 1, this%natoms
      write (unit,400) this%vel(:,i)
      400 format( 3( 1pe13.5e2: "," ) )

    end do ! for i
  end if

  ! Forces
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  if ( allocated(this%force) ) then
    write (unit,*) "f:", this%natoms
    do i = 1, this%natoms
      write (unit,500) this%force(:,i)
      500 format( 3( 1pe13.5e2: "," ) )

    end do ! for i
  end if

  ! Return success
  trr_frame_dump = xslibOK

  return
end function trr_frame_dump

! -------------------------------------------------

! Comment
integer function trr_allocate( this, nframes )
  implicit none
  class(trr_t)        :: this
  integer, intent(in) :: nframes
  integer             :: stat

  this%nframes = nframes
  if ( allocated(this%frame) ) deallocate(this%frame, STAT=stat )
  allocate( this%frame(nframes), STAT=stat )
  if ( stat /= 0 ) then
    trr_allocate = xslibNOMEM
    return
  end if

  ! Return sucess
  trr_allocate = xslibOK

  return
end function trr_allocate

! Comment
integer function trr_allocate_all( this, natoms, nframes, mode )
  implicit none
  class(trr_t)                        :: this
  integer, intent(in)                 :: natoms, nframes
  character(*), intent(in), optional  :: mode
  integer                             :: i

  ! Allocate frames
  trr_allocate_all = this%trr_allocate( nframes )
  if ( trr_allocate_all /= xslibOK ) return

  ! Allocate frame data
  do i = 1, nframes
    trr_allocate_all = this%frame(i)%allocate( natoms, mode )
    if ( trr_allocate_all /= xslibOK ) return

  end do

  ! Return sucess
  trr_allocate_all = xslibOK

  return
end function trr_allocate_all

! Comment
subroutine trr_assign( this, other )
  implicit none
  class(trr_t), intent(inout) :: this
  class(trr_t), intent(in)    :: other
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
end subroutine trr_assign

! Comment
integer function trr_open( this, file )
  use, intrinsic :: iso_fortran_env, only: INT64
  use, intrinsic :: iso_c_binding, only: c_f_pointer, C_NULL_CHAR
  implicit none
  class(trr_t)              :: this
  character(*), intent(in)  :: file
  type(trnheader)           :: sh
  real                      :: box(3,3)
  logical                   :: exist

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    trr_open = xslibFILENOTFOUND
    return
  end if

  ! Open file
  call c_f_pointer( xdrfile_open( trim(file)//C_NULL_CHAR, "r" ), this%xd )
  if ( .not. associated(this%xd) ) then
    trr_open = xslibOPEN
    return
  end if

  ! Initialize
  trr_open = xslibOK
  this%allframes = 0
  this%natoms = 0

  ! Count number of frames
  do while( trr_open == xslibOK )
    ! Read header
    trr_open = trr_header( this%xd, sh, .true. )
    if ( trr_open == xslibENDOFFILE ) exit ! ok
    if ( trr_open /= xslibOK ) return ! error

    ! Skip data
    trr_open = trr_skip( this%xd, sh, box )
    if ( trr_open /= xslibOK ) return

    ! Another frame was read/skiped
    this%allframes = this%allframes+1

    ! Save number of atoms
    this%natoms = max( this%natoms, sh%natoms )

    ! Save biggest box
    this%box(:,:) = merge( this%box, box, all(this%box>=box) )

  end do

  ! Rewind to beginig
  trr_open = xdr_seek( this%xd, 0_INT64, 0 )
  if ( trr_open /= xslibOK ) return

  ! All frames are remaining
  this%remaining = this%allframes

  ! Return success
  trr_open = xslibOK

  return
end function trr_open

! Comment
integer function trr_read( this, file, first, last, stride )
  implicit none
  class(trr_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: i, nfirst, nlast, nstride

  ! Open file.
  trr_read = this%open( file )
  if ( trr_read /= xslibOK ) return

  ! Optional parameters.
  nfirst = merge( first, 1, present(first) )
  nstride = merge( stride, 1, present(last) )
  nlast = merge( last, this%allframes, present(last) )

  ! Prevet doing stupid things.
  if ( nfirst > this%allframes ) nfirst = this%allframes
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Estimate number of frames an allocate data.
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  trr_read = this%allocate( this%nframes )
  if ( trr_read /= xslibOK ) return

  ! Skip all frames before first
  trr_read = this%skip_next( nfirst-1 )
  if ( trr_read /= xslibOK ) return

  ! ----------------
  ! Read frames
  do i = 1, this%nframes
    ! Read frame
    trr_read = this%frame(i)%read( this%xd )
    if ( trr_read /= xslibOK ) return

    ! If not last frame, skip until next frame.
    if ( i /= this%nframes ) trr_read = this%skip_next( nstride-1 )
    if ( trr_read /= xslibOK ) return

  end do ! for i

  ! No frames remain
  this%remaining = 0

  ! Close file
  trr_read = this%close()
  if ( trr_read /= xslibOK ) return

  ! Return success
  trr_read = xslibOK

  return
end function trr_read

! Comment
integer function trr_read_next( this, nframes )
  implicit none
  class(trr_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: i

  ! Allocate frames
  this%nframes = min( merge( nframes, 1, present(nframes) ), this%remaining )
  trr_read_next = this%allocate( this%nframes )
  if ( trr_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    trr_read_next = this%frame(i)%read( this%xd )
    if ( trr_read_next /= xslibOK ) return

    ! One less frame remaining
    this%remaining = this%remaining-1

  end do ! for i

  ! Return success
  trr_read_next = xslibOK

  return
end function trr_read_next

! Comment
integer function trr_skip_next( this, nframes )
  implicit none
  class(trr_t)                  :: this
  integer, intent(in), optional :: nframes
  type(trnheader)               :: sh
  real                          :: box(3,3)
  integer                       :: i

  ! Skip frame one by one
  do i = 1, min( merge( nframes, 1, present(nframes) ), this%remaining )
    ! Read header
    trr_skip_next = trr_header( this%xd, sh, .true. )
    if ( trr_skip_next /= xslibOK ) return

    ! Skip data
    trr_skip_next = trr_skip( this%xd, sh, box )
    if ( trr_skip_next /= xslibOK ) return

    ! One less frame remaining
    this%remaining = this%remaining-1

  end do ! for i

  ! Return success
  trr_skip_next = xslibOK

  return
end function trr_skip_next

! Comment
integer function trr_write( this, file )
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR, c_f_pointer
  implicit none
  class(trr_t)              :: this
  character(*), intent(in)  :: file
  type(xdrfile), pointer    :: xd => null()
  integer                   :: i

  ! Check if object is allocated
  if ( .not. allocated(this%frame) ) then
    trr_write = xslibNOMEM
    return
  end if

  ! Transform to Fortran pointer
  call c_f_pointer( xdrfile_open( trim(file)//C_NULL_CHAR, "w" ), xd )

  ! Check if opened
  if ( .not. associated(xd) ) then
    trr_write = xslibOPEN
    return
  end if

  do i = 1, this%nframes
    trr_write = this%frame(i)%write( xd )
    if ( trr_write /= xslibOK ) return

  end do ! for n

  ! Close file
  trr_write = xdrfile_close( xd )
  if ( trr_write /= xslibOK ) return

  ! Return success
  trr_write = xslibOK

  return
end function trr_write

! Comment
integer function trr_dump( this, file, unit )
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(trr_t)                        :: this
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: unit
  integer                             :: n, out, stat

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    trr_dump = xslibNOMEM
    return
  end if

  ! Select output method depending on what arguments are provided.
  if ( present(file) ) then
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      trr_dump = xslibFILENOTFOUND
      return
    end if
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Write each frame to output
  do n = 1, this%nframes
    trr_dump = this%frame(n)%dump( out )
    if ( trr_dump /= xslibOK ) return

  end do ! for n

  ! Close file if present
  if ( present(file) ) close( out )

  ! Rerturn success
  trr_dump = xslibOK

  return
end function trr_dump

! Comment
integer function trr_close( this )
  implicit none
  class(trr_t) :: this

  trr_close = xdrfile_close( this%xd )

  return
end function trr_close

! -------------------------------------------------

! Comment
integer function trr_getNatoms( this )
  implicit none
  class(trr_t) :: this
  trr_getNatoms = this%natoms
  return
end function trr_getNatoms

! Comment
integer function trr_getAllframes( this )
  implicit none
  class(trr_t) :: this
  trr_getAllframes = this%allframes
  return
end function trr_getAllframes

! Comment
function trr_getCubicBox( this )
  implicit none
  class(trr_t)  :: this
  real          :: trr_getCubicBox(3)
  trr_getCubicBox(:) = [this%box(1,1), this%box(2,2), this%box(3,3)]
  return
end function trr_getCubicBox

! Comment
function trr_getBox( this )
  implicit none
  class(trr_t)  :: this
  real          :: trr_getBox(3,3)
  trr_getBox(:,:) = this%box
  return
end function trr_getBox

end module xslib_trrio
