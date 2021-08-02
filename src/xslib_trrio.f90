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
  use iso_fortran_env, only: REAL64, INT64
  use xdrfor
  implicit none
  private
  public :: trr_t, trnheader
  ! Class independent procedures (if you are feeling adventurous)
  public :: trr_open_file, trr_close_file, trr_header, trr_data, trr_coor, trr_skip, trr_check_file

  ! Import error definitions
  include "fileio.h"

  ! Global constants
  integer, parameter      :: DIM = 3 ! Default dimension
  integer, parameter      :: MAGIC = 1993 ! Magic constant
  integer, parameter      :: TRR_MAX_LEN = 128 ! Max buffer length
  character(*), parameter :: TRN_VERSION = "GMX_trn_file" ! "Magic" string
  integer, parameter      :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2 ! Positioning constants

  type trnheader
    integer       :: ir_size = 0, e_size = 0, vir_size = 0, pres_size = 0 ! Backward compatibility
    integer       :: top_size = 0, sym_size = 0, nre = 0                  ! Backward compatibility
    integer       :: box_size = 0, x_size = 0, v_size = 0, f_size = 0     ! Non-zero if box, x, v, f are present
    integer       :: bDouble = 0                 ! Is double precision
    integer       :: natoms = 0, step = 0        ! The total number of atoms
    real          :: tf = 0.0, lambdaf = 0.0     ! Current time and value of lambda
    real(REAL64)  :: td = 0.0d0, lambdad = 0.0d0 ! Current time and value of lambda (double)
  end type trnheader

  type trr_frame
    type(trnheader), private  :: sh                               ! Header object     
    integer                   :: natoms = 0, step = 0             ! Number of atoms and num. of steps
    real                      :: lambda = 0.000, time = 0.000     ! Lambda and time step
    real                      :: box(DIM,DIM) = 0.000             ! Simulation box
    real, allocatable         :: coor(:,:), vel(:,:), force(:,:)  ! Coordinates, velocities, and forces
  contains
    procedure :: allocate => trr_frame_allocate
    procedure :: assign => trr_frame_assign
    generic   :: assignment(=) => assign
    procedure :: read => trr_frame_read
    procedure :: write => trr_frame_write
    procedure :: dump => trr_frame_dump
  end type trr_frame

  type trr_t
    type(xdrfile), pointer, private       :: xd => null()       ! XDR file pointer
    integer, private                      :: allFrames = 0      ! Num. of frames in currently opened file
    integer, private                      :: natoms = 0         ! Num. of atoms (per frame) in currently opened file
    integer, private                      :: current = 0        ! Current frame in opened file
    real, private                         :: box(DIM,DIM) = 0.  ! Box side
    integer(INT64), allocatable, private  :: offsets(:)         ! Frames offset table
    ! Public data --------------------------
    integer                               :: nframes = 0        ! Num. of frames
    type(trr_frame), allocatable          :: frame(:)           ! Frame object
  contains
    procedure :: trr_allocate, trr_allocate_all
    generic   :: allocate => trr_allocate, trr_allocate_all
    procedure :: assign => trr_assign
    generic   :: assignment(=) => assign
    procedure :: open => trr_open
    procedure :: close => trr_close
    procedure :: fseek => trr_fseek
    procedure :: ftell => trr_ftell
    procedure :: read_next => trr_read_next
    procedure :: skip_next => trr_skip_next
    procedure :: read_frame => trr_read_frame
    procedure :: read => trr_read
    procedure :: write => trr_write
    procedure :: dump => trr_dump
    procedure :: getNframes => trr_getNframes
    procedure :: getNatoms => trr_getNatoms
    procedure :: getBox => trr_getBox
  end type trr_t

contains

! -------------------------------------------------
! NOTE: xrdfile read is same as write. He he he.

! Calculates size of data from read header values.
integer function nFloatSize( sh, nflsize )
  use iso_c_binding, only: c_sizeof
  use iso_fortran_env, only: REAL32, REAL64
  implicit none
  type(trnheader), intent(inout)  :: sh
  integer, intent(out)            :: nflsize

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
  if ( nflsize /= c_sizeof(0.0_REAL32) .and. nflsize /= c_sizeof(0.0_REAL64) ) then
    nFloatSize = xslibHEADER
    return
  end if

  ! Return success
  nFloatSize = xslibOK

  return
end function nFloatSize

! Open .TRR file
integer function trr_open_file( xd, file, bRead )
  use iso_c_binding, only: c_f_pointer, C_NULL_CHAR
  implicit none
  type(xdrfile), pointer, intent(out) :: xd
  character(*), intent(in)            :: file
  logical, intent(in)                 :: bRead ! Open in read mode
  logical                             :: exist

  ! Open file in read or write mode
  if ( bRead ) then
    inquire( FILE=file, EXIST=exist )
    if ( .not. exist ) then
      trr_open_file = xslibFILENOTFOUND
      return
    end if
    call c_f_pointer( xdrfile_open( trim(file)//C_NULL_CHAR, 'r' ), xd )

  else
    call c_f_pointer( xdrfile_open( trim(file)//C_NULL_CHAR, 'w' ), xd )

  end if

  ! Check pointer association
  if ( .not. associated(xd) ) then
    trr_open_file = xslibOPEN
    return
  end if

  ! Return success
  trr_open_file = xslibOK

  return
end function trr_open_file

! Close .TRR file
integer function trr_close_file( xd )
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  trr_close_file = xdrfile_close( xd )
  return
end function trr_close_file

! Read/write .TRR file header
integer function trr_header( xd, sh, bRead )
  use iso_c_binding, only: c_sizeof
  use iso_fortran_env, only: REAL64
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  type(trnheader), intent(inout)      :: sh
  logical, intent(in)                 :: bRead
  integer                             :: imagic, slen, nflsize
  character(TRR_MAX_LEN)              :: buffer

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
  end if

  ! Check magic number
  if ( imagic /= MAGIC ) then
    trr_header = xslibMAGIC
    return
  end if

  ! Read/write some header stuff ???
  if ( bRead ) then
    if ( xdrfile_read_int( slen, 1, xd ) /= 1 ) then
      trr_header = xslibINT
      return
    else if ( slen /= len(TRN_VERSION)+1 ) then
      trr_header = xslibSTRING
      return
    end if

    if ( xdrfile_read_string( buffer, len(buffer), xd ) <= 0 ) then
      trr_header = xslibSTRING
      return
    end if

  else
    slen = len(TRN_VERSION)+1
    if ( xdrfile_read_int( slen, 1, xd ) /= 1 ) then
      trr_header = xslibINT
      return
    end if

    if ( xdrfile_write_string( TRN_VERSION, xd ) /= len(TRN_VERSION)+1 ) then
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

  ! Read more header information
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

! Read/write .TRR file data
! NOTE: Memory allocation is done within routine.
integer function trr_data( xd, sh, box, x, v, f, bRead )
  use iso_fortran_env, only: REAL32, REAL64
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  type(trnheader), intent(in)         :: sh
  real, intent(inout)                 :: box(DIM,DIM)
  real, allocatable, intent(inout)    :: x(:,:), v(:,:), f(:,:)
  logical, intent(in)                 :: bRead
  real(REAL64), allocatable           :: plcholder(:,:)
  real                                :: pvf(DIM,DIM)
  real(REAL64)                        :: pvd(DIM,DIM)
  integer                             :: stat

  ! Check if pointer is associated
  if ( .not. associated(xd) ) then
    trr_data = xslibOPEN
    return
  end if

  if ( sh%bDouble == 1 ) then

    ! NOTE: All .trr data types are single precission; if trajectory is
    ! *  double precision variables are still stored in single precision.

    ! NOTE: Use double pressicion placeholders for data transformations:
    ! * If in "read mode", allocate data memory
    ! * else copy data to placeholder (float -> double)
    ! * Use xdrfile read/write with placeholder
    ! * Copy data from placeholder (double -> float)

    ! Box size double
    if ( sh%box_size /= 0 ) then
      if ( .not. bRead ) pvd(:,:) = real(box,REAL64)
      if ( xdrfile_read_double( pvd, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_data = xslibDOUBLE
        return
      end if
      box(:,:) = real(pvd,REAL32)
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%vir_size /= 0 ) then
      if ( xdrfile_read_double( pvd, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_data = xslibDOUBLE
        return
      end if
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%pres_size /= 0 ) then
      if ( xdrfile_read_double( pvd, DIM*DIM, xd ) /= DIM*DIM ) then
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
      if ( xdrfile_read_double( plcholder, sh%natoms*DIM, xd ) /= sh%natoms*DIM ) then
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
      if ( xdrfile_read_double( plcholder, sh%natoms*DIM, xd ) /= sh%natoms*DIM ) then
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
      if ( xdrfile_read_double( plcholder, sh%natoms*DIM, xd ) /= sh%natoms*DIM ) then
        trr_data = xslib3DX
        return
      end if

      ! Copy from placeholder
      f(:,:) = real(plcholder,REAL32)

    end if

    ! Deallocate placeholder
    if ( sh%x_size /= 0 .or. sh%v_size /= 0 .or. sh%f_size /= 0 ) deallocate( plcholder )


  else ! float

    ! NOTE: No need for placeholder here, as .TRR data are REAL32.

    ! Box size
    if ( sh%box_size /= 0 ) then
      if ( xdrfile_read_float( box, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_data = xslibFLOAT
        return
      end if
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%vir_size /= 0 ) then
      if ( xdrfile_read_float( pvf, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_data = xslibFLOAT
        return
      end if
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%pres_size /= 0 ) then
      if ( xdrfile_read_float( pvf, DIM*DIM, xd ) /= DIM*DIM ) then
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
      if ( xdrfile_read_float( x, sh%natoms*DIM, xd ) /= sh%natoms*DIM ) then
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
      if ( xdrfile_read_float( v, sh%natoms*DIM, xd ) /= sh%natoms*DIM ) then
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
      if ( xdrfile_read_float( f, sh%natoms*DIM, xd ) /= sh%natoms*DIM ) then
        trr_data = xslib3DX
        return
      end if
    end if

  end if

  ! Return sucess
  trr_data = xslibOK

  return
end function trr_data

! Read (only) .TRR coordinates
! NOTE: Memory allocation is done OUTSIDE routine (for compatibility).
integer function trr_coor( xd, sh, box, x )
  use iso_c_binding, only: c_sizeof
  use iso_fortran_env, only: REAL32, REAL64, INT64
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  type(trnheader), intent(in)         :: sh
  real, intent(out)                   :: box(DIM,DIM), x(DIM,sh%natoms)
  integer                             :: stat
  real(REAL64), allocatable           :: plcholder(:,:)
  real                                :: float, pvf(DIM,DIM)
  real(REAL64)                        :: double, pvd(DIM,DIM)
  integer(INT64)                      :: framebytes

  ! Check if pointer is associated
  if ( .not. associated(xd) ) then
    trr_coor = xslibOPEN
    return
  end if

  if ( sh%bDouble == 1 ) then

    ! NOTE: All .TRR data types are single precission; if trajectory is
    ! * double precision variables are still stored in single precision.

    ! Box size double
    if ( sh%box_size /= 0 ) then
      if ( xdrfile_read_double( pvd, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_coor = xslibDOUBLE
        return
      end if
      box(:,:) = real(pvd,REAL32)
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%vir_size /= 0 ) then
      if ( xdrfile_read_double( pvd, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_coor = xslibDOUBLE
        return
      end if
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%pres_size /= 0 ) then
      if ( xdrfile_read_double( pvd, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_coor = xslibDOUBLE
        return
      end if
    end if

    ! Coordinates
    if ( sh%x_size /= 0 ) then
      allocate( plcholder(DIM,sh%natoms), STAT=stat )
      if ( stat /= 0 ) then
        trr_coor = xslibNOMEM
        return
      end if
      if ( xdrfile_read_double( plcholder, sh%natoms*DIM, xd ) /= sh%natoms*DIM ) then
        trr_coor = xslib3DX
        return
      end if
      x(:,:) = real(plcholder,REAL32)
      deallocate( plcholder )
    end if

    ! Skip velocities and forces
    framebytes = 0
    if ( sh%v_size /= 0 ) framebytes = framebytes+sh%natoms*DIM*c_sizeof(double)
    if ( sh%f_size /= 0 ) framebytes = framebytes+sh%natoms*DIM*c_sizeof(double)
    trr_coor = xdr_seek( xd, framebytes, SEEK_CUR )
    if ( trr_coor /= xslibOK ) return

  else ! float

    ! NOTE: No need for placeholder here, as .TRR data are REAL32.

    ! Box size
    if ( sh%box_size /= 0 ) then
      if ( xdrfile_read_float( box, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_coor = xslibFLOAT
        return
      end if
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%vir_size /= 0 ) then
      if ( xdrfile_read_float( pvf, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_coor = xslibFLOAT
        return
      end if
    end if

    ! Dummy read (legacy parameter?)
    if ( sh%pres_size /= 0 ) then
      if ( xdrfile_read_float( pvf, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_coor = xslibFLOAT
        return
      end if
    end if

    ! Coordinates
    if ( sh%x_size /= 0 ) then
      if ( xdrfile_read_float( x, sh%natoms*DIM, xd ) /= sh%natoms*DIM ) then
        trr_coor = xslib3DX
        return
      end if
    end if

    ! Skip velocities and forces
    framebytes = 0
    if ( sh%v_size /= 0 ) framebytes = framebytes+sh%natoms*DIM*c_sizeof(float)
    if ( sh%f_size /= 0 ) framebytes = framebytes+sh%natoms*DIM*c_sizeof(float)
    trr_coor = xdr_seek( xd, framebytes, SEEK_CUR )
    if ( trr_coor /= xslibOK ) return

  end if

  ! Return sucess
  trr_coor = xslibOK

  return
end function trr_coor

! Skip .TRR data
integer function trr_skip( xd, sh, box )
  use iso_c_binding, only: c_sizeof
  use iso_fortran_env, only: REAL32, INT64
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  type(trnheader), intent(in)         :: sh
  real, intent(out)                   :: box(DIM,DIM)
  real(REAL64)                        :: double, pvd(DIM,DIM)
  real                                :: float
  integer(INT64)                      :: framebytes

  ! Check if pointer association
  if ( .not. associated(xd) ) then
    trr_skip = xslibOPEN
    return
  end if

  if ( sh%bDouble == 1 ) then
    ! Box size double
    if ( sh%box_size /= 0 ) then
      if ( xdrfile_read_double( pvd, DIM*DIM, xd ) /= DIM*DIM ) then
        trr_skip = xslibDOUBLE
        return
      end if
      box(:,:) = real(pvd,REAL32)
    end if

    ! Calculate storage size:
    framebytes = 0
    if ( sh%vir_size /= 0 ) framebytes = framebytes + DIM*DIM*c_sizeof(double)
    if ( sh%pres_size /= 0 ) framebytes = framebytes + DIM*DIM*c_sizeof(double)
    if ( sh%x_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(double)
    if ( sh%v_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(double)
    if ( sh%f_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(double)

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
    if ( sh%vir_size /= 0 ) framebytes = framebytes + DIM*DIM*c_sizeof(float)
    if ( sh%pres_size /= 0 ) framebytes = framebytes + DIM*DIM*c_sizeof(float)
    if ( sh%x_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(float)
    if ( sh%v_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(float)
    if ( sh%f_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(float)

    ! Skip size
    trr_skip = xdr_seek( xd, framebytes, SEEK_CUR )
    if ( trr_skip /= xslibOK ) return

  end if

  ! Return success
  trr_skip = xslibOK

  return
end function trr_skip

! Check .TRR file (check frames and construct offsets table)
integer function trr_check_file( xd, natoms, nframes, offsets )
  use iso_fortran_env, only: INT64
  implicit none
  type(xdrfile), pointer, intent(in)  :: xd
  integer, intent(out)                :: natoms, nframes
  integer(INT64), allocatable         :: offsets(:)
  integer                             :: stat
  real                                :: ibox(DIM,DIM)
  type(trnheader)                     :: sh
  integer(INT64), allocatable         :: temp(:)

  ! Check pointer association
  if ( .not. associated(xd) ) then
    trr_check_file = xslibOPEN
    return
  end if

  ! Allocate frame offsets (start with size = 32)
  if ( allocated(offsets) ) deallocate( offsets, STAT=stat )
  allocate( offsets(32), SOURCE=0_INT64, STAT=stat )
  if ( stat /= 0 ) then
    trr_check_file = xslibNOMEM
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
        trr_check_file = xslibNOMEM
        return
      end if
      temp(:nframes) = offsets(:nframes)
      call move_alloc( temp, offsets )
    end if

    ! Store current position in file
    offsets(nframes+1) = xdr_tell( xd )

    ! Read header (exit loop if EOF)
    trr_check_file = trr_header( xd, sh, .true. )
    if ( trr_check_file == xslibENDOFFILE ) exit
    if ( trr_check_file /= xslibOK ) return

    ! Another frame exists
    nframes = nframes+1

    ! Skip the rest of the frame
    trr_check_file = trr_skip( xd, sh, ibox )
    if ( trr_check_file /= xslibOK ) return

    ! Keep largest values
    natoms   = max( sh%natoms, natoms )
    ! box(:,:) = max( ibox, box )

  end do ! while

  ! Rewind file
  trr_check_file = xdr_seek( xd, 0_INT64, SEEK_SET )
  if ( trr_check_file /= xslibOK ) return

  ! Return success
  trr_check_file = xslibOK

  return
end function trr_check_file

! -------------------------------------------------
! trr_frame class procedures

! Allocate frame
! NOTE: Use mode to allocate: X=Coordinates, V=Velocities, F=Forces
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
      allocate( this%coor(DIM,natoms), STAT=stat )
      if ( stat /= 0 ) then
        trr_frame_allocate = xslibNOMEM
        return
      end if

    case( "v", "V" )
      if ( allocated(this%vel) ) deallocate( this%vel, STAT=stat )
      allocate( this%vel(DIM,natoms), STAT=stat )
      if ( stat /= 0 ) then
        trr_frame_allocate = xslibNOMEM
        return
      end if

    case( "f", "F" )
      if ( allocated(this%force) ) deallocate( this%force, STAT=stat )
      allocate( this%force(DIM,natoms), STAT=stat )
      if ( stat /= 0 ) then
        trr_frame_allocate = xslibNOMEM
        return
      end if

    end select
  end do

  ! Return success
  trr_frame_allocate = xslibOK

  return
end function trr_frame_allocate

! Assigment(=) for trr_frame class
subroutine trr_frame_assign( this, other )
  implicit none
  class(trr_frame), intent(inout) :: this
  class(trr_frame), intent(in)    :: other
  integer                         :: stat

  ! Copy header data
  this%step     = other%step
  this%lambda   = other%lambda
  this%time     = other%time
  this%box(:,:) = other%box

  ! No need to copy header
  ! this%sh = other%sh

  ! Copy data
  this%natoms = other%natoms
  ! Coordinates
  if ( allocated(other%coor) ) then
    if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
    allocate( this%coor(DIM,this%natoms), SOURCE=other%coor, STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
  end if
  ! Velocities
  if ( allocated(other%vel) ) then
    if ( allocated(this%vel) ) deallocate( this%vel, STAT=stat )
    allocate( this%vel(DIM,this%natoms), SOURCE=other%vel, STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
  end if
  ! Forces
  if ( allocated(other%force) ) then
    if ( allocated(this%force) ) deallocate( this%force, STAT=stat )
    allocate( this%force(DIM,this%natoms), SOURCE=other%force, STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
  end if

  return
end subroutine trr_frame_assign

! Read .TRR frame
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
  this%step   = this%sh%step
  this%time   = this%sh%tf
  this%lambda = this%sh%lambdaf

  ! Return success
  trr_frame_read = xslibOK

  return
end function trr_frame_read

! Write .TRR frame
integer function trr_frame_write( this, xd )
  use iso_c_binding, only: c_sizeof
  use iso_fortran_env, only: REAL64
  implicit none
  class(trr_frame)                    :: this
  type(xdrfile), pointer, intent(in)  :: xd
  real                                :: float

  ! Edit trnheader
  if ( any(this%box /= 0.000) ) this%sh%box_size = int(DIM*DIM*c_sizeof(float))
  if ( allocated(this%coor) ) this%sh%x_size     = int(this%natoms*c_sizeof(float))
  if ( allocated(this%vel) ) this%sh%v_size      = int(this%natoms*c_sizeof(float))
  if ( allocated(this%force) ) this%sh%f_size    = int(this%natoms*c_sizeof(float))

  ! Copy data from trnheader
  this%sh%natoms  = this%natoms
  this%sh%step    = this%step
  this%sh%tf      = this%time
  this%sh%lambdaf = this%lambda
  this%sh%td      = real(this%time,REAL64)
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

! Write .TRR frame in human readable format
integer function trr_frame_dump( this, unit )
  implicit none
  class(trr_frame)    :: this
  integer, intent(in) :: unit
  integer             :: i
  logical             :: opened
  character(9)        :: action

  ! Check if unit is opened for writing
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
  do i = 1, DIM
    write (unit,200) this%box(:,i)
    200 format( 5x, 3( 1pe13.5e2: "," ) )
  end do ! for i

  ! Coordinates
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  if ( allocated(this%coor) ) then
    write (unit,*) "COOR:", this%natoms
    do i = 1, this%natoms
      write (unit,300) this%coor(:,i)
      300 format( 3( 1pe13.5e2: "," ) )

    end do ! for i
  end if

  ! Velocities
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  if ( allocated(this%vel) ) then
    write (unit,*) "VEL:", this%natoms
    do i = 1, this%natoms
      write (unit,400) this%vel(:,i)
      400 format( 3( 1pe13.5e2: "," ) )

    end do ! for i
  end if

  ! Forces
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  if ( allocated(this%force) ) then
    write (unit,*) "FORCE:", this%natoms
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
! trr_t class procedures

! Allocate NFRAMES of frames (only allocates frames)
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

! Allocate NFRAMES frames each with NATOMS of atoms
! * Use mode to allocate: X=Coordinates, V=Velocities, F=Forces
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

! Assigment(=) for trr_t class
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
  this%natoms    = other%natoms
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
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%nframes )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine trr_assign

! Open (& check) .TRR file and construct frame offsets table
integer function trr_open( this, file )
  implicit none
  class(trr_t)                :: this
  character(*), intent(in)    :: file

  ! Open file for reading
  trr_open = trr_open_file( this%xd, file, .true. )
  if ( trr_open /= xslibOK ) return

  ! Check file and get frame offsets
  trr_open = trr_check_file( this%xd, this%natoms, this%allframes, this%offsets )
  if ( trr_open /= xslibOK ) return

  ! Set current frame to 1
  this%current = 1

  ! Return success
  trr_open = xslibOK

  return
end function trr_open

! Close .TRR file and clean-up
integer function trr_close( this )
  implicit none
  class(trr_t)  :: this
  integer       :: stat

  ! Close file
  trr_close = trr_close_file( this%xd )
  if ( trr_close /= xslibOK ) return

  ! Reset private variables
  this%allFrames = 0
  this%natoms    = 0
  this%current   = 0

  ! Clean-up offsets table
  if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )

  ! Return success
  trr_close = xslibOK

  return
end function trr_close

! Moves UNIT to the specified FRAME.
! * If WHENCE is set to 0, the OFFSET is taken as an absolute value,
! * if set to 1, OFFSET is taken to be relative to the current position,
! * and if set to 2, relative to the end of the file.
integer function trr_fseek( this, offset, whence )
  implicit none
  class(trr_t)        :: this
  integer, intent(in) :: offset, whence
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
  integer             :: frame

  ! Check if pointer is associated
  if ( .not. associated(this%xd) ) then
    trr_fseek = xslibOPEN
    return
  end if

  ! Check if offsets table is allocated
  if ( .not. allocated(this%offsets) ) then
    trr_fseek = xslibNOMEM
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
    trr_fseek = xslibNR
    return
  end select

  ! Limit selection to bounds
  frame = max( frame, 1 )
  frame = min( frame, this%allframes+1 ) ! "+1" is EOF

  ! Move to frame
  trr_fseek = xdr_seek( this%xd, this%offsets(frame), SEEK_SET )
  if ( trr_fseek /= xslibOK ) return

  ! Update current frame
  this%current = frame

  ! Return success
  trr_fseek = xslibOK

  return
end function trr_fseek

! Retrieves the current position within an open file.
integer function trr_ftell( this )
  implicit none
  class(trr_t)        :: this
  trr_ftell = this%current
  return
end function trr_ftell

! Read next <N> frames in .TRR file
integer function trr_read_next( this, nframes )
  implicit none
  class(trr_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: frames, remaining, i

  ! Optional argument
  frames = merge( nframes, 1, present(nframes) )
  if ( frames < 0 ) frames = 1

  ! Calculate remaining frames
  remaining = this%allFrames-this%current+1

  ! Allocate frames
  this%nframes = min( frames, remaining )
  trr_read_next = this%allocate( this%nframes )
  if ( trr_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    trr_read_next = this%frame(i)%read( this%xd )
    if ( trr_read_next /= xslibOK ) return

    ! Increment current frame
    this%current = this%current+1

  end do ! for i

  ! Return success
  trr_read_next = xslibOK

  return
end function trr_read_next

! Skip next <N> frames in .TRR file
integer function trr_skip_next( this, nframes )
  implicit none
  class(trr_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: np

  ! Move to selected frame aka. "skip frames"
  np = merge( nframes, 1, present(nframes) )
  trr_skip_next = this%fseek( np, SEEK_CUR )

  return
end function trr_skip_next

! Read selected frame in .TRR file
integer function trr_read_frame( this, frame )
  implicit none
  class(trr_t)        :: this
  integer, intent(in) :: frame

  ! Move to selected frame
  if ( this%current /= frame ) then
    trr_read_frame = this%fseek( frame, SEEK_SET )
    if ( trr_read_frame /= 0 ) return
  end if

  ! Read frame
  trr_read_frame = this%read_next()

  return
end function trr_read_frame

! Read entire .TRR file
integer function trr_read( this, file, first, last, stride )
  implicit none
  class(trr_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: nfirst, nlast, nstride
  integer                       :: i, frame

  ! Open file.
  trr_read = this%open( file )
  if ( trr_read /= xslibOK ) return

  ! Optional parameters.
  nfirst  = merge( first, 1, present(first) )
  nlast   = merge( last, -1, present(last) )
  nstride = merge( stride, 1, present(stride) )

  ! Prevet doing stupid things
  if ( nfirst < 0 ) nfirst = 1
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Calculate number of frames and allocate data
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  trr_read = this%allocate( this%nframes )
  if ( trr_read /= xslibOK ) return

  ! Read frames
  do i = 1, this%nframes
    ! Actual frame number
    frame = nfirst + (i-1)*nstride

    ! Move to selected frame
    if ( this%current /= frame ) then
      trr_read = this%fseek( frame, SEEK_SET )
      if ( trr_read /= xslibOK ) return
    end if

    ! Read i-th frame
    trr_read = this%frame(i)%read( this%xd )
    if ( trr_read /= xslibOK ) return

    ! Update current frame
    this%current = this%current+1

  end do ! for i

  ! Close file
  trr_read = this%close()
  if ( trr_read /= xslibOK ) return

  ! Return success
  trr_read = xslibOK

  return
end function trr_read

! Write data in .TRR format to unit
integer function trr_write( this, file )
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

  ! Open file for writing
  trr_write = trr_open_file( xd, file, .false. )
  if ( trr_write /= xslibOK ) return

  ! Write each frame to output
  do i = 1, this%nframes
    trr_write = this%frame(i)%write( xd )
    if ( trr_write /= xslibOK ) return

  end do ! for i

  ! Close file
  trr_write = trr_close_file( xd )
  if ( trr_write /= xslibOK ) return

  ! Return success
  trr_write = xslibOK

  return
end function trr_write

! Write data in .TRR data to stdout, unit, or file in human-readable format.
integer function trr_dump( this, file, unit )
  use iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(trr_t)                        :: this
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: unit
  integer                             :: i, out, stat

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
  do i = 1, this%nframes
    trr_dump = this%frame(i)%dump( out )
    if ( trr_dump /= xslibOK ) return

  end do ! for i

  ! Close file if present
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      trr_dump = xslibCLOSE
      return
    end if
  end if

  ! Rerturn success
  trr_dump = xslibOK

  return
end function trr_dump

! -------------------------------------------------
! trr_t class utilities

! Return max. num. of atoms in currently opened file
integer function trr_getNatoms( this )
  implicit none
  class(trr_t)  :: this
  trr_getNatoms = this%natoms
  return
end function trr_getNatoms

! Return num. of frames in currently opened file
integer function trr_getNframes( this )
  implicit none
  class(trr_t)  :: this
  trr_getNframes = this%allframes
  return
end function trr_getNframes

! Return largest box in currently opened file
function trr_getBox( this ) result( box )
  implicit none
  class(trr_t)  :: this
  real          :: box(DIM,DIM)
  box(:,:) = this%box
  return
end function trr_getBox

! Return largest box in currently opened file
! function trr_getBox( this ) result( box )
!   use iso_fortran_env, only: REAL32, REAL64, INT64
!   implicit none
!   class(trr_t)    :: this
!   real            :: box(DIM,DIM), ibox(DIM,DIM)
!   integer         :: i, pos, stat
!   real            :: float
!   real(REAL64)    :: double, pvd(DIM,DIM)
!   integer(INT64)  :: framebytes
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
!   ! Move to start
!   stat = xdr_seek( this%xd, 0_INT64, SEEK_CUR )
!   if ( stat /= 0 ) return
!
!   ! loop over all frames
!   do i = 1, this%allframes
!
!     ! Move to frame
!     stat = this%fseek( i, SEEK_SET )
!     if ( stat /= xslibOK ) exit
!
!
!     if ( this%frame(i)%sh%bDouble == 1 ) then
!       ! Box size (double) use placeholder
!       if ( this%frame(i)%sh%box_size /= 0 ) then
!         if ( xdrfile_read_double( pvd, DIM*DIM, this%xd ) /= DIM*DIM ) exit
!         ibox(:,:) = real(pvd,REAL32)
!       end if
!
!     else ! float
!       if ( this%frame(i)%sh%box_size /= 0 ) then
!         if ( xdrfile_read_float( ibox, DIM*DIM, this%xd ) /= DIM*DIM ) exit
!       end if
!
!     end if
!
!     ! Keep the larger box
!     box(:,:) = max( box, ibox )
!
!   end do ! for i
!
!   ! Move back to starting position
!   stat = this%fseek( pos, SEEK_SET )
!
!   return
! end function trr_getBox

! -------------------------------------------------

! Open and check .TRR file
! integer function trr_open_file( xd, file, nframes, natoms, box )
!   use, intrinsic :: iso_fortran_env, only: INT64
!   use, intrinsic :: iso_c_binding, only: c_f_pointer, C_NULL_CHAR
!   implicit none
!   type(xdrfile), pointer, intent(out) :: xd
!   character(*), intent(in)            :: file
!   integer, intent(out)                :: nframes, natoms
!   real, intent(out)                   :: box(3,3)
!   type(trnheader)                     :: sh
!   real                                :: ibox(3,3)
!   logical                             :: exist
!
!   ! Check if file exists
!   inquire( FILE=trim(file), EXIST=exist )
!   if ( .not. exist ) then
!     trr_open_file = xslibFILENOTFOUND
!     return
!   end if
!
!   ! Open file
!   call c_f_pointer( xdrfile_open( trim(file)//C_NULL_CHAR, "r" ), xd )
!   if ( .not. associated(xd) ) then
!     trr_open_file = xslibOPEN
!     return
!   end if
!
!   ! Count and check all frames
!   nframes = 0
!   box(:,:) = 0.000
!   natoms = 0
!   do while( .true. )
!     ! Count number of atoms in file
!     trr_open_file = trr_header( xd, sh, .true. )
!     if ( trr_open_file == xslibENDOFFILE ) exit
!     if ( trr_open_file /= xslibOK ) return
!
!     ! Skip the rest of the frame
!     trr_open_file = trr_skip( xd, sh, ibox )
!     if ( trr_open_file /= xslibOK ) return
!
!     ! Update data
!     nframes = nframes+1
!     box(:,:) = max( box, ibox )
!     natoms = max( natoms, sh%natoms )
!
!   end do ! while
!
!   ! Rewind file
!   trr_open_file = xdr_seek( xd, 0_INT64, 0 )
!
!   return
! end function trr_open_file

! Skip .TRR data
! integer function trr_skip( xd, sh, box )
!   use iso_c_binding, only: c_sizeof
!   use iso_fortran_env, only: REAL32, INT64
!   implicit none
!   type(xdrfile), pointer, intent(in)  :: xd
!   type(trnheader), intent(in)         :: sh
!   real, intent(inout)                 :: box(DIM,DIM)
!   real(REAL64)                        :: double, pvd(DIM,DIM)
!   real                                :: float
!   integer(INT64)                      :: framebytes
!
!   ! Check xd pointer
!   if ( .not. associated(xd) ) then
!     trr_skip = xslibOPEN
!     return
!   end if
!
!   if ( sh%bDouble == 1 ) then
!
!     ! Box size double
!     if ( sh%box_size /= 0 ) then
!       if ( xdrfile_read_double( pvd, DIM*DIM, xd ) /= DIM*DIM ) then
!         trr_skip = xslibDOUBLE
!         return
!       end if
!       box(:,:) = real(pvd,REAL32)
!     end if
!
!     ! Calculate storage size:
!     framebytes = 0
!     if ( sh%vir_size /= 0 ) framebytes = framebytes + DIM*DIM*c_sizeof(double)
!     if ( sh%pres_size /= 0 ) framebytes = framebytes + DIM*DIM*c_sizeof(double)
!     if ( sh%x_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(double)
!     if ( sh%v_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(double)
!     if ( sh%f_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(double)
!
!     ! Skip size
!     trr_skip = xdr_seek( xd, framebytes, SEEK_CUR )
!     if ( trr_skip /= xslibOK ) return
!
!   else ! float
!
!     ! Box size double
!     if ( sh%box_size /= 0 ) then
!       if ( xdrfile_read_float( box, size(box), xd ) /= DIM*DIM ) then
!         trr_skip = xslibDOUBLE
!         return
!       end if
!     end if
!
!     ! Calculate storage size:
!     framebytes = 0
!     if ( sh%vir_size /= 0 ) framebytes = framebytes + DIM*DIM*c_sizeof(float)
!     if ( sh%pres_size /= 0 ) framebytes = framebytes + DIM*DIM*c_sizeof(float)
!     if ( sh%x_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(float)
!     if ( sh%v_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(float)
!     if ( sh%f_size /= 0 ) framebytes = framebytes + sh%natoms*DIM*c_sizeof(float)
!
!     ! Skip size
!     trr_skip = xdr_seek( xd, framebytes, SEEK_CUR )
!     if ( trr_skip /= xslibOK ) return
!
!   end if
!
!   ! Return success
!   trr_skip = xslibOK
!
!   return
! end function trr_skip

end module xslib_trrio
