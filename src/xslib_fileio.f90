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
  use xdrfor
  use xslib_groio
  use xslib_pdbio
  use xslib_xyzio
  use xslib_xtcio
  use xslib_trrio
  use xslib_dcdio
  ! use cubio
  implicit none
  private
  public :: file_t

  ! Import error definitions
  include "fileio.h"

  ! TODO: Implement cube file.

  ! NOTE: Coordinate and box units are dependant on the file type.
  ! * Default units of "file_t" are [nm].

  type file_t
    type(xdrfile), pointer, private :: xdr => null()
    integer, private      :: unit
    integer, private      :: nframes = 0, maxatoms = 0
    real, private         :: maxbox(3,3) = 0.000
    character(3), private :: ext=""
    ! Public data
    integer               :: natoms = 0
    real                  :: box(3,3) = 0.000
    real, allocatable     :: coor(:,:)
  contains
    procedure :: allocate => file_allocate
    procedure :: open => file_open
    procedure :: read_next => file_read_next
    procedure :: skip_next => file_skip_next
    procedure :: dump => file_dump
    procedure :: close => file_close
    procedure :: getAllframes => file_getAllframes
    procedure :: getNatoms => file_getNatoms
    procedure :: getBox => file_getCubicBox
  end type file_t

contains

! -------------------------------------------------

! Comment
integer function file_allocate( this, natoms )
  implicit none
  class(file_t)       :: this
  integer, intent(in) :: natoms
  integer             :: stat

  ! Set natoms
  this%natoms = natoms

  ! Allocate only if not allocated or different size.
  if ( .not. allocated(this%coor) .or. size(this%coor,DIM=2) /= natoms ) then
    if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
    allocate( this%coor(3,this%natoms), STAT=stat )
    if ( stat /= 0 ) then
      file_allocate = xslibNOMEM
      return
    end if

  end if

  ! Return sucess
  file_allocate = xslibOK

  return
end function file_allocate

! Open and check file.
integer function file_open( this, file )
  use, intrinsic :: iso_fortran_env, only: INT64
  use, intrinsic :: iso_c_binding, only: c_f_pointer, C_NULL_CHAR
  implicit none
  class(file_t)             :: this
  character(*), intent(in)  :: file
  logical                   :: exist

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    file_open = xslibFILENOTFOUND
    return
  end if

  ! Extract and check file extension
  this%ext = toLower( extension(file) )
  if ( .not. any(trim(this%ext)==["xyz","gro","pdb","xtc","trr","dcd","cub"]) ) then
    file_open = xslibOPEN ! Unsuported file type
    return
  end if

  ! NOTE: Coordinate and box units are dependant on the file type.
  ! * Default units of "file_t" are [nm].
  ! .xyz = [A]
  ! .gro = [nm]
  ! .pdb = [A]
  ! .xtc = [nm]
  ! .trr = [nm]
  ! .dcd = [A]
  ! .cub = [bohr] or [A]

  ! Open file
  select case( this%ext )
  case( "xyz" )
    file_open = xyz_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
    this%maxbox(:,:) = this%maxbox * 0.1

  case( "gro" )
    file_open = gro_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )

  case( "pdb" )
    file_open = pdb_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
    this%maxbox(:,:) = this%maxbox * 0.1

  case( "xtc" )
    file_open = xtc_open_file( this%xdr, file, this%nframes, this%maxatoms, this%maxbox )

  case( "trr" )
    file_open = trr_open_file( this%xdr, file, this%nframes, this%maxatoms, this%maxbox )

  case( "dcd" )
    file_open = dcd_open_file( this%unit, file, this%nframes, this%maxatoms, this%maxbox )
    this%maxbox(:,:) = this%maxbox * 0.1

  case( "cub" )
    error stop

  case default
    file_open = xslibNR !?

  end select

  return
end function file_open

! Read one frame from file.
integer function file_read_next( this )
  implicit none
  class(file_t)   :: this
  type(trnheader) :: sh
  character(32)   :: string
  real            :: float
  integer         :: int

  ! NOTE: Coordinate and box units are dependant on the file type.
  ! * Default units of "file_t" are [nm].
  ! .xyz = [A]
  ! .gro = [nm]
  ! .pdb = [A]
  ! .dcd = [A]
  ! .xtc = [nm]
  ! .trr = [nm]
  ! .cub = [bohr] or [A] ! what the hell?

  select case( this%ext )
  case( "xyz" )
    file_read_next = xyz_read_header( this%unit, this%natoms, string, this%box )
    if ( file_read_next /= xslibOK ) return
    file_read_next = this%allocate( this%natoms )
    if ( file_read_next /= xslibOK ) return
    file_read_next = xyz_read_coor( this%unit, this%natoms, this%coor )
    if ( file_read_next /= xslibOK ) return
    ! Transform to default units.
    this%coor(:,:) = this%coor * 0.1 ! to [nm].
    this%box(:,:) = this%box * 0.1 ! to [nm].

  case( "gro" )
    file_read_next = gro_read_header( this%unit, this%natoms, string, float )
    if ( file_read_next /= xslibOK ) return
    file_read_next = this%allocate( this%natoms )
    if ( file_read_next /= xslibOK ) return
    file_read_next = gro_read_coor( this%unit, this%natoms, this%box, this%coor )
    if ( file_read_next /= xslibOK ) return

  case( "pdb" )
    file_read_next = pdb_count( this%unit, this%natoms )
    if ( file_read_next /= xslibOK ) return
    file_read_next = this%allocate( this%natoms )
    if ( file_read_next /= xslibOK ) return
    file_read_next = pdb_read_coor( this%unit, this%natoms, this%box, this%coor )
    if ( file_read_next /= xslibOK ) return
    ! Transform to default units.
    this%coor(:,:) = this%coor * 0.1 ! to [nm].
    this%box(:,:) = this%box * 0.1 ! to [nm].

  case( "xtc" )
    file_read_next = xtc_header( this%xdr, this%natoms, int, float )
    if ( file_read_next /= xslibOK ) return
    file_read_next = this%allocate( this%natoms )
    if ( file_read_next /= xslibOK ) return
    file_read_next = xtc_data( this%xdr, this%natoms, this%box, this%coor, float, .true. )
    if ( file_read_next /= xslibOK ) return

  case( "trr" )
    file_read_next = trr_header( this%xdr, sh, .true. )
    if ( file_read_next /= xslibOK ) return
    this%natoms = sh%natoms
    file_read_next = this%allocate( this%natoms )
    if ( file_read_next /= xslibOK ) return
    file_read_next = trr_coor( this%xdr, sh, this%box, this%coor )
    if ( file_read_next /= xslibOK ) return

  case( "dcd" )
    ! Header was read uppon open.
    file_read_next = this%allocate( this%maxatoms )
    if ( file_read_next /= xslibOK ) return
    file_read_next = dcd_read_data( this%unit, this%natoms, this%box, this%coor )
    if ( file_read_next /= xslibOK ) return
    ! Transform to default units.
    this%coor(:,:) = this%coor * 0.1 ! to [nm].
    this%box(:,:) = this%box * 0.1 ! to [nm].

  case( "cub" )
    error stop

  case default
    file_read_next = xslibOPEN ! No file is opened

  end select

  return
end function file_read_next

! Skip one frame from file.
integer function file_skip_next( this )
  implicit none
  class(file_t)   :: this
  type(trnheader) :: sh
  character(32)   :: string
  real            :: float, box(3,3)
  integer         :: natoms, int

  ! Read header
  select case( this%ext )
  case( "xyz" )
    file_skip_next = xyz_read_header( this%unit, natoms, string, box )
    if ( file_skip_next /= xslibOK ) return
    file_skip_next = xyz_skip_data( this%unit, natoms )
    if ( file_skip_next /= xslibOK ) return

  case( "gro" )
    file_skip_next = gro_read_header( this%unit, natoms, string, float )
    if ( file_skip_next /= xslibOK ) return
    file_skip_next = gro_skip_data( this%unit, natoms, box )
    if ( file_skip_next /= xslibOK ) return

  case( "pdb" )
    file_skip_next = pdb_count( this%unit, natoms )
    if ( file_skip_next /= xslibOK ) return
    file_skip_next = pdb_skip_data( this%unit, box )
    if ( file_skip_next /= xslibOK ) return

  case( "xtc" )
    file_skip_next = xtc_header( this%xdr, natoms, int, float )
    if ( file_skip_next /= xslibOK ) return
    file_skip_next = xtc_skip( this%xdr, natoms, box )
    if ( file_skip_next /= xslibOK ) return

  case( "trr" )
    file_skip_next = trr_header( this%xdr, sh, .true. )
    if ( file_skip_next /= xslibOK ) return
    file_skip_next = trr_skip( this%xdr, sh, box )
    if ( file_skip_next /= xslibOK ) return

  case( "dcd" )
    file_skip_next = dcd_skip_data( this%unit, this%maxatoms, box )
    if ( file_skip_next /= xslibOK ) return

  case( "cub" )
    error stop

  case default
    file_skip_next = xslibOPEN ! No file is opened

  end select

  return
end function file_skip_next

! Dump contents to unit if present
integer function file_dump( this, file, unit )
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(file_t)                       :: this
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: unit
  integer                             :: i, out, stat
  logical                             :: opened
  character(9)                        :: action

  ! Select default output unit
  if ( present(file) ) then
    ! Check if file can be opened.
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      file_dump = xslibFILENOTFOUND
      return
    end if

  else if ( present(unit) ) then
    ! Check if unit is opened
    inquire( UNIT=unit, OPENED=opened, ACTION=action )
    if ( .not. opened .or. index(action,"WRITE") == 0 ) then
      file_dump = xslibOPEN
      return
    end if
    out = unit

  else
    ! Output to stdout
    out = OUTPUT_UNIT

  end if

  ! Dump simulation box
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  do i = 1, 3
    write (out,100) this%box(:,i)
    100 format( 5x, 3( 1pe13.5e2: "," ) )
  end do ! for i

  ! Dump coordinate data if allocated
  ! * { 7.75000e-01,  9.99000e-01,  2.95000e-01}
  if ( allocated(this%coor) ) then
    do i = 1, this%natoms
      write (out,200) this%coor(:,i)
      200 format( 3( 1pe13.5e2: "," ) )
    end do ! for i
  end if

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

! Close file.
integer function file_close( this )
  implicit none
  class(file_t) :: this

  select case( this%ext )
  case( "xyz" )
    file_close = xyz_close_file( this%unit )

  case( "gro" )
    file_close = gro_close_file( this%unit )

  case( "pdb" )
    file_close = pdb_close_file( this%unit )

  case( "xtc" )
    file_close = xtc_close_file( this%xdr )

  case( "trr" )
    file_close = trr_close_file( this%xdr )

  case( "dcd" )
    file_close = dcd_close_file( this%unit )

  case( "cub" )
    error stop

  case default
    file_close = xslibCLOSE

  end select

  ! Clean extension
  this%ext = ""

  return
end function file_close

! -------------------------------------------------

! Returns the number of frames in file.
integer function file_getAllframes( this )
  implicit none
  class(file_t) :: this
  file_getAllframes = this%nframes
  return
end  function file_getAllframes

! Gets number of atoms in current frame (without changing position in file).
integer function file_getNatoms( this )
  implicit none
  class(file_t) :: this
  file_getNatoms = this%maxatoms
  return
end function file_getNatoms

! Get box size of current frame (without changing position in file).
function file_getCubicBox( this )
  implicit none
  class(file_t) :: this
  real          :: file_getCubicBox(3)
  file_getCubicBox(:) = [this%maxbox(1,1),this%maxbox(2,2),this%maxbox(3,3)]
  return
end function file_getCubicBox

! -------------------------------------------------
! Utilities

! Return string in all lower case.
character(:) function toLower( string )
  allocatable               :: toLower
  character(*), intent(in)  :: string
  integer, parameter        :: offset=ichar("a")-ichar("A") ! ASCII offset
  integer                   :: i
  toLower = trim(string)
  do i = 1, len(toLower)
    ! Shift character by offset
    if (toLower(i:i)>="A" .and. toLower(i:i)<= "Z") toLower(i:i) = char(ichar(toLower(i:i))+offset)
  end do
  return
end function toLower

! Returns extension of file. Eg. "/path/to/file.txt" = "txt"
character(:) function extension( file )
  implicit none
  allocatable               :: extension
  character(*), intent(in)  :: file
  integer                   :: i
  i = index(file,".",BACK=.true.)
  if ( i == 0 .or. i == len_trim(file) ) then
    extension = ""
  else
    extension = file(i+1:len_trim(file))
  end if
  return
end function extension

end module xslib_fileio
