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

module xslib_cubio
  implicit none
  private
  public :: cub_t, cub_atom

  ! NOTE: Grid data has 'inverted' notation: grid(z,y,x)

  ! TODO: Implement ANGSTROM/BOHR units
  ! > If the sign of the number of voxels in a dimension is positive then the units are Bohr, if negative then Angstroms.
  ! Note to self: Who's fucking idea was this ???

  ! Import error definitions
  include "fileio.h"

  ! Global constants
  integer, parameter :: DIM = 3 ! Default dimension
  integer, parameter :: CUB_MAX_LEN = 256 ! Max buffer and title length

  type cub_atom
    integer :: z = 0            ! Atomic number
    real    :: pcharge = 0.000  ! Partial charge
    real    :: coor(DIM) = 0.000  ! Coordinates
  end type cub_atom

  type cub_t
    character(CUB_MAX_LEN)      :: comment(2) = ""        ! Comments
    integer                     :: natoms = 0             ! Num. of atoms
    type(cub_atom), allocatable :: atom(:)                ! Atom definition(s)
    real                        :: origin(DIM) = 0.000    ! Origin of coordinates
    real                        :: voxel(DIM,DIM) = 0.000 ! Voxel size
    integer                     :: nx = 0, ny = 0, nz = 0 ! Grid size
    real, allocatable           :: grid(:,:,:)            ! GRID(Z,Y,X)
  contains
    procedure :: assign => cube_assign
    generic   :: assignment(=) => assign
    procedure :: allocate => cube_allocate
    procedure :: read => cube_read
    procedure :: write => cube_write
  end type cub_t

  ! Cube file definition
  ! SOURCE: http://paulbourke.net/dataformats/cube/
  !
  ! CPMD CUBE FILE.
  !  OUTER LOOP: X, MIDDLE LOOP: Y, INNER LOOP: Z
  !     3    0.000000    0.000000    0.000000
  !    40    0.283459    0.000000    0.000000
  !    40    0.000000    0.283459    0.000000
  !    40    0.000000    0.000000    0.283459
  !     8    0.000000    5.570575    5.669178    5.593517
  !     1    0.000000    5.562867    5.669178    7.428055
  !     1    0.000000    7.340606    5.669178    5.111259
  !  -0.25568E-04  0.59213E-05  0.81068E-05  0.10868E-04  0.11313E-04  0.35999E-05
  !       :             :             :           :            :            :
  !       :             :             :           :            :            :
  !       :             :             :           :            :            :
  !         In this case there will be 40 x 40 x 40 floating point values
  !       :             :             :           :            :            :
  !       :             :             :           :            :            :
  !       :             :             :           :            :            :

contains

! -------------------------------------------------
! Class independat (low-level) routines

! Comment
integer function cube_header_read( unit, comment, natoms, origin, nx, ny, nz, voxel )
  implicit none
  integer, intent(in)       :: unit
  character(*), intent(out) :: comment(2)
  integer, intent(out)      :: natoms, nx, ny, nz
  real, intent(out)         :: origin(DIM), voxel(DIM,DIM)
  integer                   :: stat
  logical                   :: opened
  character(9)              :: action

  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    cube_header_read = xslibOPEN
    return
  end if

  ! Comment lines
  read (unit,"(a)",IOSTAT=stat) comment(1)
  read (unit,"(a)",IOSTAT=stat) comment(2)

  ! Number of atoms and origin
  read (unit,*,IOSTAT=stat) natoms, origin(:)

  ! Number of voxels and voxel
  read (unit,*,IOSTAT=stat) nx, voxel(:,1)
  read (unit,*,IOSTAT=stat) ny, voxel(:,2)
  read (unit,*,IOSTAT=stat) nz, voxel(:,3)

  ! Return success
  cube_header_read = xslibOK

  return
end function cube_header_read

! Comment
integer function cube_atoms_read( unit, z, pcharge, coor )
  implicit none
  integer, intent(in)   :: unit
  integer, intent(out)  :: z
  real, intent(out)     :: pcharge, coor(DIM)
  integer               :: stat

  ! Read atom data
  read (unit,*,IOSTAT=stat) z, pcharge, coor(:)
  if ( stat /= 0 ) then
    cube_atoms_read = xslib3DX
    return
  end if

  ! Return success
  cube_atoms_read = xslibOK

  return
end function cube_atoms_read

! Comment
integer function cube_data_read( unit, nx, ny, nz, grid )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in) :: unit, nx, ny, nz
  real, intent(out)   :: grid(:,:,:)
  integer             :: stat

  ! Check memory size
  if ( size(grid) /= abs(nx*ny*nz) ) then
    cube_data_read = xslibNOMEM
    return
  end if

  ! Read data as tensor
  read (unit,*,IOSTAT=stat) grid(:,:,:)
  if ( stat /= 0 ) then
    cube_data_read = merge( xslibENDOFFILE, xslib3DX, stat == IOSTAT_END )
    return
  end if

  ! Return sucess
  cube_data_read = xslibOK

  return
end function cube_data_read

! Comment
integer function cube_header_write( unit, comment, natoms, origin, nx, ny, nz, voxel )
  implicit none
  integer, intent(in)       :: unit
  character(*), intent(in)  :: comment(2)
  integer, intent(in)       :: natoms, nx, ny, nz
  real, intent(in)          :: origin(3), voxel(3,3)
  logical                   :: opened
  character(9)              :: action

  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    cube_header_write = xslibOPEN
    return
  end if

  ! Comment lines
  write (unit,"(a)") trim(comment(1))
  write (unit,"(a)") trim(comment(2))

  ! Number of atoms and origin
  write (unit,100) natoms, origin(:)

  ! Number of voxels and voxel dim
  write (unit,100) nx, voxel(:,1)
  write (unit,100) ny, voxel(:,2)
  write (unit,100) nz, voxel(:,3)

  100 format( i5, 3(f12.6) )

  ! Return success
  cube_header_write = xslibOK

  return
end function cube_header_write

! Comment
integer function cube_atoms_write( unit, z, pcharge, coor )
  implicit none
  integer, intent(in)   :: unit
  integer, intent(in)   :: z
  real, intent(in)      :: pcharge, coor(3)

  write (unit,100) z, pcharge, coor(:)
  100 format( i5, 4(f12.6) )

  ! Return success
  cube_atoms_write = xslibOK

  return
end function cube_atoms_write

! Comment
integer function cube_data_write( unit, nx, ny, nz, grid )
  implicit none
  integer, intent(in) :: unit, nx, ny, nz
  real, intent(in)    :: grid(:,:,:)
  integer             :: ix, iy, iz

  ! Check size
  if ( size(grid) /= abs(nx*ny*nz) ) then
    cube_data_write = xslibNOMEM
    return
  end if

  ! Read data as tensor
  do ix = 1, abs(nx)
    do iy = 1, abs(ny)
      do iz = 1, abs(nz)
        write (unit,100) grid(iz,iy,ix)
        100 format( 1pe13.5, $ ) ! No new line

        if ( mod(iz,6) == 0 ) write (unit,*) ! New line

      end do ! for k
      write (unit,*) ! New line

    end do ! for j
  end do ! for i

  ! Return sucess
  cube_data_write = xslibOK

  return
end function cube_data_write

! -------------------------------------------------
! Cube atom type routines

! -------------------------------------------------
! Cube routines

! Comment
subroutine cube_assign( this, other )
  implicit none
  class(cub_t), intent(inout) :: this
  class(cub_t), intent(in)    :: other
  integer                     :: stat

  ! Copy header
  this%comment(:) = other%comment
  this%origin(:) = other%origin
  this%voxel(:,:) = other%voxel

  ! Copy atom data
  this%natoms = other%natoms
  if ( allocated(other%atom) ) then
    if ( allocated(this%atom) ) deallocate( this%atom, STAT=stat )
    allocate( this%atom(other%natoms), STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%atom(:) = other%atom

  end if

  ! Copy grid data
  this%nx = other%nx
  this%ny = other%ny
  this%nz = other%nz
  if ( allocated(other%grid) ) then
    if ( allocated(this%grid) ) deallocate( this%grid, STAT=stat )
    allocate( this%grid(other%nz,other%ny,other%nx), STAT=stat )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%grid(:,:,:) = other%grid

  end if

  return
end subroutine cube_assign

! Comment
integer function cube_allocate( this, nx, ny, nz, natoms )
  implicit none
  class(cub_t)                  :: this
  integer, intent(in)           :: nx, ny, nz
  integer, intent(in), optional :: natoms
  integer                       :: stat

  ! Set grid size data
  this%nx = nx
  this%ny = ny
  this%nz = nz

  ! allocate grid
  if ( allocated(this%grid) ) deallocate( this%grid, STAT=stat )
  allocate( this%grid(nz,ny,nx), STAT=stat )
  if ( stat /= 0 ) then
    cube_allocate = xslibNOMEM
    return
  end if

  ! Allocate atoms
  if ( present(natoms) ) then
    this%natoms = natoms
    if ( allocated(this%atom) ) deallocate( this%atom, STAT=stat )
    allocate( this%atom(natoms), STAT=stat )
    if ( stat /= 0 ) then
      cube_allocate = xslibNOMEM
      return
    end if
  end if

  ! Return success
  cube_allocate = xslibOK

  return
end function cube_allocate

! Comment
integer function cube_read( this, file )
  implicit none
  class(cub_t)              :: this
  character(*), intent(in)  :: file
  integer                   :: i, unit, stat

  ! Open file
  open( NEWUNIT=unit, file=trim(file), ACTION="read", STATUS="old", IOSTAT=stat )
  if ( stat /= 0 ) then
    cube_read = xslibFILENOTFOUND
    return
  end if

  ! Read header
  cube_read = cube_header_read( unit, this%comment, this%natoms, this%origin, this%nx, this%ny, this%nz, this%voxel )
  if ( cube_read /= xslibOK ) return

  ! Allocate data
  cube_read = this%allocate( this%nx, this%ny, this%nz, this%natoms )
  if ( cube_read /= xslibOK ) return

  ! Read atoms
  do i = 1, this%natoms
    cube_read = cube_atoms_read( unit, this%atom(i)%z, this%atom(i)%pcharge, this%atom(i)%coor )
    if ( cube_read /= xslibOK ) return
  end do

  ! Read data
  cube_read = cube_data_read( unit, this%nx, this%ny, this%nz, this%grid )
  if ( cube_read /= xslibOK ) return

  return
end function cube_read

! Comment
integer function cube_write( this, file, unit )
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(cub_t)                        :: this
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: unit
  integer                             :: i, out, stat

  ! Select output method
  if ( present(file) ) then
    open( NEWUNIT=out, file=trim(file), ACTION="write", STATUS="unknown", IOSTAT=stat )
    if ( stat /= 0 ) then
      cube_write = xslibOPEN
      return
    end if
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Check if data is allocated
  if ( .not. allocated(this%grid) ) then
    cube_write = xslibNOMEM
    return
  end if

  ! Write header
  cube_write = cube_header_write( out, this%comment, this%natoms, this%origin, this%nx, this%ny, this%nz, this%voxel )
  if ( cube_write /= xslibOK ) return

  ! Write atoms
  if ( allocated(this%atom) ) then
    do i = 1, this%natoms
      cube_write = cube_atoms_write( out, this%atom(i)%z, this%atom(i)%pcharge, this%atom(i)%coor )
      if ( cube_write /= xslibOK ) return
    end do
  end if

  ! Write data
  cube_write = cube_data_write( out, this%nx, this%ny, this%nz, this%grid )
  if ( cube_write /= xslibOK ) return

  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) cube_write = xslibCLOSE
  end if

  return
end function cube_write

! -------------------------------------------------
! Utility

! ! Convert Bohr to angstroms and back
! ! Bohr =  5.29177210903(80)E−11 m
! ! Angs =  1.000E-10 m
! function angs2bohr( angs ) result( bohr )
!   implicit none
!   real :: angs, bohr
!   bohr = angs * 1.8897259886
!   return
! end function angs2bohr
!
! function bohr2angs( bohr ) result( angs )
!   implicit none
!   real :: bohr, angs
!   angs = bohr * 0.529177249
!   return
! end function bohr2angs

end module xslib_cubio
