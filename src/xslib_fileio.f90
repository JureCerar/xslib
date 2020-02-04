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
  use xslib_groio, only: gro_t
  use xslib_pdbio, only: pdb_t
  use xslib_xyzio, only: xyz_t
  use xslib_xtcio, only: xtc_t
  use xslib_trrio, only: trr_t
  use xslib_dcdio, only: dcd_t
  implicit none
  private
  public :: file_t

  ! Import error definitions
  include "fileio.h"

  ! NOTE: Coordinate and box units are dependant on the file type.
  ! *Default unit for `file_t` is [nm].
  ! .xyz = [A]
  ! .gro = [nm]
  ! .pdb = [A]
  ! .xtc = [nm]
  ! .trr = [nm]
  ! .dcd = [A]

  class(*), allocatable :: obj

  type file_t
    integer, private  :: allframes = 0, current = 0, first = 0, last = 0, stride = 0
    integer           :: natoms = 0
    real              :: box(3,3)
    real, allocatable :: coor(:,:)
  contains
    procedure :: open => file_open
    procedure :: close => file_close
    procedure :: read_next => file_read_next
    procedure :: skip_next => file_skip_next
    procedure :: getAllframes => file_getAllframes
    procedure :: getNatoms => file_getNatoms
    procedure :: getBox => file_getCubicBox
  end type file_t

contains

! Open file.
integer function file_open( this, file, first, last, stride )
  implicit none
  class(file_t)                 :: this
  character(*), intent(in)      :: file
  integer, optional, intent(in) :: first, last, stride
  integer                       :: stat
  logical                       :: exist
  character(:), allocatable     :: keyword

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    file_open = xslibFILENOTFOUND
    return
  end if

  ! Extract file extension
  keyword = toLower( extension(file) )

  ! Allocate polymorphic object.
  if ( allocated(obj) ) deallocate( obj, STAT=stat )
  select case ( trim(keyword) )
  case( "xyz" )
    allocate( xyz_t :: obj, STAT=stat )
  case( "gro" )
    allocate( gro_t :: obj, STAT=stat )
  case( "pdb" )
    allocate( pdb_t :: obj, STAT=stat )
  case( "xtc" )
    allocate( xtc_t :: obj, STAT=stat )
  case( "trr" )
    allocate( trr_t :: obj, STAT=stat )
  case( "dcd" )
    allocate( dcd_t :: obj, STAT=stat )
  case default
    file_open = xslibNR
    return
  end select

  ! Check memory allocation.
  if ( stat /= 0 ) then
    file_open = xslibNOMEM
    return
  end if

  ! ------------------------------------------

  ! Open file copy number of frames and skip to first frame
  select type( obj )
  class is ( xyz_t )
    file_open = obj%open(file)
    this%allframes = obj%getAllframes()

  class is ( gro_t )
    file_open = obj%open(file)
    this%allframes = obj%getAllframes()

  class is ( pdb_t )
    file_open = obj%open(file)
    this%allframes = obj%getAllframes()

  class is ( xtc_t )
    file_open = obj%open(file)
    this%allframes = obj%getAllframes()

  class is ( trr_t )
    file_open = obj%open(file)
    this%allframes = obj%getAllframes()

  class is ( dcd_t )
    file_open = obj%open(file)
    this%allframes = obj%getAllframes()

  class default
    file_open = xslibNR

  end select

  ! ------------------------------------------

  ! Load provided values
  this%first = merge( first, 1, present(first) )
  this%last = merge( last, -1, present(last) )
  this%stride = merge( stride, 1, present(stride) )

  ! Prevet doing stupid things.
  if ( this%first > this%allframes )  this%first = this%allframes
  if ( this%last < 0 .or. this%last > this%allframes ) this%last = this%allframes
  if ( this%stride < 1 ) this%stride = 1

  select type( obj )
  class is ( xyz_t )
    file_open = obj%skip_next( this%first-1 )
  class is ( gro_t )
    file_open = obj%skip_next( this%first-1 )
  class is ( pdb_t )
    file_open = obj%skip_next( this%first-1 )
  class is ( xtc_t )
    file_open = obj%skip_next( this%first-1 )
  class is ( trr_t )
    file_open = obj%skip_next( this%first-1 )
  class is ( dcd_t )
    file_open = obj%skip_next( this%first-1 )
  class default
    file_open = xslibNR
  end select

  ! Current frame number
  this%current = this%first-1

  return
end function file_open

! Read one frame from already opened file.
integer function file_read_next( this )
  implicit none
  class(file_t)   :: this
  integer         :: stat

  ! Check if file is opened
  if ( .not. allocated(obj) ) then
    file_read_next = xslibOPEN
    return
  end if

  ! Can we read any more files?
  if ( this%current >= this%last ) then
    file_read_next = xslibENDOFFILE
    return
  end if

  ! Act according to file type
  select type( obj )
  class is ( xyz_t )
    ! Read frame
    file_read_next = obj%read_next()
    if( file_read_next /= xslibOK ) return

    ! Copy frame properties
    this%natoms = obj%frame(1)%natoms
    this%box(:,:) = obj%frame(1)%box/10. ! UNITS [nm]

    ! Check if array is allocated and same size.
    if ( .not. allocated(this%coor) .or. size(this%coor) /= size(obj%frame(1)%coor) ) then
      if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
      allocate( this%coor(3,this%natoms), SOURCE=obj%frame(1)%coor, STAT=stat )
      if ( stat /= 0 ) then
        file_read_next = xslibNOMEM
        return
      end if
    else
      ! Just copy data.
      this%coor(:,:) = obj%frame(1)%coor
    end if

    ! Correct to [nm]
    this%coor(:,:) = this%coor(:,:)/10. ! UNITS [nm]

    ! Skip to next frame
    stat = obj%skip_next( this%stride-1 )
    this%current = this%current+this%stride

  class is ( gro_t )
    file_read_next = obj%read_next()
    if( file_read_next /= xslibOK ) return
    this%natoms = obj%frame(1)%natoms
    this%box(:,:) = obj%frame(1)%box
    if ( .not. allocated(this%coor) .or. size(this%coor) /= size(obj%frame(1)%coor) ) then
      if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
      allocate( this%coor(3,this%natoms), SOURCE=obj%frame(1)%coor, STAT=stat )
      if ( stat /= 0 ) then
        file_read_next = xslibNOMEM
        return
      end if
    else
      this%coor(:,:) = obj%frame(1)%coor
    end if
    stat = obj%skip_next( this%stride-1 )
    this%current = this%current+this%stride

  class is ( pdb_t )
    file_read_next = obj%read_next()
    if( file_read_next /= xslibOK ) return
    this%natoms = obj%frame(1)%natoms
    this%box(:,:) = obj%frame(1)%box/10. ! UNITS [nm]
    if ( .not. allocated(this%coor) .or. size(this%coor) /= size(obj%frame(1)%coor) ) then
      if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
      allocate( this%coor(3,this%natoms), SOURCE=obj%frame(1)%coor, STAT=stat )
      if ( stat /= 0 ) then
        file_read_next = xslibNOMEM
        return
      end if
    else
      this%coor(:,:) = obj%frame(1)%coor
    end if
    this%coor(:,:) = this%coor(:,:)/10. ! UNITS [nm]
    stat = obj%skip_next( this%stride-1 )
    this%current = this%current+this%stride

  class is ( xtc_t )
    file_read_next = obj%read_next()
    if( file_read_next /= xslibOK ) return
    this%natoms = obj%frame(1)%natoms
    this%box(:,:) = obj%frame(1)%box
    if ( .not. allocated(this%coor) .or. size(this%coor) /= size(obj%frame(1)%coor) ) then
      if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
      allocate( this%coor(3,this%natoms), SOURCE=obj%frame(1)%coor, STAT=stat )
      if ( stat /= 0 ) then
        file_read_next = xslibNOMEM
        return
      end if
    else
      this%coor(:,:) = obj%frame(1)%coor
    end if
    stat = obj%skip_next( this%stride-1 )
    this%current = this%current+this%stride

  class is ( trr_t )
    file_read_next = obj%read_next()
    if( file_read_next /= xslibOK ) return
    this%natoms = obj%frame(1)%natoms
    this%box(:,:) = obj%frame(1)%box
    if ( .not. allocated(this%coor) .or. size(this%coor) /= size(obj%frame(1)%coor) ) then
      if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
      allocate( this%coor(3,this%natoms), SOURCE=obj%frame(1)%coor, STAT=stat )
      if ( stat /= 0 ) then
        file_read_next = xslibNOMEM
        return
      end if
    else
      this%coor(:,:) = obj%frame(1)%coor
    end if
    stat = obj%skip_next( this%stride-1 )
    this%current = this%current+this%stride

  class is ( dcd_t )
    file_read_next = obj%read_next()
    if( file_read_next /= xslibOK ) return
    this%natoms = obj%frame(1)%natoms
    this%box(:,:) = obj%frame(1)%box/10. ! UNITS [nm]
    if ( .not. allocated(this%coor) .or. size(this%coor) /= size(obj%frame(1)%coor) ) then
      if ( allocated(this%coor) ) deallocate( this%coor, STAT=stat )
      allocate( this%coor(3,this%natoms), SOURCE=obj%frame(1)%coor, STAT=stat )
      if ( stat /= 0 ) then
        file_read_next = xslibNOMEM
        return
      end if
    else
      this%coor(:,:) = obj%frame(1)%coor
    end if
    this%coor(:,:) = this%coor(:,:)/10. ! UNITS [nm]
    stat = obj%skip_next( this%stride-1 )
    this%current = this%current+this%stride

  class default
    file_read_next = xslibOPEN

  end select

  return
end function file_read_next

! Read one frame from already opened file.
integer function file_skip_next( this )
  implicit none
  class(file_t) :: this
  integer       :: stride

  ! Check if file is opened
  if ( .not. allocated(obj) ) then
    file_skip_next = xslibOPEN
    return
  end if

  ! Can we read any more files?
  if ( this%current >= this%last ) then
    file_skip_next = xslibENDOFFILE
    return
  end if

  ! How many frames can we actualy skip
  stride = min(this%stride,this%last-this%current)

  ! Act according to file type
  select type( obj )
  class is ( xyz_t )
    ! Skip frame + move to next frame "(1+stride-1)"
    file_skip_next = obj%skip_next( stride )
    this%current = this%current+stride

  class is ( gro_t )
    file_skip_next = obj%skip_next( stride )
    this%current = this%current+stride

  class is ( pdb_t )
    file_skip_next = obj%skip_next( this%stride )
    this%current = this%current+this%stride

  class is ( xtc_t )
    file_skip_next = obj%skip_next( this%stride )
    this%current = this%current+this%stride

  class is ( trr_t )
    file_skip_next = obj%skip_next( this%stride )
    this%current = this%current+this%stride

  class is ( dcd_t )
    file_skip_next = obj%skip_next( this%stride )
    this%current = this%current+this%stride

  class default
    file_skip_next = xslibOPEN

  end select

  return
end function file_skip_next

! Close file.
integer function file_close( this )
  implicit none
  class(file_t) :: this

  ! Act according to file type
  select type( obj )
  class is ( xyz_t )
    file_close = obj%close()
  class is ( gro_t )
    file_close = obj%close()
  class is ( pdb_t )
    file_close = obj%close()
  class is ( xtc_t )
    file_close = obj%close()
  class is ( trr_t )
    file_close = obj%close()
  class is ( dcd_t )
    file_close = obj%close()
  class default
    ! Nothing to do
    file_close = xslibOK
  end select

  ! Deallocate data
  if ( allocated(obj) ) deallocate( obj, STAT=file_close )
  if ( file_close /= 0 ) file_close = xslibNOMEM

  return
end function file_close

! Returns the number of frames in file.
integer function file_getAllframes( this )
  implicit none
  class(file_t) :: this

  ! Act according to file type
  select type( obj )
  class is ( xyz_t )
    file_getAllframes = obj%getAllframes()
  class is ( gro_t  )
    file_getAllframes = obj%getAllframes()
  class is ( pdb_t )
    file_getAllframes = obj%getAllframes()
  class is ( xtc_t )
    file_getAllframes = obj%getAllframes()
  class is ( trr_t )
    file_getAllframes = obj%getAllframes()
  class is ( dcd_t )
    file_getAllframes = obj%getAllframes()
  class default
    file_getAllframes = 0
  end select

  return
end  function file_getAllframes

! Gets number of atoms in current frame (without changing position in file).
integer function file_getNatoms( this )
  implicit none
  class(file_t) :: this

  ! Act according to file type
  select type( obj )
  class is ( xyz_t )
    file_getNatoms = obj%getNatoms()
  class is ( gro_t )
    file_getNatoms = obj%getNatoms()
  class is ( pdb_t )
    file_getNatoms = obj%getNatoms()
  class is ( xtc_t )
    file_getNatoms = obj%getNatoms()
  class is ( trr_t )
    file_getNatoms = obj%getNatoms()
  class is ( dcd_t )
    file_getNatoms = obj%getNatoms()
  class default
    file_getNatoms = 0
  end select


  return
end function file_getNatoms

! Get box size of current frame (without changing position in file).
real function file_getCubicBox( this )
  implicit none
  class(file_t) :: this
  dimension     :: file_getCubicBox(3)

  ! Act according to file type
  select type( obj )
  class is ( xyz_t )
    file_getCubicBox(:) = obj%getBox()/10.
  class is ( gro_t )
    file_getCubicBox(:) = obj%getBox()
  class is ( pdb_t )
    file_getCubicBox(:) = obj%getBox()/10.
  class is ( xtc_t )
    file_getCubicBox(:) = obj%getBox()
  class is ( trr_t )
    file_getCubicBox(:) = obj%getBox()
  class is ( dcd_t )
    file_getCubicBox(:) = obj%getBox()/10.
  class default
    file_getCubicBox(:) = 0.000
  end select

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
