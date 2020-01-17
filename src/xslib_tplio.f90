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

module xslib_tplio
  implicit none
  private
  public :: tpl_t

  ! Import error definitions
  include "fileio.h"

  ! NOTE: xslib_tplio is not completly transferable, but is dependable on
  ! * xslib_ndxio: ndx_t type definition, in order to use "makeNdx" utility.

  ! NOTE: Comment symbol is hidden in processLine utility

  ! NOTE: To understand how pointers are connected/structures see tpl_allocate().

  type, private :: tpl_mol
    integer               :: natoms = 0, nmol = 0
    integer, pointer      :: id(:) => null()
    character(3), pointer :: name(:) => null()
    real, pointer         :: pcharge(:) => null()
  end type tpl_mol

  type tpl_t
    real                        :: box(3) = 0.000   ! Simulation box side
    integer                     :: ntypes = 0  ! Number of different types
    type(tpl_mol), allocatable  :: type(:)  ! Type of molecule
    integer                     :: natoms = 0
    integer, pointer            :: id(:) => null()
    character(3), pointer       :: name(:) => null()
    real, pointer               :: pcharge(:) => null()
  contains
    procedure :: allocate => tpl_allocate
    procedure :: read => tpl_read
    procedure :: write => tpl_write
    procedure :: makeNdx => tpl_makeNdx
  end type tpl_t

contains

! Allocate tpl memory and connect pointers.
! * ntypes is defined by size(natoms) and natoms of each type by its value.
! * tpl%allocate( [8,4] ) => 2 molecules; 8 and 4 atoms each, respectively.
integer function tpl_allocate( this, natoms )
  implicit none
  class(tpl_t)        :: this
  integer, intent(in) :: natoms(:)
  integer             :: i, stat, offset

  ! Set number of types and total number of
  this%ntypes = size(natoms)
  this%natoms = sum(natoms(:))

  ! Allocate overall data
  if ( associated(this%name) ) deallocate( this%name, this%pcharge, this%id, STAT=stat )
  allocate ( this%name(this%natoms), this%pcharge(this%natoms), this%id(this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    tpl_allocate = xslibNOMEM
    return
  end if

  ! Initialize memory
  this%name(:)    = ""
  this%pcharge(:) = 0.000
  this%id(:)      = 0

  ! Allocate molecule types
  if ( allocated(this%type) ) deallocate( this%type, STAT=stat )
  allocate( this%type(this%ntypes), STAT=stat )
  if ( stat /= 0 ) then
    tpl_allocate = xslibNOMEM
    return
  end if

  ! Construct for eac type individually
  do i = 1, this%ntypes
    ! Number of atoms and offset
    this%type(i)%natoms = natoms(i)
    offset = sum(natoms(:i-1))+1

    ! Connect pointers
    this%type(i)%name => this%name(offset:offset+natoms(i)-1)
    this%type(i)%pcharge => this%pcharge(offset:offset+natoms(i)-1)
    this%type(i)%id => this%id(offset:offset+natoms(i)-1)

  end do

  ! Return success
  tpl_allocate = xslibOK

  return
end function tpl_allocate

! Read .TPL file.
integer function tpl_read( this, file )
  use, intrinsic :: iso_fortran_env
  implicit none
  class(tpl_t)              :: this
  character(*), intent(in)  :: file
  character(256)            :: buffer
  character(32)             :: keyword
  logical                   :: exist
  integer                   :: i, n, unit, offset, stat, int

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    tpl_read = xslibFILENOTFOUND
    return
  end if

  ! Open file
  open( NEWUNIT=unit, FILE=trim(file), STATUS="old", ACTION="read", IOSTAT=stat )
  if ( stat /= 0 ) then
    tpl_read = xslibOPEN
    return
  end if

  ! Initialize
  this%box(:) = 0.000
  this%ntypes = 0
  this%natoms = 0

  ! Count num, of "MOLECULE" sections and number of atoms
  stat = 0
  do while ( stat == 0 )
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat == IOSTAT_END ) exit

    ! Check for molecule directive
    buffer = processLine( buffer )
    if ( index(buffer,"MOLECULE") /= 0 ) then
      this%ntypes = this%ntypes+1
      ! Read number of atoms
      read (buffer,*,IOSTAT=stat) keyword, n
      if ( stat /= 0) then
        tpl_read = xslibINT
        return

      end if
      this%natoms = this%natoms+n

    end if
  end do ! while

  ! Rewind file
  rewind( unit, IOSTAT=stat )

  ! Allocate overall data
  if ( associated(this%name) ) deallocate( this%name, this%pcharge, this%id, STAT=stat )
  allocate ( this%name(this%natoms), this%pcharge(this%natoms), this%id(this%natoms), STAT=stat )
  if ( stat /= 0 ) then
    tpl_read = xslibNOMEM
    return
  end if

  ! Initialize the data
  this%name(:)    = ""
  this%pcharge(:) = 0.000
  this%id(:)      = 0

  ! Allocate data each "molecule"
  if ( allocated(this%type) ) deallocate( this%type, STAT=stat )
  allocate( this%type(this%ntypes), STAT=stat )
  if ( stat /= 0 ) then
    tpl_read = xslibNOMEM
    return
  end if

  ! Actual read loop
  n = 1 ! Index of current molecule.
  do while ( stat == 0 )
    ! Read and process line
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat == IOSTAT_END ) exit
    buffer = processLine( buffer )
    if ( verify(buffer, " ") == 0 ) cycle

    ! Check first word
    read (buffer,*) keyword

    select case ( keyword )
    ! LEGACY: Box side (optional)
    case( "SIDE" )
      read (buffer,*,IOSTAT=int) keyword, this%box(1:3)
      if ( this%box(2) == 0. ) this%box(2) = this%box(1)
      if ( this%box(3) == 0. ) this%box(3) = this%box(1)

    ! LEGACY: Number of molecules (optional)
    case( "MOLTYPE" )
      ! Completly ignore
      ! read (buffer,*,IOSTAT=int) keyword, i
      ! if ( n /= this%ntypes )

    ! Each molecule definition
    case( "MOLECULE" )
      ! Read num of atoms in molecule and number of molecules in system
      read (buffer,*,IOSTAT=stat) keyword, this%type(n)%natoms, this%type(n)%nmol

      ! Pointer offset in overall data.
      offset = sum(this%type(:n-1)%natoms)+1
      if ( offset+this%type(n)%natoms-1 > this%natoms ) then
        tpl_read = xslibNATOMS
        return
      end if

      ! Associate molecule pointers to overall data
      this%type(n)%name => this%name(offset:offset+this%type(n)%natoms-1)
      this%type(n)%pcharge => this%pcharge(offset:offset+this%type(n)%natoms-1)
      this%type(n)%id => this%id(offset:offset+this%type(n)%natoms-1)

      ! Read (pseudo-)atoms
      i = 1 ! Index of current atom
      do while ( i <= this%type(n)%natoms )
        ! Read line
        read (unit,"(a)") buffer
        buffer = processLine( buffer )
        if ( verify(buffer, " ") /= 0 ) then
          ! Read name, partial charge (optional), and ID (optional)
          read (buffer,*,IOSTAT=int) this%type(n)%name(i), this%type(n)%pcharge(i), this%type(n)%id(i)

          ! Only name realy needs to be defined
          if ( verify(this%type(n)%name(i)," ") == 0 ) then
            tpl_read = xslib3DX
            return
          end if

          ! To upper case
          this%type(n)%name(i) = toUpper( this%type(n)%name(i) )

          ! Increment atom iterator
          i = i+1

        end if
      end do ! while i

      ! End if all molecules were read
      if ( n == this%ntypes ) exit

      ! Move molecule iterator
      n = n+1

    end select
  end do ! while n

  ! Close unit
  close( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    tpl_read = xslibCLOSE
    return
  end if

  ! Return success
  tpl_read = xslibOK

  return
end function tpl_read

! Write .TPL to stdout, FILE or file UNIT (default is stdout).
integer function tpl_write( this, file, unit )
  use, intrinsic :: iso_fortran_env
  implicit none
  class(tpl_t)                        :: this
  character(*), intent(in), optional  :: file
  integer, intent(in), optional       :: unit
  integer                             :: i, n, stat, out
  logical                             :: opened
  character(9)                        :: action

  ! Select output method. Default is stdout
  if ( present(file) ) then
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      tpl_write = xslibFILENOTFOUND
      return
    end if
  else if ( present(unit) ) then
    inquire( UNIT=unit, OPENED=opened, ACTION=action )
    if ( .not. opened .or. index(action,"WRITE") == 0 ) then
      tpl_write = xslibFILENOTFOUND
      return
    end if
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Check if data is allocated
  if ( .not. allocated(this%type) ) then
    tpl_write = xslibNOMEM
    return
  end if

  ! Box side
  write (out,100) "SIDE", this%box(:)
  100 format( a, 3( 2x, f9.4 ) )

  ! Molecule types
  write (out,200) "MOLTYPE", this%ntypes
  200 format( a, 3x, i0 )

  ! Molecules one by one
  do n = 1, this%ntypes
    ! Number of molecules
    write (out,"(a,2(3x,i0))") "MOLECULE", this%type(n)%natoms, this%type(n)%nmol

    ! Write depending on what is relevant
    if ( any(this%type(n)%pcharge /= 0.000) .and. any(this%type(n)%id /= 0) ) then
      ! Write name, partial-charge, and id
      do i = 1, this%type(n)%natoms
        write (out,300) this%type(n)%name(i), this%type(n)%pcharge(i), this%type(n)%id(i)
      end do

    else if ( any(this%type(n)%pcharge /= 0.000) ) then
      ! Write only name, and partial-charge
      do i = 1, this%type(n)%natoms
        write (out,300) this%type(n)%name(i), this%type(n)%pcharge(i)
      end do

    else
      ! Write only name
      do i = 1, this%type(n)%natoms
        write (out,300) this%type(n)%name(i)
      end do

    end if

    300 format( a3: 2x, f8.4: 2x, i0 )

  end do ! for n

  ! Return success
  tpl_write = xslibOK

  return
end function tpl_write

! Create ndx file based on tpl entries. Only for non-zero ID
! NOTE: ndx_t definition can be found in "xslib_ndxio.f90"
integer function tpl_makeNdx( this, ndx )
  use xslib_ndxio, only: ndx_t
  implicit none
  class(tpl_t)              :: this
  type(ndx_t), intent(out)  :: ndx
  integer                   :: i, j, n, index
  character(32)             :: name

  ! Check if this is allocated
  if ( .not. allocated(this%type) ) then
    tpl_makeNdx = xslibNOMEM
    return
  end if

  ! Initialize
  ndx%ngroups = 1 ! System group is always present.

  ! Count number of unique index groups.
  index = 0 ! Index of current group.
  do while ( .true. )
    ! Find next unique id group; Exit if not found (index==huge)
    index = minval( this%id(:), MASK=( this%id(:) > index ) )
    if ( index == huge(index) ) exit
    ndx%ngroups = ndx%ngroups+1

  end do

  ! Allocate groups
  tpl_makeNdx = ndx%allocate( ndx%ngroups )
  if ( tpl_makeNdx /= xslibOK ) return

  ! Construct "System" group
  ndx%group(1)%title = "System"
  ndx%group(1)%natoms = sum( this%type(:)%natoms * this%type(:)%nmol )
  tpl_makeNdx = ndx%group(1)%allocate( ndx%group(1)%natoms )
  if ( tpl_makeNdx /= xslibOK ) return
  ndx%group(1)%loc(:) = [(i,i=1,ndx%group(1)%natoms)]

  ! Construct each group
  index = 0
  do n = 2, ndx%ngroups
    ! Find next unique id group; Exit if not found (i==huge)
    index = minval( this%id(:), MASK=( this%id(:) > index ) )
    if ( index == huge(index) ) exit

    ! Generate name
    write (name,"(a,x,i0)") "Group", index
    ndx%group(n)%title = trim(name)

    ! Count number of atoms in each molecule (n loop)
    ndx%group(n)%natoms = 0
    do i = 1, this%ntypes
      ndx%group(n)%natoms = ndx%group(n)%natoms + this%type(i)%nmol * count( this%type(i)%id(:) == index )
    end do

    ! Allocate data
    tpl_makeNdx = ndx%group(n)%allocate( ndx%group(n)%natoms )
    if ( tpl_makeNdx /= xslibOK ) return

    ! Construct the group using "System" as template.
    ndx%group(n)%loc(:) =  pack( ndx%group(1)%loc(:), &
    & MASK=[( (this%type(i)%id(:)==index, j=1,this%type(i)%nmol), i=1,this%ntypes )] )

  end do ! for n

  ! Return sucess
  tpl_makeNdx = xslibOK

  return
end function tpl_makeNdx

! -------------------------------------------------
! Utilities

! Put to upper case and remove comment line.
function processLine( string )
  implicit none
  character(:), allocatable :: processLine
  character(*), intent(in)  :: string
  character, parameter      :: cmt = "#"
  integer                   :: i
  i = index( string, cmt )
  if ( i == 0 ) then
    ! No comment
    processLine = toUpper( trim(string) )
  else if ( i == 1 ) then
    ! Whole line is comment
    processLine = ""
  else
    processLine = toUpper( trim(string(1:i-1)) )
  end if
  return
end function processLine

! Returns string in all upper case.
character(:) function toUpper( string )
  allocatable               :: toUpper
  character(*), intent(in)  :: string
  integer, parameter        :: offset=ichar("a")-ichar("A") ! ASCII offset
  integer                   :: i
  toUpper = trim(string)
  do i = 1, len(toUpper)
    ! Shift character by offset
    if (toUpper(i:i)>="a" .and. toUpper(i:i)<= "z") toUpper(i:i) = char(ichar(toUpper(i:i))-offset)
  end do
  return
end function toUpper

end module xslib_tplio
