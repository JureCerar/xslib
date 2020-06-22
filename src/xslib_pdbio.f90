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

module xslib_pdbio
  implicit none
  private
  public :: pdb_t, pdb_frame
  public :: pdb_open_file, pdb_close_file, pdb_count, pdb_read_data
  public :: pdb_read_coor, pdb_skip_data, pdb_write_data

  ! Import error definitions
  include "fileio.h"

  ! .PDB data format
  ! type    = record_type
  ! atomn   = atom_serial_number
  ! atomnm  = atom_name
  ! altloc  = alternate_location_indicator
  ! resnm   = residue_name
  ! ch      = chain_identifier
  ! resn    = residue_sequence_number
  ! resic   = residue_insertion_code
  ! coor    = x, y, z coordinates
  ! occup   = occupancy
  ! bfac    = beta_factor
  ! elem    = element_name
  ! charge  = element_partial_charge
  ! segnm   = segment_identifier

  type pdb_frame
    real                      :: box(3,3) = 0.000
    integer                   :: natoms = 0
    character(6), allocatable :: type(:), atomnm(:), altloc(:), resnm(:), ch(:), resic(:), segnm(:), elem(:), charge(:)
    integer, allocatable      :: atomn(:), resn(:)
    real, allocatable         :: coor(:,:), occup(:), bfac(:)
  contains
    procedure :: allocate => pdb_frame_allocate
    procedure :: assign => pdb_frame_assign
    generic   :: assignment(=) => assign
    procedure :: copy => pdb_frame_copy
    procedure :: read => pdb_frame_read
    procedure :: write => pdb_frame_write
  end type pdb_frame

  type pdb_t
    integer, private              :: unit = 0, natoms = 0, allframes = 0, remaining = 0
    real, private                 :: box(3,3) = 0.000
    integer                       :: nframes = 0
    type(pdb_frame), allocatable  :: frame(:)
  contains
    procedure :: pdb_allocate, pdb_allocate_all
    generic   :: allocate => pdb_allocate, pdb_allocate_all
    procedure :: assign => pdb_assign
    generic   :: assignment(=) => assign
    procedure :: open => pdb_open
    procedure :: close => pdb_close
    procedure :: read => pdb_read
    procedure :: read_next => pdb_read_next
    procedure :: skip_next => pdb_skip_next
    procedure :: write => pdb_write
    procedure :: getAllframes => pdb_getAllframes
    procedure :: getNatoms => pdb_getNatoms
    procedure :: getBox => pdb_getCubicBox
  end type pdb_t

contains

! -------------------------------------------------
! Class independat routines

! Open and check .pdb file.
integer function pdb_open_file( unit, file, nframes, natoms, box )
  implicit none
  integer, intent(out)      :: unit
  character(*), intent(in)  :: file
  integer, intent(out)      :: nframes, natoms
  real, intent(out)         :: box(3,3)
  real                      :: ibox(3,3)
  integer                   :: inatoms, stat
  logical                   :: exist

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    pdb_open_file = xslibFILENOTFOUND
    return
  end if

  ! Open file
  open ( NEWUNIT=unit, FILE=trim(file), STATUS="old", ACTION="read", IOSTAT=stat )
  if ( stat /= 0 ) then
    pdb_open_file = xslibOPEN
    return
  end if

  ! Count and check all frames
  nframes = 0
  box(:,:) = 0.000
  natoms = 0
  do while( .true. )
    ! Count number of atoms in file
    pdb_open_file = pdb_count( unit, inatoms )
    if ( pdb_open_file == xslibENDOFFILE ) exit
    if ( pdb_open_file /= xslibOK ) return

    ! Skip the rest of the frame
    pdb_open_file = pdb_skip_data( unit, ibox )
    if ( pdb_open_file /= xslibOK ) return

    ! Update data
    nframes = nframes+1
    box(:,:) = max( box, ibox )
    natoms = max( natoms, inatoms )

  end do ! while

  ! Rewind file
  rewind ( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    pdb_open_file = xslibOPEN
    return
  end if

  ! Return sucess
  pdb_open_file = xslibOK

  return
end function pdb_open_file

! Count occurance of "ATOM" and "HETATOM" keywods in frame.
integer function pdb_count( unit, natoms )
  use iso_fortran_env, only: INT64, IOSTAT_END
  implicit none
  integer, intent(in)   :: unit
  integer, intent(out)  :: natoms
  integer(INT64)        :: offset
  character(6)          :: keyword
  integer               :: stat
  logical               :: opened
  character(9)          :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    pdb_count = xslibOPEN
    return
  end if

  ! Get current file position
  offset = ftell( unit )

  ! Count occurance of keywords "ATOM" and "HETATM"
  natoms = 0
  do while ( .true. )
    read (unit,"(a)",IOSTAT=stat) keyword
    if ( stat == IOSTAT_END ) then
      pdb_count = xslibENDOFFILE
      return
    end if
    if ( index(keyword,"ATOM") == 1 .or. index(keyword,"HETATM") == 1 ) then
      natoms = natoms+1
    else if ( index(keyword,"END") == 1 ) then
      exit
    end if
  end do ! while

  ! Return to original position
  call fseek( unit, offset, 0, STATUS=stat )
  if ( stat /= 0 ) then
    pdb_count = xslibHEADER
    return
  end if

  ! Return success
  pdb_count = xslibOK

  return
end function pdb_count

! Read .pdb data
integer function pdb_read_data( unit, natoms, type, atomn, atomnm, altloc, resnm, ch, &
& resn, resic, occup, bfac, segnm, elem, charge, box, coor )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit
  integer, intent(in)       :: natoms
  character(6), intent(out) :: type(natoms), atomnm(natoms), altloc(natoms), resnm(natoms)
  character(6), intent(out) :: ch(natoms), resic(natoms), segnm(natoms), elem(natoms), charge(natoms)
  integer, intent(out)      :: atomn(natoms), resn(natoms)
  real, intent(out)         :: occup(natoms), bfac(natoms)
  real, intent(out)         :: box(3,3)
  real, intent(out)         :: coor(3,natoms)
  integer                   :: i, stat
  logical                   :: opened
  character(128)            :: buffer
  character(6)              :: keyword
  character(9)              :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    pdb_read_data = xslibOPEN
    return
  end if

  ! Initialize
  box(:,:) = 0.000

  ! Read all lines with "ATOM" or "HETATOM" keywords
  i = 1 ! Current atom index
  do while( .true. )
    ! Read buffer line
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat /= 0 ) then
      pdb_read_data = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
      return
    end if

    ! Process keyword
    keyword = buffer(1:6)
    if ( index(keyword,"ATOM") == 1 .or. index(keyword,"HETATM") == 1 ) then
      ! Check if index is below limit
      if ( i > natoms ) then
        pdb_read_data = xslibNATOMS
        return
      end if

      ! Read PDB data
      read (buffer,100,IOSTAT=stat) type(i), atomn(i), atomnm(i), altloc(i), resnm(i), ch(i), resn(i), &
      &   resic(i), coor(:,i), occup(i), bfac(i), segnm(i), elem(i), charge(i)
      100 format( a6, i5, x, a4, a1, a3, x, a1, i4, a1, 3x, 3f8.3, 2f6.2, 6x, a4, a2, a2 )
      if ( stat /= 0 ) then
        pdb_read_data = xslib3DX
        return
      end if

      ! Increment iterator
      i = i+1

    else if ( index(keyword,"CRYST1") == 1 ) then
      ! TODO: I am too lazy to implement any other box definition options.
      ! * Just use cubic box. Please.
      read (buffer,"(6x,3f9.3)",IOSTAT=stat) box(1,1), box(2,2), box(3,3)
      if ( stat /= 0 ) then
        pdb_read_data = xslibFLOAT
        return
      end if

    ! If "END" OR "ENDMDL" is found in record exit
    else if ( index(keyword,"END") == 1 ) then
      exit

    end if
  end do ! while

  ! Return success
  pdb_read_data = xslibOK

  return
end function pdb_read_data

! Read only .pdb coordinates.
integer function pdb_read_coor( unit, natoms, box, coor )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit
  integer, intent(in)       :: natoms
  real, intent(out)         :: coor(3,natoms), box(3,3)
  integer                   :: i, stat
  logical                   :: opened
  character(128)            :: buffer
  character(6)              :: keyword
  character(9)              :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    pdb_read_coor = xslibOPEN
    return
  end if

  ! Initialize
  box(:,:) = 0.000

  ! Read all lines with "ATOM" or "HETATOM" keywords
  i = 1 ! Current atom index
  do while( .true. )
    ! Read buffer line
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat /= 0 ) then
      pdb_read_coor = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
      return
    end if

    ! Process keyword
    keyword = buffer(1:6)
    if ( index(keyword,"ATOM") == 1 .or. index(keyword,"HETATM") == 1 ) then
      ! Check if index is below limit
      if ( i > natoms ) then
        pdb_read_coor = xslibNATOMS
        return
      end if

      ! Read only coor
      read (buffer,"(30x,3f8.3)",IOSTAT=stat) coor(:,i)
      if ( stat /= 0 ) then
        pdb_read_coor = xslib3DX
        return
      end if

      ! Increment iterator
      i = i+1

    else if ( index(keyword,"CRYST1") == 1 ) then
      ! TODO: I am too lazy to implement any other box definition options.
      ! * Just use cubic box. Please.
      read (buffer,"(6x,3f9.3)",IOSTAT=stat) box(1,1), box(2,2), box(3,3)
      if ( stat /= 0 ) then
        pdb_read_coor = xslibFLOAT
        return
      end if

    ! If "END" OR "ENDMDL" is found in record exit
    else if ( index(keyword,"END") == 1 ) then
      exit

    end if
  end do ! while

  ! Return success
  pdb_read_coor = xslibOK

  return
end function pdb_read_coor

! Skip .pdb data.
integer function pdb_skip_data( unit, box )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit
  real, intent(out)         :: box(3,3)
  integer                   :: stat
  character(128)            :: buffer
  character(6)              :: keyword
  logical                   :: opened
  character(9)              :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    pdb_skip_data = xslibOPEN
    return
  end if

  ! Initialize
  box(:,:) = 0.000

  ! Read all lines with "ATOM" or "HETATOM" keywords
  stat = 0
  do while( stat == 0 )
    ! Read buffer line
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat /= 0 ) then
      pdb_skip_data = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
      return
    end if

    ! Process keyword
    keyword = buffer(1:6)
    if ( index(keyword,"CRYST1") == 1 ) then
      ! TODO: I am too lazy to implement any other box definition options.
      ! * Just use cubic box. Please.
      read (buffer,"(6x,3f9.3)",IOSTAT=stat ) box(1,1), box(2,2), box(3,3)
      if ( stat /= 0 ) then
        pdb_skip_data = xslibFLOAT
        return
      end if

    ! If "END" OR "ENDMDL" is found in record exit
    else if ( index(keyword,"END") == 1 ) then
      exit

    end if
  end do ! while

  ! Return success
  pdb_skip_data = xslibOK

  return
end function pdb_skip_data

! Write data in .pdb format.
integer function pdb_write_data( unit, natoms, type, atomn, atomnm, altloc, resnm, ch, &
& resn, resic, occup, bfac, segnm, elem, charge, box, coor )
  implicit none
  integer, intent(in)       :: unit
  integer, intent(in)       :: natoms
  character(6), intent(in)  :: type(natoms), atomnm(natoms), altloc(natoms), resnm(natoms)
  character(6), intent(in)  :: ch(natoms), resic(natoms), segnm(natoms), elem(natoms), charge(natoms)
  integer, intent(in)       :: atomn(natoms), resn(natoms)
  real, intent(in)          :: occup(natoms), bfac(natoms), box(3,3), coor(3,natoms)
  integer                   :: i
  logical                   :: opened
  character(9)              :: action

  ! Check if unit is assigned
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    pdb_write_data = xslibOPEN
    return
  end if

  ! Write remark
  write (unit,"('REMARK',5x,a)") "Generated by xslib"

  ! Simulation box side.
  ! NOTE: Use simple cubic crystal group - P1, and z-value of 1
  if ( any(box(:,:) /= 0.000) ) then
    write (unit,100) "CRYST1", box(1,1), box(2,2), box(3,3), 90., 90., 90., "P 1           1"
    100 format( a6, 3f9.3, 3f7.2, x, a )
  end if

  ! Data for every atom.
  do i = 1, natoms
    write (unit,200) type(i), atomn(i), atomnm(i), altloc(i), resnm(i), ch(i),  &
    &   resn(i), resic(i), coor(:,i), occup(i), bfac(i), segnm(i), elem(i), charge(i)
    200 format( a6, i5, x, a4, a1, a3, x, a1, i4, a1, 3x, 3f8.3, 2f6.2, 6x, a4, a2, a2 )
  end do

  ! End of entry
  write (unit,"(a)") "TER"
  write (unit,"(a)") "ENDMDL"

  ! Return sucess
  pdb_write_data = xslibOK

  return
end function pdb_write_data

! Close .pdb file handle.
integer function pdb_close_file( unit )
  implicit none
  integer, intent(in) :: unit
  integer             :: stat
  close ( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    pdb_close_file = xslibCLOSE
  else
    pdb_close_file = xslibOK
  end if
  return
end function pdb_close_file

! -------------------------------------------------

integer function pdb_frame_allocate( this, np )
  implicit none
  class(pdb_frame)    :: this  ! .PDB data file
  integer, intent(in) :: np  ! Number of points to allocate
  integer             :: stat
  ! Set number of atoms
  this%natoms = np
  ! Allocate data
  if ( allocated(this%type) ) deallocate( this%type, this%atomn, this%atomnm, this%altloc, this%resnm, this%ch, &
  &   this%resn, this%resic, this%coor, this%occup, this%bfac, this%segnm, this%elem, this%charge, STAT=stat )

  allocate( this%type(np), this%atomn(np), this%atomnm(np), this%altloc(np), this%resnm(np), this%ch(np), &
  &   this%resn(np), this%resic(np), this%coor(3,np), this%occup(np), this%bfac(np), this%segnm(np),  &
  &   this%elem(np), this%charge(np), STAT=stat )

  if ( stat /= 0 ) then
    pdb_frame_allocate = xslibNOMEM
    return
  end if

  ! Return success
  pdb_frame_allocate = xslibOK

  return
end function pdb_frame_allocate

! Comment
subroutine pdb_frame_assign( this, other )
  implicit none
  class(pdb_frame), intent(inout) :: this
  type(pdb_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy header
  this%box(:,:) = other%box

  ! Copy data
  this%natoms = other%natoms
  if ( allocated(other%coor) ) then
    stat = this%allocate( other%natoms )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%type(:) = other%type
    this%atomnm(:) = other%atomnm
    this%altloc(:) = other%altloc
    this%resnm(:) = other%resnm
    this%ch(:) = other%ch
    this%resic(:) = other%resic
    this%segnm(:) = other%segnm
    this%elem(:) = other%elem
    this%atomn(:) = other%atomn
    this%resn(:) = other%resn
    this%occup(:) = other%occup
    this%bfac(:) = other%bfac
    this%bfac(:) = other%bfac
    this%charge(:) = other%charge
    this%coor(:,:) = other%coor

  end if

  return
end subroutine pdb_frame_assign

! Comment
integer function pdb_frame_copy( this, obj, dest, src, num )
  implicit none
  class(pdb_frame)            :: this
  type(pdb_frame), intent(in) :: obj
  integer, intent(in)         :: dest, src, num

  ! Size check
  if ( (src+num-1) > obj%natoms .or. (dest+num-1) > this%natoms ) then
    pdb_frame_copy = xslibNOMEM
    return
  end if

  ! Copy data
  this%type(dest:dest+num-1)   = obj%type(src:src+num-1)
  this%atomnm(dest:dest+num-1) = obj%atomnm(src:src+num-1)
  this%altloc(dest:dest+num-1) = obj%altloc(src:src+num-1)
  this%resnm(dest:dest+num-1)  = obj%resnm(src:src+num-1)
  this%ch(dest:dest+num-1)     = obj%ch(src:src+num-1)
  this%resic(dest:dest+num-1)  = obj%resic(src:src+num-1)
  this%segnm(dest:dest+num-1)  = obj%segnm(src:src+num-1)
  this%elem(dest:dest+num-1)   = obj%elem(src:src+num-1)
  this%charge(dest:dest+num-1) = obj%charge(src:src+num-1)
  this%atomn(dest:dest+num-1)  = obj%atomn(src:src+num-1)
  this%resn(dest:dest+num-1)   = obj%resn(src:src+num-1)
  this%occup(dest:dest+num-1)  = obj%occup(src:src+num-1)
  this%bfac(dest:dest+num-1)   = obj%bfac(src:src+num-1)
  this%coor(:,dest:dest+num-1) = obj%coor(:,src:src+num-1)

  ! Return sucess
  pdb_frame_copy = xslibOK

  return
end function pdb_frame_copy

! Comment
integer function pdb_frame_read( this, unit )
  implicit none
  class(pdb_frame)    :: this
  integer, intent(in) :: unit

  ! Count number of atoms
  pdb_frame_read = pdb_count( unit, this%natoms )
  if ( pdb_frame_read /= xslibOK ) return

  ! Allocate frame
  pdb_frame_read = this%allocate( this%natoms )
  if ( pdb_frame_read /= xslibOK ) return

  ! Read frame
  pdb_frame_read = pdb_read_data( unit, this%natoms, this%type, this%atomn, this%atomnm, this%altloc, this%resnm, &
  &   this%ch, this%resn, this%resic, this%occup, this%bfac, this%segnm, this%elem, this%charge, this%box, this%coor )
  if ( pdb_frame_read /= xslibOK ) return

  ! Return success
  pdb_frame_read = xslibOK

  return
end function pdb_frame_read

! Comment
integer function pdb_frame_write( this, unit, serial )
  implicit none
  class(pdb_frame)              :: this
  integer, intent(in)           :: unit
  integer, intent(in), optional :: serial
  integer                       :: s

  ! Check memory allocation
  if ( .not. allocated(this%coor) ) then
    pdb_frame_write = xslibNOMEM
    return
  end if

  ! PDB model serial number
  s = merge( serial, 1, present(serial) )
  write (unit,"(a6,i5)") "MODEL ", mod(s,100000)

  ! Write data
  pdb_frame_write = pdb_write_data( unit, this%natoms, this%type, this%atomn, this%atomnm, this%altloc, this%resnm,  &
  &   this%ch, this%resn, this%resic, this%occup, this%bfac, this%segnm, this%elem, this%charge, this%box, this%coor )

  return
end function pdb_frame_write

! -------------------------------------------------

! Comment
integer function pdb_allocate( this, nframes )
  implicit none
  class(pdb_t)        :: this
  integer, intent(in) :: nframes
  integer             :: stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(this%nframes), STAT=stat )
  if ( stat /= 0 ) then
    pdb_allocate = xslibNOMEM
    return
  end if

  ! Return success
  pdb_allocate = xslibOK

  return
end function pdb_allocate

! Comment
integer function pdb_allocate_all( this, natoms, nframes )
  implicit none
  class(pdb_t)        :: this
  integer, intent(in) :: natoms, nframes
  integer             :: i, stat

  ! Set number of frames
  this%nframes = nframes

  ! Allocate frames
  if ( allocated(this%frame) ) deallocate( this%frame, STAT=stat )
  allocate( this%frame(this%nframes), STAT=stat )
  if ( stat /= 0 ) then
    pdb_allocate_all = xslibNOMEM
    return
  end if

  ! Allocate each frame
  do i = 1, this%nframes
    pdb_allocate_all = this%frame(i)%allocate( natoms )
    if ( pdb_allocate_all /= xslibOK ) return
  end do

  ! Return success
  pdb_allocate_all = xslibOK

  return
end function pdb_allocate_all

! Comment
subroutine pdb_assign( this, other )
  implicit none
  class(pdb_t), intent(inout) :: this
  type(pdb_t), intent(in)     :: other
  integer                     :: stat

  ! Copy header
  this%unit = other%unit
  this%natoms = other%natoms
  this%allframes = other%allframes
  this%box(:,:) = other%box

  ! Copy frames
  this%nframes = other%nframes
  if ( allocated(other%frame) ) then
    stat = this%allocate( other%nframes )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine pdb_assign

! Comment
integer function pdb_open( this, file )
  implicit none
  class(pdb_t)              :: this
  character(*), intent(in)  :: file

  ! Open file
  pdb_open = pdb_open_file( this%unit, file, this%allframes, this%natoms, this%box )
  if ( pdb_open /= xslibOK ) return

  ! All frames are left
  this%remaining = this%allframes

  return
end function pdb_open

! Read entire PDB file
integer function pdb_read( this, file, first, last, stride )
  implicit none
  class(pdb_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: i, nfirst, nlast, nstride

  nfirst = merge( first, 1, present(first) )
  nlast = merge( last, -1, present(last) )
  nstride = merge( stride, 1, present(stride) )

  ! Open file
  pdb_read = this%open( file )
  if ( pdb_read /= xslibOK ) return

  ! Prevet doing stupid things.
  if ( nfirst > this%allframes ) nfirst = this%allframes
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Estimate number of frames an allocate data.
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  pdb_read = this%allocate( this%nframes )

  ! Skip all frames before first
  pdb_read = this%skip_next( nfirst-1 )
  if ( pdb_read /= xslibOK ) return

  ! ----------------
  ! Read frames
  do i = 1, this%nframes
    ! Read frame
    pdb_read = this%frame(i)%read( this%unit )
    if ( pdb_read /= xslibOK ) return

    ! If not last frame, skip until next frame.
    if ( i /= this%nframes ) then
      pdb_read = this%skip_next( nstride-1 )
      if ( pdb_read /= xslibOK ) return

    end if
  end do ! for i

  ! No frames are left
  this%remaining = 0

  ! Close file
  pdb_read = this%close()
  if ( pdb_read /= xslibOK ) return

  ! Return success
  pdb_read = xslibOK

  return
end function pdb_read

! Comment
integer function pdb_read_next( this, nframes )
  implicit none
  class(pdb_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: i

  ! Allocate frames
  this%nframes = min( merge( nframes, 1, present(nframes) ), this%remaining )
  pdb_read_next = this%allocate( this%nframes )
  if ( pdb_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    pdb_read_next = this%frame(i)%read( this%unit )
    if ( pdb_read_next /= xslibOK ) return

    ! One less frame to read
    this%remaining = this%remaining-1

  end do ! for n

  ! Return success
  pdb_read_next = xslibOK

  return
end function pdb_read_next

! Comment
integer function pdb_skip_next( this, nframes )
  implicit none
  class(pdb_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: i
  real                          :: box(3,3)

  ! Skip secified number of frames.
  do i = 1, min( merge( nframes, 1, present(nframes) ), this%remaining )
    pdb_skip_next = pdb_skip_data( this%unit, box )
    if ( pdb_skip_next /= xslibOK ) return

    ! One less frame to read
    this%remaining = this%remaining-1

  end do

  ! Return success
  pdb_skip_next = xslibOK

  return
end function pdb_skip_next

! Write single .PDB frame to UNIT or FILE
integer function pdb_write( this, file, unit )
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(pdb_t)                        :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: i, out, stat

  ! Select output method. Default is stdout.
  if ( present(file) ) then
    open( NEWUNIT=out, FILE=trim(file), STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      pdb_write = xslibOPEN
      return
    end if
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    pdb_write = xslibNOMEM
    return
  end if

  ! Write each frame to output
  do i = 1, this%nframes
    pdb_write = this%frame(i)%write( out, SERIAL=i )
    if ( pdb_write /= xslibOK ) return

  end do ! for i

  ! Close file if present
  if ( present(file) ) then
    close( out, IOSTAT=stat )
    if ( stat /= 0 ) then
      pdb_write = xslibCLOSE
      return
    end if
  end if

  ! Rerturn success
  pdb_write = xslibOK

  return
end function pdb_write

! Close .PDB file
integer function pdb_close( this )
  implicit none
  class(pdb_t)  :: this
  pdb_close = pdb_close_file( this%unit )
  return
end function pdb_close

! -------------------------------------------------

integer function pdb_getNatoms( this )
  implicit none
  class(pdb_t)  :: this

  pdb_getNatoms = this%natoms

  return
end function pdb_getNatoms

integer function pdb_getAllframes( this )
  implicit none
  class(pdb_t)  :: this

  pdb_getAllframes = this%allframes

  return
end function pdb_getAllframes

real function pdb_getCubicBox( this )
  implicit none
  class(pdb_t)  :: this
  dimension     :: pdb_getCubicBox(3)

  pdb_getCubicBox(:) = [this%box(1,1), this%box(2,2), this%box(3,3)]

  return
end function pdb_getCubicBox

real function pdb_getBox( this )
  implicit none
  class(pdb_t)  :: this
  dimension     :: pdb_getBox(3,3)

  pdb_getBox(:,:) = this%box

  return
end function pdb_getBox

end module xslib_pdbio
