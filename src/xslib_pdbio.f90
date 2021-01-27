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
  use iso_fortran_env, only: INT64
  implicit none
  private
  public :: pdb_t
  ! Class independent procedures (if you are feeling adventurous)
  public :: pdb_open_file, pdb_close_file, pdb_count, pdb_read_data, pdb_read_coor, pdb_skip_data, pdb_write_data, pdb_check_file

  ! Import error definitions
  include "fileio.h"

  ! Global constants
  integer, parameter :: DIM = 3 ! Default dimension
  integer, parameter :: PDB_LEN = 3 ! Length of char variables
  integer, parameter :: PDB_RECORD = 6 ! Length of keyword
  integer, parameter :: PDB_MAX_LEN = 256 ! Max buffer length
  integer, parameter :: PDB_MAX_NUM = 100000 ! Max. number identifier

  ! Positioning constants
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2

  ! .PDB data LEGEND:
  ! record  = record_type
  ! atomn   = atom_serial_number
  ! atomnm  = atom_name
  ! altloc  = alternate_location_indicator
  ! resnm   = residue_name
  ! ch      = chain_identifier
  ! resn    = residue_sequence_number
  ! resic   = residue_insertion_code
  ! coor    = x, y, and z coordinates
  ! occup   = occupancy
  ! bfac    = beta_factor
  ! elem    = element_name
  ! charge  = element_partial_charge
  ! segnm   = segment_identifier

  type pdb_frame
    real                                :: box(DIM,DIM) = 0.000
    integer                             :: natoms = 0
    character(PDB_RECORD), allocatable  :: record(:)
    character(PDB_LEN), allocatable     :: atomnm(:), altloc(:), resnm(:), ch(:), resic(:), segnm(:), elem(:), charge(:)
    integer, allocatable                :: atomn(:), resn(:)
    real, allocatable                   :: coor(:,:), occup(:), bfac(:)
  contains
    procedure :: allocate => pdb_frame_allocate
    procedure :: assign => pdb_frame_assign
    generic   :: assignment(=) => assign
    ! procedure :: copy => pdb_frame_copy
    procedure :: read => pdb_frame_read
    procedure :: write => pdb_frame_write
  end type pdb_frame

  type pdb_t
    integer, private                      :: unit = 0           ! File unit
    integer, private                      :: allFrames = 0      ! Num. of frames in currently opened file
    integer, private                      :: natoms = 0         ! Num. of atoms (per frame) in currently opened file
    integer, private                      :: current = 0        ! Current frame in opened file
    real, private                         :: box(DIM,DIM) = 0.  ! Box side
    integer(INT64), allocatable, private  :: offsets(:)         ! Frames offset table
    ! Public data --------------------------
    integer                               :: nframes = 0        ! Num. of frames
    type(pdb_frame), allocatable          :: frame(:)           ! Frame object
  contains
    procedure :: pdb_allocate, pdb_allocate_all
    generic   :: allocate => pdb_allocate, pdb_allocate_all
    procedure :: assign => pdb_assign
    generic   :: assignment(=) => assign
    procedure :: open => pdb_open
    procedure :: close => pdb_close
    procedure :: fseek => pdb_fseek
    procedure :: ftell => pdb_ftell
    procedure :: read_next => pdb_read_next
    procedure :: skip_next => pdb_skip_next
    procedure :: read_frame => pdb_read_frame
    procedure :: read => pdb_read
    procedure :: write => pdb_write
    procedure :: getNframes => pdb_getNframes
    procedure :: getNatoms => pdb_getNatoms
    procedure :: getBox => pdb_getBox
  end type pdb_t

contains

! -------------------------------------------------
! Class independent procedures

! Open .PDB file
integer function pdb_open_file( unit, file, bRead )
  implicit none
  integer, intent(out)      :: unit
  character(*), intent(in)  :: file
  logical, intent(in)       :: bRead ! Open in read mode
  integer                   :: stat
  logical                   :: exist

  ! Open in read or write mode
  if ( bRead ) then
    ! For read mode file must already exist
    inquire( FILE=trim(file), EXIST=exist )
    if ( .not. exist ) then
      pdb_open_file = xslibFILENOTFOUND
      return
    end if

    ! Open file for reading
    open ( NEWUNIT=unit, FILE=trim(file), ACCESS="stream", FORM="formatted", STATUS="old", ACTION="read", IOSTAT=stat )
    if ( stat /= 0 ) then
      pdb_open_file = xslibOPEN
      return
    end if

  else
    ! Open file for writing
    open ( NEWUNIT=unit, FILE=trim(file), ACCESS="stream", FORM="formatted", STATUS="unknown", ACTION="write", IOSTAT=stat )
    if ( stat /= 0 ) then
      pdb_open_file = xslibOPEN
      return
    end if

  end if

  ! Return success
  pdb_open_file = xslibOK

  return
end function pdb_open_file

! Close .PDB file
integer function pdb_close_file( unit )
  implicit none
  integer, intent(inout)  :: unit
  integer                 :: stat
  logical                 :: opened

  ! Close any previously opened files
  inquire( UNIT=unit, OPENED=opened )
  if ( opened ) then
    close( unit, IOSTAT=stat )
    if ( stat /= 0 ) then
      pdb_close_file = xslibCLOSE
      return
    end if
  end if

  ! Return success
  pdb_close_file = xslibOK

  return
end function pdb_close_file

! Count occurance of "ATOM" and "HETATOM" keywods in frame.
integer function pdb_count( unit, natoms )
  use iso_fortran_env, only: INT64, IOSTAT_END
  implicit none
  integer, intent(in)     :: unit
  integer, intent(out)    :: natoms
  integer(INT64)          :: pos
  character(PDB_RECORD)   :: keyword
  integer                 :: stat
  logical                 :: opened
  character(9)            :: action

  ! Check if unit is opened for reading and get current file position
  inquire( UNIT=unit, OPENED=opened, ACTION=action, POS=pos )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    pdb_count = xslibOPEN
    return
  end if

  ! Count occurance of keywords "ATOM" and "HETATM"
  natoms = 0
  do while ( .true. )
    ! Read only keyword
    read (unit,"(a)",IOSTAT=stat) keyword
    if ( stat /= 0 ) then
      pdb_count = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
      return
    end if

    ! Process keyword
    if ( index(keyword,"ATOM") == 1 .or. index(keyword,"HETATM") == 1 ) then
      natoms = natoms+1
    else if ( index(keyword,"END") == 1 ) then
      exit
    end if

  end do ! while

  ! Return to original position
  if ( pos < 4 ) then
    rewind( unit, IOSTAT=stat )
  else
    ! Account for 32bit/4byte for dummy read
    read (unit,*,POS=pos-4,IOSTAT=stat) ! Dummy read
  end if

  if ( stat /= 0 ) then
    pdb_count = xslibHEADER
    return
  end if

  ! Return success
  pdb_count = xslibOK

  return
end function pdb_count

! Read (all) .PDB file data
integer function pdb_read_data( unit, natoms, record, atomn, atomnm, altloc, resnm, ch, &
& resn, resic, occup, bfac, segnm, elem, charge, box, coor )
  use, intrinsic :: iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)             :: unit
  integer, intent(in)             :: natoms
  character(PDB_RECORD)           :: record(natoms)
  character(PDB_LEN), intent(out) :: atomnm(natoms), altloc(natoms), resnm(natoms)
  character(PDB_LEN), intent(out) :: ch(natoms), resic(natoms), segnm(natoms), elem(natoms), charge(natoms)
  integer, intent(out)            :: atomn(natoms), resn(natoms)
  real, intent(out)               :: occup(natoms), bfac(natoms)
  real, intent(out)               :: box(DIM,DIM)
  real, intent(out)               :: coor(DIM,natoms)
  integer                         :: i, j, stat
  logical                         :: opened
  character(PDB_MAX_LEN)          :: buffer
  character(PDB_RECORD)           :: keyword
  character(9)                    :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    pdb_read_data = xslibOPEN
    return
  end if

  ! Initialize
  box(:,:) = 0.000

  ! Read all lines with "ATOM" or "HETATOM" keywords
  i = 1 ! Current atom index
  do while ( .true. )
    ! Read buffer line
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat /= 0 ) then
      pdb_read_data = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
      return
    end if

    ! Process keyword
    keyword = buffer(1:PDB_RECORD)
    if ( index(keyword,"ATOM") == 1 .or. index(keyword,"HETATM") == 1 ) then
      ! Check if index is below limit
      if ( i > natoms ) then
        pdb_read_data = xslibNATOMS
        return
      end if

      ! Read .PDB atom data
      read (buffer,100,IOSTAT=stat) record(i), atomn(i), atomnm(i), altloc(i), resnm(i), ch(i), resn(i), &
      &   resic(i), coor(:,i), occup(i), bfac(i), segnm(i), elem(i), charge(i)
      100 format( a6, i5, x, a4, a1, a3, x, a1, i4, a1, 3x, 3f8.3, 2f6.2, 6x, a4, a2, a2 )
      

      ! Sometimes if ATOMN indeces are to large data can become corrupt. Try without.
      if ( stat /= 0 ) then
        read (buffer,200,IOSTAT=stat) record(i), atomnm(i), altloc(i), resnm(i), ch(i), resn(i), &
        &   resic(i), coor(:,i), occup(i), bfac(i), segnm(i), elem(i), charge(i)
        200 format( a6, 5x, x, a4, a1, a3, x, a1, i4, a1, 3x, 3f8.3, 2f6.2, 6x, a4, a2, a2 )
        atomn(i) = mod(i,PDB_MAX_NUM)

      end if
			
			! Well we tried our best
      if ( stat /= 0 ) then
        pdb_read_data = xslib3DX
        return
      end if

      ! Increment iterator
      i = i+1

    else if ( index(keyword,"CRYST1") == 1 ) then
      ! TODO: I am too lazy to implement any other box definition.
      ! * Just use cubic box... Please.
      read (buffer,"(6x,3f9.3)",IOSTAT=stat) (box(j,j), j=1,DIM)
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

! Read only .PDB file coordinate data
integer function pdb_read_coor( unit, natoms, box, coor )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)       :: unit
  integer, intent(in)       :: natoms
  real, intent(out)         :: coor(DIM,natoms), box(DIM,DIM)
  integer                   :: i, j, stat
  logical                   :: opened
  character(PDB_MAX_LEN)    :: buffer
  character(PDB_RECORD)     :: keyword
  character(9)              :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    pdb_read_coor = xslibOPEN
    return
  end if

  ! Initialize
  box(:,:) = 0.000

  ! Read all lines with "ATOM" or "HETATOM" keywords
  i = 1 ! Current atom index
  do while ( .true. )
    ! Store line to buffer
    read (unit,"(a)",IOSTAT=stat) buffer
    if ( stat /= 0 ) then
      pdb_read_coor = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
      return
    end if

    ! Process keyword
    keyword = buffer(1:PDB_RECORD)
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
      ! TODO: I am too lazy to implement any other box definition.
      ! * Just use cubic box... Please.
      read (buffer,"(6x,3f9.3)",IOSTAT=stat) (box(j,j), j=1,DIM)
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

! Skip .PDB file data
integer function pdb_skip_data( unit )
  use iso_fortran_env, only: IOSTAT_END
  implicit none
  integer, intent(in)   :: unit
  integer               :: stat
  character(PDB_RECORD) :: keyword
  logical               :: opened
  character(9)          :: action

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    pdb_skip_data = xslibOPEN
    return
  end if

  ! Read until END token or EOF is encountered
  do while ( .true. )
    ! Read only keyword
    read (unit,"(a)",IOSTAT=stat) keyword
    if ( stat /= 0 ) then
      pdb_skip_data = merge( xslibENDOFFILE, xslibSTRING, stat == IOSTAT_END )
      return
    end if

    ! If "END" OR "ENDMDL" is found in record exit
    if ( index(keyword,"END") == 1 ) exit

  end do ! while

  ! Return success
  pdb_skip_data = xslibOK

  return
end function pdb_skip_data

! Write data in .PDB format
integer function pdb_write_data( unit, natoms, record, atomn, atomnm, altloc, resnm, ch, &
& resn, resic, occup, bfac, segnm, elem, charge, box, coor )
  implicit none
  integer, intent(in)       :: unit
  integer, intent(in)       :: natoms
  character(*), intent(in)  :: record(natoms)
  character(*), intent(in)  :: atomnm(natoms), altloc(natoms), resnm(natoms)
  character(*), intent(in)  :: ch(natoms), resic(natoms), segnm(natoms), elem(natoms), charge(natoms)
  integer, intent(in)       :: atomn(natoms), resn(natoms)
  real, intent(in)          :: occup(natoms), bfac(natoms), box(DIM,DIM), coor(DIM,natoms)
  integer                   :: i, stat
  logical                   :: opened
  character(9)              :: action

  ! Check if unit is opened for writing
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"WRITE") == 0 ) then
    pdb_write_data = xslibOPEN
    return
  end if

  ! Write remark
  write (unit,"('REMARK',5x,a)",IOSTAT=stat) "Generated by xslib"
  if (stat /= 0 ) then
    pdb_write_data = xslibSTRING
    return
  end if

  ! Simulation box side.
  ! NOTE: Use simple cubic crystal group - P1, and z-value of 1
  if ( any(box(:,:) /= 0.000) ) then
    write (unit,100) "CRYST1", (box(i,i), i=1,DIM), 90., 90., 90., "P 1           1"
    100 format( a6, 3f9.3, 3f7.2, x, a )
  end if

  ! Write data for every atom.
  do i = 1, natoms
    write (unit,200) record(i), mod(atomn(i),PDB_MAX_NUM), atomnm(i), altloc(i), resnm(i), ch(i),  &
    &   mod(resn(i),PDB_MAX_NUM), resic(i), coor(:,i), occup(i), bfac(i), segnm(i), elem(i), charge(i)
    200 format( a6, i5, x, a4, a1, a3, x, a1, i4, a1, 3x, 3f8.3, 2f6.2, 6x, a4, a2, a2 )

  end do

  ! End of entry
  write (unit,"(a)") "TER"
  write (unit,"(a)") "ENDMDL"

  ! Return sucess
  pdb_write_data = xslibOK

  return
end function pdb_write_data

! Check .PDB file (check frames and construct offsets table)
integer function pdb_check_file( unit, natoms, nframes, offsets )
  use iso_fortran_env, only: INT64
  implicit none
  integer, intent(in)         :: unit
  integer, intent(out)        :: natoms, nframes
  integer(INT64), allocatable :: offsets(:)
  integer                     :: iatoms, stat
  logical                     :: opened
  character(9)                :: action
  integer(INT64), allocatable :: temp(:)

  ! Check if unit is opened for reading
  inquire( UNIT=unit, OPENED=opened, ACTION=action )
  if ( .not. opened .or. index(action,"READ") == 0 ) then
    pdb_check_file = xslibOPEN
    return
  end if

  ! Allocate frame offsets (start with size = 32)
  if ( allocated(offsets) ) deallocate( offsets, STAT=stat )
  allocate( offsets(32), SOURCE=0_INT64, STAT=stat )
  if ( stat /= 0 ) then
    pdb_check_file = xslibNOMEM
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
        pdb_check_file = xslibNOMEM
        return
      end if
      temp(:nframes) = offsets(:nframes)
      call move_alloc( temp, offsets )
    end if

    ! Store current position in file
    inquire( UNIT=unit, POS=offsets(nframes+1) )

    ! Read header (exit loop if EOF)
    pdb_check_file = pdb_count( unit, iatoms )
    if ( pdb_check_file == xslibENDOFFILE ) exit
    if ( pdb_check_file /= xslibOK ) return

    ! Another frame exists
    nframes = nframes+1
    natoms  = max( iatoms, natoms )

    ! Skip the rest of the frame
    pdb_check_file = pdb_skip_data( unit )
    if ( pdb_check_file /= xslibOK ) return

  end do ! while

  ! Go back to beginning of file (this resets EOF flag)
  rewind( unit, IOSTAT=stat )
  if ( stat /= 0 ) then
    pdb_check_file = xslibOPEN
    return
  end if

  ! Return success
  pdb_check_file = xslibOK

  return
end function pdb_check_file

! -------------------------------------------------
! pdb_frame class routines

! Allocate .PDB frame
integer function pdb_frame_allocate( this, natoms )
  implicit none
  class(pdb_frame)    :: this
  integer, intent(in) :: natoms  ! Number of natoms
  integer             :: stat
  ! Set number of atoms
  this%natoms = natoms
  ! Allocate data
  if ( allocated(this%record) ) deallocate( this%record, this%atomn, this%atomnm, this%altloc, this%resnm, this%ch, &
  &   this%resn, this%resic, this%coor, this%occup, this%bfac, this%segnm, this%elem, this%charge, STAT=stat )

  allocate( this%record(natoms), this%atomn(natoms), this%atomnm(natoms), this%altloc(natoms), this%resnm(natoms),  &
  & this%ch(natoms), this%resn(natoms), this%resic(natoms), this%coor(DIM,natoms), this%occup(natoms), this%bfac(natoms), &
  & this%segnm(natoms), this%elem(natoms), this%charge(natoms), STAT=stat )
  if ( stat /= 0 ) then
    pdb_frame_allocate = xslibNOMEM
    return
  end if

  ! Return success
  pdb_frame_allocate = xslibOK

  return
end function pdb_frame_allocate

! Assigment(=) for pdb_frame class
subroutine pdb_frame_assign( this, other )
  implicit none
  class(pdb_frame), intent(inout) :: this
  type(pdb_frame), intent(in)     :: other
  integer                         :: stat

  ! Copy data
  this%box(:,:) = other%box
  this%natoms   = other%natoms
  if ( allocated(other%coor) ) then
    stat = this%allocate( other%natoms )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%record(:) = other%record
    this%atomnm(:) = other%atomnm
    this%altloc(:) = other%altloc
    this%resnm(:)  = other%resnm
    this%ch(:)     = other%ch
    this%resic(:)  = other%resic
    this%segnm(:)  = other%segnm
    this%elem(:)   = other%elem
    this%atomn(:)  = other%atomn
    this%resn(:)   = other%resn
    this%occup(:)  = other%occup
    this%bfac(:)   = other%bfac
    this%bfac(:)   = other%bfac
    this%charge(:) = other%charge
    this%coor(:,:) = other%coor

  end if

  return
end subroutine pdb_frame_assign

! Copy data from frame to frame
! integer function pdb_frame_copy( this, obj, dest, src, num )
!   implicit none
!   class(pdb_frame)            :: this
!   type(pdb_frame), intent(in) :: obj
!   integer, intent(in)         :: dest, src, num
!
!   ! Bound check
!   if ( (src+num-1) > obj%natoms .or. (dest+num-1) > this%natoms ) then
!     pdb_frame_copy = xslibNOMEM
!     return
!   end if
!
!   ! Copy data
!   this%record(dest:dest+num-1) = obj%record(src:src+num-1)
!   this%atomnm(dest:dest+num-1) = obj%atomnm(src:src+num-1)
!   this%altloc(dest:dest+num-1) = obj%altloc(src:src+num-1)
!   this%resnm(dest:dest+num-1)  = obj%resnm(src:src+num-1)
!   this%ch(dest:dest+num-1)     = obj%ch(src:src+num-1)
!   this%resic(dest:dest+num-1)  = obj%resic(src:src+num-1)
!   this%segnm(dest:dest+num-1)  = obj%segnm(src:src+num-1)
!   this%elem(dest:dest+num-1)   = obj%elem(src:src+num-1)
!   this%charge(dest:dest+num-1) = obj%charge(src:src+num-1)
!   this%atomn(dest:dest+num-1)  = obj%atomn(src:src+num-1)
!   this%resn(dest:dest+num-1)   = obj%resn(src:src+num-1)
!   this%occup(dest:dest+num-1)  = obj%occup(src:src+num-1)
!   this%bfac(dest:dest+num-1)   = obj%bfac(src:src+num-1)
!   this%coor(:,dest:dest+num-1) = obj%coor(:,src:src+num-1)
!
!   ! Return sucess
!   pdb_frame_copy = xslibOK
!
!   return
! end function pdb_frame_copy

! Read .PDB frame
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
  pdb_frame_read = pdb_read_data( unit, this%natoms, this%record, this%atomn, this%atomnm, this%altloc, this%resnm, &
  &   this%ch, this%resn, this%resic, this%occup, this%bfac, this%segnm, this%elem, this%charge, this%box, this%coor )
  if ( pdb_frame_read /= xslibOK ) return

  ! Return success
  pdb_frame_read = xslibOK

  return
end function pdb_frame_read

! Write .PDB frame
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
  pdb_frame_write = pdb_write_data( unit, this%natoms, this%record, this%atomn, this%atomnm, this%altloc, this%resnm,  &
  &   this%ch, this%resn, this%resic, this%occup, this%bfac, this%segnm, this%elem, this%charge, this%box, this%coor )

  return
end function pdb_frame_write

! -------------------------------------------------
! pdb_t class routines

! Allocate NFRAMES of frames (only allocates frames)
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

! Allocate NFRAMES frames each with NATOMS of atoms
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

! Assigment(=) for pdb_t class
subroutine pdb_assign( this, other )
  implicit none
  class(pdb_t), intent(inout) :: this
  type(pdb_t), intent(in)     :: other
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
    stat = this%allocate( other%nframes )
    if ( stat /= 0 ) stop "Segmentation fault - allocation failure"
    this%frame(:) = other%frame

  end if

  return
end subroutine pdb_assign

! Open .PDB file
integer function pdb_open( this, file )
  implicit none
  class(pdb_t)                :: this
  character(*), intent(in)    :: file

  ! Open file for reading
  pdb_open = pdb_open_file( this%unit, file, .true. )
  if ( pdb_open /= xslibOK ) return

  ! Check file and get frame offsets
  pdb_open = pdb_check_file( this%unit, this%natoms, this%allframes, this%offsets )
  if ( pdb_open /= xslibOK ) return

  ! Set current frame to 1
  this%current = 1

  ! Return success
  pdb_open = xslibOK

  return
end function pdb_open

! Close .PDB file and clean-up
integer function pdb_close( this )
  implicit none
  class(pdb_t)  :: this
  integer       :: stat

  ! Close file
  pdb_close = pdb_close_file( this%unit )
  if ( pdb_close /= xslibOK ) return

  ! Reset private variables
  this%allFrames = 0
  this%natoms    = 0
  this%current   = 0

  ! Clean-up offsets table
  if ( allocated(this%offsets) ) deallocate( this%offsets, STAT=stat )

  ! Return success
  pdb_close = xslibOK

  return
end function pdb_close

! Moves UNIT to the specified FRAME.
! * If WHENCE is set to 0, the OFFSET is taken as an absolute value,
! * if set to 1, OFFSET is taken to be relative to the current position,
! * and if set to 2, relative to the end of the file.
integer function pdb_fseek( this, offset, whence )
  implicit none
  class(pdb_t)        :: this
  integer, intent(in) :: offset, whence
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
  logical             :: opened
  integer             :: frame, stat

  ! Check if unit is opened.
  inquire ( UNIT=this%unit, OPENED=opened )
  if ( .not. opened ) then
    pdb_fseek = xslibOPEN
    return
  end if

  ! Check if offsets table is allocated
  if ( .not. allocated(this%offsets) ) then
    pdb_fseek = xslibNOMEM
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
    pdb_fseek = xslibNR
    return
  end select

  ! Limit selection to bounds
  frame = max( frame, 1 )
  frame = min( frame, this%allframes+1 ) ! "+1" is end of file

  ! Move to frame
  if ( this%offsets(frame) < 4 ) then
    rewind( this%unit, IOSTAT=stat )
  else
    ! Account for 32bit/4byte offset for dummy read
    read (this%unit,*,POS=this%offsets(frame),IOSTAT=stat) ! Dummy read
  end if
  if ( stat /= 0 ) then
    pdb_fseek = xslibENDOFFILE
    return
  end if

  ! Update current frame
  this%current = frame

  ! Return success
  pdb_fseek = xslibOK

  return
end function pdb_fseek

! Retrieves the current position within an open file.
integer function pdb_ftell( this )
  implicit none
  class(pdb_t)        :: this
  pdb_ftell = this%current
  return
end function pdb_ftell

! Read next <N> frames in .PDB file
integer function pdb_read_next( this, nframes )
  implicit none
  class(pdb_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: frames, remaining, i

  ! Optional argument
  frames = merge( nframes, 1, present(nframes) )
  if ( frames < 0 ) frames = 1

  ! Calculate remaining frames
  remaining = this%allFrames-this%current+1

  ! Allocate frames
  this%nframes = min( frames, remaining )
  pdb_read_next = this%allocate( this%nframes )
  if ( pdb_read_next /= xslibOK ) return

  ! Read frame one by one
  do i = 1, this%nframes
    pdb_read_next = this%frame(i)%read( this%unit )
    if ( pdb_read_next /= xslibOK ) return

    ! Increment current frame
    this%current = this%current+1

  end do ! for i

  ! Return success
  pdb_read_next = xslibOK

  return
end function pdb_read_next

! Skip next <N> frames in .PDB file
integer function pdb_skip_next( this, nframes )
  implicit none
  class(pdb_t)                  :: this
  integer, intent(in), optional :: nframes
  integer                       :: np

  ! Move to selected frame aka. "skip frames"
  np = merge( nframes, 1, present(nframes) )
  pdb_skip_next = this%fseek( np, SEEK_CUR )

  return
end function pdb_skip_next

! Read selected frame in .PDB file
integer function pdb_read_frame( this, frame )
  implicit none
  class(pdb_t)        :: this
  integer, intent(in) :: frame

  ! Move to selected frame
  if ( this%current /= frame ) then
    pdb_read_frame = this%fseek( frame, SEEK_SET )
    if ( pdb_read_frame /= 0 ) return
  end if

  ! Read frame
  pdb_read_frame = this%read_next()

  return
end function pdb_read_frame

! Read entire .PDB file
integer function pdb_read( this, file, first, last, stride )
  implicit none
  class(pdb_t)                  :: this
  character(*), intent(in)      :: file
  integer, intent(in), optional :: first, last, stride
  integer                       :: nfirst, nlast, nstride
  integer                       :: i, frame

  ! Open file
  pdb_read = this%open( file )
  if ( pdb_read /= xslibOK ) return

  ! Check optional arguments
  nfirst  = merge( first, 1, present(first) )
  nlast   = merge( last, -1, present(last) )
  nstride = merge( stride, 1, present(stride) )

  ! Prevet doing stupid things.
  if ( nfirst > this%allframes ) nfirst = this%allframes
  if ( nlast < 0 .or. nlast > this%allframes ) nlast = this%allframes

  ! Calculate number of frames and allocate data
  this%nframes = ceiling( (nlast-nfirst+1) / real(nstride) )
  pdb_read = this%allocate( this%nframes )
  if ( pdb_read /= xslibOK ) return

  ! Read frames
  do i = 1, this%nframes
    ! Actual frame number
    frame = nfirst + (i-1)*nstride

    ! Move to selected frame
    if ( this%current /= frame ) then
      pdb_read = this%fseek( frame, SEEK_SET )
      if ( pdb_read /= 0 ) return
    end if

    ! Read i-th frame
    pdb_read = this%frame(i)%read( this%unit )
    if ( pdb_read /= xslibOK ) return

    ! Update current frame
    this%current = this%current+1

  end do ! for i

  ! Close file
  pdb_read = this%close()
  if ( pdb_read /= xslibOK ) return

  ! Return success
  pdb_read = xslibOK

  return
end function pdb_read

! Write data in .PDB format to unit, file or stdout
integer function pdb_write( this, file, unit )
  use iso_fortran_env, only: OUTPUT_UNIT
  implicit none
  class(pdb_t)                        :: this
  integer, intent(in), optional       :: unit
  character(*), intent(in), optional  :: file
  integer                             :: i, out

  ! Check if data is allocated
  if ( .not. allocated(this%frame) ) then
    pdb_write = xslibNOMEM
    return
  end if

  ! Select output method. Default is stdout.
  if ( present(file) ) then
    pdb_write = pdb_open_file( out, file, .false. )
    if ( pdb_write /= xslibOK ) return
  else if ( present(unit) ) then
    out = unit
  else
    out = OUTPUT_UNIT
  end if

  ! Write each frame to output
  do i = 1, this%nframes
    pdb_write = this%frame(i)%write( out, SERIAL=i )
    if ( pdb_write /= xslibOK ) return
  end do ! for i

  ! Close file if present
  if ( present(file) ) then
    pdb_write = pdb_close_file( out )
    if ( pdb_write /= xslibOK ) return
  end if

  ! Rerturn success
  pdb_write = xslibOK

  return
end function pdb_write

! -------------------------------------------------
! pdb_t class utilities

! Return max. num. of atoms in currently opened file
integer function pdb_getNatoms( this )
  implicit none
  class(pdb_t)  :: this
  pdb_getNatoms = this%natoms
  return
end function pdb_getNatoms

! Return num. of frames in currently opened file
integer function pdb_getNframes( this )
  implicit none
  class(pdb_t)  :: this
  pdb_getNframes = this%allframes
  return
end function pdb_getNframes

function pdb_getBox( this ) result( box )
  implicit none
  class(pdb_t)  :: this
  real          :: box(DIM,DIM)
  box(:,:) = this%box
  return
end function pdb_getBox

! Return largest box in currently opened file
! function pdb_getBox( this ) result( box )
!   implicit none
!   class(pdb_t)            :: this
!   real                    :: box(DIM,DIM), ibox(DIM,DIM)
!   integer                 :: i, stat
!   character(PDB_MAX_LEN)  :: buffer
!   character(PDB_LEN)      :: keyword
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
!   ! Proess entire file frame
!   do while ( .true. )
!     ! Store line to buffer
!     read (this%unit,"(a)",IOSTAT=stat) buffer
!     if ( stat /= 0 ) exit
!
!     ! Process keyword
!     keyword = buffer(1:PDB_LEN)
!     if ( index(keyword,"CRYST1") == 1 ) then
!       ! TODO: I am too lazy to implement any other box definition.
!       ! * Just use cubic box... Please
!       read (buffer,"(6x,3f9.3)",IOSTAT=stat) (ibox(i,i), i=1,DIM)
!       if ( stat /= 0 ) exit
!
!       ! Keep the larger box
!       box(:,:) = max( box, ibox )
!
!     end if
!
!   end do
!
!   ! Go to beginning of file (reset EOF)
!   rewind( this%unit, IOSTAT=stat )
!   if ( stat /= 0 ) return
!
!   ! Move back to starting position
!   stat = this%fseek( pos, SEEK_SET )
!
!   return
! end function pdb_getBox

end module xslib_pdbio
