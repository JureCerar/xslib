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

#define __THISFILE__ "ioutil.F90"
#define assert(x) assert_( x, __THISFILE__, __LINE__ )
#define xslibCheck(x) xslibCheck_( x, __THISFILE__, __LINE__ )

program main
  use xslib
  implicit none
  ! Constants
  integer, parameter  :: SEEK_SET = 0, SEEK_CUR = 1, SEEL_END = 2
  integer, parameter  :: DIM = 3
  ! Extrenal files
  character(128)      :: arg, temp = "file.txt"
  character(3)        :: ext
  integer             :: next, stat
  ! Expected values
  real                :: x(DIM)       = [ 0.000, 0.263, 1.511 ] ! frame: 1, atom: 1
  integer             :: nframes      = 10
  integer             :: natoms       = 5
  real                :: box(DIM,DIM) = reshape( [2.,0.,0., 0.,2.,0., 0.,0.,2.], SHAPE=[DIM,DIM] )
  real, parameter     :: delta        = 1.0E-3

  ! Write version
  write (*,*) "xslib "//xslibInfo()
  write (*,*) "" ! Empty line

  ! Get file from command line
  next = 0
  do while ( next < command_argument_count() )
    next = next+1
    call get_command_argument( next, arg, STATUS=stat )
    if ( stat /= 0 .or. len_trim(arg) == 0 ) exit

    ! Extract file extension
    ext = toLower(extension(arg))
    write (*,*) "Processing file: '", trim(arg), "'"

    select case ( trim(ext) )
    case( "xyz" )
      call test_xyz( arg )
      call test_file( arg )
    case( "gro" )
      call test_gro( arg )
      call test_file( arg )
    case( "pdb" )
      call test_pdb( arg )
      call test_file( arg )
    case( "dcd" )
      call test_dcd( arg )
      call test_file( arg )
    case( "xtc" )
      call test_xtc( arg )
      call test_file( arg )
    case( "trr" )
      call test_trr( arg )
      call test_file( arg )
    case( "cub" )
      call test_cub( arg )
    case( "ndx" )
      call test_ndx( arg )
    case( "tpl" )
      call test_tpl( arg )
    case( "pdh" )
      call test_pdh( arg )
    case( "csv" )
    !   call test_csv( arg )
    case default
      call error( "Unknown file extension: '"//trim(ext)//"'" )
    end select

    ! Check passed
    write (*,*) "Check :: ", setColor( "[PASSED]", FG="green", ATTR="bold" )

  end do

contains

! Test for xyz_t class
subroutine test_xyz( file )
  use xslib_xyzio
  implicit none
  character(*), intent(in)  :: file
  type(xyz_t)               :: obj, cpy

  ! Open file and check some values
  call xslibCheck( obj%open(file) )
  call assert( obj%getNframes() == nframes )
  call assert( obj%getNatoms() == natoms )
  ! call assert( abs(obj%getBox()-box*10) < delta ) ! .XYZ does not have box
  call assert( obj%ftell() == 1 )

  ! Read first frame and check coor
  call xslibCheck( obj%read_next() )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]
  ! call assert( abs(obj%frame(1)%box-box*10) < delta ) ! .XYZ does not have box
  call assert( obj%ftell() == 2 )

  ! Skip one frame and check position
  call xslibCheck( obj%skip_next() )
  call assert( obj%ftell() == 3 )

  ! Read out of order (first frame) and check coor
  call xslibCheck( obj%read_frame( 1 ) )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]
  call assert( obj%ftell() == 2 )

  ! Go to frame #4 and check position
  call xslibCheck( obj%fseek( 4, SEEK_SET ) )
  call assert( obj%ftell() == 4 )

  ! Close file
  call xslibCheck( obj%close() )

  ! Read full file
  call xslibCheck( obj%read( file )  )
  call assert( obj%nframes == nframes )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]

  ! Write file to temporary file
  call xslibCheck( obj%write( FILE=temp ) )

  ! Allocate one frame
  call xslibCheck( cpy%allocate( 1 ) )
  call assert( cpy%nframes == 1 )

  ! Allocate one frame and one atom
  call xslibCheck( cpy%allocate( 1, 1 ) )
  call assert( cpy%nframes == 1 )
  call assert( cpy%frame(1)%natoms == 1 )

  ! Copy frame
  cpy%frame(1) = obj%frame(1)
  call assert( obj%frame(1)%natoms == natoms )
  call assert( abs(cpy%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]

  ! Copy object
  cpy = obj
  call assert( abs(cpy%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]
  call assert( obj%nframes == nframes )

  return
end subroutine test_xyz

! Test for gro_t class
subroutine test_gro( file )
  use xslib_groio
  implicit none
  character(*), intent(in)  :: file
  type(gro_t)               :: obj, cpy

  ! Open file and check some values
  call xslibCheck( obj%open(file) )
  call assert( obj%getNframes() == nframes )
  call assert( obj%getNatoms() == natoms )
  ! call assert( abs(obj%getBox()-box) < delta )
  call assert( obj%ftell() == 1 )

  ! Read first frame and check coor
  call xslibCheck( obj%read_next() )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  call assert( abs(obj%frame(1)%box-box) < delta )
  call assert( obj%ftell() == 2 )

  ! Skip one frame and check position
  call xslibCheck( obj%skip_next() )
  call assert( obj%ftell() == 3 )

  ! Read out of order (first frame) and check coor
  call xslibCheck( obj%read_frame( 1 ) )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  call assert( obj%ftell() == 2 )

  ! Go to frame #4 and check position
  call xslibCheck( obj%fseek( 4, SEEK_SET ) )
  call assert( obj%ftell() == 4 )

  ! Close file
  call xslibCheck( obj%close() )

  ! Read full file
  call xslibCheck( obj%read( file )  )
  call assert( obj%nframes == nframes )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )

  ! Write file to temporary file
  call xslibCheck( obj%write( FILE=temp ) )

  ! Allocate one frame
  call xslibCheck( cpy%allocate( 1 ) )
  call assert( cpy%nframes == 1 )

  ! Allocate one frame and one atom
  call xslibCheck( cpy%allocate( 1, 1 ) )
  call assert( cpy%nframes == 1 )
  call assert( cpy%frame(1)%natoms == 1 )

  ! Copy frame
  cpy%frame(1) = obj%frame(1)
  call assert( obj%frame(1)%natoms == natoms )
  call assert( abs(cpy%frame(1)%coor(:,1)-x) < delta )

  ! Copy object
  cpy = obj
  call assert( abs(cpy%frame(1)%coor(:,1)-x) < delta )
  call assert( obj%nframes == nframes )

  return
end subroutine test_gro

! Test for pdb_t class
subroutine test_pdb( file )
  use xslib_pdbio
  implicit none
  character(*), intent(in)  :: file
  type(pdb_t)               :: obj, cpy

  ! Open file and check some values
  call xslibCheck( obj%open(file) )
  call assert( obj%getNframes() == nframes )
  call assert( obj%getNatoms() == natoms )
  ! call assert( abs(obj%getBox()-box*10) < delta ) ! Correct for [A]
  call assert( obj%ftell() == 1 )

  ! Read first frame and check coor
  call xslibCheck( obj%read_next() )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]
  call assert( abs(obj%frame(1)%box-box*10) < delta ) ! Correct for [A]
  call assert( obj%ftell() == 2 )

  ! Skip one frame and check position
  call xslibCheck( obj%skip_next() )
  call assert( obj%ftell() == 3 )

  ! Read out of order (first frame) and check coor
  call xslibCheck( obj%read_frame( 1 ) )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]
  call assert( obj%ftell() == 2 )

  ! Go to frame #4 and check position
  call xslibCheck( obj%fseek( 4, SEEK_SET ) )
  call assert( obj%ftell() == 4 )

  ! Close file
  call xslibCheck( obj%close() )

  ! Read full file
  call xslibCheck( obj%read( file )  )
  call assert( obj%nframes == nframes )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]

  ! Write file to temporary file
  call xslibCheck( obj%write( FILE=temp ) )

  ! Allocate one frame
  call xslibCheck( cpy%allocate( 1 ) )
  call assert( cpy%nframes == 1 )

  ! Allocate one frame and one atom
  call xslibCheck( cpy%allocate( 1, 1 ) )
  call assert( cpy%nframes == 1 )
  call assert( cpy%frame(1)%natoms == 1 )

  ! Copy frame
  cpy%frame(1) = obj%frame(1)
  call assert( obj%frame(1)%natoms == natoms )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]

  ! Copy object
  cpy = obj
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]
  call assert( obj%nframes == nframes )

  return
end subroutine test_pdb

! Test for dcd_t class
subroutine test_dcd( file )
  use xslib_dcdio
  implicit none
  type(dcd_t)               :: obj, cpy
  character(*), intent(in)  :: file

  ! Open file and check some values
  call xslibCheck( obj%open(file) )
  call assert( obj%getNframes() == nframes )
  call assert( obj%getNatoms() == natoms )
  ! call assert( abs(obj%getBox()-box*10) < delta ) ! Correct for [A]
  call assert( obj%ftell() == 1 )

  ! Read first frame and check coor
  call xslibCheck( obj%read_next() )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]
  call assert( abs(obj%frame(1)%box-box*10) < delta ) ! Correct for [A]
  call assert( obj%ftell() == 2 )

  ! Skip one frame and check position
  call xslibCheck( obj%skip_next() )
  call assert( obj%ftell() == 3 )

  ! Read out of order (first frame) and check coor
  call xslibCheck( obj%read_frame( 1 ) )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]
  call assert( obj%ftell() == 2 )

  ! Go to frame #4 and check position
  call xslibCheck( obj%fseek( 4, SEEK_SET ) )
  call assert( obj%ftell() == 4 )

  ! Close file
  call xslibCheck( obj%close() )

  ! Read full file
  call xslibCheck( obj%read( file )  )
  call assert( obj%nframes == nframes )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]

  ! Write file to temporary file
  call xslibCheck( obj%dump( FILE=temp ) )

  ! Allocate one frame
  call xslibCheck( cpy%allocate( 1 ) )
  call assert( cpy%nframes == 1 )

  ! Allocate one frame and one atom
  call xslibCheck( cpy%allocate( 1, 1 ) )
  call assert( cpy%nframes == 1 )
  call assert( cpy%frame(1)%natoms == 1 )

  ! Copy frame
  cpy%frame(1) = obj%frame(1)
  call assert( obj%frame(1)%natoms == natoms )
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]

  ! Copy object
  cpy = obj
  call assert( abs(obj%frame(1)%coor(:,1)-x*10) < delta ) ! Correct for [A]
  call assert( obj%nframes == nframes )

  return
end subroutine test_dcd

! Test for xtc_t class
subroutine test_xtc( file )
  use xslib_xtcio
  implicit none
  type(xtc_t)               :: obj, cpy
  character(*), intent(in)  :: file

  ! Open file and check some values
  call xslibCheck( obj%open(file) )
  call assert( obj%getNframes() == nframes )
  call assert( obj%getNatoms() == natoms )
  ! call assert( abs(obj%getBox()-box) < delta )
  call assert( obj%ftell() == 1 )

  ! Read first frame and check coor
  call xslibCheck( obj%read_next() )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  call assert( abs(obj%frame(1)%box-box) < delta )
  call assert( obj%ftell() == 2 )

  ! Skip one frame and check position
  call xslibCheck( obj%skip_next() )
  call assert( obj%ftell() == 3 )

  ! Read out of order (first frame) and check coor
  call xslibCheck( obj%read_frame( 1 ) )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  call assert( obj%ftell() == 2 )

  ! Go to frame #4 and check position
  call xslibCheck( obj%fseek( 4, SEEK_SET ) )
  call assert( obj%ftell() == 4 )

  ! Close file
  call xslibCheck( obj%close() )

  ! Read full file
  call xslibCheck( obj%read( file )  )
  call assert( obj%nframes == nframes )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )

  ! Write file to temporary file
  call xslibCheck( obj%dump( FILE=temp ) )

  ! Allocate one frame
  call xslibCheck( cpy%allocate( 1 ) )
  call assert( cpy%nframes == 1 )

  ! Allocate one frame and one atom
  call xslibCheck( cpy%allocate( 1, 1 ) )
  call assert( cpy%nframes == 1 )
  call assert( cpy%frame(1)%natoms == 1 )

  ! Copy frame
  cpy%frame(1) = obj%frame(1)
  call assert( obj%frame(1)%natoms == natoms )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )

  ! Copy object
  cpy = obj
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  call assert( obj%nframes == nframes )

  return
end subroutine test_xtc

! Test for trr_t class
subroutine test_trr( file )
  use xslib_trrio
  implicit none
  type(trr_t)               :: obj, cpy
  character(*), intent(in)  :: file

  ! Open file and check some values
  call xslibCheck( obj%open(file) )
  call assert( obj%getNframes() == nframes )
  call assert( obj%getNatoms() == natoms )
  ! call assert( abs(obj%getBox()-box) < delta )
  call assert( obj%ftell() == 1 )

  ! Read first frame and check coor
  call xslibCheck( obj%read_next() )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  call assert( abs(obj%frame(1)%box-box) < delta )
  call assert( obj%ftell() == 2 )

  ! Skip one frame and check position
  call xslibCheck( obj%skip_next() )
  call assert( obj%ftell() == 3 )

  ! Read out of order (first frame) and check coor
  call xslibCheck( obj%read_frame( 1 ) )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  call assert( obj%ftell() == 2 )

  ! Go to frame #4 and check position
  call xslibCheck( obj%fseek( 4, SEEK_SET ) )
  call assert( obj%ftell() == 4 )

  ! Close file
  call xslibCheck( obj%close() )

  ! Read full file
  call xslibCheck( obj%read( file )  )
  call assert( obj%nframes == nframes )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )

  ! Write file to temporary file
  call xslibCheck( obj%dump( FILE=temp ) )

  ! Allocate one frame
  call xslibCheck( cpy%allocate( 1 ) )
  call assert( cpy%nframes == 1 )

  ! Allocate one frame and one atom
  call xslibCheck( cpy%allocate( 1, 1 ) )
  call assert( cpy%nframes == 1 )
  call assert( cpy%frame(1)%natoms == 1 )

  ! Copy frame
  cpy%frame(1) = obj%frame(1)
  call assert( obj%frame(1)%natoms == natoms )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )

  ! Copy object
  cpy = obj
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  call assert( obj%nframes == nframes )

  return
end subroutine test_trr

! Test for cub_t class
subroutine test_cub( file )
  use xslib_cubio
  implicit none
  type(cub_t)               :: obj, cpy
  character(*), intent(in)  :: file
  real, parameter           :: voxel(DIM,DIM) = reshape( [0.283459,0.,0., 0.,0.283459,0., 0.,0.,0.283459], SHAPE=[DIM,DIM] )
  real, parameter           :: val = 7.62939E-06 ! grid[1,1,1]

  ! Read file and check some values
  call xslibCheck( obj%read(file) )
  call assert( abs(obj%voxel(:,:)-voxel) < delta )
  call assert( abs(obj%grid(1,1,1)-val) < delta )

  ! Write to file
  call xslibCheck( obj%write( FILE=TEMP )  )

  ! Copy object
  cpy = obj
  call assert( abs(cpy%voxel(:,:)-voxel) < delta )
  call assert( abs(cpy%grid(1,1,1)-val) < delta )

  return
end subroutine test_cub

! -------------------------------------------
! One class to rule them all

! Test for xyz_t class
subroutine test_file( file )
  use xslib_fileio
  implicit none
  character(*), intent(in)  :: file
  type(file_t)              :: obj, cpy

  ! Open file and check some values
  call xslibCheck( obj%open(file) )
  call assert( obj%getNframes() == nframes )
  call assert( obj%getNatoms() == natoms )
  ! call assert( abs(obj%getBox()-box) < delta )
  call assert( obj%ftell() == 1 )

  ! Read first frame and check coor
  call xslibCheck( obj%read_next() )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  ! call assert( abs(obj%frame(1)%box-box) < delta )
  call assert( obj%ftell() == 2 )

  ! Skip one frame and check position
  call xslibCheck( obj%skip_next() )
  call assert( obj%ftell() == 3 )

  ! Read out of order (first frame) and check coor
  call xslibCheck( obj%read_frame( 1 ) )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )
  call assert( obj%ftell() == 2 )

  ! Go to frame #4 and check position
  call xslibCheck( obj%fseek( 4, SEEK_SET ) )
  call assert( obj%ftell() == 4 )

  ! Close file
  call xslibCheck( obj%close() )

  ! Read full file
  call xslibCheck( obj%read(file)  )
  call assert( obj%nframes == nframes )
  call assert( abs(obj%frame(1)%coor(:,1)-x) < delta )

  ! Write file to temporary file
  call xslibCheck( obj%dump( FILE=temp ) )
  ! call xslibCheck( obj%dump() )

  ! Allocate one frame
  call xslibCheck( cpy%allocate( 1 ) )
  call assert( cpy%nframes == 1 )

  ! Allocate one frame and one atom
  call xslibCheck( cpy%allocate( 1, 1 ) )
  call assert( cpy%nframes == 1 )
  call assert( cpy%frame(1)%natoms == 1 )

  ! Copy frame
  cpy%frame(1) = obj%frame(1)
  call assert( obj%frame(1)%natoms == natoms )
  call assert( abs(cpy%frame(1)%coor(:,1)-x) < delta )

  ! Copy object
  cpy = obj
  call assert( abs(cpy%frame(1)%coor(:,1)-x) < delta )
  call assert( obj%nframes == nframes )

  return
end subroutine test_file


! -------------------------------------------
! Support file classes

! Test for ndx_t class
subroutine test_ndx( file )
  use xslib_ndxio
  implicit none
  type(ndx_t)               :: obj, cpy
  character(*), intent(in)  :: file

  ! Read file and check some values
  call xslibCheck( obj%read(file) )
  call assert( obj%ngroups == 3 )
  call assert( obj%group(1)%natoms == 30 )
  call assert( obj%group(1)%loc(1) == 1 )

  ! Display index info
  call xslibCheck( obj%display() )

  ! Write file
  call xslibCheck( obj%write( FILE=temp ) )

  ! Allocate and copy group
  call xslibCheck( cpy%allocate( 1 ) )
  cpy%group(1) = obj%group(1)
  call assert( cpy%group(1)%natoms == 30 )
  call assert( obj%group(1)%loc(1) == 1 )

  ! Copy object
  call assert( obj%ngroups == 3 )
  call assert( obj%group(1)%natoms == 30 )
  call assert( obj%group(1)%loc(1) == 1 )

  return
end subroutine test_ndx

! Test for tpl_t class
subroutine test_tpl( file )
  use xslib_tplio
  use xslib_ndxio
  implicit none
  type(tpl_t)               :: obj, cpy
  type(ndx_t)               :: ndx
  character(*), intent(in)  :: file

  ! Read file and check some values
  call xslibCheck( obj%read(file) )
  call assert( obj%ntypes == 3 )
  call assert( obj%type(1)%natoms == 3 )

  ! Write file to temporary file
  call xslibCheck( obj%write(FILE=temp) )

  ! Copy object and check values
  cpy = obj
  call assert( obj%ntypes == 3 )
  call assert( obj%type(1)%natoms == 3 )

  ! Generate index (.ndx) file and check values
  call xslibCheck( tpl2ndx( obj, ndx ) )
  call assert( ndx%ngroups == 4 )
  call assert( ndx%group(1)%natoms == 260 )
  call assert( ndx%group(1)%loc(1) == 1 )

  return
end subroutine test_tpl

! -------------------------------------------
! Data file classes

! Test for pdh_t class
subroutine test_pdh( file )
  use xslib_pdhio
  implicit none
  type(pdh_t)               :: obj, cpy
  character(*), intent(in)  :: file

  ! Read file and check values
  call xslibCheck( obj%read(file) )
  call assert( obj%num_points == 100 )
  call assert( abs(obj%x(1)-1.0) < delta )

  ! Write to temporary file
  call xslibCheck( obj%write( FILE=temp ) )

  ! Copy object and check values
  cpy = obj
  call assert( cpy%num_points == 100 )
  call assert( abs(cpy%x(1)-1.0) < delta )

  return
end subroutine test_pdh

! ! Comment
! subroutine test_csv( file )
!   use xslib_csvio
!   implicit none
!   type(csv_t)               :: obj
!   character(*), intent(in)  :: file
!
!   call xslibCheck( obj%read(file,DELIM=",") )
!   write (*,*) "Read -- OK"
!   call xslibCheck( obj%write(DELIM=";") )
!   write (*,*) "Write -- OK"
!
!   return
! end subroutine test_csv



end program main
