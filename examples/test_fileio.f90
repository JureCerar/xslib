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

program main
  use xslib_error
  use xslib_cstring, only: setColor, extension
  implicit none
  ! character(:), allocatable :: arg
  character(128)  :: ext, arg
  integer        :: next, stat

  ! Get file from command line
  next = 0
  do while ( next < command_argument_count() )
    next = next+1
    call get_command_argument( next, arg, STATUS=stat )
    if ( stat /= 0 .or. verify(arg," ") == 0 ) exit

    ! Extract file extension
    ext = extension(arg)
    write (*,*) "Processing file: ", trim(arg)

    select case ( trim(ext) )
    case( "xyz" )
      call test_xyz( arg )
    case( "gro" )
      call test_gro( arg )
    case( "pdb" )
      call test_pdb( arg )
    case( "xtc" )
      call test_xtc( arg )
    case( "cub" )
      call test_cub( arg )
    case( "dcd" )
      call test_dcd( arg )
    case( "ndx" )
      call test_ndx( arg )
    case( "tpl" )
      call test_tpl( arg )
    case( "pdh" )
      call test_pdh( arg )
    case( "csv" )
      call test_csv( arg )
    case default
      call error( "Unknown file extension: "//trim(ext) )
    end select

    write (*,*) setColor( "Test successfull", FG="green" )

  end do

contains

! xslib error check wrapper.
subroutine xslibCheck( int )
  use xslib_error, only: xslibErrMsg, error
  implicit none
  integer, intent(in) :: int
  if ( int /= 0 ) call error( xslibErrMsg(int) )
  return
end subroutine xslibCheck

! Comment
subroutine test_xyz( file )
  use xslib_xyzio
  implicit none
  character(*), intent(in)  :: file
  type(xyz_t)               :: obj, cpy

  call xslibCheck( obj%read(file) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%read(file,FIRST=2,LAST=-1,STRIDE=2) )
  write (*,*) "Read+opt -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"

  ! -----------------

  call xslibCheck( obj%open( file ) )
  write (*,*) "Open -- OK"

  write (*,*) obj%getAllframes()
  write (*,*) obj%getBox()
  write (*,*) obj%getNatoms()
  write (*,*) "Data -- OK"

  ! -----------------

  call xslibCheck( obj%skip_next( 1 ) )
  write (*,*) "Skip -- OK"
  call xslibCheck( obj%read_next( 1 ) )
  write (*,*) "Read_next -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"
  call xslibCheck( obj%close() )
  write (*,*) "Close -- OK"

  ! -----------------

  cpy = obj
  write (*,*) "Copy -- OK"
  cpy%frame(1) = obj%frame(1)
  write (*,*) "Frame copy -- OK"

  return
end subroutine test_xyz

! Comment
subroutine test_gro( file )
  use xslib_groio
  implicit none
  character(*), intent(in)  :: file
  type(gro_t)               :: obj, cpy

  call xslibCheck( obj%read(file) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%read(file,FIRST=2,LAST=-1,STRIDE=2) )
  write (*,*) "Read+opt -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"

  ! -----------------

  call xslibCheck( obj%open( file ) )
  write (*,*) "Open -- OK"
  write (*,*) obj%getAllframes()
  write (*,*) obj%getBox()
  write (*,*) obj%getNatoms()
  write (*,*) "Data -- OK"

  ! -----------------

  call xslibCheck( obj%skip_next( 1 ) )
  write (*,*) "Skip -- OK"
  call xslibCheck( obj%read_next( 1 ) )
  write (*,*) "Read_next -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"
  call xslibCheck( obj%close() )
  write (*,*) "Close -- OK"

  ! -----------------

  cpy = obj
  write (*,*) "Copy -- OK"
  cpy%frame(1) = obj%frame(1)
  write (*,*) "Frame copy -- OK"

  return
end subroutine test_gro

! Comment
subroutine test_pdb( file )
  use xslib_pdbio
  implicit none
  character(*), intent(in)  :: file
  type(pdb_t)               :: obj, cpy

  call xslibCheck( obj%read(file) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%read(file,FIRST=2,LAST=-1,STRIDE=2) )
  write (*,*) "Read+opt -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"

  ! -----------------

  call xslibCheck( obj%open( file ) )
  write (*,*) "Open -- OK"
  write (*,*) obj%getAllframes()
  write (*,*) obj%getBox()
  write (*,*) obj%getNatoms()
  write (*,*) "Data -- OK"

  ! -----------------

  call xslibCheck( obj%skip_next( 1 ) )
  write (*,*) "Skip -- OK"
  call xslibCheck( obj%read_next( 1 ) )
  write (*,*) "Read_next -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"
  call xslibCheck( obj%close() )
  write (*,*) "Close -- OK"

  ! -----------------

  cpy = obj
  write (*,*) "Copy -- OK"
  cpy%frame(1) = obj%frame(1)
  write (*,*) "Frame copy -- OK"

  return
end subroutine test_pdb

! Comment
subroutine test_xtc( file )
  use xslib_xtcio
  implicit none
  type(xtc_t)               :: obj, cpy
  character(*), intent(in)  :: file

  call xslibCheck( obj%read(file) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%read(file,FIRST=2,LAST=-1,STRIDE=2) )
  write (*,*) "Read+opt -- OK"
  call xslibCheck( obj%dump() )
  write (*,*) "Write -- OK"

  ! -----------------

  call xslibCheck( obj%open( file ) )
  write (*,*) "Open -- OK"
  write (*,*) obj%getAllframes()
  write (*,*) obj%getBox()
  write (*,*) obj%getNatoms()
  write (*,*) "Data -- OK"

  ! -----------------

  call xslibCheck( obj%skip_next( 1 ) )
  write (*,*) "Skip -- OK"
  call xslibCheck( obj%read_next( 1 ) )
  write (*,*) "Read_next -- OK"
  call xslibCheck( obj%dump() )
  write (*,*) "Write -- OK"
  call xslibCheck( obj%close() )
  write (*,*) "Close -- OK"

  ! -----------------

  cpy = obj
  write (*,*) "Copy -- OK"
  cpy%frame(1) = obj%frame(1)
  write (*,*) "Frame copy -- OK"

  return
end subroutine test_xtc

! Comment
subroutine test_cub( file )
  use xslib_cubio
  implicit none
  type(cub_t)               :: obj, cpy
  character(*), intent(in)  :: file

  call xslibCheck( obj%read(file) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"
  cpy = obj
  write (*,*) "Copy -- OK"

  return
end subroutine test_cub

! Comment
subroutine test_dcd( file )
  use xslib_dcdio
  implicit none
  type(dcd_t)               :: obj, cpy
  character(*), intent(in)  :: file

  call xslibCheck( obj%read(file) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%read(file,FIRST=2,LAST=-1,STRIDE=2) )
  write (*,*) "Read+opt -- OK"
  call xslibCheck( obj%dump() )
  write (*,*) "Write -- OK"

  ! -----------------

  call xslibCheck( obj%open( file ) )
  write (*,*) "Open -- OK"
  write (*,*) obj%getAllframes()
  write (*,*) obj%getBox()
  write (*,*) obj%getNatoms()
  write (*,*) "Data -- OK"

  ! -----------------

  call xslibCheck( obj%skip_next( 1 ) )
  write (*,*) "Skip -- OK"
  call xslibCheck( obj%read_next( 1 ) )
  write (*,*) "Read_next -- OK"
  call xslibCheck( obj%dump() )
  write (*,*) "Write -- OK"
  call xslibCheck( obj%close() )
  write (*,*) "Close -- OK"

  ! -----------------

  cpy = obj
  write (*,*) "Copy -- OK"
  cpy%frame(1) = obj%frame(1)
  write (*,*) "Frame copy -- OK"

  return
end subroutine test_dcd

! ! Comment
! subroutine test_file( file )
!   use xslib_fileio
!   implicit none
!   character(*), intent(in)  :: file
!   type(file_t)              :: obj
!
!   call xslibCheck( obj%open(file) )
!   write (*,*) "Open -- OK"
!   write (*,*) obj%getAllframes()
!   write (*,*) obj%getBox()
!   write (*,*) obj%getNatoms()
!   write (*,*) "Data -- OK"
!
!   ! -----------------
!
!   call xslibCheck( obj%read_next() )
!   write (*,*) "Read next -- OK"
!
!   write (*,*) "natoms:", obj%natoms
!   write (*,*) "box:", obj%box(:,:)
!   write (*,*) "x:", obj%coor(:,1)
!   write (*,*) "Frame -- OK"
!
!   call xslibCheck( obj%read_next() )
!   write (*,*) "Close -- OK"
!
!   return
! end subroutine test_file

! Comment
subroutine test_ndx( file )
  use xslib_ndxio
  implicit none
  type(ndx_t)               :: obj, cpy
  character(*), intent(in)  :: file

  call xslibCheck( obj%read(file) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%display() )
  write (*,*) "Display -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"

  ! -----------------

  cpy = obj
  write (*,*) "obj copy -- OK"
  cpy%group(1) = obj%group(1)
  write (*,*) "obj%frame copy -- OK"

  return
end subroutine test_ndx

! Comment
subroutine test_tpl( file )
  use xslib_tplio
  use xslib_ndxio
  implicit none
  type(tpl_t)               :: obj, cpy
  type(ndx_t)               :: ndx
  character(*), intent(in)  :: file

  call xslibCheck( obj%read(file) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"

  ! -----------------

  call xslibCheck( obj%makeNDX( ndx ) )
  write (*,*) "Make NDX -- OK"
  call xslibCheck( ndx%write() )
  write (*,*) "Write NDX -- OK"

  ! -----------------

  cpy = obj
  write (*,*) "obj copy -- OK"

  return
end subroutine test_tpl

! Comment
subroutine test_pdh( file )
  use xslib_pdhio
  implicit none
  type(pdh_t)               :: obj, cpy
  character(*), intent(in)  :: file

  call xslibCheck( obj%read(file) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%write() )
  write (*,*) "Write -- OK"

  cpy = obj
  write (*,*) "obj copy -- OK"

  return
end subroutine test_pdh

! Comment
subroutine test_csv( file )
  use xslib_csvio
  implicit none
  type(csv_t)               :: obj
  character(*), intent(in)  :: file
  character(:), allocatable :: header(:)
  real, allocatable         :: data(:,:)

  call xslibCheck( obj%read( file, data, header, DELIM="," ) )
  write (*,*) "Read -- OK"
  call xslibCheck( obj%write( DATA=data, HEADER=header, DELIM=";" ) )
  write (*,*) "Write -- OK"

  return
end subroutine test_csv


end program main
