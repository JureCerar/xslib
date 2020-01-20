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
  !$ use omp_lib
  use xslib
  implicit none
  character(512)  :: arg
  integer         :: nthreads = 1
  integer         :: next, stat

  ! Set OpenMP environment
  !$ nthreads = OMP_get_max_threads()
  !$ call OMP_set_num_threads( nthreads )
  write (*,*) setColor( "Using "//str(nthreads)//" OpenMP thread(s).", ATTR="bold", FG="red" )

  ! Get file from command line
  next = 0
  do while ( next < command_argument_count() )
    next = next+1
    call get_command_argument( next, arg, STATUS=stat )
    if ( stat /= 0 .or. verify(arg," ") == 0 ) exit

    ! Run test for each file
    call file_test( arg )

  end do

contains

subroutine xslibCheck( int )
  use xslib_error, only: xslibErrMsg, error
  implicit none
  integer, intent(in) :: int
  if ( int /= xslibOK ) call error( xslibErrMsg(int) )
end subroutine xslibCheck

subroutine file_test( file )
  implicit none
  character(*), intent(in)  :: file
  type(file_t)              :: obj
  integer                   :: i, tid = 0
  integer                   :: first = 2, last = 10, stride = 2

  ! Open file and set first, last, stride
  call xslibCheck( obj%open(file, first, last, stride) )

  !$OMP PARALLEL PRIVATE( tid, i ) FIRSTPRIVATE( obj )
  !$ tid = OMP_get_thread_num()

  !$OMP DO ORDERED SCHEDULE(dynamic)
  do i = first, last, stride
    ! Read should always be performed in critical manner.
    !$OMP CRITICAL
      call xslibCheck( obj%read_next() )
    !$OMP END CRITICAL

    write(*,100) tid, i, obj%coor(:,1)
    100 format( x, "tid: ", i0, " frame: ", i0, " coor(:,1): ", 3f12.4 )

  end do
  !$OMP END DO
  !$OMP END PARALLEL

  call xslibCheck( obj%close() )

  return
end subroutine file_test

end program main
