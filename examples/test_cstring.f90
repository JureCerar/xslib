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
  use, intrinsic :: iso_fortran_env, only: INT64, REAL64
  use xslib_cstring
  use xslib_time
  implicit none
  integer, parameter :: dp = REAL64
  integer            :: i, int = 1
  integer(INT64)     :: int8 = 1_dp
  real               :: float = 1.234
  real               :: array(3) = [1.,2.,3.]
  real(REAL64)       :: float8 = 1.234d0
  logical            :: bool = .true.
  complex            :: compl = (1.,1.)
  complex(REAL64)    :: compl8 = (1.0d0,1.0d0)
  character(128)     :: token, string = "Hello World!"

  ! str() on scalar
  write (*,*) str(int), " ", str(int,"(i0.5)")
  write (*,*) str(int8), " ", str(int8,"(i0.5)")
  write (*,*) str(float), " ", str(float,"(f8.4)")
  write (*,*) str(float8), " ", str(float8,"(f8.4)")
  write (*,*) str(bool)
  write (*,*) str(compl), " ", str(compl,"(f8.4)")
  write (*,*) str(compl8), " ", str(compl8,"(f8.4)")
  write (*,*) str(string)

  ! str() on array
  write (*,*) str(array(:))
  write (*,*) str(array(:),"(f8.4)")
  write (*,*) str(array(:),DELIM=";")

  ! Transform to lower and upper case
  write (*,*) "Hello World!"
  write (*,*) toLower( "Hello World!" )
  write (*,*) toUpper( "Hello World!" )

  ! Check id it's word
  write (*,*) "abc", isWord( "abc" )
  write (*,*) "123", isWord( "123" )

  ! Replace text
  write (*,*) "Hello World!"
  write (*,*) replaceText( "Hello World!", "Hello", "Goodbye" )

  ! Tokenize string
  write (*,*) "This is a sample text."
  token = strtok( "This is a sample text.", DELIM=" " );
  do while( token /= char(0) )
    write (*,*) trim(token)
    token = strtok( char(0), " " )
  end do
  write (*,*) "Num. of tokens:", cnttok( "This is a sample text.", DELIM=" " )

  ! Set color
  write (*,*) setColor( "This text is bold.", ATTR="bold" )
  write (*,*) setColor( "This text is red.", FG="red" )
  write (*,*) setColor( "This text is black on white.", FG="black", BG="light gray" )
  write (*,*) setColor( "This text is bold + red on white.", ATTR="bold", FG="red", BG="white" )

  ! File string manipulation
  write (*,*) "path/to/file.ext"
  write (*,*) basename( "path/to/file.ext" )
  write (*,*) pathname( "path/to/file.ext" )
  write (*,*) extension( "path/to/file.ext" )

  ! Create a file and backup it
  open( NEWUNIT=int, FILE="file.txt", STATUS="unknown" )
  close( int )

  call backup( "file.txt", STATUS=int )
  if ( int /= 0 ) stop "Backup error."

  ! Start timer
  float8 = wtime()

  ! Write progres bar
  do i = 1, 20
    call progressbar( real(i)/20., SIZE=50 )
    ! Susspend execution for 10 ms
    int = msleep( 10 )

  end do

  ! Write elapsed time (~1 şec).
  write (*,*) "Elapsed time: ", write_wtime(wtime()-float8)


end program main
