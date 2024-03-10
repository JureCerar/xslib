! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2024 Jure Cerar
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

#if defined(WIN32)
#  define DIR_SEPARATOR "\\"
#else
#  define DIR_SEPARATOR "/"
#endif

program main
  use xslib_pathlib
  implicit none
  character(:), allocatable :: string
  integer :: unit, stat

  ! NOTE: Cannot test realpath? We can only test the interface
  print *, realpath(".")

  ! Test joinpath
  string = joinpath("/path/to/file", "file.txt")
  if (string /= "/path/to/file" // DIR_SEPARATOR // "file.txt") error stop
  string = joinpath("/path/to/file" // DIR_SEPARATOR, "file.txt")
  if (string /= "/path/to/file" // DIR_SEPARATOR // "file.txt") error stop
  string = joinpath("/path/to/file", DIR_SEPARATOR // "file.txt")
  if (string /= "/path/to/file" // DIR_SEPARATOR // "file.txt") error stop

  ! Test filename
  string = filename("/path/to/file" // DIR_SEPARATOR // "file.txt")
  if (string /= "file.txt") error stop
  string = filename("/path/to/file" // DIR_SEPARATOR // "file")
  if (string /= "file") error stop
  string = filename("/path/to/file" // DIR_SEPARATOR // "")
  if (string /= "") error stop 

  ! Test basename
  string = basename("file.txt")
  if (string /= "file") error stop 
  string = basename("file")
  if (string /= "file") error stop 
  string = basename("/path/to/file" // DIR_SEPARATOR // "file.txt")
  if (string /= "file") error stop
  string = basename("/path/to/file" // DIR_SEPARATOR // "file")
  if (string /= "file") error stop
  string = basename("/path/to/file" // DIR_SEPARATOR // "")
  if (string /= "") error stop 

  ! Test dirname
  string = dirname("file.txt")
  if (string /= "") error stop 
  string = dirname("/path/to/file" // DIR_SEPARATOR // "file.txt")
  if (string /= "/path/to/file") error stop
  string = dirname("/path/to/file" // DIR_SEPARATOR // "file")
  if (string /= "/path/to/file") error stop
  string = dirname("/path/to/file" // DIR_SEPARATOR // "")
  if (string /= "/path/to/file") error stop 

  ! Test extname
  string = extname("file.txt")
  if (string /= "txt") error stop 
  string = extname("/path/to/file" // DIR_SEPARATOR // "file.txt")
  if (string /= "txt") error stop
  string = extname("/path/to/file" // DIR_SEPARATOR // "file")
  if (string /= "") error stop
  string = extname("/path/to/file" // DIR_SEPARATOR // "")
  if (string /= "") error stop  

  ! Create a dummy file
  open (FILE="test.txt", NEWUNIT=unit, STATUS="new", ACTION="write", IOSTAT=stat)
  if (stat /= 0) error stop
  close (UNIT=unit, IOSTAT=stat)
  if (stat /= 0) error stop

  ! Test backup file
  call backup("test.txt", STATUS=stat)
  if (stat /= 0) error stop

end program main