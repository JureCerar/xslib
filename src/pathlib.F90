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

module xslib_pathlib
  implicit none
  private
  public ::  filename, basename, dirname, extname, backup

  ! %%%
  ! # `PATHLIB` - Functions for path and file manipulation.
  !   Module `xslib_pathlib` contains function for path and file manipulation.
  ! %%%

contains

function filename (path) result (out)
  ! %%%
  ! ## `FILENAME` - Get file name of pathname
  ! #### DESCRIPTION
  !   Return the file name of pathname `path`. If pathname is a folder it will return an empty string.
  ! #### USAGE
  !   ```Fortran
  !   out = filename(path)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: path`
  !     String containing pathname.
  !   * `character(:), allocatable :: out`
  !     String containing pathname file name.
  ! #### EXAMPLE
  !   ```Fortran
  !   > filename("/path/to/file.txt")
  !   "file.txt"
  !   > filename("/path/to/")
  !   ""
  !   ```
  ! %%%
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: path
  integer :: i

  i = 1 + index(path, DIR_SEPARATOR, BACK=.true.)
  out = path(i:len_trim(path))

end function filename


function basename (path) result (out)
  ! %%%
  ! ## `BASENAME` - Get base name of pathname
  ! #### DESCRIPTION
  !   Return the base name of pathname `path`. If pathname is folder it will return an empty string.
  ! #### USAGE
  !   ```Fortran
  !   out = basename(path)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: path`
  !     String containing pathname.
  !   * `character(:), allocatable :: out`
  !     String containing pathname base name.
  ! #### EXAMPLE
  !   ```Fortran
  !   > basename("/path/to/file.txt")
  !   "file"
  !   > basename("/path/to/")
  !   ""
  !   ```
  ! %%%
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: path
  integer :: i, j

  i = 1 + index(path, DIR_SEPARATOR, BACK=.true.)
  j = index(path, ".", BACK=.true.)
  j = merge(j-1, len_trim(path), j /= 0)
  out = path(i:j)

end function basename


function dirname (path) result (out)
  ! %%%
  ! ## `DIRNAME` - Get base name of pathname
  ! #### DESCRIPTION
  !   Return the directory name of pathname `path`. If pathname is file it will return an empty string.
  ! #### USAGE
  !   ```Fortran
  !   out = dirname(path)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: path`
  !     String containing pathname.
  !   * `character(:), allocatable :: out`
  !     String containing pathname directory name.
  ! #### EXAMPLE
  !   ```Fortran
  !   > dirname("/path/to/file.txt")
  !   "/path/to/"
  !   > dirname("file.txt")
  !   ""
  !   ```
  ! %%%
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: path
  integer :: i

  i = index(path, DIR_SEPARATOR, BACK=.true.)
  if (i < 1) then
    out = ""
  else
    out = path(:i-1)
  end if

end function dirname


function extname (path) result (out)
  ! %%%
  ! ## `EXTNAME` - Get extension name of pathname
  ! #### DESCRIPTION
  !   Return the extension name of pathname `path`. If pathname is not a file it will return an empty string.
  ! #### USAGE
  !   ```Fortran
  !   out = extname(path)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: path`
  !     String containing pathname.
  !   * `character(:), allocatable :: out`
  !     String containing pathname extension name.
  ! #### EXAMPLE
  !   ```Fortran
  !   > extname("/path/to/file.txt")
  !   "txt"
  !   > extname("/path/to/")
  !   ""
  !   ```
  ! %%%
  implicit none
  character(:), allocatable :: out, tmp
  character(*), intent(in) :: path
  integer :: i, j

  i = index(path, DIR_SEPARATOR, BACK=.true.)
  if (i < 1) then
    tmp = trim(path)
  else
    tmp = trim(path(i+1:))
  end if 
  ! j = merge(j-1, len_trim(path), j /= 0)

  j = index(tmp, ".", BACK=.true.)
  if ( j == 0 ) then
    out = ""
  else
    out = tmp(j+1:)
  end if

end function extname


subroutine backup (file, status)
  ! %%%
  ! ## `BACKUP` - Backup existing file
  ! #### DESCRIPTION
  !   Backup a existing file i.e. checks if `file` already exists and renames it to `#file.{n}#` where.
  ! #### USAGE
  !   ```Fortran
  !   call backup(file, status=status)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: file`
  !     File name to backup.
  !   * `integer, intent(OUT), OPTIONAL :: status`
  !     Return status returns 0 on success and nonzero otherwise.
  ! #### EXAMPLE
  !   ```Fortran
  !   call backup("file.txt", status)
  !   if (status != 0) error stop "Backup failed."
  !   ```
  ! %%%
  use iso_fortran_env, only: ERROR_UNIT
  implicit none
  character(*), intent(in) :: file
  integer, intent(out), optional :: status
  character(:), allocatable :: path, name, newfile
  character(128) :: number
  logical :: exist
  integer :: i, stat

  ! Check if file exists
  inquire (FILE=trim(file), EXIST=exist)
  if (.not. exist) then
    if (present(status)) status = 0
    return
  end if

  ! If file exists then create a new name inf format: "path/to/#file.ext.n#" n=1,...
  ! Extract path and base
  i = index(file, DIR_SEPARATOR, BACK=.true.)
  if (i <= 0) then
    path = ""
    name = trim(file)
  else
    path = file(1:i)
    name = file(i+1:len_trim(file))
  end if

  newfile = ""
  do i = 0, huge(i) - 1
    ! Generate new file name and try if it is free.
    write (number, "(i0)") i
    newfile = path // "#" // name // "." // trim(number) // "#"
    inquire (FILE=newfile, EXIST=exist)
    if (.not. exist) then
      write (ERROR_UNIT,*) "File '"//trim(file)//"' already exists. Backing it up as: '"//trim(newfile)//"'"
      call rename (trim(file), newfile, STATUS=stat)
      if (present(status)) status = stat
      return
    end if
  end do

  ! If we got to this point, something has failed
  if (present(status)) status = -1

end subroutine backup

end module xslib_pathlib