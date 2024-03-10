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
  public :: realpath, joinpath, filename, basename, dirname, extname, backup

  ! %%%
  ! # `PATHLIB` - Functions for path and file manipulation.
  !   Module `xslib_pathlib` contains function for path and file manipulation.
  ! %%%

contains

function realpath (path) result (out)
  ! %%%
  ! ## `REALPATH` - Get absolute name of pathname
  ! #### DESCRIPTION
  !   The `realpath` function shall derive, from the pathname pointed to 
  !   by `path`, an absolute pathname that names the same file, whose resolution
  !   does not involve `.`, `..`, or symbolic links
  ! #### USAGE
  !   ```Fortran
  !   out = realpath(path)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: path`
  !     String containing pathname.
  !   * `character(:), allocatable :: out`
  !     String containing absolute pathname.
  ! #### EXAMPLE
  !   ```Fortran
  !   > realpath(".")
  !   "/absolute/path/to/dir"
  !   ```
  ! %%%


  ! The realpath() function shall derive, from the pathname pointed to by file_name,
  ! an absolute pathname that names the same file, whose resolution does not involve
  ! '.', '..', or symbolic links.
  use iso_c_binding
  implicit none
  character(*), intent(in) :: path
  character(:), allocatable :: out
  type(C_PTR) :: ptr
  integer, parameter :: PATH_MAX = 1024 * 4
  character(1) :: a(PATH_MAX)
  character(PATH_MAX) :: buffer
  integer :: i

  interface
    ! char *realpath(const char *restrict file_name, char *restrict resolved_name);
    type(C_PTR) function c_realpath(file_name, resolved_name) bind(C, NAME="realpath")
      use, intrinsic :: iso_c_binding
      character(len=1, kind=c_char), intent(in) :: file_name(*)
      character(len=1, kind=c_char), intent(out) :: resolved_name(*)
    end function c_realpath
  end interface

  ptr = c_realpath(path // C_NULL_CHAR, a)
  buffer = transfer(a, buffer)

  ! Remove NULL character at the end
  i = index(buffer, C_NULL_CHAR)
  if (i == 0) error stop "No C_NULL_CHAR found!?"
  out = buffer(:i-1)
  
end function realpath  


function joinpath (path1, path2) result (out)
  ! %%%
  ! ## `JOINPATH` - Concatenate two path names
  ! #### DESCRIPTION
  !   Return concatenated pathname components, effectively constructing valid path.
  !   It ensures cross-platform compatibility by properly joining the components.
  ! #### USAGE
  !   ```Fortran
  !   out = joinpath(path1, path2)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: path1, path2`
  !     Strings containing pathname.
  !   * `character(:), allocatable :: out`
  !     String containing concatenated pathname.
  ! #### EXAMPLE
  !   ```Fortran
  !   > joinpath("/path/to" "file.txt")
  !   "path/to/file.txt"
  !   ```
  ! %%%
  implicit none
  character(*), intent(in) :: path1, path2
  character(:), allocatable :: out
  integer, parameter :: SEP_LEN = len(DIR_SEPARATOR)

  if (len_trim(path1) > 0 .and. len_trim(path2) > 0) then
    out = trim(path1)
    ! Check if `path1` ends with DIR_SEPARATOR
    if (out(len_trim(path1)-SEP_LEN+1:) /= DIR_SEPARATOR) then
      out = out // DIR_SEPARATOR
    end if
    ! Check if `path2` starts with DIR_SEPARATOR
    if (path2(1:SEP_LEN) == DIR_SEPARATOR) then
      out = out // trim(path2(SEP_LEN+1:))
    else
      out = out // trim(path2)
    end if
  else if (len_trim(path1) > 0) then
    out = trim(path1)
  else if (len_trim(path2) > 0) then
    out = trim(path2)
  else
    out = ""
  end if

end function joinpath


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