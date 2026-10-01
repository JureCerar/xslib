! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2026 Jure Cerar
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
  !! Module for path and file manipulation.
  use iso_c_binding
  implicit none
  private
  public :: is_dir, is_file, iterdir, realpath, joinpath
  public :: filename, basename, dirname, extname, backup

  interface

    type(C_PTR) function opendir (name) bind (C, name="opendir")
      !! Opens a directory stream corresponding to the directory `name`, and returns
      !! a pointer to the directory stream. The stream is positioned at the first entry
      !! in the directory.
      import
      implicit none
      character(kind=C_CHAR), intent(in) :: name(*)
      !! Path to iterate.
    end function opendir

    type(c_ptr) function f_readdir(dirp) bind(C, name="f_readdir")
      !! Returns next directory entry in directory stream pointed to by `dirp`
      !! It returns NULL on reaching the end of the directory stream or if an 
      !! error occurred. This is wrapper around C `readdir()` function.
      !!
      import
      type(c_ptr), value :: dirp
      !! Pointer to the directory stream.
    end function f_readdir

    integer(C_INT) function closedir (dirp) bind (C, name="closedir")
      !! Closes the directory stream associated with dirp. A successful call 
      !! also closes the underlying file descriptor associated with dirp.
      !! The directory stream descriptor dirp is not available after this call.
      !!
      !! Function returns 0 on success. On error, -1 is returned.
      import
      implicit none
      type(C_PTR), value :: dirp
      !! Pointer to the directory stream.
    end function closedir

    integer(c_int) function file_info(name) bind(C, name="file_info")
      !! Inquire about provided path. Returns 1 if path is directory, 
      !! 0 if path is file, and -1 if path is invalid, inaccessible or missing.
      import
      character(kind=c_char), intent(in) :: name(*)
      !! Path to inquire.
    end function


  end interface

contains

logical function is_dir(path)
  !! Return `.True.` if the path points to a directory. `.False.` will be returned
  !! if the path is invalid, inaccessible or missing, or if it points to something
  !! other than a directory.
  !!
  !! @warning
  !! I didn't check if this function is portable
  !! @endwarning
  !!
  !! Example:
  !! ```Fortran  
  !! print *, is_dir("/path/to/dir")  
  !! >>> .True.
  !! print *, is_dir("/path/to/file.txt")  
  !! >>> .False.
  !! ```  
  implicit none
  character(*), intent(in) :: path
  integer :: stat

  stat = file_info(trim(path) // C_NULL_CHAR)
  is_dir = (stat == 1)

end function is_dir


logical function is_file(path)
  !! Return `.True.` if the path points to a file. `.False.` will be returned
  !! if the path is invalid, inaccessible or missing, or if it points to something
  !! other than a file.
  !!
  !! @warning
  !! I didn't check if this function is portable
  !! @endwarning
  !!
  !! Example:
  !! ```Fortran  
  !! print *, is_file("/path/to/file.txt")  
  !! >>> .True.
  !! print *, is_file("/path/to/dir")  
  !! >>> .False.
  !! ``` 
  implicit none
  character(*), intent(in) :: path
  integer :: stat

  stat = file_info(trim(path) // C_NULL_CHAR)
  is_file = (stat == 0)

end function is_file


function iterdir (path) result (out)
  !! A sequence of calls to this function returns files and folder in the provided path.
  !!
  !! On a first call, the function opens and sets the directory to iterate. In subsequent
  !! calls, the function iterates through all files and folder in current path.
  !!
  !! Once the list is exhausted, a call to `iterdir`, and all subsequent calls to
  !! this function return a `null` character. 
  !!
  !! Example:
  !! ```Fortan
  !! file = iterdir("/path/to/dir")
  !! do while (file /= char(0))
  !!   print *, file
  !!   file = iterdir()
  !! end do
  !! >>> "."
  !! ... ".."
  !! ... "file.txt"
  !! ... "folder"
  !! ```
  implicit none
  character(:), allocatable :: out
  !! Returns next files or folder in the current path. 
  character(*), intent(in), optional :: path
  !! Initialize directory path.
  type(c_ptr), save :: fp
  character(256) :: name
  character(kind=c_char), pointer :: c_name(:)
  type(c_ptr) :: ptr
  integer :: i, status
  
  ! Initialize or load stored copy
  if (present(path)) then
    if (c_associated(fp)) then
      status = closedir(fp)
      if (status /= 0) error stop "Cannot close file"
    end if
    fp = opendir(trim(path) // C_NULL_CHAR)
    if (.not. c_associated(fp)) then
      out = char(0)
      return
    end if
  endif

  ! Get next file in the iterator
  ptr = f_readdir(fp)
  call c_f_pointer(ptr, c_name, [len(c_name)])
  if (.not. c_associated(ptr)) then
    out = char(0)
    return
  end if

  ! C to Fortran string
  name = ""
  do i = 1, len(name)
      if (c_name(i) == C_NULL_CHAR) exit
      name(i:i) = c_name(i)
  end do
  out = trim(name)

end function iterdir  


function realpath (path) result (out)
  !! The `realpath` function shall derive, from the pathname pointed to 
  !! by `path`, an absolute pathname that names the same file, whose resolution
  !! does not involve `.`, `..`, or symbolic links
  !!
  !! Example:
  !! ```Fortran
  !! out = realpath(".")
  !! >>> "/path/to/folder"
  !! ```
  use iso_c_binding
  implicit none
  character(*), intent(in) :: path
  !! String containing path.
  character(:), allocatable :: out
  !! String containing absolute path.
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


function joinpath (p1, p2, p3, p4, p5, p6, p7, p8, p9) result (out)
  !! Return concatenated pathname components, effectively constructing valid path.
  !! It ensures cross-platform compatibility by properly joining the components.
  !! 
  !! Example:
  !! ```Fortran
  !! print *, joinpath("folder", "file.txt")
  !! >>> "path/file.txt"
  !! print *, joinpath("folder", "folder", "file.txt")
  !! >>> "folder/folder/file.txt"
  !! ```
  implicit none
  character(*), intent(in) :: p1 
  character(*), intent(in), optional ::p2, p3, p4, p5, p6, p7, p8, p9
  character(:), allocatable :: out
  !! String containing concatenated path.

  out = p1 
  if (present(p2)) out = join_two_paths(out, p2)
  if (present(p3)) out = join_two_paths(out, p3)
  if (present(p4)) out = join_two_paths(out, p4)
  if (present(p5)) out = join_two_paths(out, p5)
  if (present(p6)) out = join_two_paths(out, p6)
  if (present(p7)) out = join_two_paths(out, p7)
  if (present(p8)) out = join_two_paths(out, p8)
  if (present(p9)) out = join_two_paths(out, p9)

end function joinpath


function join_two_paths (path1, path2) result (out)
  !! Concatenate two paths
  implicit none
  character(*), intent(in) :: path1, path2
  !! Path names
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

end function join_two_paths


function filename (path) result (out)
  !! Return the file name of pathname `path`. If pathname is a folder it will return an empty string.
  !!
  !! Example:
  !! ```Fortran
  !! print *, filename("/path/to/file.txt")
  !! >>> "file.txt"
  !! print *, filename("/path/to/")
  !! >>> ""
  !! ```
  implicit none
  character(:), allocatable :: out
  !! Path without extension or parent directories.
  character(*), intent(in) :: path
  !! Input path.
  integer :: i

  i = 1 + index(path, DIR_SEPARATOR, BACK=.true.)
  out = path(i:len_trim(path))

end function filename


function basename (path) result (out)
  !! Return the file name of the path.
  !! 
  !! Example:
  !! ```Fortran
  !! print *, basename("/path/to/file.txt")
  !! >>> "file"
  !! print *, basename("/path/to/")
  !! >>> ""
  !! ```
  implicit none
  character(:), allocatable :: out
  !! Input path parent directories.
  character(*), intent(in) :: path
  !! Input path.
  integer :: i, j

  i = 1 + index(path, DIR_SEPARATOR, BACK=.true.)
  j = index(path, ".", BACK=.true.)
  j = merge(j-1, len_trim(path), j /= 0)
  out = path(i:j)

end function basename


function dirname (path) result (out)
  !! Return the parent of path.
  !!
  !! Example:
  !! ```Fortran
  !! print *, dirname("/path/to/file.txt")
  !! >>> "/path/to"
  !! print *, dirname("/path/to")
  !! >>> "/path"
  !! print *, dirname("file.txt")
  !! >>> ""
  !! ```
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
  !! Return the extension name of path.
  !!
  !! Example:
  !! ```Fortran
  !! print *, extname("/path/to/file.txt")
  !! >>> "txt"
  !! print *, extname("/path/to/")
  !! >>> ""
  !! ```
  implicit none
  character(:), allocatable :: out
  !! Path extension.
  character(*), intent(in) :: path
  !! Input path.
  character(:), allocatable :: tmp
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
  !! Backup a existing file i.e. checks if `file` already exists and renames it to `#file.{n}#`
  !! where `n` in increasing integer if names is taken.
  !!
  !! Example:
  !! ```Fortran
  !! call backup("file.txt", status)
  !! if (status != 0) error stop "Backup failed"
  !! ```
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
      call rename(trim(file), newfile, STATUS=stat)
      if (present(status)) status = stat
      return
    end if
  end do

  ! If we got to this point, something has failed
  if (present(status)) status = -1

end subroutine backup

end module xslib_pathlib