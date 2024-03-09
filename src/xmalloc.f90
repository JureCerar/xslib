! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2022 Jure Cerar
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

module xslib_xmalloc
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: xmalloc, xcalloc, xrealloc

  ! %%%
  ! # `XMALLOC` - Memory allocation
  !   Modules `xslib_xmalloc` contains functions for memory allocation.
  !   Currently supported types are: `INT32`, `INT64`, `REAL32`, `REAL64`, and `CHARACTER`
  ! %%%

  ! Length of error string
  integer, parameter :: ERRLEN = 128 

  interface xmalloc
    module procedure :: xmalloc_int32, xmalloc_int64, xmalloc_real32, xmalloc_real64, xmalloc_bool, xmalloc_char
  end interface xmalloc

  interface xcalloc
    module procedure :: xcalloc_int32, xcalloc_int64, xcalloc_real32, xcalloc_real64, xcalloc_bool, xcalloc_char
  end interface xcalloc

  interface xrealloc
    module procedure :: xrealloc_int32, xrealloc_int64, xrealloc_real32, xrealloc_real64, xrealloc_bool, xrealloc_char
  end interface xrealloc  

contains

subroutine xmalloc_int32 (object, spec, stat, errmsg)
  ! %%%
  ! ## `XMALLOC` - Allocate memory
  ! #### DESCRIPTION
  !   Allocates a block of memory for an array of num. elements. It's just
  !   a nice wrapper for built-in allocation but with error handling.
  ! #### USAGE
  !   ```Fortran
  !   call xmalloc(object, spec, stat=stat, errmsg=errmsg)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), ALLOCATABLE, dimension(..), intent(INOUT) :: object`
  !     Object to be allocated.
  !   `integer, dimension(:), intent(IN) :: spec`
  !     Object shape specification. Must be same size as `rank(object)`.
  !   * `integer, intent(OUT), OPTIONAL :: stat`
  !     Error status code. Returns zero if no error. 
  !   * `character(*), intent(OUT), OPTIONAL :: errmsg`
  !     Error status message. Empty if no error. 
  ! #### EXAMPLE
  !   ```Fortran
  !   > call xmalloc(array, [10, 10], STAT=status, ERRMSG=message)
  !   ```
  ! %%%
  implicit none
  integer(INT32), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xmalloc_int32


subroutine xmalloc_int64 (object, spec, stat, errmsg)
  implicit none
  integer(INT64), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xmalloc_int64


subroutine xmalloc_real32 (object, spec, stat, errmsg)
  implicit none
  real(REAL32), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xmalloc_real32


subroutine xmalloc_real64 (object, spec, stat, errmsg)
  implicit none
  real(REAL64), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xmalloc_real64


subroutine xmalloc_bool (object, spec, stat, errmsg)
  implicit none
  logical, allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xmalloc_bool


subroutine xmalloc_char (object, spec, stat, errmsg)
  implicit none
  character(*), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xmalloc_char


subroutine xcalloc_int32 (object, spec, stat, errmsg)
  ! %%%
  ! ## `XCALLOC` - Allocate and initialize memory
  ! #### DESCRIPTION
  !   Allocates and initializes a block of memory for an array of num. elements. 
  !   It's just a nice wrapper for built-in allocation but with error handling.
  ! #### USAGE
  !   ```Fortran
  !   call xcalloc(object, spec, stat=stat, errmsg=errmsg)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), ALLOCATABLE, dimension(..), intent(INOUT) :: object`
  !     Object to be allocated.
  !   `integer, dimension(:), intent(IN) :: spec`
  !     Object shape specification. Must be same size as `rank(object)`.
  !   * `integer, intent(OUT), OPTIONAL :: stat`
  !     Error status code. Returns zero if no error. 
  !   * `character(*), intent(OUT), OPTIONAL :: errmsg`
  !     Error status message. Empty if no error. 
  ! #### EXAMPLE
  !   ```Fortran
  !   > call xcalloc(array, [10, 10], STAT=status, ERRMSG=message)
  !   ```
  ! %%%
  implicit none
  integer(INT32), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
      object = 0_INT32
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
      object = 0_INT32
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xcalloc_int32


subroutine xcalloc_int64 (object, spec, stat, errmsg)
  implicit none
  integer(INT64), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
      object = 0_INT64
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
      object = 0_INT64
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xcalloc_int64


subroutine xcalloc_real32 (object, spec, stat, errmsg)
  implicit none
  real(REAL32), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
      object = 0.0_REAL32
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
      object = 0.0_REAL32
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xcalloc_real32


subroutine xcalloc_real64 (object, spec, stat, errmsg)
  implicit none
  real(REAL64), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
      object = 0.0_REAL64
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
      object = 0.0_REAL64
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xcalloc_real64


subroutine xcalloc_bool (object, spec, stat, errmsg)
  implicit none
  logical, allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
      object = .True.
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
      object = .True.
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select    
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xcalloc_bool


subroutine xcalloc_char (object, spec, stat, errmsg)
  implicit none
  character(*), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(ERRLEN) :: message 
  integer :: status

  if (rank(object) /= size(spec)) then
    message = "Specified SIZE does not match object RANK"
    status = 1
  else
    select rank (object)
    rank (1)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1)), STAT=status, ERRMSG=message)
      object = ""
    rank (2)
      if (allocated(object)) deallocate (object, STAT=status)
      allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
      object = ""
    rank default
      message = "Unsupported RANK size"
      status = 1
    end select    
  end if

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xcalloc_char


subroutine xrealloc_int32 (object, spec, stat, errmsg)
  ! %%%
  ! ## `XREALLOC` - Reallocate memory
  ! #### DESCRIPTION
  !   Changes the size of memory block. Allocates a new memory block if
  !   not allocated. The content of the memory block is preserved up to
  !   the lesser of the new and old sizes, even if the block is moved to
  !   a new location. If the new size is larger, the value of the newly 
  !   allocated portion is indeterminate.
  ! #### USAGE
  !   ```Fortran
  !   call xrealloc(object, spec, stat=stat, errmsg=errmsg)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), ALLOCATABLE, dimension(..), intent(INOUT) :: object`
  !     Object to be allocated.
  !   * `integer, dimension(:), intent(IN) :: spec`
  !     Object shape specification. Must be same size as `rank(object)`.
  !   * `integer, intent(OUT), OPTIONAL :: stat`
  !     Error status code. Returns zero if no error. 
  !   * `character(*), intent(OUT), OPTIONAL :: errmsg`
  !     Error status message. Empty if no error. 
  ! #### EXAMPLE
  !   ```Fortran
  !   > call xrealloc(array, [10, 10], STAT=status, ERRMSG=message)
  !   ```
  ! %%%
  implicit none
  integer(INT32), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  integer(INT32), allocatable :: temp(:), temp2(:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j

  catch: block
    if (rank(object) /= size(spec)) then
      message = "Specified SIZE does not match object RANK"
      status = 1
    else if (allocated(object)) then
      select rank (object)
      rank (1)
        allocate (temp(spec(1)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp), size(object))
        temp(:i) = object(:i)
        call move_alloc(temp, object)
      rank (2)
        allocate (temp2(spec(1), spec(2)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp2, DIM=1), size(object, DIM=1))
        j = min(size(temp2, DIM=2), size(object, DIM=2))
        temp2(:i,:j) = object(:i,:j)
        call move_alloc(temp2, object)
      rank default
        message = "Unsupported RANK size"
        status = 1   
      end select 
    else
      select rank (object)
      rank (1)
        allocate (object(spec(1)), STAT=status, ERRMSG=message)
        object = 0
      rank (2)
        allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
        object = 0
      rank default
        message = "Unsupported RANK size"
        status = 1
      end select
    end if
  end block catch

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xrealloc_int32


subroutine xrealloc_int64 (object, spec, stat, errmsg)
  implicit none
  integer(INT64), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  integer(INT64), allocatable :: temp(:), temp2(:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j

  catch: block
    if (rank(object) /= size(spec)) then
      message = "Specified SIZE does not match object RANK"
      status = 1
    else if (allocated(object)) then
      select rank (object)
      rank (1)
        allocate (temp(spec(1)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp), size(object))
        temp(:i) = object(:i)
        call move_alloc(temp, object)
      rank (2)
        allocate (temp2(spec(1), spec(2)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp2, DIM=1), size(object, DIM=1))
        j = min(size(temp2, DIM=2), size(object, DIM=2))
        temp2(:i,:j) = object(:i,:j)
        call move_alloc(temp2, object)
      rank default
        message = "Unsupported RANK size"
        status = 1   
      end select 
    else
      select rank (object)
      rank (1)
        allocate (object(spec(1)), STAT=status, ERRMSG=message)
        object = 0
      rank (2)
        allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
        object = 0
      rank default
        message = "Unsupported RANK size"
        status = 1
      end select
    end if
  end block catch
  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xrealloc_int64


subroutine xrealloc_real32 (object, spec, stat, errmsg)
  implicit none
  real(REAL32), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  real(REAL32), allocatable :: temp(:), temp2(:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j

  catch: block
    if (rank(object) /= size(spec)) then
      message = "Specified SIZE does not match object RANK"
      status = 1
    else if (allocated(object)) then
      select rank (object)
      rank (1)
        allocate (temp(spec(1)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp), size(object))
        temp(:i) = object(:i)
        call move_alloc(temp, object)
      rank (2)
        allocate (temp2(spec(1), spec(2)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp2, DIM=1), size(object, DIM=1))
        j = min(size(temp2, DIM=2), size(object, DIM=2))
        temp2(:i,:j) = object(:i,:j)
        call move_alloc(temp2, object)
      rank default
        message = "Unsupported RANK size"
        status = 1   
      end select 
    else
      select rank (object)
      rank (1)
        allocate (object(spec(1)), STAT=status, ERRMSG=message)
        object = 0.0
      rank (2)
        allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
        object = 0.0
      rank default
        message = "Unsupported RANK size"
        status = 1
      end select
    end if
  end block catch

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xrealloc_real32


subroutine xrealloc_real64 (object, spec, stat, errmsg)
  implicit none
  real(REAL64), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  real(REAL64), allocatable :: temp(:), temp2(:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j

  catch: block
    if (rank(object) /= size(spec)) then
      message = "Specified SIZE does not match object RANK"
      status = 1
    else if (allocated(object)) then
      select rank (object)
      rank (1)
        allocate (temp(spec(1)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp), size(object))
        temp(:i) = object(:i)
        call move_alloc(temp, object)
      rank (2)
        allocate (temp2(spec(1), spec(2)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp2, DIM=1), size(object, DIM=1))
        j = min(size(temp2, DIM=2), size(object, DIM=2))
        temp2(:i,:j) = object(:i,:j)
        call move_alloc(temp2, object)
      rank default
        message = "Unsupported RANK size"
        status = 1   
      end select 
    else
      select rank (object)
      rank (1)
        allocate (object(spec(1)), STAT=status, ERRMSG=message)
        object = 0.0
      rank (2)
        allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
        object = 0.0
      rank default
        message = "Unsupported RANK size"
        status = 1
      end select
    end if
  end block catch

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xrealloc_real64


subroutine xrealloc_bool (object, spec, stat, errmsg)
  implicit none
  logical, allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  logical, allocatable :: temp(:), temp2(:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j

  catch: block
    if (rank(object) /= size(spec)) then
      message = "Specified SIZE does not match object RANK"
      status = 1
    else if (allocated(object)) then
      select rank (object)
      rank (1)
        allocate (temp(spec(1)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp), size(object))
        temp(:i) = object(:i)
        call move_alloc(temp, object)
      rank (2)
        allocate (temp2(spec(1), spec(2)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp2, DIM=1), size(object, DIM=1))
        j = min(size(temp2, DIM=2), size(object, DIM=2))
        temp2(:i,:j) = object(:i,:j)
        call move_alloc(temp2, object)
      rank default
        message = "Unsupported RANK size"
        status = 1   
      end select 
    else
      select rank (object)
      rank (1)
        allocate (object(spec(1)), STAT=status, ERRMSG=message)
        object = .True.
      rank (2)
        allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
        object = .True.
      rank default
        message = "Unsupported RANK size"
        status = 1
      end select
    end if
  end block catch

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xrealloc_bool


subroutine xrealloc_char (object, spec, stat, errmsg)
  implicit none
  character(*), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(len(object)), allocatable :: temp(:), temp2(:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j

  catch: block
    if (rank(object) /= size(spec)) then
      message = "Specified SIZE does not match object RANK"
      status = 1
    else if (allocated(object)) then
      select rank (object)
      rank (1)
        allocate (temp(spec(1)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp), size(object))
        temp(:i) = object(:i)
        call move_alloc(temp, object)
      rank (2)
        allocate (temp2(spec(1), spec(2)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp2, DIM=1), size(object, DIM=1))
        j = min(size(temp2, DIM=2), size(object, DIM=2))
        temp2(:i,:j) = object(:i,:j)
        call move_alloc(temp2, object)
      rank default
        message = "Unsupported RANK size"
        status = 1   
      end select 
    else
      select rank (object)
      rank (1)
        allocate (object(spec(1)), STAT=status, ERRMSG=message)
        object = ""
      rank (2)
        allocate (object(spec(1), spec(2)), STAT=status, ERRMSG=message)
        object = ""
      rank default
        message = "Unsupported RANK size"
        status = 1
      end select
    end if
  end block catch

  if (present(stat)) then
    stat = status
  else if (status /= 0) then
    error stop message
  end if
  if (present(errmsg)) errmsg = trim(message)

end subroutine xrealloc_char

end module xslib_xmalloc
