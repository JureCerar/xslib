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

module xslib_memory
  !! Module with functions for memory allocation.
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: reallocate

  ! Length of error string
  integer, parameter :: ERRLEN = 128 

  interface reallocate
    !! Changes the size of memory block. Allocates a new memory block if
    !! not allocated. The content of the memory block is preserved up to
    !! the lesser of the new and old sizes, even if the block is moved to
    !! a new location. If the new size is larger, the value of the newly 
    !! allocated portion is indeterminate.
    !! 
    !! Example
    !! ```Fortran
    !! allocate(array(2), STAT=status, ERRMSG=message)
    !! array = [1.0, 2.0]
    !! call reallocate(array, [3], STAT=status, ERRMSG=message)
    !! print *, array
    !! >>> [1.0, 2.0, 0.0]
    !! ```
    module procedure :: reallocate_i32, reallocate_i64, reallocate_r32, reallocate_r64, reallocate_b, reallocate_c
  end interface reallocate  

contains


subroutine reallocate_i32 (object, spec, stat, errmsg)
  implicit none
  integer(INT32), allocatable, intent(inout) :: object(..)
  !! Object to be allocated.
  integer, intent(in) :: spec(:)
  !! Object shape specification. Must be same size as `rank(object)`.
  integer, intent(out), optional :: stat
  !! Error status code. Returns 0 if no error. 
  character(*), intent(out), optional :: errmsg
  !! Error status message. Empty if no error.
  integer(INT32), allocatable :: temp(:), temp2(:,:), temp3(:,:,:), temp4(:,:,:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j, k, l

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
      rank (3)
        allocate (temp3(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp3, DIM=1), size(object, DIM=1))
        j = min(size(temp3, DIM=2), size(object, DIM=2))
        k = min(size(temp3, DIM=3), size(object, DIM=3))
        temp3(:i,:j,:k) = object(:i,:j,:k)
        call move_alloc(temp3, object)
      rank (4)
        allocate (temp4(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp4, DIM=1), size(object, DIM=1))
        j = min(size(temp4, DIM=2), size(object, DIM=2))
        k = min(size(temp4, DIM=3), size(object, DIM=3))
        l = min(size(temp4, DIM=4), size(object, DIM=4))
        temp4(:i,:j,:k,:l) = object(:i,:j,:k,:l)
        call move_alloc(temp4, object)
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
      rank (3)
        allocate (object(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        object = 0
      rank (4)
        allocate (object(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
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

end subroutine reallocate_i32


subroutine reallocate_i64 (object, spec, stat, errmsg)
  implicit none
  integer(INT64), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  integer(INT64), allocatable :: temp(:), temp2(:,:), temp3(:,:,:), temp4(:,:,:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j, k, l

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
      rank (3)
        allocate (temp3(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp3, DIM=1), size(object, DIM=1))
        j = min(size(temp3, DIM=2), size(object, DIM=2))
        k = min(size(temp3, DIM=3), size(object, DIM=3))
        temp3(:i,:j,:k) = object(:i,:j,:k)
        call move_alloc(temp3, object)
      rank (4)
        allocate (temp4(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp4, DIM=1), size(object, DIM=1))
        j = min(size(temp4, DIM=2), size(object, DIM=2))
        k = min(size(temp4, DIM=3), size(object, DIM=3))
        l = min(size(temp4, DIM=4), size(object, DIM=4))
        temp4(:i,:j,:k,:l) = object(:i,:j,:k,:l)
        call move_alloc(temp4, object)
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
      rank (3)
        allocate (object(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        object = 0
      rank (4)
        allocate (object(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
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

end subroutine reallocate_i64


subroutine reallocate_r32 (object, spec, stat, errmsg)
  implicit none
  real(REAL32), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  real(REAL32), allocatable :: temp(:), temp2(:,:), temp3(:,:,:), temp4(:,:,:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j, k, l

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
      rank (3)
        allocate (temp3(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp3, DIM=1), size(object, DIM=1))
        j = min(size(temp3, DIM=2), size(object, DIM=2))
        k = min(size(temp3, DIM=3), size(object, DIM=3))
        temp3(:i,:j,:k) = object(:i,:j,:k)
        call move_alloc(temp3, object)
      rank (4)
        allocate (temp4(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp4, DIM=1), size(object, DIM=1))
        j = min(size(temp4, DIM=2), size(object, DIM=2))
        k = min(size(temp4, DIM=3), size(object, DIM=3))
        l = min(size(temp4, DIM=4), size(object, DIM=4))
        temp4(:i,:j,:k,:l) = object(:i,:j,:k,:l)
        call move_alloc(temp4, object)
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
      rank (3)
        allocate (object(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        object = 0.0
      rank (4)
        allocate (object(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
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

end subroutine reallocate_r32


subroutine reallocate_r64 (object, spec, stat, errmsg)
  implicit none
  real(REAL64), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  real(REAL64), allocatable :: temp(:), temp2(:,:), temp3(:,:,:), temp4(:,:,:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j, k, l

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
      rank (3)
        allocate (temp3(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp3, DIM=1), size(object, DIM=1))
        j = min(size(temp3, DIM=2), size(object, DIM=2))
        k = min(size(temp3, DIM=3), size(object, DIM=3))
        temp3(:i,:j,:k) = object(:i,:j,:k)
        call move_alloc(temp3, object)
      rank (4)
        allocate (temp4(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp4, DIM=1), size(object, DIM=1))
        j = min(size(temp4, DIM=2), size(object, DIM=2))
        k = min(size(temp4, DIM=3), size(object, DIM=3))
        l = min(size(temp4, DIM=4), size(object, DIM=4))
        temp4(:i,:j,:k,:l) = object(:i,:j,:k,:l)
        call move_alloc(temp4, object)
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
      rank (3)
        allocate (object(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        object = 0.0
      rank (4)
        allocate (object(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
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

end subroutine reallocate_r64


subroutine reallocate_b (object, spec, stat, errmsg)
  implicit none
  logical, allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  logical, allocatable :: temp(:), temp2(:,:), temp3(:,:,:), temp4(:,:,:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j, k, l

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
      rank (3)
        allocate (temp3(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp3, DIM=1), size(object, DIM=1))
        j = min(size(temp3, DIM=2), size(object, DIM=2))
        k = min(size(temp3, DIM=3), size(object, DIM=3))
        temp3(:i,:j,:k) = object(:i,:j,:k)
        call move_alloc(temp3, object)
      rank (4)
        allocate (temp4(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp4, DIM=1), size(object, DIM=1))
        j = min(size(temp4, DIM=2), size(object, DIM=2))
        k = min(size(temp4, DIM=3), size(object, DIM=3))
        l = min(size(temp4, DIM=4), size(object, DIM=4))
        temp4(:i,:j,:k,:l) = object(:i,:j,:k,:l)
        call move_alloc(temp4, object)
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
      rank (3)
        allocate (object(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        object = .True.
      rank (4)
        allocate (object(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
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

end subroutine reallocate_b


subroutine reallocate_c (object, spec, stat, errmsg)
  implicit none
  character(*), allocatable, intent(inout) :: object(..)
  integer, intent(in) :: spec(:)
  integer, intent(out), optional :: stat
  character(*), intent(out), optional :: errmsg
  character(len(object)), allocatable :: temp(:), temp2(:,:), temp3(:,:,:), temp4(:,:,:,:)
  character(ERRLEN) :: message 
  integer :: status, i, j, k, l

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
      rank (3)
        allocate (temp3(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp3, DIM=1), size(object, DIM=1))
        j = min(size(temp3, DIM=2), size(object, DIM=2))
        k = min(size(temp3, DIM=3), size(object, DIM=3))
        temp3(:i,:j,:k) = object(:i,:j,:k)
        call move_alloc(temp3, object)
      rank (4)
        allocate (temp4(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
        if (status /= 0) exit catch
        i = min(size(temp4, DIM=1), size(object, DIM=1))
        j = min(size(temp4, DIM=2), size(object, DIM=2))
        k = min(size(temp4, DIM=3), size(object, DIM=3))
        l = min(size(temp4, DIM=4), size(object, DIM=4))
        temp4(:i,:j,:k,:l) = object(:i,:j,:k,:l)
        call move_alloc(temp4, object)
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
      rank (3)
        allocate (object(spec(1), spec(2), spec(3)), STAT=status, ERRMSG=message)
        object = ""
      rank (4)
        allocate (object(spec(1), spec(2), spec(3), spec(4)), STAT=status, ERRMSG=message)
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

end subroutine reallocate_c

end module xslib_memory
