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

module xslib_sort
  !! Module with different sorting algorithms.
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: swap, sort, qsort, msort, hsort

  interface swap
    !! Swap values of `a` and `b`.
    !! 
    !! Example:
    !! ```Fortran
    !! print *, a, b
    !! >>> 1.0, 2.0
    !! call swap(a, b)
    !! print *, a, b
    !! >>> 2.0, 1.0
    !! ```
    module procedure :: swap_i32, swap_i64, swap_r32, swap_r64, swap_c
  end interface swap

contains 

subroutine swap_i32 (a, b)
  implicit none
  integer(INT32), intent(inout) :: a
  !! Values to be swapped. Must be same `b`.  
  integer(INT32), intent(inout) :: b
  !! Values to be swapped. Must be same `a`.  
  integer(INT32) :: temp

  temp = a
  a = b
  b = temp

end subroutine swap_i32


subroutine swap_i64 (a, b)
  implicit none
  integer(INT64), intent(inout) :: a, b
  integer(INT64) :: temp
  
  temp = a
  a = b
  b = temp

end subroutine swap_i64


subroutine swap_r32 (a, b)
  implicit none
  real(REAL32), intent(inout) :: a, b
  real(REAL32) :: temp
  
  temp = a
  a = b
  b = temp

end subroutine swap_r32


subroutine swap_r64 (a, b)
  implicit none
  real(REAL64), intent(inout) :: a, b
  real(REAL64) :: temp
  
  temp = a
  a = b
  b = temp

end subroutine swap_r64


subroutine swap_c (a, b)
  implicit none
  character(*), intent(inout) :: a, b
  character(len(a)) :: temp
  
  temp = a
  a = b
  b = temp

end subroutine swap_c


subroutine sort (array, kind, order)
  !! Sort input array in ascending order. Different sorting algorithm can be
  !! selected: `quicksort`, `mergesort`, or `heapsort`. Default is `quicksort`.
  !! Order contains argument sort order from original array.
  !!
  !! Example:
  !! ```Fortran
  !! array = [1.0, 4.0, 3.0, 2.0]
  !! call sort(array, kind="quicksort", kind=order)
  !! print *, array
  !! >>> [1.0, 2.0, 3.0, 4.0]
  !! print *, order
  !! >>> [1, 4, 3, 2]
  !! ```
  implicit none
  class(*), intent(inout) :: array(:)
  !! Input array to be sorted. Supports array of any KIND.
  character(*), intent(in), optional :: kind
  !! Sorting algorithm: `quicksort` (default), `mergesort`, or `heapsort`.
  integer, intent(out), optional :: order(size(array))
  !! Value order in sorted array in respect to original. Same size as `array`.

  if (.not. present(kind)) then
    call qsort (array, order)
  else
    select case (kind)
    case ("quicksort", "QUICKSORT")
      call qsort (array, order)
    case ("mergesort", "MERGESORT")
      call msort (array, order)
    case ("heapsort", "HEAPSORT")
      call hsort (array, order)
    case default
      error stop "Unsupported sorting algorithm"
    end select
  end if

end subroutine sort


subroutine qsort (array, order)
  !! Sort input array in ascending order using using [Quicksort](https://en.wikipedia.org/wiki/Quicksort)
  !! algorithm. Order contains argument sort order from original array.
  !!
  !! Example
  !! ```Fortran
  !! array = [1.0, 4.0, 3.0, 2.0]
  !! call qsort(array, ORDER=order)
  !! print *, array
  !! >>> [1.0, 2.0, 3.0, 4.0]
  !! print *, order
  !! >>> [1, 4, 3, 2]
  !! ```
  ! Source: https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran
  implicit none
  class(*), intent(inout) :: array(:)
  !!  Input array to be sorted. Supports array of any KIND.
  integer, intent(out), optional :: order(size(array))
  !! Value order in sorted array in respect to original. Same size as `array`.
  ! logical, intent(in), optional :: reverse
  ! If `.True.` will sort the list descending. Default is `.False.`
  integer :: i, temp(size(array))

  temp = [(i, i = 1, size(array))]
  select type (array)
  type is (integer(INT32))
    call qsort_i32 (array, temp)
  type is (integer(INT64))
    call qsort_i64 (array, temp) 
  type is (real(REAL32))
    call qsort_r32 (array, temp) 
  type is (real(REAL64))
    call qsort_r64 (array, temp)
  type is (character(*))
    call qsort_c (array, temp)
  class default
    error stop "Unsupported KIND of variable"
  end select
  if (present(order)) order = temp

end subroutine qsort


recursive subroutine qsort_i32 (array, order)
  implicit none
  integer(INT32), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  integer(INT32) :: pivot
  integer :: left, right
  real :: random

  if (size(array) > 1) then
    call random_number (random)
    pivot = array(int(random * (size(array) - 1)) + 1)
    left = 1
    right = size(array)
    do
      do while (array(right) > pivot)
        right = right - 1
      end do
      do while (array(left) < pivot)
        left = left + 1
      end do
      if (left >= right) exit
      call swap_i32 (array(left), array(right))
      call swap_i32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_i32 (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_i32 (array(right+1:), order(right+1:))
    end if
  end if

end subroutine qsort_i32 


recursive subroutine qsort_i64 (array, order)
  implicit none
  integer(INT64), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  integer(INT64) :: pivot
  integer :: left, right
  real :: random

  if (size(array) > 1) then
    call random_number (random)
    pivot = array(int(random * (size(array) - 1)) + 1)
    left = 1
    right = size(array)
    do
      do while (array(right) > pivot)
        right = right - 1
      end do
      do while (array(left) < pivot)
        left = left + 1
      end do
      if (left >= right) exit
      call swap_i64 (array(left), array(right))
      call swap_i32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_i64 (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_i64 (array(right+1:), order(right+1:))
    end if
  end if  

end subroutine qsort_i64 


recursive subroutine qsort_r32 (array, order)
  implicit none
  real(REAL32), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  real(REAL32) :: pivot
  integer :: left, right
  real :: random

  if (size(array) > 1) then
    call random_number (random)
    pivot = array(int(random * (size(array) - 1)) + 1)
    left = 1
    right = size(array)
    do
      do while (array(right) > pivot)
        right = right - 1
      end do
      do while (array(left) < pivot)
        left = left + 1
      end do
      if (left >= right) exit
      call swap_r32 (array(left), array(right))
      call swap_i32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_r32 (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_r32 (array(right+1:), order(right+1:))
    end if
  end if

end subroutine qsort_r32 


recursive subroutine qsort_r64 (array, order)
  implicit none
  real(REAL64), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  real(REAL64) :: pivot
  integer :: left, right
  real :: random

  if (size(array) > 1) then
    call random_number (random)
    pivot = array(int(random * (size(array) - 1)) + 1)
    left = 1
    right = size(array)
    do
      do while (array(right) > pivot)
        right = right - 1
      end do
      do while (array(left) < pivot)
        left = left + 1
      end do
      if (left >= right) exit
      call swap_r64 (array(left), array(right))
      call swap_i32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_r64 (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_r64 (array(right+1:), order(right+1:))
    end if
  end if

end subroutine qsort_r64


recursive subroutine qsort_c (array, order)
  implicit none
  character(*), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  character(len(array)) :: pivot
  integer :: left, right
  real :: random

  if (size(array) > 1) then
    call random_number (random)
    pivot = array(int(random * (size(array) - 1)) + 1)
    left = 1
    right = size(array)
    do
      do while (array(right) > pivot)
        right = right - 1
      end do
      do while (array(left) < pivot)
        left = left + 1
      end do
      if (left >= right) exit
      call swap_c (array(left), array(right))
      call swap_i32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_c (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_c (array(right+1:), order(right+1:))
    end if
  end if

end subroutine qsort_c


subroutine msort (array, order)
  !! Sort input array in ascending order using using [Merge sort](https://en.wikipedia.org/wiki/Merge_sort)
  !! algorithm. Order contains argument sort order from original array.
  !!
  !! Example
  !! ```Fortran
  !! array = [1.0, 4.0, 3.0, 2.0]
  !! call msort(array, ORDER=order)
  !! print *, array
  !! >>> [1.0, 2.0, 3.0, 4.0]
  !! print *, order
  !! >>> [1, 4, 3, 2]
  !! ```
  ! See: https://rosettacode.org/wiki/Sorting_algorithms/Merge_sort
  implicit none
  class(*), intent(inout) :: array(:)
  integer, intent(out), optional :: order(size(array))
  integer :: i, temp(size(array))

  temp = [(i, i = 1, size(array))]
  select type (array)
  type is (integer(INT32))
    call msort_i32 (array, temp)
  type is (integer(INT64))
    call msort_i64 (array, temp) 
  type is (real(REAL32))
    call msort_r32 (array, temp) 
  type is (real(REAL64))
    call msort_r64 (array, temp)
  type is (character(*))
    call msort_c (array, temp)
  class default
    error stop "Unsupported KIND of variable"
  end select
  if (present(order)) order = temp

end subroutine msort


recursive subroutine msort_i32 (array, order)
  integer(INT32), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  integer(INT32) :: temp(size(array))
  integer :: cnt(size(array))
  integer :: i, j, k, half

  if (size(array) < 2) then
    continue
  else if (size(array) == 2) then
    if (array(1) > array(2)) then
      call swap_i32 (array(1), array(2))
      call swap_i32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_i32 (array(:half), order(:half))
    call msort_i32 (array(half+1:), order(half+1:))
    if (array(half) > array(half+1)) then
      i = 1; j = half + 1
      do k = 1, size(array)
        if (i <= half .and. j <= size(array)) then
          if (array(i) <= array(j)) then
            temp(k) = array(i)
            cnt(k) = order(i)
            i = i + 1
          else
            temp(k) = array(j)
            cnt(k) = order(j)
            j = j + 1
          end if
        else if (i <= half) then
          temp(k) = array(i)
          cnt(k) = order(i)
          i = i + 1
        else if (j <= size(array)) then
          temp(k) = array(j)
          cnt(k) = order(j)
          j = j + 1
        end if
      end do
      array = temp
      order = cnt
    end if
  end if  

end subroutine msort_i32


recursive subroutine msort_i64 (array, order)
  integer(INT64), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  integer(INT64) :: temp(size(array))
  integer :: cnt(size(array))
  integer :: i, j, k, half

  if (size(array) < 2) then
    continue
  else if (size(array) == 2) then
    if (array(1) > array(2)) then
      call swap_i64 (array(1), array(2))
      call swap_i32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_i64 (array(:half), order(:half))
    call msort_i64 (array(half+1:), order(half+1:))
    if (array(half) > array(half+1)) then
      i = 1; j = half + 1
      do k = 1, size(array)
        if (i <= half .and. j <= size(array)) then
          if (array(i) <= array(j)) then
            temp(k) = array(i)
            cnt(k) = order(i)
            i = i + 1
          else
            temp(k) = array(j)
            cnt(k) = order(j)
            j = j + 1
          end if
        else if (i <= half) then
          temp(k) = array(i)
          cnt(k) = order(i)
          i = i + 1
        else if (j <= size(array)) then
          temp(k) = array(j)
          cnt(k) = order(j)
          j = j + 1
        end if
      end do
      array = temp
      order = cnt
    end if
  end if  

end subroutine msort_i64


recursive subroutine msort_r32 (array, order)
  implicit none
  real(REAL32), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  real(REAL32) :: temp(size(array))
  integer :: cnt(size(array))
  integer :: i, j, k, half

  if (size(array) < 2) then
    continue
  else if (size(array) == 2) then
    if (array(1) > array(2)) then
      call swap_r32 (array(1), array(2))
      call swap_i32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_r32 (array(:half), order(:half))
    call msort_r32 (array(half+1:), order(half+1:))
    if (array(half) > array(half+1)) then
      i = 1; j = half + 1
      do k = 1, size(array)
        if (i <= half .and. j <= size(array)) then
          if (array(i) <= array(j)) then
            temp(k) = array(i)
            cnt(k) = order(i)
            i = i + 1
          else
            temp(k) = array(j)
            cnt(k) = order(j)
            j = j + 1
          end if
        else if (i <= half) then
          temp(k) = array(i)
          cnt(k) = order(i)
          i = i + 1
        else if (j <= size(array)) then
          temp(k) = array(j)
          cnt(k) = order(j)
          j = j + 1
        end if
      end do
      array = temp
      order = cnt
    end if
  end if 

end subroutine msort_r32


recursive subroutine msort_r64 (array, order)
  implicit none
  real(REAL64), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  real(REAL64) :: temp(size(array))
  integer :: cnt(size(array))
  integer :: i, j, k, half

  if (size(array) < 2) then
    continue
  else if (size(array) == 2) then
    if (array(1) > array(2)) then
      call swap_r64 (array(1), array(2))
      call swap_i32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_r64 (array(:half), order(:half))
    call msort_r64 (array(half+1:), order(half+1:))
    if (array(half) > array(half+1)) then
      i = 1; j = half + 1
      do k = 1, size(array)
        if (i <= half .and. j <= size(array)) then
          if (array(i) <= array(j)) then
            temp(k) = array(i)
            cnt(k) = order(i)
            i = i + 1
          else
            temp(k) = array(j)
            cnt(k) = order(j)
            j = j + 1
          end if
        else if (i <= half) then
          temp(k) = array(i)
          cnt(k) = order(i)
          i = i + 1
        else if (j <= size(array)) then
          temp(k) = array(j)
          cnt(k) = order(j)
          j = j + 1
        end if
      end do
      array = temp
      order = cnt
    end if
  end if 

end subroutine msort_r64


recursive subroutine msort_c (array, order)
  implicit none
  character(*), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  character(len(array)) :: temp(size(array))
  integer :: cnt(size(array))
  integer :: i, j, k, half

  if (size(array) < 2) then
    continue
  else if (size(array) == 2) then
    if (array(1) > array(2)) then
      call swap_c (array(1), array(2))
      call swap_i32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_c (array(:half), order(:half))
    call msort_c (array(half+1:), order(half+1:))
    if (array(half) > array(half+1)) then
      i = 1; j = half + 1
      do k = 1, size(array)
        if (i <= half .and. j <= size(array)) then
          if (array(i) <= array(j)) then
            temp(k) = array(i)
            cnt(k) = order(i)
            i = i + 1
          else
            temp(k) = array(j)
            cnt(k) = order(j)
            j = j + 1
          end if
        else if (i <= half) then
          temp(k) = array(i)
          cnt(k) = order(i)
          i = i + 1
        else if (j <= size(array)) then
          temp(k) = array(j)
          cnt(k) = order(j)
          j = j + 1
        end if
      end do
      array = temp
      order = cnt
    end if
  end if 

end subroutine msort_c


subroutine hsort (array, order)
  !! Sort input array in ascending order using using [Heapsort](https://en.wikipedia.org/wiki/Heapsort)
  !! algorithm. Order contains argument sort order from original array.
  !!
  !! Example
  !! ```Fortran
  !! array = [1.0, 4.0, 3.0, 2.0]
  !! call hsort(array, ORDER=order)
  !! print *, array
  !! >>> [1.0, 2.0, 3.0, 4.0]
  !! print *, order
  !! >>> [1, 4, 3, 2]
  !! ```
  ! Source: https://rosettacode.org/wiki/Sorting_algorithms/Heapsort#Fortran
  implicit none
  class(*), intent(inout) :: array(:)
  integer, intent(out), optional :: order(size(array))
  integer :: i, temp(size(array))

  temp = [(i, i = 1, size(array))]
  select type (array)
  type is (integer(INT32))
    call hsort_i32 (array, temp)
  type is (integer(INT64))
    call hsort_i64 (array, temp) 
  type is (real(REAL32))
    call hsort_r32 (array, temp) 
  type is (real(REAL64))
    call hsort_r64 (array, temp)
  type is (character(*))
    call hsort_c (array, temp)
  class default
    error stop "Unsupported KIND of variable"
  end select
  if (present(order)) order = temp

end subroutine hsort


subroutine hsort_i32 (array, order)
  implicit none
  integer(INT32), intent(inout) :: array(0:)
  integer, intent(inout) :: order(0:size(array)-1)
  integer :: root, child, start, bottom

  do start = (size(array) - 2) / 2, 0, -1
    root = start
    do while (root * 2 + 1 < size(array))
      child = root * 2 + 1
      if (child + 1 < size(array)) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_i32 (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_i32 (array(bottom), array(0))
    call swap_i32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_i32 (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_i32


subroutine hsort_i64 (array, order)
  implicit none
  integer(INT64), intent(inout) :: array(0:)
  integer, intent(inout) :: order(0:size(array)-1)
  integer :: root, child, start, bottom

  do start = (size(array) - 2) / 2, 0, -1
    root = start
    do while (root * 2 + 1 < size(array))
      child = root * 2 + 1
      if (child + 1 < size(array)) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_i64 (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_i64 (array(bottom), array(0))
    call swap_i32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_i64 (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_i64


subroutine hsort_r32 (array, order)
  implicit none
  real(REAL32), intent(inout) :: array(0:)
  integer, intent(inout) :: order(0:size(array)-1)
  integer :: root, child, start, bottom

  do start = (size(array) - 2) / 2, 0, -1
    root = start
    do while (root * 2 + 1 < size(array))
      child = root * 2 + 1
      if (child + 1 < size(array)) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_r32 (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_r32 (array(bottom), array(0))
    call swap_i32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_r32 (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_r32


subroutine hsort_r64 (array, order)
  implicit none
  real(REAL64), intent(inout) :: array(0:)
  integer, intent(inout) :: order(0:size(array)-1)
  integer :: root, child, start, bottom

  do start = (size(array) - 2) / 2, 0, -1
    root = start
    do while (root * 2 + 1 < size(array))
      child = root * 2 + 1
      if (child + 1 < size(array)) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_r64 (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_r64 (array(bottom), array(0))
    call swap_i32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_r64 (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_r64


subroutine hsort_c (array, order)
  implicit none
  character(*), intent(inout) :: array(0:)
  integer, intent(inout) :: order(0:size(array)-1)
  integer :: root, child, start, bottom

  do start = (size(array) - 2) / 2, 0, -1
    root = start
    do while (root * 2 + 1 < size(array))
      child = root * 2 + 1
      if (child + 1 < size(array)) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_c (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_c (array(bottom), array(0))
    call swap_i32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_c (array(root), array(child))
        call swap_i32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_c

end module xslib_sort
