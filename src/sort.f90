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

module xslib_sort
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: swap, sort, qsort, msort, hsort

  interface swap
    module procedure :: swap_int32, swap_int64, swap_real32, swap_real64, swap_char
  end interface swap

contains 

! Swap values of A and B
subroutine swap_int32 (a, b)
  implicit none
  integer(INT32), intent(inout) :: a, b
  integer(INT32) :: temp

  temp = a
  a = b
  b = temp

end subroutine swap_int32

subroutine swap_int64 (a, b)
  implicit none
  integer(INT64), intent(inout) :: a, b
  integer(INT64) :: temp
  
  temp = a
  a = b
  b = temp

end subroutine swap_int64

subroutine swap_real32 (a, b)
  implicit none
  real(REAL32), intent(inout) :: a, b
  real(REAL32) :: temp
  
  temp = a
  a = b
  b = temp

end subroutine swap_real32

subroutine swap_real64 (a, b)
  implicit none
  real(REAL64), intent(inout) :: a, b
  real(REAL64) :: temp
  
  temp = a
  a = b
  b = temp

end subroutine swap_real64

subroutine swap_char (a, b)
  implicit none
  character(*), intent(inout) :: a, b
  character(len(a)) :: temp
  
  temp = a
  a = b
  b = temp

end subroutine swap_char

! Returns sorted array. Sorting algorithm can be selected: "quicksort", "mergesort", and "heapsort";
! default is "quicksort". Order contains argument sort order.
subroutine sort (array, kind, order)
  implicit none
  class(*), intent(inout) :: array(:)
  character(*), intent(in), optional :: kind
  integer, intent(out), optional :: order(size(array))

  if (.not. present(kind)) then
    call qsort (array, order)
  else
    select case (kind)
    case ("quicksort")
      call qsort (array, order)
    case ("mergesort")
      call msort (array, order)
    case ("heapsort")
      call hsort (array, order)
    case default
      error stop "Unsupported sort algorithm"
    end select
  end if

end subroutine sort

! Sort array using quicksort algorithm.
! SOURCE: https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran
subroutine qsort (array, order)
  implicit none
  class(*), intent(inout) :: array(:)
  integer, intent(out), optional :: order(size(array))
  integer :: i, temp(size(array))

  temp = [(i, i = 1, size(array))]
  select type (array)
  type is (integer(INT32))
    call qsort_int32 (array, temp)
  type is (integer(INT64))
    call qsort_int64 (array, temp) 
  type is (real(REAL32))
    call qsort_real32 (array, temp) 
  type is (real(REAL64))
    call qsort_real64 (array, temp)
  type is (character(*))
    call qsort_char (array, temp)
  class default
    error stop "Unsupported KIND of variable"
  end select
  if (present(order)) order = temp

end subroutine qsort

recursive subroutine qsort_int32 (array, order)
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
      call swap_int32 (array(left), array(right))
      call swap_int32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_int32 (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_int32 (array(right+1:), order(right+1:))
    end if
  end if 

end subroutine qsort_int32 

recursive subroutine qsort_int64 (array, order)
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
      call swap_int64 (array(left), array(right))
      call swap_int32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_int64 (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_int64 (array(right+1:), order(right+1:))
    end if
  end if  

end subroutine qsort_int64 

recursive subroutine qsort_real32 (array, order)
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
      call swap_real32 (array(left), array(right))
      call swap_int32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_real32 (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_real32 (array(right+1:), order(right+1:))
    end if
  end if

end subroutine qsort_real32 

recursive subroutine qsort_real64 (array, order)
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
      call swap_real64 (array(left), array(right))
      call swap_int32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_real64 (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_real64 (array(right+1:), order(right+1:))
    end if
  end if

end subroutine qsort_real64

recursive subroutine qsort_char (array, order)
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
      call swap_char (array(left), array(right))
      call swap_int32 (order(left), order(right))
      left = left + 1
      right = right - 1  
    end do
    if (1 < left - 1) then
      call qsort_char (array(:left-1), order(:left-1))
    end if
    if (right + 1 < size(array)) then
      call qsort_char (array(right+1:), order(right+1:))
    end if
  end if

end subroutine qsort_char

! Sort array using mergesort algorithm.
! SOURCE: https://rosettacode.org/wiki/Sorting_algorithms/Merge_sort
subroutine msort (array, order)
  implicit none
  class(*), intent(inout) :: array(:)
  integer, intent(out), optional :: order(size(array))
  integer :: i, temp(size(array))

  temp = [(i, i = 1, size(array))]
  select type (array)
  type is (integer(INT32))
    call msort_int32 (array, temp)
  type is (integer(INT64))
    call msort_int64 (array, temp) 
  type is (real(REAL32))
    call msort_real32 (array, temp) 
  type is (real(REAL64))
    call msort_real64 (array, temp)
  type is (character(*))
    call msort_char (array, temp)
  class default
    error stop "Unsupported KIND of variable"
  end select
  if (present(order)) order = temp

end subroutine msort

recursive subroutine msort_int32 (array, order)
  integer(INT32), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  integer(INT32) :: temp(size(array))
  integer :: cnt(size(array))
  integer :: i, j, k, half

  if (size(array) < 2) then
    continue
  else if (size(array) == 2) then
    if (array(1) > array(2)) then
      call swap_int32 (array(1), array(2))
      call swap_int32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_int32 (array(:half), order(:half))
    call msort_int32 (array(half+1:), order(half+1:))
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

end subroutine msort_int32

recursive subroutine msort_int64 (array, order)
  integer(INT64), intent(inout) :: array(:)
  integer, intent(inout) :: order(size(array))
  integer(INT64) :: temp(size(array))
  integer :: cnt(size(array))
  integer :: i, j, k, half

  if (size(array) < 2) then
    continue
  else if (size(array) == 2) then
    if (array(1) > array(2)) then
      call swap_int64 (array(1), array(2))
      call swap_int32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_int64 (array(:half), order(:half))
    call msort_int64 (array(half+1:), order(half+1:))
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

end subroutine msort_int64

recursive subroutine msort_real32 (array, order)
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
      call swap_real32 (array(1), array(2))
      call swap_int32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_real32 (array(:half), order(:half))
    call msort_real32 (array(half+1:), order(half+1:))
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

end subroutine msort_real32

recursive subroutine msort_real64 (array, order)
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
      call swap_real64 (array(1), array(2))
      call swap_int32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_real64 (array(:half), order(:half))
    call msort_real64 (array(half+1:), order(half+1:))
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

end subroutine msort_real64

recursive subroutine msort_char (array, order)
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
      call swap_char (array(1), array(2))
      call swap_int32 (order(1), order(2))
    end if 
  else
    half = (size(array) + 1) / 2
    call msort_char (array(:half), order(:half))
    call msort_char (array(half+1:), order(half+1:))
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

end subroutine msort_char

! Sort array using heapsort algorithm.
! SOURCE: https://rosettacode.org/wiki/Sorting_algorithms/Heapsort#Fortran
subroutine hsort (array, order)
  implicit none
  class(*), intent(inout) :: array(:)
  integer, intent(out), optional :: order(size(array))
  integer :: i, temp(size(array))

  temp = [(i, i = 1, size(array))]
  select type (array)
  type is (integer(INT32))
    call hsort_int32 (array, temp)
  type is (integer(INT64))
    call hsort_int64 (array, temp) 
  type is (real(REAL32))
    call hsort_real32 (array, temp) 
  type is (real(REAL64))
    call hsort_real64 (array, temp)
  type is (character(*))
    call hsort_char (array, temp)
  class default
    error stop "Unsupported KIND of variable"
  end select
  if (present(order)) order = temp

end subroutine hsort

subroutine hsort_int32 (array, order)
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
        call swap_int32 (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_int32 (array(bottom), array(0))
    call swap_int32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_int32 (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_int32

subroutine hsort_int64 (array, order)
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
        call swap_int64 (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_int64 (array(bottom), array(0))
    call swap_int32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_int64 (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_int64

subroutine hsort_real32 (array, order)
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
        call swap_real32 (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_real32 (array(bottom), array(0))
    call swap_int32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_real32 (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_real32

subroutine hsort_real64 (array, order)
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
        call swap_real64 (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_real64 (array(bottom), array(0))
    call swap_int32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_real64 (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_real64

subroutine hsort_char (array, order)
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
        call swap_char (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do      
  end do

  do bottom = size(array) - 1, 1, -1
    call swap_char (array(bottom), array(0))
    call swap_int32 (order(bottom), order(0))
    root = 0
    do while (root * 2 + 1 < bottom)
      child = root * 2 + 1
      if (child + 1 < bottom) then
        if (array(child) < array(child+1)) child = child + 1
      end if
      if (array(root) < array(child)) then
        call swap_char (array(root), array(child))
        call swap_int32 (order(root), order(child))
        root = child
      else
        exit
      end if  
    end do   
  end do

end subroutine hsort_char

end module xslib_sort
