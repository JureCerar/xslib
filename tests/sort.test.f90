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

program main
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  use xslib_sort
  implicit none

  call sort_test_int32 ()
  call sort_test_int64 ()
  call sort_test_real32 ()
  call sort_test_real64 ()
  call sort_test_char ()

contains 

! Test different sorting methods on a random array
subroutine sort_test_int32 ()
  implicit none 
  integer, parameter :: NP = 100
  integer(INT32) :: array(NP)
  integer :: i, order(NP)

  ! Generic sort
  call genRandom (array)
  call sort(array)
  if (.not. isSorted(array)) error stop

  ! Quic ksort
  call genRandom (array)
  call qsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call qsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "quicksort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "quicksort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Merge sort
  call genRandom (array)
  call msort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call msort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "mergesort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "mergesort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Heap sort
  call genRandom (array)
  call hsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call hsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "heapsort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "heapsort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

end subroutine sort_test_int32 

subroutine sort_test_int64 ()
  implicit none 
  integer, parameter :: NP = 100
  integer(INT64) :: array(NP)
  integer :: i, order(NP)

  ! Generic sort
  call genRandom (array)
  call sort(array)
  if (.not. isSorted(array)) error stop

  ! Quic ksort
  call genRandom (array)
  call qsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call qsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "quicksort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "quicksort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Merge sort
  call genRandom (array)
  call msort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call msort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "mergesort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "mergesort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Heap sort
  call genRandom (array)
  call hsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call hsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "heapsort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "heapsort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

end subroutine sort_test_int64 

subroutine sort_test_real32 ()
  implicit none 
  integer, parameter :: NP = 100
  real(REAL32) :: array(NP)
  integer :: i, order(NP)

  ! Generic sort
  call genRandom (array)
  call sort(array)
  if (.not. isSorted(array)) error stop

  ! Quic ksort
  call genRandom (array)
  call qsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call qsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "quicksort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "quicksort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Merge sort
  call genRandom (array)
  call msort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call msort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "mergesort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "mergesort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Heap sort
  call genRandom (array)
  call hsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call hsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "heapsort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "heapsort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

end subroutine sort_test_real32 

subroutine sort_test_real64 ()
  implicit none 
  integer, parameter :: NP = 100
  real(REAL64) :: array(NP)
  integer :: i, order(NP)

  ! Generic sort
  call genRandom (array)
  call sort(array)
  if (.not. isSorted(array)) error stop

  ! Quic ksort
  call genRandom (array)
  call qsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call qsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "quicksort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "quicksort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Merge sort
  call genRandom (array)
  call msort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call msort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "mergesort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "mergesort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Heap sort
  call genRandom (array)
  call hsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call hsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "heapsort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "heapsort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

end subroutine sort_test_real64 

subroutine sort_test_char ()
  implicit none 
  integer, parameter :: NP = 100, LEN = 32
  character(LEN) :: array(NP)
  integer :: i, order(NP)

  ! Generic sort
  call genRandom (array)
  call sort(array)
  if (.not. isSorted(array)) error stop

  ! Quic ksort
  call genRandom (array)
  call qsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call qsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "quicksort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "quicksort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Merge sort
  call genRandom (array)
  call msort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call msort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "mergesort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "mergesort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

  ! Heap sort
  call genRandom (array)
  call hsort(array)
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call hsort(array, order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop
  call genRandom (array)
  call sort(array, "heapsort")
  if (.not. isSorted(array)) error stop
  call genRandom (array)
  call sort(array, "heapsort", order)
  if (.not. isSorted(array)) error stop
  if (sum(order) /= sum([(i, i = 1, NP)])) error stop 

end subroutine sort_test_char 

! Generate random array of any kind
subroutine genRandom (array)
  class(*), intent(inout) :: array(:)
  integer :: i, j, seed

  call system_clock (seed)
  call srand (seed)

  select type (array)
  type is (integer(INT32))
    array = [(int(rand()*1000, INT32), i = 1, size(array))]
  type is (integer(INT64))
    array = [(int(rand()*1000, INT64), i = 1, size(array))]
  type is (real(REAL32))
    array = [(real(rand()*1000, REAL32), i = 1, size(array))]
  type is (real(REAL64))
    array = [(real(rand()*1000, REAL64), i = 1, size(array))]
  type is (character(*))
    do i = 1, size(array)
      do j = 1, len(array)
        array(i)(j:j) = char(97 + int(rand()*25))
      end do
    end do
  class default
    error stop
  end select

end subroutine genRandom

! Check if array of any kind is sorted
function isSorted (array) result (out)
  implicit none
  logical :: out
  class(*), intent(in) :: array(:)
  integer :: i

  out = .True.
  select type (array)
  type is (integer(INT32))
    do i = 1, size(array) - 1
      out = out .and. array(i+1) >= array(i)
      if (.not. out) exit
    end do
  type is (integer(INT64))
    do i = 1, size(array) - 1
      out = out .and. array(i+1) >= array(i)
      if (.not. out) exit
    end do
  type is (real(REAL32))
    do i = 1, size(array) - 1
      out = out .and. array(i+1) >= array(i)
      if (.not. out) exit
    end do
  type is (real(REAL64))
    do i = 1, size(array) - 1
      out = out .and. array(i+1) >= array(i)
      if (.not. out) exit
    end do
  type is (character(*))
    do i = 1, size(array) - 1
      out = out .and. array(i+1) >= array(i)
      if (.not. out) exit
    end do
  class default
    error stop
  end select

end function isSorted

end program main