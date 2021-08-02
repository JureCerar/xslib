! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2021  Jure Cerar
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
  implicit none
	private
	public :: quicksort
	
	interface quicksort
    procedure quicksort_int, quicksort_float, quicksort_double, quicksort_long, quicksort_char
  end interface quicksort
	
contains
	
recursive subroutine quicksort_int(array, first, last, order)
  implicit none
  integer, intent(inout)            :: array(:)
  integer, intent(in)               :: first, last
  integer, intent(inout), optional  :: order(:)
  integer                           :: pivot, temp
  integer                           :: i, j, t
  ! Initialize
  pivot = array( (first+last) / 2 )
  i = first
  j = last
  ! Sort loop
  SORT: do
    do while (array(i) < pivot)
      i=i+1
    end do
    do while (pivot < array(j))
      j=j-1
    end do
    if (i >= j) exit
    ! Swap
    temp = array(i)
    array(i) = array(j)
    array(j) = temp
    if (present(order)) then
      t = order(i)
      order(i) = order(j)
      order(j) = t
    end if
    ! Increment
    i=i+1
    j=j-1
  end do SORT
  ! Recursive
  if (first < i-1) call quicksort_int(array, first, i-1, order)
  if (j+1 < last)  call quicksort_int(array, j+1, last, order)
  return
end subroutine quicksort_int

recursive subroutine quicksort_long(array, first, last, order)
  use iso_fortran_env, only: INT64 
  implicit none
  integer(INT64), intent(inout)     :: array(:)
  integer, intent(in)               :: first, last
  integer, intent(inout), optional  :: order(:)
  integer(INT64)                    :: pivot, temp
  integer                           :: i, j, t
  ! Initialize
  pivot = array( (first+last) / 2 )
  i = first
  j = last
  ! Sort loop
  SORT: do
    do while (array(i) < pivot)
      i=i+1
    end do
    do while (pivot < array(j))
      j=j-1
    end do
    if (i >= j) exit
    ! Swap
    temp = array(i)
    array(i) = array(j)
    array(j) = temp
    if (present(order)) then
      t = order(i)
      order(i) = order(j)
      order(j) = t
    end if
    ! Increment
    i=i+1
    j=j-1
  end do SORT
  ! Recursive
  if (first < i-1) call quicksort_long(array, first, i-1, order)
  if (j+1 < last)  call quicksort_long(array, j+1, last, order)
  return
end subroutine quicksort_long

recursive subroutine quicksort_float(array, first, last, order)
  implicit none
  real, intent(inout)               :: array(:)
  integer, intent(in)               :: first, last
  integer, intent(inout), optional  :: order(:)
  real                              :: pivot, temp
  integer                           :: i, j, t
  ! Initialize
  pivot = array( (first+last) / 2 )
  i = first
  j = last
  ! Sort loop
  SORT: do
    do while (array(i) < pivot)
      i=i+1
    end do
    do while (pivot < array(j))
      j=j-1
    end do
    if (i >= j) exit
    ! Swap
    temp = array(i)
    array(i) = array(j)
    array(j) = temp
    if (present(order)) then
      t = order(i)
      order(i) = order(j)
      order(j) = t
    end if
    ! Increment
    i=i+1
    j=j-1
  end do SORT
  ! Recursive
  if (first < i-1) call quicksort_float(array, first, i-1, order)
  if (j+1 < last)  call quicksort_float(array, j+1, last, order)
  return
end subroutine quicksort_float

recursive subroutine quicksort_double(array, first, last, order)
  use iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(inout)       :: array(:)
  integer, intent(in)               :: first, last
  integer, intent(inout), optional  :: order(:)
  real(REAL64)                      :: pivot, temp
  integer                           :: i, j, t 
  ! Initialize
  pivot = array( (first+last) / 2 )
  i = first
  j = last
  ! Sort loop
  SORT: do
    do while (array(i) < pivot)
      i=i+1
    end do
    do while (pivot < array(j))
      j=j-1
    end do
    if (i >= j) exit
    ! Swap
    temp = array(i)
    array(i) = array(j)
    array(j) = temp
    if (present(order)) then
      t = order(i)
      order(i) = order(j)
      order(j) = t
    end if
    ! Increment
    i=i+1
    j=j-1
  end do SORT
  ! Recursive
  if (first < i-1) call quicksort_double(array, first, i-1, order)
  if (j+1 < last)  call quicksort_double(array, j+1, last, order)
  return
end subroutine quicksort_double

recursive subroutine quicksort_char(array, first, last, order)
  implicit none
  character(*), intent(inout)       :: array(:)
  integer, intent(in)               :: first, last
  integer, intent(inout), optional  :: order(:)
  character(:), allocatable         :: pivot, temp
  integer                           :: i, j, t
  ! Initialize
  pivot = array( (first+last) / 2 )
  i = first
  j = last
  ! Sort loop
  SORT: do
    ! NOTE: Inverted to sort alphabetically.
    do while (array(i) < pivot)
      i=i+1
    end do
    do while (pivot < array(j))
      j=j-1
    end do
    if (i >= j) exit
    ! Swap
    temp = array(i)
    array(i) = array(j)
    array(j) = temp
    if (present(order)) then
      t = order(i)
      order(i) = order(j)
      order(j) = t
    end if
    ! Increment
    i=i+1
    j=j-1
  end do SORT
  ! Recursive
  if (first < i-1) call quicksort_char(array, first, i-1, order)
  if (j+1 < last)  call quicksort_char(array, j+1, last, order)
  return
end subroutine quicksort_char

end module xslib_sort

program main
	use xslib_sort
  implicit none
  integer, parameter :: NP = 10
  real               :: array(NP) = [9., 8., 5., 1., 10., 3., 4., 2., 6., 7. ], array2(NP)
  character(8)       :: string(NP) = [  "B", "J", "E", "A", "H", "F", "C", "G", "D", "I"]
  integer            :: order(NP), i
	
  order = [(i,i=1,NP)]

  array2=array
  print *, array
  call quicksort(array, 1, NP, order)
  print *, array
  print *, array2(order)

  print *, string
  call quicksort(string, 1, NP)
  print *, string


contains

end program main