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

module xslib_array
  !! Module for array creation and manipulation.
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: linspace, logspace, arange, eye
  

  interface linspace
    !! Return evenly spaced numbers over a specified interval. Returns `num` evenly
    !! spaced samples, calculated over the interval `[start, stop]`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, linspace(0.0, 1.0, 5)
    !! > [0.00, 0.25, 0.50, 0.75, 1.00]
    !! ```
    module procedure :: linspace_r32, linspace_r64
  end interface linspace 


  interface logspace
    !! Return numbers spaced evenly on a logarithmic scale. Returns `num` samples
    !! on a log scale in the closed interval `[start, stop]`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, logspace(1.0, 10000.0, 5)
    !! > [1.0, 10.0, 100.0, 1000.0, 10000.0]
    !! ```
    module procedure :: logspace_r32, logspace_r64
  end interface logspace 


  interface arange
    !! Return equally spaced values within a given interval.
    !!
    !! Example:
    !! ```Fortran
    !! print *, arange(0.0, 1.0, 0.25)
    !! > [0.00, 0.25, 0.50, 0.75, 1.00]
    !! ```
    module procedure :: arange_r32, arange_r64
  end interface arange 


  interface eye
    !! Return a 2-D array with ones on the diagonal and zeros elsewhere.
    !! 
    !! Example
    !! ```Fortran
    !! print *, eye(3)
    !! > [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]
    !! print *, eye(3, mold=0_INT32)
    !! > [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    !! ```
    module procedure :: eye_i32, eye_i64, eye_r32, eye_r64
  end interface eye

contains


function linspace_r32 (start, stop, num) result (out)
  implicit none
  real(REAL32) :: out(num)
  !! Equally spaced samples in the closed interval `[start, stop]`.
  real(REAL32), intent(in) :: start
  !! The starting value of the sequence.
  real(REAL32), intent(in) :: stop
  !! The end value of the sequence.
  integer, intent(in) :: num
  !! Number of samples to generate. Must be non-negative.
  real(REAL32) :: step
  integer :: i

  step = (stop - start) / (num - 1)
  out = [(start + (i - 1) * step, i = 1, num)]

end function linspace_r32


function linspace_r64 (start, stop, num) result (out)
  implicit none
  real(REAL64) :: out(num)
  real(REAL64), intent(in) :: start, stop
  integer, intent(in) :: num
  real(REAL64) :: step
  integer :: i

  step = (stop - start) / (num - 1)
  out = [(start + (i - 1) * step, i = 1, num)]

end function linspace_r64


function logspace_r32 (start, stop, num) result (out)
  implicit none
  real(REAL32) :: out(num)
  !! Equally spaced samples on a log scale in the closed interval `[start, stop]`.
  real(REAL32), intent(in) :: start
  !! The starting value of the sequence. Must be bigger than zero.
  real(REAL32), intent(in) :: stop
  !! The end value of the sequence.
  integer, intent(in) :: num
  !! Number of samples to generate. Must be non-negative.
  real(REAL32) :: step
  integer :: i

  if (start <= 0) error stop "Start value cannot be less than 0"

  step = (stop / start) ** (1.0 / (num - 1))
  out = [(start * step ** (i - 1), i = 1, num)]

end function logspace_r32


function logspace_r64 (start, stop, num) result (out)
  implicit none
  real(REAL64) :: out(num)
  real(REAL64), intent(in) :: start, stop
  integer, intent(in) :: num
  real(REAL64) :: step
  integer :: i

  if (start <= 0) error stop "Start value cannot be less than 0"

  step = (stop / start) ** (1.0 / (num - 1))
  out = [(start * step ** (i - 1), i = 1, num)]

end function logspace_r64


function arange_r32 (start, stop, step) result (out)
  implicit none
  real(REAL32), allocatable :: out(:)
  !! Equally spaced values. The size of the result is equal to `int((stop - start) / step) + 1`.
  real(REAL32), intent(in) :: start
  !! Start of interval. The interval includes this value.
  real(REAL32), intent(in) :: stop
  !! End of interval. Value is not necessarily included in sequence.
  real(REAL32), intent(in) :: step
  !! Spacing between values. For any output out, this is the distance between two adjacent values `out(i+1) - out(i)`.
  integer :: num, i

  num = int((stop - start) / step) + 1
  out = [(start + (i - 1) * step, i = 1, num)]

end function arange_r32


function arange_r64 (start, stop, step) result (out)
  implicit none
  real(REAL64), allocatable :: out(:)
  real(REAL64), intent(in) :: start, stop, step
  integer :: num, i

  num = int((stop - start) / step) + 1
  out = [(start + (i - 1) * step, i = 1, num)]

end function arange_r64


function eye_i32(n, mold) result (out)
  implicit none
  integer, intent(in) :: n
  !! Number of rows and columns in the output.
  integer(INT32), intent(in) :: mold
  !! Specify type of output array.
  integer(INT32), allocatable :: out(:,:)
  !! An array where all elements are equal to zero, except for the diagonal, whose values are equal to one.
  integer :: i
  allocate(out(n, n), mold=mold)
  out = 0
  do i = 1, n
    out(i, i) = 1
  end do
end function eye_i32


function eye_i64(n, mold) result (out)
  implicit none
  integer, intent(in) :: n
  integer(INT64), intent(in) :: mold
  integer(INT64), allocatable :: out(:,:)
  integer :: i
  allocate(out(n, n), mold=mold)
  out = 0
  do i = 1, n
    out(i, i) = 1
  end do
end function eye_i64


function eye_r32(n, mold) result (out)
  implicit none
  integer, intent(in) :: n
  real(REAL32), intent(in), optional :: mold
  ! Make variable optional to have unique signature 
  real, allocatable :: out(:,:)
  integer :: i
  allocate(out(n, n), mold=mold)
  out = 0
  do i = 1, n
    out(i, i) = 1
  end do
end function eye_r32


function eye_r64(n, mold) result (out)
  implicit none
  integer, intent(in) :: n
  real(REAL64), intent(in) :: mold
  real(REAL64), allocatable :: out(:,:)
  integer :: i
  allocate(out(n, n), mold=mold)
  out = 0
  do i = 1, n
    out(i, i) = 1
  end do
end function eye_r64

end module xslib_array