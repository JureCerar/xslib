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

module xslib_array
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: linspace, logspace, arange, diff, cumsum, cumprod, interp, trapz, gradient

  ! %%%
  ! # `ARRAY` - Array operations
  !   Module `xslib_array` contains function for array operations. Supports both single and double precision (`DP`) functions.
  ! %%%

  interface linspace
    module procedure :: linspace_real32, linspace_real64
  end interface linspace 

  interface logspace
    module procedure :: logspace_real32, logspace_real64
  end interface logspace 

  interface arange
    module procedure :: arange_real32, arange_real64
  end interface arange 

  interface diff
    module procedure :: diff_int32, diff_int64, diff_real32, diff_real64
  end interface diff

  interface cumsum
    module procedure :: cumsum_int32, cumsum_int64, cumsum_real32, cumsum_real64
  end interface cumsum

  interface cumprod
    module procedure :: cumprod_int32, cumprod_int64, cumprod_real32, cumprod_real64
  end interface cumprod

  interface interp
    module procedure :: interp_real32, interp_real64
  end interface interp

  interface trapz
    module procedure :: trapz_real32, trapz_real64, trapz_dx_real32, trapz_dx_real64
  end interface trapz

  interface gradient
    module procedure :: gradient_real32, gradient_real64, gradient_dx_real32, gradient_dx_real64
  end interface gradient

contains

function linspace_real32 (start, stop, num) result (out)
  ! %%%
  ! ## `LINSPACE` - Generate evenly spaced numbers
  ! #### DESCRIPTION
  !   Return evenly spaced numbers over a specified interval.
  ! #### USAGE
  !   ```Fortran
  !   out = linspace(start, stop, num)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), intent(IN) :: start, stop`
  !     The starting and end value of the sequence. `start` and `stop` values are included in sequence.
  !   * `integer, intent(IN) :: num`
  !     Number of samples to generate. Must be non-negative.
  !   * `real(ANY), dimension(num) :: out`
  !     Equally spaced numbers in the opened interval `(start, stop)`.
  ! #### EXAMPLE
  !   ```Fortran
  !   > linspace(0.0, 1.0, 5)
  !   [0.00, 0.25, 0.50, 0.75, 1.00] 
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out(num)
  real(REAL32), intent(in) :: start, stop
  integer, intent(in) :: num
  real(REAL32) :: step
  integer :: i

  step = (stop - start) / (num - 1)
  out = [(start + (i - 1) * step, i = 1, num)]

end function linspace_real32


function linspace_real64 (start, stop, num) result (out)
  implicit none
  real(REAL64) :: out(num)
  real(REAL64), intent(in) :: start, stop
  integer, intent(in) :: num
  real(REAL64) :: step
  integer :: i

  step = (stop - start) / (num - 1)
  out = [(start + (i - 1) * step, i = 1, num)]

end function linspace_real64


function logspace_real32 (start, stop, num) result (out)
  ! %%%
  ! ## `LOGSPACE` - Generate logarithmically spaced numbers
  ! #### DESCRIPTION
  !   Return evenly spaced numbers on a logarithmic scale over a specified interval.
  ! #### USAGE
  !   ```Fortran
  !   out = logspace(start, stop, num)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), intent(IN) :: start, stop`
  !     The starting and end value of the sequence. `start` and `stop` values are included in sequence. Must be bigger than zero.
  !   * `integer, intent(IN) :: num`
  !     Number of samples to generate. Must be non-negative.
  !   * `real(ANY), dimension(num) :: out`
  !     Logarithmically spaced numbers in the opened interval `(start, stop)`.
  ! #### EXAMPLE
  !   ```Fortran
  !   > logspace(1.0, 10000.0, 5)
  !   [1.0, 10.0, 100.0, 1000.0, 10000.0] 
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out(num)
  real(REAL32), intent(in) :: start, stop
  integer, intent(in) :: num
  real(REAL32) :: step
  integer :: i

  if (start <= 0) error stop "Start value cannot be less than 0"

  step = (stop / start) ** (1.0 / (num - 1))
  out = [(start * step ** (i - 1), i = 1, num)]

end function logspace_real32


function logspace_real64 (start, stop, num) result (out)
  implicit none
  real(REAL64) :: out(num)
  real(REAL64), intent(in) :: start, stop
  integer, intent(in) :: num
  real(REAL64) :: step
  integer :: i

  if (start <= 0) error stop "Start value cannot be less than 0"

  step = (stop / start) ** (1.0 / (num - 1))
  out = [(start * step ** (i - 1), i = 1, num)]

end function logspace_real64


function arange_real32 (start, stop, step) result (out)
  ! %%%
  ! ## `ARANGE` - Generate equally spaced values
  ! #### DESCRIPTION
  !   Return equally spaced values within a given interval.
  ! #### USAGE
  !   ```Fortran
  !   out = arange(start, stop, step)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), intent(IN) :: start, stop`
  !     The starting and end value of the sequence. `stop` value is not necessarily included in sequence.
  !   * `real(ANY), intent(IN) :: step`
  !     Spacing between values. 
  !   * `real(ANY), dimension(LENGTH) :: out`
  !     Equally spaced values. The `LENGTH` of the result is equal to `int((stop - start) / step) + 1`.
  ! #### EXAMPLE
  !   ```Fortran
  !   > arange(0.0, 1.0, 0.25)
  !   [0.00, 0.25, 0.50, 0.75, 1.00]
  !   ```
  ! %%%
  implicit none
  real(REAL32), allocatable :: out(:)
  real(REAL32), intent(in) :: start, stop, step
  integer :: num, i

  num = int((stop - start) / step) + 1
  out = [(start + (i - 1) * step, i = 1, num)]

end function arange_real32


function arange_real64 (start, stop, step) result (out)
  implicit none
  real(REAL64), allocatable :: out(:)
  real(REAL64), intent(in) :: start, stop, step
  integer :: num, i

  num = int((stop - start) / step) + 1
  out = [(start + (i - 1) * step, i = 1, num)]

end function arange_real64


function diff_int32 (a, n) result (out)
  ! %%%
  ! ## `DIFF` - Generate equally spaced values
  ! #### DESCRIPTION
  !   Calculate the n-th discrete difference along the array.
  !   The first difference is given by `out[i] = a[i+1] - a[i]`,
  !   higher differences are calculated by using diff recursively.
  ! #### USAGE
  !   ```Fortran
  !   out = diff(a, n=n)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), dimension(:), intent(IN) :: a`
  !     Input array.
  !   * `integer, intent(IN), OPTIONAL :: n`
  !     The number of times values are differentiated. If zero, the input is returned as-is.
  !   * `class(*), dimension(LENGTH) :: out`
  !     The n-th differences array. Size of output array is `size(a) - n`.
  ! #### EXAMPLE
  !   ```Fortran
  !   > diff([1.0, 2.0, 4.0, 8.0])
  !   [1.0, 2.0, 4.0]
  !   > diff([1.0, 2.0, 4.0, 8.0], n=2)
  !   [1.0, 2.0]
  !   ```
  ! %%%
  implicit none
  integer(INT32), allocatable :: out(:)
  integer(INT32), intent(in) :: a(:)
  integer, intent(in), optional :: n
  integer :: i, j

  out = a
  do i = 1, merge(n, 1, present(n))
    out = [(out(j+1) - out(j), j = 1, size(out)-1)]
  end do

end function diff_int32


function diff_int64 (a, n) result (out)
  implicit none
  integer(INT64), allocatable :: out(:)
  integer(INT64), intent(in) :: a(:)
  integer, intent(in), optional :: n
  integer :: i, j

  out = a
  do i = 1, merge(n, 1, present(n))
    out = [(out(j+1) - out(j), j = 1, size(out)-1)]
  end do

end function diff_int64


function diff_real32 (a, n) result (out)
  implicit none
  real(REAL32), allocatable :: out(:)
  real(REAL32), intent(in) :: a(:)
  integer, intent(in), optional :: n
  integer :: i, j

  out = a
  do i = 1, merge(n, 1, present(n))
    out = [(out(j+1) - out(j), j = 1, size(out)-1)]
  end do

end function diff_real32


function diff_real64 (a, n) result (out)
  implicit none
  real(REAL64), allocatable :: out(:)
  real(REAL64), intent(in) :: a(:)
  integer, intent(in), optional :: n
  integer :: i, j

  out = a
  do i = 1, merge(n, 1, present(n))
    out = [(out(j+1) - out(j), j = 1, size(out)-1)]
  end do

end function diff_real64


function cumsum_int32 (a) result (out)
  ! %%%
  ! ## `CUMSUM` - Calculate cumulative sum
  ! #### DESCRIPTION
  !   Return the cumulative sum of the elements of given array.
  ! #### USAGE
  !   ```Fortran
  !   out = cumsum(a)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), dimension(:), intent(IN) :: a`
  !     Input array.
  !   * `class(*), dimension(:) :: out`
  !     Cumulative sum result. The result has the same size as `a`.
  ! #### EXAMPLE
  !   ```Fortran
  !   > cumsum([1,0, 2.0, 3.0, 4.0, 5.0])
  !   [1.0, 3.0, 6.0, 10.0, 15.0]
  !   ```
  ! %%%
  implicit none
  integer(INT32), allocatable :: out(:)
  integer(INT32), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) + out(i-1)
  end do

end function cumsum_int32


function cumsum_int64 (a) result (out)
  implicit none
  integer(INT64), allocatable :: out(:)
  integer(INT64), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) + out(i-1)
  end do

end function cumsum_int64


function cumsum_real32 (a) result (out)
  implicit none
  real(REAL32), allocatable :: out(:)
  real(REAL32), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) + out(i-1)
  end do

end function cumsum_real32


function cumsum_real64 (a) result (out)
  implicit none
  real(REAL64), allocatable :: out(:)
  real(REAL64), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) + out(i-1)
  end do

end function cumsum_real64


function cumprod_int32 (a) result (out)
  ! %%%
  ! ## `CUMPROD` - Calculate cumulative product
  ! #### DESCRIPTION
  !   Return the cumulative product of the elements of given array.
  ! #### USAGE
  !   ```Fortran
  !   out = cumprod(a)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), dimension(:), intent(IN) :: a`
  !     Input array.
  !   * `class(*), dimension(:) :: out`
  !     Cumulative product result. The result has the same size as `a`.
  ! #### EXAMPLE
  !   ```Fortran
  !   > cumprod([1,0, 2.0, 3.0, 4.0, 5.0])
  !   [1.0, 2.0, 6.0, 24.0, 120.0]
  !   ```
  ! %%%
  implicit none
  integer(INT32), allocatable :: out(:)
  integer(INT32), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) * out(i-1)
  end do

end function cumprod_int32


function cumprod_int64 (a) result (out)
  implicit none
  integer(INT64), allocatable :: out(:)
  integer(INT64), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) * out(i-1)
  end do

end function cumprod_int64


function cumprod_real32 (a) result (out)
  implicit none
  real(REAL32), allocatable :: out(:)
  real(REAL32), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) * out(i-1)
  end do

end function cumprod_real32


function cumprod_real64 (a) result (out)
  implicit none
  real(REAL64), allocatable :: out(:)
  real(REAL64), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) * out(i-1)
  end do

end function cumprod_real64


function interp_real32 (x, xp, yp) result (out)
  ! %%%
  ! ## `INTERP` - Descritpion
  ! #### DESCRIPTION
  !   One-dimensional linear interpolation for monotonically increasing sample points.

  !   Returns the one-dimensional piecewise linear interpolant to a function with given
  !   discrete data points `(xp, fp)`, evaluated at `x`.
  ! #### USAGE
  !   ```Fortran
  !   out = interp(x, xp, yp)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(:), intent(IN) :: x`
  !     x-coordinates at which to evaluate the interpolated values.
  !   * `real(ANY), dimension(:), intent(IN) :: xp, yp`
  !     x- and y-coordinates of the data points to be interpolated. Must be same size. 
  !   * `real(DP), dimension(:) :: out`
  !     Interpolated values, same shape as `x`.
  ! #### EXAMPLE
  !   ```Fortran
  !   > x = [0.00, 1.00, 1.50, 2.50, 3.50]
  !   > out = interp(x, [1.0, 2.0, 3.0], [3.0, 2.0, 0.0])
  !   [4.00, 3.00, 2.50, 1.00, -1.00]
  !   ```
  ! %%%
  implicit none
  real(REAL32), intent(in) :: x(:)
  real(REAL32), intent(in) :: xp(:), yp(size(xp)) 
  real(REAL32) :: out(size(x))
  integer :: i, n

  do n = 1, size(x)
    i = 1
    do while (i < size(x)-1)
      if (xp(i+1) >= x(n)) exit
      i = i + 1
    end do
    out(n) = (yp(i+1) - yp(i)) / (xp(i+1) - xp(i)) * (x(n) - xp(i)) + yp(i)
  end do

end function interp_real32


function interp_real64 (x, xp, yp) result (out)
  implicit none
  real(REAL64), intent(in) :: x(:)
  real(REAL64), intent(in) :: xp(:), yp(size(xp)) 
  real(REAL64) :: out(size(x))
  integer :: i, n

  do n = 1, size(x)
    i = 1
    do while (i < size(x) - 1)
      if (xp(i+1) >= x(n)) exit
      i = i + 1
    end do
    out(n) = (yp(i+1) - yp(i)) / (xp(i+1) - xp(i)) * (x(n) - xp(i)) + yp(i)
  end do

end function interp_real64


function trapz_real32 (y, x) result (out)
  ! %%%
  ! ## `TRAPZ` - Trapezoidal rule integration 
  ! #### DESCRIPTION
  !   Integrate along the given axis using the composite trapezoidal rule.

  !   If `x` is provided, the integration happens in sequence along its
  !   elements - they are not sorted. If points are equidistant use `dx` option.
  ! #### USAGE
  !   ```Fortran
  !   out = trapz(y, x)
  !   out = trapz(y, dx)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(:), intent(IN) :: y`
  !     Input array to integrate.
  !   * `real(ANY), dimension(:), intent(IN) :: x`
  !     The sample points corresponding to the `y` values. Must be same size as `y`.
  !   * `real(ANY), intent(IN), OPTIONAL :: dx`
  !     The spacing between sample points. Default: 1.0.
  !   * `real(ANY) :: out`
  !     Definite integral of `y`.
  ! #### EXAMPLE
  !   ```Fortran
  !   > out = trapz([0.0, 1.0], [0.0, 1.0])
  !   0.50000
  !   > out = trapz([0.0, 1.0], dx=1.0)
  !   0.50000
  !   > out = trapz([0.0, 1.0])
  !   0.50000
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: y(:), x(size(y))
  integer :: i

  out = 0.
  do i = 1, size(y) - 1
    out = out + 0.5 * (y(i+1) + y(i)) * (x(i+1) - x(i))
  end do
 
end function trapz_real32


function trapz_real64 (y, x) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: y(:), x(size(y))
  integer :: i

  out = 0.
  do i = 1, size(y) - 1
    out = out + 0.5 * (y(i+1) + y(i)) * (x(i+1) - x(i))
  end do
 
end function trapz_real64


function trapz_dx_real32 (y, dx) result (out)
  implicit none
  real(REAL32) :: out, dx_
  real(REAL32), intent(in) :: y(:)
  real(REAL32), intent(in), optional :: dx
  integer :: i

  dx_ = merge(dx, 1.0, present(dx))
  out = 0.
  do i = 1, size(y) - 1
    out = out + 0.5 * (y(i+1) + y(i)) * dx_
  end do
 
end function trapz_dx_real32


function trapz_dx_real64 (y, dx) result (out)
  implicit none
  real(REAL64) :: out, dx_
  real(REAL64), intent(in) :: y(:)
  real(REAL64), intent(in), optional :: dx
  integer :: i

  dx_ = merge(dx, 1.0d0, present(dx))
  out = 0.0d0
  do i = 1, size(y) - 1
    out = out + 0.5 * (y(i+1) + y(i)) * dx_
  end do
 
end function trapz_dx_real64


function gradient_real32 (y, x) result (out)
  ! %%%
  ! ## `GRADIENT` - Calculate gradient of an array 
  ! #### DESCRIPTION
  !   Return the gradient (derivative) of an array using finite difference method.

  !   The gradient is computed using second order accurate central differences in
  !   the interior points and either first or second order accurate one-sides 
  !   (forward or backwards) differences at the boundaries. The returned gradient 
  !   hence has the same shape as the input array.
  ! #### USAGE
  !   ```Fortran
  !   out = gradient(y, x)
  !   out = gradient(y, dx=dx)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(:), intent(IN) :: y`
  !     Input array to derivate.
  !   * `real(ANY), dimension(:), intent(IN) :: x`
  !     The sample points corresponding to the `y` values. Must be same size as `y`.
  !   * `real(ANY), intent(IN), OPTIONAL :: dx`
  !     The spacing between sample points `y`. Default: 1.0.
  !   * `real(ANY), dimension(:) :: out`
  !     Derivative at each value of `y`. Is same shape as `y`.
  ! #### EXAMPLE
  !   ```Fortran
  !   > out = gradient([0.0, 1.0], [0.0, 1.0])
  !   [1.00000,1.00000]
  !   > out = gradient([0.0, 1.0], dx=1.0)
  !   [1.00000,1.00000]
  !   > out = gradient([0.0, 1.0])
  !   [1.00000,1.00000]
  !   ```
  ! %%%
  implicit none
  real(REAL32), intent(in) :: y(:), x(size(y))
  real(REAL32) :: out(size(y))
  integer :: i, np

  np = size(y)
  if (np > 2) then
    out(1) = (-3 * y(1) + 4 * y(2) - y(3)) / (-3 * x(1) + 4 * x(2) - x(3))
    do i = 2, np - 1
      out(i) = (y(i+1) - y(i-1)) / (x(i+1) - x(i-1))
    end do
    out(np) = (y(np-2) - 4 * y(np-1) + 3 * y(np)) / (x(np-2) - 4 * x(np-1) + 3 * x(np))
  else if (np > 1) then
    out = (y(2) - y(1)) / (x(2) - x(1))
  else
    out = 0.
  end if

end function gradient_real32


function gradient_real64 (y, x) result (out)
  implicit none
  real(REAL64), intent(in) :: y(:), x(size(y))
  real(REAL64) :: out(size(y))
  integer :: i, np

  np = size(y)
  if (np > 2) then
    out(1) = (-3 * y(1) + 4 * y(2) - y(3)) / (-3 * x(1) + 4 * x(2) - x(3))
    do i = 2, np - 1
      out(i) = (y(i+1) - y(i-1)) / (x(i+1) - x(i-1))
    end do
    out(np) = (y(np-2) - 4 * y(np-1) + 3 * y(np)) / (x(np-2) - 4 * x(np-1) + 3 * x(np))
  else if (np > 1) then
    out = (y(2) - y(1)) / (x(2) - x(1))
  else
    out = 0.
  end if

end function gradient_real64


function gradient_dx_real32 (y, dx) result (out)
  implicit none
  real(REAL32), intent(in) :: y(:)
  real(REAL32), intent(in), optional :: dx
  real(REAL32) :: out(size(y)), dx_
  integer :: i, np

  dx_ = merge(dx, 1.0, present(dx))
  np = size(y)
  if (np > 2) then
    out(1) = (-3 * y(1) + 4 * y(2) - y(3)) / (2 * dx_)
    do i = 2, np - 1
      out(i) = (y(i+1) - y(i-1)) / (2 * dx_)
    end do
    out(np) = (y(np-2) - 4 * y(np-1) + 3 * y(np)) / (2 * dx_)
  else if (np > 1) then
    out = (y(2) - y(1)) / dx_
  else
    out = 0.0
  end if

end function gradient_dx_real32


function gradient_dx_real64 (y, dx) result (out)
  implicit none
  real(REAL64), intent(in) :: y(:)
  real(REAL64), intent(in), optional :: dx
  real(REAL64) :: out(size(y)), dx_
  integer :: i, np

  dx_ = merge(dx, 1.0d0, present(dx))
  np = size(y)
  if (np > 2) then
    out(1) = (-3 * y(1) + 4 * y(2) - y(3)) / (2 * dx_)
    do i = 2, np - 1
      out(i) = (y(i+1) - y(i-1)) / (2 * dx_)
    end do
    out(np) = (y(np-2) - 4 * y(np-1) + 3 * y(np)) / (2 * dx_)
  else if (np > 1) then
    out = (y(2) - y(1)) / dx_
  else
    out = 0.0d0
  end if

end function gradient_dx_real64

end module xslib_array