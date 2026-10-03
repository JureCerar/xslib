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

module xslib_math
  !! Module with some basic mathematical functions.
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: diff, cumsum, cumprod, interp, trapz, gradient
  public :: factorial, perm, comb, mix, clip, gcd, lcm


  interface diff
    !! Calculate the n-th discrete difference along the array.
    !! The first difference is given by `out[i] = a[i+1] - a[i]`,
    !! higher differences are calculated by using diff recursively.
    !!
    !! Example:
    !! ```Fortran
    !! print *, diff([1.0, 2.0, 4.0, 8.0])
    !! >>> [1.0, 2.0, 4.0]
    !! print *, diff([1.0, 2.0, 4.0, 8.0], n=2)
    !! >>> [1.0, 2.0]
    !! ```
    module procedure :: diff_i32, diff_i64, diff_r32, diff_r64
  end interface diff


  interface cumsum
    !! Return the cumulative sum of the elements of given array.
    !! 
    !! Example:
    !! ```Fortran
    !! print *, cumsum([1,0, 2.0, 3.0, 4.0, 5.0])
    !! >>> [1.0, 3.0, 6.0, 10.0, 15.0]
    !! ```
    module procedure :: cumsum_i32, cumsum_i64, cumsum_r32, cumsum_r64
  end interface cumsum


  interface cumprod
    !! Return the cumulative product of the elements of given array.
    !! 
    !! Example:
    !! ```Fortran
    !! print *, cumprod([1,0, 2.0, 3.0, 4.0, 5.0])
    !! >>> [1.0, 2.0, 6.0, 24.0, 120.0]
    !! ```  
    module procedure :: cumprod_i32, cumprod_i64, cumprod_r32, cumprod_r64
  end interface cumprod


  interface interp
    !! One-dimensional linear interpolation for monotonically increasing sample points.
    !!
    !! Returns the one-dimensional piecewise linear interpolant to a function with given
    !! discrete data points `(xp, yp)`, evaluated at `x`.
    !! 
    !! Example:
    !! ```Fortran
    !! x = [0.00, 1.00, 1.50, 2.50, 3.50]
    !! print *, interp(x, [1.0, 2.0, 3.0], [3.0, 2.0, 0.0])
    !! >>> [4.00, 3.00, 2.50, 1.00, -1.00]
    !! ```  
    module procedure :: interp_r32, interp_r64
  end interface interp


  interface trapz
    !! Integrate along the given axis using the composite trapezoidal rule.
    !!
    !! If `x` is provided, the integration happens in sequence along its
    !! elements - they are not sorted. If points are equidistant use `dx` option.
    !! 
    !! Example:
    !! ```Fortran
    !! print *, trapz([0.0, 1.0], [0.0, 1.0])
    !! >>> 0.50000
    !! print *, trapz([0.0, 1.0], dx=1.0)
    !! >>> 0.50000
    !! print *, trapz([0.0, 1.0])
    !! >>> 0.50000
    !! ```  
    module procedure :: trapz_r32, trapz_r64
    module procedure :: trapz_dx_r32, trapz_dx_r64
  end interface trapz


  interface gradient
    !! Return the gradient (derivative) of an array using finite difference method.
    !! 
    !! The gradient is computed using second order accurate central differences in
    !! the interior points and either first or second order accurate one-sides 
    !! (forward or backwards) differences at the boundaries. The returned gradient 
    !! hence has the same shape as the input array.
    !!
    !! Example:
    !! ```Fortran
    !! print *, gradient([0.0, 1.0], [0.0, 1.0])
    !! >>> [1.00000, 1.00000]
    !! print *, gradient([0.0, 1.0], dx=1.0)
    !! >>> [1.00000, 1.00000]
    !! print *, gradient([0.0, 1.0])
    !! >>> [1.00000, 1.00000]
    !! ```  
    module procedure :: gradient_r32, gradient_r64
    module procedure :: gradient_dx_r32, gradient_dx_r64
  end interface gradient


  interface factorial
    !! Return factorial of an integer `n`. Throws an error if `n` is negative.
    !!
    !! Example:
    !! ```Fortran
    !! print *, factorial(5)
    !! >>> 120
    !! ```  
    module procedure :: factorial_int32, factorial_int64
  end interface factorial


  interface perm
    !! Return the number of ways to choose `k` items from `n`
    !! items without repetition and with order.
    !!
    !! Example:
    !! ```Fortran
    !! print *, perm(6, 4)
    !! >>> 360
    !! ```   
    module procedure :: perm_int32, perm_int64
  end interface perm


  interface comb
    !! Return the number of ways to choose `k` items from `n` items
    !! without repetition and without order.
    !!
    !! Example:
    !! ```Fortran
    !! print *, comb(6, 4)
    !! >>> 15
    !! ```   
    module procedure :: comb_int32, comb_int64
  end interface comb 


  interface mix
    !! Return mix *i.e.* fractional linear interpolation between two values. If
    !! `x = 0.0` then return `a` if `x = 1.0` return `b` otherwise return linear
    !! interpolation of `a` and `b`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, mix(0.0, 5.0, 0.25)
    !! >>> 1.25
    !! ```   
    module procedure :: mix_real32, mix_real64
  end interface mix 


  interface clip
    !! Clip (limit) the values in an array.
    !!
    !! Given an interval, values outside the interval are clipped to the interval
    !! edges. For example, if an interval of `[0, 1]` is specified, values smaller 
    !! than 0 become `0`, and values larger than 1 become `1`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, clip(0.9, 1.0, 2.0)
    !! >>> 1.0
    !! print *, clip(1.1, 1.0, 2.0)
    !! >>> 1.1
    !! print *, clip(2.1, 1.0, 2.0)
    !! >>> 2.0
    !! ```    
    module procedure :: clip_real32, clip_real64
  end interface clip 


  interface gcd
    !! Return the greatest common divisor (GCD) of the specified integer arguments.
    !! If any of the arguments is nonzero, then the returned value is the largest
    !! positive integer that is a divisor of all arguments. If all arguments are
    !! zero, then the returned value is 0.
    !!
    !! Example:
    !! ```Fortran
    !! print *, gcd(106, 901)
    !! >>> 53
    !! ```   
    module procedure :: gcd_int32, gcd_int64
  end interface gcd 


  interface lcm
    !! Return the least common multiple (LCM) of the specified integer arguments.
    !! If all arguments are nonzero, then the returned value is the smallest
    !! positive integer that is a multiple of all arguments. If any of the arguments
    !! is zero, then the returned value is 0.
    !!
    !! Example:
    !! ```Fortran
    !! print *, lcm(12, 17)
    !! >>> 204
    !! ```   
    module procedure :: lcm_int32, lcm_int64 
  end interface lcm 


contains


function diff_i32 (a, n) result (out)
  implicit none
  integer(INT32), allocatable :: out(:)
  !! The n-th differences array. Size of output array is `size(a) - n`.
  integer(INT32), intent(in) :: a(:)
  !! Input array.
  integer, intent(in), optional :: n
  !! The number of times values are differentiated. If zero, the input is returned as-is.
  integer :: i, j

  out = a
  do i = 1, merge(n, 1, present(n))
    out = [(out(j+1) - out(j), j = 1, size(out)-1)]
  end do

end function diff_i32


function diff_i64 (a, n) result (out)
  implicit none
  integer(INT64), allocatable :: out(:)
  integer(INT64), intent(in) :: a(:)
  integer, intent(in), optional :: n
  integer :: i, j

  out = a
  do i = 1, merge(n, 1, present(n))
    out = [(out(j+1) - out(j), j = 1, size(out)-1)]
  end do

end function diff_i64


function diff_r32 (a, n) result (out)
  implicit none
  real(REAL32), allocatable :: out(:)
  real(REAL32), intent(in) :: a(:)
  integer, intent(in), optional :: n
  integer :: i, j

  out = a
  do i = 1, merge(n, 1, present(n))
    out = [(out(j+1) - out(j), j = 1, size(out)-1)]
  end do

end function diff_r32


function diff_r64 (a, n) result (out)
  implicit none
  real(REAL64), allocatable :: out(:)
  real(REAL64), intent(in) :: a(:)
  integer, intent(in), optional :: n
  integer :: i, j

  out = a
  do i = 1, merge(n, 1, present(n))
    out = [(out(j+1) - out(j), j = 1, size(out)-1)]
  end do

end function diff_r64


function cumsum_i32 (a) result (out)
  implicit none
  integer(INT32), allocatable :: out(:)
  !! Cumulative sum result. The result has the same size as `a`.
  integer(INT32), intent(in) :: a(:)
  !! Input array.
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) + out(i-1)
  end do

end function cumsum_i32


function cumsum_i64 (a) result (out)
  implicit none
  integer(INT64), allocatable :: out(:)
  integer(INT64), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) + out(i-1)
  end do

end function cumsum_i64


function cumsum_r32 (a) result (out)
  implicit none
  real(REAL32), allocatable :: out(:)
  real(REAL32), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) + out(i-1)
  end do

end function cumsum_r32


function cumsum_r64 (a) result (out)
  implicit none
  real(REAL64), allocatable :: out(:)
  real(REAL64), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) + out(i-1)
  end do

end function cumsum_r64


function cumprod_i32 (a) result (out)
  implicit none
  integer(INT32), allocatable :: out(:)
  !! Cumulative product result. The result has the same size as `a`.
  integer(INT32), intent(in) :: a(:)
  !! Input array.
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) * out(i-1)
  end do

end function cumprod_i32


function cumprod_i64 (a) result (out)
  implicit none
  integer(INT64), allocatable :: out(:)
  integer(INT64), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) * out(i-1)
  end do

end function cumprod_i64


function cumprod_r32 (a) result (out)
  implicit none
  real(REAL32), allocatable :: out(:)
  real(REAL32), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) * out(i-1)
  end do

end function cumprod_r32


function cumprod_r64 (a) result (out)
  implicit none
  real(REAL64), allocatable :: out(:)
  real(REAL64), intent(in) :: a(:)
  integer :: i

  out = a(:)
  do i = 2, size(a)
    out(i) = out(i) * out(i-1)
  end do

end function cumprod_r64


function interp_r32 (x, xp, yp) result (out)
  implicit none
  real(REAL32), intent(in) :: x(:)
  !! The x-coordinates at which to evaluate the interpolated values.
  real(REAL32), intent(in) :: xp(:)
  !! The x-coordinates of the data points to be interpolated. Must be same size as `yp`.
  real(REAL32), intent(in) :: yp(:)
  !! The y-coordinates of the data points to be interpolated. Must be same size as `xp`.
  real(REAL32), dimension(size(x)) :: out
  !! Interpolated values, same shape as `x`.
  integer :: i, n

  if (size(xp) /= size(yp)) error stop "Input arrays must be same size"

  do n = 1, size(x)
    i = 1
    do while (i < size(x)-1)
      if (xp(i+1) >= x(n)) exit
      i = i + 1
    end do
    out(n) = (yp(i+1) - yp(i)) / (xp(i+1) - xp(i)) * (x(n) - xp(i)) + yp(i)
  end do

end function interp_r32


function interp_r64 (x, xp, yp) result (out)
  implicit none
  real(REAL64), intent(in) :: x(:)
  real(REAL64), intent(in) :: xp(:), yp(:) 
  real(REAL64) :: out(size(x))
  integer :: i, n

  if (size(xp) /= size(yp)) error stop "Input arrays must be same size"

  do n = 1, size(x)
    i = 1
    do while (i < size(x) - 1)
      if (xp(i+1) >= x(n)) exit
      i = i + 1
    end do
    out(n) = (yp(i+1) - yp(i)) / (xp(i+1) - xp(i)) * (x(n) - xp(i)) + yp(i)
  end do

end function interp_r64


function trapz_r32 (y, x) result (out)
  implicit none
  real(REAL32) :: out
  !! Definite integral of `y`.
  real(REAL32), intent(in) :: y(:)
  !! Input array to integrate.
  real(REAL32), intent(in) :: x(:)
  !! The sample points corresponding to the `y` values. Must be same size as `y`.
  integer :: i

  if (size(x) /= size(y)) error stop "Input arrays must be same size"

  out = 0.
  do i = 1, size(y) - 1
    out = out + 0.5 * (y(i+1) + y(i)) * (x(i+1) - x(i))
  end do
 
end function trapz_r32


function trapz_r64 (y, x) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: y(:), x(size(y))
  integer :: i

  out = 0.
  do i = 1, size(y) - 1
    out = out + 0.5 * (y(i+1) + y(i)) * (x(i+1) - x(i))
  end do
 
end function trapz_r64


function trapz_dx_r32 (y, dx) result (out)
  implicit none
  real(REAL32) :: out
  !! Definite integral of `y`.
  real(REAL32), intent(in) :: y(:)
  !! Input array to integrate.
  real(REAL32), intent(in), optional :: dx
  !! The spacing between sample points. Default: 1.0.
  real(REAL32) :: dx_
  integer :: i

  dx_ = merge(dx, 1.0, present(dx))
  out = 0.
  do i = 1, size(y) - 1
    out = out + 0.5 * (y(i+1) + y(i)) * dx_
  end do
 
end function trapz_dx_r32


function trapz_dx_r64 (y, dx) result (out)
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
 
end function trapz_dx_r64


function gradient_r32 (y, x) result (out)
  implicit none
  real(REAL32), intent(in) :: y(:)
  !! Input array to derivate.
  real(REAL32), intent(in) :: x(:)
  !! The sample points corresponding to the `y` values. Must be same size as `y`.
  real(REAL32) :: out(size(y))
  !! Derivative at each value of `y`. Is same shape as `y`.
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

end function gradient_r32


function gradient_r64 (y, x) result (out)
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

end function gradient_r64


function gradient_dx_r32 (y, dx) result (out)
  implicit none
  real(REAL32), intent(in) :: y(:)
  !! Input array to derivate.
  real(REAL32), intent(in), optional :: dx
  !! The spacing between sample points `y`. Default: 1.0.
  real(REAL32) :: out(size(y))
  !! Derivative at each value of `y`. Is same shape as `y`.
  real(REAL32) :: dx_
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

end function gradient_dx_r32


function gradient_dx_r64 (y, dx) result (out)
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

end function gradient_dx_r64


function factorial_int32 (n) result (out)
  implicit none
  integer(INT32) :: out
  !! Factorial of an input value.
  integer(INT32), intent(in) :: n
  !! Input value.
  integer :: i
  
  if (n < 0) error stop "Negative value input"
  out = 1
  do i = 1, n
    out = out * i
  end do
  
end function factorial_int32


function factorial_int64 (n) result (out)
  implicit none
  integer(INT64) :: out, i 
  integer(INT64), intent(in) :: n
  
  if (n < 0) error stop "Negative value input"
  out = 1
  do i = 1, n
    out = out * i
  end do
  
end function factorial_int64


function perm_int32 (n, k) result (out)
  implicit none
  integer(INT32) :: out
  !! Number of k-permutations of n.
  integer(INT32), intent(in) :: k
  !! Number of objects chosen.
  integer(INT32), intent(in) :: n
  !! Number of distinct objects.

  if (n < 0 .or. k < 0) error stop "Negative value input"
  if (k <= n) then
    out = factorial_int32(n) / factorial_int32(n - k)
  else
    out = 0
  end if

end function perm_int32 


function perm_int64 (n, k) result (out)
  implicit none
  integer(INT64) :: out
  integer(INT64), intent(in) :: n, k

  if (n < 0 .or. k < 0) error stop "Negative value input"
  if (k <= n) then
    out = factorial_int64(n) / factorial_int64(n - k)
  else
    out = 0
  end if

end function perm_int64 


function comb_int32 (n, k) result (out)
  implicit none
  integer(INT32) :: out
  !! Number of k-combinations of n.
  integer(INT32), intent(in) :: k
  !! Number of objects chosen.
  integer(INT32), intent(in) :: n
  !! Number of distinct objects.

  if (n < 0 .or. k < 0) error stop "Negative value input"
  if (k <= n) then
    out = factorial_int32(n) / (factorial_int32(k) * factorial_int32(n - k))
  else
    out = 0
  end if

end function comb_int32 


function comb_int64 (n, k) result (out)
  implicit none
  integer(INT64) :: out
  integer(INT64), intent(in) :: n, k

  if (n < 0 .or. k < 0) error stop "Negative value input"
  if (k <= n) then
    out = factorial_int64(n) / (factorial_int64(k) * factorial_int64(n - k))
  else
    out = 0
  end if

end function comb_int64 


function mix_real32 (a, b, x) result (out)
  implicit none
  real(REAL32) :: out
  !! Two input values to mix.
  real(REAL32), intent(in) :: a
  !! Fist input values to mix.
  real(REAL32), intent(in) :: b
  !! Second input values to mix.
  real(REAL32), intent(in) :: x
  !! Fraction of the mixture, ranging form 0 to 1.

  out = a + x * (b - a)

end function mix_real32


function mix_real64 (a, b, x) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a, b, x

  out = a + x * (b - a)

end function mix_real64


elemental function clip_real32 (a, lower, upper) result (out)
  implicit none
  real(REAL32) :: out
  !! Clipped number
  real(REAL32), intent(in) :: a
  !! Input value to clip.
  real(REAL32), intent(in) :: lower
  !! Lower bounds. 
  real(REAL32), intent(in) :: upper
  !! Upper bounds. 

  out = max(min(a, upper), lower)

end function clip_real32


elemental function clip_real64 (a, lower, upper) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a, lower, upper

  out = max(min(a, upper), lower)

end function clip_real64


recursive function gcd_int32 (a, b) result (out)
  implicit none
  integer(INT32) :: out
  !! Greatest common divisor of two inputs.
  integer(INT32), intent(in) :: a
  !! First input.
  integer(INT32), intent(in) :: b
  !! Second input.

  if (mod(a, b) /= 0) then
    out = gcd_int32(b, mod(a, b))
  else
    out = b
  end if

end function gcd_int32


recursive function gcd_int64 (a, b) result (out)
  implicit none
  integer(INT64) :: out
  integer(INT64), intent(in) :: a, b

  if (mod(a, b) /= 0) then
    out = gcd_int64(b, mod(a, b))
  else
    out = b
  end if

end function gcd_int64


function lcm_int32 (a, b) result (out)
  implicit none
  integer(INT32) :: out
  !! Least common multiple of two inputs.
  integer(INT32), intent(in) :: a
  !! First input.
  integer(INT32), intent(in) :: b
  !! Second input.

  out = a * b / gcd_int32(a, b)

end function lcm_int32


function lcm_int64 (a, b) result (out)
  implicit none
  integer(INT64) :: out
  integer(INT64), intent(in) :: a, b

  out = a * b / gcd_int64(a, b)

end function lcm_int64


end module xslib_math
