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

module xslib_math
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: factorial, perm, comb, mix, clip, isClose, gcd, lcm
  
  ! %%%
  ! # `MATH` - Basic mathematical functions
  !   Module `xslib_math` contains basic mathematical functions. Supports both single and double precision (`DP`).
  ! %%%

  interface factorial
    module procedure :: factorial_int32, factorial_int64
  end interface factorial

  interface perm
    module procedure :: perm_int32, perm_int64
  end interface perm

  interface comb
    module procedure :: comb_int32, comb_int64
  end interface comb 

  interface mix
    module procedure :: mix_real32, mix_real64
  end interface mix 

  interface clip
    module procedure :: clip_real32, clip_real64
  end interface clip 

  interface isClose
    module procedure :: isClose_real32, isClose_real64
  end interface isClose

  interface gcd
    module procedure :: gcd_int32, gcd_int64
  end interface gcd 

  interface lcm
    module procedure :: lcm_int32, lcm_int64 
  end interface lcm 

contains

function factorial_int32 (n) result (out)
  ! %%%
  ! ## `FACTORIAL` - Return factorial of an integer
  ! #### DESCRIPTION
  !   Return factorial of an integer `n`. Returns error if `n` is not integral or is negative.
  ! #### USAGE
  !   ```Fortran
  !   out = factorial(n)
  !   ```
  ! #### PARAMETERS
  !   * `integer(ANY), intent(IN) :: n`
  !     Input value.
  !   * `integer(ANY) :: out`
  !     Factorial of an input value.
  ! #### EXAMPLE
  !   ```Fortran
  !   > factorial(5)
  !   120
  !   ```
  ! %%%
  implicit none
  integer(INT32) :: out, i 
  integer(INT32), intent(in) :: n
  
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
  ! %%%
  ! ## `PERM` - Permutation of two numbers
  ! #### DESCRIPTION
  !   Return the number of ways to choose `k` items from `n` items without repetition and with order.
  ! #### USAGE
  !   ```Fortran
  !   out = perm(n, k)
  !   ```
  ! #### PARAMETERS
  !   * `integer(ANY), intent(IN) :: k, n`
  !     ...
  !   * `integer(ANY) :: out`
  !     ...
  ! #### EXAMPLE
  ! ```Fortran
  ! > perm(6, 4)
  ! 360
  ! ```
  ! %%%
  implicit none
  integer(INT32) :: out
  integer(INT32), intent(in) :: n, k

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
  ! %%%
  ! ## `COMB` - Combination of two numbers
  ! #### DESCRIPTION
  !   Return the number of ways to choose `k` items from `n` items without repetition and without order.
  ! #### USAGE
  !   ```Fortran
  !   out = comb(n, k)
  !   ```
  ! #### PARAMETERS
  !   * `integer(ANY), intent(IN) :: k, n`
  !     ...
  !   * `integer(ANY) :: out`
  !     ...
  ! #### EXAMPLE
  ! ```Fortran
  ! > comb(6, 4)
  ! 15
  ! ```
  ! %%%
  implicit none
  integer(INT32) :: out
  integer(INT32), intent(in) :: n, k

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
  ! %%%
  ! ## `MIX` - Linear interpolation of two numbers
  ! #### DESCRIPTION
  !   Return mix *i.e.* fractional linear interpolation between two values. If `x = 0.0` then 
  !   return `a` if `x = 1.0` return `b` otherwise return linear interpolation of `a` and `b`.
  ! #### USAGE
  !   ```Fortran
  !   out = mix(a, b, x)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), intent(IN) :: a, b`
  !     Two input values to mix.
  !   * `real(ANY), intent(IN) :: x`
  !     Fraction of the mixture: form 0.0 to 1.0.
  !   * `real(ANY) :: out`
  !     Output value.
  ! #### EXAMPLE
  !   ```Fortran
  !   > mix(0.0, 5.0, 0.25)
  !   1.25
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a, b, x

  out = a + x * (b - a)

end function mix_real32


function mix_real64 (a, b, x) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a, b, x

  out = a + x * (b - a)

end function mix_real64


elemental function clip_real32 (a, lower, upper) result (out)
  ! %%%
  ! ## `CLIP` - Limit the values in an array.
  ! #### DESCRIPTION
  !   Clip (limit) the values in an array.
  ! 
  !   Given an interval, values outside the interval are clipped to the interval edges.
  !   For example, if an interval of `[0, 1]` is specified, values smaller than 0 become `0`,
  !   and values larger than 1 become `1`.
  ! #### USAGE
  !   ```Fortran
  !   out = clip(a, lower, upper)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), intent(IN) :: a`
  !     Input value
  !   * `real(ANY), intent(IN) :: lower, upper`
  !     Upper and lower bounds. 
  !   * `real(ANY) :: out`
  !     Output value.
  ! #### EXAMPLE
  !   ```Fortran
  !   > clip(0.9, 1., 2.)
  !   1.00000
  !   > clip(1.1, 1., 2.)
  !   1.10000
  !   > clip(2.1, 1., 2.)
  !   2.00000
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a, lower, upper

  out = max(min(a, upper), lower)

end function clip_real32


elemental function clip_real64 (a, lower, upper) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a, lower, upper

  out = max(min(a, upper), lower)

end function clip_real64


function isClose_real32 (a, b, rtol, atol) result (out)
  ! %%%
  ! ## `ISCLOSE` - Checks if two values are within a tolerance
  ! #### DESCRIPTION
  !   Checks if two values are within a tolerance. The tolerance values are positive,
  !   typically very small numbers. The relative difference `(rtol * abs(b))` and the
  !   absolute difference `atol` are added together to compare against the absolute
  !   difference between `a` and `b`.
  ! #### USAGE
  !   ```Fortran
  !   out = isClose(a, b, rtol=rtol, atol=atol)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), intent(IN) :: a, b`
  !     Input values to compare.
  !   * `real(ANY), intent(IN), OPTIONAL :: rtol`
  !     The relative tolerance parameter. Default: 1.0e-05
  !   * `real(ANY), intent(IN), OPTIONAL :: rtol`
  !     The absolute tolerance parameter. Default: 1.0e-08
  !   * `logical :: out`
  !     Output value.
  ! #### EXAMPLE
  !   ```Fortran
  !   > isclose(1.1, 1.0, atol=1.0)
  !   .True.
  !   > isclose(1.5, 1.0, RTOL=1.0)
  !   .False.
  !   ```
  ! %%%
  implicit none
  logical :: out
  real(REAL32), intent(in) :: a, b
  real(REAL32), intent(in), optional :: rtol, atol
  real(REAL32) :: rtol_, atol_

  rtol_ = merge(rtol, 1.0e-05, present(rtol))
  atol_ = merge(atol, 1.0e-08, present(atol))
  out = (abs(a - b) <= (atol_ + rtol_ * abs(b)))

end function isClose_real32


function isClose_real64 (a, b, rtol, atol) result (out)
  implicit none
  logical :: out
  real(REAL64), intent(in) :: a, b
  real(REAL64), intent(in), optional :: rtol, atol
  real(REAL64) :: rtol_, atol_

  rtol_ = merge(rtol, 1.0d-05, present(rtol))
  atol_ = merge(atol, 1.0d-08, present(atol))
  out = (abs(a - b) <= (atol_ + rtol_ * abs(b)))

end function isClose_real64


recursive function gcd_int32 (a, b) result (out)
  ! %%%
  ! ## `GCD` - Return greatest common divisor
  ! #### DESCRIPTION
  !   Return the greatest common divisor (GCD) of the specified integer arguments.
  !   If any of the arguments is nonzero, then the returned value is the largest
  !   positive integer that is a divisor of all arguments. If all arguments are
  !   zero, then the returned value is 0.
  ! #### USAGE
  !   ```Fortran
  !   out = gcd (a, b)
  !   ```
  ! #### PARAMETERS
  !   * `integer(ANY), intent(IN) :: a, b`
  !     Input values.
  !   * `integer(ANY) :: out`
  !     Output value.
  ! #### EXAMPLE
  !   ```Fortran
  !   > gcd(106, 901)
  !   53
  !   ```
  ! %%%
  implicit none
  integer(INT32) :: out
  integer(INT32), intent(in) :: a, b

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
  ! %%%
  ! ## `LCM` - Return least common multiple
  ! #### DESCRIPTION
  !   Return the least common multiple (LCM) of the specified integer arguments.
  !   If all arguments are nonzero, then the returned value is the smallest
  !   positive integer that is a multiple of all arguments. If any of the arguments
  !   is zero, then the returned value is 0.
  ! #### USAGE
  !   ```Fortran
  !   out = lcm(a, b)
  !   ```
  ! #### PARAMETERS
  !   * `integer(ANY), intent(IN) :: a, b`
  !     Input values.
  !   * `integer(ANY) :: out`
  !     Output value.
  ! #### EXAMPLE
  !   ```Fortran
  !   > out = lcm(12, 17)
  !   204
  !   ```
  ! %%%
  implicit none
  integer(INT32) :: out
  integer(INT32), intent(in) :: a, b

  out = a * b / gcd_int32(a, b)

end function lcm_int32


function lcm_int64 (a, b) result (out)
  implicit none
  integer(INT64) :: out
  integer(INT64), intent(in) :: a, b

  out = a * b / gcd_int64(a, b)

end function lcm_int64

end module xslib_math
