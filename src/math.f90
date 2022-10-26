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

module xslib_math
  use iso_fortran_env, only: REAL32, REAL64
  implicit none
  private
  public :: linspace, logspace, arange, mix, clip, isClose, interp, trapz, gradient, gcd, lcm
  
  interface linspace
    module procedure :: linspace_real32, linspace_real64
  end interface linspace 

  interface logspace
    module procedure :: logspace_real32, logspace_real64
  end interface logspace 

  interface arange
    module procedure :: arange_real32, arange_real64
  end interface arange 

  interface mix
    module procedure :: mix_real32, mix_real64
  end interface mix 

  interface clip
    module procedure :: clip_real32, clip_real64
  end interface clip 

  interface isClose
    module procedure :: isClose_real32, isClose_real64
  end interface isClose

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

! Return evenly spaced numbers over a specified interval.
function linspace_real32 (start, stop, num) result (out)
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

! Return evenly spaced numbers on a log scale over a specified interval.
function logspace_real32 (start, stop, num) result (out)
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

! Return evenly spaced values within a given interval.
function arange_real32 (start, stop, step) result (out)
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

! Retrun fractional linear interpolation (mix) between two points.
function mix_real32 (a, b, x) result (out)
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

! Return clip (limit) of the value between lower and upper bound.
function clip_real32 (a, lower, upper) result (out)
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a, lower, upper

  out = max(min(a, upper), lower)

end function clip_real32

function clip_real64 (a, lower, upper) result (out)
  real(REAL64) :: out
  real(REAL64), intent(in) :: a, lower, upper

  out = max(min(a, upper), lower)

end function clip_real64

! Check if two values are within a tolerance.
function isClose_real32 (a, b, delta) result (bool)
  implicit none
  logical :: bool
  real(REAL32), intent(in) :: a, b, delta
  
  bool = (abs(a - b) <= delta)

end function isClose_real32

function isClose_real64 (a, b, delta) result (bool)
  implicit none
  logical :: bool
  real(REAL64), intent(in) :: a, b, delta
  
  bool = (abs(a - b) <= delta)

end function isClose_real64

! Linear interpolation for increasing array points.
function interp_real32 (x, xp, yp) result (out)
  implicit none
  real(REAL32), intent(in) :: x(:)
  real(REAL32), intent(in) :: xp(:), yp(size(xp)) 
  real(REAL32) :: out(size(x))
  integer :: i, n

  do n = 1, size(x)
    i = 1
    do while (i < size(x) - 1)
      if (xp(i+1) >= x(n)) exit
      i = i + 1
    end do
    out(n) = (yp(i+1)-yp(i)) / (xp(i+1)-xp(i)) * (x(n)-xp(i)) + yp(i)
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
    out(n) = (yp(i+1)-yp(i)) / (xp(i+1)-xp(i)) * (x(n)-xp(i)) + yp(i)
  end do

end function interp_real64

! Integrate given array using the composite trapezoidal rule.
function trapz_real32 (y, x) result (out)
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
  real(REAL32) :: out
  real(REAL32), intent(in) :: y(:), dx
  integer :: i

  out = 0.
  do i = 1, size(y) - 1
    out = out + 0.5 * (y(i+1) + y(i)) * dx
  end do
 
end function trapz_dx_real32

function trapz_dx_real64 (y, dx) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: y(:), dx
  integer :: i

  out = 0.
  do i = 1, size(y) - 1
    out = out + 0.5 * (y(i+1) + y(i)) * dx
  end do
 
end function trapz_dx_real64

! Calculate gradient (derivative) of an array using finite difference method.
function gradient_real32 (y, x) result (out)
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
  real(REAL32), intent(in) :: y(:), dx
  real(REAL32) :: out(size(y))
  integer :: i, np

  np = size(y)
  if (np > 2) then
    out(1) = (-3 * y(1) + 4 * y(2) - y(3)) / (2 * dx)
    do i = 2, np - 1
      out(i) = (y(i+1) - y(i-1)) / (2 * dx)
    end do
    out(np) = (y(np-2) - 4 * y(np-1) + 3 * y(np)) / (2 * dx)
  else if (np > 1) then
    out = (y(2) - y(1)) / dx
  else
    out = 0.0
  end if

end function gradient_dx_real32

function gradient_dx_real64 (y, dx) result (out)
  implicit none
  real(REAL64), intent(in) :: y(:), dx
  real(REAL64) :: out(size(y))
  integer :: i, np

  np = size(y)
  if (np > 2) then
    out(1) = (-3 * y(1) + 4 * y(2) - y(3)) / (2 * dx)
    do i = 2, np - 1
      out(i) = (y(i+1) - y(i-1)) / (2 * dx)
    end do
    out(np) = (y(np-2) - 4 * y(np-1) + 3 * y(np)) / (2 * dx)
  else if (np > 1) then
    out = (y(2) - y(1)) / dx
  else
    out = 0.0
  end if

end function gradient_dx_real64

! Return the greatest common divisor of the specified arguments
recursive function gcd (a, b) result (out)
  implicit none
  integer :: out
  integer, intent(in) :: a, b

  if (mod(a, b) /= 0) then
    out = gcd(b, mod(a, b))
  else
    out = b
  end if

end function gcd

! Return the least common multiple of the specified arguments
function lcm (a, b) result (out)
  implicit none
  integer :: out
  integer, intent(in) :: a, b

  out = a * b / gcd(a, b)

end function lcm

end module xslib_math

! program main
!   use xslib_math
!   implicit none
!   real, parameter :: PI = acos(-1.0)
!   integer, parameter :: NP = 8
!   real :: x(NP), y(NP), nx(NP*NP), ny(NP*NP)
!   integer :: i 

!   x = linspace(0., 2*PI, NP)
!   y = sin(x)
  
!   open (100, FILE="in.txt")
!   do i = 1, NP
!     write (100, *) x(i), y(i)
!   end do
!   close (100) 

!   nx = linspace(0., 2*PI, NP*NP)
!   ny = interp(nx, x, y)

!   open (100, FILE="out.txt")
!   do i = 1, NP*NP
!     write (100, *) nx(i), ny(i)
!   end do
!   close (100) 
  
! end program main