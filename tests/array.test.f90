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

program main
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  use xslib_array
  implicit none
  real, parameter :: DELTA = 0.001

  call generate_test_real32 ()
  call generate_test_real64 ()

  call diff_test_int32 ()
  call diff_test_int64 ()
  call diff_test_real32 ()
  call diff_test_real64 ()

  call cum_test_int32 ()
  call cum_test_int64 ()
  call cum_test_real32 ()
  call cum_test_real64 ()

  call interp_test_real32 ()
  call interp_test_real64 ()

  call integral_test_real32 ()
  call integral_test_real64 ()

  call derivative_test_real32 ()
  call derivative_test_real64 ()

contains

! Test array generating functions.
subroutine generate_test_real32 () 
  implicit none
  integer, parameter :: NP = 5
  real(REAL32) :: x(NP), y(NP), lower, upper, step

  lower = 1.
  upper = 5.
  x = linspace(lower, upper, NP)
  y = [1.,2.,3.,4.,5.]
  if (any(abs(x - y) > DELTA)) error stop 1

  lower = 1.0
  upper = 10000.
  x = logspace(lower, upper, NP)
  y = [1.,10.,100.,1000.,10000.]
  if (any(abs(x - y) > DELTA)) error stop 2

  lower = 1.
  upper = 5.
  step = 1.
  x = arange(lower, upper, step)
  y = [1.,2.,3.,4.,5.]
  if (any(abs(x - y) > DELTA)) error stop 3

end subroutine generate_test_real32

subroutine generate_test_real64 () 
  implicit none
  integer, parameter :: NP = 5
  real(REAL64) :: x(NP), y(NP), lower, upper, step

  lower = 1.
  upper = 5.
  x = linspace(lower, upper, NP)
  y = [1.,2.,3.,4.,5.]
  if (any(abs(x - y) > DELTA)) error stop 1

  lower = 1.
  upper = 10000.
  x = logspace(lower, upper, NP)
  y = [1.,10.,100.,1000.,10000.]
  if (any(abs(x - y) > DELTA)) error stop 2

  lower = 1.
  upper = 5.
  step = 1.
  x = arange(lower, upper, step)
  y = [1.,2.,3.,4.,5.]
  if (any(abs(x - y) > DELTA)) error stop 3

end subroutine generate_test_real64

! Test diff function
subroutine diff_test_int32 () 
  implicit none
  integer(INT32), allocatable :: x(:), y(:)

  x = diff([1, 2, 4, 8])
  if (size(x) /= 3) error stop 1
  y = [1, 2, 4]
  if (any(x /= y)) error stop 1

  x = diff([1, 2, 4, 8], n=2)
    if (size(x) /= 2) error stop 1
  y = [1, 2]
  if (any(x /= y)) error stop 1

end subroutine diff_test_int32

subroutine diff_test_int64 () 
  implicit none
  integer(INT64), allocatable :: x(:), y(:)

  x = diff([1, 2, 4, 8])
  if (size(x) /= 3) error stop 1
  y = [1, 2, 4]
  if (any(x /= y)) error stop 1

  x = diff([1, 2, 4, 8], n=2)
    if (size(x) /= 2) error stop 1
  y = [1, 2]
  if (any(x /= y)) error stop 1

end subroutine diff_test_int64

subroutine diff_test_real32 () 
  implicit none
  real(REAL32), allocatable :: x(:), y(:)

  x = diff([1.0, 2.0, 4.0, 8.0])
  if (size(x) /= 3) error stop 1
  y = [1.0, 2.0, 4.0]
  if (any(abs(x - y) > DELTA)) error stop 2

  x = diff([1.0, 2.0, 4.0, 8.0], n=2)
  if (size(x) /= 2) error stop 3
  y = [1.0, 2.0]
  if (any(abs(x - y) > DELTA)) error stop 4

end subroutine diff_test_real32

subroutine diff_test_real64 () 
  implicit none
  real(REAL64), allocatable :: x(:), y(:)

  x = diff([1.0d0, 2.0d0, 4.0d0, 8.0d0])
  if (size(x) /= 3) error stop 1
  y = [1.0d0, 2.0d0, 4.0d0]
  if (any(abs(x - y) > DELTA)) error stop 2

  x = diff([1.0d0, 2.0d0, 4.0d0, 8.0d0], n=2)
  if (size(x) /= 2) error stop 3
  y = [1.0d0, 2.0d0]
  if (any(abs(x - y) > DELTA)) error stop 4

end subroutine diff_test_real64

! Test cumulative functions
subroutine cum_test_int32 ()
  implicit none
  integer(INT32), allocatable :: x(:), y(:)

  x = cumsum([1, 2, 3, 4, 5])
  y = [1, 3, 6, 10, 15]
  if (any(x /= y)) error stop 1

  x = cumprod([1, 2, 3, 4, 5])
  y = [1, 2, 6, 24, 120]
  if (any(x /= y)) error stop 2

end subroutine cum_test_int32

subroutine cum_test_int64 ()
  implicit none
  integer(INT32), allocatable :: x(:), y(:)

  x = [1, 2, 3, 4, 5]
  x = cumsum(x)
  y = [1, 3, 6, 10, 15]
  if (any(x /= y)) error stop 1

  x = [1, 2, 3, 4, 5]
  x = cumprod(x)
  y = [1, 2, 6, 24, 120]
  if (any(x /= y)) error stop 2

end subroutine cum_test_int64

subroutine cum_test_real32 ()
  implicit none
  real(REAL32), allocatable :: x(:), y(:)

  x = cumsum([1.0, 2.0, 3.0, 4.0, 5.0])
  y = [1.0, 3.0, 6.0, 10.0, 15.0]
  if (any(abs(x - y) > DELTA)) error stop 1

  x = cumprod([1.0, 2.0, 3.0, 4.0, 5.0])
  y = [1.0, 2.0, 6.0, 24.0, 120.0]
  if (any(abs(x - y) > DELTA)) error stop 2

end subroutine cum_test_real32

subroutine cum_test_real64 ()
  implicit none
  real(REAL64), allocatable :: x(:), y(:)

  x = cumsum([1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0])
  y = [1.0d0, 3.0d0, 6.0d0, 10.0d0, 15.0d0]
  if (any(abs(x - y) > DELTA)) error stop 1

  x = cumprod([1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0])
  y = [1.0d0, 2.0d0, 6.0d0, 24.0d0, 120.0d0]
  if (any(abs(x - y) > DELTA)) error stop 2

end subroutine cum_test_real64

! Test interpolation functions.
subroutine interp_test_real32 ()
  implicit none
  real, parameter :: PI = acos(-1.0)
  integer, parameter :: NP = 100
  real(REAL32) :: x(NP), y(NP)
  real(REAL32) :: nx(2*NP), ny(2*NP)

  x = linspace(0., PI, NP)
  y = sin(x)

  nx = linspace(0., PI, 2*NP)
  ny = interp(nx, x, y)

  if (any(abs(ny - sin(nx)) > DELTA)) error stop

end subroutine interp_test_real32

subroutine interp_test_real64 ()
  implicit none
  real, parameter :: PI = acos(-1.0)
  integer, parameter :: NP = 100
  real(REAL64) :: x(NP), y(NP)
  real(REAL64) :: nx(2*NP), ny(2*NP)

  x = linspace(0., PI, NP)
  y = sin(x)

  nx = linspace(0., PI, 2*NP)
  ny = interp(nx, x, y)

  if (any(abs(ny - sin(nx)) > DELTA)) error stop

end subroutine interp_test_real64

! Test integration functions.
subroutine integral_test_real32 ()
  implicit none
  real, parameter :: PI = acos(-1.0)
  integer, parameter :: NP = 1000
  real(REAL32) :: x(NP), y(NP), dx, area

  x = linspace(0., PI, NP)
  y = sin(x)
  dx = x(2) - x(1)

  ! sin(x) dx = 2.
  area = trapz(y, x)
  if (abs(area - 2.0) > DELTA) error stop
  area = trapz(y, dx)
  if (abs(area - 2.0) > DELTA) error stop

end subroutine integral_test_real32

subroutine integral_test_real64 ()
  implicit none
  real, parameter :: PI = acos(-1.0)
  integer, parameter :: NP = 1000
  real(REAL64) :: x(NP), y(NP), dx, area

  x = linspace(0., PI, NP)
  y = sin(x)
  dx = x(2) - x(1)

  ! sin(x) dx = 2.
  area = trapz(y, x)
  if (abs(area - 2.0) > DELTA) error stop
  area = trapz(y, dx)
  if (abs(area - 2.0) > DELTA) error stop

end subroutine integral_test_real64

! Test derivative functions.
subroutine derivative_test_real32 ()
  implicit none
  real, parameter :: PI = acos(-1.0)
  integer, parameter :: NP = 100
  real(REAL32) :: x(NP), y(NP), dx, xp(NP)

  x = linspace(0., PI, NP)
  y = sin(x)
  dx = x(2) - x(1)

  ! d/dx sin(x) = cos(x)
  xp = gradient(y, x)
  if (any(abs(xp - cos(x)) > DELTA)) error stop
  xp = gradient(y, dx)
  if (any(abs(xp - cos(x)) > DELTA)) error stop

end subroutine derivative_test_real32

subroutine derivative_test_real64 ()
  implicit none
  real, parameter :: PI = acos(-1.0)
  integer, parameter :: NP = 100
  real(REAL64) :: x(NP), y(NP), dx, xp(NP)

  x = linspace(0., PI, NP)
  y = sin(x)
  dx = x(2) - x(1)

  ! d/dx sin(x) = cos(x)
  xp = gradient(y, x)
  if (any(abs(xp - cos(x)) > DELTA)) error stop
  xp = gradient(y, dx)
  if (any(abs(xp - cos(x)) > DELTA)) error stop

end subroutine derivative_test_real64

end program main