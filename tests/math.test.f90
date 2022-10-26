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
  use iso_fortran_env, only: REAL32, REAL64
  use xslib_math
  implicit none
  real, parameter :: DELTA = 0.001

  call range_test_real32 ()
  call range_test_real64 ()

  call clip_test_real32 ()
  call clip_test_real64 ()

  call interp_test_real32 ()
  call interp_test_real64 ()

  call integral_test_real32 ()
  call integral_test_real64 ()

  call derivative_test_real32 ()
  call derivative_test_real64 ()

  call common_test ()

contains

! Test array generating functions.
subroutine range_test_real32 () 
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

end subroutine range_test_real32

subroutine range_test_real64 () 
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

end subroutine range_test_real64

! Test cliping functions.
subroutine clip_test_real32 ()
  implicit none
  real(REAL32) :: lower, upper, x, y
  
  lower = 0.
  upper = 1.

  ! Mix test
  x = 0.5
  y = mix(lower, upper, x)
  if (abs(y - 0.5) > DELTA) error stop

  x = 1.5
  y = mix(lower, upper, x)
  if (abs(y - 1.5) > DELTA) error stop

  ! Clip test
  x = 0.5
  y = clip(x, lower, upper)
  if (abs(y - 0.5) > DELTA) error stop
 
  x = 1.5
  y = clip(x, lower, upper)
  if (abs(y - 1.0) > DELTA) error stop

  x = -0.5
  y = clip(x, lower, upper)
  if (abs(y - 0.0) > DELTA) error stop

  ! IsClose test
  x = 1.0
  y = 1.0
  if (isClose(x, y, real(DELTA, kind(x))) .neqv. .True.) error stop

  x = 1.0
  y = 2.0
  if (isClose(x, y, real(DELTA, kind(x))) .neqv. .False.) error stop
  
end subroutine clip_test_real32

subroutine clip_test_real64 ()
  implicit none
  real(REAL64) :: lower, upper, x, y
  
  lower = 0.
  upper = 1.

  ! Mix test
  x = 0.5
  y = mix(lower, upper, x)
  if (abs(y - 0.5) > DELTA) error stop

  x = 1.5
  y = mix(lower, upper, x)
  if (abs(y - 1.5) > DELTA) error stop

  ! Clip test
  x = 0.5
  y = clip(x, lower, upper)
  if (abs(y - 0.5) > DELTA) error stop
 
  x = 1.5
  y = clip(x, lower, upper)
  if (abs(y - 1.0) > DELTA) error stop

  x = -0.5
  y = clip(x, lower, upper)
  if (abs(y - 0.0) > DELTA) error stop

  ! IsClose test
  x = 1.0
  y = 1.0
  if (isClose(x, y, real(DELTA, kind(x))) .neqv. .True.) error stop

  x = 1.0
  y = 2.0
  if (isClose(x, y, real(DELTA, kind(x))) .neqv. .False.) error stop
  
end subroutine clip_test_real64

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

! Test GCD and LCM functions.
subroutine common_test ()
  implicit none
  
  if (gcd(106, 901) /= 53) error stop 1
  if (lcm(12, 17) /= 204) error stop 2

end subroutine common_test

end program main