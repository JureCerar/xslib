! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2022 Jure Cerar
!
! This file is part of xslib
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
  use xslib_fitting
  implicit none
  real, parameter :: DELTA = 0.001

  call linfit_test_real32 ()
  call linfit_test_real64 ()

  call polyfit_test_real32 ()
  call polyfit_test_real64 ()

  call polyval_test_real32 ()
  call polyval_test_real64 ()

contains

! Test linear fitting function.
subroutine linfit_test_real32 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL32) :: x(NP), y(NP), coeff(2)
  real :: rmin, rmax, step
  integer :: i

  rmin = -8
  rmax = 8
  step = (rmax - rmin) / (NP - 1)
  x = [(rmin + (i-1) * step, i = 1, NP)]
  y = 3.2 * x + 1.7
  
  coeff = linfit(x, y)
  
  if (abs(coeff(1) - 1.7) > DELTA) error stop
  if (abs(coeff(2) - 3.2) > DELTA) error stop

end subroutine linfit_test_real32

subroutine linfit_test_real64 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL64) :: x(NP), y(NP), coeff(2)
  real :: rmin, rmax, step
  integer :: i

  rmin = -8
  rmax = 8
  step = (rmax - rmin) / (NP - 1)
  x = [(rmin + (i-1) * step, i = 1, NP)]
  y = 3.2 * x + 1.7
  
  coeff = linfit(x, y)
  
  if (abs(coeff(1) - 1.7) > DELTA) error stop
  if (abs(coeff(2) - 3.2) > DELTA) error stop

end subroutine linfit_test_real64

! Test polynomial fitting function.
subroutine polyfit_test_real32 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL32) :: x(NP), y(NP), coeff(4)
  real :: rmin, rmax, step
  integer :: i

  rmin = -8
  rmax = 8
  step = (rmax - rmin) / (NP - 1)
  x = [(rmin + (i-1) * step, i = 1, NP)]
  y = 0.7 * x**3 + 2.1 * x**2 - 5.3 * x + 6.7
  
  coeff = polyfit(x, y, 3)
  
  if (abs(coeff(1) - 6.7) > DELTA) error stop 1
  if (abs(coeff(2) + 5.3) > DELTA) error stop 2
  if (abs(coeff(3) - 2.1) > DELTA) error stop 3 
  if (abs(coeff(4) - 0.7) > DELTA) error stop 4 

end subroutine polyfit_test_real32

subroutine polyfit_test_real64 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL64) :: x(NP), y(NP), coeff(4)
  real :: rmin, rmax, step
  integer :: i

  rmin = -8
  rmax = 8
  step = (rmax - rmin) / (NP - 1)
  x = [(rmin + (i-1) * step, i = 1, NP)]
  y = 0.7 * x**3 + 2.1 * x**2 - 5.3 * x + 6.7
  
  coeff = polyfit(x, y, 3)
  
  if (abs(coeff(1) - 6.7) > DELTA) error stop 1
  if (abs(coeff(2) + 5.3) > DELTA) error stop 2
  if (abs(coeff(3) - 2.1) > DELTA) error stop 3 
  if (abs(coeff(4) - 0.7) > DELTA) error stop 4 

end subroutine polyfit_test_real64

! Test polynomial value generation
subroutine polyval_test_real32 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL32) :: val, x(NP), y(NP), z(NP), coeff(4)
  real :: rmin, rmax, step
  integer :: i

  rmin = -8
  rmax = 8
  step = (rmax - rmin) / (NP - 1)
  x = [(rmin + (i-1) * step, i = 1, NP)]
  y = 0.7 * x**3 + 2.1 * x**2 - 5.3 * x + 6.7
  
  ! Array value test
  coeff = [6.7, -5.3, 2.1, 0.7]
  z = polyval(coeff, x)
  if (any(abs(y - z) > DELTA)) error stop 1

  ! Scalar value test
  coeff = [6.7, -5.3, 2.1, 0.7]
  val = polyval(coeff, 1.0)
  if (abs(val - 4.2) > DELTA) error stop 2

end subroutine polyval_test_real32

subroutine polyval_test_real64 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL64) :: val, x(NP), y(NP), z(NP), coeff(4)
  real :: rmin, rmax, step
  integer :: i

  rmin = -8
  rmax = 8
  step = (rmax - rmin) / (NP - 1)
  x = [(rmin + (i-1) * step, i = 1, NP)]
  y = 0.7 * x**3 + 2.1 * x**2 - 5.3 * x + 6.7
  
  ! Array value test
  coeff = [6.7, -5.3, 2.1, 0.7]
  z = polyval(coeff, x)
  if (any(abs(y - z) > DELTA)) error stop 1

  ! Scalar value test
  coeff = [6.7, -5.3, 2.1, 0.7]
  val = polyval(coeff, 1.0d0)
  if (abs(val - 4.2) > DELTA) error stop 2

end subroutine polyval_test_real64

end program main