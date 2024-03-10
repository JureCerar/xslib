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
  use xslib_math
  implicit none
  real, parameter :: DELTA = 0.001

  call factorial_test ()
  call perm_test () 
  call comb_test ()
  call mixing_test_real32 ()
  call mixing_test_real64 ()
  call common_test ()

contains

! Test factorial
subroutine factorial_test()
  implicit none

  if (factorial(5_INT32) /= 120_INT32) error stop 1
  if (factorial(5_INT32) /= 120_INT32) error stop 1

  if (factorial(8_INT64) /= 40320_INT64) error stop 2
  if (factorial(8_INT64) /= 40320_INT64) error stop 2
  
end subroutine factorial_test

! Test permutation
subroutine perm_test()
  implicit none

  if (perm(6_INT32, 4_INT32) /= 360_INT32) error stop 1
  if (perm(6_INT64, 4_INT64) /= 360_INT64) error stop 2
  
end subroutine perm_test

! Test combination
subroutine comb_test()
  implicit none

  if (comb(6_INT32, 4_INT32) /= 15_INT32) error stop 1
  if (comb(6_INT64, 4_INT64) /= 15_INT64) error stop 2
  
end subroutine comb_test

! Test cliping functions.
subroutine mixing_test_real32 ()
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
  
end subroutine mixing_test_real32

subroutine mixing_test_real64 ()
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
  
end subroutine mixing_test_real64

! Test GCD and LCM functions.
subroutine common_test ()
  implicit none
  
  if (gcd(106, 901) /= 53) error stop 1
  if (gcd(106_INT64, 901_INT64) /= 53_INT64) error stop 1

  if (lcm(12, 17) /= 204) error stop 2
  if (lcm(12_INT64, 17_INT64) /= 204_INT64) error stop 2

end subroutine common_test

end program main