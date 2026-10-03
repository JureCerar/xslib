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

  call test_array_gen_r32 ()
  call test_array_gen_r64 ()

contains

! Test array generating functions.
subroutine test_array_gen_r32 () 
  implicit none
  integer, parameter :: NP = 5
  real(REAL32) :: x(NP), y(NP), a(NP, NP), lower, upper, step

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

  a = eye(NP, mold=1_INT32)
  a = eye(NP, mold=1_INT64)
  a = eye(NP, mold=1.0_REAL32)
  a = eye(NP, mold=1.0_REAL64)
  if (sum(a) /= NP) error stop 4

end subroutine test_array_gen_r32

subroutine test_array_gen_r64 () 
  implicit none
  integer, parameter :: NP = 5
  real(REAL64) :: x(NP), y(NP), a(NP, NP), lower, upper, step

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

  a = eye(NP, mold=1_INT32)
  a = eye(NP, mold=1_INT64)
  a = eye(NP, mold=1.0_REAL32)
  a = eye(NP, mold=1.0_REAL64)
  if (sum(a) /= NP) error stop 

end subroutine test_array_gen_r64

end program main