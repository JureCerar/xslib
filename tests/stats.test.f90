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
  use xslib_stats
  implicit none
  real, parameter :: delta = 0.001

  call average_test_int32 ()
  call average_test_int64 ()
  call average_test_real32 ()
  call average_test_real64 ()

  call welford_test_int32 ()
  call welford_test_int64 ()
  call welford_test_real32 ()
  call welford_test_real64 ()

  call histogram_test_int32 ()
  call histogram_test_int64 ()
  call histogram_test_real32 ()
  call histogram_test_real64 ()

contains 

! Test averaging functions.
subroutine average_test_int32 ()
  implicit none
  integer, parameter :: NP = 100
  integer(INT32) :: array(NP)
  real(REAL32) :: val
  integer :: i

  ! Array of integers from 1 to 100
  array = [(int(i, kind(array)), i = 1, NP)]

  val = mean(array)
  if (abs(val - 50.5) > delta) error stop

  val = gmean(array)
  if (abs(val - 37.992689344834304) > delta) error stop

  val = hmean(array)
  if (abs(val - 19.277563597396004) > delta) error stop
  
  val = stdev(array)
  if (abs(val - 28.866) > delta) error stop

  val = variance(array)
  if (abs(val - 833.25) > delta) error stop

  val = median(array)
  if (abs(val - 50.5) > delta) error stop

end subroutine average_test_int32 

subroutine average_test_int64 ()
  implicit none
  integer, parameter :: NP = 100
  integer(INT64) :: array(NP)
  real(REAL64) :: val
  integer :: i

  ! Array of integers from 1 to 100
  array = [(int(i, kind(array)), i = 1, NP)]

  val = mean(array)
  if (abs(val - 50.5) > delta) error stop

  val = gmean(array)
  print *, val
  if (abs(val - 37.992689344834304) > delta) error stop

  val = hmean(array)
  if (abs(val - 19.277563597396004) > delta) error stop
  
  val = stdev(array)
  if (abs(val - 28.866) > delta) error stop

  val = variance(array)
  if (abs(val - 833.25) > delta) error stop

  val = median(array)
  if (abs(val - 50.5) > delta) error stop

end subroutine average_test_int64 

subroutine average_test_real32 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL32) :: array(NP), val
  integer :: i

  ! Array of integers from 1 to 100
  array = [(int(i, kind(array)), i = 1, NP)]

  val = mean(array)
  if (abs(val - 50.5) > delta) error stop

  val = gmean(array)
  if (abs(val - 37.992689344834304) > delta) error stop

  val = hmean(array)
  if (abs(val - 19.277563597396004) > delta) error stop
  
  val = stdev(array)
  if (abs(val - 28.866) > delta) error stop

  val = variance(array)
  if (abs(val - 833.25) > delta) error stop

  val = median(array)
  if (abs(val - 50.5) > delta) error stop

end subroutine average_test_real32 

subroutine average_test_real64 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL64) :: array(NP), val
  integer :: i

  ! Array of integers from 1 to 100
  array = [(int(i, kind(array)), i = 1, NP)]

  val = mean(array)
  if (abs(val - 50.5) > delta) error stop

  val = gmean(array)
  if (abs(val - 37.992689344834304) > delta) error stop

  val = hmean(array)
  if (abs(val - 19.277563597396004) > delta) error stop
  
  val = stdev(array)
  if (abs(val - 28.866) > delta) error stop

  val = variance(array)
  if (abs(val - 833.25) > delta) error stop

  val = median(array)
  if (abs(val - 50.5) > delta) error stop

end subroutine average_test_real64 

! Test Welfords algorithm for running average.
subroutine welford_test_int32 ()
  implicit none
  integer, parameter :: NP = 100
  integer(INT32) :: array(NP)
  real(REAL32) :: mean(NP), var(NP)
  integer :: i

  mean = 0.
  var = 0.
  do i = 1, NP
    array = int(i, kind(array))
    call welford(array, mean, var, i)
  end do
  call welford_finalize(mean, var, NP)

  if (any(abs(mean - 50.5) > delta)) error stop
  if (any(abs(var - 833.25) > delta)) error stop

end subroutine welford_test_int32

subroutine welford_test_int64 ()
  implicit none
  integer, parameter :: NP = 100
  integer(INT64) :: array(NP)
  real(REAL64) :: mean(NP), var(NP)
  integer :: i

  mean = 0.
  var = 0.
  do i = 1, NP
    array = int(i, kind(array))
    call welford(array, mean, var, i)
  end do
  call welford_finalize(mean, var, NP)

  if (any(abs(mean - 50.5) > delta)) error stop
  if (any(abs(var - 833.25) > delta)) error stop

end subroutine welford_test_int64

subroutine welford_test_real32 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL32) :: array(NP), mean(NP), var(NP)
  integer :: i

  mean = 0.
  var = 0.
  do i = 1, NP
    array = real(i, kind(array))
    call welford(array, mean, var, i)
  end do
  call welford_finalize(mean, var, NP)

  if (any(abs(mean - 50.5) > delta)) error stop
  if (any(abs(var - 833.25) > delta)) error stop

end subroutine welford_test_real32

subroutine welford_test_real64 ()
  implicit none
  integer, parameter :: NP = 100
  real(REAL64) :: array(NP), mean(NP), var(NP)
  integer :: i

  mean = 0.
  var = 0.
  do i = 1, NP
    array = real(i, kind(array))
    call welford(array, mean, var, i)
  end do
  call welford_finalize(mean, var, NP)

  if (any(abs(mean - 50.5) > delta)) error stop
  if (any(abs(var - 833.25) > delta)) error stop

end subroutine welford_test_real64

! Test histogram w/ normal distribution.
! NOTE: Array is randomly generated so `ave` and `stdev` values can be far 
! away expected values. Use large delta when comparing w/ expected values.
subroutine histogram_test_int32 ()
  implicit none
  integer, parameter :: NP = 1000, BINS = 21
  integer(INT32) :: array(NP)
  real(REAL32) :: mu, sigma, binsize, val
  integer :: i, time, hist(BINS)

  call system_clock (time)
  call srand (time)

  mu = 100.0
  sigma = 30.0
  array = [(int(normal(mu, sigma), kind(array)), i = 1, NP)]

  hist = histogram(array, BINS, 0., 200.)
  binsize = 200.0 / (BINS - 1)

  val = mean(array)
  if (abs(val - mu) > 5.0) error stop
  val = stdev(array)
  if (abs(val - sigma) > 5.0) error stop

end subroutine histogram_test_int32

subroutine histogram_test_int64 ()
  implicit none
  real, parameter :: TOL = 10.0
  integer, parameter :: NP = 1000, BINS = 21
  integer(INT64) :: array(NP)
  real(REAL64) :: mu, sigma, binsize, val
  integer :: i, time, hist(BINS)

  call system_clock (time)
  call srand (time)

  mu = 100.0
  sigma = 30.0
  array = [(int(normal(mu, sigma), kind(array)), i = 1, NP)]

  hist = histogram(array, BINS, 0., 200.)
  binsize = 200.0 / (BINS - 1)

  val = mean(array)
  if (abs(val - mu) > TOL) error stop
  val = stdev(array)
  if (abs(val - sigma) > TOL) error stop

end subroutine histogram_test_int64

subroutine histogram_test_real32 ()
  implicit none
  real, parameter :: TOL = 10.0
  integer, parameter :: NP = 1000, BINS = 21
  real(REAL32) :: array(NP)
  real(REAL32) :: mu, sigma, binsize, val
  integer :: i, time, hist(BINS)

  call system_clock (time)
  call srand (time)

  mu = 100.0
  sigma = 30.0
  array = [(int(normal(mu, sigma), kind(array)), i = 1, NP)]

  hist = histogram(array, BINS, 0., 200.)
  binsize = 200.0 / (BINS - 1)

  val = mean(array)
  if (abs(val - mu) > TOL) error stop
  val = stdev(array)
  if (abs(val - sigma) > TOL) error stop

end subroutine histogram_test_real32

subroutine histogram_test_real64 ()
  implicit none
  real, parameter :: TOL = 10.0
  integer, parameter :: NP = 1000, BINS = 21
  real(REAL64) :: array(NP)
  real(REAL64) :: mu, sigma, binsize, val
  integer :: i, time, hist(BINS)

  call system_clock (time)
  call srand (time)

  mu = 100.0
  sigma = 30.0
  array = [(int(normal(mu, sigma), kind(array)), i = 1, NP)]

  hist = histogram(array, BINS, 0., 200.)
  binsize = 200.0 / (BINS - 1)

  val = mean(array)
  if (abs(val - mu) > TOL) error stop
  val = stdev(array)
  if (abs(val - sigma) > TOL) error stop

end subroutine histogram_test_real64

end program main