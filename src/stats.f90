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

module xslib_stats
  !! Module with basic statistics functions.
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: normal, mean, hmean, gmean, stdev, variance, median
  public :: welford, welford_finalize, histogram

  interface normal
    !! Return random samples from a normal (Gaussian) distribution. Random number sequence 
    !! can be initialized with [`srand`](https://gcc.gnu.org/onlinedocs/gfortran/SRAND.html) function.
    !!
    !! Example:
    !! ```Fortran
    !! print *, normal(0.0, 1.0)
    !! >>> 0.123456
    !! ```
    module procedure :: normal_r32, normal_r64
  end interface normal


  interface mean
    !! Return arithmetic mean value of an array.
    !! 
    !! Example:
    !! ```Fortran
    !! print *, mean([1, 2, 3, 4, 5])
    !! >>> 3.0
    !! ```
    module procedure :: mean_i32, mean_i64, mean_r32, mean_r64
  end interface mean


  interface gmean
    !! Return geometric mean value of an array.
    !! 
    !! Example:
    !! ```Fortran
    !! print *, gmean([1, 2, 3, 4, 5])
    !! >>> 2.60517120
    !! ```
    module procedure :: gmean_i32, gmean_i64, gmean_r32, gmean_r64
  end interface gmean


  interface hmean
    !! Return harmonic mean value of an array.
    !! 
    !! Example:
    !! ```Fortran
    !! print *, hmean([1, 2, 3, 4, 5])
    !! >>> 2.60517120
    !! ```
    module procedure :: hmean_i32, hmean_i64, hmean_r32, hmean_r64
  end interface hmean


  interface stdev
    !! Return standard deviation of an array.
    !! 
    !! Example:
    !! ```Fortran
    !! print *, stdev([1, 2, 3, 4, 5])
    !! >>> 1.58114
    !! ```
    module procedure :: stdev_i32, stdev_i64, stdev_r32, stdev_r64
  end interface stdev


  interface variance
    !! Calculate variance of an array.
    !!
    !! Example:
    !! ```Fortran
    !! print *, variance([1, 2, 3, 4, 5])
    !! >>> 2.5
    !! ```
    module procedure :: variance_i32, variance_i64, variance_r32, variance_r64
  end interface variance


  interface median
    !! Return the median (middle value) of numeric data, using the
    !! common _mean of middle two_ method.
    !!
    !! Example:
    !! ```Fortran
    !! print *, median([1, 2, 3, 4, 5])
    !! >>> 3.0
    !! print *, median([1, 2, 3, 4])
    !! >>> 2.5
    !! ```  
    module procedure :: median_i32, median_i64, median_r32, median_r64
  end interface median


  interface welford
    !! [Welford's online algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm) 
    !! for calculating variance. Final variance must be "corrected" with `welford_finalize` call.
    !!
    !! Example:
    !! ```Fortran
    !! do i = 1, NP
    !!   call random_number(array)
    !!   call welford(array, mean, variance, i)
    !! end do
    !! call welford_finalize(mean, variance, NP)
    !! print *, mean
    !! >>> [0.45705, 0.50651, ..., 0.48604, 0.47319]
    !! print *, variance
    !! >>> [0.10649, 0.03802, ..., 0.10703, 0.04501]
    !! ```  
    module procedure :: welford_i32, welford_i64, welford_r32, welford_r64
  end interface welford


  interface welford_finalize
    !! Correct final variance of Welford's online algorithm.
    !!
    !! Example:
    !! ```Fortran
    !! do i = 1, NP
    !!   call random_number(array)
    !!   call welford(array, mean, variance, i)
    !! end do
    !! call welford_finalize(mean, variance, NP)
    !! print *, mean
    !! >>> [0.45705, 0.50651, ..., 0.48604, 0.47319]
    !! print *, variance
    !! >>> [0.10649, 0.03802, ..., 0.10703, 0.04501]
    !! ```  
    module procedure :: welford_finalize_r32, welford_finalize_r64
  end interface welford_finalize


  interface histogram
    !! Calculate histogram distribution of an array.
    !! Histogram bin size equals to `abs(max - min) / (nbins - 1)`.
    !!
    !! Example:
    !! ```Fortran
    !! call random_number(array)
    !! print *, histogram(array, 5, MIN=0.0, MAX=1.0)
    !! >>> [9, 10, 8, 11, 11]
    !! ``` 
    module procedure :: histogram_i32, histogram_i64, histogram_r32, histogram_r64
  end interface histogram

contains

function normal_r32 (mu, sigma) result (out)
  ! See: https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
  implicit none
  real, parameter :: PI = acos(-1.0)
  real(REAL32), intent(in) :: mu
  !! The mean `mu` of normal distribution.  
  real(REAL32), intent(in) :: sigma
  !! The  variance `sigma` of normal distribution.  
  real(REAL32) :: out
  !! Random value on a normal distribution.

  out = mu + sigma * sqrt(-2 * log(rand())) * cos(2 * PI * rand())

end function normal_r32


function normal_r64 (mu, sigma) result (out)
  implicit none
  real, parameter :: PI = acos(-1.0)
  real(REAL64), intent(in) :: mu, sigma
  real(REAL64) :: out

  out = mu + sigma * sqrt(-2 * log(rand())) * cos(2 * PI * rand())

end function normal_r64


function mean_i32 (array) result (out)
  implicit none
  integer(INT32), intent(in) :: array(:)
  !! Input array.
  real(REAL32) :: out
  !! Arithmetic mean of an array.

  out = sum(real(array, KIND=REAL32)) / size(array)

end function mean_i32


function mean_i64 (array) result (out)
  implicit none
  integer(INT64), intent(in) :: array(:)
  real(REAL64) :: out

  out = sum(real(array, KIND=REAL64)) / size(array)

end function mean_i64


function mean_r32 (array) result (out)
  implicit none
  real(REAL32), intent(in) :: array(:)
  real(REAL32) :: out

  out = sum(array) / size(array)

end function mean_r32


function mean_r64 (array) result (out)
  implicit none
  real(REAL64), intent(in) :: array(:)
  real(REAL64) :: out

  out = sum(array) / size(array)

end function mean_r64


function gmean_i32 (a) result (out)
  implicit none
  integer(INT32), intent(in) :: a(:)
  !! Input array.
  real(REAL32) :: out
  !! Geometric mean of an array.

  out = exp(sum(log(real(a, REAL32))) / size(a))

end function gmean_i32


function gmean_i64 (a) result (out)
  implicit none
  integer(INT64), intent(in) :: a(:)
  real(REAL64) :: out

  out = exp(sum(log(real(a, REAL64))) / size(a))

end function gmean_i64


function gmean_r32 (a) result (out)
  implicit none
  real(REAL32), intent(in) :: a(:)
  real(REAL32) :: out

  out = exp(sum(log(a)) / size(a))

end function gmean_r32


function gmean_r64 (a) result (out)
  implicit none
  real(REAL64), intent(in) :: a(:)
  real(REAL64) :: out

  out = exp(sum(log(a)) / size(a))

end function gmean_r64


function hmean_i32 (a) result (out)
  implicit none
  integer(INT32), intent(in) :: a(:)
  !! Input array. 
  real(REAL32) :: out
  !! Harmonic mean of an array. 
  integer :: i

  out = size(a) * 1.0 / sum([(1.0 / a(i), i = 1, size(a))])

end function hmean_i32


function hmean_i64 (a) result (out)
  implicit none
  integer(INT64), intent(in) :: a(:)
  real(REAL64) :: out
  integer :: i

  out = size(a) * 1.0d0 / sum([(1.0d0 / a(i), i = 1, size(a))])

end function hmean_i64


function hmean_r32 (a) result (out)
  implicit none
  real(REAL32), intent(in) :: a(:)
  real(REAL32) :: out
  integer :: i

  out = size(a) * 1.0 / sum([(1.0 / a(i), i = 1, size(a))])

end function hmean_r32


function hmean_r64 (a) result (out)
  implicit none
  real(REAL64), intent(in) :: a(:)
  real(REAL64) :: out
  integer :: i

  out = size(a) * 1.0 / sum([(1.0 / a(i), i = 1, size(a))])

end function hmean_r64


function stdev_i32 (array) result (out)
  implicit none
  integer(INT32), intent(in) :: array(:)
  !! Input array
  real(REAL32) :: out
  !! Standard deviation of array values. 
  real(REAL32) :: ave

  ave = sum(real(array, REAL32)) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stdev_i32


function stdev_i64 (array) result (out)
  implicit none
  integer(INT64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(real(array, REAL64)) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stdev_i64


function stdev_r32 (array) result (out)
  implicit none
  real(REAL32), intent(in) :: array(:)
  real(REAL32) :: out, ave

  ave = sum(array) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stdev_r32


function stdev_r64 (array) result (out)
  implicit none
  real(REAL64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(array) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stdev_r64


function variance_i32 (array) result (out)
  implicit none
  integer(INT32), intent(in) :: array(:)
  !! Input array.
  real(REAL32) :: out
  !! Variance of array values.
  real(REAL32) :: ave

  ave = sum(real(array, REAL32)) / size(array)
  out = sum((array - ave) ** 2) / size(array)

end function variance_i32


function variance_i64 (array) result (out)
  implicit none
  integer(INT64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(real(array, REAL64)) / size(array)
  out = sum((array - ave) ** 2) / size(array)

end function variance_i64


function variance_r32 (array) result (out)
  implicit none
  real(REAL32), intent(in) :: array(:)
  real(REAL32) :: out, ave

  ave = sum(array) / size(array)
  out = sum((array - ave) ** 2) / size(array)

end function variance_r32


function variance_r64 (array) result (out)
  implicit none
  real(REAL64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(array) / size(array)
  out = sum((array - ave) ** 2) / size(array)

end function variance_r64


function median_i32 (a) result (out)
  use xslib_sort, only: qsort
  implicit none
  integer(INT32), intent(in) :: a(:)
  !! Input array.
  real(REAL32) :: out
  !! Median value of an array. 
  integer(INT32), allocatable :: tmp(:)
  integer :: n

  allocate(tmp, SOURCE=a)
  call qsort(tmp)
  n = size(a)
  if (mod(n, 2) == 0) then
    out = 0.5 * (tmp(n / 2) + tmp(n / 2 + 1))
  else
    out = tmp((n + 1) / 2)
  end if

end function median_i32


function median_i64 (a) result (out)
  use xslib_sort, only: qsort
  implicit none
  real(REAL64) :: out
  integer(INT64), intent(in) :: a(:)
  integer(INT64), allocatable :: tmp(:)
  integer :: n

  allocate(tmp, SOURCE=a)
  call qsort(tmp)
  n = size(a)
  if (mod(n, 2) == 0) then
    out = 0.5d0 * (tmp(n / 2) + tmp(n / 2 + 1))
  else
    out = tmp((n + 1) / 2)
  end if

end function median_i64


function median_r32 (a) result (out)
  use xslib_sort, only: qsort
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a(:)
  real(REAL32), allocatable :: tmp(:)
  integer :: n

  allocate(tmp, SOURCE=a)
  call qsort(tmp)
  n = size(a)
  if (mod(n, 2) == 0) then
    out = 0.5 * (tmp(n / 2) + tmp(n / 2 + 1))
  else
    out = tmp((n + 1) / 2)
  end if

end function median_r32


function median_r64 (a) result (out)
  use xslib_sort, only: qsort
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a(:)
  real(REAL64), allocatable :: tmp(:)
  integer :: n

  allocate(tmp, SOURCE=a)
  call qsort(tmp)
  n = size(a)
  if (mod(n, 2) == 0) then
    out = 0.5d0 * (tmp(n / 2) + tmp(n / 2 + 1))
  else
    out = tmp((n + 1) / 2)
  end if

end function median_r64


subroutine welford_i32 (array, mean, variance, n)
  implicit none
  integer(INT32), intent(in) :: array(:)
  !! Input array.
  real(REAL32), intent(inout) :: mean(size(array))
  !! Mean values of array. Must be same size as `array`.
  real(REAL32), intent(inout) :: variance(size(array))
  !! Variance of array values. Must be same size as `array`.
  integer, intent(in) :: n
  !! Sequential number of an array. Must be non-zero.
  real(REAL32) :: delta
  integer :: i

  do i = 1, size(array)
    delta = array(i) - mean(i)
    mean(i) = mean(i) + delta / n
    variance(i) = variance(i) + delta * (array(i) - mean(i))
  end do

end subroutine welford_i32


subroutine welford_i64 (array, mean, variance, n)
  implicit none
  integer(INT64), intent(in) :: array(:)
  real(REAL64), intent(inout) :: mean(size(array)), variance(size(array))
  integer, intent(in) :: n
  real(REAL64) :: delta
  integer :: i

  do i = 1, size(array)
    delta = array(i) - mean(i)
    mean(i) = mean(i) + delta / n
    variance(i) = variance(i) + delta * (array(i) - mean(i))
  end do

end subroutine welford_i64


subroutine welford_r32 (array, mean, variance, n)
  implicit none
  real(REAL32), intent(in) :: array(:)
  real(REAL32), intent(inout) :: mean(size(array)), variance(size(array))
  integer, intent(in) :: n
  real(REAL32) :: delta
  integer :: i

  do i = 1, size(array)
    delta = array(i) - mean(i)
    mean(i) = mean(i) + delta / n
    variance(i) = variance(i) + delta * (array(i) - mean(i))
  end do

end subroutine welford_r32


subroutine welford_r64 (array, mean, variance, n)
  implicit none
  real(REAL64), intent(in) :: array(:)
  real(REAL64), intent(inout) :: mean(size(array)), variance(size(array))
  integer, intent(in) :: n
  real(REAL64) :: delta
  integer :: i

  do i = 1, size(array)
    delta = array(i) - mean(i)
    mean(i) = mean(i) + delta / n
    variance(i) = variance(i) + delta * (array(i) - mean(i))
  end do

end subroutine welford_r64


subroutine welford_finalize_r32 (mean, variance, n)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL32), intent(inout) :: mean(:)
  !! Corrected mean values of array. Must be same size as `variance`.
  real(REAL32), intent(inout) :: variance(size(mean))
  !! Corrected variance of array values. Must be same size as `mean`.
  integer, intent(in) :: n
  !! Total number of averaged arrays. Must be non-zero.

  mean = mean
  variance = merge(variance / n, ieee_value(1.0, IEEE_QUIET_NAN), n > 1)
  
end subroutine welford_finalize_r32


subroutine welford_finalize_r64 (mean, variance, n)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL64), intent(inout) :: mean(:)
  real(REAL64), intent(inout) :: variance(size(mean))
  integer, intent(in) :: n

  mean = mean
  variance = merge(variance / n, ieee_value(1.0d0, IEEE_QUIET_NAN), n > 1)
  
end subroutine welford_finalize_r64


function histogram_i32 (array, nbins, min, max) result (out)
  implicit none
  integer(INT32), intent(in) :: array(:)
  !! Input array
  integer, intent(in) :: nbins
  !! Number of equal-width bins in the given range.
  real, intent(in), optional :: min
  !! The lower range of histogram bins. Default is `min(array)`.
  real, intent(in), optional :: max
  !! The upper range of histogram bins. Default is `max(array)`.
  integer :: out(nbins)
  !! The values of the histogram.
  real :: binsize, vmin, vmax
  integer :: i, bin

  if (nbins < 2) then
    out = size(array)
  else
    out = 0
    vmin = merge(min, real(minval(array)), present(min))
    vmax = merge(max, real(maxval(array)), present(max))
    binsize = abs(vmax - vmin) / (nbins - 1)
    do i = 1, size(array)
      bin = nint((real(array(i)) - vmin) / binsize) + 1
      if (bin < 1 .or. bin > nbins) cycle
      out(bin) = out(bin) + 1
    end do
  end if

end function histogram_i32


function histogram_i64 (array, nbins, min, max) result (out)
  implicit none
  integer(INT64), intent(in) :: array(:)
  integer, intent(in) :: nbins
  real, intent(in), optional :: min, max
  integer :: out(nbins)
  real :: binsize, vmin, vmax
  integer :: i, bin

  if (nbins < 2) then
    out = size(array)
  else
    out = 0
    vmin = merge(min, real(minval(array)), present(min))
    vmax = merge(max, real(maxval(array)), present(max))
    binsize = abs(vmax - vmin) / (nbins - 1)
    do i = 1, size(array)
      bin = nint((real(array(i)) - vmin) / binsize) + 1
      if (bin < 1 .or. bin > nbins) cycle
      out(bin) = out(bin) + 1
    end do
  end if

end function histogram_i64


function histogram_r32 (array, nbins, min, max) result (out)
  implicit none
  real(REAL32), intent(in) :: array(:)
  integer, intent(in) :: nbins
  real, intent(in), optional :: min, max
  integer :: out(nbins)
  real :: binsize, vmin, vmax
  integer :: i, bin

  if (nbins < 2) then
    out = size(array)
  else
    out = 0
    vmin = merge(min, real(minval(array)), present(min))
    vmax = merge(max, real(maxval(array)), present(max))
    binsize = abs(vmax - vmin) / (nbins - 1)
    do i = 1, size(array)
      bin = nint((array(i) - vmin) / binsize) + 1
      if (bin < 1 .or. bin > nbins) cycle
      out(bin) = out(bin) + 1
    end do
  end if

end function histogram_r32


function histogram_r64 (array, nbins, min, max) result (out)
  implicit none
  real(REAL64), intent(in) :: array(:)
  integer, intent(in) :: nbins
  real, intent(in), optional :: min, max
  integer :: out(nbins)
  real :: binsize, vmin, vmax
  integer :: i, bin

  if (nbins < 2) then
    out = size(array)
  else
    out = 0
    vmin = merge(min, real(minval(array)), present(min))
    vmax = merge(max, real(maxval(array)), present(max))
    binsize = abs(vmax - vmin) / (nbins - 1)
    do i = 1, size(array)
      bin = nint((array(i) - vmin) / binsize) + 1
      if (bin < 1 .or. bin > nbins) cycle
      out(bin) = out(bin) + 1
    end do
  end if

end function histogram_r64

end module xslib_stats 
