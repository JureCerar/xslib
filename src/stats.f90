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

module xslib_stats
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: normal, mean, stdev, variance, welford, welford_finalize, histogram

  ! %%%
  ! # `STAT` - Basic statistics functions
  !   Module `xslib_stats` contains basic statistics functions. Supports both single and double precision (`DP`).
  ! %%%

  interface normal
    module procedure :: normal_real32, normal_real64
  end interface normal

  interface mean 
    module procedure :: mean_int32, mean_int64, mean_real32, mean_real64
  end interface mean

  interface stdev
    module procedure :: stdev_int32, stdev_int64, stdev_real32, stdev_real64
  end interface stdev

  interface variance
    module procedure :: variance_int32, variance_int64, variance_real32, variance_real64
  end interface variance

  interface welford
    module procedure :: welford_int32, welford_int64, welford_real32, welford_real64
  end interface welford

  interface welford_finalize
    module procedure :: welford_finalize_real32, welford_finalize_real64
  end interface welford_finalize

  interface histogram
    module procedure :: histogram_int32, histogram_int64, histogram_real32, histogram_real64
  end interface histogram

contains

function normal_real32 (mu, sigma) result (out)
  ! %%%
  ! ## `NORMAL` - Random number on a normal distribution
  ! #### DESCRIPTION
  !   Return random samples from a normal (Gaussian) distribution. Random number sequence 
  !   can be initialized with [`srand`](https://gcc.gnu.org/onlinedocs/gfortran/SRAND.html) function.
  ! #### USAGE
  !   ```Fortran
  !   out = normal(mu, sigma)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), intent(IN) :: mu, sigma`
  !     The mean `mu` and variance `sigma` values of normal distribution.  
  !   * `real(ANY) :: out`
  !     Random value on a normal distribution.  
  ! #### EXAMPLE
  !   ```Fortran
  !   > normal(0.0, 1.0)
  !   0.123456
  !   ```
  ! %%%
  ! See: https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
  implicit none
  real, parameter :: PI = acos(-1.0)
  real(REAL32), intent(in) :: mu, sigma
  real(REAL32) :: out

  out = mu + sigma * sqrt(-2 * log(rand())) * cos(2 * PI * rand())

end function normal_real32


function normal_real64 (mu, sigma) result (out)
  implicit none
  real, parameter :: PI = acos(-1.0)
  real(REAL64), intent(in) :: mu, sigma
  real(REAL64) :: out

  out = mu + sigma * sqrt(-2 * log(rand())) * cos(2 * PI * rand())

end function normal_real64


function mean_int32 (array) result (out)
  ! %%%
  ! ## `MEAN` - Arithmetic mean
  ! #### DESCRIPTION
  !   Return mean (average) value of an array.
  ! #### USAGE
  !   ```Fortran
  !   out = mean(array)
  !   ```
  ! #### PARAMETERS
  !   * `class(*) dimension(:), intent(IN) :: array`
  !     Input array of `REAL` or `INT` kind.
  !   * `real(ANY) :: out`
  !     Average value of array. 
  ! #### EXAMPLE
  !   ```Fortran
  !   > mean([1, 2, 3, 4, 5])
  !   3.0
  !   ```
  ! %%%
  implicit none
  integer(INT32), intent(in) :: array(:)
  real(REAL32) :: out

  out = sum(real(array, KIND=REAL32)) / size(array)

end function mean_int32


function mean_int64 (array) result (out)
  implicit none
  integer(INT64), intent(in) :: array(:)
  real(REAL64) :: out

  out = sum(real(array, KIND=REAL64)) / size(array)

end function mean_int64


function mean_real32 (array) result (out)
  implicit none
  real(REAL32), intent(in) :: array(:)
  real(REAL32) :: out

  out = sum(array) / size(array)

end function mean_real32


function mean_real64 (array) result (out)
  implicit none
  real(REAL64), intent(in) :: array(:)
  real(REAL64) :: out

  out = sum(array) / size(array)

end function mean_real64


function stdev_int32 (array) result (out)
  ! %%%
  ! ## `STDEV` - Get standard deviation
  ! #### DESCRIPTION
  !   Return standard deviation of an array.
  ! #### USAGE
  !   ```Fortran
  !   out = stdev(array)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), dimension(:), intent(IN) :: array`
  !     Input array of `REAL` or `INT` kind.
  !   * `real(ANY) :: out`
  !     Standard deviation of array values. 
  ! #### EXAMPLE
  !   ```Fortran
  !   > stdev([1, 2, 3, 4, 5])
  !   1.58114
  !   ```
  ! %%%
  implicit none
  integer(INT32), intent(in) :: array(:)
  real(REAL32) :: out, ave

  ave = sum(real(array, REAL32)) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stdev_int32


function stdev_int64 (array) result (out)
  implicit none
  integer(INT64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(real(array, REAL64)) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stdev_int64


function stdev_real32 (array) result (out)
  implicit none
  real(REAL32), intent(in) :: array(:)
  real(REAL32) :: out, ave

  ave = sum(array) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stdev_real32


function stdev_real64 (array) result (out)
  implicit none
  real(REAL64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(array) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stdev_real64


function variance_int32 (array) result (out)
  ! %%%
  ! ## `VARIANCE` - Get variance of an array
  ! #### DESCRIPTION
  !   Calculate variance of an array.
  ! #### USAGE
  !   ```Fortran
  !   out = variance(array)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), dimension(:), intent(IN) :: array`
  !     Input array of kind `INT` or `REAL`.
  !   * `real(ANY) :: out`
  !     Variance of array values. 
  ! #### EXAMPLE
  !   ```Fortran
  !   > variance([1, 2, 3, 4, 5])
  !   2.5
  !   ```
  ! %%%
  implicit none
  integer(INT32), intent(in) :: array(:)
  real(REAL32) :: out, ave

  ave = sum(real(array, REAL32)) / size(array)
  out = sum((array - ave) ** 2) / size(array)

end function variance_int32


function variance_int64 (array) result (out)
  implicit none
  integer(INT64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(real(array, REAL64)) / size(array)
  out = sum((array - ave) ** 2) / size(array)

end function variance_int64


function variance_real32 (array) result (out)
  implicit none
  real(REAL32), intent(in) :: array(:)
  real(REAL32) :: out, ave

  ave = sum(array) / size(array)
  out = sum((array - ave) ** 2) / size(array)

end function variance_real32


function variance_real64 (array) result (out)
  implicit none
  real(REAL64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(array) / size(array)
  out = sum((array - ave) ** 2) / size(array)

end function variance_real64


subroutine welford_int32 (array, mean, variance, n)
  ! %%%
  ! ## `WELFORD` - Welford's online variance
  ! #### DESCRIPTION
  !   [Welford's online algorithm](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm) 
  !   for calculating variance. Final variance must be "corrected" with `welford_finalize` call.
  ! #### USAGE
  !   ```Fortran
  !   call welford(array, mean, variance, n)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), dimension(:), intent(IN) :: array`
  !     Input array of kind `INT` or `REAL`.
  !   * `real(ANY), dimension(:), intent(INOUT) :: mean, variance`
  !     Mean value and variance of array values. Must be same size as `array`.
  !   * `integer, intent(IN) :: n`
  !     Sequential number of an array. Must be non-zero.
  ! #### EXAMPLE
  !   ```Fortran
  !   > do i = 1, NP
  !   >   call random_number(array)
  !   >   call welford(array, mean, variance, i)
  !   > end do
  !   > call welford_finalize(mean, variance, NP)
  !   ```
  ! %%%
  implicit none
  integer(INT32), intent(in) :: array(:)
  real(REAL32), intent(inout) :: mean(size(array)), variance(size(array))
  integer, intent(in) :: n
  real(REAL32) :: delta
  integer :: i

  do i = 1, size(array)
    delta = array(i) - mean(i)
    mean(i) = mean(i) + delta / n
    variance(i) = variance(i) + delta * (array(i) - mean(i))
  end do

end subroutine welford_int32


subroutine welford_int64 (array, mean, variance, n)
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

end subroutine welford_int64


subroutine welford_real32 (array, mean, variance, n)
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

end subroutine welford_real32


subroutine welford_real64 (array, mean, variance, n)
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

end subroutine welford_real64


subroutine welford_finalize_real32 (mean, variance, n)
  ! %%%
  ! ## `WELFORD_FINALIZE` - Correct Welford's online variance
  ! #### DESCRIPTION
  !   Correct final variance of Welford's online algorithm.
  ! #### USAGE
  !   ```Fortran
  !   call welford_finalize(mean, variance, n)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(:), intent(INOUT) :: mean, variance`
  !     Mean value and variance of arrays. Must be same size.
  !   * `integer, intent(IN) :: n`
  !     Total number of averaged arrays. Must be non-zero.
  ! #### EXAMPLE
  !   ```Fortran
  !   > do i = 1, NP
  !   >   call random_number(array)
  !   >   call welford(array, mean, variance, i)
  !   > end do
  !   > call welford_finalize(mean, variance, NP)
  !   ```
  ! %%%
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL32), intent(inout) :: mean(:)
  real(REAL32), intent(inout) :: variance(size(mean))
  integer, intent(in) :: n

  mean = mean
  variance = merge(variance / n, ieee_value(1.0, IEEE_QUIET_NAN), n > 1)
  
end subroutine welford_finalize_real32


subroutine welford_finalize_real64 (mean, variance, n)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL64), intent(inout) :: mean(:)
  real(REAL64), intent(inout) :: variance(size(mean))
  integer, intent(in) :: n

  mean = mean
  variance = merge(variance / n, ieee_value(1.0d0, IEEE_QUIET_NAN), n > 1)
  
end subroutine welford_finalize_real64


function histogram_int32 (array, nbins, min, max) result (out)
  ! %%%
  ! ## `HISTOGRAM` - Get histogram of an array.
  ! #### DESCRIPTION
  !   Calculate histogram distribution of an array.
  ! #### USAGE
  !   ```Fortran
  !   out = histogram(array, nbins, min=min, max=max)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), dimension(:), intent(IN) :: array`
  !     Input array of kind `INT` or `REAL`.
  !   * `integer, intent(IN) :: nbins`
  !     Number of equal-width bins in the given range.
  !   * `real, intent(IN), OPTIONAL :: min, max`
  !     The lower and upper range of the bins. If not provided, ranges are
  !     simply: `min(array)` and `max(array)`, respectively.
  !   * `integer, dimension(nbins) :: out`
  !     The values of the histogram.
  ! #### EXAMPLE
  !   ```Fortran
  !   > call random_number(array)
  !   > histogram(array, 5, MIN=0.0, MAX=1.0)
  !   [9, 10, 8, 11, 11]
  !   ```
  ! %%%
  implicit none
  integer(INT32), intent(in) :: array(:)
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

end function histogram_int32


function histogram_int64 (array, nbins, min, max) result (out)
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

end function histogram_int64


function histogram_real32 (array, nbins, min, max) result (out)
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

end function histogram_real32


function histogram_real64 (array, nbins, min, max) result (out)
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

end function histogram_real64

end module xslib_stats 
