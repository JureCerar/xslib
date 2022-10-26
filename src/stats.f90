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
  public :: normal, average, stddev, variance, welford, welford_finalize, histogram

  interface normal
    module procedure :: normal_real32, normal_real64
  end interface normal

  interface average 
    module procedure :: ave_int32, ave_int64, ave_real32, ave_real64
  end interface average

  interface stddev
    module procedure :: stddev_int32, stddev_int64, stddev_real32, stddev_real64
  end interface stddev

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

! Return random samples from a normal (Gaussian) distribution.
! See: https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
function normal_real32 (mu, sigma) result (out)
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

! Calculate average value of an array.
function ave_int32 (array) result (out)
  implicit none
  integer(INT32), intent(in) :: array(:)
  real(REAL32) :: out

  out = sum(real(array, KIND=REAL32)) / size(array)

end function ave_int32

function ave_int64 (array) result (out)
  implicit none
  integer(INT64), intent(in) :: array(:)
  real(REAL64) :: out

  out = sum(real(array, KIND=REAL64)) / size(array)

end function ave_int64

function ave_real32 (array) result (out)
  implicit none
  real(REAL32), intent(in) :: array(:)
  real(REAL32) :: out

  out = sum(array) / size(array)

end function ave_real32

function ave_real64 (array) result (out)
  implicit none
  real(REAL64), intent(in) :: array(:)
  real(REAL64) :: out

  out = sum(array) / size(array)

end function ave_real64

! Calculate standard deviation of an array.
function stddev_int32 (array) result (out)
  implicit none
  integer(INT32), intent(in) :: array(:)
  real(REAL32) :: out, ave

  ave = sum(real(array, REAL32)) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stddev_int32

function stddev_int64 (array) result (out)
  implicit none
  integer(INT64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(real(array, REAL64)) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stddev_int64

function stddev_real32 (array) result (out)
  implicit none
  real(REAL32), intent(in) :: array(:)
  real(REAL32) :: out, ave

  ave = sum(array) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stddev_real32

function stddev_real64 (array) result (out)
  implicit none
  real(REAL64), intent(in) :: array(:)
  real(REAL64) :: out, ave

  ave = sum(array) / size(array)
  out = sqrt(sum((array - ave) ** 2) / size(array))

end function stddev_real64

! Calculate variance of an array.
function variance_int32 (array) result (out)
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

! Welford's online algorithm for calculating variance. Final variance must be "corrected" with `welford_finalize()` call. 
! See: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm
subroutine welford_int32 (array, mean, variance, n)
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

! Correct final variance of Welford's online algorithm.
subroutine welford_finalize_real32 (mean, variance, n)
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

! Compute the histogram of an array.
function histogram_int32 (array, nbins, min, max) result (out)
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
