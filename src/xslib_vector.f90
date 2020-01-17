! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2020  Jure Cerar
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

module xslib_vector
  use, intrinsic :: iso_fortran_env, only: REAL32
  implicit none
  private
  public :: cross, rotate, deg2rad, rad2deg, crt2sph, sph2crt, crt2cyl, cyl2crt, minImg, &
  &  getDistance, getAngle, getDihedral, variance, lerp, findKClosest, findCrossOver, swap

  ! Default precision
  integer, parameter :: dp = REAL32

  ! Generic on-line variance function
  interface variance
    module procedure :: variance_float, variance_float8, variance_array, variance_array8
  end interface variance

  ! Generic linear interpolation
  interface lerp
    module procedure :: lerp_float, lerp_float8
  end interface lerp

  ! Generic find cross-over point.
  interface findCrossOver
    module procedure :: findCrossOver_int, findCrossOver_int8, findCrossOver_float, findCrossOver_float8
  end interface

  ! Generic find k closest neighbours.
  interface findKClosest
    module procedure :: findKClosest_int, findKClosest_int8, findKClosest_float, findKClosest_float8
  end interface

  ! Generic swap function.
  interface swap
    module procedure swap_int, swap_int8, swap_float, swap_float8, swap_char
  end interface swap

contains

! -------------------------------------------------

! Calculates vector product: v x u
real(dp) function cross( v, u )
  implicit none
  dimension                           :: cross(3)
  real(dp), dimension(3), intent(in)  :: v, u
  cross(1) = v(2)*u(3)-v(3)*u(2)
  cross(2) = v(3)*u(1)-v(1)*u(3)
  cross(3) = v(1)*u(2)-v(2)*u(1)
  return
end function cross

! Rotate <x> around <vector> by <angle> rad.
real(dp) function rotate( x, vector, angle )
  implicit none
  dimension            :: rotate(3)
  real(dp), intent(in) :: x(3), vector(3), angle
  real(dp)             :: u(3)
  ! SOURCE:
  ! https://en.wikipedia.org/wiki/Rotation_matrix
  u(:) = vector/norm2(vector)
  rotate(:) = u*dot_product(u,x) + cos(angle)*cross(u,x)+sin(angle)*cross(u,x)
  return
end function rotate

! Rotate <x> around <axis> by <angle> rad. Returns NaN if invalid axis-name.
real(dp) function rotate_axis( x, axis, angle )
  use, intrinsic :: ieee_arithmetic
  implicit none
  dimension                 :: rotate_axis(3)
  real(dp), intent(in)      :: x(3)
  character(*), intent(in)  :: axis
  real(dp), intent(in)      :: angle
  real(dp)                  :: R(3,3) ! rotation matrix
  ! SOURCE:
  ! https://en.wikipedia.org/wiki/Rotation_matrix
  select case (trim(axis))
  case( "x", "X" )
    R(:,:) = reshape(    [1.0_dp,      0.0_dp,      0.0_dp, &
    &                     0.0_dp,  cos(angle), -sin(angle), &
    &                     0.0_dp,  sin(angle),  cos(angle)], shape(R) )

  case( "y", "Y" )
    R(:,:) = reshape([cos(angle),      0.0_dp,  sin(angle),   &
    &                     0.0_dp,      1.0_dp,      0.0_dp,   &
    &                -sin(angle),      0.0_dp,  cos(angle)], shape(R) )

  case( "z", "Z" )
    R(:,:) = reshape([cos(angle), -sin(angle),      0.0_dp,   &
    &                 sin(angle),  cos(angle),      0.0_dp,   &
    &                     0.0_dp,      0.0_dp,      1.0_dp], shape(R) )
  case default
    ! Return NaN an exit
    rotate_axis(:) = ieee_value(x, ieee_quiet_nan)
    return
  end select
  rotate_axis(:) = matmul(R,x)
  return
end function rotate_axis

! -------------------------------------------------
! Unit transformations

! Transform degrees to radians
real(dp) function deg2rad( x )
  implicit none
  real(dp), intent(in)  :: x
  ! rad = deg/180*pi
  deg2rad = x/180.0_dp*acos(-1.0_dp)
  return
end function deg2rad

! Transform rad to degrees
real(dp) function rad2deg( x )
  implicit none
  real(dp), intent(in)  :: x
  ! deg = rad/pi*180
  rad2deg = x/acos(-1.0_dp)*180.0_dp
  return
end function rad2deg

! -------------------------------------------------
! Coordinate system transformations

! Cartesian to spherical: (x,y,z) -> (r,theta,phi)
real(dp) function crt2sph( crt )
  implicit none
  dimension             :: crt2sph(3)
  real(dp), intent(in)  :: crt(3)
  ! r = sqrt(x**2+y**2+z**2)
  ! theta = acos(z/r)
  ! phi = atan2(y/x)
  crt2sph(1) = norm2( crt(:) )
  crt2sph(2) = merge( 0.0_dp, acos(crt(3)/crt2sph(1)), crt2sph(1) /= 0.0_dp )
  crt2sph(3) = merge( 0.0_dp, atan2(crt(2),crt(1)), crt(1) /= 0.0_dp )
  return
end function crt2sph

! Spherical to cartesian: (r,theta,phi) -> (x,y,z)
real(dp) function sph2crt( sph )
  implicit none
  dimension             :: sph2crt(3)
  real(dp), intent(in)  :: sph(3)
  ! x = r*sin(theta)*cos(phi)
  ! y = r*sin(theta)*sin(phi)
  ! z = r*cos(theta)
  sph2crt(1) = sph(1)*sin(sph(2))*cos(sph(3))
  sph2crt(2) = sph(1)*sin(sph(2))*sin(sph(3))
  sph2crt(3) = sph(1)*cos(sph(2))
  return
end function sph2crt

! Cartesian to cylindrical: (x,y,z) -> (r,theta,z)
real(dp) function crt2cyl( crt )
  implicit none
  dimension             :: crt2cyl(3)
  real(dp), intent(in)  :: crt(3)
  ! r = sqrt(x**2+y**2)
  ! theta =  atan2(y/x)
  ! z = z
  crt2cyl(1) = sqrt(crt(1)**2+crt(2)**2)
  crt2cyl(2) = merge( atan2(crt(2),crt(1)), 0.0_dp, crt2cyl(1) /= 0.0_dp )
  crt2cyl(3) = crt(3)
  return
end function crt2cyl

! Cylindrical to cartesian: (r,theta,z) -> (x,y,z)
real(dp) function cyl2crt( cyl )
  implicit none
  dimension             :: cyl2crt(3)
  real(dp), intent(in)  :: cyl(3)
  ! x = r*cos(theta)
  ! y = r*sin(theta)
  ! z = z
  cyl2crt(1) = cyl(1)*cos(cyl(2))
  cyl2crt(2) = cyl(1)*sin(cyl(2))
  cyl2crt(3) = cyl(3)
  return
end function cyl2crt

! -------------------------------------------------
! Vector coordinates functions

! Reduces distance acording to minimal image convention
real(dp) function minImg( r, box )
  implicit none
  dimension                           :: minImg(3)
  real(dp), dimension(3), intent(in)  :: r, box
  minimg(:) = r(:)-box(:)*floor(r(:)/box(:)+0.5)
  ! hbox=box/2
  ! minimg(:) = r(:)-sign(hbox,r-hbox)-sign(hbox,r+hbox)
  return
end function minImg

! Calculates distance between two points.
real(dp) function getDistance( a, b )
  implicit none
  real(dp), dimension(3), intent(in)  :: a, b
  getDistance = norm2(a-b)
  ! getDistance(:) = sqrt(dot_product(a(:),b(:)))
end function getDistance

! Calculates angle between three points.
real(dp) function getAngle( a, b, c )
  implicit none
  real(dp), dimension(3), intent(in)  :: a, b, c
  real(dp), dimension(3)              :: u, v
  u(:) = a-b
  v(:) = c-b
  getAngle = atan2(norm2(cross(u,v)),dot_product(u,v))
  return
end function getAngle

! Get dihedral angle (theta) between four points
real(dp) function getDihedral( a, b, c, d )
  implicit none
  real(dp), dimension(3), intent(in) :: a, b, c, d
  real(dp), dimension(3)             :: b1, b2, b3, n1, n2, n3
  ! SOURCE:
  ! https://math.stackexchange.com/questions/47059/how-do-i-calculate-a-dihedral-angle-given-cartesian-coordinates
  ! Site-to-Site vectors
  b1(:) = b-a
  b2(:) = c-a
  b3(:) = d-a
  ! Vector normals
  n1(:) = cross(b1,b2)
  n1(:) = n1/norm2(n1)
  n2(:) = cross(b2,b3)
  n2(:) = n2/norm2(n2)
  n3(:) = cross(n1,b2/norm2(b2))
  ! Angle
  getDihedral = atan2( dot_product(n3,n2), dot_product(n1,n2) )
  return
end function getDihedral

! -------------------------------------------------
! On-line variance algorithm

! On-line variance calculation for sigle real value.
subroutine variance_float( val, mean, var, n )
  implicit none
  real, intent(in)    :: val
  real, intent(inout) :: mean, var
  integer, intent(in) :: n
  real                :: delta
  delta = val-mean
  mean = mean+delta/real(n)
  var = var+delta*(val-mean)
  return
end subroutine variance_float

! On-line variance calculation for real array.
subroutine variance_array( val, mean, var, n )
  implicit none
  real, intent(in)    :: val(:)
  real, intent(inout) :: mean(:), var(:)
  integer, intent(in) :: n
  integer             :: i
  do i = 1, size(val)
    call variance_float( val(i), mean(i), var(i), n )
  end do
  return
end subroutine variance_array

! On-line variance calculation for sigle real*8 value.
subroutine variance_float8( val, mean, var, n )
  use iso_fortran_env
  implicit none
  real(REAL64), intent(in)    :: val
  real(REAL64), intent(inout) :: mean, var
  integer, intent(in)         :: n
  real(REAL64)                :: delta
  delta = val-mean
  mean = mean+delta/real(n,REAL64)
  var = var+delta*(val-mean)
  return
end subroutine variance_float8

! On-line variance calculation for real*8 array.
subroutine variance_array8( val, mean, var, n )
  use iso_fortran_env
  implicit none
  real(REAL64), intent(in)    :: val(:)
  real(REAL64), intent(inout) :: mean(:), var(:)
  integer, intent(in)         :: n
  integer                     :: i
  do i = 1, size(val)
    call variance_float8(val(i), mean(i), var(i), n)
  end do
  return
end subroutine variance_array8

! -------------------------------------------------
! Linear interpolation: LERP

real function lerp_float( v0, v1, x )
  implicit none
  real, intent(in) :: v0, v1, x
  lerp_float = v0 + x * (v0-v1)
  return
end function lerp_float

real(REAL64) function lerp_float8( v0, v1, x )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(in) :: v0, v1, x
  lerp_float8 = v0 + x * (v0-v1)
  return
end function lerp_float8

! -------------------------------------------------
! On-line variance algorithm

! This function return K closest elements to VAL in ARRAY(:).
function findKClosest_int( val, array, k ) result( r )
  implicit none
  integer, intent(in) :: val, array(:)
  integer, intent(in) :: k
  integer             :: r(k)
  integer             :: n, right, left, cnt
  ! Find nearest point
  n = size(array)
  ! Error check
  if (k > n) return
  ! Find nearest point to the left
  left = findCrossOver( array(:), 1, n, val )
  right = left+1
  cnt = 1
  ! Check left and right from the VAL to find next nearest number
  do while (left >= 1 .and. right <= n .and. cnt <= k)
    ! Left side is closer to VAL
    if (val-array(left) < array(right)-val) then
      r(cnt) = left
      left = left-1
    ! Right side is closer to VAL
    else
      r(cnt) = right
      right = right+1
    end if
    cnt = cnt+1

  end do
  ! All remaining values are to the left
  do while (left >= 1 .and. cnt <= k)
    r(cnt) = left
    left = left-1
    cnt = cnt+1

  end do

  ! All remaining values are to the left
  do while (right <= n .and. cnt <= k)
    r(cnt) = right
    right = right+1
    cnt = cnt+1

  end do

  return
end function findKClosest_int

function findKClosest_int8( val, array, k ) result( r )
  use iso_fortran_env
  implicit none
  integer(INT64), intent(in)  :: val, array(:)
  integer(INT64), intent(in)  :: k
  integer(INT64)              :: r(k)
  integer                     :: n, right, left, cnt
  ! Find nearest point
  n = size(array)
  ! Error check
  if (k > n) return
  ! Find nearest point to the left
  left = findCrossOver( array(:), 1, n, val )
  right = left+1
  cnt = 1
  ! Check left and right from the VAL to find next nearest number
  do while (left >= 1 .and. right <= n .and. cnt <= k)
    ! Left side is closer to VAL
    if (val-array(left) < array(right)-val) then
      r(cnt) = left
      left = left-1
    ! Right side is closer to VAL
    else
      r(cnt) = right
      right = right+1
    end if
    cnt = cnt+1

  end do
  ! All remaining values are to the left
  do while (left >= 1 .and. cnt <= k)
    r(cnt) = left
    left = left-1
    cnt = cnt+1

  end do

  ! All remaining values are to the left
  do while (right <= n .and. cnt <= k)
    r(cnt) = right
    right = right+1
    cnt = cnt+1

  end do

  return
end function findKClosest_int8

function findKClosest_float( val, array, k ) result( r )
  implicit none
  real, intent(in)    :: val, array(:)
  integer, intent(in) :: k
  real                :: r(k)
  integer             :: n, right, left, cnt
  ! Find nearest point
  n = size(array)
  ! Error check
  if (k > n) return
  ! Find nearest point to the left
  left = findCrossOver (array(:), 1, n, val)
  right = left+1
  cnt = 1
  ! Check left and right from the VAL to find next nearest number
  do while (left >= 1 .and. right <= n .and. cnt <= k)
    ! Left side is closer to VAL
    if (val-array(left) < array(right)-val) then
      r(cnt) = left
      left = left-1
    ! Right side is closer to VAL
    else
      r(cnt) = right
      right = right+1
    end if
    cnt = cnt+1

  end do
  ! All remaining values are to the left
  do while (left >= 1 .and. cnt <= k)
    r(cnt) = left
    left = left-1
    cnt = cnt+1

  end do
  ! All remaining values are to the left
  do while (right <= n .and. cnt <= k)
    r(cnt) = right
    right = right+1
    cnt = cnt+1

  end do

  return
end function findKClosest_float

function findKClosest_float8( val, array, k ) result( r )
  use iso_fortran_env
  implicit none
  real(REAL64), intent(in)  :: val, array(:)
  integer, intent(in)       :: k
  real                      :: r(k)
  integer                   :: n, right, left, cnt
  ! Find nearest point
  n = size(array)
  ! Error check
  if (k > n) return
  ! Find nearest point to the left
  left = findCrossOver (array(:), 1, n, val)
  right = left+1
  cnt = 1
  ! Check left and right from the VAL to find next nearest number
  do while (left >= 1 .and. right <= n .and. cnt <= k)
    ! Left side is closer to VAL
    if (val-array(left) < array(right)-val) then
      r(cnt) = left
      left = left-1
    ! Right side is closer to VAL
    else
      r(cnt) = right
      right = right+1
    end if
    cnt = cnt+1

  end do
  ! All remaining values are to the left
  do while (left >= 1 .and. cnt <= k)
    r(cnt) = left
    left = left-1
    cnt = cnt+1

  end do
  ! All remaining values are to the left
  do while (right <= n .and. cnt <= k)
    r(cnt) = right
    right = right+1
    cnt = cnt+1

  end do

  return
end function findKClosest_float8

! Function to find the cross over point i.e. the point at
! which elements are smaller than or equal to x and after which are greater than x
recursive function findCrossOver_int( array, low, high, x ) result( r )
  implicit none
  integer, intent(in) :: array(:), x
  integer, intent(in) :: low, high
  integer             :: mid, r

  ! Bigger than all
  if (x >= array(high)) then
    r = high

  ! Smaller than all
  else if (x < array(low)) then
    r = low

  else
    mid = (low+high)/2

    if (x <= array(mid) .and. x > array(mid+1)) then
      r = mid

    else if (x > array(mid)) then
      r = findCrossOver_int(array(:), mid+1, high, x)

    else !(x < array(mid))
      r = findCrossOver_int(array(:), low, mid, x)

    end if
  end if

  return
end function findCrossOver_int

recursive function findCrossOver_int8( array, low, high, x ) result( r )
  use iso_fortran_env
  implicit none
  integer(INT64), intent(in) :: array(:), x
  integer, intent(in)        :: low, high
  integer                    :: mid, r

  ! Bigger than all
  if (x >= array(high)) then
    r = high

  ! Smaller than all
  else if (x < array(low)) then
    r = low

  else
    mid = (low+high)/2

    if (x <= array(mid) .and. x > array(mid+1)) then
      r = mid

    else if (x > array(mid)) then
      r = findCrossOver_int8(array(:), mid+1, high, x)

    else !(x < array(mid))
      r = findCrossOver_int8(array(:), low, mid, x)

    end if
  end if

  return
end function findCrossOver_int8

recursive function findCrossOver_float( array, low, high, x ) result( r )
  implicit none
  real, intent(in)    :: array(:), x
  integer, intent(in) :: low, high
  integer             :: mid, r
  ! Bigger than all
  if (x >= array(high)) then
    r = high
  ! Smaller than all
  else if (x < array(low)) then
    r = low
  else
    mid = (low+high)/2
    if (x <= array(mid) .and. x > array(mid+1)) then
      r = mid
    else if (x > array(mid)) then
      r = findcrossover_float(array, mid+1, high, x)
    else !(x < array(mid))
      r = findcrossover_float(array, low, mid, x)
    end if
  end if

  return
end function findCrossOver_float

recursive function findCrossOver_float8( array, low, high, x ) result( r )
  use iso_fortran_env
  implicit none
  real(REAL64), intent(in) :: array(:), x
  integer, intent(in)      :: low, high
  integer                  :: mid, r
  ! Bigger than all
  if (x >= array(high)) then
    r = high
  ! Smaller than all
  else if (x < array(low)) then
    r = low
  else
    mid = (low+high)/2
    if (x <= array(mid) .and. x > array(mid+1)) then
      r = mid
    else if (x > array(mid)) then
      r = findcrossover_float8(array, mid+1, high, x)
    else !(x < array(mid))
      r = findcrossover_float8(array, low, mid, x)
    end if
  end if

  return
end function findCrossOver_float8

! Swap values of A and B
subroutine swap_int( a, b )
  implicit none
  integer, intent(inout) :: a, b
  a = a+b
  b = a-b
  a = a-b
  return
end subroutine swap_int

subroutine swap_int8( a, b )
  use iso_fortran_env
  implicit none
  integer(INT64), intent(inout) :: a, b
  a = a+b
  b = a-b
  a = a-b
  return
end subroutine swap_int8

subroutine swap_float( a, b )
  implicit none
  real, intent(inout) :: a, b
  a = a+b
  b = a-b
  a = a-b
  return
end subroutine swap_float

subroutine swap_float8( a, b )
  use iso_fortran_env
  implicit none
  real(REAL64), intent(inout) :: a, b
  a = a+b
  b = a-b
  a = a-b
  return
end subroutine swap_float8

subroutine swap_char( a, b )
  implicit none
  character(*), intent(inout) :: a, b
  character(len(a))           :: temp
  temp = a
  a = b
  b = temp
  return
end subroutine swap_char

end module xslib_vector
