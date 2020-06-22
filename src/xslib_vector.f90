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
  implicit none
  private
  public :: cross, rotate, deg2rad, rad2deg, crt2sph, sph2crt, crt2cyl, cyl2crt, minImg, &
  &  getDistance, getAngle, getDihedral, variance, lerp, findKClosest, findCrossOver, swap

  ! Generic vector cross product
  interface cross
    module procedure :: cross_float, cross_double
  end interface cross

  ! Generic rotate vector
  interface rotate
    module procedure :: rotate_float, rotate_double, rotateAxis_float, rotateAxis_double
  end interface rotate

  ! Generic transformations
  interface rad2deg
    module procedure :: rad2deg_float, rad2deg_double
  end interface rad2deg

  interface deg2rad
    module procedure :: deg2rad_float, deg2rad_double
  end interface deg2rad

  ! Generic coordinate system transformations
  interface crt2sph
    module procedure :: crt2sph_float, crt2sph_double
  end interface crt2sph

  interface sph2crt
    module procedure :: sph2crt_float, sph2crt_double
  end interface sph2crt

  interface crt2cyl
    module procedure :: crt2cyl_float, crt2cyl_double
  end interface crt2cyl

  interface cyl2crt
    module procedure :: cyl2crt_float, cyl2crt_double
  end interface cyl2crt

  ! Generic on-line variance function
  interface variance
    module procedure :: variance_float, variance_double, variance_dfloat, variance_ddouble
  end interface variance

  ! Generic linear interpolation
  interface lerp
    module procedure :: lerp_float, lerp_double
  end interface lerp

  ! Generic find cross-over point.
  interface findCrossOver
    module procedure :: findCrossOver_int, findCrossOver_long, findCrossOver_float, findCrossOver_double
  end interface

  ! Generic find-K-closest neighbours.
  interface findKClosest
    module procedure :: findKClosest_int, findKClosest_long, findKClosest_float, findKClosest_double
  end interface

  ! Generic swap function.
  interface swap
    module procedure swap_int, swap_long, swap_float, swap_double, swap_char
  end interface swap

contains

! -------------------------------------------------

! Calculates vector product: v x u
function cross_float( v, u )
  implicit none
  real              :: cross_float(3)
  real, intent(in)  :: v(3), u(3)
  cross_float(1) = v(2)*u(3)-v(3)*u(2)
  cross_float(2) = v(3)*u(1)-v(1)*u(3)
  cross_float(3) = v(1)*u(2)-v(2)*u(1)
  return
end function cross_float

function cross_double( v, u )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64)              :: cross_double(3)
  real(REAL64), intent(in)  :: v(3), u(3)
  cross_double(1) = v(2)*u(3)-v(3)*u(2)
  cross_double(2) = v(3)*u(1)-v(1)*u(3)
  cross_double(3) = v(1)*u(2)-v(2)*u(1)
  return
end function cross_double

! Rotate vector "x" around "vector" by "angle" [rad].
function rotate_float( x, vector, angle )
  implicit none
  real              :: rotate_float(3)
  real, intent(in)  :: x(3), vector(3), angle
  real              :: k(3)
  ! SOURCE: https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  k(:) = vector/norm2(vector)
  rotate_float(:) = x*cos(angle) + cross(k,x)*sin(angle) + k*dot_product(k,x)*(1.0-cos(angle))
  return
end function rotate_float

function rotate_double( x, vector, angle )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64)              :: rotate_double(3)
  real(REAL64) , intent(in) :: x(3), vector(3), angle
  real(REAL64)              :: k(3)
  k(:) = vector/norm2(vector)
  rotate_double(:) = x*cos(angle) + cross(k,x)*sin(angle) + k*dot_product(k,x)*(1.0d0-cos(angle))
  return
end function rotate_double

! Rotate vector "x" around "axis" by "angle" [rad]. Returns NaN if invalid axis-name.
function rotateAxis_float( x, axis, angle )
  use, intrinsic :: ieee_arithmetic
  implicit none
  real                      :: rotateAxis_float(3)
  real, intent(in)          :: x(3)
  character(*), intent(in)  :: axis
  real, intent(in)          :: angle
  real                      :: R(3,3) ! rotation matrix
  ! SOURCE: https://en.wikipedia.org/wiki/Rotation_matrix
  ! NOTE: Rotation matrix is transposed compared to SOURCE,
  ! * because fortran is row-major language.
  select case (trim(axis))
  case( "x", "X" )
    R(:,:) = reshape(    [   1.0,         0.0,         0.0, &
    &                        0.0,  cos(angle),  sin(angle), &
    &                        0.0, -sin(angle),  cos(angle)], shape(R) )

  case( "y", "Y" )
    R(:,:) = reshape([cos(angle),         0.0, -sin(angle),   &
    &                        0.0,         1.0,         0.0,   &
    &                 sin(angle),         0.0,  cos(angle)], shape(R) )

  case( "z", "Z" )
    R(:,:) = reshape([cos(angle),  sin(angle),         0.0,   &
    &                -sin(angle),  cos(angle),         0.0,   &
    &                        0.0,         0.0,         1.0], shape(R) )

  case default
    ! Return NaN an exit
    rotateAxis_float(:) = ieee_value(x,ieee_quiet_nan)
    return

  end select

  rotateAxis_float(:) = matmul(R,x)

  return
end function rotateAxis_float

function rotateAxis_double( x, axis, angle )
  use, intrinsic :: iso_fortran_env, only: REAL64
  use, intrinsic :: ieee_arithmetic
  implicit none
  real(REAL64)              :: rotateAxis_double(3)
  real(REAL64), intent(in)  :: x(3)
  character(*), intent(in)  :: axis
  real(REAL64), intent(in)  :: angle
  real(REAL64)              :: R(3,3)
  select case (trim(axis))
  case( "x", "X" )
    R(:,:) = reshape(    [1.00d0,      0.00d0,      0.00d0, &
    &                     0.00d0,  cos(angle),  sin(angle), &
    &                     0.00d0, -sin(angle),  cos(angle)], shape(R) )
  case( "y", "Y" )
    R(:,:) = reshape([cos(angle),      0.00d0, -sin(angle),   &
    &                     0.00d0,      1.00d0,      0.00d0,   &
    &                 sin(angle),      0.00d0,  cos(angle)], shape(R) )
  case( "z", "Z" )
    R(:,:) = reshape([cos(angle),  sin(angle),      0.00d0,   &
    &                -sin(angle),  cos(angle),      0.00d0,   &
    &                     0.00d0,      0.00d0,      1.00d0], shape(R) )
  case default
    rotateAxis_double(:) = ieee_value(x,ieee_quiet_nan)
    return
  end select
  rotateAxis_double(:) = matmul(R,x)
  return
end function rotateAxis_double

! -------------------------------------------------
! Unit transformations

! Transform degrees to radians
real function deg2rad_float( x )
  implicit none
  real, intent(in)  :: x
  ! rad = deg/180*pi
  deg2rad_float = x/180.0*acos(-1.0)
  return
end function deg2rad_float

real(REAL64) function deg2rad_double( x )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(in)  :: x
  deg2rad_double = x/180.0d0*acos(-1.0d0)
  return
end function deg2rad_double

! Transform rad to degrees
real function rad2deg_float( x )
  implicit none
  real, intent(in)  :: x
  ! deg = rad/pi*180
  rad2deg_float = x/acos(-1.0)*180.0
  return
end function rad2deg_float

real(REAL64) function rad2deg_double( x )
  use, intrinsic :: iso_fortran_env, only: REAL64
  real(REAL64), intent(in)  :: x
  rad2deg_double = x/acos(-1.0d0)*180.0d0
  return
end function rad2deg_double

! -------------------------------------------------
! Coordinate system transformations

! Cartesian to spherical: (x,y,z) -> (r,theta,phi)
function crt2sph_float( crt )
  implicit none
  real              :: crt2sph_float(3)
  real, intent(in)  :: crt(3)
  ! r = sqrt(x**2+y**2+z**2)
  ! theta = acos(z/r)
  ! phi = atan2(y/x)
  crt2sph_float(1) = norm2( crt(:) )
  crt2sph_float(2) = merge( acos(crt(3)/crt2sph_float(1)), 0.0, crt2sph_float(1) /= 0.0 )
  crt2sph_float(3) = merge( atan2(crt(2),crt(1)), 0.0, crt(1) /= 0.0 )
  return
end function crt2sph_float

function crt2sph_double( crt )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64)              :: crt2sph_double(3)
  real(REAL64), intent(in)  :: crt(3)
  crt2sph_double(1) = norm2( crt(:) )
  crt2sph_double(2) = merge( acos(crt(3)/crt2sph_double(1)), 0.0d0, crt2sph_double(1) /= 0.0d0 )
  crt2sph_double(3) = merge( atan2(crt(2),crt(1)), 0.0d0, crt(1) /= 0.0d0 )
  return
end function crt2sph_double

! Spherical to cartesian: (r,theta,phi) -> (x,y,z)
function sph2crt_float( sph )
  implicit none
  real              :: sph2crt_float(3)
  real, intent(in)  :: sph(3)
  ! x = r*sin(theta)*cos(phi)
  ! y = r*sin(theta)*sin(phi)
  ! z = r*cos(theta)
  sph2crt_float(1) = sph(1)*sin(sph(2))*cos(sph(3))
  sph2crt_float(2) = sph(1)*sin(sph(2))*sin(sph(3))
  sph2crt_float(3) = sph(1)*cos(sph(2))
  return
end function sph2crt_float

function sph2crt_double( sph )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64)              :: sph2crt_double(3)
  real(REAL64), intent(in)  :: sph(3)
  sph2crt_double(1) = sph(1)*sin(sph(2))*cos(sph(3))
  sph2crt_double(2) = sph(1)*sin(sph(2))*sin(sph(3))
  sph2crt_double(3) = sph(1)*cos(sph(2))
  return
end function sph2crt_double

! Cartesian to cylindrical: (x,y,z) -> (r,theta,z)
function crt2cyl_float( crt )
  implicit none
  real              :: crt2cyl_float(3)
  real, intent(in)  :: crt(3)
  ! r = sqrt(x**2+y**2)
  ! theta =  atan2(y/x)
  ! z = z
  crt2cyl_float(1) = sqrt(crt(1)**2+crt(2)**2)
  crt2cyl_float(2) = merge( atan2(crt(2),crt(1)), 0.0, crt2cyl_float(1) /= 0.0 )
  crt2cyl_float(3) = crt(3)
  return
end function crt2cyl_float

function crt2cyl_double( crt )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64)              :: crt2cyl_double(3)
  real(REAL64), intent(in)  :: crt(3)
  crt2cyl_double(1) = sqrt(crt(1)**2+crt(2)**2)
  crt2cyl_double(2) = merge( atan2(crt(2),crt(1)), 0.0d0, crt2cyl_double(1) /= 0.0d0 )
  crt2cyl_double(3) = crt(3)
  return
end function crt2cyl_double

! Cylindrical to cartesian: (r,theta,z) -> (x,y,z)
function cyl2crt_float( cyl )
  implicit none
  real              :: cyl2crt_float(3)
  real, intent(in)  :: cyl(3)
  ! x = r*cos(theta)
  ! y = r*sin(theta)
  ! z = z
  cyl2crt_float(1) = cyl(1)*cos(cyl(2))
  cyl2crt_float(2) = cyl(1)*sin(cyl(2))
  cyl2crt_float(3) = cyl(3)
  return
end function cyl2crt_float

function cyl2crt_double( cyl )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64)              :: cyl2crt_double(3)
  real(REAL64), intent(in)  :: cyl(3)
  cyl2crt_double(1) = cyl(1)*cos(cyl(2))
  cyl2crt_double(2) = cyl(1)*sin(cyl(2))
  cyl2crt_double(3) = cyl(3)
  return
end function cyl2crt_double

! -------------------------------------------------
! Vector coordinates functions

! Reduces distance acording to minimal image convention
function minImg( r, box )
  implicit none
  real              :: minImg(3)
  real, intent(in)  :: r(3), box(3)
  minimg(:) = r(:)-box(:)*floor(r(:)/box(:)+0.5)
  ! hbox=box/2
  ! minimg(:) = r(:)-sign(hbox,r-hbox)-sign(hbox,r+hbox)
  return
end function minImg

! Calculates distance between two points.
real function getDistance( a, b )
  implicit none
  real, intent(in)  :: a(3), b(3)
  getDistance = norm2(a-b)
  ! getDistance(:) = sqrt(dot_product(a(:),b(:)))
  return
end function getDistance

! Calculates distance between two points, applying PBC.
real function getDistance_pbc( a, b, box )
  implicit none
  real, intent(in)  :: a(3), b(3), box(3)
  getDistance_pbc =  norm2( (a-b)-box*floor((a-b)/box+0.5) )
  return
end function getDistance_pbc

! Calculates angle between three points.
real function getAngle( a, b, c )
  implicit none
  real, intent(in)  :: a(3), b(3), c(3)
  real              :: u(3), v(3)
  u(:) = a-b
  v(:) = c-b
  getAngle = atan2(norm2(cross(u,v)),dot_product(u,v))
  return
end function getAngle

! Get dihedral angle (theta) between four points
real function getDihedral( a, b, c, d )
  implicit none
  real, intent(in)  :: a(3), b(3), c(3), d(3)
  real              :: b1(3), b2(3), b3(3), n1(3), n2(3), n3(3)
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

! On-line variance calculation for single value.
subroutine variance_float( val, mean, var, n )
  implicit none
  real, intent(in)    :: val
  real, intent(inout) :: mean, var
  integer, intent(in) :: n
  real                :: delta
  delta = val-mean
  mean = mean+delta/n
  var = var+delta*(val-mean)
  return
end subroutine variance_float

subroutine variance_double( val, mean, var, n )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(in)    :: val
  real(REAL64), intent(inout) :: mean, var
  integer, intent(in)         :: n
  real(REAL64)                :: delta
  delta = val-mean
  mean = mean+delta/n
  var = var+delta*(val-mean)
  return
end subroutine variance_double

! On-line variance calculation for real array.
subroutine variance_dfloat( val, mean, var, n )
  implicit none
  real, intent(in)    :: val(:)
  real, intent(inout) :: mean(:), var(:)
  integer, intent(in) :: n
  integer             :: i
  do i = 1, size(val)
    call variance_float( val(i), mean(i), var(i), n )
  end do
  return
end subroutine variance_dfloat

subroutine variance_ddouble( val, mean, var, n )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(in)    :: val(:)
  real(REAL64), intent(inout) :: mean(:), var(:)
  integer, intent(in)     :: n
  integer                 :: i
  do i = 1, size(val)
    call variance_double(val(i), mean(i), var(i), n)
  end do
  return
end subroutine variance_ddouble

! -------------------------------------------------
! Linear interpolation: LERP

! Linear interpolation
real function lerp_float( v0, v1, x )
  implicit none
  real, intent(in) :: v0, v1, x
  lerp_float = v0 + x * (v1-v0)
  return
end function lerp_float

real(REAL64) function lerp_double( v0, v1, x )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(in) :: v0, v1, x
  lerp_double = v0 + x * (v1-v0)
  return
end function lerp_double

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

function findKClosest_long( val, array, k ) result( r )
  use, intrinsic :: iso_fortran_env, only: INT64
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
end function findKClosest_long

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

function findKClosest_double( val, array, k ) result( r )
  use, intrinsic :: iso_fortran_env, only: REAL64
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
end function findKClosest_double

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

recursive function findCrossOver_long( array, low, high, x ) result( r )
  use, intrinsic :: iso_fortran_env, only: INT64
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
      r = findCrossOver_long(array(:), mid+1, high, x)

    else !(x < array(mid))
      r = findCrossOver_long(array(:), low, mid, x)

    end if
  end if

  return
end function findCrossOver_long

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

recursive function findCrossOver_double( array, low, high, x ) result( r )
  use, intrinsic :: iso_fortran_env, only: REAL64
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
      r = findCrossOver_double(array, mid+1, high, x)
    else !(x < array(mid))
      r = findCrossOver_double(array, low, mid, x)
    end if
  end if

  return
end function findCrossOver_double

! Swap values of A and B
subroutine swap_int( a, b )
  implicit none
  integer, intent(inout)  :: a, b
  integer                 :: temp
  temp = a
  a = b
  b = temp
  return
end subroutine swap_int

subroutine swap_long( a, b )
  use, intrinsic :: iso_fortran_env, only: INT64
  implicit none
  integer(INT64), intent(inout) :: a, b
  integer(INT64)                :: temp
  temp = a
  a = b
  b = temp
  return
end subroutine swap_long

subroutine swap_float( a, b )
  implicit none
  real, intent(inout) :: a, b
  real                :: temp
  temp = a
  a = b
  b = temp
  return
end subroutine swap_float

subroutine swap_double( a, b )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(inout) :: a, b
  real(REAL64)                :: temp
  temp = a
  a = b
  b = temp
  return
end subroutine swap_double

subroutine swap_char( a, b )
  implicit none
  character, intent(inout)  :: a, b
  character                 :: temp
  temp = a
  a = b
  b = temp
  return
end subroutine swap_char

end module xslib_vector
