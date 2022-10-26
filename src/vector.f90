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

module xslib_vector
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: cross, rotate, deg2rad, rad2deg, crt2sph, sph2crt, crt2cyl, cyl2crt, &
  & distance, angle, dihedral

  interface cross
    module procedure :: cross_real32, cross_real64
  end interface cross

  interface rotate
    module procedure :: rotate_real32, rotate_real64
    module procedure :: rotateAxis_real32, rotateAxis_real64
  end interface rotate

  interface rad2deg
    module procedure :: rad2deg_real32, rad2deg_real64
  end interface rad2deg

  interface deg2rad
    module procedure :: deg2rad_real32, deg2rad_real64
  end interface deg2rad

  interface crt2sph
    module procedure :: crt2sph_real32, crt2sph_real64
  end interface crt2sph

  interface sph2crt
    module procedure :: sph2crt_real32, sph2crt_real64
  end interface sph2crt

  interface crt2cyl
    module procedure :: crt2cyl_real32, crt2cyl_real64
  end interface crt2cyl

  interface cyl2crt
    module procedure :: cyl2crt_real32, cyl2crt_real64
  end interface cyl2crt

  interface distance
    module procedure :: distance_real32, distance_real64
  end interface distance

  interface angle
    module procedure :: angle_real32, angle_real64
  end interface angle

  interface dihedral
    module procedure :: dihedral_real32, dihedral_real64
  end interface dihedral

contains

! -------------------------------------------------

! Calculates vector product: a x b
function cross_real32 (a, b) result (out)
  implicit none
  real(REAL32) :: out(3)
  real, intent(in) :: a(3), b(3)

  out(1) = a(2) * b(3) - a(3) * b(2)
  out(2) = a(3) * b(1) - a(1) * b(3)
  out(3) = a(1) * b(2) - a(2) * b(1)

end function cross_real32

function cross_real64 (a, b) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: a(3), b(3)

  out(1) = a(2) * b(3) - a(3) * b(2)
  out(2) = a(3) * b(1) - a(1) * b(3)
  out(3) = a(1) * b(2) - a(2) * b(1)

end function cross_real64

! Rotate vector around specified vector by angle.
function rotate_real32 (a, vec, angle) result (out)
  implicit none
  real(REAL32) :: out(3)
  real(REAL32), intent(in) :: a(3), vec(3), angle
  real(REAL32) :: k(3)
  
  ! SOURCE: https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  k = vec / norm2(vec)
  out = a * cos(angle) + cross(k, a) * sin(angle) + k * dot_product(k, a) * (1.0 - cos(angle))

end function rotate_real32

function rotate_real64 (a, vec, angle) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: a(3), vec(3), angle
  real(REAL64) :: k(3)

  k = vec / norm2(vec)
  out = a * cos(angle) + cross(k, a) * sin(angle) + k * dot_product(k, a) * (1.0 - cos(angle))

end function rotate_real64

! Rotate vector around specified axis by angle.
function rotateAxis_real32 (a, axis, angle) result (out)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL32) :: out(3)
  real(REAL32), intent(in) :: a(3), angle
  character, intent(in) :: axis
  real(REAL32) :: rotMat(3,3)

  ! SOURCE: https://en.wikipedia.org/wiki/Rotation_matrix
  ! NOTE: Rotation matrix is transposed compared to SOURCE,
  ! * as fortran is row-major language.

  select case (trim(axis))
  case ("x", "X")
    rotMat = reshape([       1.0,         0.0,         0.0, &
    &                        0.0,  cos(angle),  sin(angle), &
    &                        0.0, -sin(angle),  cos(angle)], shape(rotMat))
  case ("y", "Y")
    rotMat = reshape([cos(angle),         0.0, -sin(angle),   &
    &                        0.0,         1.0,         0.0,   &
    &                 sin(angle),         0.0,  cos(angle)], shape(rotMat))
  case ("z", "Z")
    rotMat = reshape([cos(angle),  sin(angle),         0.0,   &
    &                -sin(angle),  cos(angle),         0.0,   &
    &                        0.0,         0.0,         1.0], shape(rotMat))
  case default
    out = ieee_value(a, IEEE_QUIET_NAN)
    return
  end select

  out = matmul(rotMat, a)

  return
end function rotateAxis_real32

function rotateAxis_real64 (a, axis, angle) result (out) 
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: a(3), angle
  character, intent(in) :: axis
  real(REAL64) :: rotMat(3,3)

  select case (trim(axis))
  case ("x", "X")
    rotMat = reshape([     1.0d0,       0.0d0,       0.0d0, &
    &                      0.0d0,  cos(angle),  sin(angle), &
    &                      0.0d0, -sin(angle),  cos(angle)], shape(rotMat))
  case ("y", "Y")
    rotMat = reshape([cos(angle),       0.0d0, -sin(angle),   &
    &                      0.0d0,       1.0d0,       0.0d0,   &
    &                 sin(angle),       0.0d0,  cos(angle)], shape(rotMat))
  case ("z", "Z")
    rotMat = reshape([cos(angle),  sin(angle),       0.0d0,   &
    &                -sin(angle),  cos(angle),       0.0d0,   &
    &                      0.0d0,       0.0d0,       1.0d0], shape(rotMat))
  case default
    out = ieee_value(a, IEEE_QUIET_NAN)
    return
  end select

  out = matmul(rotMat, a)

end function rotateAxis_real64

! -------------------------------------------------
! Unit transformations

! Convert angles from degrees to radians.

function deg2rad_real32 (angle) result (out)
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: angle

  out = angle / 180.0 * acos(-1.0)
 
end function deg2rad_real32

function deg2rad_real64 (angle) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: angle

  out = angle / 180.0d0 * acos(-1.0d0)

end function deg2rad_real64

! Convert angles from radians to degrees.
function rad2deg_real32 (angle) result (out)
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: angle

  out = angle / acos(-1.0) * 180.0

end function rad2deg_real32

function rad2deg_real64 (angle) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: angle

  out = angle / acos(-1.0d0) * 180.0d0

end function rad2deg_real64

! -------------------------------------------------
! Coordinate system transformations

! Convert cartesian to spherical coordinate system: (x, y, z) -> (r, theta, phi)
function crt2sph_real32 (a) result (out)
  implicit none
  real(REAL32) :: out(3)
  real(REAL32), intent(in) :: a(3)

  ! r = sqrt(x**2 + y**2 + z**2)
  ! theta = atan2(x**2 + y**2, z)
  ! phi = atan2(y, x)
  out(1) = norm2(a)
  out(2) = atan2(norm2(a(1:2)), a(3))
  out(3) = atan2(a(2), a(1))

end function crt2sph_real32

function crt2sph_real64 (a) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: a(3)

  out(1) = norm2(a)
  out(2) = atan2(norm2(a(1:2)), a(3))
  out(3) = atan2(a(2), a(1))

end function crt2sph_real64

! Convert spherical to cartesian coordinate system: (r, theta, phi) -> (x, y, z)
function sph2crt_real32 (a) result (out)
  implicit none
  real(REAL32) :: out(3)
  real(REAL32), intent(in) :: a(3)

  ! x = r * sin(theta) * cos(phi)
  ! y = r * sin(theta) * sin(phi)
  ! z = r * cos(theta)
  out(1) = a(1) * sin(a(2)) * cos(a(3))
  out(2) = a(1) * sin(a(2)) * sin(a(3))
  out(3) = a(1) * cos(a(2))

end function sph2crt_real32

function sph2crt_real64 (a) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: a(3)

  out(1) = a(1) * sin(a(2)) * cos(a(3))
  out(2) = a(1) * sin(a(2)) * sin(a(3))
  out(3) = a(1) * cos(a(2))

end function sph2crt_real64

! Convert cartesian to cylindrical coordinate system: (x, y, z) -> (r, theta, z)
function crt2cyl_real32 (a) result (out)
  implicit none
  real(REAL32) :: out(3)
  real(REAL32), intent(in) :: a(3)

  ! r = sqrt(x**2 + y**2)
  ! theta =  atan2(y / x)
  ! z = z
  out(1) = sqrt(a(1)**2 + a(2)**2)
  out(2) = merge(atan2(a(2), a(1)), 0.0, out(1) /= 0.0)
  out(3) = a(3)

end function crt2cyl_real32

function crt2cyl_real64 (a) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: a(3)

  out(1) = sqrt(a(1)**2 + a(2)**2)
  out(2) = merge(atan2(a(2), a(1)), 0.0d0, out(1) /= 0.0d0)
  out(3) = a(3)

end function crt2cyl_real64

! Convert cylindrical to cartesian coordinate system: (r, theta, z) -> (x, y, z)
function cyl2crt_real32 (a) result (out)
  implicit none
  real(REAL32) :: out(3)
  real(REAL32), intent(in) :: a(3)

  ! x = r * cos(theta)
  ! y = r * sin(theta)
  ! z = z
  out(1) = a(1) * cos(a(2))
  out(2) = a(1) * sin(a(2))
  out(3) = a(3)

end function cyl2crt_real32

function cyl2crt_real64 (a) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: a(3)

  out(1) = a(1) * cos(a(2))
  out(2) = a(1) * sin(a(2))
  out(3) = a(3)

end function cyl2crt_real64

! -------------------------------------------------
! Vector coordinates functions

! Calculates distance between two points.
function distance_real32 (a, b) result (out)
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a(3), b(3)

  out = norm2(a - b)

end function distance_real32

function distance_real64 (a, b) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a(3), b(3)

  out = norm2(a - b)

end function distance_real64

! Calculates angle between three points.
function angle_real32 (a, b, c) result (out)
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a(3), b(3), c(3)
  real(REAL32) :: u(3), v(3)

  u = a - b
  v = c - b
  out = atan2(norm2(cross(u, v)), dot_product(u, v))

end function angle_real32

function angle_real64 (a, b, c) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a(3), b(3), c(3)
  real(REAL64) :: u(3), v(3)

  u = a - b
  v = c - b
  out = atan2(norm2(cross(u, v)), dot_product(u, v))

end function angle_real64

! Get dihedral angle (theta) between four points.
function dihedral_real32 (a, b, c, d) result (out)
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a(3), b(3), c(3), d(3)
  real(REAL32) :: b1(3), b2(3), b3(3), n1(3), n2(3), n3(3)

  ! SOURCE: https://math.stackexchange.com/questions/47059/how-do-i-calculate-a-dihedral-angle-given-cartesian-coordinates
  b1 = b - a
  b2 = c - a
  b3 = d - a
  n1 = cross(b1, b2)
  n2 = cross(b2, b3)
  n3 = cross(n1, b2)
  n1 = n1 / norm2(n1)
  n2 = n2 / norm2(n2)
  n3 = n3 / norm2(n3)
  out = atan2(dot_product(n3, n2), dot_product(n1, n2))
  out = abs(out)

end function dihedral_real32

function dihedral_real64 (a, b, c, d) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a(3), b(3), c(3), d(3)
  real(REAL64) :: b1(3), b2(3), b3(3), n1(3), n2(3), n3(3)

  b1 = b - a
  b2 = c - a
  b3 = d - a
  n1 = cross(b1, b2)
  n2 = cross(b2, b3)
  n3 = cross(n1, b2)
  n1 = n1 / norm2(n1)
  n2 = n2 / norm2(n2)
  n3 = n3 / norm2(n3)
  out = atan2(dot_product(n3, n2), dot_product(n1, n2))
  out = abs(out)

end function dihedral_real64

end module xslib_vector
