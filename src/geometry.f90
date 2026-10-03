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

module xslib_geometry
  !! Module for geometry operations.
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: rotate, deg2rad, rad2deg, crt2sph, sph2crt, crt2cyl, cyl2crt
  public :: distance, angle, dihedral

  interface rotate
    !! Rotate vector by specified angle `angle` around vector `vec` or axis.
    !!
    !! Example:
    !! ```Fortran
    !! print *, rotate([1.0, 0.0, 0.0], [0.0, 1.0, 0.0], PI/2)
    !! >>> [0.0, 0.0, -1.0]
    !! print *, rotate([1.0, 0.0, 0.0], "Y", PI/2)
    !! >>> [0.0, 0.0, -1.0]
    !! ```
    module procedure :: rotate_r32, rotate_r64
    module procedure :: rotate_axis_r32, rotate_axis_r64
  end interface rotate

  interface rad2deg
    !! Convert angles from degrees to radians.
    !!
    !! Example:
    !! ```Fortran
    !! print *, deg2rad(180.0)
    !! >>> 3.14159274
    !! ```
    module procedure :: rad2deg_r32, rad2deg_r64
  end interface rad2deg

  interface deg2rad
    !! Convert angles from radians to degrees.
    !!
    !! Example:
    !! ```Fortran
    !! print *, deg2rad(PI)
    !! >>> 180.0
    !! ```
    module procedure :: deg2rad_r32, deg2rad_r64
  end interface deg2rad

  interface crt2sph
    !! Convert vector from spherical to cartesian coordinate
    !! system: `[x, y, z]` → `[r, theta, phi]`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, crt2sph([1.0, 0.0, 0.0])
    !! >>> [1.0, 1.5707964, 0.0]
    !! ```
    module procedure :: crt2sph_r32, crt2sph_r64
  end interface crt2sph

  interface sph2crt
    !! Convert vector from cartesian to cylindrical coordinate
    !! system: `[r, theta, phi]` → `[x, y, z]`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, sph2crt([1.0, PI/2, 0.0])
    !! >>> [1.0, 0.0, 0.0]
    !! ```
    module procedure :: sph2crt_r32, sph2crt_r64
  end interface sph2crt

  interface crt2cyl
    !! Convert vector from cartesian to cylindrical coordinate
    !! system: `[x, y, z]` → `[r, theta, z]`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, crt2cyl([0.0, 1.0, 0.0])
    !! >>> [1.0, 1.5707964, 0.0]
    !! ```
    module procedure :: crt2cyl_r32, crt2cyl_r64
  end interface crt2cyl

  interface cyl2crt
    !! Convert vector from cylindrical to cartesian coordinate
    !! system: `[r, theta, z]` → `[x, y, z]`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, cyl2crt([1.0, PI/2, 0.0])
    !! >>> [0.0, 1.0, 0.0]
    !! ```
    module procedure :: cyl2crt_r32, cyl2crt_r64
  end interface cyl2crt

  interface distance
    !! Calculates distance (norm) between two points.
    !!
    !! Example:
    !! ```Fortran
    !! print *, distance([0.0, 0.0, 0.0], [1.0, 1.0, 1.0])
    !! >>> 1.73205
    !! ```
    module procedure :: distance_r32, distance_r64
  end interface distance

  interface angle
    !! Calculates angle between three points.
    !! ```
    !! a        
    !!  \
    !!   b -- c
    !! ```
    !!
    !! Example:
    !! ```Fortran
    !! print *, angle([1.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 1.0, 0.0])
    !! >>> 1.57079637
    !! ```
    module procedure :: angle_r32, angle_r64
  end interface angle

  interface dihedral
    !! Calculates dihedral angle (theta) between four points.
    !! ```
    !! a        d
    !!  \      /
    !!   b -- c
    !! ```
    !!
    !! Example:
    !! ```Fortran
    !! print *, dihedral([0, 0, 1], [0, 0, 0], [1, 0, 0], [1, 1, 0])
    !! >>> 1.57079637
    !! ```
    module procedure :: dihedral_r32, dihedral_r64
  end interface dihedral

contains


function rotate_r32 (v, vector, angle) result (out)
  use xslib_linalg, only: cross
  implicit none
  real(REAL32) :: out(3)
  !! Rotated vector.
  real(REAL32), intent(in) :: v(3)
  !! Input vector.
  real(REAL32), intent(in) :: vector(3)
  !! Vector of rotation.
  real(REAL32), intent(in) :: angle
  !! Angle of rotation in radians.
  real(REAL32) :: k(3)
  
  ! SOURCE: https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  k = vector / norm2(vector)
  out = v * cos(angle) + cross(k, v) * sin(angle) + k * dot_product(k, v) * (1.0 - cos(angle))

end function rotate_r32


function rotate_r64 (v, vector, angle) result (out)
  use xslib_linalg, only: cross
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: v(3), vector(3), angle
  real(REAL64) :: k(3)

  k = vector / norm2(vector)
  out = v * cos(angle) + cross(k, v) * sin(angle) + k * dot_product(k, v) * (1.0 - cos(angle))

end function rotate_r64


function rotate_axis_r32 (v, axis, angle) result (out)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL32) :: out(3)
  !! Rotated vector.
  real(REAL32), intent(in) :: v(3)
  !! Input vector.
  real(REAL32), intent(in) :: angle
  !! Angle of rotation in radians.
  character, intent(in) :: axis
  !! Axis of rotation: `x`, `y`, or `z`.
  real(REAL32) :: rotMat(3, 3)

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
    out = ieee_value(v, IEEE_QUIET_NAN)
    return
  end select

  out = matmul(rotMat, v)

end function rotate_axis_r32


function rotate_axis_r64 (v, axis, angle) result (out) 
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: v(3), angle
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
    out = ieee_value(v, IEEE_QUIET_NAN)
    return
  end select

  out = matmul(rotMat, v)

end function rotate_axis_r64


function deg2rad_r32 (angle) result (out)
  implicit none
  real(REAL32) :: out
  !! Output angle in radians.
  real(REAL32), intent(in) :: angle
  !! Input angle in degrees.

  out = angle / 180.0 * acos(-1.0)
 
end function deg2rad_r32


function deg2rad_r64 (angle) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: angle

  out = angle / 180.0d0 * acos(-1.0d0)

end function deg2rad_r64


function rad2deg_r32 (angle) result (out)
  implicit none
  real(REAL32) :: out
  !! Input angle in radians.
  real(REAL32), intent(in) :: angle
  !! Output angle in degrees.

  out = angle / acos(-1.0) * 180.0

end function rad2deg_r32


function rad2deg_r64 (angle) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: angle

  out = angle / acos(-1.0d0) * 180.0d0

end function rad2deg_r64


function crt2sph_r32 (v) result (out)
  implicit none
  real(REAL32) :: out(3)
  !! Output vector in spherical coordinate system: `[r, theta, phi]`
  real(REAL32), intent(in) :: v(3)
  !! Input vector in cartesian coordinate system: `[x, y, z]`.

  ! r = sqrt(x**2 + y**2 + z**2)
  ! theta = atan2(x**2 + y**2, z)
  ! phi = atan2(y, x)
  out(1) = norm2(v)
  out(2) = atan2(norm2(v(1:2)), v(3))
  out(3) = atan2(v(2), v(1))

end function crt2sph_r32


function crt2sph_r64 (v) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: v(3)

  out(1) = norm2(v)
  out(2) = atan2(norm2(v(1:2)), v(3))
  out(3) = atan2(v(2), v(1))

end function crt2sph_r64


function sph2crt_r32 (v) result (out)
  implicit none
  real(REAL32) :: out(3)
  !! Output vector in cartesian coordinate system: `[x, y, z]`.
  real(REAL32), intent(in) :: v(3)
  !! Input vector in spherical coordinate system: `[r, theta, phi]`.

  ! x = r * sin(theta) * cos(phi)
  ! y = r * sin(theta) * sin(phi)
  ! z = r * cos(theta)
  out(1) = v(1) * sin(v(2)) * cos(v(3))
  out(2) = v(1) * sin(v(2)) * sin(v(3))
  out(3) = v(1) * cos(v(2))

end function sph2crt_r32


function sph2crt_r64 (v) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: v(3)

  out(1) = v(1) * sin(v(2)) * cos(v(3))
  out(2) = v(1) * sin(v(2)) * sin(v(3))
  out(3) = v(1) * cos(v(2))

end function sph2crt_r64


function crt2cyl_r32 (v) result (out)
  implicit none
  real(REAL32) :: out(3)
  !!  Output vector in cylindrical coordinate system: `[r, theta, z]`
  real(REAL32), intent(in) :: v(3)
  !! Input vector in cartesian coordinate system: `[x, y, z]`.

  ! r = sqrt(x**2 + y**2)
  ! theta =  atan2(y / x)
  ! z = z
  out(1) = sqrt(v(1)**2 + v(2)**2)
  out(2) = merge(atan2(v(2), v(1)), 0.0, out(1) /= 0.0)
  out(3) = v(3)

end function crt2cyl_r32


function crt2cyl_r64 (v) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: v(3)

  out(1) = sqrt(v(1)**2 + v(2)**2)
  out(2) = merge(atan2(v(2), v(1)), 0.0d0, out(1) /= 0.0d0)
  out(3) = v(3)

end function crt2cyl_r64


function cyl2crt_r32 (v) result (out)
  implicit none
  real(REAL32) :: out(3)
  !! Output vector in cartesian coordinate system: `[x, y, z]`.
  real(REAL32), intent(in) :: v(3)
  !! Input vector in cylindrical coordinate system: `[r, theta, z]`.

  ! x = r * cos(theta)
  ! y = r * sin(theta)
  ! z = z
  out(1) = v(1) * cos(v(2))
  out(2) = v(1) * sin(v(2))
  out(3) = v(3)

end function cyl2crt_r32


function cyl2crt_r64 (v) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: v(3)

  out(1) = v(1) * cos(v(2))
  out(2) = v(1) * sin(v(2))
  out(3) = v(3)

end function cyl2crt_r64


function distance_r32 (a, b) result (out)
  implicit none
  real(REAL32) :: out
  !! Distance between `a` and `b`.
  real(REAL32), intent(in) :: a(3)
  !! Input point.
  real(REAL32), intent(in) :: b(3)
  !! Input point.

  out = norm2(a - b)

end function distance_r32


function distance_r64 (a, b) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a(3), b(3)

  out = norm2(a - b)

end function distance_r64


function angle_r32 (a, b, c) result (out)
  use xslib_linalg, only: cross
  implicit none
  real(REAL32) :: out
  !! Output angle between points `a`, `b`, and `c` in radians.
  real(REAL32), intent(in) :: a(3)
  !! Input vector.
  real(REAL32), intent(in) :: b(3)
  !! Input vector.
  real(REAL32), intent(in) :: c(3)
  !! Input vector.
  real(REAL32) :: u(3), v(3)

  u = a - b
  v = c - b
  out = atan2(norm2(cross(u, v)), dot_product(u, v))

end function angle_r32


function angle_r64 (a, b, c) result (out)
  use xslib_linalg, only: cross
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a(3), b(3), c(3)
  real(REAL64) :: u(3), v(3)

  u = a - b
  v = c - b
  out = atan2(norm2(cross(u, v)), dot_product(u, v))

end function angle_r64


function dihedral_r32 (a, b, c, d) result (out)
  use xslib_linalg, only: cross
  implicit none
  real(REAL32) :: out
  !! Dihedral angle between points `a`, `b`, `c`, and `d` in radians.
  real(REAL32), intent(in) :: a(3)
  !! Input vector.
  real(REAL32), intent(in) :: b(3)
  !! Input vector.
  real(REAL32), intent(in) :: c(3)
  !! Input vector.
  real(REAL32), intent(in) :: d(3)
  !! Input vector.
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

end function dihedral_r32


function dihedral_r64 (a, b, c, d) result (out)
  use xslib_linalg, only: cross
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

end function dihedral_r64

end module xslib_geometry
