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

  ! Default vector dimension
  integer, parameter :: DIM = 3

  ! %%%
  ! # `VECTOR` - Vector functions
  !   Module `xslib_vector` contains function for vector operations. Default dimension of vectors is `DIM = 3`.
  !   Supports both single and double precision (`DP`).
  ! %%%

  interface cross
    module procedure :: cross_real32, cross_real64
  end interface cross

  interface rotate
    module procedure :: rotate_real32, rotate_real64, rotate_axis_real32, rotate_axis_real64
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

function cross_real32 (u, v) result (out)
  ! %%%
  ! ## `CROSS` - Vector cross product
  ! #### DESCRIPTION
  !   Return the cross product of two vectors i.e. `u × v`. Note that 
  !   cross product is anti-commutative *i.e.* `(u × v) = -(v × u)`.
  ! #### USAGE
  !   ```Fortran
  !   out = cross(u, v)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(DIM), intent(IN) :: u, v`
  !     Input vectors.
  !   * `real(ANY), dimension(DIM) :: out`
  !     Output vector.
  ! #### EXAMPLE
  !   ```Fortran
  !   > cross([1.0, 0.0, 0.0], [0.0, 1.0, 0.0])
  !   [0.0, 0.0, 1.0]
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out(3)
  real, intent(in) :: u(3), v(3)

  out(1) = u(2) * v(3) - u(3) * v(2)
  out(2) = u(3) * v(1) - u(1) * v(3)
  out(3) = u(1) * v(2) - u(2) * v(1)

end function cross_real32


function cross_real64 (u, v) result (out)
  implicit none
  real(REAL64) :: out(3)
  real(REAL64), intent(in) :: u(3), v(3)

  out(1) = u(2) * v(3) - u(3) * v(2)
  out(2) = u(3) * v(1) - u(1) * v(3)
  out(3) = u(1) * v(2) - u(2) * v(1)

end function cross_real64


function rotate_real32 (v, vector, angle) result (out)
  ! %%%
  ! ## `ROTATE` - Vector rotation
  ! #### DESCRIPTION
  !   Rotate vector by specified angle `angle` around vector `vec` or axis.
  ! #### USAGE
  !   ```Fortran
  !   out = rotate(v, vector, angle)
  !   out = rotate(v, axis, angle)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(DIM), intent(IN) :: v`
  !     Input vector.
  !   * `real(ANY), dimension(DIM), intent(IN) :: vector`
  !     Vector of rotation.
  !   * `character, intent(IN) :: axis`
  !     Axis of rotation: `x`, `y`, or `z`.
  !   * `real(ANY), intent(IN) :: angle`
  !     Angle of rotation in radians.
  !   * `real(ANY), dimension(DIM) :: out`
  !     Output vector.
  ! #### EXAMPLE
  !   ```Fortran
  !   > rotate([1.0, 0.0, 0.0], [0.0, 1.0, 0.0], PI/2)
  !   [0.0, 0.0, -1.0]
  !   > rotate([1.0, 0.0, 0.0], "y", PI/2)
  !   [0.0, 0.0, -1.0]
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out(DIM)
  real(REAL32), intent(in) :: v(DIM), vector(DIM), angle
  real(REAL32) :: k(DIM)
  
  ! SOURCE: https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
  k = vector / norm2(vector)
  out = v * cos(angle) + cross(k, v) * sin(angle) + k * dot_product(k, v) * (1.0 - cos(angle))

end function rotate_real32


function rotate_real64 (v, vector, angle) result (out)
  implicit none
  real(REAL64) :: out(DIM)
  real(REAL64), intent(in) :: v(DIM), vector(DIM), angle
  real(REAL64) :: k(DIM)

  k = vector / norm2(vector)
  out = v * cos(angle) + cross(k, v) * sin(angle) + k * dot_product(k, v) * (1.0 - cos(angle))

end function rotate_real64


function rotate_axis_real32 (v, axis, angle) result (out)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL32) :: out(DIM)
  real(REAL32), intent(in) :: v(DIM), angle
  character, intent(in) :: axis
  real(REAL32) :: rotMat(DIM,DIM)

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

  return
end function rotate_axis_real32


function rotate_axis_real64 (v, axis, angle) result (out) 
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL64) :: out(DIM)
  real(REAL64), intent(in) :: v(DIM), angle
  character, intent(in) :: axis
  real(REAL64) :: rotMat(DIM,DIM)

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

end function rotate_axis_real64


function deg2rad_real32 (angle) result (out)
  ! %%%
  ! ## `DEG2RAD` - Degrees to radians 
  ! #### DESCRIPTION
  !   Convert angles from degrees to radians.
  ! #### USAGE
  !   ```Fortran
  !   out = deg2rad(angle)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), intent(IN) :: angle`
  !     Input angle in degrees.
  !   * `real(ANY) :: out`
  !     Output angle in radians.
  ! #### EXAMPLE
  !   ```Fortran
  !   > deg2rad(180.0)
  !   DIM.14159274
  !   ```
  ! %%%
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


function rad2deg_real32 (angle) result (out)
  ! %%%
  ! ## `RAD2DEG` - Radians to degrees 
  ! #### DESCRIPTION
  !   Convert angles from radians to degrees.
  ! #### USAGE
  !   ```Fortran
  !   out = rad2deg(angle)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), intent(IN) :: angle`
  !     Input angle in radians.
  !   * `real(ANY) :: out`
  !     Output angle in degrees.
  ! #### EXAMPLE
  !   ```Fortran
  !   > rad2deg(PI)
  !   180.0
  !   ```
  ! %%%
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


function crt2sph_real32 (v) result (out)
  ! %%%
  ! ## `CRT2SPH` - Cartesian to spherical 
  ! #### DESCRIPTION
  !   Convert vector from cartesian to spherical coordinate system: `[x, y, z]` → `[r, theta, phi]`.
  ! #### USAGE
  !   ```Fortran
  !   out = crt2sph(v)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(DIM), intent(IN) :: v`
  !     Input vector in cartesian coordinate system.
  !   * `real(ANY) :: out`
  !     Output vector in spherical coordinate system.
  ! #### EXAMPLE
  !   ```Fortran
  !   > crt2sph([1.0, 0.0, 0.0])
  !   [1.0, 1.5707964, 0.0]
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out(DIM)
  real(REAL32), intent(in) :: v(DIM)

  ! r = sqrt(x**2 + y**2 + z**2)
  ! theta = atan2(x**2 + y**2, z)
  ! phi = atan2(y, x)
  out(1) = norm2(v)
  out(2) = atan2(norm2(v(1:2)), v(3))
  out(3) = atan2(v(2), v(1))

end function crt2sph_real32


function crt2sph_real64 (v) result (out)
  implicit none
  real(REAL64) :: out(DIM)
  real(REAL64), intent(in) :: v(DIM)

  out(1) = norm2(v)
  out(2) = atan2(norm2(v(1:2)), v(3))
  out(3) = atan2(v(2), v(1))

end function crt2sph_real64


function sph2crt_real32 (v) result (out)
  ! %%%
  ! ## `SPH2CRT` - Spherical to cartesian
  ! #### DESCRIPTION
  !   Convert vector from spherical to cartesian coordinate system: `[r, theta, phi]` → `[x, y, z]`.
  ! #### USAGE
  !   ```Fortran
  !   out = sph2crt(v)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(DIM), intent(IN) :: v`
  !     Input vector in spherical coordinate system.
  !   * `real(ANY) :: out`
  !     Output vector in cartesian coordinate system.
  ! #### EXAMPLE
  !   ```Fortran
  !   > sph2crt([1.0, PI/2, 0.0])
  !   [1.0, 0.0, 0.0]
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out(DIM)
  real(REAL32), intent(in) :: v(DIM)

  ! x = r * sin(theta) * cos(phi)
  ! y = r * sin(theta) * sin(phi)
  ! z = r * cos(theta)
  out(1) = v(1) * sin(v(2)) * cos(v(3))
  out(2) = v(1) * sin(v(2)) * sin(v(3))
  out(3) = v(1) * cos(v(2))

end function sph2crt_real32


function sph2crt_real64 (v) result (out)
  implicit none
  real(REAL64) :: out(DIM)
  real(REAL64), intent(in) :: v(DIM)

  out(1) = v(1) * sin(v(2)) * cos(v(3))
  out(2) = v(1) * sin(v(2)) * sin(v(3))
  out(3) = v(1) * cos(v(2))

end function sph2crt_real64


function crt2cyl_real32 (v) result (out)
  ! %%%
  ! ## `CRT2CYL` - Cartesian to cylindrical
  ! #### DESCRIPTION
  !   Convert vector from cartesian to cylindrical coordinate system: `[x, y, z]` → `[r, theta, z]`.
  !   <!-- TODO: Add image of cylindrical coordinate system -->
  ! #### USAGE
  !   ```Fortran
  !   out = crt2cyl(v)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(DIM), intent(IN) :: v`
  !     Input vector in cartesian coordinate system.
  !   * `real(ANY) :: out`
  !     Output vector in cylindrical coordinate system.
  ! #### EXAMPLE
  !   ```Fortran
  !   > crt2cyl([0.0, 1.0, 0.0])
  !   [1.0, 1.5707964, 0.0]
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out(DIM)
  real(REAL32), intent(in) :: v(DIM)

  ! r = sqrt(x**2 + y**2)
  ! theta =  atan2(y / x)
  ! z = z
  out(1) = sqrt(v(1)**2 + v(2)**2)
  out(2) = merge(atan2(v(2), v(1)), 0.0, out(1) /= 0.0)
  out(3) = v(3)

end function crt2cyl_real32


function crt2cyl_real64 (v) result (out)
  implicit none
  real(REAL64) :: out(DIM)
  real(REAL64), intent(in) :: v(DIM)

  out(1) = sqrt(v(1)**2 + v(2)**2)
  out(2) = merge(atan2(v(2), v(1)), 0.0d0, out(1) /= 0.0d0)
  out(3) = v(3)

end function crt2cyl_real64


function cyl2crt_real32 (v) result (out)
  ! %%%
  ! ## `CYL2CRT` - Cylindrical to cartesian 
  ! #### DESCRIPTION
  !   Convert vector from cylindrical to cartesian coordinate system: `[r, theta, z]` → `[x, y, z]`.
  ! #### USAGE
  !   ```Fortran
  !   out = cyl2crt(v)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(DIM), intent(IN) :: v`
  !     Input vector in cylindrical coordinate system.
  !   * `real(ANY) :: out`
  !     Output vector in cartesian coordinate system.
  ! #### EXAMPLE
  !   ```Fortran
  !   > cyl2crt([1.0, PI/2, 0.0])
  !   [0.0, 1.0, 0.0]
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out(DIM)
  real(REAL32), intent(in) :: v(DIM)

  ! x = r * cos(theta)
  ! y = r * sin(theta)
  ! z = z
  out(1) = v(1) * cos(v(2))
  out(2) = v(1) * sin(v(2))
  out(3) = v(3)

end function cyl2crt_real32


function cyl2crt_real64 (v) result (out)
  implicit none
  real(REAL64) :: out(DIM)
  real(REAL64), intent(in) :: v(DIM)

  out(1) = v(1) * cos(v(2))
  out(2) = v(1) * sin(v(2))
  out(3) = v(3)

end function cyl2crt_real64


function distance_real32 (a, b) result (out)
  ! %%%
  ! ## `DISTANCE` - Vector distance
  ! #### DESCRIPTION
  !   Calculates distance (norm) between two points (vectors).
  ! #### USAGE
  !   ```Fortran
  !   out = distance(a, b)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(DIM), intent(IN) :: a, b`
  !     Input vector points.
  !   * `real(ANY) :: out`
  !     Output distance.
  ! #### EXAMPLE
  !   ```Fortran
  !   > distance([0.0, 0.0, 0.0], [1.0, 1.0, 1.0])
  !   1.73205
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a(DIM), b(DIM)

  out = norm2(a - b)

end function distance_real32


function distance_real64 (a, b) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a(DIM), b(DIM)

  out = norm2(a - b)

end function distance_real64


function angle_real32 (a, b, c) result (out)
  ! %%%
  ! ## `ANGLE` - Vector angle
  ! #### DESCRIPTION
  !   Calculates angle between three points (vectors).
  ! #### USAGE
  !   ```Fortran
  !   out = angle(a, b, c)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(DIM), intent(IN) :: a, b, c`
  !     Input vector points.
  !   * `real(ANY) :: out`
  !     Output angle in radians.
  ! #### EXAMPLE
  !   ```Fortran
  !   > angle([1.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 1.0, 0.0])
  !   1.57079637
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a(DIM), b(DIM), c(DIM)
  real(REAL32) :: u(DIM), v(DIM)

  u = a - b
  v = c - b
  out = atan2(norm2(cross(u, v)), dot_product(u, v))

end function angle_real32


function angle_real64 (a, b, c) result (out)
  implicit none
  real(REAL64) :: out
  real(REAL64), intent(in) :: a(DIM), b(DIM), c(DIM)
  real(REAL64) :: u(DIM), v(DIM)

  u = a - b
  v = c - b
  out = atan2(norm2(cross(u, v)), dot_product(u, v))

end function angle_real64


function dihedral_real32 (a, b, c, d) result (out)
  ! %%%
  ! ## `DIHEDRAL` - Vector dihedral angle
  ! #### DESCRIPTION
  !   Calculates dihedral angle (theta) between four points (vectors).
  ! #### USAGE
  !   ```Fortran
  !   out = dihedral(a, b, c, d)
  !   ```
  ! #### PARAMETERS
  !   * `real(ANY), dimension(DIM), intent(IN) :: a, b, c, d`
  !     Input vector points.
  !   * `real(ANY) :: out`
  !     Output dihedral angle in radians.
  ! #### EXAMPLE
  !   ```Fortran
  !   > dihedral([0, 0, 1], [0, 0, 0], [1, 0, 0], [1, 1, 0])
  !   1.57079637
  !   ```
  ! %%%
  implicit none
  real(REAL32) :: out
  real(REAL32), intent(in) :: a(DIM), b(DIM), c(DIM), d(DIM)
  real(REAL32) :: b1(DIM), b2(DIM), b3(DIM), n1(DIM), n2(DIM), n3(DIM)

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
  real(REAL64), intent(in) :: a(DIM), b(DIM), c(DIM), d(DIM)
  real(REAL64) :: b1(DIM), b2(DIM), b3(DIM), n1(DIM), n2(DIM), n3(DIM)

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
