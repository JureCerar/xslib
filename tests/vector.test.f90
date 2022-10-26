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
  use iso_fortran_env, only: REAL32, REAL64
  use xslib_vector
  implicit none
  real, parameter :: DELTA = 0.01

  call cross_test_real32 ()
  call cross_test_real64 ()

  call rotate_test_real32 ()
  call rotate_test_real64 ()

  call radian_test_real32 ()
  call radian_test_real64 ()

  call spherical_test_real32 ()
  call spherical_test_real64 ()

  call cylindrical_test_real32 ()
  call cylindrical_test_real64 ()
  
  call coordinate_test_real32 ()
  call coordinate_test_real64 ()

contains

! Test vector cross product.
subroutine cross_test_real32 ()
  implicit none
  integer, parameter :: DIM = 3
  real(REAL32) :: a(DIM), b(DIM), c(DIM)

  a = [1.0, 1.0, 1.0]
  b = [1.0, 1.0, 1.0]
  c = cross(a, b)
  if (any(abs(c - [0.0, 0.0, 0.0]) > DELTA)) error stop  

  a = [1.0, 0.0, 0.0]
  b = [0.0, 1.0, 0.0]
  c = cross(a, b)
  if (any(abs(c - [0.0, 0.0, 1.0]) > DELTA)) error stop  

  a = [0.0, 1.0, 0.0]
  b = [0.0, 0.0, 1.0]
  c = cross(a, b)
  if (any(abs(c - [1.0, 0.0, 0.0]) > DELTA)) error stop  

  a = [0.0, 0.0, 1.0]
  b = [1.0, 0.0, 0.0]
  c = cross(a, b)
  if (any(abs(c - [0.0, 1.0, 0.0]) > DELTA)) error stop  

end subroutine cross_test_real32

subroutine cross_test_real64 ()
  implicit none
  integer, parameter :: DIM = 3
  real(REAL64) :: a(DIM), b(DIM), c(DIM)

  a = [1.0, 1.0, 1.0]
  b = [1.0, 1.0, 1.0]
  c = cross(a, b)
  if (any(abs(c - [0.0, 0.0, 0.0]) > DELTA)) error stop  

  a = [1.0, 0.0, 0.0]
  b = [0.0, 1.0, 0.0]
  c = cross(a, b)
  if (any(abs(c - [0.0, 0.0, 1.0]) > DELTA)) error stop  

  a = [0.0, 1.0, 0.0]
  b = [0.0, 0.0, 1.0]
  c = cross(a, b)
  if (any(abs(c - [1.0, 0.0, 0.0]) > DELTA)) error stop  

  a = [0.0, 0.0, 1.0]
  b = [1.0, 0.0, 0.0]
  c = cross(a, b)
  if (any(abs(c - [0.0, 1.0, 0.0]) > DELTA)) error stop  

end subroutine cross_test_real64

! Test vector rotation functions.
subroutine rotate_test_real32 ()
  implicit none
  integer, parameter :: DIM = 3
  real, parameter :: PI = acos(-1.0)
  real(REAL32) :: a(DIM), b(DIM), c(DIM), vec(DIM),  angle

  a = [1.0, 0.0, 0.0]
  vec = [1.0, 0.0, 0.0]
  angle = PI / 2
  b =  rotate(a, vec, angle)
  c =  rotate(a, "x", angle)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop
  if (any(abs(c - [1.0, 0.0, 0.0]) > DELTA)) error stop

  a = [1.0, 0.0, 0.0]
  vec = [0.0, 1.0, 0.0]
  angle = PI / 2
  b =  rotate(a, vec, angle)
  c =  rotate(a, "y", angle)
  if (any(abs(b - [0.0, 0.0, -1.0]) > DELTA)) error stop
  if (any(abs(c - [0.0, 0.0, -1.0]) > DELTA)) error stop
  
  a = [1.0, 0.0, 0.0]
  vec = [0.0, 0.0, 1.0]
  angle = PI
  b =  rotate(a, vec, angle)
  c =  rotate(a, "z", angle)
  if (any(abs(b - [-1.0, 0.0, 0.0]) > DELTA)) error stop
  if (any(abs(c - [-1.0, 0.0, 0.0]) > DELTA)) error stop

end subroutine rotate_test_real32

subroutine rotate_test_real64 ()
  implicit none
  integer, parameter :: DIM = 3
  real, parameter :: PI = acos(-1.0)
  real(REAL64) :: a(DIM), b(DIM), c(DIM), vec(DIM),  angle

  a = [1.0, 0.0, 0.0]
  vec = [1.0, 0.0, 0.0]
  angle = PI / 2
  b =  rotate(a, vec, angle)
  c =  rotate(a, "x", angle)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop
  if (any(abs(c - [1.0, 0.0, 0.0]) > DELTA)) error stop

  a = [1.0, 0.0, 0.0]
  vec = [0.0, 1.0, 0.0]
  angle = PI / 2
  b =  rotate(a, vec, angle)
  c =  rotate(a, "y", angle)
  if (any(abs(b - [0.0, 0.0, -1.0]) > DELTA)) error stop
  if (any(abs(c - [0.0, 0.0, -1.0]) > DELTA)) error stop
  
  a = [1.0, 0.0, 0.0]
  vec = [0.0, 0.0, 1.0]
  angle = PI
  b =  rotate(a, vec, angle)
  c =  rotate(a, "z", angle)
  if (any(abs(b - [-1.0, 0.0, 0.0]) > DELTA)) error stop
  if (any(abs(c - [-1.0, 0.0, 0.0]) > DELTA)) error stop

end subroutine rotate_test_real64

! Test radian and degree transformation functions.
subroutine radian_test_real32 ()
  implicit none
  real(REAL32), parameter :: PI = acos(-1.0)
  real(REAL32) :: val

  val = 0.0
  if (abs(deg2rad(val) - 0.0) > DELTA) error stop
  val = 90.0
  if (abs(deg2rad(val) - PI/2) > DELTA) error stop
  val = 180.0
  if (abs(deg2rad(val) - PI) > DELTA) error stop
  val = 270.0
  if (abs(deg2rad(val) - 3*PI/2) > DELTA) error stop
  val = 360.0
  if (abs(deg2rad(val) - 2*PI) > DELTA) error stop
  
  val = 0.
  if (abs(rad2deg(val) - 0.0) > DELTA) error stop
  val = PI/2
  if (abs(rad2deg(val) - 90.0) > DELTA) error stop
  val = PI
  if (abs(rad2deg(val) - 180.0) > DELTA) error stop
  val = 3*PI/2
  if (abs(rad2deg(val) - 270.0) > DELTA) error stop
  val = 2*PI
  if (abs(rad2deg(val) - 360.0) > DELTA) error stop

end subroutine radian_test_real32

subroutine radian_test_real64 ()
  implicit none
  real(REAL64), parameter :: PI = acos(-1.0)
  real(REAL64) :: val

  val = 0.0
  if (abs(deg2rad(val) - 0.0) > DELTA) error stop
  val = 90.0
  if (abs(deg2rad(val) - PI/2) > DELTA) error stop
  val = 180.0
  if (abs(deg2rad(val) - PI) > DELTA) error stop
  val = 270.0
  if (abs(deg2rad(val) - 3*PI/2) > DELTA) error stop
  val = 360.0
  if (abs(deg2rad(val) - 2*PI) > DELTA) error stop
  
  val = 0.
  if (abs(rad2deg(val) - 0.0) > DELTA) error stop
  val = PI/2
  if (abs(rad2deg(val) - 90.0) > DELTA) error stop
  val = PI
  if (abs(rad2deg(val) - 180.0) > DELTA) error stop
  val = 3*PI/2
  if (abs(rad2deg(val) - 270.0) > DELTA) error stop
  val = 2*PI
  if (abs(rad2deg(val) - 360.0) > DELTA) error stop

end subroutine radian_test_real64

! Test spherical coordinate transformation functions.
subroutine spherical_test_real32 ()
  implicit none
  integer, parameter :: DIM = 3
  real, parameter :: PI = acos(-1.0)
  real(REAL32) :: a(DIM), b(DIM)

  ! Cartesian to spherical
  a = [0.0, 0.0, 0.0]
  b = crt2sph(a)
  if (any(abs(b - [0.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, 0.0, 0.0]
  b = crt2sph(a)
  if (any(abs(b - [1.0, PI/2, 0.0]) > DELTA)) error stop 

  a = [0.0, 1.0, 0.0]
  b = crt2sph(a)
  if (any(abs(b - [1.0, PI/2, PI/2]) > DELTA)) error stop 

  a = [0.0, 0.0, 1.0]
  b = crt2sph(a)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop 

  ! Spherical to cartesian 
  a = [0.0, 0.0, 0.0]
  b = sph2crt(a)
  if (any(abs(b - [0.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, PI/2, 0.0]
  b = sph2crt(a)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, PI/2, PI/2]
  b = sph2crt(a)
  if (any(abs(b - [0.0, 1.0, 0.0]) > DELTA)) error stop 

  a = [1.0, 0.0, 0.0]
  b = sph2crt(a)
  if (any(abs(b - [0.0, 0.0, 1.0]) > DELTA)) error stop 

end subroutine spherical_test_real32

subroutine spherical_test_real64 ()
  implicit none
  integer, parameter :: DIM = 3
  real, parameter :: PI = acos(-1.0)
  real(REAL64) :: a(DIM), b(DIM)

  ! Cartesian to spherical
  a = [0.0, 0.0, 0.0]
  b = crt2sph(a)
  if (any(abs(b - [0.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, 0.0, 0.0]
  b = crt2sph(a)
  if (any(abs(b - [1.0, PI/2, 0.0]) > DELTA)) error stop 

  a = [0.0, 1.0, 0.0]
  b = crt2sph(a)
  if (any(abs(b - [1.0, PI/2, PI/2]) > DELTA)) error stop 

  a = [0.0, 0.0, 1.0]
  b = crt2sph(a)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop 

  ! Spherical to cartesian 
  a = [0.0, 0.0, 0.0]
  b = sph2crt(a)
  if (any(abs(b - [0.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, PI/2, 0.0]
  b = sph2crt(a)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, PI/2, PI/2]
  b = sph2crt(a)
  if (any(abs(b - [0.0, 1.0, 0.0]) > DELTA)) error stop 

  a = [1.0, 0.0, 0.0]
  b = sph2crt(a)
  if (any(abs(b - [0.0, 0.0, 1.0]) > DELTA)) error stop 

end subroutine spherical_test_real64

! Test cylindrical coordinate transformation functions.
subroutine cylindrical_test_real32 ()
  implicit none
  integer, parameter :: DIM = 3
  real, parameter :: PI = acos(-1.0)
  real(REAL32) :: a(DIM), b(DIM)

  ! Cartesian to cylindrical
  a = [0.0, 0.0, 0.0]
  b = crt2cyl(a)
  if (any(abs(b - [0.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, 0.0, 0.0]
  b = crt2cyl(a)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [0.0, 1.0, 0.0]
  b = crt2cyl(a)
  if (any(abs(b - [1.0, PI/2, 0.0]) > DELTA)) error stop 

  a = [0.0, 0.0, 1.0]
  b = crt2cyl(a)
  if (any(abs(b - [0.0, 0.0, 1.0]) > DELTA)) error stop 

  ! Cylindrical to cartesian 
  a = [0.0, 0.0, 0.0]
  b = cyl2crt(a)
  if (any(abs(b - [0.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, 0.0, 0.0]
  b = cyl2crt(a)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, PI/2, 0.0]
  b = cyl2crt(a)
  if (any(abs(b - [0.0, 1.0, 0.0]) > DELTA)) error stop 

  a = [0.0, 0.0, 1.0]
  b = cyl2crt(a)
  if (any(abs(b - [0.0, 0.0, 1.0]) > DELTA)) error stop 

end subroutine cylindrical_test_real32

subroutine cylindrical_test_real64 ()
  implicit none
  integer, parameter :: DIM = 3
  real, parameter :: PI = acos(-1.0)
  real(REAL64) :: a(DIM), b(DIM)

  ! Cartesian to cylindrical
  a = [0.0, 0.0, 0.0]
  b = crt2cyl(a)
  if (any(abs(b - [0.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, 0.0, 0.0]
  b = crt2cyl(a)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [0.0, 1.0, 0.0]
  b = crt2cyl(a)
  if (any(abs(b - [1.0, PI/2, 0.0]) > DELTA)) error stop 

  a = [0.0, 0.0, 1.0]
  b = crt2cyl(a)
  if (any(abs(b - [0.0, 0.0, 1.0]) > DELTA)) error stop 

  ! Cylindrical to cartesian 
  a = [0.0, 0.0, 0.0]
  b = cyl2crt(a)
  if (any(abs(b - [0.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, 0.0, 0.0]
  b = cyl2crt(a)
  if (any(abs(b - [1.0, 0.0, 0.0]) > DELTA)) error stop 

  a = [1.0, PI/2, 0.0]
  b = cyl2crt(a)
  if (any(abs(b - [0.0, 1.0, 0.0]) > DELTA)) error stop 

  a = [0.0, 0.0, 1.0]
  b = cyl2crt(a)
  if (any(abs(b - [0.0, 0.0, 1.0]) > DELTA)) error stop 

end subroutine cylindrical_test_real64

! Test vector functions.
subroutine coordinate_test_real32 ()
  implicit none
  integer, parameter :: DIM = 3
  real, parameter :: PI = acos(-1.0)
  real(REAL32) :: a(DIM), b(DIM), c(DIM), d(dim),  val

  a = [1., 1., 1.]
  b = [4., 1., 1.]
  val = distance(a, b)
  if (abs(val - 3.0) > DELTA) error stop

  a = [1., 0., 0.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]
  val = angle(a, b, c)
  if (abs(val - 0.0) > DELTA) error stop

  a = [1., 0., 0.]
  b = [0., 0., 0.]
  c = [0., 1., 0.]
  val = angle(a, b, c)
  if (abs(val - PI/2) > DELTA) error stop

  a = [1., 0., 0.]
  b = [0., 0., 0.]
  c = [-1., 0., 0.]
  val = angle(a, b, c)
  if (abs(val - PI) > DELTA) error stop

  ! Dihedral angle
  a = [0., 1., 0.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]  
  d = [1., 1., 0.]  
  val = dihedral(a, b, c, d)
  if (abs(val - 0.0) > DELTA) error stop

  a = [0., 0., 1.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]  
  d = [1., 1., 0.]  
  val = dihedral(a, b, c, d)
  if (abs(val - PI/2) > DELTA) error stop

  a = [0., -1., 0.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]  
  d = [1., 1., 0.]  
  val = dihedral(a, b, c, d)
  if (abs(val - PI) > DELTA) error stop

  a = [0., 0., -1.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]  
  d = [1., 1., 0.]  
  val = dihedral(a, b, c, d)
  if (abs(val - PI/2) > DELTA) error stop

end subroutine coordinate_test_real32

subroutine coordinate_test_real64 ()
  implicit none
  integer, parameter :: DIM = 3
  real, parameter :: PI = acos(-1.0)
  real(REAL64) :: a(DIM), b(DIM), c(DIM), d(dim),  val

  a = [1., 1., 1.]
  b = [4., 1., 1.]
  val = distance(a, b)
  if (abs(val - 3.0) > DELTA) error stop

  a = [1., 0., 0.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]
  val = angle(a, b, c)
  if (abs(val - 0.0) > DELTA) error stop

  a = [1., 0., 0.]
  b = [0., 0., 0.]
  c = [0., 1., 0.]
  val = angle(a, b, c)
  if (abs(val - PI/2) > DELTA) error stop

  a = [1., 0., 0.]
  b = [0., 0., 0.]
  c = [-1., 0., 0.]
  val = angle(a, b, c)
  if (abs(val - PI) > DELTA) error stop

  ! Dihedral angle
  a = [0., 1., 0.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]  
  d = [1., 1., 0.]  
  val = dihedral(a, b, c, d)
  if (abs(val - 0.0) > DELTA) error stop

  a = [0., 0., 1.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]  
  d = [1., 1., 0.]  
  val = dihedral(a, b, c, d)
  if (abs(val - PI/2) > DELTA) error stop

  a = [0., -1., 0.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]  
  d = [1., 1., 0.]  
  val = dihedral(a, b, c, d)
  if (abs(val - PI) > DELTA) error stop

  a = [0., 0., -1.]
  b = [0., 0., 0.]
  c = [1., 0., 0.]  
  d = [1., 1., 0.]  
  val = dihedral(a, b, c, d)
  if (abs(val - PI/2) > DELTA) error stop

end subroutine coordinate_test_real64

end program main