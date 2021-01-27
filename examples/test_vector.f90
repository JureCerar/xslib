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

#define __THISFILE__ "vector.F90"
! #define assert(x) assert_( x, __THISFILE__, __LINE__ )

program main
  use xslib_vector
  use xslib_error
  implicit none
  real, parameter     :: pi = acos(-1.0)
  real, dimension(3)  :: a, b, c, d
  real, allocatable   :: array(:)
  real                :: ave, var
  real                :: x, y
  real(8)             :: xd, yd
  character(1)        :: xc, yc
  integer             :: i, stat
  real, parameter     :: delta = 1.0e-3

  ! Write version
  ! write (*,*) "xslib "//xslibInfo()
  ! write (*,*) "" ! Empty line

  ! Cross-product
  a(:) = [ 1., 0., 0. ]
  b(:) = [ 0., 1., 0. ]
  c(:) = [ 0., 0., 1. ]
  call assert( cross(a,b)-c <= delta )
  write (*,*) "Cross:", cross( a, b )

  ! Rotate around z-axis by 90 deg
  a(:) = [ 1., 0., 0. ]
  b(:) = [ 0., 0., 1. ] ! z-axis
  c(:) = [ 0., 1., 0. ]
  call assert( rotate(a,b,pi/2.)-c <= delta )
  write (*,*) "Rotate:", rotate( a, b, pi/2. )
  call assert( rotate(a,"z",pi/2.)-c <= delta )
  write (*,*) "Rotate:", rotate( a, "z", pi/2. )

  ! Minimum image
  a(:) = [ 0.25,  0.75, -0.75 ]
  b(:) = [ 0.25, -0.25,  0.25 ]
  c(:) = [ 1.00,  1.00,  1.00 ] ! box
  call assert( all( minImg(a,c)-b <= delta ) )
  write (*,*) "minImg:", minImg(a,c)

  ! Vector distance
  a(:) = [ 0.0,  0.0,  0.0 ]
  b(:) = [ 1.0,  1.0,  1.0 ]
  call assert( getDistance(a,b)-sqrt(3.) <= delta )
  write (*,*) "getDistance:", getDistance(a,b)

  ! Vector angle
  a(:) = [ 1., 0., 0. ]
  b(:) = [ 0., 0., 0. ]
  c(:) = [ 0., 0., 1. ]
  call assert( getAngle(a,b,c)-(pi/2.) <= delta )
  write (*,*) "getAngle:", getAngle(a,b,c)

  ! Vector dihedral angle
  a(:) = [ 0., 1., 0. ]
  b(:) = [ 0., 0., 0. ]
  c(:) = [ 0., 0., 1. ]
  d(:) = [ 1., 0., 1. ]
  call assert( getDihedral(a,b,c,d)-(pi/2.) <= delta )
  write (*,*) "getDihedral:", getDihedral(a,b,c,d)

  ! Transform to spherical coordinate systems
  a(:) = [ 1., 1., 1. ]
  b(:) = [ 1.7320508075689, 0.95531661812451, 0.78539816339745 ]
  call assert( crt2sph(a)-b <= delta )
  write (*,*) "crt2sph:", crt2sph(a)
  call assert( sph2crt(b)-a <= delta )
  write (*,*) "sph2crt:", sph2crt(b)

  ! Transform to cylindrical coordinate systems
  a(:) = [ 1., 1., 1. ]
  b(:) = [ 1.4142135623731, 0.78539816339745, 1.0 ]
  call assert( crt2cyl(a)-b <= delta )
  write (*,*) "crt2cyl:", crt2cyl(a)
  call assert( cyl2crt(b)-a <= delta )
  write (*,*) "cyl2crt:", cyl2crt(b)

  ! Linear interpolation (exact)
  call assert( lerp(0.0,10.0,0.75) == 7.5 )
  write (*,*) "lerp:", lerp(0.0,10.0,0.75)
  call assert( lerp(0.0d0,10.0d0,0.75d0) == 7.5d0 )
  write (*,*) "lerp8:", lerp(0.0d0,10.0d0,0.75d0)

  ! On-line variance algorithm (exact)
  ! * sum of all numbers from 1 to 100
  ave = 0.000
  var = 0.000
  do i = 1, 100
    call variance( real(i), ave, var, i )
  end do
  var = merge( var/real(i-1), 0.000, i>1 )
  call assert( ave == 50.5 )
  call assert( var == 833.25 )
  write (*,*) "On-line variance:", ave, var

  ! Allocate and fill array.
  allocate( array(10), SOURCE=[(real(i),i=1,10)], STAT=stat )

  ! Find 3 closest numbers to 5.6
  ! * Should be correct order: 6,5,7 (exact)
  call assert( findKClosest(5.6,array(:),3) == [6.,5.,7.] )
  write (*,*) "Kind k-closests:", findKClosest(5.6,array(:),3)

  ! Swap numbers (exact)
  x = 1.0
  y = 2.0
  call swap(x,y)
  call assert( [x,y] == [2.0,1.0] )
  write (*,*) "Swap:", x, y

  xd = 1.0d0
  yd = 2.0d0
  call swap(xd,yd)
  call assert( [xd,yd] == [2.0d0,1.0d0] )
  write (*,*) "Swap8:", xd, yd

  xc = "a"
  yc = "z"
  call swap(xc,yc)
  call assert( [xc,yc] == ["z","a"] )
  write (*,*) "SwapC:", xc, yc

end program main
