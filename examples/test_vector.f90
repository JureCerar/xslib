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

program main
  use xslib_vector
  implicit none
  real, parameter     :: pi = acos(-1.0)
  real, dimension(3)  :: a, b, c, d
  real, allocatable   :: array(:)
  real                :: x, y, ave, err
  integer             :: i, stat

  ! Define vectors
  a(:) = [ 0., 1., 0. ]
  b(:) = [ 0., 0., 0. ]
  c(:) = [ 1., 0., 0. ]
  d(:) = [ 1., 0., 1. ]

  ! Cross-product
  write (*,*) "Cross:"
  write (*,*) cross( a, c )

  ! Rotate around z-axis by 90 deg
  write (*,*) "Rotate:"
  write (*,*) rotate( a, [0.,0.,1.], pi/2. )

  ! 'Coordinate vector operations
  write (*,*) "Coor-vector:"
  write (*,*) minImg( a, [0.75,0.75,0.75] )
  write (*,*) getDistance( a, b )
  write (*,*) getAngle( a, b, c )
  write (*,*) getDihedral( a, b, c, d )

  ! Transform coordinate systems
  write (*,*) "crt2sph & sph2crt:"
  a(:) = [ 0., 1., 0. ]
  a(:) = crt2sph( a )
  write (*,*) a(:)
  a(:) = sph2crt( a )
  write (*,*) a(:)

  write (*,*) "crt2cyl & cyl2crt:"
  a(:) = [ 0., 1., 0. ]
  a(:) = crt2cyl( a )
  write (*,*) a(:)
  a(:) = cyl2crt( a )
  write (*,*) a(:)

  ! Linear interpolation
  write (*,*) lerp( 0.0, 10.0, 0.5 )
  write (*,*) lerp( 0.0d0, 10.0d0, 0.5d0 )

  ! On-line variance
  write (*,*) "On-line variance:"
  ave = 0.000
  err = 0.000
  do i = 1, 100
    call variance( real(i), ave, err, i )
  end do

  ! Transform to true variance
  err = merge( err/(i-1), 0.000, i>1 )
  write (*,*) ave, "+-", err

  ! Allocate and fill array.
  allocate( array(10), SOURCE=[(real(i),i=1,10)], STAT=stat )

  ! Find 3 closest numbers to 5.5
  write (*,*) "Kind k-closests:"
  write (*,*) 5.5
  write (*,*) findKClosest( 5.5, array(:), 3 )

  ! Swap numbers
  write (*,*) "Swap:"
  x = 1.000
  y = 2.000
  write (*,*) x, y
  call swap( x, y )
  write (*,*) x, y

end program main
