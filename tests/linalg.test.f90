! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2022 Jure Cerar
!
! This file is part of xslib
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
    use xslib_linalg
    implicit none
    real, parameter :: DELTA = 0.01

    call cross_test_real32 ()
    call cross_test_real64 ()

    call matrix_test_real32 ()
    call matrix_test_real64 ()

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


! Test some matrix operations
subroutine matrix_test_real32 ()
    implicit none
    integer, parameter :: N = 4
    real :: v(N), m(N,N), w(N,N-1), mat(N,N)
    real :: eigval(N), eigvec(N,N)

    mat(1,:) = [    4,   -30,    60,   -35]
    mat(2,:) = [  -30,   300,  -675,   420]
    mat(3,:) = [   60,  -675,  1620, -1050]
    mat(4,:) = [  -35,   420, -1050,   700]

    ! Determinant
    if (abs(det(mat) - 23625) > DELTA) error stop 

    ! Diagonal
    v = [4, 300, 1620, 700]
    if (any((diag(mat) - v) > DELTA)) error stop 

    ! Eigenvalue & Eigenvector
    call eig(mat, eigval, eigvec)
    v = [ 0.16664286,  1.47805480,  37.1014910,  2585.25380]

    if (any((eigval - v) > DELTA)) error stop 
    m(1,:) = [0.792608,  0.582076,  0.179186,  0.0291933]
    m(2,:) = [0.451923, -0.370502, -0.741918, -0.328712 ]
    m(3,:) = [0.322416, -0.509579,  0.100228,  0.791411 ]
    m(4,:) = [0.252161, -0.514048,  0.638283, -0.514553 ]
    if (any((eigvec - m) > DELTA)) error stop
    
    ! Eigenvalue
    eigval = eigvals(mat)
    v = [ 0.16664286,  1.47805480,  37.1014910,  2585.25380]
    if (any((eigval - v) > DELTA)) error stop 

    ! Inversion
    m(1,:) = [4.0000, 2.0000, 1.3333, 1.0000]
    m(2,:) = [2.0000, 1.3333, 1.0000, 0.8000]
    m(3,:) = [1.3333, 1.0000, 0.8000, 0.6667]
    m(4,:) = [1.0000, 0.8000, 0.6667, 0.5714]
    if (any((inv(mat) - m) > DELTA)) error stop
    
    ! is_diagonal
    m(1,:) = [1, 0, 0, 0]
    m(2,:) = [0, 1, 0, 0]
    m(3,:) = [0, 0, 1, 0]
    m(4,:) = [0, 0, 0, 1]
    if (.not. is_diagonal(m)) error stop
    m(4, 1) = 1
    if (is_diagonal(m)) error stop

    ! is_square
    if (.not. is_square(m)) error stop
    if (is_square(w)) error stop

    ! is_symmetric
    m(1,:) = [1, 2, 3, 4]
    m(2,:) = [2, 1, 2, 3]
    m(3,:) = [3, 2, 1, 2]
    m(4,:) = [4, 3, 2, 1]
    if (.not. is_symmetric(m)) error stop 
    m(4, 1) = 1
    if (is_symmetric(m)) error stop 

end subroutine matrix_test_real32    


subroutine matrix_test_real64 ()
    implicit none
    integer, parameter :: N = 4
    real(REAL64) :: v(N), m(N,N), w(N,N-1), mat(N,N)
    real(REAL64) :: eigval(N), eigvec(N,N)

    mat(1,:) = [    4,   -30,    60,   -35]
    mat(2,:) = [  -30,   300,  -675,   420]
    mat(3,:) = [   60,  -675,  1620, -1050]
    mat(4,:) = [  -35,   420, -1050,   700]

    ! Determinant
    if (abs(det(mat) - 23625) > DELTA) error stop 

    ! Diagonal
    v = [4, 300, 1620, 700]
    if (any((diag(mat) - v) > DELTA)) error stop 

    ! Eigenvalue & Eigenvector
    call eig(mat, eigval, eigvec)
    v = [ 0.16664286,  1.47805480,  37.1014910,  2585.25380]

    if (any((eigval - v) > DELTA)) error stop 
    m(1,:) = [0.792608,  0.582076,  0.179186,  0.0291933]
    m(2,:) = [0.451923, -0.370502, -0.741918, -0.328712 ]
    m(3,:) = [0.322416, -0.509579,  0.100228,  0.791411 ]
    m(4,:) = [0.252161, -0.514048,  0.638283, -0.514553 ]
    if (any((eigvec - m) > DELTA)) error stop
    
    ! Eigenvalue
    eigval = eigvals(mat)
    v = [ 0.16664286,  1.47805480,  37.1014910,  2585.25380]
    if (any((eigval - v) > DELTA)) error stop 

    ! Inversion
    m(1,:) = [4.0000, 2.0000, 1.3333, 1.0000]
    m(2,:) = [2.0000, 1.3333, 1.0000, 0.8000]
    m(3,:) = [1.3333, 1.0000, 0.8000, 0.6667]
    m(4,:) = [1.0000, 0.8000, 0.6667, 0.5714]
    if (any((inv(mat) - m) > DELTA)) error stop
    
    ! is_diagonal
    m(1,:) = [1, 0, 0, 0]
    m(2,:) = [0, 1, 0, 0]
    m(3,:) = [0, 0, 1, 0]
    m(4,:) = [0, 0, 0, 1]
    if (.not. is_diagonal(m)) error stop
    m(4, 1) = 1
    if (is_diagonal(m)) error stop

    ! is_square
    if (.not. is_square(m)) error stop
    if (is_square(w)) error stop

    ! is_symmetric
    m(1,:) = [1, 2, 3, 4]
    m(2,:) = [2, 1, 2, 3]
    m(3,:) = [3, 2, 1, 2]
    m(4,:) = [4, 3, 2, 1]
    if (.not. is_symmetric(m)) error stop 
    m(4, 1) = 1
    if (is_symmetric(m)) error stop 

end subroutine matrix_test_real64    

end program main    
