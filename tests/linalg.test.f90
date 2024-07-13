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
    integer, parameter :: N = 3
    real :: v(N), m(N,N), w(N,N-1), mat(N,N)
    real :: eigval(N), eigvec(N,N)
    integer :: i

    mat(1,:) = [1.0,  2.0,  4.0]
    mat(2,:) = [2.0,  2.0, -2.0]
    mat(3,:) = [3.0, -2.0,  4.0]

    ! Determinant
    if (abs(det(mat) + 64.0) > DELTA) error stop 

    ! Diagonal
    v = [1, 2, 4]
    if (any((diag(mat) - v) > DELTA)) error stop 

    ! Eigenvalue & Eigenvector
    call eig(mat, eigval, eigvec)
    v = [-2.86867160,  3.50685835,  6.36181324]
    if (any((eigval - v) > DELTA)) error stop 
    m(1,:) = [ 0.73544981, -0.56063186,  0.56617560]
    m(2,:) = [-0.49304284, -0.82577585, -0.11467404]
    m(3,:) = [-0.46478203,  0.06153176,  0.81626898] 
    if (any((eigvec - m) > DELTA)) error stop
    
    ! Eigenvalue
    eigval = eigvals(mat)
    v = [-2.86867160,  3.50685835,  6.36181324]
    if (any((eigval - v) > DELTA)) error stop 

    ! Inversion
    m(1,:) = [-0.06250,  0.25000,  0.18750]
    m(2,:) = [ 0.21875,  0.12500, -0.15625]
    m(3,:) = [ 0.15625, -0.12500,  0.03125]
    if (any((inv(mat) - m) > DELTA)) error stop
    
    ! is_diagonal
    m(1,:) = [1., 0., 0.]
    m(2,:) = [0., 1., 0.]
    m(3,:) = [0., 0., 1.]
    if (.not. is_diagonal(m)) error stop
    m(3, 1) = 1.0
    if (is_diagonal(m)) error stop

    ! is_square
    if (.not. is_square(m)) error stop
    if (is_square(w)) error stop

    ! is_symmetric
    m(1,:) = [1., 2., 3.]
    m(2,:) = [2., 1., 2.]
    m(3,:) = [3., 2., 1.]
    if (.not. is_symmetric(m)) error stop 
    m(3, 1) = 1.0
    if (is_symmetric(m)) error stop 

end subroutine matrix_test_real32    


subroutine matrix_test_real64 ()
    implicit none
    integer, parameter :: N = 3
    double precision :: v(N), m(N,N), w(N,N-1), mat(N,N)
    double precision :: eigval(N), eigvec(N,N)
    integer :: i

    mat(1,:) = [1.0,  2.0,  4.0]
    mat(2,:) = [2.0,  2.0, -2.0]
    mat(3,:) = [3.0, -2.0,  4.0]

    ! Determinant
    if (abs(det(mat) + 64.0) > DELTA) error stop 

    ! Diagonal
    v = [1, 2, 4]
    if (any((diag(mat) - v) > DELTA)) error stop 

    ! Eigenvalue & Eigenvector
    call eig(mat, eigval, eigvec)
    v = [-2.86867160,  3.50685835,  6.36181324]
    if (any((eigval - v) > DELTA)) error stop 
    m(1,:) = [ 0.73544981, -0.56063186,  0.56617560]
    m(2,:) = [-0.49304284, -0.82577585, -0.11467404]
    m(3,:) = [-0.46478203,  0.06153176,  0.81626898] 
    if (any((eigvec - m) > DELTA)) error stop
    
    ! Eigenvalue
    eigval = eigvals(mat)
    v = [-2.86867160,  3.50685835,  6.36181324]
    if (any((eigval - v) > DELTA)) error stop 

    ! Inversion
    m(1,:) = [-0.06250,  0.25000,  0.18750]
    m(2,:) = [ 0.21875,  0.12500, -0.15625]
    m(3,:) = [ 0.15625, -0.12500,  0.03125]
    if (any((inv(mat) - m) > DELTA)) error stop
    
    ! is_diagonal
    m(1,:) = [1., 0., 0.]
    m(2,:) = [0., 1., 0.]
    m(3,:) = [0., 0., 1.]
    if (.not. is_diagonal(m)) error stop
    m(3, 1) = 1.0
    if (is_diagonal(m)) error stop

    ! is_square
    if (.not. is_square(m)) error stop
    if (is_square(w)) error stop

    ! is_symmetric
    m(1,:) = [1., 2., 3.]
    m(2,:) = [2., 1., 2.]
    m(3,:) = [3., 2., 1.]
    if (.not. is_symmetric(m)) error stop 
    m(3, 1) = 1.0
    if (is_symmetric(m)) error stop 

end subroutine matrix_test_real64    

end program main    
