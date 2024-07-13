! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2024 Jure Cerar
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

module xslib_linalg
    use iso_fortran_env, only: REAL32, REAL64
    implicit none
    private
    public :: cross, det, diag, eig, eigvals, eye, inv, is_square, is_symmetric, is_diagonal

    ! %%%
    ! # `LINALG` - Linear Algebra
    !   Module `xslib_linalg` contains function for basic linear algebra operations. Supports both single 
    !   and double precision (`DP`). If you are planning on doing _serious_ linear algebra, please use
    !   [LAPACK](https://www.netlib.org/lapack/) or similar library instead!
    ! %%%

    interface cross
        module procedure :: cross_real32, cross_real64
    end interface cross

    interface det
        module procedure :: det_real32, det_real64
    end interface det   
    
    interface diag
        module procedure :: diag_real32, diag_real64
    end interface diag  

    interface eig
        module procedure :: eig_real32, eig_real64
    end interface eig  

    interface eigvals
        module procedure :: eigvals_real32, eigvals_real64
    end interface eigvals  
            
    interface inv
        module procedure :: inv_real32, inv_real64
    end interface inv  

    interface is_diagonal
        module procedure :: is_diagonal_real32, is_diagonal_real64
    end interface is_diagonal  

    interface is_square
        module procedure :: is_square_real32, is_square_real64
    end interface is_square

    interface is_symmetric
        module procedure :: is_symmetric_real32, is_symmetric_real64
    end interface is_symmetric  

    ! interface solve
    !     module procedure :: solve_real32, solve_real64
    ! end interface solve  

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


recursive function det_real32 (mat) result (out)
    ! %%%
    ! ## `DET` - Matrix Determinant
    ! #### DESCRIPTION
    !   This function computes the determinant of a real square matrix.
    ! #### USAGE
    !   ```Fortran
    !   out = det(mat)
    !   ```
    ! #### PARAMETERS
    !   * `real(ANY), dimension(DIM,DIM), intent(IN) :: mat`
    !     Input matrix.
    !   * `real(ANY) :: out`
    !     Determinant of the matrix.
    ! #### EXAMPLE
    !   ```Fortran
    !   > det(reshape([1., 2., 3., 4.], [2, 2]))
    !   -2.0
    !   ```
    ! %%%
    implicit none
    real, intent(IN) :: mat(:, :)
    real :: out
    integer :: i, n, sgn
    real :: submat(size(mat, 1)-1, size(mat, 2)-1)

    n = size(mat, 1)
    if ( n == 1 ) then
        out = mat(1,1)
    else
        out = 0.0
        sgn = 1
        do i = 1, n
            submat( 1:n-1, 1:i-1 ) = mat( 2:n, 1:i-1 )
            submat( 1:n-1, i:n-1 ) = mat( 2:n, i+1:n )
            out = out + sgn * mat(1, i) * det_real32(submat)
            sgn = - sgn
        end do
    end if

end function det_real32


recursive function det_real64 (mat) result (out)
    implicit none
    real(REAL64), intent(IN) :: mat(:, :)
    real(REAL64) :: out
    integer :: i, n, sgn
    real(REAL64) :: submat(size(mat, 1)-1, size(mat, 1)-1)

    n = size(mat, 1)
    if ( n == 1 ) then
        out = mat(1,1)
    else
        out = 0.0
        sgn = 1
        do i = 1, n
            submat( 1:n-1, 1:i-1 ) = mat( 2:n, 1:i-1 )
            submat( 1:n-1, i:n-1 ) = mat( 2:n, i+1:n )
            out = out + sgn * mat(1, i) * det_real64(submat)
            sgn = - sgn
        end do
    end if

end function det_real64


function diag_real32 (mat) result (out)
    ! %%%
    ! ## `DIAG` - Diagonal Matrix
    ! #### DESCRIPTION
    !   Extract the diagonal elements of an matrix.
    ! #### USAGE
    !   ```Fortran
    !   out = diag(mat)
    !   ```
    ! #### PARAMETERS
    !   * `real(ANY), dimension(DIM,DIM), intent(IN) :: mat`
    !     Input matrix.
    !   * `real(ANY), dimension(DIM) :: out`
    !     Diagonal of a matrix.
    ! #### EXAMPLE
    !   ```Fortran
    !   > diag(reshape([real(i), i = 1, 9], [3, 3]))
    !   [1.0, 4.0, 9.0]
    !   ```
    ! %%%
    implicit none
    real, intent(IN) :: mat(:,:)
    real :: out(size(mat, 1))
    integer :: i

    do i = 1, size(mat, 1)
        out(i) = mat(i, i)
    end do

end function diag_real32


function diag_real64 (mat) result (out)
    implicit none
    real(REAL64), intent(IN) :: mat(:,:)
    real(REAL64) :: out(size(mat, 1))
    integer :: i

    do i = 1, size(mat, 1)
        out(i) = mat(i, i)
    end do

end function diag_real64


subroutine eig_real32 (mat, lambda, vector)
    ! %%%
    ! ## `EIG` - Calculate eigenvector and eigenvalue
    ! #### DESCRIPTION
    !   Compute the eigenvalues and right eigenvectors of a square array, where:
    !   `A⋅v = λ⋅v`, where `A` is a square, full-rank, real or complex matrix.
    ! #### USAGE
    !   ```Fortran
    !   call eig(mat, lambda, vector)
    !   ```
    ! #### PARAMETERS
    !   * `real(ANY), dimension(DIM,DIM), intent(IN) :: mat`
    !     Input matrix.
    !   * `real(ANY), dimension(DIM), intent(OUT) :: lambda`
    !     Eigenvalue of the input matrix.
    !   * `real(ANY), dimension(DIM,DIM), intent(OUT) :: vector`
    !     Eigenvector of the input matrix.
    ! #### EXAMPLE
    !   ```Fortran
    !   > mat = [[2, 2, 4], [1, 3, 5], [2, 3, 4]]
    !   > call eig(mat, lambda, vector)
    !   > print *, lambda
    !   [ 8.80916362,  0.92620912, -0.73537273]
    !   > print *, vector
    !   [[-0.52799324, -0.77557092, -0.36272811],
    !    [-0.60439100,  0.62277013, -0.71032620],
    !    [-0.59660259, -0.10318482,  0.60321224]]
    !   ```
    ! %%%
    implicit none
    real, intent(IN) :: mat(:,:)
    real, intent(OUT) :: vector(size(mat, 1), size(mat, 1))
    real, intent(OUT) :: lambda(size(mat, 1))
    real(REAL64), allocatable :: a(:,:), v(:,:), d(:)
    integer, parameter :: it_max = 100
    integer :: n, it_num, rot_num

    n = size(mat, 1)
    if (n /= size(mat, 2)) error stop "Input is not square matrix"

    allocate(v(n, n), d(n))
    allocate(a(n, n), SOURCE=real(mat, kind=REAL64))

    call jacobi_eigenvalue (n, a, v, d, it_max, it_num, rot_num)

    vector = real(v, REAL32)
    lambda = real(d, REAL32)

end subroutine eig_real32


subroutine eig_real64 (mat, lambda, vector)
    implicit none
    real(REAL64), intent(IN) :: mat(:,:)
    real(REAL64), intent(OUT) :: vector(size(mat, 1), size(mat, 1))
    real(REAL64), intent(OUT) :: lambda(size(mat, 1))
    real(REAL64), allocatable :: a(:,:)
    integer, parameter :: it_max = 100
    integer :: n, it_num, rot_num

    n = size(mat, 1)
    if (n /= size(mat, 2)) error stop "Input is not square matrix"

    allocate(a(n, n), SOURCE=mat)

    call jacobi_eigenvalue (n, a, vector, lambda, it_max, it_num, rot_num)

end subroutine eig_real64


function eigvals_real32 (mat) result (out)
    ! %%%
    ! ## `EIGVALS` - Matrix eigenvalue
    ! #### DESCRIPTION
    !   Compute the eigenvalues of a square array. Main difference 
    !   between `eigvals` and `eig`: the eigenvectors aren’t returned.
    ! #### USAGE
    !   ```Fortran
    !   out = eigvals(mat)
    !   ```
    ! #### PARAMETERS
    !   * `real(ANY), dimension(DIM,DIM), intent(IN) :: mat`
    !     Input matrix.
    !   * `real(ANY), dimension(DIM) :: out`
    !     Eigenvalue of the input matrix.
    ! #### EXAMPLE
    !   ```Fortran
    !   > mat = [[2, 2, 4], [1, 3, 5], [2, 3, 4]]
    !   > eigvals(mat)
    !   [ 8.80916362,  0.92620912, -0.73537273]
    !   ```
    ! %%%
    implicit none
    real, intent(IN) :: mat(:,:)
    real :: out(size(mat, 1))
    real(REAL64), allocatable :: a(:,:), v(:,:), d(:)
    integer, parameter :: it_max = 1000
    integer :: n, it_num, rot_num

    n = size(mat, 1)
    if (n /= size(mat, 2)) error stop "Input is not square matrix"

    allocate(v(n, n), d(n))
    allocate(a(n, n), SOURCE=real(mat, kind=REAL64))

    call jacobi_eigenvalue (n, a, v, d, it_max, it_num, rot_num)

    out = real(d, REAL32)

end function eigvals_real32


function eigvals_real64 (mat) result (out)
    implicit none
    real(REAL64), intent(IN) :: mat(:,:)
    real(REAL64) :: out(size(mat, 1))
    real(REAL64), allocatable  :: a(:,:)
    real(REAL64):: v(size(mat, 1), size(mat, 1))
    integer, parameter :: it_max = 10000
    integer :: n, it_num, rot_num

    n = size(mat, 1)
    allocate(a(n, n))
    a = mat
    
    call jacobi_eigenvalue (n, a, v, out, it_max, it_num, rot_num)

end function eigvals_real64


function eye (n, m) result (out)
    ! %%%
    ! ## `EYE` - Construct the identity matrix
    ! #### DESCRIPTION
    !   Construct the identity matrix i.e. matrix with ones 
    !   on the main diagonal and zeros elsewhere.
    ! #### USAGE
    !   ```Fortran
    !   out = eye(n, m)
    !   ```
    ! #### PARAMETERS
    !   * `integer, intent(IN) :: n, m`
    !     Dimension of the matrix.
    !   * `real(ANY), dimension(N, M) :: out`
    !     Identity matrix.
    ! #### EXAMPLE
    !   ```Fortran
    !   > eye(3, 3)
    !   [[ 1.0,  0.0,  0.0],
    !    [ 0.0,  1.0,  0.0],
    !    [ 0.0,  0.0,  1.0]]
    !   ```
    ! %%%
    implicit none
    integer, intent(IN) :: n, m
    real :: out(n, m)
    integer :: i

    out = 0.0
    do i = 1, min(n, m)
        out(i, i) = 1.0
    end do

end function eye


function inv_real32 (mat) result (out)
    ! %%%
    ! ## `INV` - Inversion of a square matrix
    ! #### DESCRIPTION
    !   Compute the inverse of a matrix. Given a square matrix A,
    !   return the matrix A<sup>-1</sup> satisfying a ∙ A<sup>-1</sup> = I, where
    !   I is identity matrix
    ! #### USAGE
    !   ```Fortran
    !   out = inv(mat)
    !   ```
    ! #### PARAMETERS
    !   * `integer, dimension(DIM,DIM), intent(IN) :: mat`
    !     Input matrix.
    !   * `integer, dimension(DIM,DIM) :: out`
    !     Inverse of matrix.
    ! #### EXAMPLE
    !   ```Fortran
    !   > inv([[2., 2., 4.], [1., 3., 5.], [2., 3., 4.]])
    !   [[ 0.5000, -0.6667,  0.3334],
    !    [-1.0000,  0.0000,  1.0000],
    !    [ 0.5000,  0.3334, -0.6667]]
    !   ```
    ! %%%
    ! SOURCE
    !   See: https://ww2.odu.edu/~agodunov/computing/programs/book2/Ch06/Inverse.f90
    implicit none
    real, intent(INOUT) :: mat(:,:)
    real :: out(size(mat, 1), size(mat, 2))
    real, allocatable :: L(:,:), U(:,:), b(:), d(:), x(:)
    real :: coeff
    integer :: np, i, j, k

    np = size(mat, 1)
    if (np /= size(mat, 2)) error stop "Input is not square matrix"

    ! step 0: Initialization 
    allocate (L(np,np), U(np,np), b(np), d(np), x(np))
    out = mat
    L = 0.0
    U = 0.0
    b = 0.0

    ! step 1: Forward elimination
    do k = 1, np-1
        do i = k+1, np
            coeff = out(i,k) / out(k,k)
            L(i,k) = coeff
            do j = k+1, np
                out(i,j) = out(i,j) - coeff * out(k,j)
            end do
        end do
    end do

    ! Step 2: prepare L and U matrices 
    do i = 1, np
        L(i,i) = 1.0
    end do
    do j = 1, np
        do i = 1, j
            U(i,j) = out(i,j)
        end do
    end do

    ! Step 3: compute columns of the inverse matrix C
    do k = 1, np
        b(k) = 1.0
        d(1) = b(1)
        do i = 2, np
            d(i) = b(i)
            do j = 1, i-1
                d(i) = d(i) - L(i,j) * d(j)
            end do
        end do
        x(np) = d(np) / U(np,np)
        do i = np-1, 1, -1
            x(i) = d(i)
            do j= np, i+1, -1
                x(i) = x(i) - U(i,j) * x(j)
            end do
            x(i) = x(i) / u(i,i)
        end do
        do i = 1, np
            out(i,k) = x(i)
        end do
        b(k) = 0.0
    end do

end function inv_real32


function inv_real64 (mat) result (out)
    implicit none
    real(REAL64), intent(INOUT) :: mat(:,:)
    real(REAL64) :: out(size(mat, 1), size(mat, 2))
    real(REAL64), allocatable :: L(:,:), U(:,:), b(:), d(:), x(:)
    real(REAL64) :: coeff
    integer :: np, i, j, k

    np = size(mat, 1)
    if (np /= size(mat, 2)) error stop "Input is not square matrix"

    allocate (L(np,np), U(np,np), b(np), d(np), x(np))
    out = mat
    L = 0.0d0
    U = 0.0d0
    b = 0.0d0

    do k = 1, np-1
        do i = k+1, np
            coeff = out(i,k) / out(k,k)
            L(i,k) = coeff
            do j = k+1, np
                out(i,j) = out(i,j) - coeff * out(k,j)
            end do
        end do
    end do

    do i = 1, np
        L(i,i) = 1.0
    end do
    do j = 1, np
        do i = 1, j
            U(i,j) = out(i,j)
        end do
    end do

    do k = 1, np
        b(k) = 1.0
        d(1) = b(1)
        do i = 2, np
            d(i) = b(i)
            do j = 1, i-1
                d(i) = d(i) - L(i,j) * d(j)
            end do
        end do
        x(np) = d(np) / U(np,np)
        do i = np-1, 1, -1
            x(i) = d(i)
            do j= np, i+1, -1
                x(i) = x(i) - U(i,j) * x(j)
            end do
            x(i) = x(i) / u(i,i)
        end do
        do i = 1, np
            out(i,k) = x(i)
        end do
        b(k) = 0.0d0
    end do

end function inv_real64


function is_diagonal_real32 (mat) result (out)
    ! %%%
    ! ## `IS_DIAGONAL` - Check if matrix is diagonal
    ! #### DESCRIPTION
    !   Returns `.true.` if the input matrix is diagonal, and `.false.` otherwise. 
    ! #### USAGE
    !   ```Fortran
    !   out = is_diagonal(mat)
    !   ```
    ! #### PARAMETERS
    !   * `integer, dimension(DIM,DIM), intent(IN) :: mat`  
    !     Input matrix.
    !   * `logical :: out`  
    !     Is matrix diagonal?
    ! #### EXAMPLE
    !   ```Fortran
    !   > is_diagonal(reshape([1., 0., 0., 4.], [2, 2]))
    !   .True.
    !   > is_diagonal(reshape([1., 0., 3., 4.], [2, 2]))
    !   .False.
    !   ```
    ! %%%
    implicit none
    real, intent(IN) :: mat(:,:)
    logical :: out
    integer :: i, j

    out = .True.
    do i = 1, size(mat, 1)
        do j = 1, size(mat, 2)
            if (i == j) cycle
            if (mat(i,j) /= 0.0) then
                out = .False.
                return
            end if
        end do
    end do

end function is_diagonal_real32


function is_diagonal_real64 (mat) result (out)
    implicit none
    real(REAL64), intent(IN) :: mat(:,:)
    logical :: out
    integer :: i, j

    out = .True.
    do i = 1, size(mat, 1)
        do j = 1, size(mat, 2)
            if (i == j) cycle
            if (mat(i,j) /= 0.0d0) then
                out = .False.
                return
            end if
        end do
    end do

end function is_diagonal_real64


function is_square_real32 (mat) result (out)
    ! %%%
    ! ## `IS_SQUARE` - Check if matrix is square
    ! #### DESCRIPTION
    !   Returns `.true.` if the input matrix is square, and `.false.` otherwise. 
    ! #### USAGE
    !   ```Fortran
    !   out = is_square(mat)
    !   ```
    ! #### PARAMETERS
    !   * `integer, dimension(DIM,DIM), intent(IN) :: mat`  
    !     Input matrix.
    !   * `logical :: out`  
    !     Is matrix square?
    ! #### EXAMPLE
    !   ```Fortran
    !   > is_diagonal(reshape([1., 2., 3., 4.], [2, 2]))
    !   .True.
    !   > is_diagonal(reshape([1., 2., 3., 4., 5., 6.], [3, 2]))
    !   .False.
    !   ```
    ! %%%
    implicit none
    real, intent(IN) :: mat(:,:)
    logical :: out

    out = (size(mat, 1) == size(mat, 2))

end function is_square_real32


function is_square_real64 (mat) result (out)
    implicit none
    real(REAL64), intent(IN) :: mat(:,:)
    logical :: out

    out = (size(mat, 1) == size(mat, 2))

end function is_square_real64


function is_symmetric_real32 (mat) result (out)
    ! %%%
    ! ## `IS_SQUARE` - Check if matrix is symmetric
    ! #### DESCRIPTION
    !   Returns `.true.` if the input matrix is symmetric, and `.false.` otherwise. 
    ! #### USAGE
    !   ```Fortran
    !   out = is_symmetric(mat)
    !   ```
    ! #### PARAMETERS
    !   * `integer, dimension(DIM,DIM), intent(IN) :: mat`  
    !     Input matrix.
    !   * `logical :: out`  
    !     Is matrix symmetric?
    ! #### EXAMPLE
    !   ```Fortran
    !   > is_diagonal(reshape([1., 0., 0., 4.], [2, 2]))
    !   .True.
    !   > is_diagonal(reshape([1., 0., 3., 4.], [2, 2]))
    !   .False.
    !   ```
    ! %%%
    implicit none
    real, intent(IN) :: mat(:,:)
    logical :: out
    integer :: i, j

    out = .True.
    do i = 1, size(mat, 1)
        do j = i+1, size(mat, 2)
            if (mat(i,j) /= mat(j,i) ) then
                out = .False.
                return
            end if
        end do
    end do

end function is_symmetric_real32


function is_symmetric_real64 (mat) result (out)
    implicit none
    real(REAL64), intent(IN) :: mat(:,:)
    logical :: out
    integer :: i, j

    out = .True.
    do i = 1, size(mat, 1)
        do j = i+1, size(mat, 2)
            if (mat(i,j) /= mat(j,i) ) then
                out = .False.
                return
            end if
        end do
    end do

end function is_symmetric_real64


subroutine jacobi_eigenvalue (n, a, v, d, it_max, it_num, rot_num)
    ! SYNOPSIS
    !   JACOBI_EIGENVALUE carries out the Jacobi eigenvalue iteration.
    ! DESCRIPTION
    !   This function computes the eigenvalues and eigenvectors of a
    !   real symmetric matrix, using Rutishauser's modfications of the classical
    !   Jacobi rotation method with threshold pivoting.
    ! PARAMETERS
    !   n = integer: the order of the matrix.
    !   mat = real: (NxN) the matrix, which must be square, real, and symmetric.
    !   vector = real: (NxN) the matrix of eigenvectors.
    !   lambda = real: (N) the eigenvalues, in descending order.
    !   max_it = integer: the maximum number of iterations.
    !   it_num = integer: the total number of iterations.
    !   rot_num = integer: the total number of rotations.
    ! SOURCE
    !   John Burkardt, Sep 2013
    !   https://people.sc.fsu.edu/~jburkardt/f_src/jacobi_eigenvalue/jacobi_eigenvalue.html
    implicit none
    integer, intent(IN) :: n
    real(REAL64), intent(INOUT) :: a(n,n) 
    real(REAL64), intent(OUT) :: v(n,n) 
    real(REAL64), intent(OUT) :: d(n)
    integer, intent(IN) :: it_max
    integer, intent(OUT) :: it_num
    integer, intent(OUT) :: rot_num
    ! ------
    real(REAL64) :: bw(n), c, g, gapq, h
    real(REAL64) :: s, t, tau, term, thresh, termp, termq, theta, w(n), zw(n)
    integer :: i, j, k, l, m, p, q

    do j = 1, n
        do i = 1, n
            v(i,j) = 0.0
        end do
        v(j,j) = 1.0
    end do

    do i = 1, n
        d(i) = a(i,i)
    end do

    bw(1:n) = d(1:n)
    zw(1:n) = 0.0
    it_num = 0
    rot_num = 0

    do while (it_num < it_max)

        it_num = it_num + 1

        ! The convergence threshold is based on the size of the elements in
        ! the strict upper triangle of the matrix.
        thresh = 0.0
        do j = 1, n
            do i = 1, j - 1
                thresh = thresh + a(i,j) ** 2
            end do
        end do

        thresh = sqrt(thresh) / real (4 * n, kind=kind(thresh))

        if (thresh == 0.0) then
            exit 
        end if

        do p = 1, n
            do q = p + 1, n
                gapq = 10.0 * abs (a(p,q))
                termp = gapq + abs (d(p))
                termq = gapq + abs (d(q))

                if ( 4 < it_num .and. termp == abs (d(p)) .and. termq == abs(d(q))) then
                    !  Annihilate tiny off-diagonal elements.
                    a(p,q) = 0.0

                    
                else if ( thresh <= abs ( a(p,q) ) ) then
                    !  Otherwise, apply a rotation.
                    h = d(q) - d(p)
                    term = abs(h) + gapq

                    if ( term == abs ( h ) ) then
                        t = a(p,q) / h
                    else
                        theta = 0.5 * h / a(p,q)
                        t = 1.0 / (abs(theta)+sqrt(1.0 + theta * theta))
                        if (theta < 0.0) then 
                            t = - t
                        end if
                    end if

                    c = 1.0 / sqrt (1.0 + t * t)
                    s = t * c
                    tau = s / (1.0 + c)
                    h = t * a(p,q)

                    ! Accumulate corrections to diagonal elements.
                    zw(p) = zw(p) - h                  
                    zw(q) = zw(q) + h
                    d(p) = d(p) - h
                    d(q) = d(q) + h

                    a(p,q) = 0.0

                    ! Rotate, using information from the upper triangle of A only.
                    do j = 1, p - 1
                        g = a(j,p)
                        h = a(j,q)
                        a(j,p) = g - s * (h + g * tau)
                        a(j,q) = h + s * (g - h * tau)
                    end do

                    do j = p + 1, q - 1
                        g = a(p,j)
                        h = a(j,q)
                        a(p,j) = g - s * (h + g * tau)
                        a(j,q) = h + s * (g - h * tau)
                    end do

                    do j = q + 1, n
                        g = a(p,j)
                        h = a(q,j)
                        a(p,j) = g - s * (h + g * tau)
                        a(q,j) = h + s * (g - h * tau)
                    end do

                    !  Accumulate information in the eigenvector matrix.
                    do j = 1, n
                        g = v(j,p)
                        h = v(j,q)
                        v(j,p) = g - s * (h + g * tau)
                        v(j,q) = h + s * (g - h * tau)
                    end do

                    rot_num = rot_num + 1

                end if

            end do
        end do

        bw(1:n) = bw(1:n) + zw(1:n)
        d(1:n) = bw(1:n)
        zw(1:n) = 0.0

    end do

    ! Restore upper triangle of input matrix.
    do j = 1, n
        do i = 1, j - 1
        a(i,j) = a(j,i)
        end do
    end do

    !  Ascending sort the eigenvalues and eigenvectors.
    do k = 1, n - 1

        m = k

        do l = k + 1, n
            if (d(l) < d(m)) m = l
        end do

        if ( m /= k ) then
            t = d(m)
            d(m) = d(k)
            d(k) = t

            w(1:n) = v(1:n,m)
            v(1:n,m) = v(1:n,k)
            v(1:n,k) = w(1:n)

        end if

    end do

end subroutine jacobi_eigenvalue


! function solve_real32 (a, b) result (out)
!     implicit none
!     real, intent(IN) :: a(:,:), b(:)
!     real :: out(size(b))
!     error stop "Not implemented yet"
! end function solve_real32


end module xslib_linalg


   