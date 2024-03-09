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

module xslib_fitting
  use iso_fortran_env, only: REAL32, REAL64
  implicit none
  private
  public :: linfit, polyfit, polyval

  ! %%%
  ! # `FITTING` - Function fitting
  !   Module `xslib_fitting` contains basic regression functions. Supports both single and double precision (`DP`). 
  ! %%%

  interface linfit
    module procedure :: linfit_real32, linfit_real64
  end interface linfit

  interface polyfit
    module procedure :: polyfit_real32, polyfit_real64
  end interface polyfit

  interface polyval
    module procedure :: polyval_real32, polyval_real64
    module procedure :: polyval_array_real32, polyval_array_real64
  end interface polyval

contains

subroutine inverse_real32 (matrix)
  ! DESCRIPTION
  !   Calculate inverse of given matrix.
  !   See: https://ww2.odu.edu/~agodunov/computing/programs/book2/Ch06/Inverse.f90
  implicit none
  real(REAL32), intent(inout) :: matrix(:,:)
  real(REAL32), allocatable :: L(:,:), U(:,:), b(:), d(:), x(:)
  real(REAL32) :: coeff
  integer :: np, i, j, k

  np = size(matrix, 1)
  if (np /= size(matrix, 2)) error stop "Input is not square matrix"

  ! step 0: Initialization 
  allocate (L(np,np), U(np,np), b(np), d(np), x(np))
  L = 0.0
  U = 0.0
  b = 0.0

  ! step 1: Forward elimination
  do k = 1, np-1
    do i = k+1, np
      coeff = matrix(i,k) / matrix(k,k)
      L(i,k) = coeff
      do j = k+1, np
        matrix(i,j) = matrix(i,j) - coeff * matrix(k,j)
      end do
    end do
  end do

  ! Step 2: prepare L and U matrices 
  do i = 1, np
    L(i,i) = 1.0
  end do
  do j = 1, np
    do i = 1, j
      U(i,j) = matrix(i,j)
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
      matrix(i,k) = x(i)
    end do
    b(k) = 0.0
  end do

end subroutine inverse_real32


subroutine inverse_real64 (matrix)
  implicit none
  real(REAL64), intent(inout) :: matrix(:,:)
  real(REAL64), allocatable :: L(:,:), U(:,:), b(:), d(:), x(:)
  real(REAL64) :: coeff
  integer :: np, i, j, k

  np = size(matrix, 1)
  if (np /= size(matrix, 2)) error stop "Input is not square matrix"

  ! step 0: Initialization 
  allocate (L(np,np), U(np,np), b(np), d(np), x(np))
  L = 0.0
  U = 0.0
  b = 0.0

  ! step 1: Forward elimination
  do k = 1, np-1
    do i = k+1, np
      coeff = matrix(i,k) / matrix(k,k)
      L(i,k) = coeff
      do j = k+1, np
        matrix(i,j) = matrix(i,j) - coeff * matrix(k,j)
      end do
    end do
  end do

  ! Step 2: prepare L and U matrices 
  do i = 1, np
    L(i,i) = 1.0
  end do
  do j = 1, np
    do i = 1, j
      U(i,j) = matrix(i,j)
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
      matrix(i,k) = x(i)
    end do
    b(k) = 0.0
  end do

end subroutine inverse_real64


function linfit_real32 (x, y) result (out)
  ! %%%
  ! ## `LINFIT` - Least squares linear fit
  ! #### DESCRIPTION
  !   Calculate a linear least-squares regression for two sets of data.
  ! #### USAGE
  !   ```fortran
  !   out = linfit(x, y)
  !   ```
  ! #### PARAMETERS
  ! * `real(ANY), dimension(:), intent(IN) :: x, y`
  !   Two sets of data points to be fitted. Both arrays must have the same length.  
  ! * `real(ANY), dimension(2) :: out`
  !   Linear coefficients: slope, and intercept (highest power first).
  ! #### EXAMPLE
  ! ```Fortran
  ! > linfit([1.0, 2.0, 3.0], [2.0, 3.0, 4.0])
  ! [1.0, 2.0]
  ! ```
  ! #### NOTE
  !   For large array sizes use [LAPACK](https://www.netlib.org/lapack/).
  ! %%%
  implicit none
  real(REAL32), intent(in) :: x(:), y(size(x))
  real(REAL32) :: out(2)
  real(REAL32) :: Sx, Sy, Sxx, Sxy
  integer :: np

  np = size(x)
  Sx = sum(x)
  Sy = sum(y)
  Sxx = sum(x * x)
  Sxy = sum(x * y)
  out(1) = (Sxx * Sy - Sxy * Sx) / (np * Sxx - Sx * Sx)
  out(2) = (np * Sxy - Sx * Sy) / (np * Sxx - Sx * Sx)

end function linfit_real32


function linfit_real64 (x, y) result (out)
  implicit none
  real(REAL64), intent(in) :: x(:), y(size(x))
  real(REAL64) :: out(2)
  real(REAL64) :: Sx, Sy, Sxx, Sxy
  integer :: np

  np = size(x)
  Sx = sum(x)
  Sy = sum(y)
  Sxx = sum(x * x)
  Sxy = sum(x * y)
  out(1) = (Sxx * Sy - Sxy * Sx) / (np * Sxx - Sx * Sx)
  out(2) = (np * Sxy - Sx * Sy) / (np * Sxx - Sx * Sx)

end function linfit_real64


function polyfit_real32 (x, y, deg) result (out)
  ! %%%
  ! ## `POLYFIT` - Least squares polynomial fit
  ! #### DESCRIPTION
  !   Calculate a polynomial least-squares regression for two sets of data.
  ! #### USAGE
  !   ```fortran
  !   out = polyfit(x, y, deg)
  !   ```
  ! #### PARAMETERS
  ! * `real(ANY), dimension(:), intent(IN) :: x, y`
  !   Two sets of data points to be fitted. Both arrays must have the same length.
  ! * `integer, intent(IN) :: deg`
  !   Degree of the fitting polynomial.
  ! * `real(ANY), dimension(DEG) :: out`
  !   Polynomial coefficients: highest powers first.
  ! #### EXAMPLE
  ! ```Fortran
  ! > polyfit([1.0, 2.0, 3.0], [6.0, 11.0, 18.0], 2)
  ! [1.0, 2.0, 3.0]
  ! ```
  ! #### NOTE
  !   For large arrays use [LAPACK](https://rosettacode.org/wiki/Polynomial_regression#Fortran).
  ! %%%
  implicit none
  real(REAL32), intent(in) :: x(:), y(size(x))
  integer, intent(in) :: deg
  real(REAL32) :: out(deg+1)
  real(REAL32), allocatable :: M(:,:), MT(:,:), MTM(:,:)
  integer :: np, i, j

  np = size(x)
  allocate(M(np, deg+1), MT(deg+1, np), MTM(np, np))
  do i = 0, deg
    do j = 1, np
      M(j, i+1) = x(j)**i
    end do 
  end do
  MT = transpose(M)
  MTM = matmul(MT, M)
  call inverse_real32(MTM)
  out = matmul(matmul(MTM, MT), y)

end function polyfit_real32


function polyfit_real64 (x, y, deg) result (out)
  implicit none
  real(REAL64), intent(in) :: x(:), y(size(x))
  integer, intent(in) :: deg
  real(REAL64) :: out(deg+1)
  real(REAL64), allocatable :: M(:,:), MT(:,:), MTM(:,:)
  integer :: np, i, j

  np = size(x)
  allocate(M(np, deg+1), MT(deg+1, np), MTM(np, np))
  do i = 0, deg
    do j = 1, np
      M(j, i+1) = x(j)**i
    end do 
  end do
  MT = transpose(M)
  MTM = matmul(MT, M)
  call inverse_real64(MTM)
  out = matmul(matmul(MTM, MT), y)

end function polyfit_real64


function polyval_real32 (p, x) result (out)
  ! %%%
  ! ## `POLYVAL` - Evaluate a polynomial 
  ! #### DESCRIPTION
  !   Evaluate a polynomial at specific values.
  ! #### USAGE
  !   ```fortran
  !   out = polyval(p, x)
  !   ```
  ! #### PARAMETERS
  ! * `real(ANY), dimension(:), intent(IN) :: p`
  !   Polynomial coefficients: highest powers first.
  ! * `real(ANY), dimension(..), intent(IN) :: x`
  !   Value or array at which to evaluate the polynomial. 
  ! * `real(ANY), dimension(..) :: out`
  !   Value or array of polynomial.
  ! #### EXAMPLE
  ! ```Fortran
  ! > polyval([1.0, 2.0, 3.0], 1.0)
  ! 6.0
  ! > polyval([1.0, 2.0, 3.0], [1.0, 2.0, 3.0])
  ! [6.0, 11.0, 18.0]
  ! ```
  ! %%%  
  implicit none
  real(REAL32), intent(in) :: x, p(:)
  real(REAL32) :: out
  integer :: i

  out = sum([(p(i) * x ** (i-1), i = 1, size(p))])

end function polyval_real32


function polyval_real64 (p, x) result (out)
  implicit none
  real(REAL64), intent(in) :: x, p(:)
  real(REAL64) :: out
  integer :: i

  out = sum([(p(i) * x ** (i-1), i = 1, size(p))])

end function polyval_real64


function polyval_array_real32 (p, x) result (out)
  implicit none
  real(REAL32), intent(in) :: x(:), p(:)
  real(REAL32) :: out(size(x))
  integer :: i

  out = p(1)
  do i = 2, size(p)
    out = out + p(i) * x ** (i-1)
  end do

end function polyval_array_real32


function polyval_array_real64 (p, x) result (out)
  implicit none
  real(REAL64), intent(in) :: x(:), p(:)
  real(REAL64) :: out(size(x))
  integer :: i

  out = p(1)
  do i = 2, size(p)
    out = out + p(i) * x ** (i-1)
  end do

end function polyval_array_real64

end module xslib_fitting

