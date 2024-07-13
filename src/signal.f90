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

module xslib_signal
    use iso_fortran_env, only: REAL64
    implicit none
    private
    public :: savgol_filter, savgol_coeff

    ! Length of error string
    integer, parameter :: ERR_LEN = 128 

    ! %%%
    ! # `SIGNAL` - Basic signal processing functions
    !   Module `xslib_signal` contains basic signal processing functions. Supports both single and double precision (`DP`).
    ! %%%

contains

function savgol_filter(y, window, polyorder, deriv, mode, stat, errmsg) result (result)
    ! %%%
    ! ## `SAVGOL_FILTER` - Apply Savitzky-Golay filter
    ! #### DESCRIPTION
    !   Apply a [Savitzky-Golay](https://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter)
    !   filter to an array. 
    ! #### USAGE
    !   ```Fortran
    !   result = savgol_filter(y, window, polyorder, deriv, mode, STAT=stat, ERRMSG=errmsg)
    !   ```
    ! #### PARAMETERS
    !   * `real(*), dimension(:), intent(IN) :: y`
    !     The data to be filtered.
    !   * `integer, intent(IN) :: window`
    !     The length of the filter window (i.e., the number of coefficients). Must be 
    !     less than or equal to the size of `y`.
    !   * `integer, intent(IN) :: polyorder`
    !     The order of the polynomial used to fit the samples. Must be less than `window`.
    !   * `integer, intent(IN) :: deriv`
    !     The order of the derivative to compute. This must be a non-negative integer. Value of 0
    !     means to filter the data without differentiating.
    !   * `character(*), intent(IN) :: mode`
    !     Must be `none`, `nearest`, `mirror`, or `wrap`. This determines the type of extension
    !     to use for the padded signal to which the filter is applied. See notes bellow.
    !   * `integer, intent(OUT), OPTIONAL :: stat`
    !     Error status code. Returns zero if no error.
    !   * `character(:), intent(OUT), OPTIONAL :: errmsg`
    !     Error message.
    !   * `real(*):: result`
    !     The filtered data. Size is equal to `y`.
    ! #### SOURCE
    !   Peng Jun, https://github.com/cran/tgcd
    ! #### NOTES
    !    Assuming `window` is 7, the following shows the extended data for the various mode options:
    !    ```
    !    mode       |   Ext   |         Input          |   Ext
    !    -----------+---------+------------------------+---------
    !    'none'     |         | 1  2  3  4  5  6  7  8 |        
    !    'nearest'  | 1  1  1 | 1  2  3  4  5  6  7  8 | 8  8  8
    !    'mirror'   | 4  3  2 | 1  2  3  4  5  6  7  8 | 7  6  5
    !    'wrap'     | 6  7  8 | 1  2  3  4  5  6  7  8 | 1  2  3
    !    ```
    ! #### EXAMPLE
    !   ```Fortran
    !   > ny = savgol_filter(y, 5, 2, 0, 'nearest')
    !   [1.74, 3.03, ..., 4.60, 7.97]
    !   ```
    ! %%%
    implicit none
    real(REAL64), intent(IN) :: y(:)
    integer, intent(IN) :: window, polyorder, deriv
    character(*), intent(IN) :: mode
    integer, intent(OUT), OPTIONAL :: stat
    character(*), intent(OUT), OPTIONAL :: errmsg
    character(ERR_LEN) :: message
    real(REAL64) :: result(size(y))
    real(REAL64) :: temp(window+size(y)-1)
    integer :: np, left, right, status

    result = y

    np = size(y)
    left = floor((window - 1) / 2.0)
    right = ceiling((window - 1) / 2.0)

    catch: block

        if (window < 0) then
            message = "Invalid window size"
            status = 1
            exit catch
        end if

        if (deriv > polyorder) then
            message = "Invalid derivative size"
            status = 1
            exit catch
        end if

        if (left+right < polyorder) then
            message = "Invalid polyorder size"
            status = 1
            exit catch
        end if

        select case (mode)
        case ("NONE", "none")
            result = y
            call savgol(result, window, polyorder, deriv, STAT=status, ERRMSG=message)
            if (status /= 0) exit catch

        case ("MIRROR", "mirror")
            ! Add mirrored borders
            temp(:left) = y(1+left:2:-1)
            temp(left+1:left+np) = y
            temp(left+np+1:) = y(np-1:np-right:-1)

            call savgol(temp, window, polyorder, deriv, STAT=status, ERRMSG=errmsg)
            if (status /= 0) exit catch

            result = temp(left+1:left+np)

        case ("NEAREST", "nearest")
            ! Add extended borders
            temp(:left) = y(1)
            temp(left+1:left+np) = y
            temp(left+np+1:) = y(np)

            call savgol(temp, window, polyorder, deriv, STAT=status, ERRMSG=errmsg)
            if (status /= 0) exit catch

            result = temp(left+1:left+np)

        case ("WRAP", "wrap")
            ! Add wrapped borders
            temp(:left) = y(np-left+1:np)
            temp(left+1 : left+np) = y
            temp(left+np+1:) = y(:right)

            call savgol(temp, window, polyorder, deriv, STAT=status, ERRMSG=errmsg)
            if (status /= 0) exit catch

            result = temp(left+1:left+np)

        case default
            message = "Unsupported mode: '" // trim(mode) // "'" 
            status = 1

        end select

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end function savgol_filter


subroutine savgol(y, window, polyorder, deriv, stat, errmsg)
    ! Calculates SG filtered data for given y
    implicit none
    real(REAL64), intent(INOUT):: y(:)
    integer, intent(in):: window, deriv, polyorder
    integer, intent(OUT), OPTIONAL :: stat
    character(*), intent(OUT), OPTIONAL :: errmsg
    character(ERR_LEN) :: message 
    integer :: i, j, nr, nl, n1, xl(window), status
    real(REAL64):: y0(size(y)), coef(window)

    catch: block

        n1 = size(y)
        xl(1) = 0
        y0 = y

        nl = floor((window - 1) / 2.0)
        nr = ceiling((window - 1) / 2.0)    

        do i = 1, nl
            xl(i+1) = -i
        end do

        do i=1, nr
            xl(1+nl+i) = nr-i+1
        end do

        coef = savgol_coeff(window, polyorder, deriv, status, message)
        if (status /= 0) exit catch

        do i = 1, n1-nr
            y(i) = 0.0
            do j=1, nl+nr+1
                if (i+xl(j) .gt. 0) then
                    y(i) = y(i) + coef(j)*y0(i+xl(j))
                end if
            end do
        end do

        if (deriv==0) then
            y(1:nl) = y0(1:nl)
            y(n1-nr+1:n1) = y0(n1-nr+1:n1)

        else 
            y(1:nl) = y(nl+1)
            y(n1-nr+1:n1) = y(n1-nr)
    
        end if

    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine savgol


function savgol_coeff(window, polyorder, deriv, stat, errmsg) result (result)
    ! %%%
    ! ## `SAVGOL_COEFF` - Calculate Savitzky-Golay filter coefficients
    ! #### DESCRIPTION
    !   This routine is used to calculate a set of Savitzky-Golay filter coefficients.
    ! #### USAGE
    !   ```Fortran
    !   result = savgol_coeff(window, polyorder, deriv, STAT=stat, ERRMSG=errmsg)
    !   ```
    ! #### PARAMETERS
    !   * `integer, intent(IN) :: window`
    !     The length of the filter window (i.e., the number of coefficients).
    !   * `integer, intent(IN) :: polyorder`
    !     The order of the polynomial used to fit the samples. Must be less than `window`.
    !   * `integer, intent(IN) :: deriv`
    !     The order of the derivative to compute. This must be a non-negative integer. Value of 0
    !     means to filter the data without differentiating.
    !   * `integer, intent(OUT), OPTIONAL :: stat`
    !     Error status code. Returns zero if no error.
    !   * `character(:), intent(OUT), OPTIONAL :: errmsg`
    !     Error message.
    !   * `real(*):: result`
    !     The filter coefficients. Size is equal to `window`.
    ! #### SOURCE
    !   Peng Jun, https://github.com/cran/tgcd
    ! #### EXAMPLE
    !   ```Fortran
    !   > coeff = savgol_coeff(5, 2, deriv=1)
    !   [ 2.000e-01,  1.000e-01,  2.075e-16, -1.000e-01, -2.000e-01]
    !   ```
    ! %%%
    implicit none
    integer, intent(in):: window, deriv, polyorder
    integer, intent(out), OPTIONAL :: stat
    character(*), intent(out), OPTIONAL :: errmsg
    real(REAL64):: result(window)
    character(ERR_LEN) :: message
    integer :: imj, ipj, k, kk, mm, nl, nr, indx(polyorder+1), status
    real(REAL64):: d, fac, summ, a(polyorder+1, polyorder+1), b(polyorder+1)

    stat = 0
    result = 0.0

    catch: block

        nl = floor((window - 1) / 2.0)
        nr = ceiling((window - 1) / 2.0)

        ! if (nl < 0 .or. nr < 0 .or. deriv > polyorder .or. nl+nr < polyorder) then
        !     stat = 1
        !     return
        ! end if

        if (nl < 0 .or. nr < 0) then
            message = "Invalid window size"
            status = 1
            exit catch
        end if

        if (deriv > polyorder) then
            message = "Invalid derivative size"
            status = 1
            exit catch
        end if

        if (nl+nr < polyorder) then
            message = "Invalid polyorder size"
            status = 1
            exit catch
        end if

        do ipj = 0, 2*polyorder
            summ = 0.0
            if (ipj == 0) summ = 1.0
            do k=1, nr
                summ = summ + float(k)**ipj
            end do
            do k=1, nl
                summ = summ + float(-k)**ipj
            end do
            mm = min(ipj, 2*polyorder-ipj)
            do imj=-mm, mm, 2
                a(1+(ipj+imj)/2, 1+(ipj-imj)/2) = summ
            end do
        end do

        call ludcmp(a, polyorder+1, indx, d, status)
        if (status /= 0) then
            message = "Singular matrix"
            exit catch
        end if

        b = 0.0
        b(deriv+1) = 1.0

        call lubksb(a, polyorder+1, indx, b)

        do k = -nl, nr
            summ = b(1)
            fac = 1.0
            do mm = 1, polyorder
                fac = fac * k
                summ = summ + b(mm+1) * fac
            end do
            kk = mod(nl+nr+1-k, nl+nr+1) + 1
            result(kk) = summ
        end do
    
    end block catch

    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end function savgol_coeff


subroutine ludcmp(a, n, indx, d, stat)
    ! DESCRIPTION
    !   This routine is used in combination with lubksb to solve 
    !   linear equations or invert a matrix.
    ! ARGUMENTS
    !   a(n,n) = real: a matrix to be decomposed.
    !   n = integer: the dimension of the matrix.
    !   indx(n) = integer: vector that records the row permutation effected by the partial pivoting.
    !   d = integer: output as 1 or -1 depending on whether the number of row interchanges was even or odd.
    !   stat = integer: error message, 0=success, 1=singular matrix.
    ! SOURCE
    !   Peng Jun, https://github.com/cran/tgcd
    ! REFERENCES
    !    This subroutine is remodified from page 38 in Press et al, 1986, Numberic 
    !    recipes in Fortran 77, the Art of Scientific Computing, second edition. 
    implicit none
    integer, intent(in) :: n
    integer, intent(out) :: indx(n), stat
    real(REAL64), intent(inout) :: a(n,n)
    real(REAL64), intent(out) :: d
    integer :: i, j, k, imax
    real(REAL64) :: aamax, dum, summ, vv(n)
   
    indx = 0
    stat = 0
    d = 1.0
    imax = 0

    do i = 1, n
        aamax = 0.0
        do j = 1, n
            if (abs(a(i,j)) > aamax) aamax = abs(a(i,j))
        end do
        if (aamax == 0.0) then 
            stat = 1
            return
        end if
        vv(i) = 1.0 / aamax
    end do

    do j = 1, n
        do i = 1, j-1
            summ = a(i,j)
            do k = 1, i-1
                summ = summ - a(i,k) * a(k,j)
            end do
            a(i,j) = summ
        end do

        aamax = 0.0
        do i = j, n
            summ = a(i,j)
            do k = 1, j-1
                summ = summ - a(i,k) * a(k,j)
            end do

            a(i,j) = summ
            dum = vv(i) * abs(summ)
            if (dum >= aamax) then
                imax = i
                aamax = dum
            end if
        end do

        if (j /= imax) then
            do k = 1, n
                dum = a(imax,k)
                a(imax,k) = a(j,k)
                a(j,k) = dum
            end do
            d = -d
            vv(imax) = vv(j)
        end if

        indx(j) = imax
        if (a(j,j) == 0.0) a(j,j) = tiny(0.0D+00)

        if (j /= n) then
            dum = 1.0 / a(j,j)
            do i = j+1, n
                a(i,j) = a(i,j) * dum
            end do
        end if

    end do

end subroutine ludcmp    


subroutine lubksb(a, n, indx, b)
    ! DESCRIPTION
    !   This routine is used in combination with ludcmp to solve 
    !   linear equations or invert a matrix.
    ! ARGUMENTS
    !   a(n,n) = real: a matrix to be decomposed.
    !   n = integer: the dimension of the matrix.
    !   indx(n) = integer: vector that records the row permutation effected by the partial pivoting.
    !   b = integer: the solution vector X for linear equations A*X=B.
    ! SOURCE
    !   Peng Jun, https://github.com/cran/tgcd
    ! REFERENCES
    !    This subroutine is remodified from page 39 in Press et al, 1986, Numberic 
    !    recipes in Fortran 77, the Art of Scientific Computing, second edition. 
    implicit none
    integer, intent(in):: n, indx(n)
    real(REAL64), intent(in):: a(n,n)
    real(REAL64), intent(inout):: b(n)
    integer:: i, ii, j, ll
    real(REAL64):: summ

    ii = 0

    do i = 1, n
        ll = indx(i)
        summ = b(ll)
        b(ll) = b(i)
        if (ii /= 0) then
            do j = ii, i-1
                summ = summ - a(i,j) * b(j)
            end do
        else if (summ /= 0.0) then
            ii = i
        end if
        b(i) = summ
    end do

    do i = n, 1, -1
        summ = b(i)
        do j=i+1, n
            summ = summ - a(i,j) * b(j)
        end do
        b(i) = summ / a(i,i)
    end do

end subroutine lubksb


end module xslib_signal
