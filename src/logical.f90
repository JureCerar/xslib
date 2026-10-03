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

module xslib_logical
    use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
    implicit none
    private
    public :: isClose, isAllClose

    interface isClose
        !! Returns
        module procedure :: isClose_r32, isClose_r64
    end interface isClose

    interface isAllClose
        !! Returns `.True.` if two arrays are element-wise equal within a tolerance.
        module procedure :: isAllClose_r32, isAllClose_r64
    end interface isAllClose

contains


function isClose_r32 (a, b, rtol, atol) result (out)
    ! %%%
    ! ## `ISCLOSE` - Checks if two values are within a tolerance
    ! #### DESCRIPTION
    !   Checks if two values are within a tolerance. The tolerance values are positive,
    !   typically very small numbers. The relative difference `(rtol * abs(b))` and the
    !   absolute difference `atol` are added together to compare against the absolute
    !   difference between `a` and `b`.
    ! #### USAGE
    !   ```Fortran
    !   out = isClose(a, b, rtol=rtol, atol=atol)
    !   ```
    ! #### PARAMETERS
    !   * `real(ANY), intent(IN) :: a, b`
    !     Input values to compare.
    !   * `real(ANY), intent(IN), OPTIONAL :: rtol`
    !     The relative tolerance parameter. Default: 1.0e-05
    !   * `real(ANY), intent(IN), OPTIONAL :: rtol`
    !     The absolute tolerance parameter. Default: 1.0e-08
    !   * `logical :: out`
    !     Output value.
    ! #### EXAMPLE
    !   ```Fortran
    !   > isclose(1.1, 1.0, atol=1.0)
    !   .True.
    !   > isclose(1.5, 1.0, RTOL=1.0)
    !   .False.
    !   ```
    ! %%%
    implicit none
    logical :: out
    real(REAL32), intent(in) :: a(..)
    real(REAL32), intent(in) :: b
    real(REAL32), intent(in), optional :: rtol, atol
    real(REAL32) :: rtol_, atol_

    rtol_ = merge(rtol, 1.0e-05, present(rtol))
    atol_ = merge(atol, 1.0e-08, present(atol))
    select rank (a)
    rank (0)
        out = (abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank (1)
        out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank (2)
        out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank (3)
        out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank (4)
        out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank default
        error stop "Unsupported RANK size"
    end select

end function isClose_r32


function isClose_r64 (a, b, rtol, atol) result (out)
    implicit none
    logical :: out
    real(REAL64), intent(in) :: a(..), b
    real(REAL64), intent(in), optional :: rtol, atol
    real(REAL64) :: rtol_, atol_

    rtol_ = merge(rtol, 1.0d-05, present(rtol))
    atol_ = merge(atol, 1.0d-08, present(atol))
    select rank (a)
    rank (0)
        out = (abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank (1)
        out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank (2)
        out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank (3)
        out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank (4)
        out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
    rank default
        error stop "Unsupported RANK size"
    end select

end function isClose_r64


function isAllClose_r32 (a, b, rtol, atol) result (out)
    implicit none
    logical :: out
    real(REAL32), intent(in) :: a(..), b(..)
    real(REAL32), intent(in), optional :: rtol, atol
    real(REAL32) :: rtol_, atol_

    rtol_ = merge(rtol, 1.0e-05, present(rtol))
    atol_ = merge(atol, 1.0e-08, present(atol))
    if (rank(a) /= rank(b)) error stop "RANK size mismatch"
    select rank (a)
    rank (1)
        select rank (b)
        rank (1)
            if (size(a) == size(b)) then
                out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
            else
                out = .false.
            end if
        end select
    rank (2)
        select rank (b)
        rank (2)
            if (size(a) == size(b)) then
                out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
            else
                out = .false.
            end if
        end select
    rank (3)
        select rank (b)
        rank (3)
            if (size(a) == size(b)) then
                out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
            else
                out = .false.
            end if
        end select
    rank (4)
        select rank (b)
        rank (4)
            out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
        end select
    rank default
        error stop "Unsupported RANK size"
    end select

end function isAllClose_r32


function isAllClose_r64 (a, b, rtol, atol) result (out)
    implicit none
    logical :: out
    real(REAL64), intent(in) :: a(..), b(..)
    real(REAL64), intent(in), optional :: rtol, atol
    real(REAL64) :: rtol_, atol_

    rtol_ = merge(rtol, 1.0d-05, present(rtol))
    atol_ = merge(atol, 1.0d-08, present(atol))
    if (rank(a) /= rank(b)) error stop "RANK size mismatch"
    select rank (a)
    rank (1)
        select rank (b)
        rank (1)
            if (size(a) == size(b)) then
                out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
            else
                out = .false.
            end if
        end select
    rank (2)
        select rank (b)
        rank (2)
            if (size(a) == size(b)) then
                out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
            else
                out = .false.
            end if
        end select
    rank (3)
        select rank (b)
        rank (3)
            if (size(a) == size(b)) then
                out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
            else
                out = .false.
            end if
        end select
    rank (4)
        select rank (b)
        rank (4)
            out = all(abs(a - b) <= (atol_ + rtol_ * abs(b)))
        end select
    rank default
        error stop "Unsupported RANK size"
    end select

end function isAllClose_r64

end module xslib_logical