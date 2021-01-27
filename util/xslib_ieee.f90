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

module xslib_ieee
  implicit none
  private

contains

! Example if IEEE exception
real(REAL64) function func( a, b )
  use, intrinsic :: iso_fortran_env, only: REAL64, INT64
  use, intrinsic :: ieee_exceptions, only: ieee_set_flag, IEEE_OVERFLOW
  use, intrinsic :: ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  implicit none
  real(REAL64), intent(in) :: a, b
  real(REAL64), parameter  :: MAX64 = huge(0.0_REAL64)

  if ( b > MAX64-a ) then
    ! Set IEEE_OVERFLOW flag and return NaN
    call ieee_set_flag(IEEE_OVERFLOW,.true.)
    func = ieee_value(func,IEEE_QUIET_NAN)
  else
    func = a + b
  end if

  return
end function func

end module xslib_ieee
