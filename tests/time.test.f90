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
  use iso_fortran_env, only: REAL64
  use xslib_time
  implicit none
  real(REAL64) :: time
  real, parameter :: delta = 0.01 

  time = wtime()

  time = 0.0_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "0.000") error stop

  time = 1.1111_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "1.111") error stop

  time = 11.1111_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "11.111") error stop

  time = 71.1111_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "1:11.111") error stop

  time = 671.1111_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "11:11.111") error stop

  time = 4271.1111_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "1:11:11.111") error stop

  time = 40271.1111_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "11:11:11.111") error stop

  time = 126671.1111_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "1-11:11:11.111") error stop

  time = 990671.1111_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "11-11:11:11.111") error stop

  time = 9630671.1111_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "111-11:11:11.111") error stop

  time = 86399999.9999_REAL64
  print *, writeTime(time)
  if (writeTime(time) /= "999-23:59:59.999") error stop

  ! Sleep for 0.5 sec
  time = wtime()
  call msleep (250)
  time = wtime() - time
  if (abs(time - 0.250) > delta) error stop

end program main