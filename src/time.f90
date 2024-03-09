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

module xslib_time
  use iso_fortran_env, only: REAL64, INT64
  implicit none
  private
  public :: wtime, writeTime, msleep

  ! %%%
  ! # `TIME` - Time functions
  !   Module `xslib_time` contains functions for timing and displaying time.
  ! %%%

contains

function wtime () result (out)
  ! %%%
  ! ## `WTIME` - Precise time
  ! #### DESCRIPTION
  !   Returns precise wall time in seconds since an unspecified time. The absolute
  !   value of `wtime` is meaningless, only differences between subsequent calls to
  !   this function should be used.
  ! #### USAGE
  !   ```Fortran
  !   out = wtime()
  !   ```
  ! #### PARAMETERS
  !   * `real(REAL64) :: out`
  !     Precise time in seconds. 
  ! #### EXAMPLE
  !   ```Fortran
  !   > wtime()
  !   1234.567890000
  !   ```
  ! %%%
  implicit none
  real(REAL64) :: out
  integer(INT64) :: count, count_rate

  call system_clock (count, count_rate)
  out = count / real(count_rate, KIND=REAL64)

end function wtime


function writeTime (time) result (out)
  ! %%%
  ! ## `WRITETIME` - Write precise time
  ! #### DESCRIPTION
  !   Transforms time in seconds from `wtime` or `OMP_get_wtime` to string
  !   in format `ddd-hh:mm:ss.sss`. Format can be shorter depending on the length of time. 
  ! #### USAGE
  !   ```Fortran
  !   out = writeTime(time)
  !   ```
  ! #### PARAMETERS
  !   * `real(REAL64), intent(IN) :: time`
  !     Precise time in seconds. 
  !   * `character(64) :: out`
  !     Time string in format `ddd-hh:mm:ss.sss`. 
  ! #### EXAMPLE
  !   ```Fortran
  !   > writeTime(151501.992d0)
  !   "1-18:05:01.992"
  !   ```
  ! %%%
  implicit none
  character(64) :: out
  real(REAL64), intent(in) :: time ! seconds
  real(REAL64) :: rtime  ! remaining time
  integer :: day, hour, min, sec, msec

  ! It's the final countdown...
  rtime = time
  day = int(rtime / 86400)
  rtime = mod(rtime, 86400.)
  hour = int(rtime / 3600)
  rtime = mod(rtime, 3600.)
  min = int(rtime / 60)
  rtime = mod(rtime, 60.)
  sec = int(rtime)
  rtime = mod(rtime, 1.)
  msec = int(rtime * 1000)

  ! Format "ddd-hh:mm:ss.sss"
  if (day > 0) then
    100 format (i3, "-", i2.2, ":", i2.2, ":", i2.2, ".", i3.3)
    write (out, 100) day, hour, min, sec, msec

  else if (hour > 0) then
    200 format (i2, ":", i2.2, ":", i2.2, ".", i3.3)
    write (out, 200) hour, min, sec, msec

  else if (min > 0) then
    300 format (i2, ":", i2.2, ".", i3.3)
    write (out, 300) min, sec, msec

  else
    400 format (i2, ".", i3.3)
    write (out, 400) sec, msec

  end if

  out = trim(adjustl(out))

end function writeTime


subroutine msleep (time)
  ! %%%
  ! ## `MSLEEP` - Suspend execution for time interval
  ! #### DESCRIPTION
  !   Suspends execution for specified millisecond interval.
  ! #### USAGE
  !   ```Fortran
  !   call msleep(time)
  !   ```
  ! #### PARAMETERS
  !   * `integer, intent(IN) :: time`
  !     Time interval in milliseconds. 
  ! #### EXAMPLE
  !   ```Fortran
  !   > call msleep(1000)
  !   ```
  ! %%%
  use iso_c_binding, only: C_INT
  implicit none
  integer, intent(in) :: time
  integer(C_INT) :: out

  interface
    ! `usleep` - Suspend execution for microsecond intervals
    ! SOURCE: https://linux.die.net/man/3/usleep
    integer(C_INT) function usleep (usec) bind( C )
      import
      integer(C_INT), value :: usec ! microseconds
    end function usleep
  end interface

  out = usleep(int(time*1000, KIND=C_INT))

end subroutine msleep

end module xslib_time
