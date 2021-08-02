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

module xslib_time
  implicit none

contains

! Return precise time in sec.
real(REAL64) function wtime()
  use, intrinsic :: iso_fortran_env, only: REAL64, INT64
  implicit none
  integer(INT64) :: count, count_rate
  call system_clock( count, count_rate )
  wtime = count/real(count_rate,KIND=REAL64)
  return
end function wtime

! Transforms time in sec to string in format "ddd hh:mm:ss.sss".
! * Use with either OMP_get_wtime() or wtime()
! * Example: 151501.992 = "001 18:05:01.992"
character(16) function write_wtime( time )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), intent(in) :: time   ! seconds
  real(REAL64)             :: rtime  ! remaining time
  integer                  :: day, hour, min, sec, msec

  ! It's the final countdown...
  rtime = time
  day = int( rtime / (24*3600) )
  rtime = mod(rtime, 24.*3600)
  hour = int( rtime / 3600 )
  rtime = mod(rtime, 3600.)
  min = int( rtime / 60 )
  rtime = mod(rtime, 60.)
  sec = int( rtime )
  rtime = mod(rtime, 1.)
  msec = int( rtime*1000 )

  ! Format "ddd hh:mm:ss.sss".
  write (write_wtime,100) day, hour, min, sec, msec
  100 format( i3, x, i2.2, 2(':',i2.2), '.', i3.3 )

  return
end function write_wtime

! Susspend execution for millisecond intervals
integer function msleep( time )
  use, intrinsic :: iso_c_binding, only: C_INT
  implicit none
  integer, intent(in) :: time
  interface
    ! usleep - suspend execution for microsecond intervals
    ! SOURCE: https://linux.die.net/man/3/usleep
    integer(C_INT) function usleep( usec ) bind( C )
      import
      integer(C_INT), value :: usec ! microseconds
    end function usleep
  end interface
  msleep = usleep(int(time*1000,KIND=C_INT))
  return
end function msleep

end module xslib_time
