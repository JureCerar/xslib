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
    use xslib_signal
    implicit none
    real, parameter :: DELTA = 0.01
    real(REAL64), allocatable :: x(:), y(:), y_corr(:), y_filter(:)
    character(128) :: infile, message, mode
    integer :: i, num_points, window, polyorder, deriv, stat

    ! Get file name from command line
    call get_command_argument (1, infile, STATUS=stat)
    if (stat /= 0) error stop

    ! Read dummy data
    open(FILE=infile, UNIT=100, ACTION="read", IOSTAT=stat)
    if (stat /= 0) error stop 
    read (100, *, IOSTAT=stat) num_points, window, polyorder, deriv, mode
    if (stat /= 0) error stop
    allocate (x(num_points), y(num_points), y_corr(num_points), y_filter(num_points))
    do i = 1, num_points
        read (100, *, IOSTAT=stat) x(i), y(i), y_corr(i)
        if (stat /= 0) error stop
    end do
    close(100, IOSTAT=stat)
    if (stat /= 0) error stop

    ! Apply SG filter w/ errors 
    y_filter = savgol_filter(y, window, polyorder, deriv, mode, STAT=stat, ERRMSG=message)
    if (stat /= 0) error stop

    y_filter = savgol_filter(y, window, polyorder, deriv, mode)
    
    ! Compare values to Scipy
    if (any(abs(y_filter - y_corr) > DELTA)) error stop

    ! Try other modes w/ errors
    y_filter = savgol_filter(y, window, polyorder, deriv, "none", STAT=stat, ERRMSG=message)
    if (stat /= 0) error stop
    y_filter = savgol_filter(y, window, polyorder, deriv, "nearest", STAT=stat, ERRMSG=message)
    if (stat /= 0) error stop
    y_filter = savgol_filter(y, window, polyorder, deriv, "mirror", STAT=stat, ERRMSG=message)
    if (stat /= 0) error stop
    y_filter = savgol_filter(y, window, polyorder, deriv, "wrap", STAT=stat, ERRMSG=message)
    if (stat /= 0) error stop

    y_filter = savgol_filter(y, window, polyorder, deriv, "none")
    y_filter = savgol_filter(y, window, polyorder, deriv, "nearest")
    y_filter = savgol_filter(y, window, polyorder, deriv, "mirror")
    y_filter = savgol_filter(y, window, polyorder, deriv, "wrap")

end program main