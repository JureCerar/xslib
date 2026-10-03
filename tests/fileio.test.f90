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
    use xslib_fileio
    implicit none
    class(*), allocatable :: array(:,:)
    character(256) :: file, message
    integer :: i, status

    ! Get 1st file from command line
    call get_command_argument (1, file, STATUS=status)
    if (status /= 0) error stop

    array = fromtxt(file)

    array = fromtxt(file, STAT=status, ERRMSG=message)
    if (status /= 0) error stop trim(message)

    ! Test different molds
    ! array = fromtxt(file, mold=1)
    array = fromtxt(file, mold=1.0)
    array = fromtxt(file, mold=file)

    ! ----------------------------------------------------------------------
    ! Get 2nd file from command line
    call get_command_argument(2, file, STATUS=status)
    if (status /= 0) error stop

    ! For hetero files only selected options should work
    array = fromtxt(file, skiprows=1, usecols=[1,2,4], STAT=status, ERRMSG=message)
    if (status /= 0) error stop
    
    select type (array)
    type is (integer)
        do i = 1, size(array, DIM=2)
            write (*, *) array(:,i)
        end do
    type is (real)
        do i = 1, size(array, DIM=2)
            write (*, *) array(:,i)
        end do
    type is (character(*))
        do i = 1, size(array, DIM=2)
            write (*, *) array(:,i)
        end do
    class default
        error stop "Unknown TYPE"
    end select

end program main