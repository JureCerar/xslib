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

module xslib_errorh
  use iso_fortran_env, only: ERROR_UNIT
  implicit none
  private
  public :: error, error_, warning, warning_, assert, assert_

  ! Keywords for error and warning
  character(*), parameter, private :: errorKey = char(27)//"[1;91m"//"[Error]:"//char(27)//"[m"
  character(*), parameter, private :: warningKey = char(27)//"[1;95m"//"[Warning]:"//char(27)//"[m"

contains

! Write error message to STDERR and terminate the program.
subroutine error (message)
  implicit none
  character(*), intent(in) :: message

  write (ERROR_UNIT, "(2(x,a))") errorKey, trim(message)
  call exit (1)

end subroutine error

! Write extended error message to STDERR and terminate the program.
! Hint: #define error(x) error_ (x, __FILE__, __LINE__)
subroutine error_ (message, file, line)
  implicit none
  character(*), intent(in)  :: message, file
  integer, intent(in) :: line

  write (ERROR_UNIT, "(x,2a,i0,2a,x,a)") trim(file), ":", line, ":", errorKey, trim(message)
  call exit (1)

end subroutine error_

! Write warning message to STDERR.
subroutine warning (message)
  implicit none
  character(*), intent(in) :: message

  write (ERROR_UNIT, "(2(x,a))") warningKey, trim(message)

end subroutine warning

! Write extended warning message to STDERR.
! Hint: #define warning(x) warning_ (x, __FILE__, __LINE__)
subroutine warning_ (message, file, line)
  implicit none
  character(*), intent(in) :: message, file
  integer, intent(in) :: line

  write (ERROR_UNIT, "(x,2a,i0,2a,x,a)") trim(file), ":", line, ":", warningKey, trim(message)

end subroutine warning_

! Assert logical expression and write error message on fail.
subroutine assert (expression)
  implicit none
  logical, intent(in) :: expression(..)
  character(128) :: message 
  integer :: i, j

  select rank (expression)
  rank (0)
    if (.not. expression) then 
      call error ("Assertion failed")
    end if
  rank (1)
    do i = 1, size(expression)
      if (.not. expression(i)) then
        write (message, "(a,i0,a)") "Assertion failed at: [", i ,"]"
        call error (message)
      end if
    end do
  rank (2)
    do i = 1, size(expression, DIM=2)
      do j = 1, size(expression, DIM=1)
        if (.not. expression(j,i)) then
          write (message, "(a,i0,a,i0,a)") "Assertion failed at: [", j, ",", i ,"]"
          call error (message)
        end if
      end do
    end do
  rank default
    error stop "Unsupported RANK size"
  end select

end subroutine assert

! Assert logical expression and write extended error message on fail.
! Hint: #define assert(x) assert_ (x, __FILE__, __LINE__)
subroutine assert_ (expression, file, line)
  implicit none
  logical, intent(in) :: expression(..)
  character(*), intent(in) :: file
  integer, intent(in) :: line
  character(128) :: message
  integer :: i, j

  select rank (expression)
  rank (0)
      if (.not. expression) then 
        write (message, "(a)") "Assertion failed"
        call error_ (message, file, line) 
      end if
  rank (1)
    do i = 1, size(expression)
      if (.not. expression(i)) then 
        write (message, "(a,i0,a)") "Assertion failed at: [", i ,"]"
        call error_ (message, file, line)
      end if
    end do
  rank (2)
    do i = 1, size(expression, DIM=2)
      do j = 1, size(expression, DIM=1)
        if (.not. expression(j,i)) then
          write (message, "(a,i0,a,i0,a)") "Assertion failed at: [", j, ",", i ,"]"
          call error_ (message, file, line)
        end if
      end do
    end do
  rank default
    error stop "Unsupported RANK size"
  end select

end subroutine assert_

end module xslib_errorh
