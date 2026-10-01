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

module xslib_errorh
  !! Module with functions for error and warning handling.
  use iso_fortran_env, only: ERROR_UNIT
  implicit none
  private
  public :: error, error_, warning, warning_, assert, assert_

  ! Keywords for error and warning
  character(*), parameter, private :: errorKey = char(27)//"[1;91m"//"[Error]:"//char(27)//"[m"
  character(*), parameter, private :: warningKey = char(27)//"[1;95m"//"[Warning]:"//char(27)//"[m"

contains

subroutine error (message)
  !! Write error message to `STDERR` and terminate the program.
  !!
  !! Example:
  !! ```Fortran
  !! call error("Invalid input value")
  !! >>> "Error: Invalid input value" 
  !! ```
  !! 
  !! @note
  !! Use with `__FILE__` and `__LINE__` macros to get extended error message:
  !!
  !! ```
  !!  #define error(x) error_(x, __FILE__, __LINE__)
  !! ```
  !! @endnote
  implicit none
  character(*), intent(in) :: message
  !! Error message to display.

  write (ERROR_UNIT, "(2(x,a))") errorKey, trim(message)
  call exit (1)

end subroutine error


subroutine error_ (message, file, line)
  !! Write extended error message to `STDERR` and terminate the program.
  !!
  !! Example:
  !! ```Fortran
  !! call error_("Invalid input value", __FILE__, __LINE__)
  !! >>> "Error:main.f90:1107: Invalid input value" 
  !! ```
  implicit none
  character(*), intent(in)  :: message
  !! Error message to display.
  character(*), intent(in)  :: file
  !! File name.
  integer, intent(in) :: line
  !! Line number.

  write (ERROR_UNIT, "(x,2a,i0,2a,x,a)") trim(file), ":", line, ":", errorKey, trim(message)
  call exit (1)

end subroutine error_


subroutine warning (message)
  !! Write warning message to `STDERR`.
  !!
  !! Example:
  !! ```Fortran
  !! call warning("Invalid input value")
  !! >>> "Warning: Invalid input value" 
  !! ```
  !! 
  !! @note
  !! Use with `__FILE__` and `__LINE__` macros to get extended warning message:
  !!
  !! ```
  !!  #define warning(x) warning_(x, __FILE__, __LINE__)
  !! ```
  !! @endnote
  implicit none
  character(*), intent(in) :: message
  !! Warning message to display.

  write (ERROR_UNIT, "(2(x,a))") warningKey, trim(message)
 
end subroutine warning


subroutine warning_ (message, file, line)
  !! Write extended warning message to `STDERR`.
  !!
  !! Example:
  !! ```Fortran
  !! call warning("Invalid input value", __FILE__, __LINE__)
  !! >>> "Warning:main.f90:1107: Invalid input value" 
  !! ```
  implicit none
  character(*), intent(in)  :: message
  !! Warning message to display.
  character(*), intent(in)  :: file
  !! File name.
  integer, intent(in) :: line
  !! Line number.

  write (ERROR_UNIT, "(x,2a,i0,2a,x,a)") trim(file), ":", line, ":", warningKey, trim(message)

end subroutine warning_


subroutine assert (expression)
  !! Category: experimental
  !! Assert logical expression. On fail write error message to `STDERR` and terminate the program.
  !!
  !! Example:
  !! ```Fortran
  !! call assert (array == 0)
  !! >>> "Error: Assertion failed at (1,1)"
  !! ```
  !!
  !! @note
  !! Use with `__FILE__` and `__LINE__` macros to get extended assertion message:
  !!
  !! ```
  !!  #define assert(x) assert_(x, __FILE__, __LINE__)
  !! ```
  !! @endnote
  !! 
  !! @warning
  !! I don't know why, but somtimes GCC compiler incorectly optimizes the
  !! function and the assertion fails as if the array is out of bounds.
  !! @endwarning
  implicit none
  logical, intent(in) :: expression(..)
  !! Logical expression to be evaluated.
  character(256) :: message 
  integer :: i, j

  select rank (expression)
  rank (0)
    if (.not. expression) then 
      call error ("Assertion failed")
    end if
  rank (1)
    do i = 1, size(expression)
      if (.not. expression(i)) then
        write (message, "(a,i0,a)") "Assertion failed at (", i ,")"
        call error (message)
      end if
    end do
  rank (2)
    do i = 1, size(expression, DIM=2)
      do j = 1, size(expression, DIM=1)
        if (.not. expression(j, i)) then
          write (message, "(a,i0,a,i0,a)") "Assertion failed at (", j, ",", i ,")"
          call error (message)
        end if
      end do
    end do
  rank default
    error stop "Unsupported RANK size"
  end select

end subroutine assert


subroutine assert_ (expression, file, line)
  !! Category: experimental
  !! Assert logical expression. On fail write extended error message to `STDERR` and terminate the program.
  !!
  !! Example:
  !! ```Fortran
  !! call assert_(array == 0, __FILE__, __LINE__)
  !! >>> "Error:main.f90:1107: Assertion failed at (1,1)"
  !! ```
  implicit none
  logical, intent(in) :: expression(..)
  !! Logical expression to be evaluated.
  character(*), intent(in) :: file
  integer, intent(in) :: line
  character(256) :: message
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
        write (message, "(a,i0,a)") "Assertion failed at (", i ,")"
        call error_ (message, file, line)
      end if
    end do
  rank (2)
    do i = 1, size(expression, DIM=2)
      do j = 1, size(expression, DIM=1)
        if (.not. expression(j,i)) then
          write (message, "(a,i0,a,i0,a)") "Assertion failed at (", j, ",", i ,")"
          call error_ (message, file, line)
        end if
      end do
    end do
  rank default
    error stop "Unsupported RANK size"
  end select

end subroutine assert_

end module xslib_errorh
