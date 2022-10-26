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

! Use with MACRO call to override the function.
! #define __THISFILE__ "error.test.F90"
! #define error (x) error_ (x, __THISFILE__, __LINE__)
! #define warning (x) warning_ (x, __THISFILE__, __LINE__)
! #define assert (x) assert_ (x, __THISFILE__, __LINE__)

program main
  use xslib_errorh
  implicit none

  call assert_test ()
  call warning_test ()
  call error_test ()

  ! NOTE: This test returns error.

contains

! Test assertion routine.
subroutine assert_test ()
  implicit none

  call assert (1 == 1)
  call assert ([1,1] == 1)
  call assert (reshape([1,1,1,1],[2,2]) == 1)

  call assert_ (1 == 1, "assert.test", 1)
  call assert_ ([1,1] == 1, "assert.test", 2)
  call assert_ (reshape([1,1,1,1],[2,2]) == 1, "assert.test", 3)

end subroutine assert_test 

! Test warning routine.
subroutine warning_test ()
  implicit none

  call warning ("Warning message.")
  call warning_ ("Warning message.", "warning.test", 1)

end subroutine warning_test 

! Test error routine.
subroutine error_test ()
  implicit none

  call error ("Error message.")
  call error_ ("Error message.", "error.test", 1)

end subroutine error_test 

end program main