! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2023 Jure Cerar
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
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  use xslib_list
  implicit none
  type(list_t) :: list
  integer(INT32) :: i32
  integer(INT64) :: i64
  real(REAL32) :: r32
  real(REAL64) :: r64
  logical :: bool
  character(128) :: string

  ! Try list initialization
  print *, list_t(1)
  print *, list_t([1, 2, 3, 4, 5])

  ! Try adding different types of variables to list
  call list%append(1_INT32)
  call list%append(2_INT64)
  call list%append(3.0_REAL32)
  call list%append(4.0_REAL64)
  call list%append(.true.)
  call list%append("abc")
  print *, list

  ! Try different query functions
  print *, list%len()
  print *, list%count(1)

  print *, list%index(1)
  print *, list%index(3.0)
  print *, list%index(.true.)
  print *, list%index("abc")
  print *, list%index(0)

  ! Try clearing list
  call list%clear()
  print *, list

  call list%extend([1, 2, 3, 4, 5])
  print *, list

  call list%insert(6, 6)
  call list%insert(10000, 7)
  call list%insert(0, 0)
  print *, list

  call list%pop(6)
  call list%pop(10000)
  call list%pop(0)
  print *, list

  call list%reverse()
  print *, list

  call list%set(1, "x")
  print *, list

  call list%clear()
  call list%append(1_INT32)
  call list%append(2_INT64)
  call list%append(3.0_REAL32)
  call list%append(4.0_REAL64)
  call list%append(.true.)
  call list%append("abc")

  ! Try getting items from list
  call list%get(1, i32)
  call list%get(1, i64)
  call list%get(1, r32)
  call list%get(1, r64)
  call list%get(1, string)
  print *, i32, i64, r32, r64, trim(string)

  call list%get(2, i32)
  call list%get(2, i64)
  call list%get(2, r32)
  call list%get(2, r64)
  call list%get(2, string)
  print *, i32, i64, r32, r64, trim(string)

  call list%get(3, i32)
  call list%get(3, i64)
  call list%get(3, r32)
  call list%get(3, r64)
  call list%get(3, string)
  print *, i32, i64, r32, r64, trim(string)

  call list%get(4, i32)
  call list%get(4, i64)
  call list%get(4, r32)
  call list%get(4, r64)
  call list%get(4, string)
  print *, i32, i64, r32, r64, trim(string)

  call list%get(5, bool)
  call list%get(5, string)
  print *, bool, trim(string)

  call list%get(6, string)
  print *, trim(string)

end program main