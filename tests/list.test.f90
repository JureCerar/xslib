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

  call init_test ()
  call adding_test ()
  call count_test ()
  call index_test ()
  call removing_test ()
  call get_test ()
  call type_test ()
  call other_test ()

contains

subroutine init_test()
  implicit none
  type(list_t) :: list

  ! Initialize with scalar
  list = list_t(1)
  list = list_t(1_INT64)
  list = list_t(1.0)
  list = list_t(1.0d0)
  list = list_t("abc")
  list = list_t(.True.)

  ! Initialize with array
  list = list_t([1, 2, 3])
  list = list_t([1_INT64, 2_INT64, 3_INT64])
  list = list_t([1.0, 2.0, 3.0])
  list = list_t([1.0d0, 2.0d0, 3.0d0])
  list = list_t(["a", "b", "c"])
  list = list_t([.True., .False., .True.])

end subroutine init_test

subroutine adding_test ()
  implicit none
  type(list_t) :: list
  
  ! Test append
  call list%append(1)
  call list%append(1_INT64)
  call list%append(1.0)
  call list%append(1.0d0)
  call list%append("abc")
  call list%append(.True.)
  print *, list

  ! Test extend
  call list%extend([1, 2])
  call list%extend([1_INT64, 2_INT64])
  call list%extend([1.0, 2.0])
  call list%extend([1.0d0, 2.0d0])
  call list%extend(["abc", "abc"])
  call list%extend([.True., .False.])
  print *, list

  ! Test insert
  call list%insert(1, 1)
  call list%insert(1, 1_INT64)
  call list%insert(1, 1.0)
  call list%insert(1, 1.0d0)
  call list%insert(1, "abc")
  call list%insert(1, .True.)
  print *, list

  call list%insert(6, 1.0)
  call list%insert(10000, 1.0)
  call list%insert(0, 1.0)
  print *, list

  ! Test set
  call list%set(1, 1)
  call list%set(1, 1_INT64)
  call list%set(1, 1.0)
  call list%set(1, 1.0d0)
  call list%set(1, "abc")
  call list%set(1, .True.)

end subroutine adding_test

subroutine count_test ()
  implicit none
  type(list_t) :: list
  integer :: cnt

  list = list_t([1, 2, 1, 2, 1])

  cnt = list%len()
  if (cnt /= 5) error stop

  cnt = list%count(1)
  if (cnt /= 3) error stop

end subroutine count_test

subroutine index_test ()
  implicit none
  type(list_t) :: list
  integer :: pos

  ! Construct list
  call list%append(1)
  call list%append(1_INT64)
  call list%append(1.0)
  call list%append(1.0d0)
  call list%append("abc")
  call list%append(.True.)

  ! Inquire form list
  pos = list%index(1)
  if (pos /= 1) error stop
  pos = list%index(1_INT64)
  if (pos /= 2) error stop
  pos = list%index(1.0)
  if (pos /= 3) error stop
  pos = list%index(1.0d0)
  if (pos /= 4) error stop
  pos = list%index("abc")
  if (pos /= 5) error stop
  pos = list%index(.True.)
  if (pos /= 6) error stop

end subroutine index_test

subroutine removing_test ()
  implicit none
  type(list_t) :: list
  integer :: cnt

  ! Test clear
  list = list_t([1, 2, 3, 4, 5])
  call list%clear()
  cnt = list%len()
  if (cnt /= 0) error stop  

  ! Test pop
  list = list_t([1, 2, 3, 4, 5])
  call list%pop(1)
  call list%pop(10000)
  call list%pop(0)

end subroutine removing_test

subroutine get_test()
  implicit none
  type(list_t) :: list
  integer(INT32) :: i32
  integer(INT64) :: i64
  real(REAL32) :: r32
  real(REAL64) :: r64
  logical :: bool
  character(128) :: str

  ! Construct list
  call list%append(1)
  call list%append(1_INT64)
  call list%append(1.0)
  call list%append(1.0d0)
  call list%append(.True.)
  call list%append("abc")

  ! Try getting items from list
  call list%get(1, i32)
  call list%get(1, i64)
  call list%get(1, r32)
  call list%get(1, r64)
  call list%get(1, str)
  print *, i32, i64, r32, r64, trim(str)

  call list%get(2, i32)
  call list%get(2, i64)
  call list%get(2, r32)
  call list%get(2, r64)
  call list%get(2, str)
  print *, i32, i64, r32, r64, trim(str)

  call list%get(3, i32)
  call list%get(3, i64)
  call list%get(3, r32)
  call list%get(3, r64)
  call list%get(3, str)
  print *, i32, i64, r32, r64, trim(str)

  call list%get(4, i32)
  call list%get(4, i64)
  call list%get(4, r32)
  call list%get(4, r64)
  call list%get(4, str)
  print *, i32, i64, r32, r64, trim(str)

  call list%get(5, bool)
  call list%get(5, str)
  print *, bool, trim(str)

  call list%get(6, str)
  print *, trim(str)

end subroutine get_test

subroutine type_test()
  implicit none
  type(list_t) :: list

  ! Test append
  call list%append(1)
  call list%append(1_INT64)
  call list%append(1.0)
  call list%append(1.0d0)
  call list%append("abc")
  call list%append(.True.)

  ! Check if right type
  if (list%same_type_as(1, 1.0)) error stop
  if (.not. list%same_type_as(1, 1)) error stop
  if (.not. list%same_type_as(2, 1_INT64)) error stop
  if (.not. list%same_type_as(3, 1.0)) error stop
  if (.not. list%same_type_as(4, 1.0d0)) error stop
  if (.not. list%same_type_as(5, "abc")) error stop
  if (.not. list%same_type_as(6, .True.)) error stop

end subroutine type_test

subroutine other_test()
  implicit none
  type(list_t) :: list
  integer :: val

  ! Reverse list
  list = list_t([1, 2, 3, 4, 5])
  call list%reverse()
  call list%get(1, val)
  if (val /= 5) error stop

end subroutine other_test

end program main