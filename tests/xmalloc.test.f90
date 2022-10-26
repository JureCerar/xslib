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
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  use xslib_xmalloc
  implicit none

  call xmalloc_test ()
  call xcalloc_test ()
  call xrealloc_test ()

contains

subroutine xmalloc_test ()
  implicit none
  integer, parameter :: NP = 10
  integer(INT32), allocatable :: i32(:)
  integer(INT64), allocatable :: i64(:)
  real(REAL32), allocatable :: r32(:)
  real(REAL64), allocatable :: r64(:)
  logical, allocatable :: bool(:)
  character(32), allocatable :: string(:)
  character(128) :: errmsg
  integer :: i, stat

  ! Allocate data.
  do i = 1, 2
    call xmalloc (i32, [NP])
    if (.not. allocated(i32) .or. size(i32) /= NP) error stop
    call xmalloc (i64, [NP])
    if (.not. allocated(i64) .or. size(i64) /= NP) error stop
    call xmalloc (r32, [NP])
    if (.not. allocated(r32) .or. size(r32) /= NP) error stop
    call xmalloc (r64, [NP])
    if (.not. allocated(r64) .or. size(r64) /= NP) error stop
    call xmalloc (bool, [NP])
    if (.not. allocated(bool) .or. size(bool) /= NP) error stop
    call xmalloc (string, [NP])
    if (.not. allocated(string) .or. size(string) /= NP) error stop

  end do ! for i

  ! Allocate data w/ optional parameters.
  call xmalloc (i32, [NP], stat, errmsg)
  call xmalloc (i64, [NP], stat, errmsg)
  call xmalloc (r32, [NP], stat, errmsg)
  call xmalloc (r64, [NP], stat, errmsg)
  call xmalloc (bool, [NP], stat, errmsg)
  call xmalloc (string, [NP], stat, errmsg)

end subroutine xmalloc_test

subroutine xcalloc_test ()
  implicit none
  integer, parameter :: NP = 10
  integer(INT32), allocatable :: i32(:)
  integer(INT64), allocatable :: i64(:)
  real(REAL32), allocatable :: r32(:)
  real(REAL64), allocatable :: r64(:)
  logical, allocatable :: bool(:)
  character(32), allocatable :: string(:)
  character(128) :: errmsg
  integer :: i, stat

  ! Allocate data.
  do i = 1, 2
    call xcalloc (i32, [NP])
    if (.not. allocated(i32) .or. size(i32) /= NP .or. all(i32 /= 0)) error stop
    call xcalloc (i64, [NP])
    if (.not. allocated(i64) .or. size(i64) /= NP .or. all(i64 /= 0)) error stop
    call xcalloc (r32, [NP])
    if (.not. allocated(r32) .or. size(r32) /= NP .or. all(r32 /= 0.0)) error stop
    call xcalloc (r64, [NP])
    if (.not. allocated(r64) .or. size(r64) /= NP .or. all(r64 /= 0.0)) error stop
    call xcalloc (bool, [NP])
    if (.not. allocated(bool) .or. size(bool) /= NP .or. .not. all(bool)) error stop
    call xcalloc (string, [NP])
    if (.not. allocated(string) .or. size(string) /= NP .or. all(string /= "")) error stop

  end do ! for i
  
  ! Allocate data w/ optional parameters.
  call xcalloc (i32, [NP], stat, errmsg)
  call xcalloc (i64, [NP], stat, errmsg)
  call xcalloc (r32, [NP], stat, errmsg)
  call xcalloc (r64, [NP], stat, errmsg)
  call xcalloc (bool, [NP], stat, errmsg)
  call xcalloc (string, [NP], stat, errmsg)

end subroutine xcalloc_test

subroutine xrealloc_test ()
  implicit none
  integer, parameter :: NP = 10
  integer(INT32), allocatable :: i32(:)
  integer(INT64), allocatable :: i64(:)
  real(REAL32), allocatable :: r32(:)
  real(REAL64), allocatable :: r64(:)
  logical, allocatable :: bool(:)
  character(32), allocatable :: string(:)
  character(128) :: errmsg
  integer :: i, stat

  ! Allocate data. Try allocating alredy allocated data.
  do i = 1, 2
    call xrealloc (i32, [NP])
    if (.not. allocated(i32) .or. size(i32) /= NP .or. all(i32 /= 0)) error stop
    call xrealloc (i64, [NP])
    if (.not. allocated(i64) .or. size(i64) /= NP .or. all(i64 /= 0)) error stop
    call xrealloc (r32, [NP])
    if (.not. allocated(r32) .or. size(r32) /= NP .or. all(r32 /= 0.0)) error stop
    call xrealloc (r64, [NP])
    if (.not. allocated(r64) .or. size(r64) /= NP .or. all(r64 /= 0.0)) error stop
    call xrealloc (bool, [NP])
    if (.not. allocated(bool) .or. size(bool) /= NP .or. .not. all(bool)) error stop
    call xrealloc (string, [NP])
    if (.not. allocated(string) .or. size(string) /= NP .or. all(string /= "")) error stop
  end do

  ! Set dummy data
  i32 = 1
  i64 = 1
  r32 = 1.0
  r64 = 1.0
  bool = .True.
  string = "Foo"

  ! Reallocate bigger data set.
  call xrealloc (i32, [2*NP], stat, errmsg)
  if (.not. allocated(i32) .or. size(i32) /= 2*NP .or. all(i32(:NP) /= 1)) error stop
  call xrealloc (i64, [2*NP], stat, errmsg)
  if (.not. allocated(i64) .or. size(i64) /= 2*NP .or. all(i64(:NP) /= 1)) error stop
  call xrealloc (r32, [2*NP], stat, errmsg)
  if (.not. allocated(r32) .or. size(r32) /= 2*NP .or. all(r32(:NP) /= 1.0)) error stop
  call xrealloc (r64, [2*NP], stat, errmsg)
  if (.not. allocated(r64) .or. size(r64) /= 2*NP .or. all(r64(:NP) /= 1.0)) error stop
  call xrealloc (bool, [2*NP], stat, errmsg)
  if (.not. allocated(r64) .or. size(r64) /= 2*NP .or. .not. all(bool(:NP))) error stop
  call xrealloc (string, [2*NP], stat, errmsg)
  if (.not. allocated(string) .or. size(string) /= 2*NP .or. all(string(:NP) /= "Foo")) error stop

  ! Reallocate smaller data set.
  call xrealloc (i32, [NP/2], stat, errmsg)
  if (.not. allocated(i32) .or. size(i32) /= NP/2 .or. all(i32(:NP/2) /= 1)) error stop
  call xrealloc (i64, [NP/2], stat, errmsg)
  if (.not. allocated(i64) .or. size(i64) /= NP/2 .or. all(i64(:NP/2) /= 1)) error stop
  call xrealloc (r32, [NP/2], stat, errmsg)
  if (.not. allocated(r32) .or. size(r32) /= NP/2 .or. all(r32(:NP/2) /= 1.0)) error stop
  call xrealloc (r64, [NP/2], stat, errmsg)
  if (.not. allocated(r64) .or. size(r64) /= NP/2 .or. all(r64(:NP/2) /= 1.0)) error stop
  call xrealloc (bool, [NP/2], stat, errmsg)
  if (.not. allocated(r64) .or. size(r64) /= NP/2 .or. .not. all(bool(:NP/2))) error stop
  call xrealloc (string, [NP/2], stat, errmsg)
  if (.not. allocated(string) .or. size(string) /= NP/2 .or. all(string(:NP/2) /= "Foo")) error stop

  ! Reallocate data w/ optional parameters.
  call xrealloc (i32, [NP], stat, errmsg)
  call xrealloc (i64, [NP], stat, errmsg)
  call xrealloc (r32, [NP], stat, errmsg)
  call xrealloc (r64, [NP], stat, errmsg)
  call xrealloc (bool, [NP], stat, errmsg)
  call xrealloc (string, [NP], stat, errmsg)

end subroutine xrealloc_test

end program main