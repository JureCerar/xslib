! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2024 Jure Cerar
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
    use xslib_dict
    implicit none

    call hash_test ()
    call put_get_test ()
    call remove_test ()
    call count_test ()
    call type_test ()
    call iterate_test ()
    call resize_test ()

contains

function random_string (len) result (string)
    !! Helper: Generate random string of length 'len'
    implicit none
    integer, intent(IN) :: len
    character(len) :: string
    integer :: i

    do i = 1, len
        string(i:i) = achar(32 + int((126 - 32 + 1) * rand())) 
    end do
    
end function random_string


subroutine hash_test ()
    implicit none
    type(dict_t) :: dict
    integer :: hash

    ! Try Different variable types
    print *, dict%hash_function(1)
    print *, dict%hash_function(1_INT64)
    print *, dict%hash_function(1.0)
    print *, dict%hash_function(1.0d0)
    print *, dict%hash_function((1.0, 1.0))
    print *, dict%hash_function((1.0d0, 1.0d0))
    print *, dict%hash_function("string")

    ! Same input should produce same hash
    if (dict%hash_function(1) /= dict%hash_function(1)) error stop
    if (dict%hash_function(1_INT64) /= dict%hash_function(1_INT64)) error stop
    if (dict%hash_function(1.0) /= dict%hash_function(1.0)) error stop
    if (dict%hash_function(1.0d0) /= dict%hash_function(1.0d0)) error stop
    if (dict%hash_function((1.0, 1.0)) /= dict%hash_function((1.0, 1.0))) error stop
    if (dict%hash_function((1.0d0, 1.0d0)) /= dict%hash_function((1.0d0, 1.0d0))) error stop
    if (dict%hash_function("string") /= dict%hash_function("string")) error stop

end subroutine hash_test


subroutine put_get_test ()
    implicit none
    type(dict_t) :: dict
    integer(INT32) :: i32
    integer(INT64) :: i64
    real(REAL32) :: f32
    real(REAL32) :: f64
    complex(REAL32) :: c32
    complex(REAL32) :: c64
    character(128) :: s
    logical :: b

    ! First lets try adding every combination to dictionary

    ! Integer key
    call dict%put(1, 0)
    call dict%put(2, 0_INT64)
    call dict%put(3, 0.0)
    call dict%put(4, 0.0d0)
    call dict%put(5, (0.0, 0.0))
    call dict%put(6, (0.0d0, 0.0d0))
    call dict%put(7, .True.)
    call dict%put(8, "zero")

    ! Long key
    call dict%put(1_INT64, 0)
    call dict%put(2_INT64, 0_INT64)
    call dict%put(3_INT64, 0.0)
    call dict%put(4_INT64, 0.0d0)
    call dict%put(5_INT64, (0.0, 0.0))
    call dict%put(6_INT64, (0.0d0, 0.0d0))
    call dict%put(7_INT64, .True.)
    call dict%put(8_INT64, "zero")

    ! Float key
    call dict%put(1.0, 0)
    call dict%put(2.0, 0_INT64)
    call dict%put(3.0, 0.0)
    call dict%put(4.0, 0.0d0)
    call dict%put(5.0, (0.0, 0.0))
    call dict%put(6.0, (0.0d0, 0.0d0))
    call dict%put(7.0, .True.)
    call dict%put(8.0, "zero")

    ! Double key
    call dict%put(1.0d0, 0)
    call dict%put(2.0d0, 0_INT64)
    call dict%put(3.0d0, 0.0)
    call dict%put(4.0d0, 0.0d0)
    call dict%put(5.0d0, (0.0, 0.0))
    call dict%put(6.0d0, (0.0d0, 0.0d0))
    call dict%put(7.0d0, .True.)
    call dict%put(8.0d0, "zero")

    ! Complex key
    call dict%put((1.0, 1.0), 0)
    call dict%put((2.0, 2.0), 0_INT64)
    call dict%put((3.0, 3.0), 0.0)
    call dict%put((4.0, 4.0), 0.0d0)
    call dict%put((5.0, 5.0), (0.0, 0.0))
    call dict%put((6.0, 6.0), (0.0d0, 0.0d0))
    call dict%put((7.0, 7.0), .True.)
    call dict%put((8.0, 8.0), "zero")

    ! Double Complex key
    call dict%put((1.0d0, 1.0d0), 0)
    call dict%put((2.0d0, 2.0d0), 0_INT64)
    call dict%put((3.0d0, 3.0d0), 0.0)
    call dict%put((4.0d0, 4.0d0), 0.0d0)
    call dict%put((5.0d0, 5.0d0), (0.0, 0.0))
    call dict%put((6.0d0, 6.0d0), (0.0d0, 0.0d0))
    call dict%put((7.0d0, 7.0d0), .True.)
    call dict%put((8.0d0, 8.0d0), "zero")

    ! Character key
    call dict%put("one", 0)
    call dict%put("two", 0_INT64)
    call dict%put("three", 0.0)
    call dict%put("four", 0.0d0)
    call dict%put("five", (0.0, 0.0))
    call dict%put("six", (0.0d0, 0.0d0))
    call dict%put("seven", .True.)
    call dict%put("eight", "zero")

    ! Now lets try retrieving values

    ! Integer key
    call dict%get(1, i32)
    call dict%get(2, i64)
    call dict%get(3, f32)
    call dict%get(4, f64)
    call dict%get(5, c32)
    call dict%get(6, c64)
    call dict%get(7, b)
    call dict%get(8, s)
    print *, i32, i64, f32, f64, c32, c64, b, s

    ! Long key
    call dict%get(1_INT64, i32)
    call dict%get(2_INT64, i64)
    call dict%get(3_INT64, f32)
    call dict%get(4_INT64, f64)
    call dict%get(5_INT64, c32)
    call dict%get(6_INT64, c64)
    call dict%get(7_INT64, b)
    call dict%get(8_INT64, s)
    print *, i32, i64, f32, f64, c32, c64, b, s

    ! Float key
    call dict%get(1.0, i32)
    call dict%get(2.0, i64)
    call dict%get(3.0, f32)
    call dict%get(4.0, f64)
    call dict%get(5.0, c32)
    call dict%get(6.0, c64)
    call dict%get(7.0, b)
    call dict%get(8.0, s)
    print *, i32, i64, f32, f64, c32, c64, b, s

    ! Double key
    call dict%get(1.0d0, i32)
    call dict%get(2.0d0, i64)
    call dict%get(3.0d0, f32)
    call dict%get(4.0d0, f64)
    call dict%get(5.0d0, c32)
    call dict%get(6.0d0, c64)
    call dict%get(7.0d0, b)
    call dict%get(8.0d0, s)
    print *, i32, i64, f32, f64, c32, c64, b, s

    ! Complex key
    call dict%put((1.0, 1.0), i32)
    call dict%put((2.0, 2.0), i64)
    call dict%put((3.0, 3.0), f32)
    call dict%put((4.0, 4.0), f64)
    call dict%put((5.0, 5.0), c32)
    call dict%put((6.0, 6.0), c64)
    call dict%put((7.0, 7.0), b)
    call dict%put((8.0, 8.0), s)
    print *, i32, i64, f32, f64, c32, c64, b, s

    ! Double Complex key
    call dict%put((1.0d0, 1.0d0), i32)
    call dict%put((2.0d0, 2.0d0), i64)
    call dict%put((3.0d0, 3.0d0), f32)
    call dict%put((4.0d0, 4.0d0), f64)
    call dict%put((5.0d0, 5.0d0), c32)
    call dict%put((6.0d0, 6.0d0), c64)
    call dict%put((7.0d0, 7.0d0), b)
    call dict%put((8.0d0, 8.0d0), s)
    print *, i32, i64, f32, f64, c32, c64, b, s

    ! Character key
    call dict%put("one", i32)
    call dict%put("two", i64)
    call dict%put("three", f32)
    call dict%put("four", f64)
    call dict%put("five", c32)
    call dict%put("six", c64)
    call dict%put("seven", b)
    call dict%put("eight", s)
    print *, i32, i64, f32, f64, c32, c64, b, s

    ! Also test write and print if we already have everything set up    
    write (*, *) dict
    print *, dict

end subroutine put_get_test


subroutine remove_test ()
    implicit none
    type(dict_t) :: dict
    integer :: v

    call dict%put("one", 1)
    call dict%put("two", 1)

    call dict%remove("one")
    call dict%remove("three")

    call dict%get("two", v)
    if (v /= 1) error stop
    call dict%get("one", v, default=0)
    if (v /= 0) error stop

    call dict%clear()
    call dict%get("two", v, default=0)
    if (v /= 0) error stop

end subroutine remove_test


subroutine count_test ()
    implicit none
    type(dict_t) :: dict
    integer :: cnt

    if (dict%len() /= 0) error stop

    call dict%put("one", 1)
    call dict%put("two", 2)
    call dict%put("three", 3)
    call dict%put("four", 4)
    call dict%put("five", 5)

    if (dict%len() /= 5) error stop

    call dict%clear()
    if (dict%len() /= 0) error stop

end subroutine count_test


subroutine type_test()
    implicit none
    type(dict_t) :: dict

    call dict%put("one", 0)
    call dict%put("two", 0_INT64)
    call dict%put("three", 0.0)
    call dict%put("four", 0.0d0)
    call dict%put("five", (0.0, 0.0))
    call dict%put("six", (0.0d0, 0.0d0))
    call dict%put("seven", .True.)
    call dict%put("eight", "zero")

    ! Check if right type
    if (      dict%same_type_as("one", 1.0)) error stop
    if (.not. dict%same_type_as("one", 1)) error stop
    if (.not. dict%same_type_as("two", 1_INT64)) error stop
    if (.not. dict%same_type_as("three", 1.0)) error stop
    if (.not. dict%same_type_as("four", 1.0d0)) error stop
    if (.not. dict%same_type_as("five", (1.0, 1.0))) error stop
    if (.not. dict%same_type_as("six", (1.0d0, 1.0d0))) error stop
    if (.not. dict%same_type_as("seven", .False.)) error stop
    if (.not. dict%same_type_as("eight", "abc")) error stop

end subroutine type_test


subroutine iterate_test()
    implicit none
    type(dict_t) :: dict
    integer :: i
    character(128) :: k, v

    call dict%put("one", 0)
    call dict%put("two", 0_INT64)
    call dict%put("three", 0.0)
    call dict%put("four", 0.0d0)
    call dict%put("five", (0.0, 0.0))
    call dict%put("six", (0.0d0, 0.0d0))
    call dict%put("seven", .True.)
    call dict%put("eight", "zero")

    ! Check keys
    do i = 1, dict%len()
        call dict%keys(i, k)
        print *, i, trim(k)
    end do

    ! Check values
    do i = 1, dict%len()
        call dict%values(i, v)
        print *, i, trim(v)
    end do

    ! Check items
    do i = 1, dict%len()
        call dict%items(i, k, v)
        print *, i, trim(k), trim(v)
    end do

end subroutine iterate_test


subroutine resize_test()
    implicit none
    integer, parameter :: NP = 256 * 256
    type(dict_t) :: dict
    character(16) :: k
    integer :: i, v

    ! Test if dictionary resizes correctly
    call dict%put("one", 1)

    do i  = 1, NP
        k = random_string(len(k))
        call dict%put(k, rand())
    end do

    call dict%get("one", v)
    if (v /= 1) error stop

end subroutine resize_test

end program main 