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

module xslib_cstring
  !! Module for manipulating character strings.
  use iso_fortran_env, only: INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
  implicit none
  private
  public :: str, join, smerge, toLower, toUpper, toTitle, swapCase
  public :: replace, strip, strtok, cnttok, isAlpha, isDigit, isSpace

  
  interface str
    !! Converts the specified VALUE or ARRAY (of any kind) into a string. Optionally,
    !! output format can be defined with `fmt` argument. In case input values is an ARRAY
    !! a custom delimiter can be defined (default is whitespace).
    !! 
    !! Example
    !! ```Fortran
    !! print *, str(1)
    !! > "1"
    !! print *, str(1.0, FMT="(f5.3)")
    !! > "1.000"
    !! print *, str([0, 1, 2], DELIM=",")
    !! > "0,1,2"
    !! ```
    module procedure :: str, join
  end interface str


contains

function str (value, fmt) result (out)
  !! Converts the specified VALUE (of any kind) into a string. Optionally,
  !! output format can be defined with `fmt` argument.
  !! 
  !! Example
  !! ```Fortran
  !! print *, str(1)
  !! "1"
  !! print *, str(1.0, FMT="(f5.3)")
  !! "1.000"
  !! ```
  implicit none
  character(:), allocatable :: out
  !! Output string.
  class(*), intent(in) :: value
  !! Value of any kind to be transformed into character. 
  character(*), intent(in), optional :: fmt
  !! Valid fortran format specifier. Default is compiler representation.
  character(265) :: tmp

  select type (value)
  type is (integer(INT8))
    if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (integer(INT16))
    if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (integer(INT32))
    if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (integer(INT64))
      if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (real(REAL32))
      if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (real(REAL64))
      if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (real(REAL128))
      if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (complex(REAL32))
    if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (complex(REAL64))
    if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (complex(REAL128))
    if (present(fmt)) then
      write (tmp, fmt) value
    else 
      write (tmp, *) value
    end if

  type is (logical)
    if (present(fmt)) then
      write (tmp, fmt) value
    else 
      tmp = merge("True ", "False", value)
    end if

  type is (character(*))
    tmp = trim(adjustl(value))

  class default
    ! Well, we tried our best... can't have them all.
    error stop "Usupported variable KIND"

  end select

  out = trim(adjustl(tmp))

end function str


function join (array, fmt, delim) result (out)
  !! Converts the specified ARRAY (of any kind) into a string. Optionally,
  !! output format can be defined with `fmt` argument. Additionally,
  !! a custom delimiter can be defined (default is whitespace).
  !!
  !! Example
  !! ```Fortran
  !! print *, join([0,1,2])
  !! > "0 1 2"
  !! print *, join([0,1,2], FMT="I0.2")
  !! > "00 01 02"
  !! print *, join([0,1,2], DELIM=",")
  !! > "0,1,2"
  !! ```
  implicit none
  character(:), allocatable :: out
  !!  Output string.
  class(*), intent(in) :: array(:)
  !! Array of any kind or shape to be transformed into character.
  character(*), intent(in), optional :: fmt
  !! Valid fortran format specifier. Default is compiler representation.
  character(*), intent(in), optional :: delim
  !! Separator to use when joining the string. Default is whitespace.
  integer :: i

  out = str(array(1),fmt)
  do i = 2, size(array)
    if (present(delim)) then
      out = out // trim(delim) // str(array(i), fmt)
    else
      out = out // " " // str(array(i), fmt)
    end if
  end do

end function join


function smerge (tsource, fsource, mask) result (out)
  !! Select values two arbitrary length stings according to a logical mask. The result
  !! is equal to `tsource` if `mask` is `.True.`, or equal to `fsource` if it is `.False.`.  
  !! 
  !! Example:
  !! ```fortran
  !! print *, smerge("Top", "Bottom", .True.)
  !! > "Top"
  !! print *, smerge("Top", "Bottom", .False.)
  !! > "Bottom"
  !! ```
  implicit none
  character(:), allocatable :: out
  !! Return string of variable length.
  character(*), intent(in) :: tsource
  !! Return string if mask is `.True.`.
  character(*), intent(in) :: fsource
  !! Return string if mask is `.False.`.
  logical, intent(in) :: mask
  !! Selection logical mask.

  if (mask) then
    out = tsource
  else
    out = fsource
  end if

end function smerge


function toLower (string) result (out)
  !! Return the string with all the cased characters are converted to lowercase.
  !!
  !! Example:  
  !! ```fortran
  !! print *, toLower("Hello, WORLD!")
  !! > "hello, world!"
  !! ```
  implicit none
  character(*), intent(in) :: string
  !! Input string.
  character(len(string)) :: out
  !! Output string. Same length as input string.
  integer, parameter :: OFFSET = ichar("a") - ichar("A")
  integer :: i

  out = trim(string)
  do i = 1, len_trim(out)
    select case (out(i:i))
    case ("A" : "Z")
      out(i:i) = char(ichar(out(i:i)) + OFFSET)
    end select
  end do

end function toLower


function toUpper (string) result (out)
  !! Return the string with all the cased characters are converted to uppercase.
  !!
  !! Example:  
  !! ```fortran
  !! print *, toLower("Hello, WORLD!")
  !! > "HELLO, WORLD!"
  !! ```
  implicit none
  character(*), intent(in) :: string
  !! Input string.
  character(len(string)) :: out
  !! Output string. Same length as input string.
  integer, parameter :: OFFSET = ichar("a") - ichar("A")
  integer :: i

  out = trim(string)
  do i = 1, len_trim(out)
    select case (out(i:i))
    case ("a" : "z")
      out(i:i) = char(ichar(out(i:i)) - OFFSET)
    end select
  end do

end function toUpper


function toTitle (string) result (out)
  !! Return a titlecased version of the string where words start with an
  !! uppercase character and the remaining characters are lowercase.
  !!
  !! Example:  
  !! ```fortran
  !! print *, toLower("hello, world!")
  !! > "Hello, World!"
  !! ```
  implicit none
  character(*), intent(in) :: string
  character(len(string)) :: out
  integer, parameter :: OFFSET = ichar("a") - ichar("A")
  integer :: i

  out = trim(string)
  select case (out(1:1))
  case ("a" : "z")
    out(1:1) = char(ichar(out(1:1)) - OFFSET)
  end select
  do i = 2, len_trim(out)
    if (out(i-1:i-1) == " ") then
      select case (out(i:i))
      case ("a" : "z")
        out(i:i) = char(ichar(out(i:i)) - OFFSET)
      end select
    end if 
  end do

end function toTitle


function swapCase (string) result (out)
  !! Return a string with uppercase characters converted to lowercase and vice versa.
  !! Note that it is not necessarily true that `swapCase(swapCase(s)) == s`.
  !! 
  !! Example:
  !! ```fortran
  !! > swapCase("Hello, WORLD!")
  !! "hELLO, world!"
  !! ```
  implicit none
  character(*), intent(in) :: string
  character(len(string)) :: out
  integer, parameter :: OFFSET = ichar("a") - ichar("A")
  integer :: i

  out = trim(string)
  do i = 1, len_trim(out)
    select case (out(i:i))
    case ("a" : "z")
      out(i:i) = char(ichar(out(i:i)) - OFFSET)
    case ("A" : "Z")
      out(i:i) = char(ichar(out(i:i)) + OFFSET)
    end select
  end do

end function swapCase


function strip (string, delim) result (out)
  !! Return a string with the leading and trailing characters removed. The `delim` argument is 
  !! a string specifying the characters after which characters to be removed. If omitted,
  !! the argument defaults to removing whitespace. Useful for removing "comments" from a string.
  !!
  !! Example:
  !! ```Fortran
  !! print *, strip("Hello, WORLD!", ",")
  !! > "Hello"
  !! ```
  implicit none
  character(*), intent(in) :: string
  !! Input string.
  character(*), intent(in) :: delim
  !! Separator use for delimiting a string.
  character(len(string)) :: out
  !! Output string.
  integer :: i

  i = index(string, delim)
  if (i == 0) then
    out = trim(string)
  else if (i == 1) then
    out = ""
  else
    out = string(:i-1)
  end if

end function strip


function replace (string, old, new) result (out)
  !! Return a string with all occurrences of substring `old` replaced by `new`.
  !!
  !! Example:
  !!  ```fortran
  !!  print *, replace("Hello, World!", "World", "Universe")
  !!  > "Hello, Universe!"
  !!  ```
  implicit none
  character(:), allocatable :: out
  !! Output string.
  character(*), intent(in) :: string
  !! Input string.
  character(*), intent(in) :: old
  !! The string to search for.
  character(*), intent(in) :: new
  !! The string to replace the old value with.
  integer :: current, next

  current = 1
  out = trim(string)
  do while (.true.)
    next = index(out(current:), old) - 1
    if (next == -1) exit
    out = out(:current+next-1) // new // out(current+next+len(old):)
    current = current + len(new)
  end do

end function replace


function isAlpha (string) result (out)
  !! Return `.True.` if all characters in the string are alphabetic and
  !! there is at least one character, `.False.` otherwise.
  !!
  !! Example:
  !! ```Fortran
  !! print *, isAlpha("ABC")
  !! > .True.
  !! print *, isAlpha("123")
  !! > .False.
  !! ```
  implicit none
  character(*), intent(in) :: string
  !! Input string.
  logical :: out
  !! Are all characters in the string an ASCII alphabetical characters.
  integer :: i

  out = .False.
  do i = 1, len_trim(string)
    select case (string(i:i))
    case ("A":"Z", "a":"z")
      out = .True.
    case default
      out = .False.
      exit
    end select
  end do

end function isAlpha


function isDigit (string) result (out)
  !! Return `.True.` if all characters in the string are numeric characters
  !! and there is at least one character, `.False.` otherwise.
  !!
  !! Example:
  !! ```Fortran
  !! print *, isDigit("ABC")
  !! > .False.
  !! print *, isDigit("123")
  !! > .True.
  !! ```
  implicit none
  character(*), intent(in) :: string
  !! Input string.
  logical :: out
  !! Are all characters in the string an numeric characters.
  integer :: i

  out = .False.
  do i = 1, len_trim(string)
    select case (string(i:i))
    case ("0":"9")
      out = .True.
    case default
      out = .False.
      exit
    end select
  end do

end function isDigit


function isSpace (string) result (out)
  !! Return `.True.` if there are only whitespace characters in the string
  !! and there is at least one character, `.False.` otherwise.
  !! 
  !! Example:
  !! ```Fortran
  !! > isSpace(" ")
  !! .True.
  !! > isSpace("A")
  !! .False.
  !! ```
  implicit none
  character(*), intent(in) :: string
  !! Input string.
  logical :: out
  !! Are all characters in the string an whitespaces.
  integer :: i

  out = .False.
  do i = 1, len(string)
    if (string(i:i) == " ") then
      out = .True.
    else
      out = .False.
      exit
    end if
  end do

end function isSpace


function strtok (delim, string) result (out)
  !! A sequence of calls to this function split string into tokens, which are sequences
  !! of contiguous characters separated by the delimiter `delim`.
  !!
  !! On a first call, the function returns first token and saves the string. In subsequent
  !! calls, if string parameter is not provided function continues at the position right 
  !! after the end of the last token as the new starting location for scanning.
  !!
  !! Once the end character of string is found in a call to `strtok`, all subsequent calls to
  !! this function return a null (`char(0)`) character.
  !!
  !! See [`strtok`](https://cplusplus.com/reference/cstring/strtok) for full reference.
  !!
  !! @note@
  !! Should be thread-safe with OpenMP.
  !! @endnote@
  !!
  !! Example:
  !! ```Fortran
  !! print *, strtok(" ", "Hello, World!")
  !! >>> "Hello,"
  !! print *, strtok(" ")
  !! >>> "World!"
  !! print *, strtok(" ")
  !! >>> NULL
  !! ```
  !!
  !! ```Fortran
  !! str = strtok(" ", "Hello, World!")
  !! do while (str /= char(0))
  !!     print *, str
  !!     str = strtok(" ")
  !! end do
  !! >>> "Hello,"
  !! ... "World!"
  !! ```
  implicit none
  character(:), allocatable :: out
  !! Next output character token.
  character(*), intent(in) :: delim
  !! Separator use for delimiting a string.
  character(*), intent(in), optional :: string
  !! Input string. If not present get next token on previous (saved) string.
  character(:), allocatable, save :: saved_string
  integer, save :: saved_start
  integer :: start, finish
  !$OMP THREADPRIVATE (saved_string, saved_start)

  ! SOURCE: http://fortranwiki.org/fortran/show/strtok

  ! Initialize stored copy of input string and pointer into input string on first call
  if (present(string)) then
    saved_start = 1                 ! Beginning of unprocessed data
    saved_string = trim(string)     ! Save input string from first call in series
  endif

  ! Start from where we left
  start = saved_start

  ! Skip until next non-delimiter
  do while (start <= len(saved_string))
    if (index(delim, saved_string(start:start)) /= 0) then
      start = start + 1
    else
      exit
    end if
  end do

  ! If we reach end of string
  if (start > len(saved_string)) then
    out = char(0)
    return
  end if

  ! Find next delimiter
  finish = start
  do while (finish <= len(saved_string))
    if (index(delim, saved_string(finish:finish)) == 0) then
      finish = finish + 1
    else
      exit
    end if
  end do

  ! Set result and update where we left
  out = saved_string(start:finish-1)
  saved_start = finish

end function strtok


function cnttok (string, delim) result (out)
  !! Count number of tokens in a string separated by a delimiter `delim`.
  !!
  !! Example:
  !! ```Fortran
  !! print *, cnttok("Hello, World!", " ")
  !! > 2 
  !! ```
  implicit none
  integer :: out
  !! Number of tokens in the string.
  character(*), intent(in) :: string
  !! Input string.
  character(*), intent(in) :: delim
  !! Separator used for delimiting a string.  
  integer :: i

  out = 0
  i = len_trim(string)
  do while (i > 0)
    out = out + 1
    i = index(trim(string(:i-1)), delim, BACK=.True.)
  end do

end function cnttok


end module xslib_cstring
