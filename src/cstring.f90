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

module xslib_cstring
  use iso_fortran_env, only: INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
  implicit none
  private
  public :: str, smerge, toLower, toUpper, toTitle, swapCase, replace, strip, strtok, cnttok,  &
  &  setColor, getColor, isAlpha, isNumber, isSpace, baseName, pathName, extension

  interface str
    module procedure :: str, stra
  end interface str

contains

! Converts argument of any kind to character.
function str (value, fmt) result (out)
  implicit none
  character(:), allocatable :: out
  class(*), intent(in) :: value
  character(*), intent(in), optional :: fmt
  character(128) :: tmp

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

! Converts array argument of any kind to character.
function stra (value, fmt, delim) result (out)
  implicit none
  character(:), allocatable :: out
  class(*), intent(in) :: value(:)
  character(*), intent(in), optional :: fmt, delim
  integer :: i

  out = str(value(1),fmt)
  do i = 2, size(value)
    if (present(delim)) then
      out = out // trim(delim) // str(value(i), fmt)
    else
      out = out // " " // str(value(i), fmt)
    end if
  end do

end function stra

! -------------------------------------------------

! Merge two stings of arbitrary length based on logical mask.
function smerge (tsource, fsource, mask) result (out)
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: tsource, fsource
  logical, intent(in) :: mask

  if (mask) then
    out = tsource
  else
    out = fsource
  end if

end function smerge

! Return string in all lower case.
function toLower (string) result (out)
  implicit none
  character(*), intent(in) :: string
  character(len(string)) :: out
  integer, parameter :: offset = ichar("a") - ichar("A")
  integer :: i

  out = trim(string)
  do i = 1, len_trim(out)
    select case (out(i:i))
    case ("A" : "Z")
      out(i:i) = char( ichar(out(i:i)) + offset)
    end select
  end do

end function toLower

! Returns string in all upper case.
function toUpper (string) result (out)
  implicit none
  character(*), intent(in) :: string
  character(len(string)) :: out
  integer, parameter :: offset = ichar("a") - ichar("A")
  integer :: i

  out = trim(string)
  do i = 1, len_trim(out)
    select case (out(i:i))
    case ("a" : "z")
      out(i:i) = char(ichar(out(i:i)) - offset)
    end select
  end do

end function toUpper

! Return string where all words are capitalized.
function toTitle (string) result (out)
  implicit none
  character(*), intent(in) :: string
  character(len(string)) :: out
  integer, parameter :: offset = ichar("a") - ichar("A")
  integer :: i

  out = trim(string)
  select case (out(1:1))
  case ("a" : "z")
    out(1:1) = char(ichar(out(1:1)) - offset)
  end select
  do i = 2, len_trim(out)
    if (out(i-1:i-1) == " ") then
      select case (out(i:i))
      case ("a" : "z")
        out(i:i) = char(ichar(out(i:i)) - offset)
      end select
    end if 
  end do

end function toTitle

! Return string all cases have been fliped.
function swapCase (string) result (out)
  implicit none
  character(*), intent(in) :: string
  character(len(string)) :: out
  integer, parameter :: offset = ichar("a") - ichar("A")
  integer :: i

  out = trim(string)
  do i = 1, len_trim(out)
    select case (out(i:i))
    case ("a" : "z")
      out(i:i) = char(ichar(out(i:i)) - offset)
    case ("A" : "Z")
      out(i:i) = char(ichar(out(i:i)) + offset)
    end select
  end do

end function swapCase

! Removes all characters trailing sign.
function strip (string, delim) result (out)
  implicit none
  character(*), intent(in) :: string
  character(*), intent(in) :: delim
  character(len(string)) :: out
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

! Replace OLD with NEW within STRING.
function replace (string, old, new) result (out)
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: string, old, new
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

! -------------------------------------------------

! Check if string is ASCII alphabetical character element wise.
function isAlpha (string) result (out)
  implicit none
  character(*), intent(in) :: string
  logical :: out
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

! heck if string is ASCII number character element wise.
function isNumber (string) result (out)
  implicit none
  character(*), intent(in) :: string
  logical :: out
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

end function isNumber

! Check if character is space.
function isSpace (string) result (out)
  implicit none
  character(*), intent(in) :: string
  logical :: out
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

! -------------------------------------------------

! Add some colors in your life & set terminal color. ATTR=attribute, FG=foreground, BG=background
! * Colors: black, red, yellow, blue, magenta, cyan, white, and light variants (e.g. lightblue)
! * Attributes: bold, dim, underline, blink, reverse and  hidden
! NOTE: Max len. of output string is 11; "e[*;**;***m"
function getColor (fg, bg, attr) result (out)
  implicit none
  character(:), allocatable :: out
  character(*), intent(in), optional :: fg, bg, attr
  character, parameter :: esc = char(27)

  ! <ESC>[{attr};{fg};{bg};"
  ! SOURCE: https://www.linuxjournal.com/article/8603
  ! SOURCE: https://misc.flogisoft.com/bash/tip_colors_and_formatting

  ! Start sequence
  out = esc // "["

  ! Select attribute
  if (present(attr)) then
    select case (toLower(attr))
    case ("bold", "bright")
      out = out // "1"
    case ("dim")
      out = out // "2"
    case ("underline")
      out = out // "4"
    case ("blink")
      out = out // "5"
    case ("reverse")
      out = out // "7"
    case ("hidden")
      out = out // "8"
    case default
      out = out // "0"
    end select
  end if

  ! Select color
  if (present(fg)) then
    select case (toLower(fg))
    case ("black")
      out = out // ";30"
    case ("red")
      out = out // ";31"
    case ("green")
      out = out // ";32"
    case ("yellow")
      out = out // ";33"
    case ("blue")
      out = out // ";34"
    case ("magenta")
      out = out // ";35"
    case ("cyan")
      out = out // ";36"
    case ("white")
      out = out // ";37"
    case ("lightblack")
      out = out // ";90"
    case ("lightred")
      out = out // ";91"
    case ("lightgreen")
      out = out // ";92"
    case ("lightyellow")
      out = out // ";93"
    case ("lightblue")
      out = out // ";94"
    case ("lightmagenta")
      out = out // ";95"
    case ("lightcyan")
      out = out // ";96"
    case ("lightwhite")
      out = out // ";97"
    case default
      out = out // ";39"
    end select
  end if

  ! Select background
  if (present(bg)) then
    select case (toLower(bg))
    case ("black")
      out = out // ";040"
    case ("red")
      out = out // ";041"
    case ("green")
      out = out // ";042"
    case ("yellow")
      out = out // ";043"
    case ("blue")
      out = out // ";044"
    case ("magenta")
      out = out // ";045"
    case ("cyan")
      out = out // ";046"
    case ("white")
      out = out // ";047"
    case ("lightblack")
      out = out // ";100"
    case ("lightred")
      out = out // ";101"
    case ("lightgreen")
      out = out // ";102"
    case ("lightyellow")
      out = out // ";103"
    case ("lightblue")
      out = out // ";104"
    case ("lightmagenta")
      out = out // ";105"
    case ("lightcyan")
      out = out // ";106"
    case ("lightwhite")
      out = out // ";107"
    case default
      out = out // ";049"
    end select
  end if

  ! End sequence
  out = out // "m"

  return
end function getColor

! Wrapper for getColor().
function setColor (string, attr, fg, bg) result (out)
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: string
  character(*), intent(in), optional :: fg, bg, attr

  out = getColor(fg, bg, attr) // string // getColor()

end function setColor

! -------------------------------------------------
! String tokenization

! Breaks string str into a series of tokens using the delimiter delim.
! * Works in same way as strtok in C, except instead of NULL feed `char(0)`
function strtok (string, delim) 
  implicit none
  character(:), allocatable :: strtok
  character(*), intent(in) :: string, delim
  character(:), allocatable, save :: saved_string
  integer, save :: saved_start
  integer :: start, finish
  !$OMP THREADPRIVATE (saved_string, saved_start)

  ! SOURCE: http://fortranwiki.org/fortran/show/strtok

  ! initialize stored copy of input string and pointer into input string on first call
  if (string(1:1) /= char(0)) then
    saved_start = 1                 ! beginning of unprocessed data
    saved_string = trim(string)     ! save input string from first call in series
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
    strtok = char(0)
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
  strtok = saved_string(start:finish-1)
  saved_start = finish

end function strtok

! Count number of tokens in string.
integer function cnttok (string, delim)
  implicit none
  character(*), intent(in) :: string
  character(*), intent(in) :: delim
  integer :: i

  cnttok = 0
  i = len_trim(string)
  do while (i > 0)
    cnttok = cnttok + 1
    i = index(trim(string(:i-1)), delim, BACK=.True.)
  end do

end function cnttok

! -------------------------------------------------
! File manipulation

! Returns basename of file. Eg. "/path/to/file.txt" = "file"
function basename (string) result (out)
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: string
  integer :: i, j

  i = 1 + index(string, "/", BACK=.true.)
  j = index(string, ".", BACK=.true.)
  j = merge(j-1, len_trim(string), j /= 0)
  out = string(i:j)

end function basename

! Returns pathname of file. Eg. "/path/to/file.txt" = "/path/to"
! TODO: Account for different OS: Windows, Linux, or iOS
function pathname (string) result (out)
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: string
  integer :: i

  i = index( string, "/", BACK=.true. )
  if (i < 1) then
    out = ""
  else
    out = string(:i-1)
  end if

end function pathname

! Returns extension of file. Eg. "/path/to/file.txt" = "txt"
function extension (string) result (out)
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: string
  integer :: i

  i = index(string, ".", BACK=.true.)
  if ( i == 0 ) then
    out = ""
  else
    out = string(i+1:len_trim(string))
  end if

end function extension

end module xslib_cstring
