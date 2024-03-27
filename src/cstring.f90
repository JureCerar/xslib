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
  public :: str, smerge, toLower, toUpper, toTitle, &
  &  swapCase, replace, strip, strtok, cnttok, &
  &  setColor, getColor, isAlpha, isDigit, isSpace

  ! %%%
  ! # `CSTRING` - Functions for string manipulation.
  !   Module `xslib_cstring` contains function for manipulating character strings.
  ! %%%

  interface str
    module procedure :: str, stra
  end interface str

contains

function str (value, fmt) result (out)
  ! %%%
  ! ## `STR` - Converts value to string
  ! #### DESCRIPTION
  !   Converts the specified SCALAR or ARRAY (of any kind) into a string. Optionally,
  !   output format can be defined with `fmt` argument. In case input values is an ARRAY
  !   a custom delimiter can be defined (default is whitespace).  
  ! #### USAGE
  !   ```fortran
  !   out = str(value, fmt=fmt, delim=delim)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), dimension(..), intent(IN) :: value`  
  !     Value of any kind or shape to be transformed into character.  
  !   * `character(*), intent(IN), OPTIONAL :: fmt`
  !     Valid fortran format specifier. Default is compiler representation.
  !   * `character(*), intent(IN), OPTIONAL :: delim`
  !     Separator to use when joining the string. Only applies for ARRAYS. Default is whitespace.
  !   * `character(:), allocatable :: out`
  !     Output string.  
  ! #### EXAMPLE
  !   ```Fortran
  !   > str(1)
  !   "1"
  !   > str(1.0, "(f5.3)")
  !   "1.000"
  !   > str([1,2], DELIM=",")
  !   "1,2"
  !   ```
  ! %%%
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


function smerge (tsource, fsource, mask) result (out)
  ! %%%
  ! ## `SMERGE` - Merge stings
  ! #### DESCRIPTION
  !   Select values two arbitrary length stings according to a logical mask. The result
  !   is equal to `tsource` if `mask` is `.TRUE.`, or equal to `fsource` if it is `.FALSE.`.  
  ! #### USAGE
  !   ```fortran
  !   out = smerge(tsource, fsource, mask)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: fsource`  
  !     Return string if mask is `.TRUE.`.  
  !   * `character(*), intent(IN) :: fsource`  
  !     Return string if mask is `.FALSE.`.  
  !   `logical, intent(IN) :: mask`  
  !     Selection logical mask. 
  !   * `character(:), allocatable :: out`  
  !     Return string of variable length. 
  ! #### EXAMPLE
  !   ```fortran
  !   > smerge("Top", "Bottom", .True.)
  !   "Top"
  !   > smerge("Top", "Bottom", .False.)
  !   "Bottom"
  !   ```
  ! %%%
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


function toLower (string) result (out)
  ! %%%
  ! ## `TOLOWER` - Return lower case 
  ! #### DESCRIPTION
  !   Return the string with all the cased characters are converted to lowercase.
  ! #### USAGE
  !   ```fortran
  !   out = toLower(string)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string` 
  !     Input string.  
  !   * `character(LEN) :: out`  
  !     Output string. Same length as `string`.  
  ! ###### *Example*
  !   ```fortran
  !   > toLower("Hello, WORLD!")
  !   "hello world!"
  !   ```
  ! %%%
  implicit none
  character(*), intent(in) :: string
  character(len(string)) :: out
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
  ! %%%
  ! ## `TOUPPER` - Return upper case 
  ! #### DESCRIPTION
  !   Return the string with all the cased characters are converted to uppercase.
  ! #### USAGE
  !   ```fortran
  !   out = toUpper(string)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`
  !     Input string.
  !   * `character(LEN) :: out`
  !     Output string. Same length as `string`.
  ! ###### *Example*
  !   ```fortran
  !   > toLower("Hello, WORLD!")
  !   "HELLO WORLD!"
  !   ```
  ! %%%
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
    end select
  end do

end function toUpper


function toTitle (string) result (out)
  ! %%%
  ! ## `TOTITLE` - Capitalize letters
  ! #### DESCRIPTION
  !   Return a titlecased version of the string where words start with an
  !   uppercase character and the remaining characters are lowercase.
  ! #### USAGE
  !   ```fortran
  !   out = toTitle(string)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`
  !     Input string.  
  !   * `character(LEN) :: out`  
  !     Output string. Same length as `string`.  
  ! #### EXAMPLE
  !   ```fortran
  !   > toTitle("hello, world!")
  !   "Hello, World!"
  !   ```
  ! %%%
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
  ! %%%
  ! ## `SWAPCASE` - Swap case
  ! #### DESCRIPTION
  !   Return a string with uppercase characters converted to lowercase and vice versa.
  !   Note that it is not necessarily true that `swapCase(swapCase(s)) == s`.
  ! #### USAGE
  !   ```fortran
  !   out = swapCase(string)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`  
  !     Input string.  
  !   * `character(LEN) :: out`  
  !     Output string. Same length as `string`.  
  ! #### EXAMPLE
  !   ```fortran
  !   > swapCase("Hello, WORLD!")
  !   "hELLO, world!"
  !   ```
  ! %%%
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
  ! %%%
  ! ## `STRIP` - Removes trailing characters
  ! #### DESCRIPTION
  !  Return a string with the leading and trailing characters removed. The `delim` argument is 
  !  a string specifying the characters after which characters to be removed. If omitted,
  !  the argument defaults to removing whitespace. Useful for removing "comments" from a string.
  ! #### USAGE
  !   ```fortran
  !   out = strip(string, delim)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`  
  !     Input string.  
  !   * `character(*), intent(IN) :: delim`  
  !     Separator use for delimiting a string. 
  !   * `character(LEN) :: out `
  !     Output string. Same length as `string`.
  ! #### EXAMPLE
  !   ```fortran
  !   > strip("Hello, WORLD!", ",")
  !   "Hello"
  !   ```
  ! %%% 
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


function replace (string, old, new) result (out)
  ! %%%
  ! ## `REPLACE` - Replace string
  ! #### DESCRIPTION
  !   Return a string with all occurrences of substring `old` replaced by `new`.
  ! #### USAGE
  !   ```fortran
  !   out = replace(string, old, new)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`
  !     Input string.  
  !   * `character(*), intent(IN) :: old`
  !     String to be replaced.   
  !   * `character(*), intent(IN) :: new`  
  !     String to be replaced with.  
  !   * `character(:), allocatable :: out`  
  !     Output string.  
  ! #### EXAMPLE
  !   ```fortran
  !   > "Hello, World!", "World", "Universe")
  !   "Hello, Universe!"
  !   ```
  ! %%%
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


function isAlpha (string) result (out)
  ! %%%
  ! ## `ISALPHA` - Is alphanumeric character
  ! #### DESCRIPTION
  !   Return `.True.` if all characters in the string are alphabetic and
  !   there is at least one character, `.False.` otherwise.
  ! #### USAGE
  !   ```Fortran
  !   out = isAlpha(string)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`  
  !     Input string.  
  !   * `logical :: out`  
  !     Is input string an ASCII alphabetical character.  
  ! #### EXAMPLE
  !   ```Fortran
  !   > isAlpha("ABC")
  !   .True.
  !   > isAlpha("123")
  !   .False.
  !   ```
  ! %%%
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


function isDigit (string) result (out)
  ! %%%
  ! ## `ISDIGIT` - Is digit character
  ! #### DESCRIPTION
  !   Return `.True.` if all characters in the string are digits and
  !   there is at least one character, `.False.` otherwise.
  ! #### USAGE
  !   ```Fortran
  !   out = isDigit(string)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`  
  !     Input string.  
  !   * `logical :: out`  
  !     Is input a digit?  
  ! #### EXAMPLE
  !   ```Fortran
  !   > isDigit("ABC")
  !   .False.
  !   > isDigit("123")
  !   .True.
  !   ```
  ! %%%  
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

end function isDigit


function isSpace (string) result (out)
  ! %%%
  ! ## `ISSPACE` - Is whitespace character
  ! #### DESCRIPTION
  !   Return `.True.` if there are only whitespace characters in the string
  !   and there is at least one character, `.False.` otherwise.
  ! #### USAGE
  !   ```Fortran
  !   out = isSpace(string)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`  
  !     Input string.  
  !   * `logical :: out`  
  !     Is input a whitespace?  
  ! #### EXAMPLE
  !   ```Fortran
  !   > isSpace(" ")
  !   .True.
  !   > isSpace("A")
  !   .False.
  !   ```
  ! %%%  
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


function getColor (fg, bg, attr) result (out)
  ! %%%
  ! ## `GETCOLOR` - Get ANSI escape color code
  ! #### DESCRIPTION
  !   Add some colors in your life! Get terminal ANSI escape sequences to set
  !   terminal text background, foreground, and attribute. Available colors are:
  !   `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`, and their `light`
  !   variants (e.g. `lightblue`). Available attributes are: `bold`, `dim`, `underline`,
  !   `blink`, `reverse`, and `hidden`. If no input is provided function returns "reset"
  !   escape code. Your experience may vary depending on the terminal used. For more 
  !   information see [ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code).
  ! #### USAGE
  !   ```fortran
  !   out = getColor(fg=fg, bg=bg, attr=attr)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN), OPTIONAL :: fg`  
  !     Foreground text color. Default is None.  
  !   * `character(*), intent(IN), OPTIONAL :: bg`  
  !     Background text color. Default is None.  
  !   * `character(*), intent(IN), OPTIONAL :: attr`  
  !     Text color attribute. Default is None.  
  !   * `character(:), allocatable :: out`  
  !     Terminal color ANSI escape code sequence.  
  ! #### EXAMPLE
  !   ```Fortran
  !   > getColor("white", "red", "bold")
  !   ESC[1;37;041m
  !   ```
  ! #### NOTE
  !   Max length of output string is 11 i.e. `E[*;**;***m`.
  ! %%%
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


function setColor (string, attr, fg, bg) result (out)
  ! %%%
  ! ## `SETCOLOR` - Set string ANSI color
  ! #### DESCRIPTION
  !   Add some colors in your life! Set text background, foreground color, and attribute
  !   using ANSI escape sequences. Available colors are: `black`, `red`, `green`,
  !   `yellow`, `blue`, `magenta`, `cyan`, `white`, and their `light` variants (e.g. `lightblue`).
  !   Available attributes are: `bold`, `dim`, `underline`, `blink`, `reverse`, and `hidden`.
  !   Your experience may vary depending on the terminal used. For more information
  !   see [ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code).
  ! #### USAGE
  !   ```fortran
  !   out = setColor(string, fg=fg, bg=gb, attr=attr)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`
  !     Input string. 
  !   * `character(*), intent(IN), OPTIONAL :: fg`
  !     Foreground text color. Default is None.  
  !   * `character(*), intent(IN), OPTIONAL :: bg`
  !     Background text color. Default is None.  
  !   * `character(*), intent(IN), OPTIONAL :: attr`
  !     Text color attribute. Default is None.  
  !   * `character(:), allocatable :: out`
  !     Terminal color ANSI escape code sequence.  
  ! #### EXAMPLE
  !   ```Fortran
  !   > setColor("This string is bold and red", "red", "bold")
  !   "This string is bold and red" ! Use your imagination
  !   ```
  ! %%%
  implicit none
  character(:), allocatable :: out
  character(*), intent(in) :: string
  character(*), intent(in), optional :: fg, bg, attr

  out = getColor(fg, bg, attr) // string // getColor()

end function setColor


function strtok (string, delim) 
  ! %%%
  ! ## `STRTOK` - Split string into tokens
  ! #### DESCRIPTION
  !   A sequence of calls to this function split string into tokens, which are sequences
  !   of contiguous characters separated by the delimiter `delim`.
  ! 
  !   On a first call, the function a string as argument for str, whose first character
  !   is used as the starting location to scan for tokens. In subsequent calls, the function
  !   expects a null character `char(0)` and uses the position right after the end of the last
  !   token as the new starting location for scanning.
  ! 
  !   Once the end character of string is found in a call to `strtok`, all subsequent calls to
  !   this function (with a null character as the first argument) return a null character.
  ! 
  !   See [`strtok`](https://cplusplus.com/reference/cstring/strtok) for full reference.
  ! #### USAGE
  !   ```Fortran
  !   out = strtok(string, delim)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`  
  !     Input string. Feed null character `char(0)` to get next token on precious string.  
  !   * `character(*), intent(IN) :: delim`  
  !     Separator use for delimiting a string.  
  !   * `character(:), allocatable :: out`  
  !     Next output character token.  
  ! #### EXAMPLE
  !   ```Fortran
  !   > strtok("Hello, World!", " ")
  !   "Hello,"
  !   > strtok(char(0), " ")
  !   "World!"
  !   > strtok(char(0), " ")
  !   NULL
  !   ```
  ! #### NOTES
  !   Should be thread-safe with OpenMP.
  ! %%%
  implicit none
  character(:), allocatable :: strtok
  character(*), intent(in) :: string, delim
  character(:), allocatable, save :: saved_string
  integer, save :: saved_start
  integer :: start, finish
  !$OMP THREADPRIVATE (saved_string, saved_start)

  ! SOURCE: http://fortranwiki.org/fortran/show/strtok

  ! Initialize stored copy of input string and pointer into input string on first call
  if (string(1:1) /= char(0)) then
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


function cnttok (string, delim) result (out)
  ! %%%
  ! ## `CNTTOK` - Count tokens in a string
  ! #### DESCRIPTION
  !   Count number of tokens in a string separated by a delimiter `delim`.
  ! #### USAGE
  !   ```fortran
  !   out = cnttok(string, delim)
  !   ```
  ! #### PARAMETERS
  !   * `character(*), intent(IN) :: string`  
  !     Input string.  
  !   * `character(*), intent(IN) :: delim`  
  !     Separator used for delimiting a string.  
  !   * `integer :: out`
  !     Number of tokens on the string.  
  ! #### EXAMPLE
  !   ```Fortran
  !   > cnttok("Hello, World!", " ")
  !   2 
  !   ```
  ! %%%
  implicit none
  character(*), intent(in) :: string
  character(*), intent(in) :: delim
  integer :: out, i

  out = 0
  i = len_trim(string)
  do while (i > 0)
    out = out + 1
    i = index(trim(string(:i-1)), delim, BACK=.True.)
  end do

end function cnttok

end module xslib_cstring
