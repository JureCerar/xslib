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

module xslib_ansi
  !! Module for adding some colors in your life.
  implicit none
  private
  public :: getColor, setColor

contains

function getColor (fg, bg, attr) result (out)
  !! Add some colors in your life! Get terminal ANSI escape sequences to set terminal
  !! text background, foreground, and attribute. Your experience may vary depending on
  !! the terminal used. For more information see
  !! [ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code).
  !!
  !! Available colors are: `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`,
  !! `white`,  and their `light` variants (e.g. `lightblue`).
  !!
  !! Available attributes are: `bold`, `dim`, `underline`, `blink`, `reverse`, and `hidden`.
  !!
  !! @note
  !! Max length of output string is 11 i.e. `E[*;**;***m`.
  !! @endnote
  !!
  !! Example:
  !!  ```Fortran
  !!  c1 = getColor("white", "red", "bold")
  !!  c0 = getColor()
  !!  print *, c1 // "Hello, World!" // c0
  !!  >>> "Hello, World!"      ! Use your imagination for colors
  !!  ```
  use xslib_cstring, only: toLower
  implicit none
  character(:), allocatable :: out
  !! Terminal color ANSI escape code sequence.  
  character(*), intent(in), optional :: fg
  !! Foreground text color. Default is None.
  character(*), intent(in), optional :: bg
  !! Background text color. Default is None.  
  character(*), intent(in), optional :: attr
  !! Text color attribute. Default is None.
  character, parameter :: esc = char(27)
  ! <ESC>[{attr};{fg};{bg};"
  ! See: https://www.linuxjournal.com/article/8603
  ! See: https://misc.flogisoft.com/bash/tip_colors_and_formatting

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
  !! Add some colors in your life! Set text background, foreground color,
  !! and attribute using ANSI escape sequences. Your experience may vary depending on
  !! depending on the terminal used. For more information see
  !! [ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code).
  !!
  !! Available colors are: `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`,
  !! `white`, and their `light` variants (e.g. `lightblue`).
  !!
  !! Available attributes are: `bold`, `dim`, `underline`, `blink`, `reverse`, and `hidden`.
  !!
  !! Example:
  !!  ```Fortran
  !!  print *, setColor("Hello, World!", "red", "bold")
  !!  >>> "Hello, World!"      ! Use your imagination for colors
  !!  ```
  implicit none
  character(:), allocatable :: out
  !! String with set color ANSI escape code sequences.  
  character(*), intent(in), optional :: fg
  !! Foreground text color. Default is None.
  character(*), intent(in), optional :: bg
  !! Background text color. Default is None.  
  character(*), intent(in), optional :: attr
  !! Text color attribute. Default is None.
  character(*), intent(in) :: string
  !! Input string. 

  out = getColor(fg, bg, attr) // string // getColor()

end function setColor

end module xslib_ansi