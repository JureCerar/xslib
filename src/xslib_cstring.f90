! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2020  Jure Cerar
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
  implicit none
  private
  public :: str, toLower, toUpper, stripComment, isWord, replaceText, setColor, &
  &   strtok, cnttok, basename, pathname, extension, backup, progressbar

  interface str
    procedure :: strx, stra
  end interface

contains

! -------------------------------------------------
! String transformations

! Trnasforms scalar of any kind to character.
character(:) function strx( x, fmt )
  use iso_fortran_env, only: REAL64, INT64
  implicit none
  allocatable             :: strx
  class(*), intent(in)    :: x
  character(*), optional  :: fmt
  ! Act according to the type of variable
  select type ( x )
  type is ( integer )
    strx = itoa( x, FMT=fmt )
  type is ( integer(INT64) )
    strx = i8toa( x, FMT=fmt )
  type is ( real )
    strx = ftoa( x, FMT=fmt )
  type is ( real(REAL64) )
    strx = f8toa( x, FMT=fmt )
  type is ( complex )
    strx = ctoa( x, FMT=fmt )
  type is ( complex(REAL64) )
    strx = c8toa( x, FMT=fmt )
  type is ( logical )
    strx = btoa( x )
  type is ( character(*) )
    strx = atoa( x )
  class default
    ! Can't have everything
    strx = "???"
  end select
  return
end function strx

! Trnasforms array of any kind to character.
character(:) function stra( array, fmt, delim )
  implicit none
  allocatable                         :: stra
  class(*), intent(in)                :: array(:)
  character(*), intent(in), optional  :: fmt, delim
  integer                              :: i
  select type( array )
  class default
    stra = str(array(1),FMT=fmt)
    do i = 2, size(array)
      if ( present(delim) ) then
        stra = stra//trim(delim)//str(array(i),FMT=fmt)
      else
        stra = stra//" "//str(array(i),FMT=fmt)
      end if
    end do
  end select
  return
end function stra

! -------------------------------------------------
! Individual transformations for Integer, Real, Complex, Logical and Character to string

! Integer to character; "fmt" format (optional).
character(:) function itoa( int, fmt )
  allocatable                         :: itoa
  integer, intent(in)                 :: int
  character(*), intent(in), optional  :: fmt
  character(32)                       :: tmp
  if ( present(fmt) ) then
    write (tmp,fmt) int
  else
    write (tmp,"(i0)") int
  end if
  itoa = trim(adjustl(tmp))
  return
end function itoa

! Double integer to character; "fmt" format (optional).
character(:) function i8toa( int, fmt )
  use iso_fortran_env
  implicit none
  allocatable                         :: i8toa
  integer(INT64), intent(in)          :: int
  character(*), intent(in), optional  :: fmt
  character(32)                       :: tmp
  if (present(fmt)) then
    write (tmp,fmt) int
  else
    write (tmp, "(i0)") int
  end if
  i8toa = trim(adjustl(tmp))
  return
end function i8toa

! Real to character; "fmt" format (optional).
character(:) function ftoa( float, fmt )
  implicit none
  allocatable                         :: ftoa
  real, intent(in)                    :: float
  character(*), intent(in), optional  :: fmt
  character(32)                       :: tmp
  if (present(fmt)) then
    write (tmp, fmt) float
  else
    write (tmp,*) float
  end if
  ftoa = trim(adjustl(tmp))
  return
end function ftoa

! Double real to character; "fmt" format (optional).
character(:) function f8toa( float, fmt )
  use iso_fortran_env
  implicit none
  allocatable                         :: f8toa
  real(REAL64), intent(in)            :: float
  character(*), intent(in), optional  :: fmt
  character(64)                       :: tmp
  if (present(fmt)) then
    write (tmp, fmt) float
  else
    write (tmp,*) float
  end if
  f8toa = trim(adjustl(tmp))
  return
end function f8toa

! Complex to character; "fmt" format (optional).
character(:) function ctoa( imag, fmt )
  implicit none
  allocatable                         :: ctoa
  complex, intent(in)                 :: imag
  character(*), intent(in), optional  :: fmt
  ! Real part
  if ( present(fmt) ) then
    ctoa = ftoa( real(imag), FMT=fmt )
  else
    ctoa = ftoa( real(imag) )
  end if
  ! Sign
  if ( aimag(imag) >= 0.0 ) ctoa = ctoa//"+"
  ! Imaginary part
  if ( present(fmt) ) then
    ctoa = ctoa//ftoa( real(imag), FMT=fmt )//"i"
  else
    ctoa = ctoa//ftoa( real(imag) )//"i"
  end if
  return
end function ctoa

! Complex to character; "fmt" format (optional).
character(:) function c8toa( imag, fmt )
  use iso_fortran_env
  implicit none
  allocatable                         :: c8toa
  complex(REAL64), intent(in)         :: imag
  character(*), intent(in), optional  :: fmt
  ! Real part
  if ( present(fmt) ) then
    c8toa = f8toa( real(imag), FMT=fmt )
  else
    c8toa = f8toa( real(imag) )
  end if
  ! Sign
  if ( aimag(imag) >= 0.0 ) c8toa = c8toa//"+"
  ! Imaginary part
  if ( present(fmt) ) then
    c8toa = c8toa//f8toa( real(imag), FMT=fmt )//"i"
  else
    c8toa = c8toa//f8toa( real(imag) )//"i"
  end if
  return
end function c8toa

! Boolean (logical) to character; FMT is ignored.
character function btoa( bool, fmt )
  implicit none
  logical, intent(in)                 :: bool
  character(*), intent(in), optional  :: fmt
  btoa = merge("T","F",bool)
  return
end function btoa

! Character to character; FMT is ignored.
character(:) function atoa( char, fmt )
  allocatable                         :: atoa
  character(*), intent(in)            :: char
  character(*), intent(in), optional  :: fmt
  atoa = trim(adjustl( char ))
  return
end function atoa

! -------------------------------------------------
! String manipulation

! Return string in all lower case.
character(:) function toLower( string )
  implicit none
  allocatable               :: toLower
  character(*), intent(in)  :: string
  integer, parameter        :: offset=ichar("a")-ichar("A") ! ASCII offset
  integer                   :: i
  toLower = trim(string)
  do i = 1, len(toLower)
    ! Shift character by offset
    if (toLower(i:i)>="A" .and. toLower(i:i)<= "Z") toLower(i:i) = char(ichar(toLower(i:i))+offset)
  end do
  return
end function toLower

! Returns string in all upper case.
character(:) function toUpper( string )
  implicit none
  allocatable               :: toUpper
  character(*), intent(in)  :: string
  integer, parameter        :: offset=ichar("a")-ichar("A") ! ASCII offset
  integer                   :: i
  toUpper = trim(string)
  do i = 1, len(toUpper)
    ! Shift character by offset
    if (toUpper(i:i)>="a" .and. toUpper(i:i)<= "z") toUpper(i:i) = char(ichar(toUpper(i:i))-offset)
  end do
  return
end function toUpper

! Removes all characters trailing comment sign `cmt`.
! * "xyz #foobar" -> "xyz"
character(:) function stripComment( string, cmt )
  implicit none
  allocatable               :: stripComment
  character(*), intent(in)  :: string
  character(*), intent(in)  :: cmt
  integer                   :: i
  i = index( string, cmt )
  if ( i == 0 ) then
    ! No comment line
    stripComment = trim(string)
  else if ( i == 1 ) then
    ! Whole line is coment; is empty line
    stripComment = ""
  else
    stripComment = string(1:i-1)
  end if
  return
end function stripComment

! Check if string contains any ASCII letters.
logical function isWord( string )
  implicit none
  character(*), intent(in)  :: string
  isWord = ( scan(trim(string), "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz") /= 0 )
  return
end function isWord

! Replace <old> with <new> within <string>.
character(:) function replaceText( string, old, new )
  allocatable               :: replacetext
  character(*), intent(in)  :: string, old, new
  integer                   :: pos, cur

  ! Initialize
  replaceText = string
  cur = 1
  do
    pos = index( replacetext(cur:), old )
    if (pos == 0) exit
    replacetext = replacetext(:(cur-1+pos)-1) // new // string((cur-1+pos)+len(old):)
    cur = cur+pos-1

  end do

  return
end function replaceText


! Add some colors in your life & set terminal color. ATTR=attribute, FG=foreground, BG=background
! * Attributes: bold, dim, underline, blink, reverse and  hidden
! * Colors: black, red, yellow, blue, magenta, cyan, light grey, dark grey,
! & light red, light yellow, light blue, light magenta, light cyan and white
! NOTE: Max len. of output string is 11; "e[*;**;***m"
character(:) function getColor( attr, fg, bg )
  implicit none
  allocatable                         :: getColor
  character(*), intent(in), optional  :: attr, fg, bg
  character, parameter                :: esc = char(27)

  ! <ESC>[{attr};{fg};{bg};"
  ! SOURCE: https://www.linuxjournal.com/article/8603
  ! SOURCE: https://misc.flogisoft.com/bash/tip_colors_and_formatting

  ! Start sequence
  getColor = esc//"["

  ! Select attribute
  if ( present(attr) ) then
    select case( toLower(attr) )
    case( "bold", "bright" )
      getColor = getColor//"1"
    case( "dim" )
      getColor = getColor//"2"
    case( "underline" )
      getColor = getColor//"4"
    case( "blink" )
      getColor = getColor//"5"
    case( "reverse" )
      getColor = getColor//"7"
    case( "hidden" )
      getColor = getColor//"8"
    case default
      getColor = getColor//"0"
    end select
  end if

  ! Select color
  if ( present(fg) ) then
    select case( toLower(fg) )
    case( "black" )
      getColor = getColor//";30"
    case( "red" )
      getColor = getColor//";31"
    case( "green" )
      getColor = getColor//";32"
    case( "yellow" )
      getColor = getColor//";33"
    case( "blue" )
      getColor = getColor//";34"
    case( "magenta" )
      getColor = getColor//";35"
    case( "cyan" )
      getColor = getColor//";36"
    case( "light grey" )
      getColor = getColor//";37"
    case( "dark grey" )
      getColor = getColor//";90"
    case( "light red" )
      getColor = getColor//";91"
    case( "light green" )
      getColor = getColor//";92"
    case( "light yellow" )
      getColor = getColor//";93"
    case( "light blue" )
      getColor = getColor//";94"
    case( "light magenta" )
      getColor = getColor//";95"
    case( "light cyan" )
      getColor = getColor//";96"
    case( "white" )
      getColor = getColor//";97"
    case default
      getColor = getColor//";39"
    end select
  end if

  ! Select background
  if ( present(bg) ) then
    select case( toLower(bg) )
    case( "black" )
      getColor = getColor//";40"
    case( "red" )
      getColor = getColor//";41"
    case( "green" )
      getColor = getColor//";42"
    case( "yellow" )
      getColor = getColor//";43"
    case( "blue" )
      getColor = getColor//";44"
    case( "magenta" )
      getColor = getColor//";45"
    case( "cyan" )
      getColor = getColor//";46"
    case( "light grey" )
      getColor = getColor//";47"
    case( "dark grey" )
      getColor = getColor//";100"
    case( "light red" )
      getColor = getColor//";101"
    case( "light green" )
      getColor = getColor//";102"
    case( "light yellow" )
      getColor = getColor//";103"
    case( "light blue" )
      getColor = getColor//";104"
    case( "light magenta" )
      getColor = getColor//";105"
    case( "light cyan" )
      getColor = getColor//";106"
    case( "white" )
      getColor = getColor//";107"
    case default
      getColor = getColor//";49"
    end select
  end if

  ! End sequence
  getColor = getColor//"m"

  return
end function getColor

! Wrapper for getColor().
character(:) function setColor( string, attr, fg, bg )
  implicit none
  allocatable                         :: setColor
  character(*), intent(in)            :: string
  character(*), intent(in), optional  :: attr, fg, bg

  setColor = getColor(ATTR=attr,FG=fg,BG=bg)//string//char(27)//"[m"

  return
end function setColor


! -------------------------------------------------
! String tokenization

! Breaks string str into a series of tokens using the delimiter delim.
! * Works in same way as strtok in C, except instead of NULL feed char(0)
character(:) function strtok( string, delim )
  implicit none
  allocatable                     :: strtok
  character(*),intent(in)         :: string
  character(*),intent(in)         :: delim
  character(:), allocatable, save :: saved_string
  integer, save                   :: saved_start
  integer                         :: start, finish
  !$OMP THREADPRIVATE( saved_string, saved_start )

  ! SOURCE: http://fortranwiki.org/fortran/show/strtok

  ! initialize stored copy of input string and pointer into input string on first call
  if ( string(1:1) /= char(0) ) then
      saved_start = 1                 ! beginning of unprocessed data
      saved_string = trim(string)     ! save input string from first call in series
  endif

  ! Start from where we left
  start = saved_start

  ! Skip until next non-delimiter
  do while ( start <= len(saved_string) )
    if ( index(delim,saved_string(start:start)) /= 0 ) then
      start = start+1
    else
      exit
    end if
  end do

  ! If we reach end of string
  if ( start > len(saved_string) ) then
    strtok = char(0)
    return
  end if

  ! Find next delimiter
  finish = start
  do while ( finish <= len(saved_string) )
    if ( (index(delim,saved_string(finish:finish)) == 0) ) then
      finish = finish+1
    else
      exit
   end if
  end do

  ! Set result and update where we left
  strtok = saved_string(start:finish-1)
  saved_start = finish

  return
end function strtok

! Count number of tokens in string.
integer function cnttok( string, delim )
  implicit none
  character(*),intent(in)   :: string
  character(*),intent(in)   :: delim
  character(:), allocatable :: token
  ! Initialize
  cnttok = 0

  ! Get first token
  token = strtok( string, delim )

  ! Walk through other tokens
  do while ( token /= char(0) )
    cnttok = cnttok+1
    token = strtok( char(0), delim )
  end do

  return
end function cnttok

! -------------------------------------------------
! File manipulation

! Returns basename of file. Eg. "/path/to/file.txt" = "file"
character(:) function basename( file )
  implicit none
  allocatable               :: basename
  character(*), intent(in)  :: file
  integer                   :: i, j
  i = 1+index( file, "/", BACK=.true. )
  j = index( file, ".", BACK=.true. )
  j = merge( j-1, len_trim(file), j /= 0 )
  basename = file(i:j)
  return
end function basename

! Returns pathname of file. Eg. "/path/to/file.txt" = "/path/to/"
! TODO: Add feature to ignore "/" if preceded by "\"
character(:) function pathname( file )
  implicit none
  allocatable               :: pathname
  character(*), intent(in)  :: file
  integer                   :: i
  i = index( file, "/", BACK=.true. )
  if ( i == 0 ) then
    pathname = ""
  else
    pathname = file(1:i)
  end if
  return
end function pathname

! Returns extension of file. Eg. "/path/to/file.txt" = "txt"
character(:) function extension( file )
  implicit none
  allocatable               :: extension
  character(*), intent(in)  :: file
  integer                   :: i
  i = index( file, ".", BACK=.true. )
  if ( i == 0 ) then
    extension = ""
  else
    extension = file(i+1:len_trim(file))
  end if
  return
end function extension

! Backups file; Checks if "file.ext" exists and renames it to "#file.ext.1#".
subroutine backup( file, status )
  use, intrinsic :: iso_fortran_env, only: error_unit
  implicit none
  character(*), intent(in)       :: file
  integer, intent(out), optional :: status
  character(:), allocatable      :: path, name, newfile
  character(32)                  :: number
  logical                        :: exist
  integer                        :: i, n, stat

  ! Check if file exists
  inquire( FILE=trim(file), EXIST=exist )
  if ( .not. exist ) then
    if ( present(status) ) status = 0
    return
  end if

  ! If it exists then create a 'new' name - path/to/#file.ext.n# n=1,...
  ! Extract path and base
  i = index(file,"/",BACK=.true.)
  if ( i <= 0 ) then
    path = ""
    name = trim(file)
  else
    path = file(1:i)
    name = file(i+1:len_trim(file))
  end if

  ! Generate new file name and try if it is free.
  n = 0
  do while ( n < huge(n) )
    ! Increment iterator
    n = n+1
    write (number,"(i0)") n
    newfile = path//"#"//name//"."//trim(number)//"#"
    ! Check if this newfile exists
    inquire( FILE=newfile, EXIST=exist )
    if ( .not. exist ) then
      write (error_unit,*) "File '"//trim(file)//"' already exists. Backing it up as: '"//trim(newfile)//"'"
      call rename( trim(file), newfile, STATUS=stat )
      if ( present(status) ) status = stat
      return
    end if
  end do ! while n

  ! Return failure
  if ( present(status) ) status = -1

  return
end subroutine backup

! -------------------------------------------------
! Miscellaneous

! Prints progress bar (on the same line). Eg. "50.0% |==  |"
! NOTE: Write to output is forbiden until progress reaches 1.0;
subroutine progressBar( x, size )
  implicit none
  real, intent(in)              :: x ! Progres real in rage 0.0 - 1.0
  integer, intent(in), optional :: size    ! Size of the progress bar
  integer                       :: i, n, s, stat
  character(:), allocatable     :: bar
  character, parameter          :: backspace = char(13)

  ! Initialize bar size
  s = merge( size, 50, present(size) )
  allocate( character(s+10) :: bar, STAT=stat )
  if ( stat /= 0 ) return

  ! Construct blank progress bar
  ! ???.?%_|          |
  write (bar,"(1024a)") "???.?% |", (" ", i = 1, s),"|"

  ! Add missing data
  if ( 0.0 <= x .and. x <= 1.0 ) then
    ! Add procentage
    n = nint( x*s )
    write (bar(1:5),"(f5.1)") x*100
    ! Add progress bar
    write (bar(9:9+n-1),"(1024a)") ("=", i=1,n)
    write (*,"(2a,$)") backspace, trim(bar)
  else
    write (*,"(2a,$)") backspace, trim(bar)
  end if
  ! Go to new line
  if ( x >= 1.0 ) write (*,*) ""
  return
end subroutine progressBar

end module xslib_cstring
