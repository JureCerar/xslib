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
  public :: str, smerge, toLower, toUpper, stripComment, isNumber, replaceText, &
  &   setColor, getColor, strtok, cnttok, basename, pathname, extension, backup, progressbar

  interface str
    procedure :: strx, stra, strm
  end interface

contains

! -------------------------------------------------
! String transformations

! Trnasforms scalar of any kind to character.
character(:) function strx( obj, fmt )
  use iso_fortran_env, only: REAL64, INT64
  implicit none
  allocatable             :: strx
  class(*), intent(in)    :: obj
  character(*), optional  :: fmt
  character(64)           :: tmp

  ! Act according to the type of variable
  select type ( obj )
  type is ( integer )
    if ( present(fmt) ) then
      write (tmp,fmt) obj
    else
      write (tmp,"(i0)") obj
    end if
    strx = trim(adjustl(tmp))

  type is ( integer(INT64) )
    if ( present(fmt) ) then
      write (tmp,fmt) obj
    else
      write (tmp,"(i0)") obj
    end if
    strx = trim(adjustl(tmp))

  type is ( real )
    if ( present(fmt) ) then
      write (tmp,fmt) obj
    else
      write (tmp,*) obj
    end if
    strx = trim(adjustl(tmp))
    if ( strx(1:1) == "." ) then
      strx = "0"//trim(strx)
    else if ( strx(1:2) == "-." ) then
      strx = "-0"//trim(strx(3:))
    else if ( strx(1:2) == "+." ) then
      strx = "+0"//trim(strx(3:))
    end if

  type is ( real(REAL64) )
    if ( present(fmt) ) then
      write (tmp,fmt) obj
    else
      write (tmp,*) obj
    end if
    strx = trim(adjustl(tmp))
    if ( strx(1:1) == "." ) then
      strx = "0"//trim(strx)
    else if ( strx(1:2) == "-." ) then
      strx = "-0"//trim(strx(3:))
    else if ( strx(1:2) == "+." ) then
      strx = "+0"//trim(strx(3:))
    end if

  type is ( complex )
    if ( present(fmt) ) then
      write (tmp,fmt) obj
    else
      write (tmp,*) obj
    endif
    strx = trim(adjustl(tmp))

  type is ( complex(REAL64) )
    if ( present(fmt) ) then
      write (tmp,fmt) obj
    else
      write (tmp,*) obj
    endif
    strx = trim(adjustl(tmp))

  type is ( logical )
    if ( present(fmt) ) then
      write (tmp,fmt) obj
    else
      write (tmp,*) merge( "true ", "false", obj )
    end if
    strx = trim(adjustl(tmp))

  type is ( character(*) )
    strx = trim(adjustl(obj))

  class default
    ! Well... we can't have everything
    strx = "???"

  end select

  return
end function strx

! Trnasforms array of any kind to character.
character(:) function stra( obj, fmt, delim )
  implicit none
  allocatable                         :: stra
  class(*), intent(in)                :: obj(:)
  character(*), intent(in), optional  :: fmt, delim
  integer                             :: i
  stra = strx(obj(1),FMT=fmt)
  do i = 2, size(obj)
    if ( present(delim) ) then
      stra = stra//delim//strx(obj(i),FMT=fmt)
    else
      stra = stra//" "//strx(obj(i),FMT=fmt)
    end if
  end do
  return
end function stra

! Trnasforms matrix (2D) of any kind to character.
character(:) function strm( obj, fmt, delim )
  implicit none
  allocatable                         :: strm
  class(*), intent(in)                :: obj(:,:)
  character(*), intent(in), optional  :: fmt, delim
  integer                             :: i
  strm = stra(obj(:,1),FMT=fmt,DELIM=delim)
  do i = 2, size(obj,DIM=2)
    strm = strm//new_line("x")//" "//stra(obj(:,1),FMT=fmt,DELIM=delim)
  end do
  return
end function strm

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
  if ( ftoa(1:1) == "." ) then
    ftoa = "0"//trim(ftoa)
  else if ( ftoa(1:2) == "-." ) then
    ftoa = "-0"//trim(ftoa(3:))
  else if ( ftoa(1:2) == "+." ) then
    ftoa = "+0"//trim(ftoa(3:))
  end if
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
  if ( f8toa(1:1) == "." ) then
    f8toa = "0"//trim(f8toa)
  else if ( f8toa(1:2) == "-." ) then
    f8toa = "-0"//trim(f8toa(3:))
  else if ( f8toa(1:2) == "+." ) then
    f8toa = "+0"//trim(f8toa(3:))
  end if
  return
end function f8toa

! Complex to character; "fmt" format (optional).
character(:) function ctoa( imag, fmt )
  implicit none
  allocatable                         :: ctoa
  complex, intent(in)                 :: imag
  character(*), intent(in), optional  :: fmt
  character(64)                       :: tmp
  if ( present(fmt) ) then
    write (tmp,fmt) imag
  else
    write (tmp,*) imag
  end if
  ctoa = trim(adjustl(tmp))
  return
end function ctoa

! Double complex to character; "fmt" format (optional).
character(:) function c8toa( imag, fmt )
  use iso_fortran_env
  implicit none
  allocatable                         :: c8toa
  complex(REAL64), intent(in)         :: imag
  character(*), intent(in), optional  :: fmt
  character(64)                       :: tmp
  if ( present(fmt) ) then
    write (tmp,fmt) imag
  else
    write (tmp,*) imag
  end if
  c8toa = trim(adjustl(tmp))
  return
end function c8toa

! Boolean (logical) to character; "fmt" format (optional).
character(:) function btoa( bool, fmt )
  implicit none
  allocatable                         :: btoa
  logical, intent(in)                 :: bool
  character(*), intent(in), optional  :: fmt
  character(64)                       :: tmp
  if ( present(fmt) ) then
    write (tmp,fmt) bool
  else
    write (tmp,*) bool
  endif
  btoa = trim(tmp)
  return
end function btoa

! -------------------------------------------------
! String manipulation

! Merge two stings of arbitrary length based on logical mask
character(:) function smerge( tsource, fsource, mask )
  implicit none
  allocatable               :: smerge
  character(*), intent(in)  :: tsource, fsource
  logical, intent(in)       :: mask
  if ( mask ) then
    smerge = tsource
  else
    smerge = fsource
  end if
  return
end function smerge

! Return string in all lower case.
character(:) function toLower( string )
  implicit none
  allocatable               :: toLower
  character(*), intent(in)  :: string
  integer, parameter        :: offset = ichar("a") - ichar("A") ! ASCII offset
  integer                   :: i
  toLower = trim(string)
  do i = 1, len_trim(toLower)
    select case ( toLower(i:i) )
    case ( "A" : "Z" )
      ! Shift character by offset
      toLower(i:i) = char(ichar(toLower(i:i))+offset)
    end select
  end do
  return
end function toLower

! Returns string in all upper case.
character(:) function toUpper( string )
  implicit none
  allocatable               :: toUpper
  character(*), intent(in)  :: string
  integer, parameter        :: offset = ichar("a") - ichar("A") ! ASCII offset
  integer                   :: i
  toUpper = trim(string)
  do i = 1, len_trim(toUpper)
    select case ( toUpper(i:i) )
    case ( "a" : "z" )
      ! Shift character by offset
      toUpper(i:i) = char(ichar(toUpper(i:i))-offset)
    end select
  end do
  return
end function toUpper

! Removes all characters trailing comment sign CMT.
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
    ! Comment line
    stripComment = ""
  else
    stripComment = string(1:i-1)
  end if
  return
end function stripComment

! Check if character is ASCII character.
logical function isChar( char )
  implicit none
  character, intent(in) :: char
  isChar = ichar(char) > ichar('A') .and. ichar(char) < ichar('z')
  return
end function isChar

! Check if character is ASCII number.
logical function isNum( char )
  implicit none
  character, intent(in) :: char
  isNum = ichar(char) > ichar('0') .and. ichar(char) < ichar('9')
  return
end function isNum

! Check if character is space.
logical function isSpace( char )
  implicit none
  character, intent(in) :: char
  isSpace = ( char == ' ' )
  return
end function isSpace

! Check if sting is number.
logical function isNumber( string )
  implicit none
  character(*), intent(in) :: string
  real                     :: float
  integer                  :: stat
  read (string,*,IOSTAT=stat) float
  isNumber = ( stat == 0 )
  return
end function isNumber

! Replace OLD with NEW within STRING.
character(:) function replaceText( string, old, new )
  implicit none
  allocatable               :: replacetext
  character(*), intent(in)  :: string, old, new
  integer                   :: current, next
  replaceText = trim(string)
  current = 1
  do while ( .true. )
    next = index(replaceText(current:),old)-1
    if ( next == -1 ) exit
    replacetext = replacetext( : current+next-1 ) // new // replacetext( current+next+len(old) : )
    current = current+len(new)
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
  setColor = getColor(ATTR=attr,FG=fg,BG=bg)//string//getColor()
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
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
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

  ! Dummy initialize
  newfile = ""

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
      write (ERROR_UNIT,*) "File '"//trim(file)//"' already exists. Backing it up as: '"//trim(newfile)//"'"
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

! Prints progress bar (on the same line). Eg. "Progress: [ 50%] [####....]"
! NOTE: Write to output is "forbiden" until progress reaches 1.0;
subroutine progressBar( x, size )
  implicit none
  real, intent(in)              :: x    ! Progres real in rage 0.0 - 1.0
  integer, intent(in), optional :: size ! Size of the progress bar
  integer                       :: i, ni, len
  character, parameter          :: CR = char(13), ESC = char(27)

  if ( 0.0 <= x .and. x <= 1.0 ) then
    ! How many bars are full?
    len = merge( size, 100, present(size) )
    ni = nint( x*len )
    ! First write "progress" then "bar"
    write (*,100,ADVANCE="no") CR, ESC//"[7m", 'Progress: [', int(x*100), "%]", ESC//"[0m"
    write (*,200,ADVANCE="no") "[", ('#', i=1,ni),  ('.', i=1,len-ni), "]"
    ! Go to new line
    if ( x == 1.0 ) write (*,*) ""

  end if

  100 format( a,x,2a,i3,2a )
  200 format( x,*(a) )

  return
end subroutine progressBar

end module xslib_cstring
