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
  use iso_fortran_env, only: INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128
  use xslib_cstring
  implicit none

  call str_test ()
  call color_test ()
  call token_test ()
  call is_test ()
  call other_test ()

contains

! Test for "to string" transformation fuction.
subroutine str_test ()
  implicit none
  character(128) :: buffer 

  ! Test printing to STDOUT for scalars.
  print *, str(1_INT8)
  print *, str(1_INT16)
  print *, str(1_INT32)
  print *, str(1_INT64)
  print *, str(1.0_REAL32)
  print *, str(1.0_REAL64)
  print *, str(1.0_REAL128)
  print *, str((1.0_REAL32,1.0_REAL32))
  print *, str((1.0_REAL64,1.0_REAL64))
  print *, str((1.0_REAL128,1.0_REAL128))
  print *, str(.True.)
  print *, str("Foo")

  ! Test printing to STDOUT for arrays.
  print *, str([1_INT8,2_INT8])
  print *, str([1_INT16,2_INT16])
  print *, str([1_INT32,2_INT32])
  print *, str([1_INT64,2_INT64])
  print *, str([1.0_REAL32,2.0_REAL32])
  print *, str([1.0_REAL64,2.0_REAL64])
  print *, str([1.0_REAL128,2.0_REAL128])
  print *, str([(1.0_REAL32,1.0_REAL32),(2.0_REAL32,2.0_REAL32)])
  print *, str([(1.0_REAL64,1.0_REAL64),(2.0_REAL64,2.0_REAL64)])
  print *, str([(1.0_REAL128,1.0_REAL128),(2.0_REAL128,2.0_REAL128)])
  print *, str([.True.,.False.])
  print *, str(["Foo", "Bar"])

  ! Check if output is correct for scalars.
  buffer = str(1_INT8,"(i0.3)")
  if (buffer /= "001") error stop
  buffer = str(1_INT16,"(i0.3)")
  if (buffer /= "001") error stop
  buffer = str(1_INT32,"(i0.3)")
  if (buffer /= "001") error stop
  buffer = str(1_INT64,"(i0.3)")
  if (buffer /= "001") error stop
  buffer = str(1.0_REAL32,"(f5.2)")
  if (buffer /= "1.00") error stop
  buffer = str(1.0_REAL64,"(f5.2)")
  if (buffer /= "1.00") error stop
  buffer = str(1.0_REAL128,"(f5.2)")
  if (buffer /= "1.00") error stop 
  buffer = str((1.0_REAL32,1.0_REAL32),"(f5.2,SP,f5.2,'i')")
  if (buffer /= "1.00+1.00i") error stop
  buffer = str((1.0_REAL64,1.0_REAL64),"(f5.2,SP,f5.2,'i')")
  if (buffer /= "1.00+1.00i") error stop
  buffer = str((1.0_REAL128,1.0_REAL128),"(f5.2,SP,f5.2,'i')")
  if (buffer /= "1.00+1.00i") error stop
  buffer = str(.True.,"(l2)")
  if (buffer /= "T") error stop
  buffer = str("Foo","(a)")
  if (buffer /= "Foo") error stop

  ! Check if output is correct for arrays.
  buffer =  str([1_INT8,2_INT8],"(i0.3)",",")
  if (buffer /= "001,002") error stop
  buffer =  str([1_INT16,2_INT16],"(i0.3)",",")
  if (buffer /= "001,002") error stop
  buffer =  str([1_INT32,2_INT32],"(i0.3)",",")
  if (buffer /= "001,002") error stop
  buffer =  str([1_INT64,2_INT64],"(i0.3)",",")
  if (buffer /= "001,002") error stop
  buffer = str([1.0_REAL32,2.0_REAL32],"(f5.2)",",")
  if (buffer /= "1.00,2.00") error stop
  buffer = str([1.0_REAL64,2.0_REAL64],"(f5.2)",",")
  if (buffer /= "1.00,2.00") error stop
  buffer = str([1.0_REAL128,2.0_REAL128],"(f5.2)",",")
  if (buffer /= "1.00,2.00") error stop
  buffer = str([(1.0_REAL32,1.0_REAL32),(2.0_REAL32,2.0_REAL32)],"(f5.2,SP,f5.2,'i')",",")
  if (buffer /= "1.00+1.00i,2.00+2.00i") error stop
  buffer = str([(1.0_REAL64,1.0_REAL64),(2.0_REAL64,2.0_REAL64)],"(f5.2,SP,f5.2,'i')",",")
  if (buffer /= "1.00+1.00i,2.00+2.00i") error stop
  buffer = str([(1.0_REAL128,1.0_REAL128),(2.0_REAL128,2.0_REAL128)],"(f5.2,SP,f5.2,'i')",",")
  if (buffer /= "1.00+1.00i,2.00+2.00i") error stop
  buffer = str([.True.,.False.],"(l2)",",")
  if (buffer /= "T,F") error stop
  buffer = str(["Foo","Bar"],"(a)",",")
  if (buffer /= "Foo,Bar") error stop

end subroutine str_test

! Test color functions. Feel the rainbow, taste the rainbow.
subroutine color_test ()
  implicit none
  character(32), allocatable :: color(:), attribute(:)
  character(32) :: buffer
  integer :: i, status, ncolors, nattr

  ! Check nocolor string
  if (getColor() /= char(27)//"[m") error stop

  ! Check setColor vs getColor
  buffer = getColor(ATTR="bold", FG="red", BG="black") // "Foo" // getColor()
  if (buffer /= setColor("Foo", ATTR="bold", FG="red", BG="black")) error stop

  ncolors = 17
  allocate (color(ncolors), STAT=status)
  if (status /= 0) error stop
  color(1) = "black"
  color(2) = "red"
  color(3) = "green"
  color(4) = "yellow"
  color(5) = "blue"
  color(6) = "magenta"
  color(7) = "cyan"
  color(8) = "white"
  color(9) = "lightblack"
  color(10) = "lightred"
  color(11) = "lightgreen"
  color(12) = "lightyellow"
  color(13) = "lightblue"
  color(14) = "lightmagenta"
  color(15) = "lightcyan"
  color(16) = "lightwhite"
  color(17) = "none"

  ! Test foreground colors
  do i = 1, ncolors
    print *, getColor(FG=color(i)), trim(color(i)), getColor()
  end do
  
  ! Test background colors
  do i = 1, ncolors
    print *, getColor(BG=color(i)), trim(color(i)), getColor()
  end do

  nattr = 8
  allocate (attribute(nattr), STAT=status)
  if (status /= 0) error stop
  attribute(1) = "bold"
  attribute(2) = "bright"
  attribute(3) = "dim"
  attribute(4) = "underline"
  attribute(5) = "blink"
  attribute(6) = "reverse"
  attribute(7) = "hidden"
  attribute(8) = "none"

  do i = 1, nattr
    print *, getColor(ATTR=attribute(i)), trim(attribute(i)), getColor()
  end do

end subroutine color_test

! Test string tokenization.
subroutine token_test ()
  implicit none
  character(32) :: buffer, token
  
  buffer = "Foo,Bar,Foobar"
  
  ! Test string tokenization
  token = strtok(buffer, ",")
  if (token /= "Foo") error stop
  token = strtok(char(0), ",")
  if (token /= "Bar") error stop
  token = strtok(char(0), ",")
  if (token /= "Foobar") error stop
  token = strtok(char(0), ",")
  if (token /= char(0)) error stop

  token = strtok(buffer, ",")
  do while (token /= char(0))
    token = strtok(char(0), ",")
  end do

  ! Test counting string tokens
  if (cnttok(buffer, ",") /= 3) error stop

end subroutine token_test 

! Test "isThing" functions.
subroutine is_test ()
  implicit none

  if (isAlpha("A") .neqv. .True.) error stop
  if (isAlpha("1") .neqv. .False.) error stop
  if (isAlpha("@") .neqv. .False.) error stop
  if (isAlpha(" ") .neqv. .False.) error stop

  if (isDigit("A") .neqv. .False.) error stop
  if (isDigit("1") .neqv. .True.) error stop
  if (isDigit("@") .neqv. .False.) error stop
  if (isDigit(" ") .neqv. .False.) error stop

  if (isSpace("A") .neqv. .False.) error stop
  if (isSpace("1") .neqv. .False.) error stop
  if (isSpace("@") .neqv. .False.) error stop
  if (isSpace(" ") .neqv. .True.) error stop

end subroutine is_test 

! Test other cstring functions
subroutine other_test ()
  implicit none
  character(32) :: buffer

  ! Test smerge
  buffer = smerge("True", "False", .True.)
  if (buffer /= "True") error stop
  buffer = smerge("True", "False", .False.)
  if (buffer /= "False") error stop
  
  ! Test replace
  buffer = replace("This is Foo", "Foo", "Bar")
  if (buffer /= "This is Bar") error stop

  ! Test case transform
  buffer = toLower("Foo")
  if (buffer /= "foo") error stop
  buffer = toUpper("Foo")
  if (buffer /= "FOO") error stop
  buffer = toTitle("foo bar")
  if (buffer /= "Foo Bar") error stop
  buffer = swapCase("Foo")
  if (buffer /= "fOO") error stop

  ! Test strip
  buffer = strip("Foo,Bar,Foobar", ",")
  if (buffer /= "Foo") error stop
  
end subroutine other_test 

end program main