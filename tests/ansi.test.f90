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
    use xslib_ansi
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


    ! Test foreground colors
    do i = 1, ncolors
        print *, getColor(FG=color(i)), trim(color(i)), "Foo", getColor(), "Bar"
        print *, setColor("Foo", fg=trim(color(i))), "Bar"
    end do
    
    ! Test background colors
    do i = 1, ncolors
        print *, getColor(BG=color(i)), trim(color(i)), "Foo", getColor(), "Bar"
        print *, setColor("Foo", bg=trim(color(i))), "Bar"
    end do

    ! Test text attributes 
    do i = 1, nattr
        print *, getColor(ATTR=attribute(i)), trim(attribute(i)), "Foo", getColor(), "Bar"
        print *, setColor("Foo", attr=trim(attribute(i))), "Bar"
    end do

end program main