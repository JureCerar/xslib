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

program main
  use xslib_list
  implicit none
  type(list_t)  :: list
  integer       :: val

  ! Construct simple list
  call list%add( 1 )
  call list%add( 2 )
  call list%add( 3 )

  ! Move aroud the list
  call list%start()
  call list%next()
  call list%prev()
  call list%end()

  ! Insert value at begining
  call list%start()
  call list%insert( 0 )

  ! Delete last value
  call list%pop()

  ! Delete first value
  call list%start()
  call list%remove()

  ! Retrive first value from the list
  call list%start()
  val = list

  ! Print list directly and value
  write (*,*) "list:", list, "value:", val

  ! Execute command on list
  call list%execute( addOne )

  ! Print entire list
  call list%printAll()

  ! Clear list
  call list%clear()
  if ( list%isEmpty() ) write (*,*) "*Empty*"

contains

! Adds one to val
subroutine addOne( val )
  implicit none
  class(*), intent(inout) :: val

  select type ( val )
  type is ( integer )
    val = val+1
  class default
    stop "error"
  end select

  return
end subroutine addOne

end program main
