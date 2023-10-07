! This file is part of xslib
! https://github.com/JureCerar/xslib
!
! Copyright (C) 2019-2023 Jure Cerar
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

module xslib_list
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: list_t

  type link_t
    class(*), pointer :: value => null()
    type(link_t), pointer :: next => null()
  end type link_t

  type list_t
    class(link_t), pointer, private :: first => null()
    class(link_t), pointer, private :: last => null()
  contains
    procedure :: append => list_append
    procedure :: clear => list_clear
    procedure :: count => list_count
    procedure :: extend => list_extend
    procedure :: index => list_index
    procedure :: len => list_len
    procedure :: insert => list_insert
    procedure :: get => list_get
    procedure :: pop => list_pop
    procedure :: remove => list_remove
    procedure :: reverse => list_reverse
    procedure :: set => list_set
    procedure :: sort => list_sort
    procedure, private :: write_formatted
    generic :: write(formatted) => write_formatted
  end type list_t

  interface list_t
    module procedure :: list_constructor
  end interface list_t

  ! NOTE: List currently supports only INT32, INT64, REAL32, REAL64, LOGICAL, and CHARACTER(*) 
  ! variable types. To add new derived TYPE support add them to: `equal`, `copy`, and (optional) `write`.

contains

! Compare (strictly) two unlimited polymorphic variables
logical function equal (a, b)
  implicit none
  class(*), intent(in) :: a, b
  
  equal = .false.
  select type (a)
  type is (integer(INT32))
    select type (b)
    type is (integer(INT32))
      equal = (a == b)
    end select

  type is (integer(INT64))
    select type (b)
    type is (integer(INT64))
      equal = (a == b)
    end select

  type is (real(REAL32))
    select type (b)
    type is (real(REAL32))
      equal = (a == b)
    end select

  type is (real(REAL64))
    select type (b)
    type is (real(REAL64))
      equal = (a == b)
    end select

  type is (logical)
    select type (b)
    type is (logical)
      equal = (a .eqv. b)
    end select

  type is (character(*))
    select type (b)
    type is (character(*))
      equal = (a == b)
    end select
  end select

end function equal

! Copy value between two unlimited polymorphic variables
subroutine copy (in, out)
  implicit none
  class(*), intent(in) :: in
  class(*), intent(out) :: out
  character(128) :: buffer

  select type (out)
  type is (integer(INT32))
    select type (in)
    type is (integer(INT32))
      out = int(in, INT32)
    type is (integer(INT64))
      out = int(in, INT32)
    type is (real(REAL32))
      out = int(in, INT32)
    type is (real(REAL64))
      out = int(in, INT32)
    class default
      error stop "Cannot convert data types"
    end select

  type is (integer(INT64))
    select type (in)
    type is (integer(INT32))
      out = int(in, INT64)
    type is (integer(INT64))
      out = int(in, INT64)
    type is (real(REAL32))
      out = int(in, INT64)
    type is (real(REAL64))
      out = int(in, INT64)
    class default
      error stop "Cannot convert data types"
    end select

  type is (real(REAL32))
    select type (in)
    type is (integer(INT32))
      out = real(in, REAL32)
    type is (integer(INT64))
      out = real(in, REAL32)
    type is (real(REAL32))
      out = real(in, REAL32)
    type is (real(REAL64))
      out = real(in, REAL32)
    class default
      error stop "Cannot convert data types"
    end select

  type is (real(REAL64))
    select type (in)
    type is (integer(INT32))
      out = real(in, REAL64)
    type is (integer(INT64))
      out = real(in, REAL64)
    type is (real(REAL32))
      out = real(in, REAL64)
    type is (real(REAL64))
      out = real(in, REAL64)
    class default
      error stop "Cannot convert data types"
    end select

  type is (logical)
    select type (in)
    type is (logical)
      out = in
    class default
      error stop "Cannot convert data types"
    end select

  type is (character(*))
    select type (in)
    type is (integer(INT32))
      write (buffer,*) in
      out = trim(adjustl(buffer))
    type is (integer(INT64))
      write (buffer,*) in
      out = trim(adjustl(buffer))
    type is (real(REAL32))
      write (buffer,*) in
      out = trim(adjustl(buffer))
    type is (real(REAL64))
      write (buffer,*) in
      out = trim(adjustl(buffer))
    type is (logical)
      write (buffer,*) in
      out = trim(adjustl(buffer))
    type is (character(*))
      out = in
    class default
      error stop "Cannot convert data types"
    end select

  class default
    error stop "Cannot convert data types"

  end select 

end subroutine copy

! Create a new link w/ value
function constructor (value)
  implicit none
  class(link_t), pointer :: constructor
  class(*) :: value

  allocate (constructor)
  allocate (constructor%value, SOURCE=value)

end function constructor

! Destroy link (returns next pointer).
function destructor (this)
  implicit none
  class(link_t), pointer :: this
  class(link_t), pointer :: destructor

  destructor => null()
  if (associated(this)) then
    destructor => this%next
    deallocate (this%value)
    this%value => null()
    this%next => null()
    deallocate (this)
    this => null()
  end if

end function destructor

! List constructor
function list_constructor (value) result (list)
  implicit none
  class(*), intent(in) :: value(..)
  type(list_t) :: list

  select rank (value)
  rank (0)
    call list%append(value)
  rank (1)
    call list%extend(value)
  rank default
    error stop "Unsupported RANK size"
  end select

end function list_constructor

! Adds an element at the end of the list
subroutine list_append (this, value)
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: value
  class(link_t), pointer :: new

  if (.not. associated(this%first)) then
    ! Create first link_t and update last
    this%first => constructor(value)
    this%last => this%first

  else
    ! Append new link and update last.
    new => constructor(value)
    this%last%next => new
    this%last => new

  end if

end subroutine list_append

! Removes all the elements from the list
subroutine list_clear (this)
  implicit none
  class(list_t) :: this
  class(link_t), pointer :: curr => null()

  curr => this%first
  do while (associated(curr))
    curr => destructor(curr)
  end do
  this%first => null()
  this%last => null()

end subroutine list_clear

! Returns the number of elements with the specified value.
integer function list_count (this, value)
  implicit none
  class(list_t) :: this
  class(*) :: value
  class(link_t), pointer :: curr

  list_count = 0
  curr => this%first
  do while (associated(curr))
    if (equal(value, curr%value)) list_count = list_count + 1
    curr => curr%next 
  end do

end function list_count

! Add an array to the end of the current list
subroutine list_extend (this, array)
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: array(:)
  integer :: i

  do i = 1, size(array)
    call this%append(array(i))
  end do

end subroutine list_extend

! Returns the index of the first element with the specified value
integer function list_index (this, value)
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: value
  class(link_t), pointer :: curr
  integer :: i

  list_index = 0
  i = 0
  curr => this%first
  do while (associated(curr))
    i = i + 1 
    if (equal(value, curr%value)) then
      list_index = i
      exit
    end if
    curr => curr%next
  end do

end function list_index

! Adds an element at the specified position
subroutine list_insert (this, pos, value)
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  class(*), intent(in) :: value
  class(link_t), pointer :: new, curr, prev
  integer :: i

  new => null()

  if (.not. associated(this%first)) then
    ! Special case if not associated
    this%first => constructor(value)
    this%last => this%first
  
  else
    ! Go to selected link
    curr => this%first
    do i = 1, pos - 1
      if (.not. associated(curr%next)) exit
      prev => curr
      curr => curr%next    
    end do

    new => constructor(value)
    if (associated(this%first, curr)) then
      ! Special case if first
      new%next => this%first
      this%first => new

    else if (associated(this%last, curr)) then
      ! Special case if last
      curr%next => new
      this%last => new

    else
      new%next => curr
      prev%next => new

    end if
  end if

end subroutine list_insert

! Get number of elements on the list
integer function list_len (this)
  implicit none
  class(list_t) :: this
  class(link_t), pointer :: curr

  list_len = 0
  curr => this%first
  do while (associated(curr))
    list_len = list_len + 1
    curr => curr%next
  end do

end function list_len 

! Get an element from the list
subroutine list_get (this, pos, value)
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  class(*), intent(out) :: value
  class(link_t), pointer :: curr
  integer :: i

  if (.not. associated(this%first)) then
    error stop "No elements present on list"
  end if
    
  curr => this%first
  do i = 1, pos - 1
    if (.not. associated(curr%next)) exit
    curr => curr%next
  end do
  call copy (curr%value, value)
  
end subroutine list_get

! Removes the element at the specified position
subroutine list_pop (this, pos)
  implicit none
  class(list_t) :: this
  integer, intent(in), optional :: pos
  class(link_t), pointer :: curr, prev
  integer :: i

  if (.not. associated(this%first)) then
    error stop "No elements present on list"
  end if

  if (associated(this%first, this%last)) then
    ! Special case if only one element
    this%first => destructor(this%first)
    this%last => null()
  
  else
    ! Go to selected link
    if (present(pos)) then
      curr => this%first
      do i = 1, pos - 1
        if (.not. associated(curr%next)) exit
        prev => curr
        curr => curr%next    
      end do
    
    else
      curr => this%first
      do while (associated(curr%next))
        prev => curr
        curr => curr%next 
      end do

    end if
    
    if (associated(this%first, curr)) then
      ! Special case if first
      this%first => destructor(this%first)

    else if (associated(this%last, curr)) then
      ! Special case if last
      prev%next => destructor(curr)
      this%last => prev

    else
      prev%next => curr%next
      curr => destructor(curr)

    end if
  end if

end subroutine list_pop

! Removes the first item with the specified value
subroutine list_remove (this, value)
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: value
  integer :: i
  
  i = this%index(value)
  if (i > 0) call this%pop(i)
  
end subroutine list_remove

! Reverses the order of the list
subroutine list_reverse (this)
  implicit none
  class(list_t) :: this
  class(link_t), pointer :: curr, prev, next

  ! Source: https://www.geeksforgeeks.org/reverse-a-linked-list/

  prev => null()
  curr => this%first
  do while (associated(curr)) 
    next => curr%next
    curr%next => prev
    prev => curr
    curr => next
  end do
  this%last => this%first
  this%first => prev

end subroutine list_reverse

! Set a value of an element from the list
subroutine list_set (this, pos, value)
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  class(*), intent(in) :: value
  class(link_t), pointer :: curr
  integer :: i

  if (.not. associated(this%first)) then
    error stop "No elements present on list"
  end if
    
  curr => this%first
  do i = 1, pos - 1
    if (.not. associated(curr%next)) exit
    curr => curr%next
  end do
  
  deallocate (curr%value)
  allocate (curr%value, SOURCE=value)

end subroutine list_set

! Sorts the list
subroutine list_sort (this)
  implicit none
  class(list_t) :: this
  class(link_t), pointer :: curr

  curr => this%first
  
  error stop "Pending implementation"

end subroutine list_sort

! Write list as formatted string
subroutine write_formatted (this, unit, iotype, v_list, iostat, iomsg)
  implicit none
  class(list_t), intent(in) :: this
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype 
  integer, intent(in) :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg
  class(link_t), pointer :: curr 
  character(:), allocatable :: buffer
  character(128) :: tmp

  catch: block

    ! Print each element on a list to a buffer 
    buffer = ""
    if (associated(this%first)) then
      curr => this%first
      do while (associated(curr))
        select type (value => curr%value)
        type is (integer(INT32))
          write (tmp, *) value
        type is (integer(INT64))
          write (tmp, *) value
        type is (real(REAL32))
          write (tmp, *) value
        type is (real(REAL64))
          write (tmp, *) value
        type is (logical)
          write (tmp, *) value
        type is (character(*))
          write (tmp, *) "'" // value // "'"
        class default
          iostat = 1
          iomsg = "Usupported variable KIND"
          exit catch
        end select

        ! Add delimiter to string
        if (associated(curr%next)) then
          buffer = buffer // trim(adjustl(tmp)) // ", "
        else
          buffer = buffer // trim(adjustl(tmp))
        end if

        curr => curr%next

      end do
    end if

    ! Add formatting to buffer
    buffer = "[" // buffer // "]"
    
    if (iotype == "LISTDIRECTED") then
      write (unit, "(a)", IOSTAT=iostat, IOMSG=iomsg) buffer
      if (iostat /= 0) exit catch

    else if (iotype == "DT") then
      if (size(v_list) /= 0) then
        iomsg = "Integer-list for DT descriptor not supported"
        iostat = 1
        exit catch
      end if

      write (unit, "(a)", IOSTAT=iostat, IOMSG=iomsg) buffer
      if (iostat /= 0) exit catch

    else
      iostat = 1
      iomsg = "Unsupported iotype"
      exit catch

    end if

  end block catch

end subroutine write_formatted

end module xslib_list
