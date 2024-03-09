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

  ! %%%
  ! #  `LIST` - Linked list functions
  !   Module `xslib_list` contains primitive implementation of unlimited polymorphic linked list.
  !   List currently supports only `INT32`, `INT64`, `REAL32`, `REAL64`, `LOGICAL`, and `CHARACTER(*)`
  !   variable types. To add new derived TYPE support you only have write extension to `equal`, `copy`,
  !   and (optional) `write` functions.
  ! %%%

  type link_t
    class(*), pointer :: value => null()
    type(link_t), pointer :: next => null()
  end type link_t

  type list_t
    ! %%%
    ! ## `LIST_T` - Polymorphic linked list
    ! #### DESCRIPTION
    !   Implementation of unlimited polymorphic linked list derived type variable. Supports `INT32`, `INT64`, 
    !   `REAL32`, `REAL64`, `LOGICAL`, and `CHARACTER(*)` variable types. Variables on list cannot be directly
    !   accessed and can be set via `append`, `extend`, and `set` functionality or retrieved via `get` functionality.  
    ! #### USAGE
    !   ```Fortran
    !   > type(list_t) :: list
    !   ```
    ! %%%
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
    procedure :: same_type_as => list_same_type_as
    procedure :: set => list_set
    procedure :: sort => list_sort
    procedure, private :: write_formatted
    generic :: write(formatted) => write_formatted
  end type list_t

  interface list_t
    module procedure :: list_constructor
  end interface list_t
 
contains

logical function equal (a, b)
  ! Strictly compare (===) two unlimited polymorphic variables
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


subroutine copy (src, dest)
  ! Copy value from src to dest of two unlimited polymorphic variables
  implicit none
  class(*), intent(in) :: src
  class(*), intent(out) :: dest
  character(128) :: buffer

  select type (dest)
  type is (integer(INT32))
    select type (src)
    type is (integer(INT32))
      dest = int(src, INT32)
    type is (integer(INT64))
      dest = int(src, INT32)
    type is (real(REAL32))
      dest = int(src, INT32)
    type is (real(REAL64))
      dest = int(src, INT32)
    class default
      error stop "Cannot convert data types"
    end select

  type is (integer(INT64))
    select type (src)
    type is (integer(INT32))
      dest = int(src, INT64)
    type is (integer(INT64))
      dest = int(src, INT64)
    type is (real(REAL32))
      dest = int(src, INT64)
    type is (real(REAL64))
      dest = int(src, INT64)
    class default
      error stop "Cannot convert data types"
    end select

  type is (real(REAL32))
    select type (src)
    type is (integer(INT32))
      dest = real(src, REAL32)
    type is (integer(INT64))
      dest = real(src, REAL32)
    type is (real(REAL32))
      dest = real(src, REAL32)
    type is (real(REAL64))
      dest = real(src, REAL32)
    class default
      error stop "Cannot convert data types"
    end select

  type is (real(REAL64))
    select type (src)
    type is (integer(INT32))
      dest = real(src, REAL64)
    type is (integer(INT64))
      dest = real(src, REAL64)
    type is (real(REAL32))
      dest = real(src, REAL64)
    type is (real(REAL64))
      dest = real(src, REAL64)
    class default
      error stop "Cannot convert data types"
    end select

  type is (logical)
    select type (src)
    type is (logical)
      dest = src
    class default
      error stop "Cannot convert data types"
    end select

  type is (character(*))
    select type (src)
    type is (integer(INT32))
      write (buffer,*) src
      dest = trim(adjustl(buffer))
    type is (integer(INT64))
      write (buffer,*) src
      dest = trim(adjustl(buffer))
    type is (real(REAL32))
      write (buffer,*) src
      dest = trim(adjustl(buffer))
    type is (real(REAL64))
      write (buffer,*) src
      dest = trim(adjustl(buffer))
    type is (logical)
      write (buffer,*) src
      dest = trim(adjustl(buffer))
    type is (character(*))
      dest = src
    class default
      error stop "Cannot convert data types"
    end select

  class default
    error stop "Cannot convert data types"

  end select 

end subroutine copy


function constructor (value)
  ! Create a new link w/ value
  implicit none
  class(link_t), pointer :: constructor
  class(*) :: value

  allocate (constructor)
  allocate (constructor%value, SOURCE=value)

end function constructor


function destructor (this)
  ! Destroy link (returns next pointer).
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


function list_constructor (value) result (list)
  ! List constructor
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


subroutine list_append (this, elem)
  ! %%%
  ! ## `LIST%APPEND` - Append element to the list
  ! #### DESCRIPTION
  !   Append new element to the end of the list. 
  ! #### USAGE
  !   ```Fortran
  !   call list%append(elem)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), intent(IN) :: elem`
  !     Element to be added end of the list.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 3])
  !   > call list%append(4])
  !   [1, 2, 3, 4]
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: elem
  class(link_t), pointer :: new

  if (.not. associated(this%first)) then
    ! Create first link and update last
    this%first => constructor(elem)
    this%last => this%first

  else
    ! Append new link and update last.
    new => constructor(elem)
    this%last%next => new
    this%last => new

  end if

end subroutine list_append


subroutine list_clear (this)
  ! %%%
  ! ## `LIST%CLEAR` - Removes all elements from the list
  ! #### DESCRIPTION
  !   Remove ALL elements from the list.
  ! #### USAGE
  !   ```Fortran
  !   call list%clear()
  !   ```
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(lit_t) :: list
  !   > list = list_t([1, 2, 3])
  !   > call list%clear()
  !   []
  !   ```  
  ! %%%
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


function list_count (this, elem) result (out)
  ! %%%
  ! ## `LIST%COUNT` - Count elements on the list
  ! #### DESCRIPTION
  !   Returns the number of `elem` elements (with the specified value) on the list.
  ! #### USAGE
  !   ```Fortran
  !   out = list%count(elem)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), intent(IN) :: elem`
  !     Value of elements to search on the list.
  !   * `integer :: out`
  !     Number of elements with specified value on the list.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 1])
  !   > list%count(1)
  !   2
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: elem
  integer :: out
  class(link_t), pointer :: curr

  out = 0
  curr => this%first
  do while (associated(curr))
    if (equal(elem, curr%value)) out = out + 1
    curr => curr%next 
  end do

end function list_count


subroutine list_extend (this, array)
  ! %%%
  ! ## `LIST%EXTEND` - Append array of elements to the list
  ! #### DESCRIPTION
  !   Append array of elements to the end of the list. 
  ! #### USAGE
  !   ```Fortran
  !   call list%append(array)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), dimension(:), intent(IN) :: array`
  !     Array of elements to be added to the list.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2])
  !   > call list%extend([3, 4, 5])
  !   [1, 2, 3, 4, 5]
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: array(:)
  integer :: i

  do i = 1, size(array)
    call this%append(array(i))
  end do

end subroutine list_extend


integer function list_index (this, elem)
  ! %%%
  ! ## `LIST%INDEX` - Return index of element on the list
  ! #### DESCRIPTION
  !   Returns the index of the first element on list with `elem` value.
  ! #### USAGE
  !   ```Fortran
  !   out = list%index(elem)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), intent(IN) :: elem`
  !     Value of element to index.
  !   * `integer :: out`
  !     Index of element on the list. Returns zero if element is not present.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 3])
  !   > list%index(1)
  !   1
  !   > list%index(4)
  !   0
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: elem
  class(link_t), pointer :: curr
  integer :: i

  list_index = 0
  i = 0
  curr => this%first
  do while (associated(curr))
    i = i + 1 
    if (equal(elem, curr%value)) then
      list_index = i
      exit
    end if
    curr => curr%next
  end do

end function list_index


subroutine list_insert (this, pos, elem)
  ! %%%
  ! ## `LIST%INSERT` - Append element to the list at specified position
  ! #### DESCRIPTION
  !   Adds an element `elem` at the specified `pos` position on the list. If position index is outside the list range
  !   it is either appended to the list if the index is larger than the list or prepended in index is smaller than one.  
  ! #### USAGE
  !   ```Fortran
  !   call list%insert(pos, elem)
  !   ```
  ! #### PARAMETERS
  !   * `integer, intent(IN) :: pos`
  !     A number specifying in which position to insert the element.
  !   * `class(*), intent(IN) :: elem`
  !     Element to be added to the list.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 3])
  !   > list%insert(2, 1.5)
  !   [1, 1.5, 2, 3]
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  class(*), intent(in) :: elem
  class(link_t), pointer :: new, curr, prev
  integer :: i

  new => constructor(elem)

  if (.not. associated(this%first)) then
    ! Special case if not associated
    this%first => new
    this%last => this%first
  
  else
    ! Go to selected link
    curr => this%first
    do i = 1, pos - 1
      if (.not. associated(curr%next)) exit
      prev => curr
      curr => curr%next    
    end do

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


integer function list_len (this)
  ! %%%
  ! ## `LIST%LEN` - Count number of elements on the list
  ! #### DESCRIPTION
  !   Get number of ALL elements on the list.
  ! #### USAGE
  !   ```Fortran
  !   out = list%len()
  !   ```
  ! #### PARAMETERS
  !   * `integer :: out`
  !     Number of all elements on the list.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 3])
  !   > list%len()
  !   3
  !   ```
  ! %%%
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


subroutine list_get (this, pos, elem)
  ! %%%
  ! ## `LIST%GET` - Get an element from the list
  ! #### DESCRIPTION
  !   Get element at `pos` index from the list. Raises error if `pos` index
  !   is out of range.
  ! #### USAGE
  !   ```Fortran
  !   call list%get(pos, elem)
  !   ```
  ! #### PARAMETERS
  !   * `integer, intent(IN) :: pos`
  !     A number specifying at which position to get element.
  !   * `class(*), intent(IN) :: elem`
  !     Corresponding element from to the list.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 3])
  !   > call list%get(1, val)
  !   > print *, val
  !   1
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  class(*), intent(out) :: elem
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
  call copy (curr%value, elem)
  
end subroutine list_get


subroutine list_pop (this, pos)
  ! %%%
  ! ## `LIST%POP` - Removes element at the specified position from the list
  ! #### DESCRIPTION
  !   Removes the element at the specified position `pos`. Last element is removed if
  !   `pos` is not specified.
  ! #### USAGE
  !   ```Fortran
  !   call list%pop(pos=pos)
  !   ```
  ! #### PARAMETERS
  !   * `integer, intent(IN), OPTIONAL :: pos`
  !     A number specifying the position of the element you want to remove.
  !     Last element is removed if not specified.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 3, 4])
  !   > call list%pop()
  !   [1, 2, 3]
  !   > call list%pop(1)
  !   [2, 3]
  !   ```
  ! %%%
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
    this%first => null()
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


subroutine list_remove (this, elem)
  ! %%%
  ! ## `LIST%REMOVE` - Remove element from the list
  ! #### DESCRIPTION
  !   Removes the first occurrence of `elem` from the list.
  ! #### USAGE
  !   ```Fortran
  !   call list%remove(value)
  !   ```
  ! #### PARAMETERS
  !   * `class(*), intent(IN) :: elem`
  !     Element to be removed from the list.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 3])
  !   > call list%remove(2)
  !   [1, 3]
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: elem
  integer :: i
  
  i = this%index(elem)
  if (i > 0) call this%pop(i)
  
end subroutine list_remove


subroutine list_reverse (this)
  ! %%%
  ! ## `LIST%REVERSE` - Reverse element order on the list
  ! #### DESCRIPTION
  !   Reverse element order on the list.
  ! #### USAGE
  !   ```Fortran
  !   call list%reverse()
  !   ```
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 3])
  !   > call list%reverse()
  !   [3, 2, 1]
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  class(link_t), pointer :: curr, prev, next

  ! See: https://www.geeksforgeeks.org/reverse-a-linked-list/

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


function list_same_type_as (this, pos, elem) result (out)
  ! %%%
  ! ## `LIST%SAME_TYPE_AS` - Check if element on list is same type as reference
  ! #### DESCRIPTION
  !   Returns `.True.` if the dynamic type of element at index `pos` is the same as the dynamic type of `elem`.
  ! #### USAGE
  !   ```Fortran
  !   out = list%same_type_as(pos, elem)
  !   ```
  ! #### PARAMETERS
  !   * `integer, intent(IN) :: pos`
  !     A number specifying at which position to check the element.
  !   * `class(*), intent(IN) :: elem`
  !     Element against which to compare the type.
  !   * `logical :: out`
  !     Returns `.True.` if evaluated elements are of same type.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > call list%extend([1, 2])
  !   > list%same_type_as(1, 0)
  !   .True.
  !   > list%same_type_as(1, "one")
  !   .False.
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  class(*), intent(in) :: elem
  class(link_t), pointer :: curr
  logical :: out
  integer :: i

  if (.not. associated(this%first)) then
    error stop "No elements present on list"
  end if
    
  curr => this%first
  do i = 1, pos - 1
    if (.not. associated(curr%next)) exit
    curr => curr%next
  end do
  
  out = same_type_as(curr%value, elem)

end function list_same_type_as


subroutine list_set (this, pos, elem)
  ! %%%
  ! ## `LIST%SET` - Change element on the list
  ! #### DESCRIPTION
  !   Change value of element at index `pos` on the list. Raises error if `pos` index
  !   is out of list range.
  ! #### USAGE
  !   ```Fortran
  !   call list%set(pos, elem)
  !   ```
  ! #### PARAMETERS
  !   * `integer, intent(IN) :: pos`
  !     A number specifying at which position to set element.
  !   * `class(*), intent(IN) :: elem`
  !     Element to be replaced on the list.
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([1, 2, 3])
  !   > call list%set(1, 0)
  !   [0, 2, 3]
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  class(*), intent(in) :: elem
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
  allocate (curr%value, SOURCE=elem)

end subroutine list_set


subroutine list_sort (this)
  ! %%%
  ! ## `LIST%SORT` - Sort elements on the list
  ! #### DESCRIPTION
  !   Sort elements on the list in ascending order.
  !   __WARING:__ Implementation pending!
  ! #### USAGE
  !   ```Fortran
  !   call list%sort()
  !   ```
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_t([3, 1, 2, 4])
  !   > call list%sort()
  !   [1, 2, 3, 4]
  !   ```
  ! %%%
  implicit none
  class(list_t) :: this
  class(link_t), pointer :: curr

  curr => this%first
  error stop "Pending implementation"

end subroutine list_sort


subroutine write_formatted (this, unit, iotype, v_list, iostat, iomsg)
  ! %%%
  ! ## `LIST%WRITE` - Formatted and unformatted write 
  ! #### DESCRIPTION
  !   Allows for formatted and unformatted write of `list_t`.
  ! #### USAGE
  !   ```Fortran
  !   print *, list_t
  !   write (*, *) list_t
  !   ```
  ! #### EXAMPLE
  !   ```Fortran
  !   > type(list_t) :: list
  !   > list = list_T([1, 2, 3])
  !   > print *, list
  !   [1, 2, 3]
  !   > write (*, *) list_t
  !   [1, 2, 3]
  !   ```
  ! %%%  
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

        ! Add delimiter to string if not last element
        if (associated(curr, this%last)) then
          buffer = buffer // trim(adjustl(tmp))
        else
          buffer = buffer // trim(adjustl(tmp))  // "," // " "
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
