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

module xslib_list
  !! Module with primitive implementation of unlimited polymorphic linked list.
  !!
  !! @note
  !! To add new custom derived TYPE support you only have write extension
  !! to `equal`, and `copy` functions.
  !! @endnote
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  implicit none
  private
  public :: list_t

  type :: link_t
    !! Element of linked list
    class(*), pointer :: value => null()
    type(link_t), pointer :: next => null()
  end type link_t

  type :: list_t
    !! Implementation of unlimited polymorphic linked list derived type variable.
    !!
    !! A linked list is a linear data structure composed of individual elements called nodes.
    !! Unlike arrays, elements in a linked list are not stored in contiguous (adjacent) memory
    !! locations. Main advantage of lists over arrays is they can be expanded (infinitely) 
    !! without any memory allocation overhead.
    !!
    !! Example:
    !! ```Fortran
    !! type(list_t) :: list
    !! ```
    class(link_t), pointer, private :: first => null()
    class(link_t), pointer, private :: last => null()
  contains
    procedure :: append => list_append
    !! Append new element to the end of the list. 
    !!
    !! Example:
    !! ```Fortran
    !! type(list_t) :: list
    !! ```
    procedure :: clear => list_clear
    !! Remove all elements from the list.
    !!
    !! Example:
    !! ```Fortran
    !! call list%clear()
    !! print *, list
    !! >>> []
    !! ```
    procedure :: count => list_count
    !! Returns the number of `elem` elements (with the specified value) on the list.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! print *, list%count(1)
    !! >>> 2
    !! ```
    procedure :: extend => list_extend
    !! Append array of elements to the end of the list. 
    !!
    !! Example:
    !! ```Fortran
    !! call list%extend([1, 1, 2, 3, 5, 8, 13])
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! ```
    procedure :: index => list_index
    !! Returns the index of the first element on list with `elem`
    !! value. Returns 0 if element is not present.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! print *, list%index(8)
    !! >>> 6
    !! print *, list%index(21)
    !! >>> 0
    !! ```
    procedure :: len => list_len
    !! Returns number of all elements on the list.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! print *, list%len()
    !! >>> 7
    !! ```
    procedure :: insert => list_insert
    !! Adds an element `elem` at the specified `pos` position on the list.
    !! If position index is outside the list range it is either appended to
    !! the list if the index is larger than the list or prepended in index
    !! is smaller than one.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 1]
    !! call list%insert(2, 0)
    !! >>> [1, 0, 1, 1]
    !! ```
    procedure :: get => list_get
    !! Get element at `pos` index from the list. Raises error if `pos` index
    !! is out of range.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! call list%get(1, value)
    !! print *, value
    !! >>> 1
    !! ```
    procedure :: pop => list_pop
    !! Removes the element at the specified position `pos`. Last element is 
    !! removed if `pos` is not specified.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! call list%pop()
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8]
    !! call list%pop(1)
    !! print *, list
    !! >>> [1, 2, 3, 5, 8]
    !! ```
    procedure :: remove => list_remove
    !! Removes the __first__ occurrence of `elem` from the list. Does
    !!  nothing if elem is not on the list.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! call list%remove(2)
    !! print *, list
    !! >>> [1, 1, 3, 5, 8, 13]
    !! call list%remove(2)
    !! >>> [1, 1, 3, 5, 8, 13]
    !! ```
    procedure :: reverse => list_reverse
    !! Reverse element order on the list.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! call list%reverse()
    !! print *, list
    !! >>> [13, 8, 5, 3, 2, 1, 1]
    !! ```
    procedure :: same_type_as => list_same_type_as
    !! Returns `.True.` if the dynamic type of element at index `pos`
    !! is the same as the dynamic type of `elem`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! print *, list%same_type_as(1, 0)
    !! >>> .True.
    !! print *, list%same_type_as(1, 0.0)
    !! >>> .False.
    !! ```
    procedure :: set => list_set
    !! Change value of element at index `pos` on the list. Raises error if
    !! `pos` index is out of list range.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! call list%set(1, 0)
    !! print *, list
    !! >>> [0, 1, 2, 3, 5, 8, 13]
    !! ```
    procedure :: sort => list_sort
    !! Sort elements on the list in ascending order.
    !!
    !! @warning
    !! Implementation pending!
    !! @endwarning
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [8, 3, 13, 1, 2, 5, 1]
    !! call list%sort()
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! ```
    procedure, private :: write_formatted
    generic :: write(formatted) => write_formatted
    !! Formatted and unformatted write of `list_t`.
    !!
    !! Example:
    !! ```Fortran
    !! print *, list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! write (*, *) list
    !! >>> [1, 1, 2, 3, 5, 8, 13]
    !! ```
  end type list_t

  interface list_t
    !! Custom list constructor from an array.
    !!
    !! Example:
    !! ```Fortran
    !! list = list_t([1, 2, 3])
    !! print *, list
    !! >>> [1, 2, 3]
    !! ```
    module procedure :: list_constructor
  end interface list_t
 
contains

logical function equal (a, b)
  !! Strictly compare (===) two unlimited polymorphic variables
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

  type is (complex(REAL32))
    select type (b)
    type is (complex(REAL32))
      equal = (a == b)
    end select

  type is (complex(REAL64))
    select type (b)
    type is (complex(REAL64))
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


subroutine copy (src, dest, strict, stat, errmsg)
    !! Copy value from src to dest of two unlimited polymorphic variables.
    implicit none
    class(*), intent(IN) :: src
    !! Unlimited polymorphic variable to copy FROM.
    class(*), intent(OUT) :: dest
    !! Unlimited polymorphic variable to copy TO.
    logical, intent(IN), OPTIONAL :: strict
    !! Raise error if not same type. Default: .False.
    integer, intent(OUT), OPTIONAL :: stat
    !! Error status code. Returns zero if no error.
    character(*), intent(OUT), OPTIONAL :: errmsg
    !! Error message.
    character(256) :: buffer, message
    integer :: status

    status = 0
    catch: block 
 
        ! Check if strict copy
        if (present(strict)) then
            if (.not. same_type_as(src, dest) .and. strict) then
                status = 2
                message = "Type Error: Not same type"
                exit catch
            end if  
        end if

        select type (dest)
        type is (integer(INT32))
            select type (src)
            type is (integer(INT32))
                dest = int(src, kind=INT32)
            type is (integer(INT64))
                dest = int(src, kind=INT32)
            type is (real(REAL32))
                dest = int(src, kind=INT32)
            type is (real(REAL64))
                dest = int(src, kind=INT32)
            type is (complex(REAL32))
                dest = int(src, kind=INT32)
            type is (complex(REAL64))
                dest = int(src, kind=INT32)
            type is (character(*))
                read (src, *, iostat=status, iomsg=message) dest
            class default
                status = 1
                message = "Type Error: Could not convert for type 'int32'"
            end select

        type is (integer(INT64))
            select type (src)
            type is (integer(INT32))
                dest = int(src, kind=INT64)
            type is (integer(INT64))
                dest = int(src, kind=INT64)
            type is (real(REAL32))
                dest = int(src, kind=INT64)
            type is (real(REAL64))
                dest = int(src, kind=INT64)
            type is (complex(REAL32))
                dest = int(src, kind=INT32)
            type is (complex(REAL64))
                dest = int(src, kind=INT32)
            type is (character(*))
                read (src, *, iostat=status, iomsg=message) dest
            class default
                status = 1
                message = "Type Error: Could not convert for type 'int64'"
            end select

        type is (real(REAL32))
            select type (src)
            type is (integer(INT32))
                dest = real(src, kind=REAL32)
            type is (integer(INT64))
                dest = real(src, kind=REAL32)
            type is (real(REAL32))
                dest = real(src, kind=REAL32)
            type is (real(REAL64))
                dest = real(src, kind=REAL32)
            type is (complex(REAL32))
                dest = real(src, kind=REAL32)
            type is (complex(REAL64))
                dest = real(src, kind=REAL32)
            type is (character(*))
                read (src, *, iostat=status, iomsg=message) dest
            class default
                status = 1
                message = "Type Error: Could not convert for type 'real32'"
            end select

        type is (real(REAL64))
            select type (src)
            type is (integer(INT32))
                dest = real(src, kind=REAL64)
            type is (integer(INT64))
                dest = real(src, kind=REAL64)
            type is (real(REAL32))
                dest = real(src, kind=REAL64)
            type is (real(REAL64))
                dest = real(src, kind=REAL64)
            type is (complex(REAL32))
                dest = real(src, kind=REAL64)
            type is (complex(REAL64))
                dest = real(src, kind=REAL64)
            type is (character(*))
                read (src, *, iostat=status, iomsg=message) dest
            class default
                status = 1
                message = "Type Error: Could not convert for type 'real64'"
            end select

        type is (complex(REAL32))
            select type (src)
            type is (integer(INT32))
                dest = cmplx(src, kind=REAL32)
            type is (integer(INT64))
                dest = cmplx(src, kind=REAL32)
            type is (real(REAL32))
                dest = cmplx(src, kind=REAL32)
            type is (real(REAL64))
                dest = cmplx(src, kind=REAL32)
            type is (complex(REAL32))
                dest = cmplx(src, kind=REAL32)
            type is (complex(REAL64))
                dest = cmplx(src, kind=REAL32)
            type is (character(*))
                read (src, *, iostat=status, iomsg=message) dest
            class default
                status = 1
                message = "Type Error: Could not convert for type 'complex32'"
            end select

        type is (complex(REAL64))
            select type (src)
            type is (integer(INT32))
                dest = cmplx(src, kind=REAL64)
            type is (integer(INT64))
                dest = cmplx(src, kind=REAL64)
            type is (real(REAL32))
                dest = cmplx(src, kind=REAL64)
            type is (real(REAL64))
                dest = cmplx(src, kind=REAL64)
            type is (complex(REAL32))
                dest = cmplx(src, kind=REAL64)
            type is (complex(REAL64))
                dest = cmplx(src, kind=REAL64)
            type is (character(*))
                read (src, *, iostat=status, iomsg=message) dest
            class default
                status = 1
                message = "Type Error: Could not convert for type 'complex64'"
            end select

        type is (logical)
            select type (src)
            type is (logical)
                dest = src
            type is (character(*))
                read (src, *, iostat=status, iomsg=message) dest
            class default
                status = 1
                message = "Type Error: Could not convert for type 'logical'"
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
            type is (complex(REAL32))
                write (buffer,*) src
                dest = trim(adjustl(buffer))
            type is (complex(REAL64))
                write (buffer,*) src
                dest = trim(adjustl(buffer))
            type is (character(*))
                dest = src
            class default
                status = 1
                message = "Type Error: Could not convert for type 'character'"
            end select

        class default
            status = 1
            message = "Type Error: Unsupported data type"

        end select

    end block catch

    ! Error handling
    if (present(stat)) then
        stat = status
    else if (status /= 0) then
        error stop message
    end if
    if (present(errmsg)) errmsg = trim(message)

end subroutine copy


function constructor (value)
  !! Create a new link w/ value
  implicit none
  class(link_t), pointer :: constructor
  class(*) :: value

  allocate (constructor)
  allocate (constructor%value, SOURCE=value)

end function constructor


function destructor (this)
  !! Destroy link (returns next pointer).
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
  !! List constructor
  implicit none
  class(*), intent(in) :: value(..)
  type(list_t) :: list

  select rank (value)
  rank (0)
    call list%append(value)
  rank (1)
    call list%extend(value)
  rank default
    error stop "Value Error: Unsupported RANK size"
  end select

end function list_constructor


subroutine list_append (this, elem)
  !! Append new element to the end of the list. 
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: elem
  !! Element to be added end of the list.
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
  !! Remove all elements from the list.
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
  !! Count number of `elem` elements on the list.
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: elem
  !! Value of elements to search on the list.
  integer :: out
  !! Number of elements with specified value on the list.
  class(link_t), pointer :: curr

  out = 0
  curr => this%first
  do while (associated(curr))
    if (equal(elem, curr%value)) out = out + 1
    curr => curr%next 
  end do

end function list_count


subroutine list_extend (this, array)
  !! Append array of elements to the end of the list. 
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: array(:)
  !! Array of elements to be added to the list.
  integer :: i

  do i = 1, size(array)
    call this%append(array(i))
  end do

end subroutine list_extend


function list_index (this, elem) result (out)
  !! Returns the index of the first element on list with `elem` value.
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: elem
  !! Value of element to index.
  integer :: out
  !! Index of element on the list. Returns 0 if element is not present.
  class(link_t), pointer :: curr
  integer :: i

  out = 0
  i = 0
  curr => this%first
  do while (associated(curr))
    i = i + 1 
    if (equal(elem, curr%value)) then
      out = i
      exit
    end if
    curr => curr%next
  end do

end function list_index


subroutine list_insert (this, pos, elem)
  !! Append element to the list at specified position
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  !! A number specifying in which position to insert the element.
  class(*), intent(in) :: elem
  !! Element to be added to the list.
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


function list_len (this) result (out)
  !! Count number of elements on the list.
  implicit none
  class(list_t) :: this
  integer :: out
  !! Number number of elements on the list.
  class(link_t), pointer :: curr

  out = 0
  curr => this%first
  do while (associated(curr))
    out = out + 1
    curr => curr%next
  end do

end function list_len 


subroutine list_get (this, pos, elem)
  !! Get element at `pos` index from the list. Raises error if `pos` index is out of range.
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  !! A number specifying at which position to get element.
  class(*), intent(out) :: elem
  !! Corresponding element from to the list.
  class(link_t), pointer :: curr
  integer :: i

  if (.not. associated(this%first)) then
    error stop "Index Error: No elements present on list"
  end if
    
  curr => this%first
  do i = 1, pos - 1
    if (.not. associated(curr%next)) exit
    curr => curr%next
  end do
  call copy (curr%value, elem)
  
end subroutine list_get


subroutine list_pop (this, pos)
  !! Removes the element at the specified position `pos`.
  implicit none
  class(list_t) :: this
  integer, intent(in), optional :: pos
  !! A number specifying the position of the element you want to remove.
  !! Last element is removed if not specified.
  class(link_t), pointer :: curr, prev
  integer :: i

  if (.not. associated(this%first)) then
    error stop "Index Error: No elements present on list"
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
  !! Removes the first occurrence of `elem` from the list.
  implicit none
  class(list_t) :: this
  class(*), intent(in) :: elem
  !! Element to be removed from the list.
  integer :: i
  
  i = this%index(elem)
  if (i > 0) call this%pop(i)
  
end subroutine list_remove


subroutine list_reverse (this)
  !! Reverse element order on the list
  ! See: https://www.geeksforgeeks.org/reverse-a-linked-list/
  implicit none
  class(list_t) :: this
  class(link_t), pointer :: curr, prev, next

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
  !! Check if element on list is same type as reference
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  !! A number specifying at which position to check the element.
  class(*), intent(in) :: elem
  !! Element against which to compare the type.
  logical :: out
  !! Returns `.True.` if evaluated elements are of same type.
  class(link_t), pointer :: curr
  integer :: i

  if (.not. associated(this%first)) then
    error stop "Index Error: No elements present on list"
  end if
    
  curr => this%first
  do i = 1, pos - 1
    if (.not. associated(curr%next)) exit
    curr => curr%next
  end do
  
  out = same_type_as(curr%value, elem)

end function list_same_type_as


subroutine list_set (this, pos, elem)
  !! Change value of element at index `pos` on the list. Raises error 
  !! if `pos` index is out of list range.
  implicit none
  class(list_t) :: this
  integer, intent(in) :: pos
  !! A number specifying at which position to set element.
  class(*), intent(in) :: elem
  !! Element to be replaced on the list.
  class(link_t), pointer :: curr
  integer :: i

  if (.not. associated(this%first)) then
    error stop "Index Error: No elements present on list"
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
  !! Sort elements on the list in ascending order.
  implicit none
  class(list_t) :: this

  error stop "Not implemented"

end subroutine list_sort


subroutine write_formatted (this, unit, iotype, v_list, iostat, iomsg)
  !! Formatted and unformatted write 
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
        call copy(curr%value, tmp, stat=iostat, errmsg=iomsg)
        if (iostat /= 0) exit catch

        ! Add quotation marks if string
        select type (v => curr%value)
        type is (character(*))
          tmp = "'" // trim(tmp) // "'"
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
        iomsg = "I/O Error: Integer-list for DT descriptor not supported"
        iostat = 1
        exit catch
      end if

      write (unit, "(a)", IOSTAT=iostat, IOMSG=iomsg) buffer
      if (iostat /= 0) exit catch

    else
      iostat = 1
      iomsg = "I/O Error: Unsupported iotype"
      exit catch

    end if

  end block catch

end subroutine write_formatted

end module xslib_list
