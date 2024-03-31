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

module xslib_dict
    use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
    implicit none
    private
    public :: dict_t

    ! %%%
    ! #  `LIST` - Dictionary (hash table)
    !   Module `xslib_dict` contains primitive of unlimited polymorphic dictionary (hash table). It supports
    !   `INTEGER`, `REAL`, `COMPLEX`, `LOGICAL`, and `CHARACTER(*)` variable types (single or double precision).
    !   To add new custom derived TYPE support you only have to write extension to `hash_function` and `copy` functions.
    ! %%%

    ! Hash value default precision
    integer, parameter :: HDP = INT32

    type :: link_t
        integer(HDP) :: hash
        class(*), pointer :: key => null()
        class(*), pointer :: value => null()
        type(link_t), pointer :: next => null()
    end type link_t

    type :: list_t
        integer :: length = 0
        class(link_t), pointer :: first => null()
        class(link_t), pointer :: last => null()
    contains
        procedure :: append => list_append
        procedure :: clear => list_clear
        procedure :: pop => list_pop
        procedure :: get => list_get
    end type list_t

    ! Initial bucket size and max number of items in bucket
    ! TODO: Optimize this values!  
    integer, parameter :: INIT_BUCKETS = 128
    integer, parameter :: MAX_LENGTH = 64

    type :: dict_t
        ! %%%
        ! ## `DICT_T` - Polymorphic dictionary (hash table)
        ! #### DESCRIPTION
        !   Implementation of unlimited polymorphic dictionary (hash table) derived type variable. Supports `INTEGER`, `REAL`, 
        !   `COMPLEX`, `LOGICAL`, and `CHARACTER(*)` variable types (single or double precision). Values and keys cannot be
        !   directly accessed, only via `put`, `get`, `keys`, `values` or `items` functionality.  
        ! #### USAGE
        !   ```Fortran
        !   > type(dict_t) :: dict
        !   ```
        ! %%%
        integer, private :: n_buckets = 0 
        type(list_t), allocatable, private :: bucket(:)  
    contains
        procedure :: clear => dict_clear
        procedure, private :: fetch => dict_fetch
        procedure :: get => dict_get
        procedure, non_overridable, nopass :: hash_function
        procedure :: items => dict_items
        procedure :: keys => dict_keys
        procedure :: len => dict_len
        procedure :: put => dict_put
        procedure :: remove => dict_remove
        procedure, private :: resize => dict_resize
        procedure :: same_type_as => dict_same_type_as
        procedure :: values => dict_values
        procedure, private :: write_formatted
        generic :: write(formatted) => write_formatted
    end type dict_t

contains

! Return DJB2 hash for a given key
function hash_function (key) result (hash)
    implicit none
    class(*), intent(in) :: key
    integer(HDP) :: hash
    character(256) :: buffer
    integer :: n

    select type (key)
    type is (integer(INT32))
        write (buffer, *) key
    type is (integer(INT64))
        write (buffer, *) key 
    type is (real(REAL32))
        write (buffer, *) key 
    type is (real(REAL64))
        write (buffer, *) key
    type is (complex(REAL32))
        write (buffer, *) key 
    type is (complex(REAL64))
        write (buffer, *) key 
    type is (character(*))
        buffer = key
    class default
        ! Well, we tried our best... can't have them all.
        error stop "Type Error: Unsupported variable type"
    end select

    hash = 5381
    do n = 1, len_trim(buffer)
        hash = (shiftl(hash, 5) + hash) + iachar(buffer(n:n))
    end do

end function hash_function


! Copy value from src to dest of two unlimited polymorphic variables
subroutine copy (src, dest, strict, stat, errmsg)
    implicit none
    class(*), intent(IN) :: src
    !> Unlimited polymorphic variable to copy FROM.
    class(*), intent(OUT) :: dest
    !> Unlimited polymorphic variable to copy TO.
    logical, intent(IN), OPTIONAL :: strict
    !> Raise error if not same type. Default: .False.
    integer, intent(OUT), OPTIONAL :: stat
    !> Error status code. Returns zero if no error.
    character(*), intent(OUT), OPTIONAL :: errmsg
    !> Error message.
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


! Create a new link w/ key-value pair
function link_constructor (hash, key, value) result (link)
  implicit none
  class(link_t), pointer :: link
  integer(HDP), intent(IN) :: hash
  class(*), intent(IN) :: key, value

  allocate (link)
  link%hash = hash
  allocate (link%key, SOURCE=key)
  allocate (link%value, SOURCE=value)

end function link_constructor


! Destroy link and return link's next pointer
function link_destructor (link) result (next)
  implicit none
  class(link_t), pointer :: link
  class(link_t), pointer :: next

  next => null()
  if (associated(link)) then
    next => link%next
    deallocate (link%key, link%value)
    link%key => null()
    link%value => null()
    link%next => null()
    deallocate (link)
    link => null()
  end if

end function link_destructor


! Append or replace key-value pair to the list
subroutine list_append (this, hash, key, value)
    implicit none
    class(list_t) :: this
    integer(HDP) :: hash
    class(*), intent(in) :: key, value
    class(link_t), pointer :: curr, prev, new
    
    if (.not. associated(this%first)) then
        ! Special case if empty list
        this%first => link_constructor(hash, key, value)
        this%last => this%first

    else
        ! Iterate through list and compare hashes
        curr => this%first
        do while (associated(curr))
            if (curr%hash == hash) exit
            prev => curr
            curr => curr%next    
        end do

        if (associated(curr)) then
            ! If hash already exists replace value
            deallocate (curr%key, curr%value)
            allocate (curr%key, SOURCE=key)
            allocate (curr%value, SOURCE=value)

        else
            ! Append new link and update last.
            new => link_constructor(hash, key, value)
            this%last%next => new
            this%last => new

        end if 
    end if

    ! Increase list length    
    this%length = this%length + 1

end subroutine list_append


! Removes all key-value pairs from the list
subroutine list_clear (this)
    implicit none
    class(list_t) :: this
    class(link_t), pointer :: curr => null()

    curr => this%first
    do while (associated(curr))
        curr => link_destructor(curr)
    end do
    this%first => null()
    this%last => null()
    this%length = 0

end subroutine list_clear


! Removes key-value pair with specified hash value from the list
subroutine list_pop (this, hash)
    implicit none
    class(list_t) :: this
    integer, intent(in) :: hash
    class(link_t), pointer :: curr, prev

    ! Skip if list is empty
    if (.not. associated(this%first)) return

    if (associated(this%first, this%last)) then
        ! Special case if only one element
        this%first => link_destructor(this%first)
        this%first => null()
        this%last => null()
    
    else
        ! Iterate through list and compare hashes
        curr => this%first
        do while (associated(curr))
            if (curr%hash == hash) exit
            prev => curr
            curr => curr%next    
        end do

        ! Skip if hash is not on the list
        if (.not. associated(curr)) return

        if (associated(this%first, curr)) then
            ! Special case if first
            this%first => link_destructor(this%first)

        else if (associated(this%last, curr)) then
            ! Special case if last
            prev%next => link_destructor(curr)
            this%last => prev

        else
            prev%next => curr%next
            curr => link_destructor(curr)

        end if
    end if

    ! Decrease list length
    this%length = this%length - 1 

end subroutine list_pop


! Get key-value with pair from the list
subroutine list_get (this, hash, key, value)
    implicit none
    class(list_t) :: this
    integer, intent(IN) :: hash
    class(*), pointer, intent(OUT) :: key
    class(*), pointer, intent(OUT) :: value
    class(link_t), pointer :: curr

    key => null()
    value => null()

    if (.not. associated(this%first)) return
        
    curr => this%first
    do while (associated(curr))
        if (curr%hash == hash) then
            key => curr%key
            value => curr%value
            exit
        end if 
        curr => curr%next
    end do
  
end subroutine list_get


! Clear all key-value pairs from dictionary
subroutine dict_clear (this)
    ! %%%
    ! ## `DICT_T%CLEAR` - Removes all items from dictionary
    ! #### DESCRIPTION
    !   Removes ALL items from dictionary.
    ! #### USAGE
    !   ```Fortran
    !   call dict%clear()
    !   ```
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%clear()
    !   > print *, dict
    !   {}
    !   ```
    ! %%%
    implicit none
    class(dict_t) :: this
    integer :: i

    if (allocated(this%bucket)) then
        do i = 1, this%n_buckets 
            call this%bucket(i)%clear()
        end do
        deallocate (this%bucket)
        this%n_buckets = 0
    end if

end subroutine dict_clear


! Get key-value pair from the n-th position in dictionary
subroutine dict_fetch (this, pos, key, value)
    implicit none
    class(dict_t) :: this
    integer, intent(IN) :: pos
    class(*), pointer, intent(OUT) :: key, value
    type(link_t), pointer :: curr
    integer :: i, n, cnt

    ! Check input
    if (.not. allocated(this%bucket) .or. this%len() < pos) then
        error stop "Index Error: Index outside defined range"
    end if

    ! Find correct bucket
    cnt = pos
    n = 1
    do while (n < this%n_buckets)
        if (cnt <= this%bucket(n)%length) exit
        cnt = cnt - this%bucket(n)%length
        n = n + 1 
    end do

    ! Go to correct item in bucket
    curr => this%bucket(n)%first
    do i = 1, cnt - 1
        curr => curr%next
    end do    
    
    ! Grab item from the list
    if (associated(curr)) then
        key => curr%key
        value => curr%value
    else
        error stop "Index Error: Something went wrong"
    end if

end subroutine dict_fetch


! Get value from dictionary
subroutine dict_get (this, key, value, default)
    ! %%%
    ! ## `DICT_T%GET` - Get value from dictionary
    ! #### DESCRIPTION
    !   Get the value of the item with the specified key. Returns default value if item is not
    !   in the dictionary if present, other raises an error.
    ! #### USAGE
    !   ```Fortran
    !   call list%get(key, value, default=default)
    !   ```
    ! #### PARAMETERS
    !   * `class(*), intent(IN) :: key`
    !     The key of the item you want to return the value from.
    !   * `class(*), intent(OUT) :: value`
    !     A value of specified key.
    !   * `class(*), intent(IN), OPTIONAL :: default`
    !     A value to return if the specified key does not exist.
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%put("one", 1)
    !   > call dict%get("one", value)
    !   > print *, value
    !   1
    !   > call dict%get("one", value, default=0)
    !   > print *, value
    !   0
    !   ```  
    ! %%%
    implicit none
    class(dict_t) :: this
    class(*), intent(IN) :: key
    class(*), intent(OUT) :: value
    class(*), intent(IN), optional :: default
    class(*), pointer :: k, v
    integer(HDP) :: hash
    integer :: i

    if (allocated(this%bucket)) then
        hash = this%hash_function(key)
        i = modulo(hash, this%n_buckets)
        call this%bucket(i)%get(hash, k, v)
    end if

    if (associated(v)) then
        call copy(v, value)
    else if (present(default)) then
        call copy(default, value)
    else 
        error stop "Key Error: Not in dictionary"
    end if 

end subroutine dict_get


! Get n-th key-value pair from dictionary
subroutine dict_items (this, pos, key, value)
    ! %%%
    ! ## `DICT_T%ITEMS` - Get n-th item from dictionary
    ! #### DESCRIPTION
    !   Get the key-value item pair from `pos`-th position in dictionary. Raises error
    !   if index is out of range. Order of items in dictionary is not the same as order
    !   they were put in. 
    ! #### USAGE
    !   ```Fortran
    !   call list%items(pos, key, value)
    !   ```
    ! #### PARAMETERS
    !   * `integer, intent(IN) :: pos`
    !      A number specifying at which position to get element.
    !   * `class(*), intent(OUT) :: key`
    !     Corresponding key from to the dictionary.
    !   * `class(*), intent(OUT) :: value`
    !     Corresponding value from to the dictionary.
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%put("one", 1)
    !   > call dict%items(1, key, value)
    !   > print *, key, value
    !   "one", 1
    !   ```  
    ! %%%
    implicit none
    class(dict_t) :: this
    integer, intent(IN) :: pos
    class(*), intent(OUT) :: key
    class(*), intent(OUT) :: value
    class(*), pointer :: k, v

    ! Grab item from the list
    call this%fetch(pos, k, v)
    call copy(k, key)
    call copy(v, value)

end subroutine dict_items


! Get n-th key from dictionary
subroutine dict_keys (this, pos, key)
    ! %%%
    ! ## `DICT_T%KEYS` - Get n-th key from dictionary
    ! #### DESCRIPTION
    !   Get the key from `pos`-th position in dictionary. Raises error if index is out
    !   of range. Order of items in dictionary is not the same as order they were put in.
    !   __NOTE:__ Variable `key` must be exactly same type as dictionary key.  
    ! #### USAGE
    !   ```Fortran
    !   call list%keys(pos, key)
    !   ```
    ! #### PARAMETERS
    !   * `integer, intent(IN) :: pos`
    !      A number specifying at which position to get element.
    !   * `class(*), intent(OUT) :: key`
    !     Corresponding key from to the dictionary.
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%put("one", 1)
    !   > call dict%keys(1, key)
    !   > print *, key
    !   "one"
    !   ```  
    ! %%%
    implicit none
    class(dict_t) :: this
    integer, intent(IN) :: pos
    class(*), intent(OUT) :: key
    class(*), pointer :: k, v

    ! Grab item from the list
    call this%fetch(pos, k, v)
    call copy(k, key, strict=.True.)

end subroutine dict_keys


! Get number of items stored in dictionary
function dict_len (this) result (length)
    ! %%%
    ! ## `DICT_T%LEN` - Count number of items in the dictionary
    ! #### DESCRIPTION
    !   Count number of items in the dictionary
    ! #### USAGE
    !   ```Fortran
    !   out = list%len()
    !   ```
    ! #### PARAMETERS
    !   * `integer :: out`
    !     Number of items in the dictionary.
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%put("one", 1)
    !   > print *, dict%len()
    !   1
    !   > call dict%put("two", 2)
    !   > print *, dict%len()
    !   2
    !   ```
    ! %%%
    implicit none
    class(dict_t) :: this
    integer :: length

    if (allocated(this%bucket)) then
        length = sum(this%bucket(:)%length)
    else
        length = 0
    end if 

end function dict_len


! Put value to dictionary
subroutine dict_put (this, key, value)
    ! %%%
    ! ## `DICT_T%PUT` - Add item to dictionary
    ! #### DESCRIPTION
    !   Add the value of the item with the specified key. If key is already exists,
    !   value will be replaced.
    ! #### USAGE
    !   ```Fortran
    !   call list%put(key, value)
    !   ```
    ! #### PARAMETERS
    !   * `class(*), intent(IN) :: key`
    !     The key of the item you want to store.
    !   * `class(*), intent(IN) :: value`
    !     The value of the item you want to store.
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%put("one", 1)
    !   > call dict%put("two", 2)
    !   > print *, dict
    !   {'one':1, 'two':2}
    !   > call dict%put("one", 0)
    !   > print *, dict
    !   {'one':0, 'two':2}
    !   ```  
    ! %%%
    implicit none
    class(dict_t) :: this
    class(*), intent(IN) :: key
    class(*), intent(IN) :: value
    integer :: i, hash

    ! Check if allocated
    if (.not. allocated(this%bucket)) then
        call this%resize(INIT_BUCKETS)
    end if

    ! Add entry to bucker
    hash = this%hash_function(key)
    i = modulo(hash, this%n_buckets)
    call this%bucket(i)%append(hash, key, value)

    ! Check if bucket is full and resize if exceeded
    if (this%bucket(i)%length > MAX_LENGTH) then
        call this%resize(2 * this%n_buckets)
    end if

end subroutine dict_put


! Remove value from dictionary
subroutine dict_remove (this, key)
    ! %%%
    ! ## `DICT_T%REMOVE` - Remove item from dictionary
    ! #### DESCRIPTION
    !   Removes the element with the specified key from dictionary
    ! #### USAGE
    !   ```Fortran
    !   call dict%remove(key)
    !   ```
    ! #### PARAMETERS
    !   * `class(*), intent(IN) :: key`
    !     The key of the item you want to remove.
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%put("one", 1)
    !   > call dict%put("two", 2)
    !   > call list%remove("one")
    !   > print *, dict
    !   {'two':2}
    !   ```
    ! %%%
    implicit none
    class(dict_t) :: this
    class(*), intent(IN) :: key
    integer(HDP) :: hash
    integer :: i

    if (allocated(this%bucket)) then
        hash = this%hash_function(key)
        i = modulo(hash, this%n_buckets)
        call this%bucket(i)%pop(hash)
    end if

end subroutine dict_remove


! Resize dictionary bucket
subroutine dict_resize (this, new_size)
    implicit none
    class(dict_t), intent(INOUT) :: this
    integer, intent(IN) :: new_size
    type(list_t), allocatable :: new_bucket(:)
    type(link_t), pointer :: curr
    integer :: i, j

    ! Sanity check
    if (new_size < 1) then
        error stop "Input Error: Invalid bucket size" 
    end if

    ! Allocate new buckets
    allocate (new_bucket(new_size))

    ! Resort old data if allocated
    if (allocated(this%bucket)) then
        do i = 1, this%n_buckets
            curr => this%bucket(i)%first
            do while (associated(curr))
                j = modulo(curr%hash, new_size)
                call new_bucket(j)%append(curr%hash, curr%key, curr%value)
            end do
            call this%bucket(i)%clear()
        end do
    end if
    
    ! Transfer allocation
    this%n_buckets = new_size
    call move_alloc (new_bucket, this%bucket) 

end subroutine dict_resize


! Check if key is same type as reference
function dict_same_type_as (this, key, elem) result (result)
    ! %%%
    ! ## `DICT_T%SAME_TYPE_AS` - Check if value is the same type as reference
    ! #### DESCRIPTION
    !   Returns `.True.` if the dynamic type of key value is the same as the dynamic 
    !   type of `elem`. Returns `.False.` if key value is empty.
    ! #### USAGE
    !   ```Fortran
    !   result = list%key_same_type_as(key, elem)
    !   ```
    ! #### PARAMETERS
    !   * `logical :: result`
    !      A number specifying at which position to get element.
    !   * `class(*), intent(IN) :: key`
    !     Corresponding key from to the dictionary.
    !   * `class(*), intent(IN) :: elem`
    !     Corresponding value from to the dictionary.
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%put("one", 1)
    !   > print *, dict%same_type_as("one", 1)
    !   .True.
    !   > print *, dict%same_type_as("one", 1.0)
    !   .False.
    !   ```  
    ! %%%
    implicit none
    logical :: result
    class(dict_t) :: this
    class(*), intent(IN) :: key
    class(*), intent(IN) :: elem
    class(*), pointer :: k, v
    integer(HDP) :: hash
    integer :: i

    if (allocated(this%bucket)) then
        hash = this%hash_function(key)
        i = modulo(hash, this%n_buckets)
        call this%bucket(i)%get(hash, k, v)
    end if

    if (associated(v)) then
        result = same_type_as(v, elem)
    else 
        result = .False.
    end if 
    
end function dict_same_type_as


! Get n-th value from dictionary
subroutine dict_values (this, pos, value)
    ! %%%
    ! ## `DICT_T%VALUES` - Get n-th value from dictionary
    ! #### DESCRIPTION
    !   Get value from the `pos`-th position in dictionary. Raises error if index is out
    !   of range. Order of items in dictionary is not the same as order they were put in.
    ! #### USAGE
    !   ```Fortran
    !   call list%values(pos, value)
    !   ```
    ! #### PARAMETERS
    !   * `integer, intent(IN) :: pos`
    !      A number specifying at which position to get element.
    !   * `class(*), intent(OUT) :: value`
    !     Corresponding value from to the dictionary.
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%put("one", 1)
    !   > call dict%values(1, value)
    !   > print *, value
    !   1
    !   ```  
    ! %%%
    implicit none
    class(dict_t) :: this
    integer, intent(IN) :: pos
    class(*), intent(OUT) :: value
    class(*), pointer :: k, v

    ! Grab item from the list
    call this%fetch(pos, k, v)
    call copy(v, value)

end subroutine dict_values


! Formatted write for dictionary
subroutine write_formatted (this, unit, iotype, v_list, iostat, iomsg)
    ! %%%
    ! ## `DICT_T%WRITE` - Formatted and unformatted write 
    ! #### DESCRIPTION
    !   Allows for formatted and unformatted write of `dict_t`.
    ! #### USAGE
    !   ```Fortran
    !   print *, dict_t
    !   write (*, *) dict_t
    !   ```
    ! #### EXAMPLE
    !   ```Fortran
    !   > call dict%put("one", 1)
    !   > call dict%put("two", 2)
    !   > print *, dict
    !   {'one':1, 'two':2}
    !   > write (*, *) dict
    !   {'one':1, 'two':2}
    !   ```
    ! %%% 
    implicit none
    class(dict_t), intent(IN) :: this
    integer, intent(IN) :: unit
    character(*), intent(IN) :: iotype 
    integer, intent(IN) :: v_list(:)
    integer, intent(OUT) :: iostat
    character(*), intent(INOUT) :: iomsg
    class(link_t), pointer :: curr 
    character(:), allocatable :: buffer
    character(256) :: key, value
    logical :: first
    integer :: i

    catch: block

        ! Get each key-value pair from dictionary and write them to a buffer 
        buffer = ""
        first = .True.
        if (allocated(this%bucket)) then
            do i = 1, this%n_buckets
                curr => this%bucket(i)%first
                do while (associated(curr))
                    call copy(curr%key, key, stat=iostat, errmsg=iomsg)
                    if (iostat /= 0) exit catch
                    call copy(curr%value, value, stat=iostat, errmsg=iomsg)
                    if (iostat /= 0) exit catch
                    ! Add quotations marks if character
                    select type (k => curr%key)
                    type is (character(*))
                        key = "'" // trim(key) // "'"
                    end select 
                    select type (v => curr%value)
                    type is (character(*))
                        value = "'" // trim(value) // "'"
                    end select 
                    ! Do not add delimiter to string if first element
                    if (first) then
                        buffer = buffer // trim(key) // ":" // trim(value)
                        first = .False.
                    else
                        buffer = buffer // "," // " " // trim(key) // ":" // trim(value) 
                    end if
                    ! Next element
                    curr => curr%next
                end do
            end do
        end if

        ! Add formatting to buffer
        buffer = "{" // buffer // "}"
        
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

end module xslib_dict
