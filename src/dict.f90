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

module xslib_dict
    !! Module with primitive implementation of unlimited polymorphic dictionary (hash table).
    !!
    !! @note
    !! To add new custom derived TYPE support you only have to write extension to `hash_function` and `copy` functions.
    !! @endnote
    use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
    implicit none
    private
    public :: dict_t

    integer, parameter :: HASH_KIND = INT32
    !! Hash value default precision

    type :: link_t
        !! Element of linked list
        integer(HASH_KIND) :: hash
        class(*), pointer :: key => null()
        class(*), pointer :: value => null()
        type(link_t), pointer :: next => null()
    end type link_t

    type :: list_t
        !! Linked list
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
    ! TODO: Optimize this values.
    integer, parameter :: INIT_BUCKETS = 128
    integer, parameter :: MAX_LENGTH = 32

    type :: dict_t
        !! Implementation of unlimited polymorphic dictionary (hash table).
        !!
        !! A hash table stores data in key-value pairs. It operates on the hashing
        !! concept, where each key is translated by a hash function into a distinct
        !! index in an array. The index functions as a storage location for the matching
        !! value. In simple words, it maps the keys with the value.
        !!
        !! Example:
        !! ```Fortran
        !! type(dict_t) :: dict
        !! ```
        integer, private :: n_buckets = 0 
        type(list_t), allocatable, private :: bucket(:)  
    contains
        procedure :: clear => dict_clear
        !! Remove all key-value pairs from dictionary.
        !! 
        !! Example:
        !! ```Fortran
        !! call dict%clear()
        !! print *, dict
        !! >>> {}
        !! ```
        procedure, private :: fetch => dict_fetch
        procedure :: get => dict_get
        !! Get the value of the item with the specified key. Returns default value
        !! (if present) if item is not in the dictionary, otherwise raises an error.
        !! 
        !! Example:
        !! ```Fortran
        !! print *, dict
        !! >>> {'foo': 1, 'bar': 2}
        !! call dict%get("foo", value)
        !! print *, value
        !! >>> 1
        !! call dict%get("bar", value, default=0)
        !! print *, value
        !! >>> 0
        !! ```
        procedure, non_overridable, nopass :: hash_function
        procedure :: items => dict_items
        !! Get the key-value item pair from `n`-th position in dictionary. Raises error
        !! if index is out of range. Order of items in dictionary is not the same as order
        !! they were put in. 
        !! 
        !! Example:
        !! ```Fortran
        !! print *, dict
        !! >>> {'foo': 1, 'bar': 2}
        !! call dict%items(1, key, value)
        !! print *, key, value
        !! >>> "foo", 1
        !! ```
        procedure :: keys => dict_keys
        !! Get the key from `n`-th position in dictionary. Raises error if index is out
        !! of range. Order of keys in dictionary is not the same as order they were put in.
        !!
        !! @note
        !! Variable `key` must be exactly same type as dictionary key.
        !! @endnote
        !! 
        !! Example:
        !! ```Fortran
        !! print *, dict
        !! >>> {'foo': 1, 'bar': 2}
        !! call dict%keys(1, key)
        !! print *, key
        !! >>> "foo"
        !! ```
        procedure :: len => dict_len
        !! Count number of key-value pairs in the dictionary.
        !! 
        !! Example:
        !! ```Fortran
        !! print *, dict
        !! >>> {'foo': 1, 'bar': 2}
        !! print *, dict%len()
        !! >>> 2
        !! ```
        procedure :: put => dict_put
        !! Add new key-value pair to dictionary.
        !! 
        !! Example:
        !! ```Fortran
        !! call dict%put("foo", 1)
        !! call dict%put("bar", 2)
        !! print *, dict
        !! >>> {'foo': 1, 'bar': 2}
        !! ```
        procedure :: remove => dict_remove
        !! Remove value with the specified key from dictionary.
        !! 
        !! Example:
        !! ```Fortran
        !! print *, dict
        !! >>> {'foo': 1, 'bar': 2}
        !! call dict%remove("foo")
        !! print *, dict
        !! >>> {'bar': 2}
        !! ```
        procedure, private :: resize => dict_resize
        procedure :: same_type_as => dict_same_type_as
        !! Returns `.True.` if the dynamic type of key value is the same as the dynamic 
        !! type of `elem`. Returns `.False.` if key value is empty.
        !! 
        !! Example:
        !! ```Fortran
        !! print *, dict
        !! >>> {'foo': 1, 'bar': 2}
        !! print *, dict%same_type_as("foo", 1)
        !! >>> .True.
        !! print *, dict%same_type_as("foo", 1.0)
        !! >>> .False.
        !! ```
        procedure :: values => dict_values
        !! Get value from the `n`-th position in dictionary. Raises error if index is out
        !! of range. Order of items in dictionary is not the same as order they were put in.
        !! 
        !! Example:
        !! ```Fortran
        !! print *, dict
        !! >>> {'foo': 1, 'bar': 2}
        !! call dict%values(1, value)
        !! print *, value
        !! >>> 1
        !! ```
        procedure, private :: write_formatted
        generic :: write(formatted) => write_formatted
        !! Formatted and unformatted write of `dict_t`.
        !!
        !! Example:
        !! ```Fortran
        !! print *, dict
        !! >>> {'foo': 1, 'bar': 2}
        !! write (*,*) dict
        !! >>> {'foo': 1, 'bar': 2}
        !! ```
    end type dict_t

contains

function hash_function (key) result (hash)
    !! Return DJB2 hash for a given key
    implicit none
    class(*), intent(in) :: key
    integer(HASH_KIND) :: hash
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


function link_constructor (hash, key, value) result (link)
    !! Create a new link w/ key-value pair
    implicit none
    class(link_t), pointer :: link
    integer(HASH_KIND), intent(IN) :: hash
    class(*), intent(IN) :: key, value

    allocate (link)
    link%hash = hash
    allocate (link%key, SOURCE=key)
    allocate (link%value, SOURCE=value)

end function link_constructor


function link_destructor (link) result (next)
    !! Destroy link and return link's next pointer
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


subroutine list_append (this, hash, key, value)
    !! Append or replace key-value pair to the list
    implicit none
    class(list_t) :: this
    integer(HASH_KIND) :: hash
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


subroutine list_clear (this)
    !! Removes all key-value pairs from the list
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


subroutine list_pop (this, hash)
    !! Removes key-value pair with specified hash value from the list
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


subroutine list_get (this, hash, key, value)
    !! Get key-value with pair from the list
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


subroutine dict_clear (this)
    !! Remove all items from dictionary
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


subroutine dict_fetch (this, pos, key, value)
    !! Get key-value pair from the n-th position in dictionary
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


subroutine dict_get (this, key, value, default)
    !! Get value from dictionary
    implicit none
    class(dict_t) :: this
    class(*), intent(IN) :: key
    !! The key of the item you want to return the value from.
    class(*), intent(OUT) :: value
    !! A value of specified key.
    class(*), intent(IN), optional :: default
    !! A value to return if the specified key does not exist. Default: None
    class(*), pointer :: k, v
    integer(HASH_KIND) :: hash
    integer :: i

    if (allocated(this%bucket)) then
        hash = this%hash_function(key)
        i = modulo(hash, this%n_buckets) + 1
        call this%bucket(i)%get(hash, k, v)
    else
        v => null()
    end if

    if (associated(v)) then
        call copy(v, value)
    else if (present(default)) then
        call copy(default, value)
    else 
        error stop "Key Error: Not in dictionary"
    end if 

end subroutine dict_get


subroutine dict_items (this, n, key, value)
    !! Get n-th item from dictionary
    implicit none
    class(dict_t) :: this
    integer, intent(IN) :: n
    !! A number specifying at which position to get value.
    class(*), intent(OUT) :: key
    !! Corresponding key from to the dictionary.
    class(*), intent(OUT) :: value
    !! Corresponding value from to the dictionary.
    class(*), pointer :: k, v

    ! Grab item from the list
    call this%fetch(n, k, v)
    call copy(k, key)
    call copy(v, value)

end subroutine dict_items


subroutine dict_keys (this, n, key)
    !! Get n-th key from dictionary
    implicit none
    class(dict_t) :: this
    integer, intent(IN) :: n
    !! A number specifying at which position to get key.
    class(*), intent(OUT) :: key
    !! Corresponding key from to the dictionary.
    class(*), pointer :: k, v

    ! Grab item from the list
    call this%fetch(n, k, v)
    call copy(k, key, strict=.True.)

end subroutine dict_keys


function dict_len (this) result (length)
    !! Count number of items in the dictionary
    implicit none
    class(dict_t) :: this
    integer :: length
    !! Number of items in the dictionary.

    if (allocated(this%bucket)) then
        length = sum(this%bucket(:)%length)
    else
        length = 0
    end if 

end function dict_len


subroutine dict_put (this, key, value)
    !! Add new key-value pair to dictionary
    implicit none
    class(dict_t) :: this
    class(*), intent(IN) :: key
    !! The key of the key-value pair you want to store.
    class(*), intent(IN) :: value
    !! The value of the key-value pair you want to store.
    integer :: i, hash

    ! Check if allocated
    if (.not. allocated(this%bucket)) then
        call this%resize(INIT_BUCKETS)
    end if

    ! Add entry to bucker
    hash = this%hash_function(key)
    i = modulo(hash, this%n_buckets) + 1
    call this%bucket(i)%append(hash, key, value)

    ! Check if bucket is full and resize if exceeded
    if (this%bucket(i)%length > MAX_LENGTH) then
        ! print *, "resizing to:", 2 * this%n_buckets
        call this%resize(2 * this%n_buckets)
    end if

end subroutine dict_put


subroutine dict_remove (this, key)
    !! Remove key-value pair from dictionary
    implicit none
    class(dict_t) :: this
    class(*), intent(IN) :: key
    !! The key of the key-value pair you want to remove.
    integer(HASH_KIND) :: hash
    integer :: i

    if (allocated(this%bucket)) then
        hash = this%hash_function(key)
        i = modulo(hash, this%n_buckets) + 1
        call this%bucket(i)%pop(hash)
    end if

end subroutine dict_remove


subroutine dict_resize (this, new_size)
    !! Resize dictionary bucket
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
                j = modulo(curr%hash, new_size) + 1
                call new_bucket(j)%append(curr%hash, curr%key, curr%value)
                ! curr => link_destructor(curr)
                curr => curr%next
            end do
            call this%bucket(i)%clear()
        end do
    end if
    
    ! Transfer allocation
    this%n_buckets = new_size
    call move_alloc (new_bucket, this%bucket) 

end subroutine dict_resize


function dict_same_type_as (this, key, elem) result (result)
    !! Check if value is the same type as reference
    implicit none
    logical :: result
    !! Check if TYPE of elem and value match.
    class(dict_t) :: this
    class(*), intent(IN) :: key
    !! Corresponding key from to the dictionary.
    class(*), intent(IN) :: elem
    !! Element to be checked against value from to the dictionary.
    class(*), pointer :: k, v
    integer(HASH_KIND) :: hash
    integer :: i

    if (allocated(this%bucket)) then
        hash = this%hash_function(key)
        i = modulo(hash, this%n_buckets) + 1
        call this%bucket(i)%get(hash, k, v)
    end if

    if (associated(v)) then
        result = same_type_as(v, elem)
    else 
        result = .False.
    end if 
    
end function dict_same_type_as


subroutine dict_values (this, n, value)
    !! Get n-th value from dictionary
    implicit none
    class(dict_t) :: this
    integer, intent(IN) :: n
    !! A number specifying at which position to get key-value pair.
    class(*), intent(OUT) :: value
    !! Corresponding value from to the dictionary.
    class(*), pointer :: k, v

    ! Grab item from the list
    call this%fetch(n, k, v)
    call copy(v, value)

end subroutine dict_values


subroutine write_formatted (this, unit, iotype, v_list, iostat, iomsg)
    !! Formatted and unformatted write for dictionary
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
