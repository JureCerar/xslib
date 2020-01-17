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

module xslib_list
  implicit none
  private
  public :: list_t, assignment(=)

  ! Good linked list example:
  ! https://github.com/LadaF/fortran-list/blob/master/src/lists.f90

  type link_t
    class(*), pointer   :: value => null()
    class(link_t), pointer :: next => null()
    class(link_t), pointer :: prev => null()
  end type link_t

  ! ------------------------------------------

  type list_t
    private
    class(link_t), pointer  :: first => null()
    class(link_t), pointer  :: last => null()
    class(link_t), pointer  :: iterator => null()
  contains
    ! Check list
    procedure :: isEmpty => list_isEmpty
    ! Add/remove elements on list
    procedure :: add => list_add
    procedure :: insert => list_Insert
    procedure :: pop => list_pop
    procedure :: remove => list_remove
    procedure :: clear => list_clear
    ! Read/write overide
    procedure :: write => list_write
    generic   :: write(formatted) => write
    procedure :: read => list_read
    generic   :: read(formatted) => read
    ! Assigment
    ! procedure :: assign => list_assignment
    ! generic   :: assignment(=) => assign
    ! Iterator control pointer control
    procedure :: next => list_next
    procedure :: prev => list_prev
    procedure :: start => list_first
    procedure :: end => list_last
    ! Execute command on linked list
    procedure :: execute => list_execute
    procedure :: printAll => list_printAll
  end type list_t

  ! Get value directly from linked list
  interface assignment(=)
    module procedure :: list_assignment
  end interface

contains

! Custom constuctor function
function constructor( value )
  implicit none
  class(link_t), pointer  :: constructor
  class(*)                :: value

  ! Allocate data
  allocate( constructor )
  allocate( constructor%value, SOURCE=value )

  return
end function constructor

function destructor( this )
  implicit none
  class(link_t), pointer  :: this
  class(link_t), pointer  :: destructor

  if ( associated(this) ) then
    deallocate( this%value )
    deallocate( this )
  end if

  ! Return null pointer
  destructor => null()

  return
end function destructor

! ------------------------------------------

! Is this list empty?
logical function list_isEmpty( this )
  implicit none
  class(list_t) :: this
  list_isEmpty = .not. associated(this%first)
  return
end function list_isEmpty

! Add value to the list
subroutine list_add( this, value )
  implicit none
  class(list_t)           :: this
  class(*)                :: value
  class(link_t), pointer  :: newLink

  if ( this%isEmpty() ) then
    ! Create first link and update last
    this%first => constructor( value )
    this%last => this%first
    ! point iterator to first
    this%iterator => this%first

  else
    ! Create new link
    newLink => constructor( value )
    ! Update connections
    newLink%prev => this%last
    this%last%next => newLink
    ! Update last link
    this%last => newLink

  end if

  return
end subroutine list_add

subroutine list_insert( this, value )
  implicit none
  class(list_t)           :: this
  class(*), intent(in)    :: value
  class(link_t), pointer  :: newLink

  if ( this%isEmpty() ) then
    ! Create first link and update last
    this%first => constructor( value )
    this%last => this%first
    this%iterator => this%first

  else
    newLink => constructor( value )
    newLink%next => this%iterator
    ! Update previous node (if exists)
    if ( associated(this%iterator%prev) ) then
      this%iterator%prev%next => newLink
      newLink%prev => this%iterator%prev
    end if
    this%iterator%prev => newLink

    ! Update first pointer if necessery
    if ( associated(this%first,this%iterator) ) this%first => newLink

  end if

  return
end subroutine list_insert

subroutine list_pop( this )
  implicit none
  class(list_t) :: this

  if ( this%isEmpty() ) then
    ! Nothing to do
    return

  else if ( associated(this%first,this%last) ) then
    ! If only one link left
    this%first => destructor( this%first )
    this%last => null()
    this%iterator => null()

  else
    ! Move iterator
    if ( associated(this%iterator,this%last) ) this%iterator => this%last%prev
    ! General case
    this%last => this%last%prev
    this%last%next => destructor( this%last%next )

  end if

  return
end subroutine list_pop

subroutine list_remove( this )
  implicit none
  class(list_t)           :: this
  class(link_t), pointer  :: temp

  if ( .not. associated(this%iterator) ) then
    ! Empty pointer
    return

  else if ( associated(this%last,this%first) ) then
    ! Is there only one node
    this%first => destructor( this%first )
    this%last => null()

  else if ( associated(this%iterator,this%first) ) then
    ! This is first node
    this%iterator => this%iterator%next
    this%iterator%prev => destructor(this%iterator%prev)
    ! Update first
    this%first => this%iterator

  else if ( associated(this%iterator,this%last) ) then
    ! This is last node
    this%iterator => this%iterator%prev
    this%iterator%next => destructor(this%iterator%next)
    ! Update last
    this%last => this%iterator

  else
    ! General case
    this%iterator%prev%next => this%iterator%next
    this%iterator%next%prev => this%iterator%prev
    ! Update iterator
    temp => this%iterator
    ! this%iterator => this%iterator%next ! Move forward
    this%iterator => this%iterator%prev ! Move backward
    ! Destroy
    temp => destructor( temp )

  end if

  return
end subroutine list_remove

subroutine list_clear( this )
  implicit none
  class(list_t)           :: this
  class(link_t), pointer  :: link

  ! One way to do it.
  do while ( associated(this%first) )
    call this%pop()
  end do

  return
end subroutine list_clear

! This is more or less for debugging purposes
subroutine list_printAll( this )
  implicit none
  class(list_t)           :: this
  class(link_t), pointer  :: link

  if ( .not. associated(this%first) ) then
    write (*,*) "*Empty list*"
    return
  end if

  link => this%first
  do while ( associated(link) )
    select type ( val => link%value )
    type is ( integer )
      write (*,"(i0,$)") val
    type is ( real )
      write (*,"(f8.4,$)") val
    type is ( character(*) )
      write (*,"(a,$)") trim(val)
    class default
      stop "fatal"
    end select

    ! Next link in chain
    link => link%next

    ! Field separator
    if ( associated(link) ) then
      write(*,"(',',$)")
    else
      write (*,*)
    end if

  end do

  return
end subroutine list_printAll

subroutine list_assignment( this, other )
  use, intrinsic :: iso_fortran_env
  implicit none
  class(*), intent(out)     :: this
  class(list_t), intent(in) :: other

  if ( .not. associated(other%iterator) ) return

  select type( this )
  type is ( integer )
    select type( val => other%iterator%value )
    type is ( integer )
      this = val
    class default
      stop "fatal"
    end select
  type is ( integer(INT64) )
    select type( val => other%iterator%value )
    type is ( integer(INT64) )
      this = val
    class default
      stop "fatal"
    end select
  type is ( real )
    select type( val => other%iterator%value )
    type is ( real )
      this = val
    class default
      stop "fatal"
    end select
  type is ( real(REAL64) )
    select type( val => other%iterator%value )
    type is ( real(REAL64) )
      this = val
    class default
      stop "fatal"
    end select
  type is ( character(*) )
    select type( val => other%iterator%value )
    type is ( character(*) )
      this = val
    class default
      stop "fatal"
    end select
  class default
    stop "fatal"
  end select

  return
end subroutine list_assignment

! ------------------------------------------
! https://software.intel.com/en-us/fortran-compiler-developer-guide-and-reference-examples-of-user-defined-derived-type-i-o

subroutine list_write( this, unit, iotype, v_list, iostat, iomsg )
  use, intrinsic :: iso_fortran_env
  implicit none
  class(list_t), intent(in)   :: this
  integer, intent(in)         :: unit
  character(*), intent(in)    :: iotype
  integer, intent(in)         :: v_list(:)    ! parameters from fmt spec.
  integer, intent(out)        :: iostat      ! non zero on error, etc.
  character(*), intent(inout) :: iomsg  ! define if iostat non zero.

  if ( associated(this%iterator) ) then
    select type ( val => this%iterator%value )
    type is ( integer )
      write (unit,*,IOSTAT=iostat,IOMSG=iomsg) val
    type is ( integer(INT64) )
      write (unit,*,IOSTAT=iostat,IOMSG=iomsg) val
    type is ( real )
      write (unit,*,IOSTAT=iostat,IOMSG=iomsg) val
    type is ( REAL(REAL64) )
      write (unit,*,IOSTAT=iostat,IOMSG=iomsg) val
    type is ( character(*) )
      write (unit,*,IOSTAT=iostat,IOMSG=iomsg) val
    class default
      stop "fatal"
    end select
  else
    write (unit,*,IOSTAT=iostat,IOMSG=iomsg) ""
  end if

  return
end subroutine list_write

subroutine list_read( this, unit, iotype, v_list, iostat, iomsg )
  implicit none
  class(list_t), intent(inout)  :: this
  integer, intent(in)           :: unit
	character(*), intent(in)     :: iotype
  integer, intent(in)           :: v_list(:)
  integer, intent(out)          :: iostat      ! non zero on error, etc.
  character(*), intent(inout)   :: iomsg  ! define if iostat non zero.
  character(512)                :: buffer

  read (unit,*,IOSTAT=iostat,IOMSG=iomsg) buffer
  call this%add( trim(buffer) )

  return
end subroutine list_read

! ------------------------------------------

! List position control -- next
subroutine list_next( this )
  implicit none
  class(list_t) :: this
  if ( associated(this%iterator%next) ) this%iterator => this%iterator%next
  return
end subroutine list_next

! List position control -- prev
subroutine list_prev( this )
  implicit none
  class(list_t) :: this
  if ( associated(this%iterator%prev) ) this%iterator => this%iterator%prev
  return
end subroutine list_prev

! List position control -- first
subroutine list_first( this )
  implicit none
  class(list_t) :: this
  if ( associated(this%first) ) this%iterator => this%first
  return
end subroutine list_first

! List position control -- last
subroutine list_last( this )
  implicit none
  class(list_t) :: this
  if ( associated(this%last) ) this%iterator => this%last
  return
end subroutine list_last

! ------------------------------------------
! https://github.com/LadaF/fortran-list/blob/master/src/lists.f90

subroutine list_execute( this, routine )
  implicit none
  class(list_t)           :: this
  class(link_t), pointer  :: link
  interface
    subroutine routine( value )
      class(*), intent(inout) :: value
    end subroutine routine
  end interface

  link => this%first
  do while ( associated(link) )
    call routine( link%value )
    link => link%next

  end do

  return
end subroutine list_execute

end module xslib_list
