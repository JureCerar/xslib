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

module xslib_xmalloc
  implicit none
  private
  public :: xmalloc, xcalloc, xrealloc

  ! Fortran wrapper for allocate with "integrated" error handling.
  ! NOTE: Only supports arrays of rank up to 2 AND variables of type int, int64, real, real64, and character(*).

  ! xmalloc - Allocates a block of memory for an array of num. elements.
  ! * The content of the newly allocated block of memory is not initialized, remaining with indeterminate values.

  ! xcalloc - Allocates a block of memory for an array of num. elements,
  ! * each of them size bytes long, and initializes all its bits to zero.

  ! xrealloc - Changes the size of memory block. The content of the memory block is preserved up to the
  ! * lesser of the new and old sizes, even if the block is moved to a new location. If the new size is larger,
  ! * the value of the newly allocated portion is indeterminate.
  ! * If the new size you specify is the same as the old size, xrealloc is guaranteed
  ! * to change nothing and return the same address that you gave.

  interface xmalloc
    procedure :: xmalloc_int, xmalloc_long, xmalloc_float, xmalloc_double, xmalloc_char
    procedure :: xmalloc_int_2d, xmalloc_long_2d, xmalloc_float_2d, xmalloc_double_2d, xmalloc_char_2d
  end interface xmalloc

  interface xcalloc
    procedure :: xcalloc_int, xcalloc_long, xcalloc_float, xcalloc_double, xcalloc_char
    procedure :: xcalloc_int_2d, xcalloc_long_2d, xcalloc_float_2d, xcalloc_double_2d, xcalloc_char_2d
  end interface xcalloc

  interface xrealloc
    procedure :: xrealloc_int, xrealloc_long, xrealloc_float, xrealloc_double, xrealloc_char
    procedure :: xrealloc_int_2d, xrealloc_long_2d, xrealloc_float_2d, xrealloc_double_2d, xrealloc_char_2d
  end interface xrealloc

contains

integer function xmalloc_int( object, spec, errmsg )
  implicit none
  integer, allocatable, intent(inout) :: object(:)
  integer, intent(in)                 :: spec(:)
  character(*), intent(out), optional :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_int = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_int )
    allocate( object(spec(1)), STAT=xmalloc_int, ERRMSG=errmsg )
  end if

  return
end function xmalloc_int

integer function xmalloc_int_2d( object, spec, errmsg )
  implicit none
  integer, allocatable, intent(inout) :: object(:,:)
  integer, intent(in)                 :: spec(:)
  character(*), intent(out), optional :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_int_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_int_2d )
    allocate( object(spec(1),spec(2)), STAT=xmalloc_int_2d, ERRMSG=errmsg )
  end if

  return
end function xmalloc_int_2d

integer function xmalloc_long( object, spec, errmsg )
  use iso_fortran_env, only: INT64
  implicit none
  integer(INT64), allocatable, intent(inout)  :: object(:)
  integer, intent(in)                         :: spec(:)
  character(*), intent(out), optional         :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_long = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_long )
    allocate( object(spec(1)), STAT=xmalloc_long, ERRMSG=errmsg )
  end if

  return
end function xmalloc_long

integer function xmalloc_long_2d( object, spec, errmsg )
  use iso_fortran_env, only: INT64
  implicit none
  integer(INT64), allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                         :: spec(:)
  character(*), intent(out), optional         :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_long_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_long_2d )
    allocate( object(spec(1),spec(2)), STAT=xmalloc_long_2d, ERRMSG=errmsg )
  end if

  return
end function xmalloc_long_2d

integer function xmalloc_float( object, spec, errmsg )
  implicit none
  real, allocatable, intent(inout)    :: object(:)
  integer, intent(in)                 :: spec(:)
  character(*), intent(out), optional :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_float = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_float )
    allocate( object(spec(1)), STAT=xmalloc_float, ERRMSG=errmsg )
  end if

  return
end function xmalloc_float

integer function xmalloc_float_2d( object, spec, errmsg )
  implicit none
  real, allocatable, intent(inout)    :: object(:,:)
  integer, intent(in)                 :: spec(:)
  character(*), intent(out), optional :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_float_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_float_2d )
    allocate( object(spec(1),spec(2)), STAT=xmalloc_float_2d, ERRMSG=errmsg )
  end if

  return
end function xmalloc_float_2d

integer function xmalloc_double( object, spec, errmsg )
  use iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), allocatable, intent(inout)  :: object(:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_double = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_double )
    allocate( object(spec(1)), STAT=xmalloc_double, ERRMSG=errmsg )
  end if

  return
end function xmalloc_double

integer function xmalloc_double_2d( object, spec, errmsg )
  use iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_double_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_double_2d )
    allocate( object(spec(1),spec(2)), STAT=xmalloc_double_2d, ERRMSG=errmsg )
  end if

  return
end function xmalloc_double_2d

integer function xmalloc_char( object, spec, errmsg )
  character(*), allocatable, intent(inout)  :: object(:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_char = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_char )
    allocate( object(spec(1)), STAT=xmalloc_char, ERRMSG=errmsg )
  end if

  return
end function xmalloc_char

integer function xmalloc_char_2d( object, spec, errmsg )
  character(*), allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg

  if ( rank(object) /= size(spec) ) then
    xmalloc_char_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xmalloc_char_2d )
    allocate( object(spec(1),spec(2)), STAT=xmalloc_char_2d, ERRMSG=errmsg )
  end if

  return
end function xmalloc_char_2d

! --------------------------------

integer function xcalloc_int( object, spec, errmsg )
  implicit none
  integer, allocatable, intent(inout) :: object(:)
  integer, intent(in)                 :: spec(:)
  character(*), intent(out), optional :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_int = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_int )
    allocate( object(spec(1)), SOURCE=0, STAT=xcalloc_int, ERRMSG=errmsg )
  end if

  return
end function xcalloc_int

integer function xcalloc_int_2d( object, spec, errmsg )
  implicit none
  integer, allocatable, intent(inout) :: object(:,:)
  integer, intent(in)                 :: spec(:)
  character(*), intent(out), optional :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_int_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_int_2d )
    allocate( object(spec(1),spec(2)), SOURCE=0, STAT=xcalloc_int_2d, ERRMSG=errmsg )
  end if

  return
end function xcalloc_int_2d

integer function xcalloc_long( object, spec, errmsg )
  use iso_fortran_env, only: INT64
  implicit none
  integer(INT64), allocatable, intent(inout)  :: object(:)
  integer, intent(in)                         :: spec(:)
  character(*), intent(out), optional         :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_long = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_long )
    allocate( object(spec(1)), SOURCE=0_INT64, STAT=xcalloc_long, ERRMSG=errmsg )
  end if

  return
end function xcalloc_long

integer function xcalloc_long_2d( object, spec, errmsg )
  use iso_fortran_env, only: INT64
  implicit none
  integer(INT64), allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                         :: spec(:)
  character(*), intent(out), optional         :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_long_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_long_2d )
    allocate( object(spec(1),spec(2)), SOURCE=0_INT64, STAT=xcalloc_long_2d, ERRMSG=errmsg )
  end if

  return
end function xcalloc_long_2d

integer function xcalloc_float( object, spec, errmsg )
  implicit none
  real, allocatable, intent(inout)    :: object(:)
  integer, intent(in)                 :: spec(:)
  character(*), intent(out), optional :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_float = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_float )
    allocate( object(spec(1)), SOURCE=0.0, STAT=xcalloc_float, ERRMSG=errmsg )
  end if

  return
end function xcalloc_float

integer function xcalloc_float_2d( object, spec, errmsg )
  implicit none
  real, allocatable, intent(inout)    :: object(:,:)
  integer, intent(in)                 :: spec(:)
  character(*), intent(out), optional :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_float_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_float_2d )
    allocate( object(spec(1),spec(2)), SOURCE=0.0, STAT=xcalloc_float_2d, ERRMSG=errmsg )
  end if

  return
end function xcalloc_float_2d

integer function xcalloc_double( object, spec, errmsg )
  use iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), allocatable, intent(inout)  :: object(:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_double = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_double )
    allocate( object(spec(1)), SOURCE=0.0d0, STAT=xcalloc_double, ERRMSG=errmsg )
  end if

  return
end function xcalloc_double

integer function xcalloc_double_2d( object, spec, errmsg )
  use iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_double_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_double_2d )
    allocate( object(spec(1),spec(2)), SOURCE=0.0d0, STAT=xcalloc_double_2d, ERRMSG=errmsg )
  end if

  return
end function xcalloc_double_2d

integer function xcalloc_char( object, spec, errmsg )
  character(*), allocatable, intent(inout)  :: object(:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_char = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_char )
    allocate( object(spec(1)), STAT=xcalloc_char, ERRMSG=errmsg )
    object(:) = ""
  end if

  return
end function xcalloc_char

integer function xcalloc_char_2d( object, spec, errmsg )
  character(*), allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg

  if ( rank(object) /= size(spec) ) then
    xcalloc_char_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  else
    if ( allocated(object) ) deallocate( object, STAT=xcalloc_char_2d )
    allocate( object(spec(1),spec(2)), STAT=xcalloc_char_2d, ERRMSG=errmsg )
    object(:,:) = ""
  end if

  return
end function xcalloc_char_2d

! --------------------------------

integer function xrealloc_int( object, spec, errmsg )
  implicit none
  integer, allocatable, intent(inout)  :: object(:)
  integer, intent(in)                  :: spec(:)
  character(*), intent(out), optional  :: errmsg
  integer, allocatable                 :: temp(:)
  integer                              :: nx

  if ( rank(object) /= size(spec) ) then
    xrealloc_int = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1)), STAT=xrealloc_int, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      allocate( temp(spec(1)), STAT=xrealloc_int, ERRMSG=errmsg )
      if ( xrealloc_int /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      temp(1:nx) = object(1:nx)
      call move_alloc(temp,object)

    else
      xrealloc_int = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if


  return
end function xrealloc_int

integer function xrealloc_int_2d( object, spec, errmsg )
  implicit none
  integer, allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                  :: spec(:)
  character(*), intent(out), optional  :: errmsg
  integer, allocatable                 :: temp(:,:)
  integer                              :: nx, ny

  if ( rank(object) /= size(spec) ) then
    xrealloc_int_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1),spec(2)), STAT=xrealloc_int_2d, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      allocate( temp(spec(1),spec(2)), STAT=xrealloc_int_2d, ERRMSG=errmsg )
      if ( xrealloc_int_2d /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      ny = min( spec(2), size(object,DIM=2) )
      temp(1:nx,1:ny) = object(1:nx,1:ny)
      call move_alloc(temp,object)

    else
      xrealloc_int_2d = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if

  return
end function xrealloc_int_2d

integer function xrealloc_long( object, spec, errmsg )
  use, intrinsic :: iso_fortran_env, only: INT64
  implicit none
  integer(INT64), allocatable, intent(inout)  :: object(:)
  integer, intent(in)                         :: spec(:)
  character(*), intent(out), optional         :: errmsg
  integer(INT64), allocatable                 :: temp(:)
  integer                                     :: nx

  if ( rank(object) /= size(spec) ) then
    xrealloc_long = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1)), STAT=xrealloc_long, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      allocate( temp(spec(1)), STAT=xrealloc_long, ERRMSG=errmsg )
      if ( xrealloc_long /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      temp(1:nx) = object(1:nx)
      call move_alloc(temp,object)

    else
      xrealloc_long = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if

  return
end function xrealloc_long

integer function xrealloc_long_2d( object, spec, errmsg )
  use, intrinsic :: iso_fortran_env, only: INT64
  implicit none
  integer(INT64), allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                         :: spec(:)
  character(*), intent(out), optional         :: errmsg
  integer(INT64), allocatable                 :: temp(:,:)
  integer                                     :: nx, ny

  if ( rank(object) /= size(spec) ) then
    xrealloc_long_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."

  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1),spec(2)), STAT=xrealloc_long_2d, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      allocate( temp(spec(1),spec(2)), STAT=xrealloc_long_2d, ERRMSG=errmsg )
      if ( xrealloc_long_2d /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      ny = min( spec(2), size(object,DIM=2) )
      temp(1:nx,1:ny) = object(1:nx,1:ny)
      call move_alloc(temp,object)

    else
      xrealloc_long_2d = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if

  return
end function xrealloc_long_2d

integer function xrealloc_float( object, spec, errmsg )
  implicit none
  real, allocatable, intent(inout)     :: object(:)
  integer, intent(in)                  :: spec(:)
  character(*), intent(out), optional  :: errmsg
  real, allocatable                    :: temp(:)
  integer                              :: nx

  if ( rank(object) /= size(spec) ) then
    xrealloc_float = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1)), STAT=xrealloc_float, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      allocate( temp(spec(1)), STAT=xrealloc_float, ERRMSG=errmsg )
      if ( xrealloc_float /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      temp(1:nx) = object(1:nx)
      call move_alloc(temp,object)

    else
      xrealloc_float = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if

  return
end function xrealloc_float

integer function xrealloc_float_2d( object, spec, errmsg )
  implicit none
  real, allocatable, intent(inout)     :: object(:,:)
  integer, intent(in)                  :: spec(:)
  character(*), intent(out), optional  :: errmsg
  real, allocatable                    :: temp(:,:)
  integer                              :: nx, ny

  if ( rank(object) /= size(spec) ) then
    xrealloc_float_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1),spec(2)), STAT=xrealloc_float_2d, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      allocate( temp(spec(1),spec(2)), STAT=xrealloc_float_2d, ERRMSG=errmsg )
      if ( xrealloc_float_2d /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      ny = min( spec(2), size(object,DIM=2) )
      temp(1:nx,1:ny) = object(1:nx,1:ny)
      call move_alloc(temp,object)

    else
      xrealloc_float_2d = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if

  return
end function xrealloc_float_2d

integer function xrealloc_double( object, spec, errmsg )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), allocatable, intent(inout)  :: object(:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg
  real(REAL64), allocatable                 :: temp(:)
  integer                                   :: nx

  if ( rank(object) /= size(spec) ) then
    xrealloc_double = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1)), STAT=xrealloc_double, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      allocate( temp(spec(1)), STAT=xrealloc_double, ERRMSG=errmsg )
      if ( xrealloc_double /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      temp(1:nx) = object(1:nx)
      call move_alloc(temp,object)

    else
      xrealloc_double = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if

  return
end function xrealloc_double

integer function xrealloc_double_2d( object, spec, errmsg )
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  real(REAL64), allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg
  real(REAL64), allocatable                 :: temp(:,:)
  integer                                   :: nx, ny

  if ( rank(object) /= size(spec) ) then
    xrealloc_double_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1),spec(2)), STAT=xrealloc_double_2d, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      allocate( temp(spec(1),spec(2)), STAT=xrealloc_double_2d, ERRMSG=errmsg )
      if ( xrealloc_double_2d /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      ny = min( spec(2), size(object,DIM=2) )
      temp(1:nx,1:ny) = object(1:nx,1:ny)
      call move_alloc(temp,object)

    else
      xrealloc_double_2d = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if

  return
end function xrealloc_double_2d

integer function xrealloc_char( object, spec, errmsg )
  implicit none
  character(*), allocatable, intent(inout)  :: object(:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg
  character(:), allocatable                 :: temp(:)
  integer                                   :: nlen, nx

  if ( rank(object) /= size(spec) ) then
    xrealloc_char = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1)), STAT=xrealloc_char, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      nlen = len(object)
      allocate( character(nlen)::temp(spec(1)), STAT=xrealloc_char, ERRMSG=errmsg )
      if ( xrealloc_char /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      temp(1:nx) = object(1:nx)
      call move_alloc(temp,object)

    else
      xrealloc_char = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if

  return
end function xrealloc_char

integer function xrealloc_char_2d( object, spec, errmsg )
  implicit none
  character(*), allocatable, intent(inout)  :: object(:,:)
  integer, intent(in)                       :: spec(:)
  character(*), intent(out), optional       :: errmsg
  character(:), allocatable                 :: temp(:,:)
  integer                                   :: nlen, nx, ny

  if ( rank(object) /= size(spec) ) then
    xrealloc_char_2d = -1
    if ( present(errmsg) ) errmsg = "Specified SIZE does not match object RANK."
  end if

  if ( .not. allocated(object) ) then
    allocate( object(spec(1),spec(2)), STAT=xrealloc_char_2d, ERRMSG=errmsg )

  else
    if ( any(shape(object)/=spec(:)) ) then
      nlen = len(object)
      allocate( character(nlen)::temp(spec(1),spec(2)), STAT=xrealloc_char_2d, ERRMSG=errmsg )
      if ( xrealloc_char_2d /= 0 ) return
      nx = min( spec(1), size(object,DIM=1) )
      ny = min( spec(2), size(object,DIM=2) )
      temp(1:nx,1:ny) = object(1:nx,1:ny)
      call move_alloc(temp,object)

    else
      xrealloc_char_2d = 0
      if ( present(errmsg) ) errmsg = ""

    end if
  end if

  return
end function xrealloc_char_2d

end module xslib_xmalloc
