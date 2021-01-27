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

#define __THISFILE__ "xmalloc.F90"
! #define assert(x) assert_( x, __THISFILE__, __LINE__ )
#define xslibCheck(x) xslibCheck_( x, __THISFILE__, __LINE__ )

program main
  use iso_fortran_env, only: INT32, INT64, REAL32, REAL64
  use xslib_xmalloc
  use xslib_error
  implicit none
  integer, parameter          :: DIM = 5
  integer, allocatable        :: int_d(:), int_2d(:,:)
  integer(INT64), allocatable :: long_d(:), long_2d(:,:)
  real, allocatable           :: float_d(:), float_2d(:,:)
  real(REAL64), allocatable   :: double_d(:), double_2d(:,:)
  character(128)              :: errmsg

  ! ----------------------------------------------------------------
  ! Test xmalloc
  call xslibCheck( xmalloc(int_d,[DIM]) )
  call xslibCheck( xmalloc(long_d,[DIM]) )
  call xslibCheck( xmalloc(float_d,[DIM]) )
  call xslibCheck( xmalloc(double_d,[DIM]) )
  ! Check allocation and size
  call assert( [allocated(int_d),allocated(long_d),allocated(float_d),allocated(double_d)] ) ! Check allocation
  call assert( [size(int_d),size(long_d),size(float_d),size(double_d)] == DIM ) ! Check size
  deallocate(int_d,long_d,float_d,double_d)
  write (*,*) "xmalloc(): DIM=1 -- OK"

  call xslibCheck( xmalloc(int_2d,[DIM,DIM]) )
  call xslibCheck( xmalloc(long_2d,[DIM,DIM]) )
  call xslibCheck( xmalloc(float_2d,[DIM,DIM]) )
  call xslibCheck( xmalloc(double_2d,[DIM,DIM]) )
  ! Check allocation, and size
  call assert( [allocated(int_2d),allocated(long_2d),allocated(float_2d),allocated(double_2d)] ) ! Check allocation
  call assert( [size(int_2d),size(long_2d),size(float_2d),size(double_2d)] == DIM*DIM ) ! Check size
  deallocate(int_2d,long_2d,float_2d,double_2d)
  write (*,*) "xmalloc(): DIM=2 -- OK"

  ! ----------------------------------------------------------------
  ! Test xcalloc
  call xslibCheck( xcalloc(int_d,[DIM]) )
  call xslibCheck( xcalloc(long_d,[DIM]) )
  call xslibCheck( xcalloc(float_d,[DIM]) )
  call xslibCheck( xcalloc(double_d,[DIM]) )
  ! Check allocation, size, and initialization
  call assert( [allocated(int_d),allocated(long_d),allocated(float_d),allocated(double_d)] ) ! Check allocation
  call assert( [size(int_d),size(long_d),size(float_d),size(double_d)] == DIM ) ! Check size
  call assert( sum(int_d)+sum(long_d)+sum(float_d)+sum(double_d) == 0.000d0 ) ! Check formating
  ! deallocate(int_d,long_d,float_d,double_d)
  write (*,*) "xcalloc(): DIM=1 -- OK"

  call xslibCheck( xcalloc(int_2d,[DIM,DIM]) )
  call xslibCheck( xcalloc(long_2d,[DIM,DIM]) )
  call xslibCheck( xcalloc(float_2d,[DIM,DIM]) )
  call xslibCheck( xcalloc(double_2d,[DIM,DIM]) )
  ! Check allocation, size, and initialization
  call assert( [allocated(int_2d),allocated(long_2d),allocated(float_2d),allocated(double_2d)] ) ! Check allocation
  call assert( [size(int_2d),size(long_2d),size(float_2d),size(double_2d)] == DIM*DIM ) ! Check size
  call assert( sum(int_2d)+sum(long_2d)+sum(float_2d)+sum(double_2d) == 0.000d0 ) ! Check formating
  ! deallocate(int_2d,long_2d,float_2d,double_2d)
  write (*,*) "xcalloc(): DIM=2 -- OK"

  ! ----------------------------------------------------------------
  ! Test xrealloc

  ! Initialize to 1
  int_d(:)    = 1_INT32
  long_d(:)   = 1_INT64
  float_d(:)  = 1.0_REAL32
  double_d(:) = 1.0_REAL64
  ! Reallocate memory
  call xslibCheck( xrealloc(int_d,[2*DIM]) )
  call xslibCheck( xrealloc(long_d,[2*DIM]) )
  call xslibCheck( xrealloc(float_d,[2*DIM]) )
  call xslibCheck( xrealloc(double_d,[2*DIM]) )
  ! Check allocation, size, and memcheck.
  call assert( [allocated(int_d),allocated(long_d),allocated(float_d),allocated(double_d)] ) ! Check allocation
  call assert( [size(int_d),size(long_d),size(float_d),size(double_d)] == 2*DIM ) ! Check size
  call assert( [all(int_d(1:DIM)==1),all(long_d(1:DIM)==1_INT64),all(float_d(1:DIM)==1.0),all(double_d(1:DIM)==1.0_REAL64)] ) ! Check memcpy
  ! deallocate(int_d,long_d,float_d,double_d)
  write (*,*) "xrealloc(): DIM=1 -- OK"

  ! Initialize to 1
  int_2d(:,:)    = 1_INT32
  long_2d(:,:)   = 1_INT64
  float_2d(:,:)  = 1.0_REAL32
  double_2d(:,:) = 1.0_REAL64
  ! Reallocate memory
  call xslibCheck( xrealloc(int_2d,[2*DIM,2*DIM]) )
  call xslibCheck( xrealloc(long_2d,[2*DIM,2*DIM]) )
  call xslibCheck( xrealloc(float_2d,[2*DIM,2*DIM]) )
  call xslibCheck( xrealloc(double_2d,[2*DIM,2*DIM]) )
  ! Check allocation, size, and initialization
  call assert( [allocated(int_2d),allocated(long_2d),allocated(float_2d),allocated(double_2d)] ) ! Check allocation
  call assert( [size(int_2d),size(long_2d),size(float_2d),size(double_2d)] == 4*DIM*DIM ) ! Check size
  call assert( [all(int_2d(1:DIM,1:DIM)==1),all(long_2d(1:DIM,1:DIM)==1_INT64)] ) ! Check memcpy
  call assert( [all(float_2d(1:DIM,1:DIM)==1.0),all(double_2d(1:DIM,1:DIM)==1.0_REAL64)] ) ! part 2
  ! deallocate(int_2d,long_2d,float_2d,double_2d)
  write (*,*) "xrealloc(): DIM=2 -- OK"

  ! ----------------------------------------------------------------
  ! Test error message
  if ( xmalloc(int_d,[DIM],ERRMSG=errmsg) /= 0 ) write (*,*) trim(errmsg) ! OK
  if ( xmalloc(int_d,[DIM,DIM],ERRMSG=errmsg) /= 0 ) write (*,*) trim(errmsg) ! ERROR

contains

end program main
