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

module xslib_cpuinfo
  implicit none
  private
  public :: cpu_t

  type cpu_t
    character(32)   :: arch = ""    ! CPU architecture
    character(128)  :: model = ""   ! CPU model name
    real            :: freq = 0.000 ! MAX CPU frequency in MHz
    integer         :: threads = 1  ! Number of threads (or strands).
    integer         :: cores = 1    ! Number of cores.
    integer         :: sockets = 1  ! Number of sockets
    integer         :: nodes = 1    ! Number of NUMA nodes
    integer         :: L1 = 0       ! Size of L1 cache in KB
    integer         :: L2 = 0       ! Size of L2 cache in KB
    integer         :: L3 = 0       ! Size of L3 cache in KB
  contains
    procedure :: getInfo => read_lscpu
    procedure :: dump => cpu_dump
  end type cpu_t

  ! Interface to C routines
  ! Example: http://fortranwiki.org/fortran/show/m_process
  interface

    ! popen - Open pipe stream to or from a process.
    ! FILE *popen( const char *command, const char *type );
    type(C_PTR) function popen( command, type ) bind( C, name='popen' )
      use, intrinsic :: iso_c_binding
      implicit none
      character(C_CHAR), intent(in) :: command(*)
      character(C_CHAR), intent(in) :: type(*)
    end function popen

    ! pclose - Close pipe stream to or from a process.
    ! int pclose( FILE *stream );
    integer(C_INT) function pclose( stream ) bind( C, name='pclose' )
      use, intrinsic :: iso_c_binding
      implicit none
      type(C_PTR), value, intent(in) :: stream
    end function pclose

    ! fgets - Reads characters from stream and stores them as a C string into str until (num-1) characters
    ! * have been read or either a newline or the end-of-file is reached, whichever happens first.
    ! char *fgets( char *str, int num, FILE *stream );
    type(C_PTR) function fgets( str, num, stream ) bind( C, name='fgets' )
      use, intrinsic :: iso_c_binding
      implicit none
      character(C_CHAR), intent(out)    :: str
      integer(C_INT), value, intent(in) :: num
      type(C_PTR), value , intent(in)   :: stream
    end function fgets

  end interface

contains

! Read cpu info using UNIX lscpu utility.
! * lscpu - display information about the CPU architecture.
integer function read_lscpu( this )
  use, intrinsic :: iso_c_binding, only: C_PTR, C_NULL_CHAR, C_NULL_PTR, c_associated
  implicit none
  class(cpu_t)    :: this
  type(C_PTR)     :: stream = C_NULL_PTR
  character(128)  :: buf

  ! Open stream to lscpu
  stream = popen( "/usr/bin/lscpu"//C_NULL_CHAR, "r"//C_NULL_CHAR )
  if ( .not. c_associated(stream) ) then
    read_lscpu = -1
    return
  end if

  ! NOTE: L1 cache is listed as L1d (data cache) and L1i (instruction cache).

  ! Read from stream
  do while( c_associated(fgets( buf(:), len(buf)-1, stream )) )
    if ( lookup( buf, "Architecture", this%arch ) ) then
    else if ( lookup( buf, "Model name", this%model ) ) then
    else if ( lookup( buf, "CPU max MHz", this%freq ) ) then
    else if ( lookup( buf, "Thread(s) per core", this%threads ) ) then
    else if ( lookup( buf, "Core(s) per socket", this%cores ) ) then
    else if ( lookup( buf, "Socket(s)", this%sockets ) ) then
    else if ( lookup( buf, "NUMA node(s)", this%nodes ) ) then
    else if ( lookup_cache( buf, "L1d cache", this%L1 ) ) then
    else if ( lookup_cache( buf, "L1i cache", this%L1 ) ) then
    else if ( lookup_cache( buf, "L2 cache", this%L2 ) ) then
    else if ( lookup_cache( buf, "L3 cache", this%L3 ) ) then
    end if
  end do

  ! Correct thread and core count
  this%cores = this%sockets * this%cores
  this%threads = this%cores * this%threads

  ! Close sstream
  read_lscpu = pclose( stream )

  return
end function read_lscpu

! Parse buffer.
logical function lookup( buffer, token, val )
  use, intrinsic :: iso_c_binding, only: C_NULL_CHAR
  implicit none
  character(*), intent(in) :: buffer, token
  class(*), intent(out)    :: val
  integer                  :: stat, ni, nj

  lookup=.false.
  ni = index( buffer, token )
  if ( ni > 0 ) then
    ni = ni+len_trim(token)+1
    select type( val )
    type is( integer )
      read (buffer(ni:),*,IOSTAT=stat) val
    type is( real )
      read (buffer(ni:),*,IOSTAT=stat) val
    type is( character(*) )
      nj = index(buffer,C_NULL_CHAR)-2 ! Account for /null
      read (buffer(ni:nj),"(a)",IOSTAT=stat) val
      val = trim(adjustl(val))
    class default
      stat = -1
    end select
    if ( stat == 0 ) lookup=.true.
  end if

  return
end function lookup

! Parse buffer for cache line.
! * NOTE: Increment extisting value of val
logical function lookup_cache( buffer, token, val )
  implicit none
  character(*), intent(in) :: buffer, token
  integer, intent(inout)   :: val
  integer                  :: x, stat, ni, nj

  lookup_cache=.false.
  ni = index( buffer, token )
  if ( ni > 0 ) then
    ni = ni+len_trim(token)+1 ! Account for ":"
    nj = index(buffer(ni:),"K",BACK=.true.)-2
    nj = merge( ni+nj, len(buffer), nj > 0 )
    read (buffer(ni:nj),*,IOSTAT=stat) x
    if ( stat == 0 ) lookup_cache=.true.
    val = val+x
  end if

  return
end function lookup_cache

! Write cpu info
integer function cpu_dump( this )
  implicit none
  class(cpu_t)  :: this

  ! General info
  write (*,100) adjustl("Arch:"), trim(this%arch)
  write (*,100) "Model:", trim(this%model)
  100 format( x,a13,2x,a)

  write (*,200) "CPU freq:", this%freq, "MHz"
  200 format( x,a13,2x,f0.2,a)

  write (*,300) "Thread(s):", this%threads
  write (*,300) "Core(s):", this%cores
  write (*,300) "Socket(s):", this%sockets
  write (*,300) "NUMA node(s):", this%nodes
  300 format( x,a13,2x,i0)

  write (*,400) "L1 Cache:", this%L1, "K"
  write (*,400) "L2 Cache:", this%L2, "K"
  write (*,400) "L3 Cache:", this%L3, "K"
  400 format( x,a13,2x,i0,a)

  ! Return Sucess
  cpu_dump = 0

  return
end function cpu_dump

end module xslib_cpuinfo

! ! Driver
! program main
!   use xslib_cpuinfo
!   implicit none
!   type(cpu_t) :: cpu
!   integer     :: stat
!   stat = cpu%getInfo()
!   stat = cpu%dump()
! end program main
