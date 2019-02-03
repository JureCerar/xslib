! This file WAS a part of libgmxfort
! https://github.com/wesbarnett/libgmxfort
!
! Copyright (c) 2016,2017 James W. Barnett
!
! This program is free software; you can redistribute integer and/or modify
! integer under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that integer will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along
! with this program; if not, write to the Free Software Foundation, Inc.,
! 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
!
!
! This code was modified by Jure Cerar -- 23. Apr 2018
! This is originally libgmxfort's "Trajectory.f90"
! The names of variables has been changed in order to make more unified API.

module xslib_trj
	use, intrinsic :: iso_c_binding, only: C_PTR, C_CHAR, C_FLOAT, C_INT
	use XsLib_common
	use xslib_ndx
	implicit none
	private
	public xdrfile_open, write_xtc, xdrfile_close

	type :: Frame
		real(C_FLOAT), allocatable :: coor(:,:)
		integer(C_INT) :: STEP
		real(C_FLOAT) :: box(3,3), prec, time
	end type

	type, public :: trj_file
		type(xdrfile), pointer :: xd
		type(Frame), allocatable :: frameArray(:)
		type(ndx_file) :: ndx
		integer :: NFRAMES
		integer :: NUMATOMS, N
		integer :: FRAMES_REMAINING
		logical :: read_only_index_group
	contains
		procedure :: open => trajectory_open
		procedure :: read => trajectory_read
		procedure :: read_next => trajectory_read_next
		procedure :: close => trajectory_close
		procedure :: x => trajectory_get_coor
		procedure :: natoms => trajectory_get_natoms
		procedure :: box => trajectory_get_box
		procedure :: time => trajectory_get_time
		procedure :: step => trajectory_get_step
	end type

	! the data type located in libxdrfile
	type, public, bind(C) :: xdrfile
		type(C_PTR) :: fp, xdr
		character(kind=C_CHAR) :: mode
		integer(C_INT) :: buf1, buf1size, buf2, buf2size
	end type xdrfile

	! interface with libxdrfile
	interface

		integer(C_INT) function read_xtc_natoms(filename,NUMATOMS) bind(C, name='read_xtc_natoms')
			import
			character(kind=C_CHAR), intent(in) :: filename
			integer(C_INT), intent(out) :: NUMATOMS
		end function

		type(C_PTR) function xdrfile_open(filename,mode) bind(C, name='xdrfile_open')
			import
			character(kind=C_CHAR), intent(in) :: filename(*), mode(*)
		end function

		integer(C_INT) function read_xtc(xd,NUMATOMS,STEP,time,box,x,prec) bind(C, name='read_xtc')
			import
			type(xdrfile), intent(in) :: xd
			integer(C_INT), intent(out) :: NUMATOMS, STEP
			real(C_FLOAT), intent(out) :: time, prec, box(*), x(*)
		end function

		! TODO: Not used in this module
		integer(C_INT) function write_xtc(xd,NUMATOMS,STEP,time,box,x,prec) bind(C, name='write_xtc')
			import
			type(xdrfile), intent(in) :: xd
			integer(C_INT), value, intent(in) :: NUMATOMS, STEP
			real(C_FLOAT), intent(in) :: box(*), x(*)
			real(C_FLOAT), value, intent(in) :: time, prec
		end function

		integer(C_INT) function xdrfile_close(xd) bind(C,name='xdrfile_close')
			import
			type(xdrfile), intent(in) :: xd
		end function

		integer(C_INT) function read_xtc_n_frames(filename, N_FRAMES, EST_NFRAMES, OFFSETS) bind(C,name="read_xtc_n_frames")
			import
			character(kind=C_CHAR), intent(in) :: filename(*)
			integer(C_INT), intent(out) :: N_FRAMES, EST_NFRAMES
			type(C_PTR) :: OFFSETS
		end function

	end interface

contains

	subroutine trajectory_open(this, filename_in, ndxfile)

		use, intrinsic :: iso_c_binding, only: C_NULL_CHAR, c_f_pointer

		implicit none
		class(trj_file), intent(inout) :: this
		type(C_PTR) :: xd_c
		character (len=*), intent(in) :: filename_in
		character (len=*), intent(in), optional :: ndxfile
		character (len=206) :: filename
		logical :: ex
		integer :: EST_NFRAMES
		type(C_PTR) :: OFFSETS_C

		inquire(file=trim(filename_in), exist=ex)

		if (ex .eqv. .false.) then
			call error(trim(filename_in)//" does not exist.", NAME="LIBGMXFORT")
		end if

		! Set the file name to be read in for C.
		filename = trim(filename_in)//C_NULL_CHAR

		! Get number of atoms in system
		if (read_xtc_natoms(filename, this%NUMATOMS) .ne. 0) then
			call error("Problem reading in "//trim(filename_in)//". Is it really an xtc file?", NAME="LIBGMXFORT")
		end if

		this%N = this%NUMATOMS ! Save for use when user selects just one group
		if (present(ndxfile)) call this%ndx%indexfile_read(ndxfile, this%NUMATOMS)

		! Get total number of frames in the trajectory file
		if (read_xtc_n_frames(filename, this%NFRAMES, EST_NFRAMES, OFFSETS_C) .ne. 0) then
			call error("Problem getting number of frames in xtc file.", NAME="LIBGMXFORT")
		end if
		this%FRAMES_REMAINING = this%NFRAMES

		! Open the file for reading. Convert C pointer to Fortran pointer.
		xd_c = xdrfile_open(filename,"r")
		call c_f_pointer(xd_c, this % xd)

	end subroutine trajectory_open

	subroutine trajectory_read(this, xtcfile, ndxfile, ndxgrp)

		implicit none
		class(trj_file), intent(inout) :: this
		character (len=*) :: xtcfile
		character (len=*), optional :: ndxfile, ndxgrp
		integer :: N

		call this%open(xtcfile, ndxfile)

		N = this%read_next(this%NFRAMES, ndxgrp)

		call this%close()

	end subroutine trajectory_read

	function trajectory_read_next(this, F, ndxgrp)

		implicit none
		integer :: trajectory_read_next
		class(trj_file), intent(inout) :: this
		integer, intent(in), optional :: F
		character (len=*), optional :: ndxgrp
		real :: box_trans(3,3)
		real, allocatable :: coor(:,:)
		integer :: STAT = 0, I, N, J, NATOMS

		! If the user specified how many frames to read and it is greater than one, use it
		N = merge(F, 1, present(F))

		! Are we near the end of the file?
		N = min(this%FRAMES_REMAINING, N)
		this%FRAMES_REMAINING = this%FRAMES_REMAINING - N

		if (allocated(this%frameArray)) deallocate(this%frameArray)
		allocate(this%frameArray(N))

		this%read_only_index_group = .false.

		if (present(ndxgrp)) then

			allocate(coor(3,this%N))
			this%NUMATOMS = this%natoms(trim(ndxgrp))
			do I = 1, N

				if (modulo(I, 1000) .eq. 0) call print_frames_saved(I)

				STAT = read_xtc(this%xd, NATOMS, this%frameArray(I)%STEP, this%frameArray(I)%time, box_trans, &
					coor, this%frameArray(I)%prec)
				! C is row-major, whereas Fortran is column major. Hence the following.
				this%frameArray(I)%box = transpose(box_trans)
				allocate(this%frameArray(I)%coor(3,this%NUMATOMS))

				do J = 1, size(this%ndx%group)
					if (trim(this%ndx%group(J)%title) .eq. trim(ndxgrp)) then
						this%frameArray(I)%coor = coor(:,this%ndx%group(J)%LOC)
						exit
					end if
				end do

			end do
			deallocate(coor)

			this%read_only_index_group = .true.

		else

			do I = 1, N

				if (modulo(I, 1000) .eq. 0) call print_frames_saved(I)

				allocate(this%frameArray(I)%coor(3,this%NUMATOMS))
				STAT = read_xtc(this%xd, NATOMS, this%frameArray(I)%STEP, this%frameArray(I)%time, box_trans, &
					this%frameArray(I)%coor, this%frameArray(I)%prec)
				! C is row-major, whereas Fortran is column major. Hence the following.
				this%frameArray(I)%box = transpose(box_trans)

			end do

		end if

		call print_frames_saved(N)
		trajectory_read_next = N

	end function trajectory_read_next

	subroutine print_frames_saved(I)

		implicit none
		integer, intent(in) :: I

		! write(error_unit,'(a,i0)') achar(27)//"[1A"//achar(27)//"[K"//"Frames saved: ", I

	end subroutine print_frames_saved

	subroutine trajectory_close(this)

		implicit none
		class(trj_file), intent(inout) :: this

		if (xdrfile_close(this % xd) /=0) call error("Problem closing xtc file.", NAME="LIBGMXFORT")

	end subroutine trajectory_close

	function trajectory_get_coor(this, frame, atom, group)

		implicit none
		real :: trajectory_get_coor(3)
		integer, intent(in) :: frame, atom
		integer :: atom_tmp, natoms
		class(trj_file), intent(inout) :: this
		character (len=*), intent(in), optional :: group
		character (len=1024) :: message

		call trajectory_check_frame(this, frame)

		if (this%read_only_index_group .and. present(group)) then
			call error("Do not specify an index group in x() when already specifying an &
				&index group with read() or read_next().", NAME="LIBGMXFORT")
		end if

		atom_tmp = merge(this%ndx%get(group, atom), atom, present(group))
		natoms = merge(this%natoms(group), this%natoms(), present(group))

		if (atom > natoms .or. atom < 1) then
			write(message, "(a,i0,a,i0,a)") "Tried to access atom number ", atom_tmp, " when there are ", &
				natoms, ". Note that Fortran uses one-based indexing."
			call error(trim(message), NAME="LIBGMXFORT")
		end if

		trajectory_get_coor = this%frameArray(frame)%coor(:,atom_tmp)

	end function trajectory_get_coor

	function trajectory_get_natoms(this, group)

		implicit none
		integer :: trajectory_get_natoms
		class(trj_file), intent(inout) :: this
		character (len=*), intent(in), optional :: group

		if (this%read_only_index_group .and. present(group)) then
			call error("Do not specify an index group in natoms() when already specifying an &
				&index group with read() or read_next().", NAME="LIBGMXFORT")
		end if

		trajectory_get_natoms = merge(this%ndx%get_natoms(group), this%NUMATOMS, present(group))

	end function trajectory_get_natoms

	function trajectory_get_box(this, frame)

		implicit none
		real :: trajectory_get_box(3,3)
		class(trj_file), intent(in) :: this
		integer, intent(in) :: frame

		call trajectory_check_frame(this, frame)
		trajectory_get_box = this%frameArray(frame)%box

	end function trajectory_get_box

	function trajectory_get_time(this, frame)

		implicit none
		real :: trajectory_get_time
		class(trj_file), intent(in) :: this
		integer, intent(in) :: frame

		call trajectory_check_frame(this, frame)
		trajectory_get_time = this%frameArray(frame)%time

	end function trajectory_get_time

	function trajectory_get_step(this, frame)

		implicit none
		integer :: trajectory_get_step
		class(trj_file), intent(in) :: this
		integer, intent(in) :: frame

		call trajectory_check_frame(this, frame)
		trajectory_get_step = this%frameArray(frame)%step

	end function trajectory_get_step

	subroutine trajectory_check_frame(this, frame)

		implicit none
		class(trj_file), intent(in) :: this
		integer, intent(in) :: frame
		character (len=1024) :: message

		if (frame > this%NFRAMES .or. frame < 1) then
			write(message, "(a,i0,a,i0,a)") "Tried to access frame number ", frame, " when there are ", &
				this%NFRAMES, ". Note that Fortran uses one-based indexing."
			call error(trim(message), NAME="LIBGMXFORT")
		end if

	end subroutine trajectory_check_frame

end module xslib_trj
