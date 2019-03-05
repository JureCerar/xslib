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
! This is originally libgmxfort's "indexfile.f90"
! I have included my own write/read procedures.
! Some variables have been renamed for API cinsistancy.

module xslib_ndx
	use xslib_common
    implicit none
    private
	public :: ndx_file

    type ndxgroups
      integer, allocatable 						:: loc(:)
      integer 												:: natoms
      character (len=:), allocatable 	:: title
    end type ndxgroups

    type ndx_file
      type (ndxgroups), allocatable 	:: group(:)
			integer															:: ngroups=0
      logical 												:: group_warning=.true.
    contains
			procedure :: read => read_ndx
			procedure :: write => write_ndx
    	procedure :: indexfile_read
    	procedure :: get => indexfile_get
    	procedure :: get_natoms => indexfile_get
    end type ndx_file

contains

	! Read GROMACS index .ndx file
	subroutine read_ndx (this, file)
		implicit none
		class (ndx_file)					:: this
		character*(*), intent(in)	:: file
		logical										:: exist
		integer										:: i, j, n, ncol, nlines, offset, cnt, unit,  stat
		integer										:: array(100)
		character*1024						:: line
		character*3								:: dmy

		! Open file
		open (NEWUNIT=unit, FILE=trim(file), STATUS="old", IOSTAT=stat)
		if (stat/=0) call error ("Cannot open file: '"//trim(file)//"'", NAME="ndx%read()")

		! Is it index file?
		read (unit, "(a)", IOSTAT=stat) dmy
			if (index(dmy, "[") == 0) call error(trim(file)//" is not a valid index file.", NAME="ndx%read()")
		backspace (unit) !rewind

		! Count number of groups in file
		this%ngroups = 0
		do while (stat == 0)
			read (unit, "(a)", IOSTAT=stat) dmy
			if (stat /= 0) exit
			if (index(dmy, "[") /= 0) this%ngroups = this%ngroups+1

		end do
		rewind (unit)

		! Allocate data
		if (allocated(this%group)) deallocate (this%group, STAT=stat)
		allocate (this%group(this%ngroups), STAT=stat)

		! Read for each group
		do n = 1, this%ngroups
			! Read title
			read (unit, "(a)", IOSTAT=stat) line
			if (stat /= 0) call error ("Unknown error.", NAME="ndx%read()")
			this%group(n)%title = trim(line(index(line,"[")+2:index(line,"]")-2))

			this%group(n)%natoms = 0
			ncol = size(array)
			nlines = 0

			! Read lines and evalueate number of columns
			do while (stat == 0)
				read (unit, "(a)", IOSTAT=stat) line
					if (stat /= 0) exit

				if (index(line, "[") /= 0) then
					backspace (unit)
					exit
				end if

				! Number of lines read
				nlines = nlines+1

				stat = 1
				do while (stat /= 0)
					read (line, *, IOSTAT=stat) array(1:ncol)
					if (stat /= 0) ncol = ncol-1
				end do

				! Number of atoms is the number of columns
				this%group(n)%natoms = this%group(n)%natoms+ncol

			end do

			! Rewind record for number of lines read
			do i = 1, nlines
				backspace (unit, IOSTAT=stat)
			end do

			! When last entry is read backspace once more to account for EoF.
			if (n == this%ngroups) backspace (unit)

			! Allocate actual data
			if (allocated(this%group(n)%loc)) deallocate(this%group(n)%loc, STAT=stat)
			allocate(this%group(n)%loc(this%group(n)%natoms), STAT=stat)

			! Read the actual data
			ncol = size(array)
			offset = 1
			do i = 1, nlines
				read (unit, "(a)", IOSTAT=stat) line
					if (stat /= 0) call error("Unknown error.", NAME="ndx%read()")

				! Evaluate number of columns.
				stat = 1
				do while (stat /= 0)
					read (line, *, IOSTAT=stat) array(1:ncol)
					if (stat /= 0) ncol = ncol-1

				end do

				! Write actual data
				this%group(n)%loc(offset:offset+ncol-1) = array(1:ncol)
				offset = offset+ncol

			end do

		end do ! for n

		close (unit)

		return
	end subroutine read_ndx

	! Write index file to file UNIT, FILE or STDOUT
	subroutine write_ndx (this, file, unit)
		use iso_fortran_env
		implicit none
		class (ndx_file)										:: this
		character*(*), intent(in), optional	:: file
		integer, intent(in), optional				:: unit
		logical															:: opened
		integer															:: i, n, u, rows, cols, stat
		character*64												:: fmt

		! Check if data is allocated
		if (.not. allocated(this%group)) call error ("Data not allocated.", NAME="ndx%write()")

		if (present(unit)) then
			u = unit
			inquire (UNIT=u, OPENED=opened)
			if (.not. opened) call error ("File UNIT not assigned.", NAME="ndx%write()")

		else if (present(file)) then
			open (NEWUNIT=u, FILE=trim(file), STATUS="unknown", IOSTAT=stat)
			if (stat/=0) call error ("Cannot open file: '"//trim(file)//"'", NAME="ndx%read()")

		else
			u = output_unit

		end if

		! For each group
		do n = 1, this%ngroups
			! Title
			write (u, 100) trim(this%group(n)%title)
			100 format ("[",x,a,x,"]")

			! Default number of rows and cols
			cols = 15
			rows = this%group(n)%natoms/cols

			! Create custom format
			write (fmt,"(a,i0,a)") "(", cols, "(i0,x))"

			! Write "full" rows and columns
			do i = 1, rows, cols
				write (u, fmt) this%group(n)%loc(i:i+cols-1)

			end do
			! Write remaining columns
			write (u, fmt) this%group(n)%loc(i:this%group(n)%natoms)

		end do ! for n

		! Close unit only for newly opened file
		if (present(file)) close (u)

		return
	end subroutine write_ndx


  subroutine indexfile_read(this, filename, N)
		use iso_fortran_env
	  implicit none
	  class(ndx_file), intent(inout) :: this
	  character (len=*), intent(in) :: filename
	  character (len=2048) :: line, NCOLS_string, fmt_string
	  integer :: INDEX_FILE_UNIT, IO_STATUS, NGRPS, I, J, NCOLS
	  integer, allocatable :: INDICES_TMP(:), TITLE_LOC(:), num_array(:)
	  logical :: ex
	  integer, intent(in) :: N

	  ! Does the file exist?
	  inquire(file=trim(filename), exist=ex)
	  if (ex .eqv. .false.) call error(trim(filename)//" does not exist.", NAME="LIBGMXFORT")

	  ! Is in index file?
	  open(newunit=INDEX_FILE_UNIT, file=trim(filename), status="old")
	  read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line
	  if (index(line, "[") .eq. 0) call error(trim(filename)//" is not a valid index file.", NAME="LIBGMXFORT")

	  ! How many groups are in it?
	  rewind INDEX_FILE_UNIT
	  IO_STATUS = 0
	  NGRPS = 0
	  do while (IO_STATUS .eq. 0)
	      read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line
	      if (IO_STATUS .ne. 0) goto 100
	      if (index(line, "[") .ne. 0) NGRPS = NGRPS + 1
	  end do

		100	continue

	  if (allocated(this%group)) deallocate(this%group)
	  allocate(this%group(NGRPS), TITLE_LOC(NGRPS+1)) ! Add one to include end of file

	  ! Now find the title locations and save their names
	  rewind INDEX_FILE_UNIT
	  I = 1
	  J = 1
	  IO_STATUS = 0
	  do while (IO_STATUS .eq. 0)

	      read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line
	      if (IO_STATUS .ne. 0) goto 200
	      if (index(line, "[") .ne. 0) then
	          this%group(I)%title = trim(line(index(line, "[")+2:index(line, "]")-2))
	          TITLE_LOC(I) = J
	          I = I + 1
	      end if
	      J = J + 1

	  end do

		200	continue

	  if (this%group(1)%title .ne. "System") call error ( &
		"Index file does not have 'System' group as first group.", NAME="LIBGMXFORT")

	  ! Index files can have a varying number of columns. This attempts to
	  ! detect the correct number by reading in the second line of the file,
	  ! which should be a list of indices for the "System" group.
	  NCOLS = 100
	  allocate(num_array(NCOLS))
	  IO_STATUS = 5000
	  do while (IO_STATUS .ne. 0)
	      NCOLS = NCOLS - 1
	      write(NCOLS_string, '(i0)') NCOLS
	      write(fmt_string, '(a)') '('//trim(NCOLS_string)//'i0)'
	      rewind INDEX_FILE_UNIT
	      read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line
	      read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line
	      read(line, *, iostat=IO_STATUS) num_array(1:NCOLS)
	  end do

	  TITLE_LOC(I) = J ! End of file location

	  ! Now finally get all of the indices for each group
	  ! Allocate for total number of atoms in system, since that is the maximum
	  allocate(INDICES_TMP(N))
	  do I = 1, NGRPS

	      ! Initial guess only how many items are in the group
	      ! Add 1, bc loop subtracts 1 at the beginning
	      this%group(I)%natoms = (TITLE_LOC(I+1)-TITLE_LOC(I)-1)*NCOLS + 1

	      if (N < this%group(I)%natoms) this%group(I)%natoms = N + 1
	      IO_STATUS = 5000

	      do while (IO_STATUS .ne. 0)

	          ! Our guess was too large if we made it back here, go to the beginning and reduce our guess by 1, try again
	          rewind INDEX_FILE_UNIT
	          this%group(I)%natoms = this%group(I)%natoms - 1
	          if (this%group(I)%natoms .le. 0) then
	              this%group(I)%natoms = 0
	              goto 300
	          end if

	          ! Read all the way to the group
	          do J = 1, TITLE_LOC(I); read(INDEX_FILE_UNIT, '(a)', iostat=IO_STATUS) line; end do

	          ! Attempt to read into array
	          read(INDEX_FILE_UNIT, *, iostat=IO_STATUS) INDICES_TMP(1:this%group(I)%natoms)

	      end do

	      ! Specifying array bounds for array to be allocated is not required for F2008 but is required for F2003
				! `SOURCE=` directive has been replaced
				allocate(this%group(I)%LOC(1:this%group(I)%natoms))
				this%group(I)%LOC(:) = INDICES_TMP(1:this%group(I)%natoms)


				300	cycle

	  	end do
	  	deallocate(INDICES_TMP)

			do I = 1, NGRPS-1
			  do J = I+1, NGRPS
			      if (I .ne. J) then
			          if (this%group(I)%title .eq. this%group(J)%title) then
			              write(error_unit,*)
			              write(error_unit,'(a, a, a)') "LIBDCDFORT WARNING: Index group ", this%group(I)%title, &
			                  " was specified more than once in index file."
			              write(error_unit,*)
			          end if
			      end if
			  end do
			end do

		  ! Error checking to see if index file goes with the trajectory file

		  ! If the number of atoms is not the same in the System group (first group) and xtc file
		  if (this%group(1)%natoms .ne. N .or. this%group(1)%loc(this%group(1)%natoms) .ne. N) then
		      call error("Index file does not match xtc file.", NAME="LIBGMXFORT")
		  end if

		  do i = 1, NGRPS
		      ! If number of atoms in index group is larger than number of atoms in xtc file
		      if (this%group(i)%natoms .gt. N) call error("Index file does not match xtc file.", NAME="LIBGMXFORT")

		      ! If a location number is greater than number of atoms in xtc file
		      do j = 1, this%group(i)%natoms
		          if (this%group(i)%loc(j) .gt. N) call error("Index file does not match xtc file.", NAME="LIBGMXFORT")
		      end do

		  end do


	  close(INDEX_FILE_UNIT)

		return
	end subroutine indexfile_read

  ! Gets the number of atoms in a group. If an atom is specified, integer returns the overall index for that atom.
  function indexfile_get(this, group_name, I)
		use iso_fortran_env
    implicit none
    integer :: indexfile_get
    class(ndx_file), intent(inout) :: this
    character (len=*), intent(in) :: group_name
    integer, intent(in), optional :: I
    integer :: J

    do J = 1, size(this%group)

        if (trim(this%group(J)%title) .eq. trim(group_name)) then

            indexfile_get = merge(this%group(J)%LOC(I), this%group(J)%natoms, present(I))
            return

        end if

    end do

    ! If user asked to get the number of atoms in an index group, and that index group is not
    ! in the index file, just return 0. If the user specified an atom number, then throw an error,
    ! since the overall index cannot be returned in that case
    if (.not. present(I)) then
        if (this%group_warning) then
            write(error_unit, '(a)') "LIBDCDFORT WARNING: No atoms found in index group '"//trim(group_name)//"'."
            write(error_unit, '(a)') "This warning will not appear again for any other index groups."
            this%group_warning = .false.
        end if
        indexfile_get = 0
    else
        indexfile_get = -1
        write(error_unit, '(a)') "LIBDCDFORT ERROR: "//trim(group_name)//" is not in index file. The groups available are:"
        do J = 1, size(this%group)
            write(error_unit,'(a10,a,i0,a)') this%group(J)%title, " (", this%group(J)%natoms, ")"
        end do
        stop 1
    end if

		return
  end function indexfile_get

end module xslib_ndx
