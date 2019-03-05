module xslib_xyz
	use xslib_common
	private
	public :: xyz_file

	type xyz_frame
		integer 									:: natoms
		character*512							:: comment
		character*5, allocatable	:: name(:)
		real, allocatable					:: coor(:,:)
		real											:: box(3)	! Actual .xyz does not contain box info
	contains
		procedure	:: allocate => allocate_xyz_frame
		procedure	:: deallocate => deallocate_xyz_frame
		procedure	:: initialize => initialize_xyz_frame
	end type

	type xyz_file
		integer, private							:: unit
		integer 											:: nframes, framesLeft
		type(xyz_frame), allocatable	:: frameArray(:)
	contains
		procedure	:: open => open_xyz
		procedure	:: close => close_xyz
		procedure	:: allocate => allocate_xyz
		procedure	:: read_next => read_next_xyz
		procedure	:: read => read_xyz
		procedure	:: box => box_xyz
		procedure	:: natoms => natoms_xyz
		procedure	:: write => read_xyz
	end type

contains

	! Deallocate and allocate XYZ file
	subroutine allocate_xyz_frame (this, np, initialize)
		class(xyz_frame)	:: this
		integer, intent(in)	:: np	! Number of points to allocate
		class(*), optional	:: initialize
		integer				:: stat

		! Array size check
		if (np < 1) call error("Invalid array size: "//itoa(np), NAME="xyz%allocate()")

		! Deallocate
		call this%deallocate()
		allocate (this%name(np), this%coor(3,np), STAT=stat)

		! Initialize data
		if (present(initialize)) call this%initialize()

		return
	end subroutine allocate_xyz_frame

	! Initialize frame data
	subroutine initialize_xyz_frame (this)
		implicit none
		class(xyz_frame)	:: this	! .GRO data file

		this%comment = ""

		! Initialize data if allocated or not
		if (allocated(this%coor)) then
			this%natoms 	= size(this%coor, 2)
			this%name(:)	= ""
			this%coor 		= 0.
			this%box		= 0.

		else
			this%natoms 	= 0

		end if

		return
	end subroutine initialize_xyz_frame

	! Deallocate single frame
	subroutine deallocate_xyz_frame (this)
		implicit none
		class(xyz_frame)	:: this
		integer				:: stat

		deallocate(this%coor, this%name, STAT=stat)

		return
	end subroutine deallocate_xyz_frame

	! ============================================

	! Open .xyz file (and read number of frames)
	subroutine open_xyz (this, file)
		implicit none
		class(xyz_file)	:: this
		character*(*)	:: file
		integer			:: i, np, stat
		character*3		:: dmy

		open (NEWUNIT=this%unit, FILE=trim(file), STATUS="unknown", IOSTAT=stat)
		if (stat/=0) call error("Cannot open file: "//trim(file), NAME="xyz%open()")

		! XYZ format
		! <num of atoms>
		! comment
		! <name> <x> <y> <z>  ("name" is optional!)
		! ...

		! Count number of frames as occurance of END until EOF
	   	this%nframes = 0
	   	do while (stat==0)
		   read (this%unit, *, END=100) np ! number of particles

		   ! If we go this far another frame must exist
		   this%nframes = this%nframes+1

		   ! Dummy read next
		   do i = 1, np+1 ! + first line is comment
			   read (this%unit, *, END=200) ! dummy read
		   end do

	   	end do

	   ! Rewind the file
	   100 rewind (this%unit)

	   ! All frames are left
	   this%framesLeft = this%nframes

		return
		200 call error("Incorect number of lines.", NAME="xyz%open()")
	end subroutine open_xyz

	! Close .xyz file
	subroutine close_xyz (this)
		implicit none
		class(xyz_file)	:: this
		integer			:: stat
		close (this%unit)
		return
	end subroutine close_xyz

	! Allocate .xyz frameArray
	subroutine allocate_xyz (this, np)
		implicit none
		class(xyz_file)		:: this
		integer, intent(in)	:: np
		character*128		:: message
		integer				:: stat

		! Error check
		if (np < 0) then
			write (message, "(a,i0)") "Invalid array size: ", np
			call error(message, NAME="xyz%frameArray%allocate()")
		end if

		if (allocated(this%frameArray)) deallocate(this%frameArray, STAT=stat)
		allocate (this%frameArray(np), STAT=stat)
		if (stat /= 0) call error("Not enough memory.", NAME="xyz%frameArray%allocate()")

		return
	end subroutine allocate_xyz

	! Read frames from opened .XYZ fales. if NFRAMES not specified read only one frame.
	function read_next_xyz (this, nframes) result (framesRead)
		implicit none
		class(xyz_file)		:: this
		integer, optional	:: nframes
		integer				:: framesRead
		! internal
		character*256		:: file, buffer
		integer				:: i, n, np, stat
		logical				:: opened

		! If number of frames in not defined read only one frame
		np = 1
		if (present(nframes)) np = nframes

		! Check if opened
		inquire (UNIT=this%unit, OPENED=opened)
		if (.not. opened) call error("File UNIT not assigned.", NAME="xyz%read_next()")

		! XYZ format
		! <num of atoms>
		! comment
		! <name> <x> <y> <z>  ("name" is optional!)
		! ...

		! How many frames are left
		if (np > this%framesLeft) then
			framesRead = this%framesLeft
		else
			framesRead = np
		end if

		! Allocate frameArray
		if (allocated(this%frameArray)) deallocate(this%frameArray, STAT=stat)
		if (framesRead /= 0) allocate (this%frameArray(framesRead), STAT=stat)
		if (stat /= 0) call error("Not enough memory.", NAME="xyz%read_next()")

		do n = 1, framesRead
			! One less frame to read
			this%framesLeft = this%framesLeft-1

			! Read number of lines
			read (this%unit, *, IOSTAT=stat, END=100) this%frameArray(n)%natoms
			if (stat/=0) call error ("Invalid number of atoms in frame: "//str(n), NAME="xyz%read_next()")

			! Read comment
			read (this%unit, "(a)", END=100) this%frameArray(n)%comment

			! Allocate memmory
			call this%frameArray(n)%allocate(this%frameArray(n)%natoms)

			! Read lines
			do i = 1, this%frameArray(n)%natoms
				read (this%unit, "(a)", END=100) buffer

				! Read <name> only if present
				if (scan(trim(buffer), "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz")/=0) then
					read (buffer, *, ERR=110) this%frameArray(n)%name(i), this%frameArray(n)%coor(:,i)

				else
					read (buffer, *, ERR=110) this%frameArray(n)%coor(:,i)
					this%frameArray(n)%name(i) = ""

				end if

			end do

			! Return DUMMY BOX info
			this%frameArray(n)%box(:) = -1.

		end do ! for n

		return
		100 call error("Unexpected EoF.", NAME="xyz%read_next()")
		110 call error("Cannot phrase line: '"//trim(buffer)//"'", NAME="xyz%read_next()")
	end function read_next_xyz

	! Opens file and read all the data
	subroutine read_xyz (this, file)
		implicit none
		class(xyz_file)	:: this
		character*(*)	:: file
		integer			:: stat

		call this%open(file)
		stat = this%read_next(this%nframes)
		call this%close()

		return
	end subroutine read_xyz

	! .xyz data does not contain box information; return -1 and prompt warning
	function box_xyz (this) result (box)
		use iso_fortran_env
		implicit none
		class(xyz_file)	:: this
		real			:: box(3)

		box = -1.
		call warning (".xyz file does not contain box data.")

		return
	end function box_xyz

	! returns num of atoms in current frame without changing possition in file
	function natoms_xyz (this) result (natoms)
		implicit none
		class(xyz_file)	:: this
		integer			:: natoms
		logical			:: opened
		integer			:: stat
		character*6		:: dmy

		! Initialize
		natoms = 0

		! Error check
		inquire (UNIT=this%unit, OPENED=opened)
		if (.not. opened) call error ("File UNIT not assigned.", NAME="xyz%natoms()")

		! If data was already read
		if (allocated(this%frameArray)) then
			natoms = this%frameArray(1)%natoms

		! Else it must be first time reading (OK to use rewind)
		else
			! just read first line and rewind
			read (this%unit, *) natoms
			rewind (this%unit)

		end if

		return
	end function natoms_xyz


	! Write .XYZ data to either UNIT, FILE or STDOUT (if neither are present)
	subroutine write_xyz (this, file, unit)
		use iso_fortran_env
		implicit none
		class(xyz_file)			:: this
		character*(*), optional	:: file
		integer, optional		:: unit
		! internal
		character*128			:: string
		integer					:: i, n, u, stat
		logical					:: opened

		! Check if opened
		if (present(unit)) then
			! Check if opened
			inquire (UNIT=unit, OPENED=opened)
			if (.not. opened) call error ("File UNIT not assigned.", NAME="xyz%write()")
			u = unit

		else if (present(file)) then
			open (NEWUNIT=u, FILE=trim(file), STATUS="unknown", IOSTAT=stat)
			if (stat /= 0) call error ("Cannot open file: '"//trim(file)//"'", NAME="xyz%write()")

		else
			u = output_unit ! stdout

		end if

		! XYZ format
		! <num of atoms>
		! comment
		! <name> <x> <y> <z>  ("name" is optional!)
		! ...

		do n = 1, this%nframes
			! Number of lines
			write (string, "(i0)") this%frameArray(n)%natoms
			write (u, "(a)") adjustl(trim(string))

			! Comment
			write (u, "(a)") adjustl(trim(this%frameArray(n)%comment))

			! Read lines
			do i = 1, this%frameArray(n)%natoms
				write (unit, "(a5,3f10.5)") this%frameArray(n)%name(i), this%frameArray(n)%coor(:,i)

			end do
		end do ! for n

		return
	end subroutine write_xyz


end module xslib_xyz
