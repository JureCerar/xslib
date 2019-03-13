module xslib_gro
	use xslib_common
	implicit none
	private
	public :: gro_file

	! .GRO file format
	type gro_frame
		character*512							:: title=""
		real											:: time=0.0
		integer										:: natoms=0
		real											:: box(3)=[0.,0.,0.]
		integer, allocatable			:: res_num(:), atom_num(:)
		character*5, allocatable	:: res_name(:), atom_name(:)
		real, allocatable					:: coor(:,:), vel(:,:)
	contains
		procedure	:: allocate => allocate_gro_frame
		procedure	:: deallocate => deallocate_gro_frame
		procedure	:: initialize => initialize_gro_frame
	end type


	type gro_file
		integer, private							:: unit
		integer												:: nframes, framesLeft
		type(gro_frame), allocatable	:: frameArray(:)
	contains
		procedure	:: open => open_gro
		procedure	:: close => close_gro
		procedure	:: allocate => allocate_gro
		procedure	:: read_next => read_next_gro
		procedure	:: read => read_gro
		procedure	:: write => write_gro
		procedure	:: natoms => natoms_gro
		procedure	:: box => box_gro
	end type gro_file

contains

	! Allocate .GRO file.; if np = 0 only deallocate
	subroutine allocate_gro_frame (this, np, onlycoor, initialize)
		class(gro_frame)		:: this	! .GRO data file
		integer, intent(in)	:: np	! Number of points to allocate
		class(*), optional	:: onlycoor, initialize	! if NOT present - allocate everything, if present - allocate only coordinates
		integer							:: stat
		character*128				:: message

		! Error check
		if (np < 0) then
			write (message, "(a,i0)") "Invalid array size: ", np
			call error (message, NAME="gro%frameArray%allocate()")
		end if

		! Deallocate memory
		call this%deallocate()

		! number of points
		this%natoms = np

		! Allocate
		if (present(onlyCoor)) then
			allocate( this%coor(3,np), STAT=stat)

		else
			allocate( this%res_num(np), this%res_name(np), this%atom_name(np), &
					this%atom_num(np), this%coor(3,np), this%vel(3,np), STAT=stat)

		end if

		! Initialize array
		if (present(initialize)) call this%initialize()

		return
	end subroutine allocate_gro_frame

	! Deallocate and allocate .GRO file.
	subroutine deallocate_gro_frame (this)
		class(gro_frame)	:: this	! .GRO data file
		integer						:: stat

		this%natoms = 0
		stat = 0

		if (allocated(this%coor)) deallocate(this%coor, STAT=stat)
		if (allocated(this%res_num)) deallocate( this%res_num, this%res_name, this%atom_name, &
		  																		 & this%atom_num, this%coor, this%vel, STAT=stat)

		return
	end subroutine deallocate_gro_frame

	! Initialize (format) .GRO file
	subroutine initialize_gro_frame (this)
		implicit none
		class(gro_frame)	:: this	! .GRO data
		integer						:: i, stat	! 0 = ok, 1 = memory not allocated

		! Initialize
		stat = 0

		! Header
		this%title	= ""
		this%time 	= 0.0
		this%box(:)	= 0.0
		this%natoms = 0

		if (allocated(this%coor)) then
			this%natoms = size(this%coor, DIM=2)
			this%coor(:,:) = 0.

			! Allocatable data
			if (allocated(this%res_num)) then
				this%res_name(:) 	= ""
				this%res_num(:)		= 1
				this%atom_num(:) 	= [(i, i = 1, this%natoms)]
				this%atom_name(:) 	= ""

				if (allocated(this%vel)) this%vel(:,:) = 0.

			end if
		end if

		return
	end subroutine initialize_gro_frame

	! =======================================================

	! Open .gro file
	subroutine open_gro (this, file)
		implicit none
		class(gro_file)						:: this
		character*(*), intent(in)	:: file
		! Internal
		integer										:: i, np, stat

		! Open file
		open (NEWUNIT=this%unit, FILE=trim(file), STATUS="old", IOSTAT=stat)
		if (stat /= 0) call error ("Cannot open file: '"//trim(file)//"'", NAME="gro%open()")

		! Count number of frames as occurance of END until EOF
		this%nframes = 0
		do while (stat == 0)
			read (this%unit, *, END=100) ! Title
			read (this%unit, *, END=100) np	 ! number of particles
			! If we go this far another frame must exist
			this%nframes = this%nframes+1
			do i = 1, np+1 ! + last line is box size
				read (this%unit, *, END=200) ! dummy read
			end do

		end do

		! Rewind the file
		100 rewind (this%unit)

		! All frames are left
		this%framesLeft = this%nframes

		return
		200 call error ("Incorect number of lines.", NAME="gro%open()")
	end subroutine open_gro

	! close .gro file
	subroutine close_gro (this)
		implicit none
		class(gro_file)	:: this
		integer					:: stat
		close (this%unit, IOSTAT=stat)
		return
	end subroutine close_gro

	! Allocate frameArray of GRO object file
	subroutine allocate_gro (this, np)
		implicit none
		class(gro_file)	:: this
		integer					:: np
		integer 				:: stat
		character*128		:: message

		if (np < 0) then
			write (message, "(a,i0)") "Invalid array size: ",np
			call error (message, NAME="gro%allocate()")
		end if

		! Allocate memory
		if (allocated(this%frameArray)) deallocate(this%frameArray, STAT=stat)
		allocate (this%frameArray(np), STAT=stat)
		if (stat /= 0) call error ("Not enough memory.", NAME="gro%allocate()")

		! Intitialize basic data
		this%frameArray(:)%natoms = 0

		return
	end subroutine allocate_gro

	! Read multiple frames.
	function read_next_gro (this, nframes, onlycoor) result (framesRead)
		implicit none
		class(gro_file)								:: this		! .GRO data file
		integer, intent(in), optional	:: nframes	! how many frames to read
		class(*), optional						:: onlycoor	! if present read only coordinates
		integer												:: framesRead
		integer												:: i, n, np, ok, stat, line
		logical												:: opened, coor
		character*128									:: message

		! If number of frames in not defined read only one frame
		np = 1
		if (present(nframes)) np = nframes

		! Check if opened
		inquire (UNIT=this%unit, OPENED=opened)
		if (.not. opened) call error ("No file opened.", NAME="gro%read_next()")

		! How many frames are left?
		if (np > this%framesLeft) then
			framesRead = this%framesLeft
		else
			framesRead = np
		end if

		! Allocate memory
		call this%allocate(framesRead)

		! Main read loop
		do n = 1, framesRead
			! One less frame to read
			this%framesLeft = this%framesLeft-1

			! Title
			read (this%unit, "(a)", IOSTAT=stat, END=110) this%frameArray(n)%title
			line = line+1

			! Read time if present
			i = index(this%frameArray(n)%title, "t=")
			if (i /= 0) then
				read (this%frameArray(n)%title(i+2:), *, IOSTAT=stat) this%frameArray(n)%time
				if (stat /= 0) this%frameArray(n)%time = 0.
			end if

			! Number of points
			read (this%unit, *, ERR=120, END=110) this%frameArray(n)%natoms
			line = line+1

			! Allocate data
			if (present(onlycoor)) then
				call this%frameArray(n)%allocate(this%frameArray(n)%natoms, ONLYCOOR=.true.)
				coor=.true.
			else
				call this%frameArray(n)%allocate(this%frameArray(n)%natoms)
				coor=.false.
			end if

			! Read data
			if (coor) then
				! Read only coordinates
				do i = 1, this%frameArray(n)%natoms
					read (this%unit, 200, ERR=120, END=110)	this%frameArray(n)%coor(:,i)
					line = line+1

					200 format (20x,3f8.3)

				end do
			else
				do i = 1, this%frameArray(n)%natoms
					read (this%unit, 210, ERR=120, END=110)	&
						this%frameArray(n)%res_num(i), this%frameArray(n)%res_name(i), this%frameArray(n)%atom_name(i), 	&
						this%frameArray(n)%atom_num(i), this%frameArray(n)%coor(:,i), this%frameArray(n)%vel(:,i)
					line = line+1

					210 format (i5,2a5,i5,3f8.3:6f8.4)

				end do
			end if

			! Box side
			read (this%unit, *, ERR=110) this%frameArray(n)%box(1:3)
			line = line+1

		end do

		return
		110 call error ("Unexpected EoF.", NAME="gro%read_next()")
		120 write (message,"(2(a,i0))") "Could not phrase line: ", line, " in frame: ", this%nframes-this%framesLeft
		call error (message, NAME="gro%read_next()")
	end function read_next_gro

	! Read entire .GRO file
	subroutine read_gro (this, file)
		implicit none
		class(gro_file)	:: this
		character*(*)		:: file
		integer					:: stat

		call this%open(file)
		stat = this%read_next(this%nframes)
		call this%close

		return
	end subroutine read_gro

	! Write single .GRO frame to UNIT or FILE.
	subroutine write_gro (this, unit, file)
		use iso_fortran_env
		implicit none
		class(gro_file)					:: this		! .GRO data file
		integer, optional				:: unit		! File UNIT id
		character*(*), optional	:: file
		integer									:: i, n, u, stat
		logical									:: opened, vel

		! Check if fully allocated
		if (.NOT. allocated(this%frameArray)) call error ("Data not allocated.", NAME="gro%write()")

		! Choose output method
		if (present(unit)) then
			! Check if unit is opened
			inquire (UNIT=unit, OPENED=opened)
			if (.not. opened) call error ("File unit not assigned.", NAME="gro%write()")
			u = unit

		else if (present(file)) then
			open (NEWUNIT=u, FILE=trim(file), STATUS="unknown", IOSTAT=stat)
			if (stat /= 0) call error ("Cannot open file: '"//trim(file)//"'", NAME="gro%write()")

		else
			u = output_unit ! stdout

		end if

		! ----------------------------------------------------------------------------

		! Write frame by frame
		do n = 1, size(this%frameArray)
			! Error check
			if (.NOT. allocated(this%frameArray(n)%coor)) call error ("Data not allocated.", NAME="gro%write()")
			if (.NOT. allocated(this%frameArray(n)%res_name)) call error ("Cannot write 'COORONLY' data.", NAME="gro%write()")

			! Does data include viscosities and are those allocated?
			vel=.false.
      if (allocated(this%frameArray(n)%vel)) then
				if (any(this%frameArray(n)%vel(:,:) /= 0.)) vel=.true.

			end if

			! GRO title
			write (u, "(a)") trim(this%frameArray(n)%title)
			! Number of points
			write (u, "(2x,i0)") this%frameArray(n)%natoms
			! Write data atom by atom
			do i = 1, this%frameArray(n)%natoms
				write (u, 200) this%frameArray(n)%res_num(i), adjustl(this%frameArray(n)%res_name(i)),	&
				adjustr(trim(this%frameArray(n)%atom_name(i))), this%frameArray(n)%atom_num(i), this%frameArray(n)%coor(:,i)

				200 format (i5,2a5,i5,3f8.3,$) ! $ = "AVANCE=NO"

				! Write velocity only if not equal to 0
				if (vel) write (u, "(3f8.3,$)") this%frameArray(n)%vel(:,i)
				! New line
				write (u, *) ""

			end do

			! Box side
			write (u, "(3(x,f9.5))") this%frameArray(n)%box(1:3)

		end do

		return
	end subroutine write_gro

	! Return Box size from file
	function box_gro (this) result (box)
		implicit none
		class(gro_file)	:: this
		real						:: box(3)
		logical					:: opened
		integer					:: i, np

		! Return ERROR if file is not opened
		inquire (UNIT=this%unit, OPENED=opened)
			if (.not. opened) call error ("File unit not assigned.", NAME="gro%box()")

		! If some data was already read return current first fame
		if (allocated(this%frameArray)) then
			box = this%frameArray(1)%box

		else ! read the file and find box
			read (this%unit, *) ! Title
			read (this%unit, *) np	 ! number of particles

			do i = 1, np
				read (this%unit, *) ! Atom coordinates
			end do

			! Read the box size
			read (this%unit, *) box

			rewind (this%unit)

		end if

		return
	end function box_gro

	! Return Number of atoms present in file
	function natoms_gro (this) result (natoms)
		implicit none
		class(gro_file)	:: this
		integer					:: natoms
		logical					:: opened
		integer					:: i, np

		! Return ERROR if file is not opened
		inquire (UNIT=this%unit, OPENED=opened)
		if (.not. opened) call error ("No file is assigned to object.", NAME="gro%natoms()")

		! If some data was already read return current first fame
		if (allocated(this%frameArray)) then
			natoms = this%frameArray(1)%natoms

		else ! read the file and find box entry
			read (this%unit, *) ! Title
			read (this%unit, *) natoms

			rewind (this%unit)

		end if

		return
	end function natoms_gro

	! ! Re-numbers .GRO format that is out of sequence
	! subroutine renumber_gro (this)
	! 	implicit none
	! 	class(gro_file)	:: this
	! 	integer					:: i, n, atom_cnt, res_cnt, compare
	!
	! 	do n = 1, size(this%frameArray(:))
	! 		! Initialize
	! 		atom_cnt = 1
	! 		res_cnt = 1
	!
	! 		! Create initial compare from first entry
	! 		compare = this%frameArray(n)%res_num(1)
	!
	! 		do i = 1, this%frameArray(n)%natoms
	! 			! Renumber residue
	! 			if (this%frameArray(n)%res_num(i) == compare) then
	! 				! If same resnum as previous atom
	! 				this%frameArray(n)%res_num(i) = res_cnt
	!
	! 			else
	! 				! If not from same residue as previous atoms, move counter
	! 				compare = this%frameArray(n)%res_num(i)
	! 				res_cnt = res_cnt+1
	! 				if (res_cnt >= int(1e5)) res_cnt = 1 ! Counter limit
	!
	! 				this%frameArray(n)%res_num(i) = res_cnt
	!
	! 			end if
	!
	! 			! Renumber atoms
	! 			this%frameArray(n)%atom_num(i) = atom_cnt
	! 			atom_cnt = atom_cnt+1
	! 			if (atom_cnt >= int(1e5)) atom_cnt = 1	! Counter limit
	!
	! 		end do
	! 	end do
	!
	! 	return
	! end subroutine renumber_gro

	! Append GRO data file to existing GRO data
	! subroutine add_gro (gro, add, err, msg)
	! 	implicit none
	! 	class(gro_file)							:: gro
	! 	type(gro_file), intent(in)				:: add
	! 	integer, optional, intent(out)			:: err
	! 	character*(*), optional, intent(out)	:: msg
	! 	! Local
	! 	integer			:: i, n, newSize
	! 	type(gro_file)	:: temp
	!
	! 	! Check if allocated
	! 	if (.not. allocated(gro%res_num)) then
	! 		! Fatal
	! 		if (present(err)) err = 1
	! 		if (present(msg)) msg = "Input not allocated."
	! 	end if
	!
	! 	! Check if allocated
	! 	if (.not. allocated(add%res_num)) then
	! 		! Fatal
	! 		if (present(err)) err = 1
	! 		if (present(msg)) msg = "Input not allocated."
	! 	end if
	!
	! 	! Temporary memory
	! 	newSize = gro%natoms+add%natoms
	! 	call temp%allocate(newSize)
	!
	! 	! Adjust number of points
	! 	temp%natoms = newSize
	!
	! 	! Copy header data
	! 	! temp%title 	= gro%title
	! 	! temp%time	= gro%time
	! 	! temp%box 	= gro%box(:)
	!
	! 	! Transcribe data
	! 	n = gro%natoms
	!
	! 	! 1. copy all original GRO data
	! 	temp%coor(:,1:n) 	= gro%coor(:,:)
	! 	temp%vel(:,1:n) 	= gro%vel(:,:)
	! 	temp%res_name(1:n)	= gro%res_name(:)
	! 	temp%res_num(1:n)	= gro%res_num(:)
	! 	temp%atom_name(1:n)	= gro%atom_name(:)
	! 	temp%atom_num(1:n)	= gro%atom_num(:)
	!
	! 	! 2. Copy all from ADD
	! 	temp%coor(:,n+1:) 		= add%coor(:,:)
	! 	temp%vel(:,n+1:) 		= add%vel(:,:)
	! 	temp%res_name(n+1:)		= add%res_name(:)
	! 	temp%atom_name(n+1:)	= add%atom_name(:)
	! 	! Offset numbers
	! 	temp%atom_num(n+1:)		= add%atom_num(:)+temp%atom_num(n)
	! 	temp%res_num(n+1:)		= add%res_num(:)+temp%res_num(n)
	!
	! 	! Reallocate GRO
	! 	call gro%allocate(newSize)
	!
	! 	! Adjust number of points
	! 	gro%natoms = newSize
	!
	! 	! Copy back
	! 	gro%coor(:,:) 		= temp%coor(:,:)
	! 	gro%vel(:,:) 		= temp%vel(:,:)
	! 	gro%res_name(:) 	= temp%res_name(:)
	! 	gro%res_num(:)	 	= temp%res_num(:)
	! 	gro%atom_name(:) 	= temp%atom_name(:)
	! 	gro%atom_num(:) 	= temp%atom_num(:)
	!
	! 	! Cleanup
	! 	call temp%allocate(0)
	!
	! 	return
	! end subroutine add_gro

end module xslib_gro
