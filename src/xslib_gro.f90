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
		integer, private							:: unit, first=1, last=-1, stride=1
		integer												:: nframes, allframes, framesLeft
		type(gro_frame), allocatable	:: frameArray(:)
	contains
		procedure	:: open => open_gro
		procedure	:: close => close_gro
		procedure	:: allocate => allocate_gro
		procedure :: next => next_gro
		procedure :: set => setframe_gro
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
		logical, optional		:: onlycoor, initialize	! if NOT present - allocate everything, if present - allocate only coordinates
		logical							:: coor, init
		integer							:: stat
		character*128				:: message

		! Error check
		if (np < 0) then
			write (message, "(a,i0)") "Invalid array size: ", np
			call error (message, NAME="gro%frameArray%allocate()")
		end if

		! Process poptional variables
		coor = merge(onlycoor, .false., present(onlycoor))
		init = merge(initialize, .false., present(initialize))

		! Deallocate memory
		call this%deallocate()

		! number of points
		this%natoms = np

		! Allocate
		if (coor) then
			allocate( this%coor(3,np), STAT=stat)

		else
			allocate( this%res_num(np), this%res_name(np), this%atom_name(np), &
					this%atom_num(np), this%coor(3,np), this%vel(3,np), STAT=stat)

		end if

		! Initialize array
		if (init) call this%initialize()

		return
	end subroutine allocate_gro_frame

	! Deallocate and allocate .GRO file.
	subroutine deallocate_gro_frame (this)
		class(gro_frame)	:: this	! .GRO data file
		integer						:: stat

		this%natoms = 0

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
		this%allframes = 0
		do while (stat==0)
			! Skip frames until EoF.
			stat = this%next()-1
			if (stat==0) this%allframes = this%allframes+1
		end do

		! Rewind the file
		100 rewind (this%unit)

		! All frames are left
		this%nframes = this%allframes
		this%framesLeft = this%nframes

		! Reset first last and stride values
		this%first = 1
		this%last = this%nframes
		this%stride = 1

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

		! Update number of frames
		this%nframes = np

		! Allocate memory
		if (allocated(this%frameArray)) deallocate(this%frameArray, STAT=stat)
		allocate (this%frameArray(this%nframes), STAT=stat)
		if (stat /= 0) call error ("Not enough memory.", NAME="gro%allocate()")

		! Intitialize basic data
		this%frameArray(:)%natoms = 0

		return
	end subroutine allocate_gro

	! Skip "stride" number of frames. returning number si the number of frames skipped.
	function next_gro (this, nframes) result (stat)
		implicit none
		class(gro_file)		:: this
		integer, optional	:: nframes
		integer						:: i, ip, n, np, stat

		! Default status
		stat = 0

		! Number of frames to skip
		np = merge(nframes, 1, present(nframes))

		do n = 1, np, 1
			read (this%unit, *, END=100) ! Title
			read (this%unit, *, END=100) ip	 ! number of particles
			! If we go this far another frame must exist
			do i = 1, ip+1 ! + last line is box size
				read (this%unit, *, END=200) ! dummy read
			end do
			! At this point frame skip was succesfull
			stat = stat+1
		end do

		100 continue

		return
		200 call error ("Incorrect number of lines?", NAME="gro%next()")
	end function next_gro

	! Set first/last frame and add frame stride.
	subroutine setframe_gro (this, first, last, stride)
		implicit none
		class(gro_file)		:: this
		integer, optional	:: first, last, stride
		integer						:: stat
		logical 					:: opened

		! Check if file is opened
		inquire (UNIT=this%unit, OPENED=opened)
		if (.NOT. opened) call error ("File UNIT not assigned.", NAME="gro%set()")

		! Reset file
		rewind (UNIT=this%unit)

		! Change global variables
		this%first = merge(first, 1, present(first))
		this%last = merge(last, -1, present(last))
		this%stride = merge(stride, 1, present(stride))

		! IF last is "-1" select all
		if (this%last==-1) this%last = this%allframes

		! Error check for first and last
		if (this%first < 1) then
			call warning ("Invalid first frame: '"//str(this%first)//"'. Setting to '1'.")
			this%first = 1
		end if

		if (this%last > this%allframes) then
			call warning ("Invalid last frame: '"//str(this%last)//"'. Setting to '"//str(this%allframes)//"'.")
			this%last = this%allframes
		end if

		if (this%stride == 0) then
			call warning ("Invalid frame stride: '"//str(this%stride)//"'. Setting to '1'.")
			this%stride = 1
		end if

		! ---------------------------------

		! Modify number of frames and number of frames left
		this%nframes = ceiling((this%last-this%first+1)/real(this%stride))
		this%framesleft = this%nframes

		if (this%first/=1) then
			! Cycle through frames to the "first" frame
			stat = this%next(this%first-1)
		end if

		return
	end subroutine setframe_gro

	! Read multiple frames.
	function read_next_gro (this, nframes, onlycoor) result (framesRead)
		implicit none
		class(gro_file)								:: this		! .GRO data file
		integer, intent(in), optional	:: nframes	! how many frames to read
		logical, optional							:: onlycoor	! if present read only coordinates
		integer												:: framesRead
		integer												:: i, n, np, ok, stat, line
		logical												:: opened, coor
		character*128									:: message

		! Check if opened
		inquire (UNIT=this%unit, OPENED=opened)
		if (.not. opened) call error ("No file opened.", NAME="gro%read_next()")

		! If number of frames in not defined read only one frame
		np = merge(nframes, 1, present(nframes))

		! Read only coordinates ?
		coor = merge(onlycoor, .false., present(onlycoor))

		! How many frames are left? (select lower value)
		framesRead = min(this%framesLeft, np)

		! Allocate memory
		call this%allocate(framesRead)

		! Main read loop
		do n = 1, framesRead
			! One less frame to read
			this%framesLeft = this%framesLeft-1

			! Line counter
			line = 0

			! Title
			read (this%unit, "(a)", IOSTAT=stat, END=110) this%frameArray(n)%title
			line = line+1

			! Read time if present
			i = index(this%frameArray(n)%title, "t=")
			if (i/=0) then
				read (this%frameArray(n)%title(i+2:), *, IOSTAT=stat) this%frameArray(n)%time
				if (stat/=0) this%frameArray(n)%time = 0.
			end if

			! Number of points
			read (this%unit, *, ERR=120, END=110) this%frameArray(n)%natoms
			line = line+1

			! Allocate data
			call this%frameArray(n)%allocate(this%frameArray(n)%natoms, ONLYCOOR=coor)

			! Read data
			if (coor) then
				! Read only coordinates
				do i = 1, this%frameArray(n)%natoms
					read (this%unit, "(20x,3f8.3)", ERR=120, END=110)	this%frameArray(n)%coor(:,i)
					line = line+1

				end do
			else
				do i = 1, this%frameArray(n)%natoms
					read (this%unit, "(i5,2a5,i5,3f8.3:6f8.4)", ERR=120, END=110)	&
						this%frameArray(n)%res_num(i), this%frameArray(n)%res_name(i), this%frameArray(n)%atom_name(i), 	&
						this%frameArray(n)%atom_num(i), this%frameArray(n)%coor(:,i), this%frameArray(n)%vel(:,i)
					line = line+1

				end do
			end if

			! Box side
			read (this%unit, *, ERR=110) this%frameArray(n)%box(:)
			line = line+1

			! Skip specified number of frames
			stat = this%next(this%stride-1)

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
		integer									:: i, n, u, cnt, stat
		logical									:: opened, vel, onlycoor

		! Check if data is allocated
		if (.NOT. allocated(this%frameArray)) call error ("Data not allocated.", NAME="gro%write()")

		! Choose output method
		if (present(unit)) then
			inquire (UNIT=unit, OPENED=opened)
			if (.NOT. opened) call error ("File UNIT not assigned.", NAME="gro%write()")
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

			! Does data include viscosities and are those allocated?
			vel=.false.
			if (allocated(this%frameArray(n)%vel)) vel = .NOT. all(this%frameArray(n)%vel==0.)

			! Is all data present or just coordinates?
			onlycoor = .NOT. allocated(this%frameArray(n)%res_name)

			if (onlycoor) then
				! GRO title
				write (u, "(a)") trim(this%frameArray(n)%title)
				! Number of points
				write (u, "(2x,i0)") this%frameArray(n)%natoms
				! Write data atom by atom
				cnt = 1
				do i = 1, this%frameArray(n)%natoms
					! Fill in the missing data
					write (u, "(i5,2a5,i5,3f8.3)") cnt, "LIG  ", "    X", cnt, this%frameArray(n)%coor(:,i)
					cnt = cnt+1
					if (cnt>int(1E5)) cnt = 1
				end do

			else
				write (u, "(a)") trim(this%frameArray(n)%title)
				write (u, "(2x,i0)") this%frameArray(n)%natoms
				do i = 1, this%frameArray(n)%natoms
					write (u, "(i5,2a5,i5,3f8.3,$)") this%frameArray(n)%res_num(i), adjustl(this%frameArray(n)%res_name(i)),	&
					adjustr(trim(this%frameArray(n)%atom_name(i))), this%frameArray(n)%atom_num(i), this%frameArray(n)%coor(:,i)
					! Write velocity only if not equal to 0
					if (vel) write (u, "(3f8.3,$)") this%frameArray(n)%vel(:,i)
					! New line
					write (u, *) ""

				end do
			end if

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
		integer					:: n, np

		! Return ERROR if file is not opened
		inquire (UNIT=this%unit, OPENED=opened)
		if (.NOT. opened) call error ("File UNIT not assigned.", NAME="gro%box()")

		! If some data was already read return current first fame
		if (allocated(this%frameArray)) then
			box(:) = this%frameArray(1)%box(:)

		else ! read the file and find box
			read (this%unit, *) ! Title
			read (this%unit, *) np	 ! number of particles
			do n = 1, np
				read (this%unit, *) ! Atom coordinates
			end do
			! Read the box size
			read (this%unit, *) box(:)
			! Reset position
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
		if (.NOT. opened) call error ("No file is assigned to object.", NAME="gro%natoms()")

		! If some data was already read return current first fame
		if (allocated(this%frameArray)) then
			natoms = this%frameArray(1)%natoms

		else ! read the file and find box entry
			read (this%unit, *) ! Title
			read (this%unit, *) natoms
			! Reset position
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
