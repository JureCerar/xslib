module xslib_frame
	use xslib_common
	use xslib_utilities, only: extension, toLower
	use xslib_gro, only: gro_file
	use xslib_pdb, only: pdb_file
	use xslib_xyz, only: xyz_file
	use xslib_trj, only: trj_file
	implicit none
	private
	public	:: frame_file

	type frame_file
		integer				:: natoms
		real				:: box(3)
		real, allocatable	:: coor(:,:)
	contains
		procedure			:: open => open_frame
		procedure			:: read_next => read_next_frame
		procedure			:: close => close_frame
		procedure			:: nframes => nframes_frame
		procedure			:: get_natoms => get_natoms_frame
		procedure			:: get_box => get_box_frame
	end type frame_file

	! Hidden information
	character*3		:: fileType=""
	logical			:: opened=.false.
	integer			:: nframes=0

	! Private data handlers
	character*3		:: validTypes(5)=["gro", "pdb", "xyz", "xtc", "trr"]
	type(gro_file)	:: gro
	type(pdb_file)	:: pdb
	type(xyz_file)	:: xyz
	type(trj_file)	:: trj

contains
	! NOTE: ALL of this can be simplified by using single POLYMORPHIC variable,
	! but due to legacy reasons we are sticking to this code

	! Open file.
	subroutine open_frame (this, file)
		implicit none
		class(frame_file)			:: this
		character*(*), intent(in)	:: file
		logical						:: exist

		! Check if file exists
		inquire (FILE=trim(file), EXIST=exist)
			if (.not. exist) call error ("Cannot find file: '"//trim(file)//"'")

		! Check if supported file type
		! NOTE: Very crude implementation
		fileType = toLower(extension(trim(file)))
		if (.not. any(validTypes(:)==fileType)) &
		& call error ("Unrecognionized file type: '"//trim(fileType)//"'", NAME="frame%open()")

		! Open file
		select case (fileType)
		case("gro")
			call gro%open(trim(file))
			nframes = gro%nframes

		case("pdb")
			call pdb%open(trim(file))
			nframes = pdb%nframes

		case("xyz")
			call xyz%open(trim(file))
			nframes = xyz%nframes

		case("xtc","trr")
			call trj%open(trim(file))
			nframes = trj%nframes

		end select

		! File is now oppened
		opened=.true.

		return
	end subroutine open_frame

	! Read one frame from already opened file.
	function read_next_frame (this) result (stat)
		! use mylib, only : extension
		implicit none
		class(frame_file)		:: this
		! class(*), allocatable	:: object
		integer 				:: stat, ok
		integer					:: i, j

		! Check if any files are opened
		if (.not. opened) call error ("No opened file.", NAME="frame%read_next()")

		! Read file
	 	select case (fileType)
		case ("gro")
			stat = gro%read_next(ONLYCOOR=.true.)
			if (stat==1) then
				! Num of atoms
				this%natoms = gro%frameArray(1)%natoms
				! Simulation box
				this%box(:) = gro%frameArray(1)%box(:)
				! Allocate data only if not allocated or array is different size
				if (allocated(this%coor) .and. size(this%coor,2)==gro%frameArray(1)%natoms) then ! Only copy data
					this%coor = gro%frameArray(1)%coor(:,:)

				else
					if (allocated(this%coor)) deallocate(this%coor, STAT=ok)
					allocate(this%coor(3,this%natoms), SOURCE=gro%frameArray(1)%coor(:,:), STAT=ok)
					if (ok/=0) call error ("Not enough memory.")

				end if
			end if

		case ("pdb")
			stat = pdb%read_next(ONLYCOOR=.true.)
			if (stat == 1) then
				this%natoms = pdb%frameArray(1)%natoms
				this%box = pdb%frameArray(1)%box
				if (allocated(this%coor) .and. size(this%coor,2)==pdb%frameArray(1)%natoms) then
					this%coor = pdb%frameArray(1)%coor(:,:)

				else
					if (allocated(this%coor)) deallocate(this%coor, STAT=ok)
					allocate(this%coor(3,this%natoms), SOURCE=pdb%frameArray(1)%coor(:,:), STAT=ok)
					if (ok/=0) call error ("Not enough memory.")

				end if
			end if

		case ("xyz")
			stat = xyz%read_next()
			if (stat == 0) then
				this%natoms = xyz%frameArray(1)%natoms
				this%box(:) = xyz%frameArray(1)%box(:) ! FUN FACT: xyz does not contain box info
				if (allocated(this%coor) .and. size(this%coor,2)==xyz%frameArray(1)%natoms) then
					this%coor = xyz%frameArray(1)%coor(:,:)

				else
					if (allocated(this%coor)) deallocate(this%coor, STAT=ok)
					allocate(this%coor(3,this%natoms), SOURCE=xyz%frameArray(1)%coor(:,:), STAT=ok)
					if (ok/=0) call error ("Not enough memory.")

				end if
			end if

		case ("xtc","trr")
			stat = trj%read_next()
			if (stat == 1) then
				this%natoms = trj%numatoms
				! In .trr/.xtc box is 3x3 matrix; Extract only diagonal.
				this%box(:) = [trj%frameArray(1)%box(1,1), trj%frameArray(1)%box(2,2), trj%frameArray(1)%box(3,3)]
				if (allocated(this%coor) .and. size(this%coor,2)==trj%numatoms) then
					this%coor = trj%frameArray(1)%coor(:,:)

				else
					if (allocated(this%coor)) deallocate(this%coor, STAT=ok)
					! For som F***ing reason ifort returns allocated() TRUE when array is CLEARLY deallocated; hence STAT=ok.
					allocate(this%coor(3,this%natoms), SOURCE=trj%frameArray(1)%coor(:,:), STAT=ok)
					if (ok/=0) call error ("Not enough memory.")

				end if
			end if

		end select

		return
	end function read_next_frame

	subroutine close_frame (this)
		implicit none
		class(frame_file)	:: this

		! Close file and memory clean-up
		if (opened) then
			select case (fileType)
			case ("gro")
				call gro%close()
				! call gro%deallocate()

			case ("pdb")
				call pdb%close()
				! call pdb%deallocate()

			case ("xyz")
				call xyz%close()
				! call xyz%deallocate()

			case ("xtc","trr")
				call trj%close()
				! deallocate(trj%frameArray(), STAT=stat)

			end select
		end if

		! Reset parameters
		nframes=0
		fileType=""
		opened=.false.

		return
	end subroutine close_frame

	! Returns the number of frames in file.
	function nframes_frame (this) result (stat)
		implicit none
		class(frame_file)	:: this
		integer				:: stat

		stat = nframes

		return
	end	function nframes_frame

	! Gets number of atoms in current frame (without changing position in file).
	function get_natoms_frame (this) result (natoms)
		implicit none
		class(frame_file)	:: this
		integer				:: natoms

		if (.not. opened) call error ("No opened file.", NAME="frame%get_natoms()")

		select case (fileType)
		case("gro")
			natoms = gro%natoms()

		case("pdb")
			natoms = pdb%natoms()

		case("xyz")
			natoms = xyz%natoms()

		case("xtc","trr")
			! trj%box() returns 3x3 matix; Off-diagonal elements are eq to 0.
			natoms = trj%natoms()

		end select

		! Just in case write it down
		this%natoms = natoms

		return
	end function get_natoms_frame

	! Get box size of current frame (without changing position in file).
	function get_Box_frame (this) result (box)
		implicit none
		class(frame_file)	:: this
		real				:: box(3)

		if (.not. opened) call error ("No opened file.", NAME="frame%read_next()")

		select case (fileType)
		case("gro")
			box = gro%box()

		case("pdb")
			box = pdb%box()
			! Not all .pdb files contain box size
			if (all(box(:)==-1.)) call warning (".pdb file does not contain box size.")

		case("xyz")
			! FUN FACT: .xyz file does not contain box info
			box = xyz%box()
			! %box() procedure already prompts warning.
			! call warning (".xyz file does not contain box size.")

		case("xtc","trr")
			! trj%box() returns 3x3 matix; Off-diagonal elements are eq to 0.
			box = pack(trj%box(1), MASK=trj%box(1)/=0.)

		end select

		! Just in case write it down
		this%box = box

		return
	end function get_Box_frame

end module xslib_frame
