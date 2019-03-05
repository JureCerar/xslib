module xslib_frame
	use xslib_common
	use xslib_utilities, only: extension, toLower
	use xslib_gro, only: gro_file
	use xslib_pdb, only: pdb_file
	use xslib_xyz, only: xyz_file
	use xslib_trj, only: trj_file
	implicit none
	private
	public :: frame_file

	type frame_file
		integer							:: natoms
		real								:: box(3)
		real, allocatable		:: coor(:,:)
	contains
		procedure :: open => open_frame
		procedure :: read_next => read_next_frame
		procedure :: close => close_frame
		procedure	:: nframes => nframes_frame
		procedure :: get_natoms => get_natoms_frame
		procedure :: get_box => get_box_frame
	end type frame_file

	! Hidden information
	logical								:: frame_opened=.false.
	integer								:: frame_nframes=0
	class(*), allocatable	:: frame

contains

	! Open file.
	subroutine open_frame (this, file)
		implicit none
		class(frame_file)					:: this
		character*(*), intent(in)	:: file
		logical										:: exist
		character*3								:: type

		! Check if file exists
		inquire (FILE=trim(file), EXIST=exist)
		if (.NOT. exist) call error ("Cannot find file: '"//trim(file)//"'")

		! Check if supported file type
		type = toLower(extension(file))

		! Allocate polymorphic varible
		select case (trim(type))
		case("gro")
			allocate(gro_file :: frame)

		case("pdb")
				allocate(pdb_file :: frame)

		case("xyz")
			allocate(pdb_file :: frame)

		case("xtc","trr")
			allocate(trj_file :: frame)

		case default
			call error ("Unrecognionized file type: '."//trim(type)//"'", NAME="frame%open()")

		end select

		! Allocate Open file and read number of frames
		select type (frame)
		class is (gro_file)
			call frame%open(file)
			frame_nframes = frame%nframes

		class is (pdb_file)
			call frame%open(file)
			frame_nframes = frame%nframes

		class is (gro_file)
			call frame%open(file)
			frame_nframes = frame%nframes

		class is (xyz_file)
			call frame%open(file)
			frame_nframes = frame%nframes

		class is (trj_file)
			call frame%open(file)
			frame_nframes = frame%nframes

		end select

		! Mark as opened
		frame_opened=.true.


		return
	end subroutine open_frame

	! Read one frame from already opened file.
	function read_next_frame (this) result (stat)
		implicit none
		class(frame_file)	:: this
		integer 					:: stat
		integer						:: i, j

		! Check if any files are opened
		if (.NOT. frame_opened) call error ("No opened file.", NAME="frame%read_next()")

	 	select type (frame)
		class is (gro_file)
			stat = frame%read_next(ONLYCOOR=.true.)
			if (stat==1) then
				! Num of atoms
				this%natoms = frame%frameArray(1)%natoms
				! Simulation box
				this%box(:) = frame%frameArray(1)%box(:)
				! Allocate data only if not allocated or array is different size
				if (allocated(this%coor) .and. size(this%coor,2)==frame%frameArray(1)%natoms) then ! Only copy data
					this%coor = frame%frameArray(1)%coor(:,:)

				else
					if (allocated(this%coor)) deallocate(this%coor, STAT=stat)
					allocate(this%coor(3,this%natoms), SOURCE=frame%frameArray(1)%coor(:,:), STAT=stat)
					if (stat/=0) call error ("Not enough memory.", NAME="frame%read_next()")

				end if
			end if

		class is (pdb_file)
			stat = frame%read_next(ONLYCOOR=.true.)
			if (stat==1) then
				this%natoms = frame%frameArray(1)%natoms
				this%box(:) = frame%frameArray(1)%box(:)
				if (allocated(this%coor) .and. size(this%coor,2)==frame%frameArray(1)%natoms) then
					this%coor = frame%frameArray(1)%coor(:,:)
				else
					if (allocated(this%coor)) deallocate(this%coor, STAT=stat)
					allocate(this%coor(3,this%natoms), SOURCE=frame%frameArray(1)%coor(:,:), STAT=stat)
					if (stat/=0) call error ("Not enough memory.", NAME="frame%read_next()")
				end if
			end if

		class is (xyz_file)
			stat = frame%read_next()
			if (stat==1) then
				this%natoms = frame%frameArray(1)%natoms
				this%box(:) = frame%frameArray(1)%box(:)
				if (allocated(this%coor) .and. size(this%coor,2)==frame%frameArray(1)%natoms) then
					this%coor = frame%frameArray(1)%coor(:,:)
				else
					if (allocated(this%coor)) deallocate(this%coor, STAT=stat)
					allocate(this%coor(3,this%natoms), SOURCE=frame%frameArray(1)%coor(:,:), STAT=stat)
					if (stat/=0) call error ("Not enough memory.", NAME="frame%read_next()")
				end if
			end if

		class is (trj_file)
			stat = frame%read_next()
			if (stat == 1) then
				this%natoms = frame%numatoms
				! In .trr/.xtc box is 3x3 matrix; Extract only diagonal.
				this%box(:) = [frame%frameArray(1)%box(1,1), frame%frameArray(1)%box(2,2), frame%frameArray(1)%box(3,3)]
				if (allocated(this%coor) .AND. size(this%coor,2)==frame%numatoms) then
					this%coor = frame%frameArray(1)%coor(:,:)

				else
					if (allocated(this%coor)) deallocate(this%coor, STAT=stat)
					allocate(this%coor(3,this%natoms), SOURCE=frame%frameArray(1)%coor(:,:), STAT=stat)
					if (stat/=0) call error ("Not enough memory.", NAME="frame%read_next()")

				end if
			end if

		class default
			call error ("Undefined data type", NAME="frame%read_next()")

		end select

		return
	end function read_next_frame

	subroutine close_frame (this)
		implicit none
		class(frame_file)	:: this
		integer						:: stat

		! Close file and memory clean-up
		if (frame_opened) then
			select type (frame)
			class is (gro_file)
				call frame%close()

			class is (pdb_file)
				call frame%close()

			class is (xyz_file)
				call frame%close()

			class is (trj_file)
				call frame%close()

			class default
				call error ("Undefined data type.", NAME="frame%read_next()")

			end select
		end if

		if (allocated(frame)) deallocate(frame, STAT=stat)

		! Reset parameters
		frame_nframes=0
		frame_opened=.false.

		return
	end subroutine close_frame

	! Returns the number of frames in file.
	function nframes_frame (this) result (stat)
		implicit none
		class(frame_file)	:: this
		integer						:: stat
		stat = frame_nframes
		return
	end	function nframes_frame

	! Gets number of atoms in current frame (without changing position in file).
	function get_natoms_frame (this) result (natoms)
		implicit none
		class(frame_file)	:: this
		integer						:: natoms

		if (.NOT. frame_opened) call error ("No opened file.", NAME="frame%get_natoms()")

		select type (frame)
		class is (gro_file)
			natoms = frame%natoms()
		class is (pdb_file)
			natoms = frame%natoms()
		class is (xyz_file)
			natoms = frame%natoms()
		class is (trj_file)
			natoms = frame%natoms()
		class default
			call error ("Undefined data type.", NAME="frame%read_next()")
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

		if (.NOT. frame_opened) call error ("No opened file.", NAME="frame%read_next()")

		select type (frame)
		class is (gro_file)
			box = frame%box()
		class is (pdb_file)
			box = frame%box()
		class is (xyz_file)
			! FUN FACT: .xyz file does not contain box info
			! xyz%box() procedure will prompts warning.
			box = frame%box()
		class is (trj_file)
			box = pack(frame%box(1), MASK=reshape([1,0,0, 0,1,0, 0,0,1]==1, [3,3]))
		class default
			call error ("Undefined data type.", NAME="frame%read_next()")
		end select

		! Just in case write it down
		this%box = box

		return
	end function get_Box_frame

end module xslib_frame
