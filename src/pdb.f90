module xslib_pdb
	use xslib_common
	implicit none
	private
	public :: pdb_file

	! .PDB data format
	type pdb_frame
		real 						:: box(3)
		integer						:: natoms
		character*20, allocatable	:: record_type(:), atom_name(:), alt_loc_indicator(:), residue_name(:), chain_identifier(:)
		character*20, allocatable	:: res_insert_code(:), segment_identifier(:), element_symbol(:)
		integer, allocatable		:: atom_serial_number(:), residue_sequence_number(:)
		real, allocatable			:: coor(:,:) ! (index, x:y:z)
		real, allocatable			:: occupancy(:), temp_factor(:), charge(:)
	contains
		procedure	:: allocate => allocate_pdb_frame
		procedure	:: deallocate => deallocate_pdb_frame
		procedure	:: initialize => initialize_pdb_frame
	end type pdb_frame


	type pdb_file
		integer, private				:: unit, framesLeft
		integer							:: nframes
		type(pdb_frame), allocatable	:: frameArray(:)
	contains
		procedure 	:: open => open_pdb
		procedure 	:: close => close_pdb
		procedure	:: allocate => allocate_pdb
		procedure	:: read_next => read_next_pdb
		procedure	:: read => read_pdb
		procedure	:: box => box_pdb
		procedure	:: natoms => natoms_pdb
		procedure	:: write => write_pdb
	end type pdb_file

contains

	! Deallocate and allocate .PDB file.
	subroutine allocate_pdb_frame (this, np, onlycoor, initialize)
		implicit none
		class(pdb_frame)	:: this	! .GRO data file
		integer, intent(in)	:: np	! Number of points to allocate
		class(*), optional	:: onlycoor, initialize	! if NOT present - allocate everything, if present - allocate only coordinates
		integer				:: stat
		character*128		:: message

		! Error check
		if (np < 0) then
			write (message, "(a,i0)") "Invalid array size: ", np
			call error(message, NAME="pdh%allocate()")
		end if

		! Deallocate
		call this%deallocate()
		this%natoms = np

		! Allocate only coordinates
		if (present(onlycoor)) then
			allocate (this%coor(3,np), STAT=stat)

		! Allocate entire .PDB memory
		else
			allocate (this%record_type(np), this%atom_serial_number(np), this%atom_name(np),		&
			&	this%alt_loc_indicator(np), this%residue_name(np), this%chain_identifier(np),		&
			&	this%residue_sequence_number(np), this%res_insert_code(np), this%coor(3,np),		&
			&	this%occupancy(np), this%temp_factor(np), this%segment_identifier(np), this%element_symbol(np), this%charge(np), STAT=stat)

		end if

		! Initialize data
		if (present(initialize)) call this%initialize()

		return
	end subroutine allocate_pdb_frame

	! deallocate .pdb data.
	subroutine deallocate_pdb_frame (this)
		implicit none
		class(pdb_frame)	:: this	! .GRO data file
		integer				:: stat

		! number of particles
		this%natoms = 0

		! First Coor
		if (allocated(this%coor)) deallocate(this%coor, STAT=stat)

		! Remaining
		if (allocated(this%record_type)) &
		deallocate (this%record_type, this%atom_serial_number, this%atom_name,			&
		&	this%alt_loc_indicator, this%residue_name, this%chain_identifier,			&
		&	this%residue_sequence_number, this%res_insert_code, this%occupancy,			&
		&	 this%temp_factor, this%segment_identifier, this%element_symbol, this%charge, STAT=stat)

		return
	end subroutine deallocate_pdb_frame

	! Initialize .PDB file
	subroutine  initialize_pdb_frame (this)
		implicit none
		class(pdb_frame)	:: this
		integer				:: i, stat

		! non-allocatable
		this%natoms = 0
		this%box(:) = 0.

		! If only coordinates are allocated
		if (allocated(this%coor)) then
			this%natoms = size(this%coor(1,:))
			this%coor(:,:) = 0.

		end if

		! Everithing else
		if (allocated(this%record_type)) then
			this%record_type(:) = "HETATM"
			this%atom_name(:) = ""
			this%alt_loc_indicator(:) = ""
			this%residue_name(:) = ""
			this%chain_identifier(:) = ""
			this%res_insert_code(:) = ""
			this%segment_identifier(:) = ""
			this%element_symbol(:) = ""
			this%atom_serial_number(:) = [(i, i = 1, this%natoms)]
			this%residue_sequence_number(:) = 0
			this%occupancy(:) = 0.0
			this%temp_factor(:) = 0.0
			this%charge(:) = 0.0

		end if

		return
	end subroutine  initialize_pdb_frame

	! ========================================================

	! open_pdbfile
	subroutine open_pdb (this, filename)
		implicit none
		class(pdb_file)				:: this
		character*(*), intent(in)	:: filename
		integer						:: stat
		character*4					:: dmy

		! Open file
		open (NEWUNIT=this%unit, FILE=trim(filename), STATUS="unknown", IOSTAT=stat)
		if (stat /= 0) call error("Cannot open file: "//trim(filename), NAME="pdb%open()")

		! Count number of frames as occurance of END until EOF
		this%nframes = 0
		do while (stat == 0)
			read (this%unit, "(a)", END=100) dmy
			if (trim(dmy)=="END") this%nframes = this%nframes+1

		end do

		100 rewind (this%unit)

		! All frames are left
		this%framesLeft = this%nframes

		return
	end subroutine open_pdb

	! close PDB file
	subroutine close_pdb (this)
		implicit none
		class(pdb_file)	:: this
		integer			:: stat
		close(this%unit, IOSTAT=stat)
		return
	end subroutine close_pdb

	! allocate FrameArray
	subroutine allocate_pdb (this, np)
		implicit none
		class(pdb_file)		:: this
		integer				:: np
		character*128		:: message
		integer				:: stat

		! Error check
		if (np < 0) then
			write (message, "(a,i0)") "Invalid array size: ", np
			call error(message, NAME="pdb%frameArray%allocate()")
		end if

		if (allocated(this%frameArray)) deallocate(this%frameArray, STAT=stat)
		allocate (this%frameArray(np), STAT=stat)
		if (stat /= 0) call error("Not enough memory.", NAME="pdb%frameArray%allocate()")

		this%frameArray%natoms = 0

		return
	end subroutine allocate_pdb

	! Read single .PDB frame from UNIT. ONLYCOOR = read only coordinates.
	function read_next_pdb (this, nframes, onlycoor) result (framesRead)
		implicit none
		class(pdb_file)					:: this		! .PDB data file
		integer, intent(in), optional	:: nframes	! how many frames to read
		class(*), optional				:: onlycoor	! if present read only coordinates
		integer							:: framesRead
		character*256					:: buffer, file, message
		character*6						:: dmy
		integer							:: i, n, np, stat, cnt
		logical							:: opened, coor

		! If file unit is esternaly defined
		np = 1
		if (present(nframes)) np = nframes

		! Check if opened
		inquire (UNIT=this%unit, OPENED=opened)
		if (.not. opened) call error("File UNIT not assigned.", NAME="pdb%read_next()")

		! how many frames are left
		if (np > this%framesLeft) then
			framesRead = this%framesLeft
		else
			framesRead = np
		end if

		! Allocate memory
		call this%allocate(framesRead)

		do n = 1, framesRead
			! One less frame to read
			this%framesLeft = this%framesLeft-1

			! Count all atoms are until end of frame
			this%frameArray(n)%natoms = 0
			cnt = 0	! how many lines were read
			stat = 0
			do while (stat==0)
				read (this%unit, *, END=110) dmy
				cnt = cnt+1
				if (index(dmy,"ATOM")/=0 .or. index(dmy,"HETATM")/=0) then
					this%frameArray(n)%natoms = this%frameArray(n)%natoms+1

				else if (trim(dmy)=="END") then
					exit

				end if

			end do

			! Rewind number of lines that were read
			do i = 1, cnt
				backspace (UNIT=this%unit)

			end do

			! If last line account for EoF
			if (this%framesLeft == 0) backspace (UNIT=this%unit)

			! Allocate data
			if (present(onlycoor)) then
				call this%frameArray(n)%allocate(this%frameArray(n)%natoms, ONLYCOOR=.true.)
				coor=.true.
			else
				call this%frameArray(n)%allocate(this%frameArray(n)%natoms)
				coor=.false.
			end if

			! Main read loop
			i = 0
			do while (stat == 0)
				read (this%unit, "(a)",  IOSTAT=stat, END=110) buffer

				! Read dummy from buffer
				read (buffer, "(a)") dmy

				if (index(dmy,"ATOM")/=0 .or. index(dmy,"HETATM")/=0) then

					! line counter
					i = i+1

					! if SWITCH is on, read only coordinates (Computational efficiency)
					if (coor) then
						read (buffer, 210, IOSTAT=stat, ERR=120, END=110) this%frameArray(n)%coor(1:3,i)
						210	format (30x,3f8.3)

					else
						read (buffer, 220, IOSTAT=stat, ERR=120, END=110)	&
							this%frameArray(n)%record_type(i), this%frameArray(n)%atom_serial_number(i), this%frameArray(n)%atom_name(i),			&
							this%frameArray(n)%alt_loc_indicator(i), this%frameArray(n)%residue_name(i), this%frameArray(n)%chain_identifier(i),	&
							this%frameArray(n)%residue_sequence_number(i), this%frameArray(n)%res_insert_code(i), this%frameArray(n)%coor(1:3,i),	&
							this%frameArray(n)%occupancy(i), this%frameArray(n)%temp_factor(i), this%frameArray(n)%segment_identifier(i),			&
						 	this%frameArray(n)%element_symbol(i), this%frameArray(n)%charge(i)

						220	format (a6,i5,x,a4,a1,a3,x,a1,i4,a1,3x,3f8.3,2f6.2,6x,a4,a2,f7.4)

					end if

				! If "END" is found in record, update num. of points and exit
				else if (trim(dmy)=="END") then
					exit

				! Read box dimensions
				else if (index(dmy,"CRYST1")/=0) then
					read (buffer, 200, ERR=120) this%frameArray(n)%box(1:3)
					200	  format (6x,3f9.3)

				end if

			end do ! while stat
		end do ! for n

		return
		! Error handling
		110 call error("Unexpected EoF.", NAME="pdb%read_next()")
		120 write (message,"(2(a,i0))") "Could not phrase line: ", i, " in frame: ", this%nframes-this%framesLeft
		call error(message, NAME="pdb%read_next()")
	end function read_next_pdb

	! Read entire PDB file
	subroutine read_pdb (this, file)
		implicit none
		class(pdb_file)	:: this
		character*(*)	:: file
		integer			:: stat

		call this%open(file)
		stat = this%read_next(this%nframes)
		call this%close()

		return
	end subroutine read_pdb

	! Get box size from PDB file
	! NOTE: returns -1. array if it cannote be found.
	function box_pdb (this) result (box)
		implicit none
		class(pdb_file)	:: this
		real			:: box(3)
		logical			:: opened
		integer			:: stat
		character*128	:: line
		character*6		:: dmy

		! Initialize box to return -1. if it cannote be found
		box = -1.

		! Error check
		inquire (UNIT=this%unit, OPENED=opened)
		if (.not. opened) call error ("No file opened.", NAME="pdb%box()")

		! If data was already read
		if (allocated(this%frameArray)) then
			box = this%frameArray(1)%box

		! Else it must be first time readin (OK to use rewind)
		else
			stat = 0
			do while (stat == 0)
				read (this%unit, "(a)", IOSTAT=stat) line
				read (line, "(a)") dmy

				if (index(dmy,"CRYST1")/=0) then
					read (line, *) dmy, box
					exit
				end if

			end do

			rewind (this%unit)

		end if

		return
	end function box_pdb

	! get number of atoms from PDB file
	function natoms_pdb (this) result (natoms)
		implicit none
		class(pdb_file)	:: this
		integer			:: natoms
		logical			:: opened
		integer			:: stat
		character*6		:: dmy

		! Initialize
		natoms = 0

		! Error check
		inquire (UNIT=this%unit, OPENED=opened)
		if (.not. opened) call error ("File UNIT not assigned.", NAME="pdb%box()")

		! If data was already read
		if (allocated(this%frameArray)) then
		natoms = this%frameArray(1)%natoms

		! Else it must be first time readin (OK to use rewind)
		else
			stat = 0
			do while (stat == 0)
				read (this%unit, "(a)", IOSTAT=stat) dmy

				! Lines containing atom information start with "ATOM" or "HETATOM"
				if (index(dmy,"ATOM")/=0 .or. index(dmy,"HETATM")/=0) then
					natoms = natoms+1

				! Frame ends with either END or ENDMDL entry
				else if (index(dmy,"END")/=0) then
					exit

				end if

			end do

			! Rewind file
			rewind (this%unit)

		end if

		return
	end function natoms_pdb

	! Write single .PDB frame to UNIT or FILE
	! NOTE: Data that is "OnlyCoor" cannot be writen.
	subroutine write_pdb (this, unit, file)
		use iso_fortran_env
		implicit none
		class(pdb_file)				:: this		! .PDB data file
		character*(*), optional		:: file
		integer, optional			:: unit
		integer						:: i, u, n, stat, serial
		logical						:: opened

		! Initialize
		serial = 1 ! serial number of frame

		! Check if fully allocated
		if (.not. allocated(this%frameArray)) call error("Data not allocated.", NAME="pdb%write()")

		! Check if unit is opened
		if (present(unit)) then
			inquire (UNIT=unit, OPENED=opened)
			if (.not. opened) call error("File unit not assigned.", NAME="pdb%write()")
			u = unit

		else if (present(file)) then
			open (NEWUNIT=u, FILE=trim(file), STATUS="unknown", IOSTAT=stat)
			if (stat /= 0) call error("Cannot open file: "//trim(file), NAME="pdb%write()")

		else
			u = output_unit ! stdout

		end if

		! Print all arrays
		do n = 1, size(this%frameArray(:))
			! Error check
			if (.not. allocated(this%frameArray(n)%coor)) call error("Data not allocated.", NAME="pdb%write()")
			if (.not. allocated(this%frameArray(n)%record_type)) call error("Cannot write 'COORONLY' data.", NAME="pdb%write()")

			! Box dimensions
			if (any(this%frameArray(n)%box(:) /= 0.)) then
				write (this%unit, 200) "CRYST1", this%frameArray(n)%box(1:3), 90., 90., 90.
				200 format(a6, 3f9.3, 3f7.2)
			end if

			! Model serial number
			write (this%unit, "(a6,i5)") "MODEL ", serial
			serial = serial+1

			! Coordinate data
			do i = 1, this%frameArray(n)%natoms
				write (this%unit, 220)	&
				"HETATM", this%frameArray(n)%atom_serial_number(i), this%frameArray(n)%atom_name(i),									&
				this%frameArray(n)%alt_loc_indicator(i), this%frameArray(n)%residue_name(i), this%frameArray(n)%chain_identifier(i),	&
				this%frameArray(n)%residue_sequence_number(i), this%frameArray(n)%res_insert_code(i), this%frameArray(n)%coor(1:3,i),	&
				this%frameArray(n)%occupancy(i), this%frameArray(n)%temp_factor(i), this%frameArray(n)%segment_identifier(i),			&
				this%frameArray(n)%element_symbol(i), this%frameArray(n)%charge(i)

				220	format (a6,i5,x,a4,a1,a3,x,a1,i4,a1,3x,3f8.3,2f6.2,6x,a4,a2,f7.4)

			end do

			! END of model or end of file
			write (this%unit, "(a)") "ENDMDL"

		end do

		return
	end subroutine write_pdb

end module xslib_pdb
