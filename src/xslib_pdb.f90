module xslib_pdb
	use xslib_common
	implicit none
	private
	public :: pdb_file

	! .PDB data format
	type pdb_frame
		real 											:: box(3)
		integer										:: natoms
		character*20, allocatable	:: record_type(:), atom_name(:), alt_loc_indicator(:), residue_name(:), chain_identifier(:)
		character*20, allocatable	:: res_insert_code(:), segment_identifier(:), element_symbol(:)
		integer, allocatable			:: atom_serial_number(:), residue_sequence_number(:)
		real, allocatable					:: coor(:,:)
		real, allocatable					:: occupancy(:), temp_factor(:), charge(:)
	contains
		procedure	:: allocate => allocate_pdb_frame
		procedure	:: deallocate => deallocate_pdb_frame
		procedure	:: initialize => initialize_pdb_frame
	end type pdb_frame


	type pdb_file
		integer, private							:: unit, first=1, last=-1, stride=1
		integer												:: nframes, framesLeft, allframes
		type(pdb_frame), allocatable	:: frameArray(:)
	contains
		procedure :: open => open_pdb
		procedure :: close => close_pdb
		procedure	:: allocate => allocate_pdb
		procedure	:: next => next_pdb
		procedure	:: set => setframe_pdb
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
		class(pdb_frame)		:: this	! .GRO data file
		integer, intent(in)	:: np	! Number of points to allocate
		logical, optional		:: onlycoor, initialize	! if NOT present - allocate everything, if present - allocate only coordinates
		integer							:: stat
		logical							:: coor, init
		character*128				:: message

		! Error check
		if (np < 0) then
			write (message, "(a,i0)") "Invalid array size: ", np
			call error(message, NAME="pdh%frameArray%allocate()")
		end if

		! Read only coordinates ?
		coor = merge(onlycoor, .false., present(onlycoor))

		! Initialize file ?
		init = merge(initialize, .false., present(initialize))

		! Deallocate
		call this%deallocate()
		this%natoms = np

		! Allocate only coordinates
		if (coor) then
			allocate (this%coor(3,np), STAT=stat)

		! Allocate entire .PDB memory
		else
			allocate (this%record_type(np), this%atom_serial_number(np), this%atom_name(np),		&
			&	this%alt_loc_indicator(np), this%residue_name(np), this%chain_identifier(np),		&
			&	this%residue_sequence_number(np), this%res_insert_code(np), this%coor(3,np),		&
			&	this%occupancy(np), this%temp_factor(np), this%segment_identifier(np), this%element_symbol(np), this%charge(np), STAT=stat)

		end if

		if (stat/=0) call error("Not enough memory.", NAME="pdh%frameArray%allocate()")

		! Initialize data
		if (init) call this%initialize()

		return
	end subroutine allocate_pdb_frame

	! deallocate .pdb data.
	subroutine deallocate_pdb_frame (this)
		implicit none
		class(pdb_frame)	:: this
		integer						:: stat

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
	subroutine initialize_pdb_frame (this)
		implicit none
		class(pdb_frame)	:: this
		integer						:: i, stat

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
		class(pdb_file)						:: this
		character*(*), intent(in)	:: filename
		integer										:: stat
		character*4								:: dmy

		! Open file
		open (NEWUNIT=this%unit, FILE=trim(filename), STATUS="unknown", IOSTAT=stat)
		if (stat/=0) call error("Cannot open file: '"//trim(filename)//"'.", NAME="pdb%open()")

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
	end subroutine open_pdb

	! close PDB file
	subroutine close_pdb (this)
		implicit none
		class(pdb_file)	:: this
		integer					:: stat
		close(this%unit, IOSTAT=stat)
		return
	end subroutine close_pdb

	! allocate FrameArray
	subroutine allocate_pdb (this, np)
		implicit none
		class(pdb_file)	:: this
		integer					:: np
		character*128		:: message
		integer					:: stat

		! Error check
		if (np < 0) then
			write (message, "(a,i0)") "Invalid array size: ", np
			call error(message, NAME="pdb%frameArray%allocate()")
		end if

		! Set new number of frames
		this%nframes = np

		if (allocated(this%frameArray)) deallocate(this%frameArray, STAT=stat)
		allocate (this%frameArray(this%nframes), STAT=stat)
		if (stat/=0) call error("Not enough memory.", NAME="pdb%allocate()")

		this%frameArray%natoms = 0

		return
	end subroutine allocate_pdb

	! Skip "stride" number of frames. returning number si the number of frames skipped.
	function next_pdb (this, nframes) result (stat)
		implicit none
		class(pdb_file)		:: this
		integer, optional	:: nframes
		integer						:: n, np, stat
		character*6				:: dmy

		! Default status
		stat = 0

		! Number of frames to skip
		np = merge(nframes, 1, present(nframes))

		do n = 1, np, 1
			do
				read (this%unit, "(a)", END=100) dmy
				if (index(dmy,"END")==1) exit
			end do
			! At this point frame skip was succesfull
			stat = stat+1
		end do

		100 continue

		return
	end function next_pdb

	! Set first/last frame and add frame stride.
	subroutine setframe_pdb (this, first, last, stride)
		implicit none
		class(pdb_file)		:: this
		integer, optional	:: first, last, stride
		integer						:: stat
		logical 					:: opened

		! Check if file is opened
		inquire (UNIT=this%unit, OPENED=opened)
		if (.NOT. opened) call error ("File UNIT not assigned.", NAME="pdb%set()")

		! Reset file to begining
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

		! --------------------------

		! Modify number of frames and number of frames left
		this%nframes = ceiling((this%last-this%first+1)/real(this%stride))
		this%framesleft = this%nframes

		if (this%first>1) then
			! Cycle through frames to the "first" frame
			stat = this%next(this%first-1)
		end if

		return
	end subroutine setframe_pdb

	! Read single .PDB frame from UNIT. ONLYCOOR = read only coordinates.
	function read_next_pdb (this, nframes, onlycoor) result (framesRead)
		implicit none
		class(pdb_file)								:: this		! .PDB data file
		integer, intent(in), optional	:: nframes	! how many frames to read
		logical, optional							:: onlycoor	! if present read only coordinates
		integer												:: framesRead
		character*256									:: buffer, message
		character*6										:: dmy
		integer												:: n, np, next, line, current, stat
		logical												:: opened, coor

		! If file unit is esternaly defined
		np = merge(nframes, 1, present(nframes))

		! Check if opened
		inquire (UNIT=this%unit, OPENED=opened)
		if (.not. opened) call error("File UNIT not assigned.", NAME="pdb%read_next()")

		! How many frames are left
		framesRead = min(this%framesLeft, np)

		! Read only coordinates?
		coor = merge(onlycoor, .false., present(onlycoor))

		! Allocate memory
		call this%allocate(framesRead)

		do n = 1, framesRead
			! One less frame to read
			this%framesLeft = this%framesLeft-1

			! Store current opsition
			current = ftell(this%unit)

			! Count all atoms are until end of frame
			this%frameArray(n)%natoms = 0
			do
				read (this%unit, *, END=110) dmy
				if (index(dmy,"ATOM")/=0 .or. index(dmy,"HETATM")/=0) then
					this%frameArray(n)%natoms = this%frameArray(n)%natoms+1
				else if (index(dmy,"END")==1) then
					exit
				end if
			end do

			! Return to starting position
			call fseek (this%unit, current, WHENCE=0, STATUS=stat)

			! --------------------

			! Allocate data
			call this%frameArray(n)%allocate(this%frameArray(n)%natoms, ONLYCOOR=coor)

			! Main read loop
			line = 0
			next = 0
			do while (stat==0)
				read (this%unit, "(a)",  IOSTAT=stat, END=110) buffer
				line = line+1

				! Read dummy from buffer
				read (buffer, "(a)") dmy

				if (index(dmy,"ATOM")/=0 .or. index(dmy,"HETATM")/=0) then
					! line counter
					next = next+1
					! if SWITCH is on, read only coordinates (Computational efficiency)
					if (coor) then
						read (buffer, "(30x,3f8.3)", IOSTAT=stat, ERR=120, END=110) this%frameArray(n)%coor(:,next)

					else
						read (buffer, "(a6,i5,x,a4,a1,a3,x,a1,i4,a1,3x,3f8.3,2f6.2,6x,a4,a2,f7.4)", IOSTAT=stat, ERR=120, END=110)	&
							this%frameArray(n)%record_type(next), this%frameArray(n)%atom_serial_number(next), &
							this%frameArray(n)%atom_name(next), this%frameArray(n)%alt_loc_indicator(next), &
							this%frameArray(n)%residue_name(next), this%frameArray(n)%chain_identifier(next),	&
							this%frameArray(n)%residue_sequence_number(next), this%frameArray(n)%res_insert_code(next), &
							this%frameArray(n)%coor(:,next), this%frameArray(n)%occupancy(next), this%frameArray(n)%temp_factor(next), &
							this%frameArray(n)%segment_identifier(next), this%frameArray(n)%element_symbol(next), this%frameArray(n)%charge(next)


					end if

				! If "END" OR "ENDMDL" is found in record exit
				else if (index(dmy,"END")==1) then
					exit

				! Read box dimensions
				else if (index(dmy,"CRYST1")/=0) then
					read (buffer, 200, ERR=120) this%frameArray(n)%box(:)
					200	  format (6x,3f9.3)

				end if

			end do ! while stat

			! Skip specified number of frames
			stat = this%next(this%stride-1)

		end do ! for n

		return
		! Error handling
		110 call error("Unexpected EoF.", NAME="pdb%read_next()")
		120 write (message,"(2(a,i0))") "Could not phrase line: ", line, " in frame: ", this%nframes-this%framesLeft
		call error (message, NAME="pdb%read_next()")
	end function read_next_pdb

	! Read entire PDB file
	subroutine read_pdb (this, file)
		implicit none
		class(pdb_file)	:: this
		character*(*)		:: file
		integer					:: stat
		call this%open(file)
		stat = this%read_next(this%nframes)
		call this%close()
		return
	end subroutine read_pdb

	! Get box size from PDB file
	! NOTE: Returns [0,0,0] array if box cannot be found.
	function box_pdb (this) result (box)
		implicit none
		class(pdb_file)	:: this
		real						:: box(3)
		logical					:: opened
		integer					:: stat
		character*128		:: buffer
		character*6			:: dmy

		! Initialize box
		box(:) = 0.

		! Error check
		inquire (UNIT=this%unit, OPENED=opened)
		if (.NOT. opened) call error ("No file opened.", NAME="pdb%box()")

		! If data was already read
		if (allocated(this%frameArray)) then
			box(:) = this%frameArray(1)%box(:)

		! Else it must be first time readin (OK to use rewind)
		else
			stat = 0
			do while (stat==0)
				read (this%unit, "(a)", IOSTAT=stat) buffer
				read (buffer, "(a)") dmy

				if (index(dmy,"CRYST1")/=0) then
					read (buffer, *) dmy, box(:)
					exit

				else if (index(dmy,"END")==1) then
					exit

				end if
			end do

			! Reset file
			rewind (this%unit)

		end if

		return
	end function box_pdb

	! get number of atoms from PDB file
	function natoms_pdb (this) result (natoms)
		implicit none
		class(pdb_file)	:: this
		integer					:: natoms
		logical					:: opened
		integer					:: stat
		character*6			:: dmy

		! Initialize
		natoms = 0

		! Error check
		inquire (UNIT=this%unit, OPENED=opened)
		if (.NOT. opened) call error ("File UNIT not assigned.", NAME="pdb%box()")

		! If data was already read
		if (allocated(this%frameArray)) then
			natoms = this%frameArray(1)%natoms

		! Else it must be first time readin (OK to use rewind)
		else
			stat=0
			do while (stat==0)
				read (this%unit, "(a)", IOSTAT=stat) dmy

				! Lines containing atom information start with "ATOM" or "HETATOM"
				if (index(dmy,"ATOM")/=0 .OR. index(dmy,"HETATM")/=0) then
					natoms = natoms+1

				! Frame ends with either END or ENDMDL entry
				else if (index(dmy,"END")==1) then
					exit

				end if
			end do

			! Reset file
			rewind (this%unit)

		end if

		return
	end function natoms_pdb

	! Write single .PDB frame to UNIT or FILE
	! NOTE: Data that is "OnlyCoor" cannot be writen.
	subroutine write_pdb (this, unit, file)
		use iso_fortran_env
		implicit none
		class(pdb_file)					:: this		! .PDB data file
		character*(*), optional	:: file
		integer, optional				:: unit
		integer									:: i, u, n, cnt, stat, serial
		logical									:: opened, onlycoor

		! Initialize
		serial = 1 ! serial number of frame

		! Check if allocated
		if (.NOT. allocated(this%frameArray)) call error("Data not allocated.", NAME="pdb%write()")

		! Select output method
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

		! ---------------------------

		! Print all arrays
		do n = 1, size(this%frameArray(:))

			! Check if  data is allocated
			if (.NOT. allocated(this%frameArray(n)%coor)) call error("Data not allocated.", NAME="pdb%write()")

			! Does data contain only coordinates
			onlycoor = .NOT. allocated(this%frameArray(n)%record_type)

			! Box dimensions
			if (any(this%frameArray(n)%box(:)/=0.)) write (u, "(a6, 3f9.3, 3f7.2)") "CRYST1", this%frameArray(n)%box(:), 90., 90., 90.

			! Model serial number
			write (u, "(a6,i5)") "MODEL ", serial
			serial = serial+1
			if (serial>int(1E5)) serial=1

			! If only coordinates are allocated fill in the missing data
			if (onlycoor) then
				cnt = 1
				do i = 1, this%frameArray(n)%natoms
					write (u, "(a6,i5,x,a4,x,a3,4x,i4,4x,3f8.3,2f6.2)") "HETATM", cnt, "X", "LIG", 1, this%frameArray(n)%coor(:,i), 1.0, 0.0
					! Atom counter
					cnt = cnt+1
					if (serial>int(1E5)) cnt=1

				end do

			else
				do i = 1, this%frameArray(n)%natoms
					write (u, "(a6,i5,x,a4,a1,a3,x,a1,i4,a1,3x,3f8.3,2f6.2,6x,a4,a2,f7.4)")	&
					"HETATM", this%frameArray(n)%atom_serial_number(i), this%frameArray(n)%atom_name(i),									&
					this%frameArray(n)%alt_loc_indicator(i), this%frameArray(n)%residue_name(i), this%frameArray(n)%chain_identifier(i),	&
					this%frameArray(n)%residue_sequence_number(i), this%frameArray(n)%res_insert_code(i), this%frameArray(n)%coor(1:3,i),	&
					this%frameArray(n)%occupancy(i), this%frameArray(n)%temp_factor(i), this%frameArray(n)%segment_identifier(i),			&
					this%frameArray(n)%element_symbol(i), this%frameArray(n)%charge(i)

				end do
			end if

			! END of model or end of file
			write (u, "(a)") "ENDMDL"

		end do

		return
	end subroutine write_pdb

end module xslib_pdb
