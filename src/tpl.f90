module xslib_tpl
	use xslib_common
	use xslib_utilities, only: stripComment,  toLower
	implicit none

	! .TPL file format
	type, private :: sub_tpl
		integer					:: natoms, nmol
		integer, pointer		:: id(:) => null()
		character*3, pointer	:: name(:) => null()
		real, pointer			:: pcharge(:) => null()
	end type sub_tpl

	type tpl_file
		integer, private					:: unit
		real								:: box(3) 		! Simulation box side
		integer								:: nTypes		! Number of different types
		type(sub_tpl), allocatable			:: type(:)		! Type of molecule
		! Concentrated data (contains actual data)
		integer								:: natoms
		integer, pointer					:: id(:) => null()
		character*3, pointer				:: name(:) => null()
		real, pointer						:: pcharge(:) => null()
	contains
		procedure	:: read => read_tpl
		procedure 	:: write => write_tpl
		procedure	:: assignID => assignID_tpl
		procedure	:: createList => createList_tpl
		procedure	:: createMask => createMask_tpl
	end type tpl_file

contains

	! Read .TPL file from UNIT
	subroutine read_tpl (this, file)
		implicit none
		class(tpl_file)				:: this
		character*(*), intent(in)	:: file
		character*256				:: arg, buffer, message
		integer						:: i, n, np, unit, offset, cnt, stat, dmy, next
		logical						:: opened

		! open file
		open (NEWUNIT=unit, FILE=trim(file), STATUS="old", IOSTAT=stat)
		if (stat /= 0) call error ("Could not open file: '"//trim(file)//"'", NAME="tpl%read()")

		! Initialize
		this%box(:) = 0.
		this%ntypes = 0
		this%natoms = 0

		! Count Number of "MOLECULE" section and "NATOMS"
		stat = 0
		do while (stat == 0)
			read (unit, "(a)", IOSTAT=stat) buffer
			if (stat /= 0) exit

			! Remove comment
			buffer = stripComment(buffer, "#")

			if (index(toLower(buffer), "molecule")==0) cycle
			this%ntypes = this%ntypes+1

			read (buffer, *, ERR=200) arg, np
			this%natoms = this%natoms+np

		end do

		! Rewind file
		rewind (unit)

		! Allocate main data
		if (associated(this%name)) nullify(this%name, this%pcharge, this%id)
		allocate (this%name(this%natoms), this%pcharge(this%natoms), this%id(this%natoms), STAT=stat)
		if (stat /= 0) call error("Not enough memory.", NAME="tpl%read()")
		! Format
		this%name(:)	= ""
		this%pcharge(:)	= 0.
		this%id(:) 		= 0

		! Allocate data for each "molecule"
		if (allocated(this%type)) deallocate(this%type, STAT=stat)
		allocate (this%type(this%ntypes), STAT=stat)
		if (stat /= 0) call error("Not enough memory.", NAME="tpl%read()")


		! Actual read loop
		stat = 0
		next = 1
		do while (stat == 0)
			! Read line
			read (unit, "(a)", IOSTAT=stat) buffer
			if (stat /= 0) exit

			! Remove comment & cycle if empty line
			buffer = stripComment(buffer, "#")
			if (verify(buffer, " 	")==0) cycle

			! Check first word
			read (buffer, *, ERR=200) arg

			select case (trim(toLower(arg)))
			! Box side (optional) >> legacy parameter
			case ("side")
				read (buffer, *, IOSTAT=dmy) arg, this%box(1:3)
				if (this%box(2) == 0.) this%box(2) = this%box(1)
				if (this%box(3) == 0.) this%box(3) = this%box(1)

			! Number of molecules (optional) >> legacy parameter
			case ("moltype")
				read (buffer, *, IOSTAT=dmy, ERR=200) arg, np
				if (np /= this%ntypes) then
					write (message, "(a,i0)") "Invalid number of molecules: ", np
					call error(message, NAME="tpl%read()")
				end if

				np = this%ntypes

			! Each molecule definition
			case ("molecule")
				! Read num of atoms in molecule and number of molecules in system
				read (buffer, *, IOSTAT=stat) arg, this%type(next)%natoms, this%type(next)%nmol
				if (stat /= 0) go to 200

				! Main array offset
				offset = sum(this%type(:next-1)%natoms)+1
				! Error if pointer will be out of bounds
				if (offset+this%type(next)%natoms-1 > this%natoms) call error("Invalid number of atoms.", NAME="tpl%read()")

				! Assign pointers
				this%type(next)%name => this%name(offset:offset+this%type(next)%natoms-1)
				this%type(next)%pcharge => this%pcharge(offset:offset+this%type(next)%natoms-1)
				this%type(next)%id => this%id(offset:offset+this%type(next)%natoms-1)

				! Read (pseudo)atoms
				cnt = 1
				print *, this%type(next)%natoms
				do while (cnt <= this%type(next)%natoms)
					! Read line
					read (unit, "(a)", END=100) buffer
					buffer = stripComment(buffer, "#")
					if (verify(buffer, " ")==0) cycle

					! Read name, partial charge (optional), and ID (optional)
					read (buffer, *, IOSTAT=dmy) this%type(next)%name(cnt), this%type(next)%pcharge(cnt), this%type(next)%id(cnt)
					print *, this%type(next)%name(cnt), this%type(next)%pcharge(cnt), this%type(next)%id(cnt)
					! Only name needs to be defined
					if (this%type(next)%name(cnt) == "") go to 200

					! Move (pseudo)atom counter
					cnt = cnt+1

				end do

				! End if all molecules were read
				if (next == this%ntypes) exit

				! Move molecule counter
				next = next+1

			end select
		end do

		return
		! Error handling
		100 call error ("Unexpected EoF.", NAME="tpl%read()")
		200 call error ("Cannot phrase line: "//trim(buffer), NAME="tpl%read()")
	end subroutine read_tpl

	! Write .TPL file to FILE, UNIT or STDOUT (if both are missing)
	subroutine write_tpl (this, file, unit)
		use iso_fortran_env
		implicit none
		class(tpl_file)						:: this
		character*(*), intent(in), optional	:: file
		integer, intent(in), optional 		:: unit
		integer			:: i, j, stat, u
		logical			:: opened, pcharge, id

		! print to std:output if not specified otherwise
		if (present(file)) then
			open (NEWUNIT=u, FILE=trim(file), STATUS="unknown", IOSTAT=stat)
			if (stat /= 0) call error ("Cannot open file: '"//trim(file)//"'", NAME="tpl%write()")

		else if (present(unit)) then
			u = unit
			inquire(UNIT=u, OPENED=opened)
			if (.not. opened) call error ("File UNIT not assigned.", NAME="tpl%write()")

		else
			u = output_unit

		end if

		! Quick check if data is allocated
		if (.not. allocated(this%type)) call error ("Data not allocated.", NAME="tpl%write()")

		! Check if pcharge or id has non-zero values
		if (any(this%pcharge(:)/=0.)) then
			pcharge=.true.
		else
			pcharge=.false.
		end if

		if (any(this%id(:)/=0)) then
			id=.true.
		else
			id=.false.
		end if

		! --------------------------------
		! Box side
		write (u, "(a, 3(2x,f9.4))") "side", this%box(1:3)

		! Molecule types
		write (u, "(a,3x,i0)") "moltype", this%ntypes

		! Molecules one by one
		do i = 1, this%ntypes
			! Number of molecules
			write (u, "(a,2(3x,i0))") "molecule", this%type(i)%natoms, this%type(i)%nmol

			! Write Name, partial charge, and id
			do j = 1, this%type(i)%natoms
				write (u, "(a3,$)") this%type(i)%name(j)
				if (pcharge) write (u, "(2x,f8.4,$)") this%type(i)%pcharge(j)
				if (id) write (u, "(2x,i0,$)") this%type(i)%id(j)
				write (u, "(x)") ! End of line

			end do
		end do

		return
	end subroutine write_tpl

	! Assign IDs to the particle based on name and pcharge
	! Particles that are not on the list "H C N O S CH CH2 CH3" have ID 0, unless extended list is provided
	subroutine assignID_tpl (this, validlist)
		use, intrinsic	:: iso_fortran_env
		implicit none
		class(tpl_file), intent(inout)	:: this
		character*(*), intent(in)		:: validlist
		! internal
		integer		:: i, j, n, np, cnt, offset, stat

		! Initialize data
		this%id(:) = -1

		! Assign ID
		cnt = 1
		do i = 1, this%natoms
			if (this%id(i) /= -1) cycle
			! Assign ID if on valid particle list
			if (index(validlist, trim(this%name(i))) /= 0) then
				this%id(i) = cnt
				cnt = cnt+1

			else
				this%id(i) = 0
				call warning ("Invalid particle - '"//trim(this%name(i))// &
				& "'! Particle will be excluded form the calculations.")

			end if

			! All same particles get same ID
			do j = i+1, np, 1
				if (this%name(i)==this%name(j) .and. this%pcharge(i)==this%pcharge(j)) this%id(j) = this%id(i)

			end do
		end do

		return
	end subroutine assignID_tpl

	! The seconf variation where every valid particle gets unique ID
	subroutine assignID2_tpl (this, validlist)
		use, intrinsic	:: iso_fortran_env
		implicit none
		class(tpl_file), intent(inout)	:: this
		character*(*), intent(in)		:: validlist
		! internal
		integer							:: i, j, n, np, cnt, offset, stat

		! Initialize data
		this%id(:) = -1

		! Assign ID
		cnt = 1
		do i = 1, this%natoms
			if (index(validList, this%name(i)) /= 0) then
				this%id(i) = cnt
				cnt = cnt+1
			else
				this%id(i) = 0
				call warning ("Invalid particle - '"//trim(this%name(i))// &
				& "'! Particle will be excluded form the calculations.")

			end if

		end do

		return
	end subroutine assignID2_tpl


	! This is a subroutine because of FUCKING PREHISTORIC FKKT CIKLON
	subroutine createList_tpl (this, list)
		implicit none
		class(tpl_file)				:: this
		integer, allocatable, intent(inout)	:: list(:)
		integer		:: i, j, n, np, stat, offset

		! Get list size
		np = sum(this%type(:)%nmol*this%type(:)%natoms)
		if (allocated(list)) deallocate(list, STAT=stat)
		allocate(list(np), STAT=stat)
		if (stat /= 0) call error ("Not enough memory.", NAME="tpl%createList()")

		offset = 1
		do n = 1, this%ntypes
			! Calculate offset due to previous molecules
			do i = 1, this%type(n)%nmol
				list(offset:offset+this%type(n)%natoms-1) = this%type(n)%id(:)
				offset = offset+this%type(n)%natoms
			end do
		end do

		return
	end subroutine createList_tpl


	function createMask_tpl (this, ref) result (mask)
		implicit none
		class(tpl_file)			:: this
		integer					:: ref
		logical, allocatable	:: mask(:)
		integer					:: i, j, n, np, stat, offset

		! Get list size
		np = sum(this%type(:)%nmol*this%type(:)%natoms)
		if (allocated(mask)) deallocate(mask, STAT=stat)
		allocate(mask(np), SOURCE=.false., STAT=stat)
		if (stat /= 0) call error ("Not enough memory.", NAME="tpl%createMask()")

		do n = 1, this%ntypes
			! Calculate offset due to previous molecules
			offset = sum(this%type(:n-1)%nmol*this%type(:n-1)%natoms)+1
			do i = 1, this%type(n)%natoms
				if (this%type(n)%id(i) /= ref) cycle

				! Construct list
				mask(offset+i-1:) = [(.true., j = 1, this%type(n)%nmol, this%type(n)%natoms)]

			end do
		end do

		return
	end function createMask_tpl

end module xslib_tpl
