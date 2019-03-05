module xslib_pdh
	use xslib_common
	use xslib_utilities, only: linest
	use xslib_array, only: findKClosest
	implicit none

	! .PDH file format
	type pdh_file
		character*80			:: text
		character*4				:: key_words(16)
		integer						:: int_const(8)
		integer						:: num_points
		real							:: real_const(10)
		real, allocatable	:: x(:), y(:), y_error(:)
	contains
		procedure	:: allocate => allocate_pdh
		procedure	:: deallocate => deallocate_pdh
		procedure	:: initialize => initialize_pdh
		procedure	:: read => read_pdh
		procedure	:: write => write_pdh
		! Internal operations
		procedure	:: smearing => smearing_pdh
		procedure	:: smearLength => smearLength_pdh
		procedure	:: smearWidth => smearWidth_pdh
		procedure	:: bining => bining_pdh
		procedure	:: normalize => normalize_pdh
	end type pdh_file

contains

	! Deallocate and allocate .PDH file.
	subroutine allocate_pdh (this, np, initialize)
		class(pdh_file)			:: this
		integer, intent(in)		:: np
		class(*), optional		:: initialize
		Integer					:: stat

		! Error check for valid num. points
		if (np < 1) call error ("Invalid number of points.", NAME="pdh%allocate()")

		call this%deallocate()
		allocate(this%x(np), this%y(np), this%y_error(np), STAT=stat)
		if (stat/=0) call error ("Not enough memory.", NAME="pdh%allocate()")

		! Initialize data
		if (present(initialize)) call this%initialize()

		return
	end subroutine allocate_pdh

	! Deallocate and allocate .PDH file.
	subroutine deallocate_pdh (this)
		class(pdh_file)		:: this
		Integer				:: stat

		if (allocated(this%x)) deallocate(this%x, this%y, this%y_error, STAT=stat)

		return
	end subroutine deallocate_pdh

	! Initialize (format) .PDH file
	subroutine initialize_pdh (this)
		implicit none
		class(pdh_file)		:: this
		integer				:: stat

		this%text 			= ""
		this%key_words(:) 	= ""
		this%int_const(:) 	= 0
		this%real_const(:)	= 0.0

		! Initialize if allocated or not.
		if (allocated(this%x)) then
			this%num_points = size(this%x)
			this%x(:)		= 0.0
			this%y(:)		= 0.0
			this%y_error(:)	= 0.0

		else
			this%num_points = 0

		end if

		stat = 0
		return
	end subroutine initialize_pdh

	! Read .PDH file
	subroutine read_pdh (this, file)
		implicit none
		class(pdh_file)				:: this
		character*(*), intent(in)	:: file
		integer						:: i, unit, line, stat

		! Line counter
		line = 0

		open (NEWUNIT=unit, FILE=trim(file), STATUS="old", IOSTAT=stat)
		if (stat/=0) call error ("File UNIT not assigned.", NAME="pdh%read()")

		! Title
		read (unit, "(a80)", ERR=100, END=110) this%text
		line = line+1

		! Keywords
		read (unit, "(16(a4,1x))", ERR=100, END=110) this%key_words
		line = line+1

		! Parameters
		read (unit, 200, ERR=100, END=110) this%num_points, this%int_const(2:8)
		line = line+1
		read (unit, 210, ERR=100, END=110) this%real_const(1:5)
		line = line+1
		read (unit, 210, ERR=100, END=110) this%real_const(6:10)
		line = line+1

		200	format (8(i9,1x))
		210 format (5(e14.6,1x))

		! Allocate memory
		call this%allocate(this%num_points)

		! Points
		do i = 1, this%num_points
			read (unit, 220, ERR=100, END=110) this%x(i), this%y(i), this%y_error(i)
			line = line+1

			220 format (3(1pe14.6,1x))

		end do

		close (unit)

		return
		! Error handling
		100	call error ("Could not phrase line: '"//itoa(line)//"'", NAME="pdh%read()")
		110	call error ("Unexpected EOF.", NAME="pdh%read()")
	end subroutine read_pdh

	! write .PDH to file or unit
	subroutine write_pdh (this, unit, file)
		use iso_fortran_env
		implicit none
		class(pdh_file)						:: this
		integer, intent(in), optional		:: unit
		character*(*), intent(in), optional	:: file
		integer								:: i, u, ok
		logical								:: opened

		! Check either UNIT or FILE
		if (present(unit)) then
			u = unit
			inquire (UNIT=u, OPENED=opened)
			if (.not. opened) call error("File UNIT not assigned.", NAME="pdh%write()")

		else if (present(file)) then
			open (NEWUNIT=u, FILE=trim(file), STATUS="unknown", IOSTAT=ok)
			if (ok /= 0) call error("Cannot open file: '"//trim(file)//"'", NAME="pdh%write()")

		else
			u = output_unit !stdout

		end if

		! Title
		write (u, "(a80)") this%text

		! Keywords
		write (u, "(16(a4,1x))") this%key_words

		! Parameters
		write (u, 200) this%num_points, this%int_const(2:8)
		write (u, 210) this%real_const(1:5)
		write (u, 210) this%real_const(6:10)

		200	format (8(i9,1x))
		210 format (5(e14.6,1x))

		! Points
		do i = 1, this%num_points
			write (u, 220) this%x(i), this%y(i), this%y_error(i)

		end do

		220 format (3(1pe14.6,1x))

		! Close file
		if (present(file)) close (u)

		return
	end subroutine write_pdh

	! Width profile SAXS semaring
	subroutine smearWidth_pdh (this, width)
		! use mylib, only : findKClosest, linest
		implicit none
		class(pdh_file), intent(inout)	:: this
		type(pdh_file), intent(in)		:: width
		! Internal
		real, allocatable 	:: temp(:)
		integer 			:: i, j, p(2), stat
		real				:: Im, m, step, k(2)

		! Assume width profile bin size
		step = abs(width%x(2)-width%x(1))

		! Temporary data
		allocate(temp(this%num_points), SOURCE=0., STAT=stat)
		if (stat /= 0) call error ("Not enough memory.", NAME="pdh%read()")

		do i = 1, this%num_points ! Loop over data
			do j = 1, width%num_points	! Loop over profile

				m = this%x(i)-width%x(j)
				! Linear inter/extrapolation
				p = findKClosest(m, this%x(:), 2)
				k = linest(this%x(p), this%y(p))
				Im = k(1)*m+k(2)

				temp(i) = temp(i)+width%y(j)*Im*step

			end do
		end do
		! Rewrite data
		this%y(:) = temp
		! Clean-up
		deallocate(temp, STAT=stat)
		return
	end subroutine smearWidth_pdh

	! Length profile SAXS smearing
	subroutine smearLength_pdh (this, length)
		! use mylib, only : findKClosest, linest
		implicit none
		class(pdh_file), intent(inout)	:: this
		type(pdh_file), intent(in)		:: length
		! Internal
		real, allocatable 	:: temp(:)
		integer 			:: i, j, p(2), stat
		real				:: Im, k(2), m, step

		! Assume width profile bin size
		step = abs(length%x(2)-length%x(1))

		! Temporary data
		allocate(temp(this%num_points), SOURCE=0., STAT=stat)
		if (stat /= 0) call error ("Not enough memory.", NAME="pdh%read()")

		do i = 1, this%num_points ! Loop over data
			do j = 1, length%num_points	! Loop over profile

				m = sqrt(this%x(i)**2+length%x(j)**2)
				! Linear inter/extrapolation
				p = findKclosest(m, this%x(:), 2)
				k = linest(this%x(p), this%y(p))
				Im = k(1)*m+k(2)

				temp(i) = temp(i)+length%y(j)*Im*step

			end do
		end do
		! Rewrite data
		this%y(:) = temp
		! Clean-up
		deallocate(temp, STAT=stat)
		return
	end subroutine smearLength_pdh

	! Numericaly smeares (width and length) input file
	! NOTE: width and length profiles must be BOTH bined and normalized.
	subroutine smearing_pdh (this, width, length)
		implicit none
		class(pdh_file), intent(inout)			:: this
		type(pdh_file), intent(in), optional	:: width, length ! Primary beam width and length profiles (optional)

		! Error check
		if (.not. allocated(this%x)) call error("Input data not allocated.", NAME="pdh%read()")

		! If both profiles are present
		if (present(length)) then
			if (.not. allocated(length%x)) call error("Length data not allocated.", NAME="pdh%read()")
			call this%smearLength(length)

		end if

		! Width smearing
		if (present(width)) then
			if (.not. allocated(width%x)) call error("Width data not allocated.", NAME="pdh%read()")
			call this%smearWidth(width)

		end if

		return
	end subroutine smearing_pdh

	! Interpolates np equidistant points between fist and last point.
	subroutine bining_pdh (this, np)
		! use mylib, only : findKClosest, linest
		implicit none
		class(pdh_file)		:: this
		integer, intent(in)	:: np
		integer				:: i, p(2), stat
		real 				:: x, k(2)
		real				:: minval, maxval, step
		real, allocatable 	:: temp(:)

		! Find MIN and MAX x value
		minval = this%x(1)
		maxval = this%x(this%num_points)

		! Determine step size
		step = (maxval-minval)/real(np-1)

		! Allocate temporary
		allocate(temp(np), STAT=stat)

		do i = 1, np
			x = minval+(i-1)*step

			! Find 2 nearest point
			p = findKclosest(x, this%x(:), 2)
			! linear interpolation
			k = linest(this%x(p), this%y(p))

			temp(i) = k(1)*x+k(2)
		end do

		! Re:Allocate pdh file
		call this%allocate(np)

		! Copy temporary data
		this%x(:) 		= [(minval+(i-1)*step, i = 1, np)]
		this%y(:) 		= temp(:)
		this%y_error(:)	= -1.0

		! Clean-up
		deallocate (temp, STAT=stat)

		return
	end subroutine bining_pdh

	! Normalizes area under the curve to 1. Data must be binned
	subroutine normalize_pdh (this)
		implicit none
		class(pdh_file)	:: this
		integer			:: i
		real	        :: area

		! Trapeze integration
		area = this%y(1)+2*sum(this%y(2:this%num_points-1))+this%y(this%num_points)
		area = area/2.*(this%x(2)-this%x(1))
		! Normalize
		this%y(:) = this%y(:)/area

		return
	end subroutine normalize_pdh

end module xslib_pdh
