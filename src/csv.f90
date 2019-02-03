module xslib_csv
	use xslib_common
	implicit none

	type :: csv_file
	contains
		procedure :: read => read_csv
		procedure :: write => write_csv
	end type csv_file

contains

	! Reads DATA and (optional) HEADER from csv file.
	subroutine read_csv (this, file, data, header)
		implicit none
		class(csv_file)							:: this
		real, allocatable, intent(inout)		:: data(:,:)
		character*(*), intent(in)				:: file
		character*(*), allocatable, optional	:: header(:)
		integer									:: i, unit, stat
		integer									:: cols, rows
		character*512							:: buffer
		real									:: dmy(128)
		logical 								:: hasHeader

		open (NEWUNIT=unit, FILE=trim(file), STATUS="old", IOSTAT=stat)
			if (stat /= 0) call error ("Could not open file: '"//trim(file)//"'", NAME="csv%read")

		! -------------------
		! Read first line and check if file has header
		read (unit, "(a)", IOSTAT=stat) buffer
			if (stat /= 0) call error ("Empty file: '"//trim(file)//"'", NAME="csv%read")

		! Check for ASCII characters
		if (scan(trim(buffer),"AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz")/=0) then
			hasHeader=.true.
			! Load next line to buffer
			read (unit, "(a)", IOSTAT=stat) buffer
				if (stat /= 0) call error("Empty file: '"//trim(file)//"'", NAME="csv%read")

		else
			hasHeader=.false.

		end if

		! -------------------
		! Count number of columns from the first line with data.
		cols = 0
		do while (stat==0)
			cols = cols+1
			read (buffer, *, IOSTAT=stat) dmy(1:cols)
			if (stat/=0) cols = cols-1

		end do

		! Rewind file
		rewind (unit, IOSTAT=stat)
		if (hasHeader) read (unit, *) ! Dummy read header

		! -------------------
		! Count number of rows
		rows = 0
		do while (stat==0)
			read (unit, *, IOSTAT=stat)
			if (stat==0) rows = rows+1

		end do

		! Rewind file
		rewind (unit, IOSTAT=stat)

		! -------------------
		! Allocate memory

		! Data
		if (allocated(data)) deallocate(data, STAT=stat)
		allocate(data(cols,rows), SOURCE=0., STAT=stat)
		if (stat /= 0) call error ("Not enough memory.", NAME="csv%read")

		! Header
		if (present(header)) then
			if (allocated(header)) deallocate(header, STAT=stat)
			allocate (header(cols), STAT=stat)
			header=""

		end if

		! -------------------
		! Read header if present
		if (hasHeader) then
			if (present(header)) then
				read (unit, *, IOSTAT=stat) header
				! Add ' ' if header contains empty spaces
				do i = 1, cols
					if (scan(trim(header(i)), " ")/=0) header(i)="'"//trim(header(i))//"'"
				end do

			else
				read (unit, *, IOSTAT=stat)

			end if
		end if

		! Read actual data
		do i = 1, rows
			read (unit, *, IOSTAT=stat) data(:,i)

		end do

		return
	end subroutine read_csv

	! Write DATA and (optional) HEADER in csv format to UNIT, FILE or STDOUT (if both absent).
	! Default DELIMITER is comma - ","
	subroutine write_csv (this, data, header, unit, file, delimiter)
		use iso_fortran_env
		implicit none
		class(csv_file)			:: this
		real, intent(in)		:: data(:,:)
		character*(*), optional	:: header(:), file, delimiter
		integer, optional		:: unit
		integer					:: i, j, u, cols, rows, stat
		logical 				:: opened
		character*8				:: dl ! delimiter
		character*64			:: buffer(size(data,2)), fmt

		if (present(unit)) then
			inquire (UNIT=unit, OPENED=opened)
			if (.not. opened) call error ("File UNIT not assigned.", NAME="csv%write")

		else if (present(file)) then
			open (NEWUNIT=u, FILE=trim(file), STATUS="old", IOSTAT=stat)
			if (stat/=0) call error ("Cannot open file:'"//trim(file)//"'", NAME="csv%write")

		else
			u = output_unit !stdout

		end if

		! ================================
		! delimiter
		if (present(delimiter)) then
			dl = trim(adjustl(delimiter))
		else
			dl = ","
		end if

		! ================================
		! Data array dimensions
		cols = size(data, 1)
		rows = size(data, 2)

		! Form output format
		write (fmt,"(i0)") 2*cols-1 ! n entries + n-1 delimiters
		fmt = "("//trim(fmt)//"a)"

		! Write header
		if (present(header)) then
			write (u, fmt, IOSTAT=stat) (trim(header(i)), trim(dl), i=1, cols-1), trim(header(cols))

		end if

		! Write data row by row
		do i = 1, rows
			! Write columns to buffer
			do j=1, cols
				write (buffer(j),*) data(j,i)
				buffer(j) = adjustl(buffer(j))

			end do ! for j

			! Write buffer + delimiters
			write (u, fmt) (trim(buffer(j)), trim(dl) , j=1, cols-1), trim(buffer(cols))

		end do ! for i

		return
	end subroutine write_csv
end module xslib_csv
