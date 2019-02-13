module xslib_common
	implicit none

	! LEGACY CODE:
	! interface str
	! 	module procedure :: itoa, i8toa, ftoa, f8toa, ctoa, btoa
	! end interface

	interface errorHandling
		module procedure :: error
	end interface

contains

	! Write error message to errout and terminate the program.
	! <name> ERROR: <message>
	subroutine error (message, name)
		use, intrinsic	:: iso_fortran_env
		implicit none
		character*(*), intent(in)			:: message
		character*(*), optional, intent(in)	:: name

		if (present(name)) then
			write (error_unit,*) trim(name)//" ERROR: "//trim(message)
		else
			write (error_unit,*) "ERROR: "//trim(message)
		end if

		call exit (1)
	end subroutine error

	! Write warning (but does not terminate) message to errout.
	! <name> WARNING: <message>
	subroutine warning (message, name)
		use, intrinsic	:: iso_fortran_env
		implicit none
		character*(*), intent(in)			:: message
		character*(*), optional, intent(in)	:: name

		if (present(name)) then
			write (error_unit,*) trim(name)//" WARNING: "//trim(message)
		else
			write (error_unit,*) "WARNING: "//trim(message)
		end if

		return
	end subroutine warning

	! -------------------------------------------------

	! Trnasforms scalar of any kind to character of shorter possible lenght.
	recursive function str (value, fmt) result (string)
		implicit none
		class(*)					:: value
		character*(*), optional		:: fmt
		character*(:), allocatable	:: string
		character*32				:: deffmt, buffer

		! Act according to the type of variable
		select type (value)
		type is (integer)
			! Default format
			deffmt = "(i0)"
			! Overide format if externally defined
			if (present(fmt)) deffmt = fmt
			! Write to buffer
			write (buffer, deffmt) value
			! Write buffer back to result
			string = trim(adjustl(buffer))

		type is (integer(KIND=8))
			deffmt = "(i0)"
			if (present(fmt)) deffmt = fmt
			write (buffer, deffmt) value
			string = trim(adjustl(buffer))

		type is (real)
			deffmt = "(f0.3)"
			if (present(fmt)) deffmt = fmt
			write (buffer, deffmt) value
			string = trim(adjustl(buffer))

		type is (real(KIND=8))
			deffmt = "(f0.6)"
			if (present(fmt)) deffmt = fmt
			write (buffer, deffmt) value
			string = trim(adjustl(buffer))

		type is (complex)
			deffmt = "(f0.3)"
			if (present(fmt)) deffmt = fmt

			if (aimag(value) > 0) then
				write (buffer, *) str(real(value), FMT=deffmt)//"+"// &
				str(aimag(value), FMT=deffmt)//"i"
			else
				write (buffer, *) str(real(value), FMT=deffmt)//"-"// &
				str(abs(aimag(value)), FMT=deffmt)//"i"
			end if
			string = trim(adjustl(buffer))

		type is (logical)
			if (value) then
				string = "T"
			else
				string = "F"
			end if

		type is (character*(*))
			string = trim(adjustl(value))

		class default
			! Can't have everything
			! buffer = "???"
			string = "???"

		end select

		! string = trim(adjustl(buffer))

		return
	end function str

	! -------------------------------------------------
	! Individual transformations for Integer, Real, Complex and logical to string

	! Integer to character, in "fmt" format (optional).
	function itoa (int, fmt) result (string)
		character*(:), allocatable			:: string
		integer, intent(in)					:: int
		character*(*), intent(in), optional	:: fmt
		character*32						:: tmp, ifmt

		ifmt = "(i0)" ! Default format
		if (present(fmt)) ifmt = fmt

		write (tmp, ifmt) int
		string = trim(adjustl(tmp))

		return
	end function itoa

	! Integer*8 to character, in "fmt" format (optional).
	function i8toa (int, fmt) result (string)
		character*(:), allocatable			:: string
		integer*8, intent(in)				:: int
		character*(*), intent(in), optional	:: fmt
		character*32						:: tmp, ifmt

		ifmt = "(i0)" ! Default format
		if (present(fmt)) ifmt = fmt

		write (tmp, ifmt) int
		string = trim(adjustl(tmp))

		return
	end function i8toa

	! Real to character, in "fmt" format (optional).
	function ftoa (float, fmt) result (string)
		! Real to character
		character*(:), allocatable			:: string
		real, intent(in)					:: float
		character*(*), intent(in), optional	:: fmt
		character*32						:: tmp, ifmt

		ifmt = "(f0.3)"
		if (present(fmt)) ifmt = fmt

		write (tmp, ifmt) float
		string = trim(adjustl(tmp))

		return
	end function ftoa

	! Real to character, in "fmt" format (optional).
	function f8toa (float, fmt) result (string)
		! Real to character
		character*(:), allocatable			:: string
		real*8, intent(in)					:: float
		character*(*), intent(in), optional	:: fmt
		character*32						:: tmp, ifmt

		ifmt = "(f0.6)"
		if (present(fmt)) ifmt = fmt

		write (tmp, ifmt) float
		string = trim(adjustl(tmp))

		return
	end function f8toa

	function ctoa (imag,fmt) result (string)
		implicit none
		! Real to character
		character*(:), allocatable			:: string
		complex, intent(in)					:: imag
		character*(*), intent(in), optional	:: fmt
		character*32						:: tmp, ifmt

		if (.not. present(fmt)) then
			ifmt = "(f0.3)"	! Default format
		else
			ifmt = fmt
		end if

		if (aimag(imag) > 0) then
			write (tmp, *) ftoa(real(imag), FMT=ifmt),"+", &
			ftoa(aimag(imag),FMT=ifmt),"i"
		else
			write (tmp, *) ftoa(real(imag), FMT=ifmt),"-", &
			ftoa(abs(aimag(imag)),FMT=ifmt),"i"
		end if

		string = trim(adjustl(tmp))

		return
	end function ctoa

	function btoa (bool) result (string)
		implicit none
		logical, intent(in)	:: bool
		character*1			:: string

		if (bool) then
			string = "T"
		else
			string = "F"
		end if

		return
	end function btoa

	! -------------------------------------------------

	! Finds next free file unit.
	integer function newUnit (unit)
		implicit none
		integer, optional, intent(out)	:: unit
		logical							:: opened

		! First 10 units are usually system reserved
		do newunit = 10, 9999
			inquire (UNIT=newunit, OPENED=opened)
			if (.not. opened) then
				if (present(unit)) unit = newunit
				return

			end if

		end do
	end function newUnit

end module xslib_common
