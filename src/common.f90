module xslib_common
	implicit none

	interface str ! Like python
		module procedure :: itoa, i8toa, ftoa, f8toa, ctoa, btoa
	end interface

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
	! Integer, Real, Complex, logical to string

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

	function ctoa (imag,fmt) result (res)
		! Real to character
		character*(:), allocatable			:: res
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

		res = trim(adjustl(tmp))

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
