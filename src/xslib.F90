#define _VERSION "v@PROJECT_VERSION@"

module xslib
	use xslib_common
	use xslib_utilities
	use xslib_array
	use xslib_csv
	use xslib_gro
	use xslib_pdh
	use xslib_pdb
	use xslib_tpl
	use xslib_ndx
	use xslib_trj
	use xslib_xyz
	use xslib_frame
	implicit none

	character*16, parameter :: xslib_version=_VERSION
	character*32, parameter :: xslib_date=__DATE__//" "//__TIME__

contains

	! Returns Xslib version and compile date. By default only version is returned.
	function xslibinfo (version, date)
		implicit none
		character*(:), allocatable	:: xslibinfo
		logical, optional			 			:: version, date
		logical											:: v, d

		v = merge(version, .true., present(version))
		d = merge(date, .false., present(date))

		xslibinfo=""
		if (v .AND. d) then
			xslibinfo = trim(xslib_version)//" -- "//trim(xslib_date)
		else if (v) then
			xslibinfo = trim(xslib_version)
		else if (d) then
			xslibinfo = trim(xslib_date)
		end if

		return
	end function xslibinfo

end module xslib
