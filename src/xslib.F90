#ifndef _VERSION
#define _VERSION "vX.Y.Z"
#endif

module xslib
	use xslib_common
	use xslib_utilities
	use xslib_array
	! ! use mod_list  ! -- contains generic linked list routines
	use xslib_csv ! ! comma separated values
	use xslib_gro
	use xslib_pdh
	use xslib_pdb
	use xslib_tpl
	use xslib_ndx
	use xslib_trj
	use xslib_xyz
	use xslib_frame
	implicit none

	! Information about Library
	character*64, parameter	:: XsLibINFO = _VERSION//" -- "//__DATE__//" "//__TIME__

	contains
end module xslib
