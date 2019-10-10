program main
	use xslib
	implicit none
	write (*,*) "xslib: ", xslibinfo(DATE=.TRUE.)
	call exit (0)
end program main
