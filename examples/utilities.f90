program main
	use xslib
	!$ use omp_lib
	implicit none
	real, parameter			:: pi=4.*atan(1.)
	integer							:: i
	integer							:: int=1
	integer*8						:: int8=1
	real 								:: float=1.234
	real*8							:: float8=1.234d0
	logical							:: bool=.true.
	complex 						:: compl=(1.,1.)
	character*128				:: string="hello"
	real, dimension(3) 	:: a, b, c, d
	real, dimension(32)	:: x, y
	real 								:: dr(2), R2
	real								:: ave, err

	! Transform to string
	write (*,*) str(int), str(float), str(bool), str(compl)
	write (*,*) str(float,FMT="(f8.5)")

	! Warning and error
	call warning ("This is warning message.", NAME="example")
	! call error ("This is error message,", NAME="example")

	! Find new unit
	write (*,*) newUnit(int)
	write (*,*) int

	! Vector operations
	a=[1.,0.,0.]
	b=[0.,1.,0.]
	c=[0.,0.,1.]
	d=[-1.,-1.,-1.]

	write (*,*) cross(a, b)
	write (*,*) minImg(1.5*a, [1.,1.,1.])
	write (*,*) getDistance(a, b)
	write (*,*) getAngle(a, b, c)
	write (*,*) getDihedral(a, b, c, d)

	! Transform operations
	write (*,*) deg2rad(30.)
	write (*,*) rad2deg(pi/2.)
	write (*,*) crt2sph(a), sph2crt(b)
	write (*,*) crt2cyl(a), cyl2crt(b)

	! Time operations
	float8 = get_wtime()
	call msleep (100)
	write (*,*) write_time(get_wtime()-float8)

	! Time operations with OMP
	!$ float8 = OMP_get_wtime()
	!$ call msleep (100)
	!$ write (*,*) write_time(OMP_get_wtime()-float8)

	! Average and variance
	ave=0.
	err=0.
	do i = 1, 100
		call variance(real(i), ave, err, i)
	end do
	! Transform to true variance
	err = merge(err/(i-1), -1., i>1)
	write (*,*) ave, err

	! Basic file name operations
	string="path/to/file.txt"
	write (*,*) trim(string), " -> ", basename(string)
	write (*,*) trim(string), " -> ", extension(string)
	write (*,*) trim(string), " -> ", nextFreeName(string)

	! File backup
	! call backup (string)

	! Basic string operations
	write (*,*) stripComment("message #comment", "#")
	write (*,*) isEmpty(" 	")
	write (*,*) isWord("12345"), isWord("12345abc")
	write (*,*) replaceText("This is bad.", "bad", "good")
	write (*,*) tab2space("	")
	write (*,*) toUpper("THIS is FOO bar.")
	write (*,*) toLower("THIS is FOO bar.")

	! Write options
	call printOpt ("-x", "Select a number", TYPE="int", VALUE=int)
	call printOpt ("-x", "Select a number", TYPE="real", VALUE=float)
	call printOpt ("-x", "Select a number", TYPE="double", VALUE=float8)
	call printOpt ("-x", "Select a number", TYPE="bool", VALUE=bool)
	call printOpt ("-x", "Select a number", TYPE="char", VALUE=string)

	! Progres bar
	int = 10*1000
	do i = 0, int, 1
		float = real(i)/int
		call progressBar(float, SIZE=30, MESSAGE="Please wait.")
	end do

	! Linear regression
	! Curve with slope=2. and intercept=-1.
	do i = 1, size(x)
		x(i) = real(i)
		y(i) = real(i)*2.-1.
	end do

	write (*,*) linest(x(:), y(:), d=dr, R2=R2)
	write (*,*) dr, R2

	call exit (0)
end program main
