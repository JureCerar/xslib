program example
	use xslib
	!$ use omp_lib
	implicit none
	integer							:: i, n, int
	integer*8						:: int8
	real 								:: float
	real*8							:: float8
	real, dimension(3) 	:: a, b, c, d
	real, dimension(32)	:: x, y
	real 								:: dr(2), R2
	real								:: ave, err
	logical							:: bool
	complex 						:: compl
	character*128				:: string

	! ======================================
	int=1
	float=1.234
	bool=.true.
	compl=(1.,1.)

	write (*,"(4(a,2x))") str(int), str(float), str(bool), str(compl)
	write (*,*) str(float,FMT="(f8.5)")

	! ======================================
	call warning ("This is warning message.", NAME="main")
	! call error ("This is error message,", NAME="main")

	! ======================================
	write (*,*) newUnit(int)
	write (*,*) int

	! ======================================
	a=[1.,0.,0.]
	b=[0.,1.,0.]
	c=[0.,0.,1.]
	d=[-1.,-1.,-1.]

	c = cross(a, b)
	write (*,*) c

	c = minImg(1.5*a,[1.,1.,1.])
	write (*,*) c

	float = getDistance(a, b)
	write (*,*) float

	float = getAngle(a, b, c)
	write (*,*) float

	float = getDihedral(a, b, c, d)
	write (*,*) float

	float = deg2rad(30.) ! 30 deg
	write (*,*) float

	float = rad2deg(float)
	write (*,*) float

	a=1
	b=crt2sph(a)
	write (*,*) crt2sph(a), sph2crt(b)

	b=crt2cyl(a)
	write (*,*) crt2cyl(a), cyl2crt(b)

	! ======================================
	write (*,*) timeStamp()

	! ======================================
	int = getTime()
	write (*,*) "Wait for a moment ..."
	do i = 1, 10000 ! dummy loop
		float = i/sin(i/3.)**2.
	end do
	write (*,*) elapsedTime(getTime()-int)

	!$ float8 = OMP_get_wtime()
	!$ write (*,*) "Wait for 1 sec ..."
	!$ do i = 1, 10000 ! dummy loop
	!$	float = i/sin(i/3.)**2.
	!$ end do
	!$ write (*,*) elapsedTime(OMP_get_wtime()-float8)

	! ======================================
	ave=0.
	err=0.
	do i = 1, 100
		float = real(i)
		call variance(float, ave, err, i)

	end do

	! Transform to true variance
	err = merge(err/(i-1), -1., i>1)

	write (*,*) ave, err

	! ======================================
	string="message #comment"
	write (*,*) trim(string), " -> ", stripComment(string, "#")

	string="path/to/file.txt"
	write (*,*) trim(string), " -> ", basename(string)
	write (*,*) trim(string), " -> ", extension(string)
	write (*,*) trim(string), " -> ", nextFreeName(string)

	! call backup (string)

	! ======================================
	string = "	"
	write (*,*) isEmpty(string), "'"//trim(string)//"'"
	string = "a      "
	write (*,*) isEmpty(string), "'"//trim(string)//"'"

	string = "1,2,3,4"
	write (*,*) isWord(string), trim(string)
	string = "1,a,3,4"
	write (*,*) isWord(string), trim(string)

	string="This is bad."
	write (*,*) trim(string), " -> ", trim(replaceText(string, "bad", "good"))

	string="|	|"
	write (*,*) trim(tab2space(string)), trim(string)

	string="THIS is FOO bar."
	write (*,*) trim(string), " -> ", toUpper(string)
	write (*,*) trim(string), " -> ", toLower(string)

	! ======================================

	! Different combos
	call printOpt ("-a")
	call printOpt ("-b", "Argument.")
	call printOpt ("-c", "Argument.", TYPE="int")
	call printOpt ("-d", "Argument.", TYPE="int", VALUE=1)
	call printOpt ("-e", "Argument.", VALUE=1)
	call printOpt ("-f", VALUE=1)
	call printOpt ("-g", TYPE="int", VALUE=1)
	! What if argument is too long
	call printOpt ("1234567890", "123456789012345678901234567890", TYPE="1234567890", VALUE="12345678901234567890")
	! Different types
	call printOpt ("-x", TYPE="int", VALUE=1)
	call printOpt ("-x", TYPE="real", VALUE=1.)
	call printOpt ("-x", TYPE="double", VALUE=1.0d0)
	call printOpt ("-x", TYPE="bool", VALUE=.true.)
	call printOpt ("-x", TYPE="char", VALUE="abcd")

	! ======================================

	int = 10*1000
	do i = 0, int
		float = real(i)/int
		call progressBar(float, SIZE=30, MESSAGE="Please wait.")

	end do

	! ======================================

	! Curve with slope 2. and intercept -1.
	do i = 1, size(x)
		x(i) = real(i)
		y(i) = real(i)*2.-1.

	end do

	write (*,*) linest(x(:), y(:), d=dr, R2=R2)
	write (*,*) dr, R2

	call exit (0)
end program example
