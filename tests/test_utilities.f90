program example
	use xslib
	!$ use omp_lib
	implicit none
	integer				:: i, n, int
	integer*8			:: int8
	real 				:: float
	real*8				:: float8
	real, dimension(3) 	:: a, b, c, d
	real, dimension(32)	:: x, y
	real 				:: dr(2), R2
	real				:: ave, err
	logical				:: bool
	complex 			:: compl
	character*128		:: string

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
	write (*,*) "Wait for 1 sec ..."
	call sleep (1)
	write (*,*) elapsedTime(getTime()-int)

	!$ float8 = OMP_get_wtime()
	!$ write (*,*) "Wait for 1 sec ..."
	!$ call sleep (1)
	!$ write (*,*) elapsedTime(OMP_get_wtime()-float8)

	! ======================================
	ave=0.
	err=0.
	do i = 1, 100
		float = real(i)
		call variance(float, ave, err, i)

	end do

	! Transform to true variance
	if (i > 1) then
		err = err/(i-1)
	else
		err = -1
	end if

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
