module xslib_utilities
	implicit none

	interface variance
		module procedure variance_ARRAY, variance_REAL, variance_REAL8, variance_ARRAY8
	end interface variance

	interface elapsedTime
		module procedure elapsedTime_INT, elapsedTime_FLOAT8
	end interface elapsedTime


contains

	! =======================================================
	! Basic linear algebra

	! Calculates vector product: v x u
	function cross (v,u)
		implicit none
		real	:: cross(3)
		real	:: v(3), u(3)

		cross(1) = v(2)*u(3)-v(3)*u(2)
		cross(2) = v(3)*u(1)-v(1)*u(3)
		cross(3) = v(1)*u(2)-v(2)*u(1)

		return
	end function

	! Reduces distance acording to minimal image convention
	function minImg (r, box)
		implicit none
		real, dimension(3)	:: r, box, minimg

		minimg(:) = r(:)-box(:)*floor(r(:)/box(:)+0.5)

		return
	end function minImg

	! Calculates distance between two points.
	function getDistance (a, b) result (distance)
		implicit none
		real, dimension(3)	:: a, b
		real				:: distance

		! distance = sqrt(dot_product(a,b))
		! distance = norm2(a-b)
		distance = sqrt(sum((a-b)**2))

		return
	end function getDistance

	! Caluclates angle between three points.
	function getAngle (a, b, c) result (angle)
		implicit none
		real, dimension(3)	:: a, b, c
		real				:: l1, l2
		real				:: angle
		real, dimension(3)	:: r1, r2

		!     a
		!	 /
		!	b -- c

		! Center A & C to reference B and account for Min. image
		r1(:) = a(:)-b(:)
		r2(:) = c(:)-b(:)

		! Euclidian norm
		l1 = sqrt(dot_product(r1,r1))
		l2 = sqrt(dot_product(r2,r2))

		! Cosine rule: a*b = |a|*|b|*cos(theta)
		angle = acos(dot_product(r1(:), r2(:))/(l1*l2))

		return
	end function getAngle

	! Get dihedral angle (theta) between four points
	function getDihedral (a, b, c, d) result (dihedral)
		implicit none
		real, dimension(3)	:: a, b, c, d
		real 				:: dihedral
		real, dimension(3)	:: b1, b2, b3, n1, n2, n3
		real				:: x, y

		!	a		  d
		!	 \		 /
		!	  b --- c

		! HOW TO: https://math.stackexchange.com/questions/47059/how-do-i-calculate-a-dihedral-angle-given-cartesian-coordinates

		! Site-to-Site vectors
		b1 = b-a
		b2 = c-a
		b3 = d-a

		! Normals
		n1 = cross(b1,b2)
		n1 = n1/sqrt(dot_product(n1,n1))

		n2 = cross(b2, b3)
		n2 = n2/sqrt(dot_product(n2,n2))

		n3 = cross(n1,b2/sqrt(dot_product(b2,b2)))

		! x and y
		x = dot_product(n1, n2)
		y = dot_product(n3, n2)

		! Angle
		dihedral = atan2(y, x)

		! NOTE: acos() is numerically unstable for angles close to /(pi) or 0.
		! r1(:) = cross(a(:)-b(:), b(:)-c(:))
		! r2(:) = cross(b(:)-c(:), c(:)-d(:))
		! dihedral = acos(dot_product(r1, r2)/(norm2(r1)*norm2(r2)))

		return
	end function getDihedral

	! =======================================================

	! Transform degrees to radians
	function deg2rad (x) result (y)
		implicit none
		real	:: x, y
		! rad = deg/180*pi
		y = x/180.*4.*atan(1.)
		return
	end function deg2rad

	! Transform rad to degrees
	function rad2deg (x) result (y)
		implicit none
		real	:: x, y
		! deg = rad/pi*180
		y = x/(4.*atan(1.))*180.
		return
	end function rad2deg

	! Cartesian to spherical: (x, y, z) -> (r, theta phi)
	function crt2sph (crt) result (sph)
		implicit none
		real	:: crt(3)
		real	:: sph(3)
		! r = sqrt(x**2+y**2+z**2)
		sph(1) = sqrt(sum(crt(:)**2))
		! theta = acos(z/r)
		if (sph(1) /= 0.) then
			sph(2) = acos(crt(3)/sph(1))
		else
			sph(2) = 0.0
		end if
		! phi = atan2(y/x)
		if (crt(1) /= 0.) then
			sph(3) = atan2(crt(2),crt(1))
		else
			sph(3) = 0.0
		end if
		return
	end function crt2sph

	! Spherical to cartesian: (r, theta phi)-> (x, y, z)
	function sph2crt (sph) result (crt)
		implicit none
		real	sph(3)
		real	:: crt(3)
		! x = r*sin(theta)*cos(phi)
		crt(1) = sph(1)*sin(sph(2))*cos(sph(3))
		! y = r*sin(theta)*sin(phi)
		crt(2) = sph(1)*sin(sph(2))*sin(sph(3))
		! z = r*cos(theta)
		crt(3) = sph(1)*cos(sph(2))
		return
	end function sph2crt

	! Cartesian to cylindrical: (x, y, z) -> (r, theta, z)
	function crt2cyl (crt) result (cyl)
		implicit none
		real	:: crt(3)
		real	:: cyl(3)
		! r = sqrt(x**2+y**2)
		cyl(1) = sqrt(crt(1)**2+crt(2)**2)
		! theta =  atan2(y/x)
		if (cyl(1) /= 0.) then
			cyl(2) = atan2(crt(2), crt(1))
		else
			cyl(2) = 0.
		end if
		! z = z
		cyl(3) = crt(3)
		return
	end function crt2cyl

	! Cylindrical to cartesian :  (r, theta, z) -> (x, y, z)
	function cyl2crt (cyl) result (crt)
		implicit none
		real	:: cyl(3)
		real	:: crt(3)
		! x = r*cos(theta)
		crt(1) = cyl(1)*cos(cyl(2))
		! y = r*sin(theta)
		crt(2) = cyl(1)*sin(cyl(2))
		! z = z
		crt(3) = cyl(3)
		return
	end function cyl2crt

	! =======================================================
	! Time and timing

	! Returns time and date in format: "hh:nn:ss dd-mm-yyyy".
	function timeStamp () result (string)
		implicit none
		character*19	:: string
		integer			:: time(8)

		call date_and_time (VALUES=time)
		write (string, 200) time(5:7),time(3),time(2),time(1)
		200	format(i2.2,":",i2.2,":",i2.2,x,i2.2,"-",i2.2,"-",i4.4)

		return
	end function timeStamp

	! Function wrapper for system_clock(COUNT=time)
	function getTime () result (time)
		implicit none
		integer :: time

		call system_clock(COUNT=time)

		return
	end function getTime

	! Returns elapsed time as string in format: "ddd:hh:mm:ss.sss".
	! Use with call system_clock(COUNT=time) or getTime()
	function elapsedTime_INT (time) result (string)
		! returns time as string in format ddd:hh:mm:ss.sss
		! 1 day 18 hours 53 minutes 43 seconds 128 millsec = 154423128 millisec
		implicit none
		integer, intent(in)	:: time 	!milisec
		character*16		:: string
		integer				:: days, hours, min, sec, msec

		days 	= int(time/86400000)
		hours 	= int(mod(time/3600000,24))
		min 	= int(mod(time/60000,60))
		sec		= int(mod(time/1000,60))
		msec	= time-days*86400000-hours*3600000-min*60000-sec*1000


		write (string, 200) days, hours, min, sec, msec
		200	format (i3.3, ":", i2.2, ":", i2.2, ":", i2.2, ".", i3.3) ! ddd:hh:mm:ss.sss

		return
	end function elapsedTime_INT

	! Returns elapsed time as string in format: "ddd:hh:mm:ss.sss".
	! Use with OMP_get_wtime()
	function elapsedTime_FLOAT8 (time) result (string)
		! returns time as string in format ddd:hh:mm:ss.sss
		! 1 day 18 hours 53 minutes 43 seconds 128 millsec = 154423.128 sec
		implicit none
		real*8, intent(in)	:: time 	!seconds
		real 				:: itime
		character*16		:: string
		integer				:: days, hours, min, sec, msec

		itime = real(time, KIND=4)

		days 	= itime/(24.*3600.)

		itime 	= mod(itime,(24.*3600.))
		hours 	= itime/3600.

		itime	= mod(itime,(3600.))
		min 	= itime/60.

		itime 	= mod(itime, 60.)
		sec		= itime

		msec	= (itime-int(itime))*1000.

		write (string, 200) days, hours, min, sec, msec
		200	format (i3.3, ":", i2.2, ":", i2.2, ":", i2.2, ".", i3.3) ! ddd:hh:mm:ss.sss

		return
	end function elapsedTime_FLOAT8

	! =======================================================
	! On-line variance algorithm

	! On-line variance calculation for sigle real value.
	subroutine variance_REAL (val, mean, var, n)
		implicit none
		real, intent(in)	:: val
		real, intent(inout)	:: mean, var
		integer, intent(in) :: n
		real :: delta
		delta = val-mean
		mean = mean+delta/real(n)
		var = var+delta*(val-mean)
		return
	end subroutine variance_REAL

	! On-line variance calculation for real array.
	subroutine variance_ARRAY (val, mean, var, n)
		implicit none
		real, intent(in)	:: val(:)
		real, intent(inout)	:: mean(:), var(:)
		integer, intent(in) :: n
		integer :: i, np
		do i = 1, size(val)
			call variance_REAL(val(i), mean(i), var(i), n)
		end do
		return
	end subroutine variance_ARRAY

	! On-line variance calculation for sigle real*8 value.
	subroutine variance_REAL8 (val, mean, var, n)
		implicit none
		real*8, intent(in)		:: val
		real*8, intent(inout)	:: mean, var
		integer, intent(in) 	:: n
		real*8 					:: delta
		delta = val-mean
		mean = mean+delta/real(n,KIND=8)
		var = var+delta*(val-mean)
		return
	end subroutine variance_REAL8

	! On-line variance calculation for real*8 array.
	subroutine variance_ARRAY8 (val, mean, var, n)
		implicit none
		real*8, intent(in)		:: val(:)
		real*8, intent(inout)	:: mean(:), var(:)
		integer, intent(in) 	:: n
		integer 				:: i
		do i = 1, size(val)
			call variance_REAL8(val(i), mean(i), var(i), n)
		end do
		return
	end subroutine variance_ARRAY8

	! =======================================================
	! String operations

	! Returns basename of file. Eg. "/path/to/file.txt" = "file"
	function basename (file) result (base)
		implicit none
		character*(*)				:: file
		character*(:), allocatable	:: base
		integer						:: m, n

		! Find "/" symbol
		m = index(file, "/", BACK=.true.)
		if (m == 0) m = 0

		! Find "." symbol
		n = index(file, ".", BACK=.true.)
		if (n == 0) n = len_trim(file)+1

		! Extract base
		base = file(m+1:n-1)

		return
	end function basename

	! Returns extension of file. Eg. "/path/to/file.txt" = "txt"
	function extension (file) result (ext)
		implicit none
		character*(*)				:: file
		character*(:), allocatable	:: ext
		integer						:: n

		n = index(file, ".", BACK=.true.)
		if (n == 0) then
			ext = ""
		else
			ext = file(n+1:len_trim(file))
		end if

		return
	end function extension

	! Remove all characters after comment sign "cmt". Eg. "xyz #foobar" = "xyz"
	function stripComment (string, cmt) result (res)
		implicit none
		character*(*), intent(in)	:: string
		character*(*), intent(in)	:: cmt
		character*(len(string))		:: res

		character*32				:: fmt
		integer 					:: i

		i = index(string, cmt)

		if (i == 0) then
			! No comment line
			res = string

		else if (i == 1) then
			! Whole line is coment; is empty line
			res = ""

		else
			write (fmt,"(a,i0,a)") "(a", i-1, ")"
			read (string, fmt) res
			res = trim(res)

		end if

		return
	end function stripComment

	! Retuns next free available file name. Eg. "out.txt" = "out.1.txt"
	function nextFreeName (string) result (res)
		implicit none
		character*(*) 				:: string
		character(:), allocatable	:: res
		character*(len(string)+11)	:: tmp, base, ext
		logical 					:: exists
		integer						:: i

		! initialize
		res=""

		! ! Check default file
		inquire (FILE=trim(string), EXIST=exists)
		if (.not. exists) then
			res = trim(string)
			return

		end if

		! Aquire file base and extention
		base = basename(string)
		ext = extension(string)

		i = 0
		do while (.true.)
			i = i+1
			write (tmp,"(a,'.',i0,'.',a)") trim(base), i, trim(ext)
			inquire(FILE=trim(tmp), EXIST=exists)
			if (.not. exists) then
				res = trim(tmp)
				return

			end if

		end do
	end function nextFreeName

	! Backups all files named "file" as "#file.ext.1#"
	subroutine backup (file)
		use iso_fortran_env
		implicit none
		character*(*), intent(in)	:: file
		character*512				:: temp
		logical						:: exist
		integer						:: i

		! Check if file exists
		inquire (FILE=trim(file), EXIST=exist)
		if (.not. exist) return

		! If it exists rename it to: #file.ext.1#
		do i = 1, 1000
			write (temp, "('#',a,'.',i0,'#')") trim(file), i
			! Check if this 'new' file exists
			inquire (FILE=trim(temp), EXIST=exist)
			if (.not. exist) then
				write (error_unit, "(a)") "File '"//trim(file)//"' already exists. Backing it up as: '"//trim(temp)//"'"
				call rename (trim(file), trim(temp))
				return

			end if
		end do ! for i

		return
	end subroutine backup

	! Check if string is empty - "tab" and "space" does not count
	logical function isEmpty (string)
		implicit none
		character*(*)	:: string
		integer			:: n

		if (verify(trim(string), " 	")==0) then
			isEmpty = .true.

		else
			isEmpty = .false.

		end if

		return
	end function isempty

	! Check if string contains ASCII letters
	logical function isWord (string)
		implicit none
		character*(*)	:: string

		if (scan(trim(string), "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz")/=0) then
			isWord = .true.

		else
			isword = .false.

		end if

		return
	end function isWord

	! Replace "text" with "rep" within string
	function replaceText (string, text, rep) result (out)
		implicit none
		character(*)			:: string, text, rep
		character(len(string))	:: out

		integer	:: i, n

		out = trim(string)
		n = len(text)

		! Do while all text is replaced
		do
			i = index(out, trim(text))
			if (i == 0) exit
			out = out(:i-1) // trim(rep) // out(i+n:)
		end do

		return
	end function replaceText

	! Transforms TAB to 4*SPACE
	function tab2space (string) result(out)
		implicit none
		character(*)			:: string
		character(len(string))	:: out
		integer	:: i, n

		out = trim(string)
		n = len(out)

		do i = 1, n
			if (ichar(out(i:i)) == 9) out = out(:i-1) // "    " // out(i+1:)
		end do

		return
	end function tab2space

	! Return string in all lower case.
	function toLower (string) result(res)
		character(*) 			:: string
		character(len(string))	:: res
		integer, parameter	:: offset=ichar("a")-ichar("A")
		integer				:: i
		character*1			:: chr

		! Initialize
		res=string

		do i = 1, len(string)
			chr = res(i:i)
			! Shift character by offset
			if (chr >= "A" .and. chr <= "Z") res(i:i) = char(ichar(chr)+offset)

		end do

		return
	end function toLower

	! Returns string in all upper case
	function toUpper (string) result(res)
		character(*) 			:: string
		character(len(string))	:: res
		integer, parameter	:: offset=ichar("a")-ichar("A")
		integer				:: i
		character*1			:: chr

		! initialize
		res=string

		do i = 1, len(string)
			chr = res(i:i)
			! Shift character by offset
			if (chr >= "a" .and. chr <= "z") res(i:i) = char(ichar(chr)-offset)

		end do


		return
	end function toUpper

	! Prints progress bar (on the same line). Eg. "50.0% |##--|+message"
	! NOTE: Write to output is "forbiden" until progress reaches 1.0; Use "MESSAGE"
	subroutine progressBar (progress, size, message)
		implicit none
		real, intent(in)					:: progress ! Progres real in rage 0.0 - 1.0
		integer, optional, intent(in)		:: size		! Size of the progress bar
		character(*), intent(in), optional	:: message	! Adittiona message to be printed

		integer			:: i, n, s
		character*(512)	:: bar, msg

		! Initialize
		s=50
		if (present(size)) then
			s = size
			if (s < 1 .or. s > 512-9) s=50
		end if

		msg=""
		if (present(message)) msg=trim(message)

		! Construct "blank" progress bar
		! ???.?%_|-----------|
		bar = ""
		write (bar, "(512a)") "???.?% |", ("-", i = 1, s),"|"

		if (progress >= 0.0 .and. progress <= 1.0) then
			n = nint(s*progress)
			! Add procentage
			write (bar(1:5), "(f5.1)") progress*100
			! Add progress bar
			write (bar(9:9+n-1), "(512a)") ("#", i = 1, n)

			write (*,"(a,a,x,a,$)") char(13), trim(bar), trim(msg)

			! Go to new line
			if (progress == 1.0) write (*,"(x)")

		else
			write (*,"(a,a,x,a,$)") char(13), trim(bar), trim(msg)

		end if

		return
	end subroutine progressBar

	! =======================================================
	! REGRESION OPERATIONS

	! Returns simple linear regresion (SLR) of x,y as C(:) = [k,n]
	function linest (x, y, d, R2) result (c)
		implicit none
		real, intent(in)			:: x(:), y(:)
		real, intent(out), optional	:: d(2), R2
		real						:: c(2)
		! internal
		real :: np, AveX, AveY
		real :: Sxx, Syy, Sxy

		! Initialize
		c = 0.

		! Number of points
		np = size(x)
		if (np < 2 .or. np /= size(y)) then
			! Error
			c = -1.
			return
		end if

		AveX	= sum(x)/np
		AveY	= sum(y)/np
		Sxx		= sum((x(:)-AveX)**2)
		Syy		= sum((y(:)-AveY)**2)
		Sxy 	= sum((x(:)-AveX)*(y(:)-aveY))

		! Result
		c(1) = Sxy/Sxx
		c(2) = AveY-c(1)*AveX

		! Standard deviation
		if (present(d)) then
			if (np > 2) then
				d(1) = sqrt((Syy-c(1)**2*Sxx)/((np-2)*Sxx))
				d(2) = sqrt((Syy-c(1)**2*Sxx)/(np-2))*sqrt(1/(np-sum(x)**2/sum(x**2)))

			else
				d = -1.

			end if
		end if

		! R^2 value
		if (present(R2)) then
			R2 = Sxy**2/(Sxx*Syy)

		end if

		return
	end function linest

end module xslib_utilities
