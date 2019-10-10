module xslib_array

	interface findCrossOver
		module procedure findCrossOver_INT, findCrossOver_REAL
	end interface

	interface findKClosest
		module procedure findKClosest_INT, findKClosest_REAL
	end interface

	! Generic swap routine
	interface swap
		module procedure swap_INT, swap_CHAR, swap_REAL
	end interface swap


contains


	! Returns simple linear regresion (SLR) of x,y as C(:) = [k,n]
	function linest (x, y, d, R2) result (c)
		implicit none
		real, intent(in)						:: x(:), y(:)
		real, intent(out), optional	:: d(2), R2
		real												:: c(2)
		! internal
		real 												:: np, AveX, AveY
		real 												:: Sxx, Syy, Sxy

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

	! This function return K closest elements to VAL in ARRAY(:).
	function findKClosest_INT (val, array, k) result(r)
		implicit none
		integer, intent(in) :: val, array(:)
		integer, intent(in)	:: k
		integer							:: r(k)
		! Internal
		integer 						:: i, n, right, left, cnt

		! Find nearest point
		n = size(array)
		! Error check
		if (k > n) return

		! Find nearest point to the left
		left = findCrossOver (array(:), 1, n, val)
		right = left+1
		cnt = 1

		! Check left and right from the VAL to find next nearest number
		do while (left >= 1 .and. right <= n .and. cnt <= k)
			! Left side is closer to VAL
			if (val-array(left) < array(right)-val) then
				r(cnt) = left
				left = left-1

			! Right side is closer to VAL
			else
				r(cnt) = right
				right = right+1

			end if
			cnt = cnt+1

		end do

		! All remaining values are to the left
		do while (left >= 1 .and. cnt <= k)
			r(cnt) = left
			left = left-1
			cnt = cnt+1

		end do

		! All remaining values are to the left
		do while (right <= n .and. cnt <= k)
			r(cnt) = right
			right = right+1
			cnt = cnt+1

		end do

		return
	end function findKClosest_INT

	function findKClosest_REAL (val, array, k) result(r)
		implicit none
		real, intent(in) 	:: val, array(:)
		integer, intent(in)	:: k
		real				:: r(k)
		! Internal
		integer :: i, n, right, left, cnt

		! Find nearest point
		n = size(array)
		! Error check
		if (k > n) return

		! Find nearest point to the left
		left = findCrossOver (array(:), 1, n, val)
		right = left+1
		cnt = 1

		! Check left and right from the VAL to find next nearest number
		do while (left >= 1 .and. right <= n .and. cnt <= k)
			! Left side is closer to VAL
			if (val-array(left) < array(right)-val) then
				r(cnt) = left
				left = left-1

			! Right side is closer to VAL
			else
				r(cnt) = right
				right = right+1

			end if
			cnt = cnt+1

		end do

		! All remaining values are to the left
		do while (left >= 1 .and. cnt <= k)
			r(cnt) = left
			left = left-1
			cnt = cnt+1

		end do

		! All remaining values are to the left
		do while (right <= n .and. cnt <= k)
			r(cnt) = right
			right = right+1
			cnt = cnt+1

		end do

		return
	end function findKClosest_REAL

	! Function to find the cross over point (the point
	! which elements are smaller than or equal to x and after which greater than x
	recursive function findCrossOver_INT (array, low, high, x) result (r)
		implicit none
		integer, intent(in) :: array(:), x
		integer							:: mid, low, high, r

		! Bigger than all
		if (x >= array(high)) then
			r = high

		! Smaller than all
		else if (x < array(low)) then
			r = low

		else
			mid = (low+high)/2

			if (x <= array(mid) .and. x > array(mid+1)) then
				r = mid

			else if (x > array(mid)) then
				r = findCrossOver_int(array(:), mid+1, high, x)

			else !(x < array(mid))
				r = findCrossOver_int(array(:), low, mid, x)

			end if
		end if

		return
	end function findCrossOver_INT

	recursive function findCrossOver_REAL (array, low, high, x) result (r)
		implicit none
		real, intent(in)	:: array(:), x
		integer						:: mid, low, high, r

		! Bigger than all
		if (x >= array(high)) then
			r = high

		! Smaller than all
		else if (x < array(low)) then
			r = low

		else
			mid = (low+high)/2

			if (x <= array(mid) .and. x > array(mid+1)) then
				r = mid

			else if (x > array(mid)) then
				r = findcrossover_real(array, mid+1, high, x)

			else !(x < array(mid))
				r = findcrossover_real(array, low, mid, x)

			end if
		end if

		return
	end function findCrossOver_REAL

	! Swap values of A and B
	subroutine swap_INT (a, b)
		integer, intent(in out) :: a, b
		a = a+b
		b = a-b
		a = a-b
	end subroutine swap_int

	subroutine swap_REAL (a, b)
		real, intent(in out) :: a, b
		a = a+b
		b = a-b
		a = a-b
	end subroutine swap_real

	subroutine swap_CHAR (a, b)
		character(*), intent(in out) :: a, b
		character(len(a)) :: temp
		temp = a
		a = b
		b = temp
	end subroutine swap_char

end module xslib_array
