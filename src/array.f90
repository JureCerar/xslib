module xslib_array

	interface findCrossOver
		module procedure findCrossOver_int, findCrossOver_real
	end interface

	interface findKClosest
		module procedure findKClosest_int, findKClosest_real
	end interface

	! Generic swap routine
	interface swap
		module procedure swap_INT, swap_CHAR, swap_REAL
	end interface swap


contains

	! This function return K closest elements to VAL in ARRAY(:).
	function findKClosest_int (val, array, k) result(r)
		implicit none
		integer, intent(in) :: val, array(:)
		integer, intent(in)	:: k
		integer				:: r(k)
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
	end function findKClosest_int

	function findKClosest_real (val, array, k) result(r)
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
	end function findKClosest_real

	! Function to find the cross over point (the point
	! which elements are smaller than or equal to x and after which greater than x
	recursive function findCrossOver_int (array, low, high, x) result (r)
		implicit none
		integer, intent(in) :: array(:), x
		integer				:: mid, low, high, r

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
	end function findCrossOver_int

	recursive function findCrossOver_real (array, low, high, x) result (r)
		implicit none
		real, intent(in)	:: array(:), x
		integer				:: mid, low, high, r

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
	end function findCrossOver_real

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
