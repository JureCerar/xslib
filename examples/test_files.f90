program example
	use xslib
	!$ use omp_lib
	implicit none
	! Derived type
	type(gro_file)		:: gro
	type(pdb_file)		:: pdb
	type(xyz_file)		:: xyz
	type(trj_file)		:: trj
	type(frame_file)	:: frame
	! Data files
	type(pdh_file)		:: pdh
	type(csv_file)		:: csv
	! Supporting
	type(ndx_file)		:: ndx
	type(tpl_file)		:: tpl
	! Internal
	real, allocatable						:: array(:,:)
	character*128, allocatable	:: header(:)
	integer											:: i, nframes, stat
	character*128								:: file
	integer           					:: first, last, stride
	! OpenMP
	integer						:: tid=0, nthreads=1
	real*8						:: time

	!$ nthreads = OMP_get_max_threads()
	time = get_wtime()

	! ====================================
	file = "conf.gro"
	write (*,*) trim(file)

	call gro%open(file)

	write (*,*) gro%nframes
	write (*,*) gro%box()
	write (*,*) gro%natoms()

	do i = 1, gro%nframes
		stat = gro%read_next()
		write (*,*) gro%frameArray(1)%coor(:,1)
		write (*,*) gro%frameArray(1)%box(:)

	end do

	call gro%close()
	! ====================================
	file = "conf.pdb"
	write (*,*) trim(file)

	call pdb%open(file)

	write (*,*) pdb%nframes
	write (*,*) pdb%box()
	write (*,*) pdb%natoms()

	do i = 1, pdb%nframes
		stat = pdb%read_next(ONLYCOOR=.true.)
		! write (*,*) pdb%frameArray(1)%coor(:,1)
		write (*,*) pdb%frameArray(1)%box

	end do

	call pdb%close()

	! ====================================
	file = "conf.xtc"
	write (*,*) trim(file)

	call trj%open(file)

	write (*,*) trj%nframes
	! write (*,*) trj%box(1)
	write (*,*) trj%natoms()

	do i = 1, trj%nframes
		stat = trj%read_next()
		write (*,*) trj%box(1)
		write (*,*) trj%frameArray(1)%coor(:,1)

	end do

	call trj%close()

	! ====================================
	file = "conf.xyz"
	write (*,*) trim(file)

	call xyz%open(file)

	write (*,*) xyz%nframes
	write (*,*) xyz%box()
	write (*,*) xyz%natoms()

	do i = 1, xyz%nframes
		stat = xyz%read_next()
		write (*,*) xyz%frameArray(1)%coor(:,1)

	end do

	call xyz%close()

	! ====================================
	file = "conf.gro" !.pdb / .gro / .xtc / .xyz
	write (*,*) trim(file)

	call frame%open(file)

	nframes = frame%nframes()

	write (*,*) frame%nframes()
	write (*,*) frame%get_box()
	write (*,*) frame%get_natoms()
	!$OMP PARALLEL NUM_THREADS(nthreads) PRIVATE(tid, stat) FIRSTPRIVATE(frame)
	!$ tid = OMP_get_thread_num()

	!$OMP DO SCHEDULE (dynamic)
	do i = 1, nframes
		write (*,*) "Thread "//str(tid)//" reading frame: "//str(i)//"/"//str(nframes)
		!$OMP CRITICAL
		stat = frame%read_next()
		!$OMP END CRITICAL
		write (*,*) frame%coor(:,1)

	end do
	!$OMP END DO
	!$OMP END PARALLEL

	call frame%close()

	! ====================================
	file = "conf.gro" !.pdb / .gro / .xtc / .xyz
	write (*,*) trim(file)

	first = 10
	last = 20
	stride = 2

	call frame%open(file)
	call frame%set(FIRST=first, LAST=last, STRIDE=stride)
	do i = first, last, stride
		stat = frame%read_next()
		if (stat/=0) write (*,*) i, "=", frame%coor(1,1)
	end do
	call frame%close()

	! ====================================
	file = "index.ndx"
	write (*,*) trim(file)

	call ndx%read(file)
	call ndx%write()

	write (*,"(a)") "Available static index groups:"
	do i = 1, ndx%ngroups
	write (*,"(x,a)") "Group "//str(i)//" '"//ndx%group(i)%title//"' ("// &
		& str(ndx%group(i)%natoms)//" atoms)"
	end do

	! ====================================
	file = "temp.tpl"
	write (*,*) trim(file)

	call tpl%read(file)
	call tpl%write()

	! ====================================
	file = "data.pdh"
	write (*,*) trim(file)

	call pdh%read(file)
	call pdh%write()

	! ====================================
	file = "data.csv"
	write (*,*) trim(file)

	call csv%read(file, DATA=array, HEADER=header)
	write (*,*) size(array,1), size(array,2)
	call csv%write(array(:,:), HEADER=header(:), DELIMITER=";")

	! ====================================
	write (*,*)
	write (*,*) "Finished."
	write (*,*) "Elapsed time: ", write_time(get_wtime()-time)

	call exit (0)
end program example
