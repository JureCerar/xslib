program main
	use xslib
	implicit none

	call test_gro ("conf.gro")
	call test_pdb ("conf.pdb")
	call test_xyz ("conf.xyz")
	call test_trj ("conf.xtc") ! .OR. "conf.trr"
	call test_frame ("conf.gro") ! Any file

	! ---------------------
	call test_ndx ("index.ndx")
	call test_tpl ("temp.tpl")

	! ---------------------
	call test_pdh ("data.pdh")
	call test_csv ("data.csv")

	call exit (0)
contains

	subroutine test_gro (file)
		implicit none
		type(gro_file)	:: gro
		character*(*)		:: file
		integer					:: i, stat

		write (*,*) "Test GRO ..."

		call gro%read(file)

		! -----------------

		call gro%open(file)
		call gro%set(FIRST=10, LAST=20, STRIDE=2)

		write (*,*) gro%nframes
		write (*,*) gro%box()
		write (*,*) gro%natoms()

		do i = 1, gro%nframes
			stat = gro%read_next()
			call gro%write()
		end do

		call gro%close()

		return
	end subroutine test_gro

	subroutine test_pdb (file)
		implicit none
		type(pdb_file)	:: pdb
		character*(*)		:: file
		integer					:: i, stat

		write (*,*) "Test PDB ..."

		call pdb%read(file)

		! ------------------------------

		call pdb%open(file)
		call pdb%set(FIRST=10, LAST=20, STRIDE=2)

		write (*,*) pdb%nframes
		write (*,*) pdb%box()
		write (*,*) pdb%natoms()

		do i = 1, pdb%nframes
			stat = pdb%read_next()
			call pdb%write()
		end do

		call pdb%close()

		return
	end subroutine test_pdb

	subroutine test_trj (file)
		implicit none
		type(trj_file)	:: trj
		character*(*)		:: file
		integer					:: i, stat

		write (*,*) "Test TRJ ..."

		call trj%read(file)

		! ------------------------------

		call trj%open(file)
		call trj%set(FIRST=10, LAST=20, STRIDE=2)

		write (*,*) trj%nframes
		write (*,*) trj%box(1)
		write (*,*) trj%natoms()

		do i = 1, trj%nframes
			stat = trj%read_next()
			! call trj%write()
		end do

		call trj%close()

		return
	end subroutine test_trj

	subroutine test_xyz (file)
		implicit none
		type(xyz_file)	:: xyz
		character*(*)		:: file
		integer					:: i, stat

		write (*,*) "Test XYZ ..."

		call xyz%read(file)

		! ------------------------------

		call xyz%open(file)
		call xyz%set(FIRST=10, LAST=20, STRIDE=2)

		write (*,*) xyz%nframes
		write (*,*) xyz%box()
		write (*,*) xyz%natoms()

		do i = 1, xyz%nframes
			stat = xyz%read_next()
			call xyz%write()
		end do

		call xyz%close()

		return
	end subroutine test_xyz

	subroutine test_frame (file)
		!$ use omp_lib
		implicit none
		type(frame_file)	:: frame
		character*(*)			:: file
		integer						:: i, np, stat, tid=0, nthreads=1

		write (*,*) "Test FRAME ..."

		call frame%open(file)
		call frame%set(FIRST=10, LAST=20, STRIDE=2)

		write (*,*) frame%nframes()
		write (*,*) frame%get_box()
		write (*,*) frame%get_natoms()

		np = frame%nframes()
		!$ nthreads = OMP_get_max_threads()

		!$OMP PARALLEL NUM_THREADS(nthreads) PRIVATE(tid, stat) FIRSTPRIVATE(frame)
		!$ tid = OMP_get_thread_num()

		!$OMP DO SCHEDULE (dynamic)
		do i = 1, np
			write (*,*) "Thread ",str(tid)," reading frame: ",str(i),"/",str(np)
			!$OMP CRITICAL
				stat = frame%read_next()
			!$OMP END CRITICAL
			write (*,*) frame%coor(:,1), frame%box(:)
		end do
		!$OMP END DO
		!$OMP END PARALLEL

		call frame%close()

		return
	end subroutine test_frame

	subroutine test_ndx (file)
		implicit none
		type(ndx_file)	:: ndx
		character*(*)		:: file

		write (*,*) "Test NDX ..."

		call ndx%read(file)
		call ndx%write()

		call ndx%display()

		return
	end subroutine test_ndx

	subroutine test_tpl (file)
		implicit none
		type(tpl_file)	:: tpl
		type(ndx_file)	:: ndx
		character*(*)		:: file

		write (*,*) "Test TPL ..."

		call tpl%read(file)
		call tpl%write()

		! ------------------------

		call ndx%tpl2ndx(tpl, SYSTEM=.true.)
		call ndx%display()

		return
	end subroutine test_tpl

	subroutine test_pdh (file)
		implicit none
		type(pdh_file)	:: pdh
		character*(*)		:: file

		write (*,*) "Test PDH ..."

		call pdh%read(file)
		call pdh%write()

		! -------------------

		call pdh%bining(400)
		call pdh%normalize()

		! call pdh%smearing(WIDTH=, LENGTH=)

		return
	end subroutine test_pdh

	subroutine test_csv (file)
		implicit none
		type(csv_file)							:: csv
		character*(*)								:: file
		real, allocatable						:: array(:,:)
		character*128, allocatable	:: header(:)

		write (*,*) "Test CSV ..."

		call csv%read(file, DATA=array, HEADER=header)
		write (*,*) size(array,1), size(array,2)

		call csv%write(array(:,:), HEADER=header(:), DELIMITER="--")

		return
	end subroutine test_csv

end program main
