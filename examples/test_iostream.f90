! program main
!   use xslib_iostream
!   implicit none
!   integer         :: unit, pos, stat
!   character(128)  :: buffer
!
!   ! Open file as formatted stream
!   call sfopen( UNIT=unit, FILE="data.txt", STATUS="old", ACTION="read", IOSTAT=stat )
!   if ( stat /= 0 ) error stop
!
!   ! Skip 2 lines
!   read (unit,"(a)") buffer
!   read (unit,"(a)") buffer
!
!   ! Store position
!   pos = sftell(unit)
!
!   ! Read & write next line
!   read (unit,"(a)") buffer
!   write (*,*) "buffer=", trim(buffer)
!
!   ! Return to previous position
!   call sfseek(unit,pos,0)
!
!   ! Read & write next line (should be same output)
!   read (unit,"(a)") buffer
!   write (*,*) "buffer=", trim(buffer)
!
!   ! Close file stream
!   call sfclose (UNIT=unit)
!
!   call sfseek(unit,pos,0,STATUS=stat)
!   print *, stat
!   print *, sftell(unit)
!
!
! end program main
! 
! program main
!   use xslib
!   implicit none
!   type(pdb_t) :: pdb
!   integer     :: i, offset, stat

!   stat = pdb%read("EAN_NO3_128.pdb")
!   if ( stat /= 0 ) error stop

!   do i = 1, pdb%frame(1)%natoms
!     if ( pdb%frame(1)%ch(i) == "B" ) pdb%frame(1)%resn(i) = pdb%frame(1)%resn(i)+128

!   end do

!   stat = pdb%write("out.pdb")
!   if ( stat /= 0 ) error stop

! end program main





! program main
!   use xslib_iostream
!   implicit none
!   integer :: unit, stat, pos
!   character(128) :: buffer
!
!   call fopen(UNIT=unit,FILE="data.txt",IOSTAT=stat)
!   if ( stat /= 0 ) error stop
!
!   read (unit,*) buffer
!   read (unit,*) buffer
!
!   pos = ftell(unit)
!   ! write (*,*) "pos=", pos
!   read (unit,*) buffer
!   write (*,*) "buffer=", trim(buffer)
!
!   call fseek(unit,pos,SEEK_SET)
!
!   pos = ftell(unit)
!   write (*,*) "pos=", pos
!   read (unit,*) buffer
!   write (*,*) "buffer=", trim(buffer)
!
!   call fclose(unit,IOSTAT=stat)
!   if ( stat /= 0 ) error stop
!
! end program main