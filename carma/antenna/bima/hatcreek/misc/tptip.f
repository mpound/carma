c	routine to measure total power with new detectors for
c	tipping curve measurements (itime=3.84 s).	jrf - oct 95
c
	character*80 error
	character units*4, name*16
	real atod,tp(10),vtok(12)
	integer*4 adr,nint/12/

	call mbusopen(error)
	call comgetr('TPWRTOK',vtok,12,error)

c	zero total power array
	do j=1,10
		tp(j)=0
	end do
c
c   tp (integrating a/d address 1106; use software integration over nint ticks)
c	old antennas use address 1096
c
	   do k=1,nint
	     do j=1,10
		adr=1106
		if(j.eq.3) adr=1094
		tp(j)=tp(j)+atod(j,adr,iraw,units,name,error)
	     end do
	     call wait_ticks(32)
	   end do
c
	   do j=1,10
		tp(j)=tp(j)*vtok(j)/nint
	   end do
c
	   type 920,tp
  920	   format(10f7.2)
c
  98   	stop
	end
