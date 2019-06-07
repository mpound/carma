c	routine to measure correlator room thermistors	
c								jrf - oct 97
	character*80 error
	character units*4, name*16
	real atod,tp(3)
        integer*4 day,mon,year,ticks
	integer iant/10/,ncount,nadd(3)/1024,1025,1026/,nwait/100/
        real*8 ut,lst
	data ncount/1024/

	call mbusopen(error)

        OPEN(1,FILE='Tlab.dat',err=98,ACCESS='APPEND',STATUS='UNKNOWN')

	call get_times(ticks,2,mon,day,year,ut,lst,error)
	write(1,900) mon,day,year
	write(1,910)
	type 900,mon,day,year
	type 910
  
  900	format(' # labtherm (Tlab.dat)  date = ',i2,'/',i2,'/',i2)
  910	format(' #     hour       T1       T2       T3')

	do i=1,ncount						! begin master loop
	   call get_times(ticks,2,mon,day,year,ut,lst,error)
	   ut=ut*3.81972+16
	   if(ut.ge.24.0) ut=ut-24

c   measure thermistors, write out data

	   do j=1,3
		tp(j)=atod(iant,nadd(j),iraw,units,name,error)
	   end do

	   type 920,i,ut,tp
	   write(1,920) i,ut,tp
  920	   format(i4,x,f9.5,3f9.2)
	   call wait_ticks(nwait)
	end do					! over main loop

	close(1)
  98   	stop ' - labtherm ends - '

	end
