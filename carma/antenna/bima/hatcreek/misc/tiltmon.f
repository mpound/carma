c  TILTMON.F
c
c	routine to rapidly measure tiltmeter values
c	(updated for 9 ants jun 95)	jrf - june 93
c
	character*80 error
	character units*4, name*16
	real atod,tm1(9)
        integer*4 day,mon,year,ticks
	integer iant(9),nint,ncount
	integer tiltadd(2) /1092,1093/
        real*8 ut,lst,s1(9),m1(9)
	data iant/1,2,3,4,5,6,7,8,9/,nint/50/,ncount/1500/

	call mbusopen(error)

        OPEN(1,FILE='tiltmon.dat',err=98,ACCESS='APPEND',STATUS='NEW')

	call get_times(ticks,2,mon,day,year,ut,lst,error)
	write(1,900) mon,day,year
	write(1,910)
	type 900,mon,day,year
	type 910
  
  900	format(' #  tiltmonitor (tiltmon.dat)  date = ',i2,'/',i2,'/',i2)
  910	format(' #     secs       1        2        3        4        5 ',
	1 '       6        7        8        9')


	do i=1,ncount	! begin master loop

	   call get_times(ticks,2,mon,day,year,ut,lst,error)
	   ut=ut*3.81972+16
	   ut=(ut-int(ut))*3600.

c   measure tiltmeters (only 1)
	     do j=1,9
		tm1(j)=atod(iant(j),tiltadd(1),iraw,units,name,error)
c   gather statistics
                m1(j)=m1(j)+tm1(j)
                s1(j)=s1(j)+tm1(j)*tm1(j)
             end do
 
c	     call wait_ticks(1)

	   type 920,i,ut,tm1
	   write(1,920) i,ut,tm1
  920	   format(i4,x,f9.3,9f9.3)
	end do
					! over main loop
c  calculate statistics
	do j=1,9
		m1(j)=m1(j)/ncount			! mean
		s1(j)=sqrt((s1(j)/ncount)-m1(j)*m1(j))	! sigma
	end do
	type 930, m1,s1
	write(1,930) m1,s1
  930	format(' # mean = ',9f9.4,/,' # rms  = ',9f9.4)

	close(1)
  98   	type *, 'File "tiltmon.dat" exists; delete or rename it'
	stop ' - tiltmon ends - '

	end
