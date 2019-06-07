c  TILTMON.F
c
c	routine to measure tiltmeter values for both tiltmeters on
c	a single antenna (sampled every tick).		jrf - dec 95
c
	character*80 error
	character units*4, name*16
	real atod,tm1,tm2
        integer*4 day,mon,year,ticks
	integer iant(9),nint,ncount
	integer tiltadd(2) /1092,1093/
        real*8 ut,lst,s1,m1,s2,m2
	data iant/1,2,3,4,5,6,7,8,9/,nint/50/,ncount/150/

	call mbusopen(error)

        OPEN(1,FILE='tiltmon2.dat',err=98,ACCESS='APPEND',STATUS='NEW')

  	type *, 'Enter antenna number:'
	accept *, iant(1)

	call get_times(ticks,2,mon,day,year,ut,lst,error)
	write(1,900) iant(1),mon,day,year
	write(1,910)
	type 900,iant(1),mon,day,year
	type 910

  900	format(' # Ant ',i3,'  tiltmonitor (tiltmon2.dat)  date = ',i2,'/',i2,'/',i2)
  910	format(' #     secs       1        2   tiltmeters')


	do i=1,ncount	! begin master loop

	   call get_times(ticks,2,mon,day,year,ut,lst,error)
	   ut=ut*3.81972+16
	   ut=(ut-int(ut))*3600.

c   measure tiltmeters (both slots for the selected antenna)
		tm1=atod(iant(1),tiltadd(1),iraw,units,name,error)
		tm2=atod(iant(1),tiltadd(2),iraw,units,name,error)

c   gather statistics
                m1=m1+tm1
                s1=s1+tm1*tm1
                m1=m1+tm1
                s1=s1+tm1*tm1
 
c	     call wait_ticks(1)

	   type 920,i,ut,tm1,tm2
	   write(1,920) i,ut,tm1,tm2
  920	   format(i4,x,3f9.3)
	end do
					! over main loop
c  calculate statistics
		m1=m1/ncount			! mean
		s1=sqrt((s1/ncount)-m1*m1)	! sigma
		m2=m2/ncount			! mean
		s2=sqrt((s2/ncount)-m2*m2)	! sigma
	type 930, m1,m2,s1,s2
	write(1,930) m1,s1
  930	format(' # mean = ',2f9.4,/,' # rms  = ',2f9.4)

	close(1)
  98   	type *, 'Delete or rename "tiltmon2.dat" before next run!'
	stop ' - tiltmon2 ends - '

	end
