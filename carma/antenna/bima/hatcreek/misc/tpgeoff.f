c	routine to measure total power (new detectors)
c								jrf - oct 95
	character*80 error
	character units*4, name*16
	real atod,tp(9),tpi(2)
        integer*4 day,mon,year,ticks
	integer iant(9),nint,ncount,address
        real*8 ut,lst,s(11),m(11),p(11)
	data iant/1,2,3,4,5,6,7,8,9/,nint/32/,ncount/12000/

	call mbusopen(error)

        OPEN(1,FILE='tpnew.dat',err=98,ACCESS='APPEND',STATUS='UNKNOWN')

	call get_times(ticks,2,mon,day,year,ut,lst,error)
	write(1,900) mon,day,year,nint
	write(1,910)
	type 900,mon,day,year,nint
	type 910
  
  900	format(' #  tpmonitor (tpnew.dat)  date = ',i2,'/',i2,'/',i2,' integration time = ', i3,' ticks')
  910	format(' #     hour       tp1      tp2      tp3      tp4      tp5      tp6      tp7      tp8      tp9      TP2      TP8')

	do i=1,ncount	! begin master loop

c   zero arrays each major cycle
	   do j=1,9
		tp(j) = 0.
	   end do
	   do j=1,2
		tpi(j) = 0.
	   end do

	   call get_times(ticks,2,mon,day,year,ut,lst,error)
	   ut=ut*3.81972+16
	   if(ut.ge.24.0) ut=ut-24

c   tp (original a/d address 1094; integrating = 1106) - sample once per nint ticks
	     do j=1,9
		address=1106
		if((j.eq.1).or.(j.eq.3)) address=1094
		tp(j)=tp(j)+atod(iant(j),address,iraw,units,name,error)
	     end do
	     call wait_ticks(nint)

c   TP msmt (integrating a/d address 1106) for ants 2 & 8 - measure once only each nint cycle
	   tpi(1)=atod(2,1106,iraw,units,name,error)
	   tpi(2)=atod(8,1106,iraw,units,name,error)

c   average above to form integrated values, and calculate statistical quantities needed later
	   do j=1,9
		m(j)=m(j)+tp(j)
		s(j)=s(j)+tp(j)*tp(j)
	   end do
	   do j=10,11
		k = j - 9
		m(j)=m(j)+tpi(k)
		s(j)=s(j)+tpi(k)*tpi(k)
	   end do
c
	   type 920,i,ut,tp,tpi
	   write(1,920) i,ut,tp,tpi
  920	   format(i4,x,f9.5,11f9.5)
	end do
					! over main loop
c  calculate statistics
	do j=1,11
		m(j)=m(j)/ncount			! mean
		s(j)=sqrt((s(j)/ncount)-m(j)*m(j))	! sigma
		p(j)=s(j)*10000/m(j)			! % of mean
	end do
	type 930, m,s,p
	write(1,930) m,s,p
  930	format(' # mean = ',11f9.4,/,' # sig  = ',11f9.6,/,' # %10-4= ',
	1	11f9.1)

	close(1)
  98   	stop ' - tpnew ends - '

	end
