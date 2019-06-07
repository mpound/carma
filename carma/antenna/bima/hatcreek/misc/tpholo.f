c	routine to measure total power (integ. detectors + azimuth)
c								jrf - jan 98
	character*80 error
	character units*4, name*16
	real atod,tp(10),tpwrtok(12)
        integer*4 day,mon,year,ticks
	integer iant(10),nants,ncount,address
        real*8 ut,lst,s(20),m(20),p(20),azim(12)
	data nants/12/,iant/1,2,3,4,5,6,7,8,9,10/,ncount/1024/

	call mbusopen(error)
	call comgetr('TPWRTOK',tpwrtok,nants,error)

        OPEN(1,FILE='tph.dat',err=98,ACCESS='APPEND',STATUS='UNKNOWN')

	call get_times(ticks,2,mon,day,year,ut,lst,error)
	write(1,900) mon,day,year
	write(1,905) tpwrtok
	write(1,910)
	type 900,mon,day,year
	type 910
  
  900	format(' # tp2monitor (tp2.dat)  date = ',i2,'/',i2,'/',i2,' integration time = ', i3,' ticks')
  905   format(' # TPWRTOK:  ',12f9.2)
  910	format(' #     hour       tp1      tp2      tp3      tp4      tp5      tp6      tp7      tp8      tp9     tpA')

	do i=1,ncount	! begin master loop

	   call get_times(ticks,2,mon,day,year,ut,lst,error)
	   ut=ut*3.81972+16
	   if(ut.ge.24.0) ut=ut-24
	   call comgetd('AZIMTRUE',azim,nants,error)

c   TP msmt (integrating a/d address 1106) - measure once only each .32 seconds
c					(ant 3 does not have integrating card)
	     do j=1,10
		address=1106
		if(j.eq.3) address=1094
		tp(j)=atod(iant(j),address,iraw,units,name,error)
	     end do
	     call wait_ticks(32)	! 0.32 sec time constant for detectors

c   multiply by tpwrtok to convert from volts to kelvins
	   do j=1,10
		m(j)=m(j)+tp(j)
		s(j)=s(j)+tp(j)*tp(j)
		tp(j)=tp(j)*tpwrtok(j)
	   end do
c
	   type 920,i,ut,tp
	   write(1,920) i,ut,tp,azim
  920	   format(i4,x,f9.5,10f9.2,3x,12f12.4)
	end do
					! over main loop
c  calculate statistics
	do j=1,20
		m(j)=m(j)/ncount			! mean
		s(j)=sqrt((s(j)/ncount)-m(j)*m(j))	! sigma
		p(j)=s(j)*10000/m(j)			! % of mean
	end do

c	put statistics in K units (avoided earlier due to large numbers)

           do j=1,10
                m(j)=m(j)*tpwrtok(j)
                s(j)=s(j)*tpwrtok(j)
           end do
           do j=11,20
                k = j - 10
                m(j)=m(j)*tpwrtok(k)
                s(j)=s(j)*tpwrtok(k)
           end do

	type 930, m,s,p
	write(1,930) m,s,p
  930	format(' # mean= ',20f9.2,/,' # sig= ',20f9.2,/,' # %10-4= ',
	1	20f9.1)

	close(1)
  98   	stop ' - tpholo ends - '

	end
