	program vlbi_pause

	implicit none

	character*80 error
        integer*4 day,mon,year,ticks
        real*8 ut,lst
	real*8 offs,stime
	character*20 sut
	integer*4 sday,shour,smin
	integer i,j


	call keyini
	call keya('ut',sut,'0000000')
	call keyd('offset',offs,0d0)

	read (sut(1:3),'(i3)') sday
	read (sut(4:5),'(i2)') shour
	read (sut(6:7),'(i2)') smin
	stime=shour+smin/60.+(offs)/60.	

	i=0
			
10	call mbusopen(error)
	call get_times(ticks,2,mon,day,year,ut,lst,error)
	ut=ut*3.81972
	if(ut.ge.24.0) ut=ut-24
	
	if (ut .ge. stime) goto 999

	if (i .eq. 0) then
		write (*,*) 'VPAUSE:  Waiting for ut=',stime
		i=1
	endif
	call wait_ticks(200)

	goto 10

999	continue

	end
