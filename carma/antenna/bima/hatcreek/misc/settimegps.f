c
c= SETTIME  set the system clock
c: basic
c+
	PROGRAM SETTIME

C  SETTIME:	sets the multibus and computer clocks accurately.
c		The system runs in timezone UTC.
c		This version uses the gps time receiver.
c-- 
c	may 91 wh
c	use gps  receiver
c	get year from gps receiver
c	ticks and memory are now on vme   (jul 99)
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='SETTIME (version 4.0 12-jul-99)')

	include '../inc/constants.inc'
	character*80 buffer,error,merror
	character*24 suntime,vmetime,msettime,ctime
	character*4  action
	character*14 angles,anangle
	double precision julian, day1, day2, ut, lst, timesec
	real second
	integer month,day,year,hour,min,systime,time,ticks,tp(2),tunix(2)
	integer year1,year2
	real delta
	integer systimeset
	real*4 dut1,dut2

	integer*4 yd,sec


	call OUTPUT(version)

C  --	read the vme clock and the system clock together
	call getticks(ticks)
	systime = time()
	suntime = ctime(systime)

c  --	calculate time from vme clock using stored quantities-
	CALL READ_VME_TIME_PARAMETERS
	1		(MTICKS,TP,DAY1,DUT1,DAY2,DUT2,MERROR)
	  IF(MERROR(1:2).EQ.'OK') THEN
	    TICKS = TICKS - MTICKS
	    MSETTIME = CTIME(TP(1))
	    TUNIX(1) = TP(1)+TICKS/100
	    TUNIX(2) = TP(2)+MOD(TICKS,100)*10000
	    TUNIX(1) = TUNIX(1)+TUNIX(2)/1000000
	    TUNIX(2) = MOD(TUNIX(2),1000000)
	    VMETIME = CTIME(TUNIX(1))
	  END IF

c	call GPS_OPEN(error)
c	  if (error(1:2).ne.'OK') goto 99


	type *
	TYPE *, 'GPS CLOCK STATUS:'
c  --	force correct mode
c	call GPS_MODE('F',121.4690d0,40.8173d0,1021.4d0,error)
c	  if (error(1:2).ne.'OK') type *, error
	call wait_ticks(100)

c	call GPS_STATUS(error)
c	  if (error(1:2).ne.'OK') type *, error
	
c	call GPS_TIME(year,yd,hour,min,sec,error)
c	  if (error(1:2).ne.'OK') type *, error
c	call watchgps(delta,prec,idummy,error)
c	  if (error(1:2).ne.'OK') type *, error

	TYPE '(a,i4,''+'',I4.4,2x,I3.2,'':'',I2.2,'':'',I2.2)',
	1	 ' Present GPS Date:',year,yd,hour,min,sec

c --	report all of the times
	type *,'UNIX system time:  ',suntime
	if (merror(1:2).eq.'OK') then
	  type *,'Current VME time: ',vmetime
	  TYPE *, ' The Vme-GPS time difference is ',delta,' secs'
	  type *,'Last set time:     ',msettime
	  type 999,'dUT parameters:    ',dut1,' sec at ',day1,' and ',
	1		dut2,' sec at ',day2
999	  format(x,a,f6.3,a,f12.2,a,f6.3,a,f12.2)
	else
	  type *,'Time must be reset because: ',merror(1:40)
	end if
	type *
	type *

c  --	what action or actions do we take?
	type *,'Action? [Reset/Update_dut/RU/Exit] ::'
	accept *,action
	call UCASE(action)

c --	ACTION: update the delta UT parameters from the Naval Circ.
	IF(index(ACTION,'U').gt.0 .or. index(ACTION,'R').gt.0) THEN
	  type *
	  type *,'Enter delta UT offsets from circular:'
	  type *,'Beginning Month,Day,Year,delta_UT [ex:2 20 90 .05]:'
	  accept *,month,day,year1,dut1
	  day1 = julian(day,month,year1,0.d0)
	  type *,'Ending Month,Day,Year,delta_UT [ex:2 20 90 .05]:'
	  accept *,month,day,year2,dut2
	  day2 = julian(day,month,year2,0.d0)
	END IF

c --	ACTION: reset the times using GPS time
	IF(index(ACTION,'R').gt.0) THEN

	  type *,'enter hour min sec month day year'
	  accept *,hour,min,second,month,day,year	  

c	  -- reset vme on next gps tick
c	  call VME_SET(MTICKS,YD,HOUR,MIN,SECOND,P,ERROR)
	   if(error(1:2).ne.'OK') type *,error

c  --	set the UNIX system time 
c --	calculate secs since jan 1,70 for unix (no corr for dUT here)
c --	(year comes from gps_time call above)
	  ut = (hour*60.d0+min+second/60.d0)*dtupi/1440.d0
cc	  timesec = (julian(0,1,year,ut) + yd - 2440587.5d0) * 86400.d0
	  timesec = (julian(day,month,year,ut) - 2440587.5d0) * 86400.d0
	  tp(1) = int(timesec)
	  tp(2) = int((timesec - aint(timesec)) * 1000000.)
	  is = systimeset(tp(1))
	  IF (is.ne.0) type *,'UNIX system time setting fails'
	END IF	
	
C  --	set the vmebus memory storage of time parameters
	IF(index(ACTION,'R').gt.0  .OR. index(ACTION,'U').gt.0) THEN
	  call WRITE_VME_TIME_PARAMETERS
	1	(MTICKS,TP,DAY1,DUT1,DAY2,DUT2,ERROR)
	  IF(ERROR(1:2).NE.'OK') type *,error
	  call LOGIT('Time reset (GPS)')
	  write(buffer,*)'Delta UT equals',DUT1,' secs on JULday',DAY1
	  type *,buffer
	  call LOGIT(buffer)
	  write(buffer,*)'Delta UT equals',DUT2,' secs on JULday',DAY2
	  type *,buffer
	  call LOGIT(buffer)
	call exit
	END IF

c --	report all of the times again
	call GET_TIMES(ticks,2,month,day,year,ut,lst,error)
	if(error(1:2).ne.'OK') type *,'get_times:',error
	anangle = angles(ut*12.d0/dpi)
	type *,'UT: ',anangle,' date:',month,'/',day,'/',year
	anangle = angles(lst*12.d0/dpi)
	type *,'LST: ',anangle,'    VMETicks:',ticks

99	type *,error
	END

c --	little subroutine to read the vme time correctly
	subroutine getticks(ticks)
	include '../inc/constants.inc'
	integer vme_inpl
	external vme_inpl!$pragma c(vme_inpl)
	integer vclock /0/
	integer ticks

	TICKS = vme_inpl(%VAL(vmeunit),%VAL(vclock))

	return
	end
