c
c= SETTIME  set the system clock
c: basic
c+
	PROGRAM SETTIME

C  SETTIME:	sets the multibus and computer clocks accurately.
c		The system runs in timezone UTC.
c-- 
c	may 91 wh
c	use wwv   jun 92 wh
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='SETTIME (version 2.0 26-jun-92)')

	include '../inc/constants.inc'
	character*80 buffer,error,errord,merror
	character*24 suntime,multitime,msettime,ctime
	character*4  action
	character*14 angles,anangle
	double precision julian, day1, day2, ut, lst, timesec
	integer month,day,year,hour,min,systime,time,ticks,tp(2),tunix(2)
	real delta
	integer systimeset
	real*4 dut1,dut2
	integer repeat,necessary/-1/,notnecessary/0/

	character*8 wbaud,wdaysave,wzone,wdelayc,wdelayh,wfreq
	character*8 wtrans,wwait,wyear
	character*60 wstatus
	character*20 wfreqs
	integer*4 hr,min
	real*4 sec

	call OUTPUT(version)
	call mbusopen(error)
	if(error(1:2).ne.'OK') goto 99

C  --	read the mbus clock and the system clock together
	call getticks(ticks)
	systime = time()
	suntime = ctime(systime)

c  --	calculate time from multibus clock using stored quantities-
	CALL READ_MULTIBUS_TIME_PARAMETERS
	1		(MTICKS,TP,DAY1,DUT1,DAY2,DUT2,MERROR)
	  IF(MERROR(1:2).EQ.'OK') THEN
	    TICKS = TICKS - MTICKS
	    MSETTIME = CTIME(TP(1))
	    TUNIX(1) = TP(1)+TICKS/100
	    TUNIX(2) = TP(2)+MOD(TICKS,100)*10000
	    TUNIX(1) = TUNIX(1)+TUNIX(2)/1000000
	    TUNIX(2) = MOD(TUNIX(2),1000000)
	    MULTITIME = CTIME(TUNIX(1))
	  END IF

	call WWV_OPEN(error)
	  if (error.ne.'OK') goto 99

	call WWV_STATUS(wfreqs,wbaud,wdaysave,wzone,wyear,wdelayc,wdelayh,
	1		   wstatus,wfreq,wtrans,wwait,error)
	  if (error.ne.'OK') type *, error
	
	type *
	TYPE *, 'WWV CLOCK STATUS:'
	TYPE *,
	1	'Freqs: ',wfreqs,'  Baud rate: ',wbaud,' 12/24: ',wdaysave
	TYPE *,
	1	'Zone: ',wzone,'Year: ',wyear,'Colorado delay: ',wdelayc,
	1		'Hawaii delay: ',wdelayh
	TYPE *, 'Present trans: ',wtrans,'   Present freq: ',wfreq
	TYPE *, 'Status: ',wstatus(1:25),'   Time since good trans: ',wwait
	TYPE *

	call WWV_TIME(hr,min,sec,error)
	   if(error(1:2).ne.'OK') type *,error
	call WATCHTIME(delta,wfreq,wtrans,wwait,error)
	   if (error(1:2).ne.'OK') type *, error

	TYPE '(a,I3.2,'':'',I2.2,'':'',F5.2)', ' Present WWV time:',hr,min,sec

c --	report all of the times
	type *,'UNIX system time:  ',suntime
	if (merror(1:2).eq.'OK') then
	  type *,'Current mbus time: ',multitime
	  TYPE *, ' The multibus-WWV time difference is ',delta,' secs'
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
	  accept *,month,day,year,dut1
	  day1 = julian(day,month,year,0.d0)
	  type *,'Ending Month,Day,Year,delta_UT [ex:2 20 90 .05]:'
	  accept *,month,day,year,dut2
	  day2 = julian(day,month,year,0.d0)
	END IF

c --	ACTION: reset the times using WWV time
	IF(index(ACTION,'R').gt.0) THEN

	  
C	  --read the time to issue warning if necessary--
	  call GETTICKS(mticks)
	  daysleft=(4294967295. - float(mticks)) / (86400.*100.)
	  type *,' There are ',daysleft,' days left before the clock OVERFLOWS'

c	  -- capture tick and wwv simultaneously--
	 repeat=necessary
	 DO WHILE (repeat .eq. necessary)
	  call GETTICKS(mtickso)
	  call WWV_TIME(HOUR,MIN,SEC,ERROR)
	  call GETTICKS(mticks)
	  call WWV_DATE(MONTH,DAY,YEAR,ERRORD)
	  repeat= notnecessary
	  IF(ERROR(1:2).NE.'OK') THEN
		type *,error
		repeat=necessary
	  END IF
	  IF(ERRORD(1:2).NE.'OK') THEN
		type *,errord
		repeat=necessary
	  END IF
	  IF(MTICKS.GT.MTICKSO+3) THEN
		type *,'Interuption while reading time, delta=',
	1	mticks-mtickso,' ticks (retried)'
		repeat=necessary
	  END IF
	END DO


c  --	set the UNIX system time 
c --	calculate secs since jan 1,70 for unix (no corr for dUT here)
	  sec = sec + .03		!+.02 for wwv rec latency
	  ut = (hour*60.d0+min+sec/60.d0)*dtupi/1440.d0
	  timesec = (julian(day,month,year,ut) - 2440587.5d0) * 86400.d0
	  tp(1) = int(timesec)
	  tp(2) = int((timesec - aint(timesec)) * 1000000.)
	  is = systimeset(tp(1))
	  IF (is.ne.0) type *,'UNIX system time setting fails'
	END IF	
	
C  --	set the multibus memory storage of time parameters
	IF(index(ACTION,'R').gt.0  .OR. index(ACTION,'U').gt.0) THEN
	  call WRITE_MULTIBUS_TIME_PARAMETERS
	1	(MTICKS,TP,DAY1,DUT1,DAY2,DUT2,ERROR)
	  IF(ERROR(1:2).NE.'OK') type *,error
	  call LOGIT('Time reset')
	  write(buffer,*)'Delta UT equals',DUT1,' secs on JULday',DAY1
	  type *,buffer
	  call LOGIT(buffer)
	  write(buffer,*)'Delta UT equals',DUT2,' secs on JULday',DAY2
	  type *,buffer
	  call LOGIT(buffer)
	END IF

c --	report all of the times again
	call GET_TIMES(ticks,2,month,day,year,ut,lst,error)
	if(error(1:2).ne.'OK') type *,'get_times:',error
	anangle = angles(ut*12.d0/dpi)
	type *,'UT: ',anangle,' date:',month,'/',day,'/',year
	anangle = angles(lst*12.d0/dpi)
	type *,'LST: ',anangle,'    MbusTicks:',ticks

99	type *,error
	END

c --	little subroutine to read the mbus time correctly
	subroutine getticks(ticks)
	integer kclock /'A00'x/
	integer ticks,hiclock,loclock,lastticks,peek

	    TICKS = -1
	    DO WHILE (TICKS.NE.LASTTICKS)
		LASTTICKS = TICKS
		HICLOCK = PEEK(KCLOCK)
		HICLOCK = (.not.HICLOCK) .AND. 65535
		LOCLOCK = PEEK(KCLOCK+2)
		loclock = (.not.loclock) .AND. 65535
		TICKS = LSHIFT(HICLOCK,16) .OR. LOCLOCK
	    END DO

	return
	end
