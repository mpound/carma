c= STATUS - Display everything about the interferometer.
	PROGRAM STATUS
c& wh
c: observing
c+
c	STATUS displays everything that we could think of about the
c	Hat Creek Interferometer. Because the result is more than a
c	screen, the FLAGS parameter can be used to select information
c	subsets.  The program will also repeat at intervals if minutes
c	is given.  Typing STATUS alone will dump everything onto the
c	screen.
c
c@ of
c	Select which subsets of information to display.  If missing or blank,
c	everything is displayed.
c	 A = Antenna Pointing information
c	 I = Interferometer information
c	 M = Messages from programs
c	 S = System temperatures
c	 P = Pointing corrections
c
c@ out
c	Output file name. Missing or blank means the terminal.
c
c@ minutes
c	Repetition rate in minutes > 0. If missing, don't repeat.
c--
c	feb 90 wh
c	unix-> nov 90
c	more apc/epc  wh jun 95
c	10 ants  wh apr 97
c----------------------------------------------------------------c
	character version*(*)
	parameter(version='STATUS (version 1.4 30apr97)')
	include '../inc/constants.inc'
	character flags*10, file*50
	integer reprate

	real*8 ut,lst,yd1,yd2
	integer mon,day,year,id
	character*80 error
	integer is,ticks
	character*14 utstring,lststring,angles

c -- get the arguments
	call output(version)

	call mbusopen(error)
	 if(error(1:2).ne.'OK') goto 99

	CALL KEYINI
	CALL KEYA('of',FLAGS,'SIPMA')
	CALL UCASE(FLAGS)
	CALL KEYA('out',FILE,'term')
	CALL KEYI('minutes',REPRATE,0)

c -- open the file
	IF (FILE(1:4).NE.'term') THEN
	  OPEN(6,FILE=FILE,STATUS='UNKNOWN',FILEOPT='EOF',IOSTAT=IS)
	  IF (IS.NE.0) THEN
		TYPE *,'STATUS: can''t open '//FILE
		STOP
	  ENDIF
	END IF

c  -- the header
1	CALL GET_TIMES(TICKS,2,MON,DAY,YEAR,UT,LST,ERROR)
	UTSTRING = ANGLES(UT*12.D0/DPI)
	LSTSTRING = ANGLES(LST*12.D0/DPI)
	WRITE(6,*)
	WRITE(6,'(A,X,A,X,A,I2,A,I2.2,A,I2.2)',IOSTAT=IS)
	1	'STATUS of Hat Creek Interferometer at UT:',
	2	utstring,'  Date: ',mon,'/',day,'/',year
	IF (ERROR(1:2).NE.'OK') WRITE(6,'(A,A)') '  error: ',ERROR
	IF (IS.NE.0) WRITE(6,'(A,I6)') '  error: status = ',IS

c  -- line about times
	WRITE(6,*)
	CALL READ_MULTIBUS_TIME_PARAMETERS(TICKS,ID,YD1,DUT1,YD2,DUT2,ERROR)
	IF(ERROR(1:2).NE.'OK') WRITE(6,'(A,A)') '  error:'//ERROR
	WRITE(6,'(A,A,A,2F10.1,A,2F6.3)') 'LST:',LSTSTRING,
	1	'   Yeardays for dUT:',yd1,yd2,' dUT:',dut1,dut2

c  -- line about the weather
	CALL STAT_WEATHER
c  -- line about source
	CALL STAT_POINT
C  -- line about frequencies
	CALL STAT_FREQ 
c  -- line about correlator
	CALL STAT_CORR
c  -- lines about the az/el pointing
	IF(INDEX(FLAGS,'A').GT.0) CALL STAT_POINTING
c  -- lines about pointing corrections
	IF(INDEX(FLAGS,'P').GT.0) CALL STAT_CORRECTIONS
c  -- lines about the interferometer registers
	IF(INDEX(FLAGS,'I').GT.0) CALL STAT_INTERFEROMETER
c  -- lines about the system temperatures
	IF(INDEX(FLAGS,'S').GT.0) CALL STAT_SYSTEMPS
c  -- lines about the incoming messages
	IF(INDEX(FLAGS,'M').GT.0) CALL STAT_MESSAGES

	IF(REPRATE.EQ.0) STOP
	CALL SLEEP(REPRATE*60)
	GOTO 1

99	type *,error
	END
c
c STAT_WEATHER
c: display
	SUBROUTINE STAT_WEATHER

C  STAT_WEATHER:	get the weather information and output for STATUS
c
c  ARGUMENTS:
c--
c	FEP 90 WH
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	character*80 error
	real airtemp,winddir,pressmb,precipmm,dewpoint,relhumid

C	 --GET THE WEATHER VALUES--
	    CALL COMGETR('WINDMPH',windmph, 1,error) 
	    CALL COMGETR('WINDDIR',winddir, 1,error) 
	    CALL COMGETR('AIRTEMP',airtemp, 1,error) 
	    CALL COMGETR('PRESSMB',pressmb, 1,error) 
	    CALL COMGETR('PRECIPMM',precipmm, 1,error) 
	    CALL COMGETR('DEWPOINT',dewpoint, 1,error) 
	    CALL COMGETR('RELHUMID',relhumid, 1,error) 

	WRITE(6,'(A,F5.1,A,I3,A,I4.2,A,I3,A)') 'AIR TEMP:',AIRTEMP,
	1	' C  Precip H2O:',int(PRECIPMM),' mm ',int(RELHUMID),
	2	'%  DEWPOINT:',int(DEWPOINT),' C'
	WRITE(6,'(A,I4,A,I4.3,A,I4,A)')
	1  ' Wind:',int(WINDMPH),' mph from ',int(WINDDIR),' Deg  Pressure:',
	2	int(pressmb),' mb'
	RETURN
	END
c
c STAT_SYSTEMPS
c: display
c+
	SUBROUTINE STAT_SYSTEMPS
c
c  STAT_SYSTEMPS:	Prints all the system temperatures for status.
c
c
c  ARGUMENTS:
c--
c	feb 90 wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	real*4 systempav(nants),systemp(16,nants)
	character*80 error

	CALL COMGETR('SYSTEMPAV',SYSTEMPAV,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETR('SYSTEMP',SYSTEMP,nants*16,ERROR)
	   CALL ERROR_CHECK(ERROR)

	WRITE(6,'(a)') 'SYSTEMPS'
	WRITE(6,'(A)') '  ant   Ave      1      2  '//
	1	'     3       4       5       6       7       8'
	DO I=1,10
	  WRITE(6,'(2x,I2,9F8.1)') I,SYSTEMPAV(I),(SYSTEMP(I,J),J=1,8)
	END DO
	RETURN
	END
c
c STAT_CORRECTIONS
c: display
c+
	SUBROUTINE STAT_CORRECTIONS
c
c  STAT_CORRECTIONS:	Prints the pointing corrections
c
c  ARGUMENTS:
c--
c	feb 90 wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	real*4 apc(9,nants),epc(8,nants)
	character*80 error

	  CALL COMGETR('APC',APC,9*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETR('EPC',EPC,8*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)

	WRITE(6,'(a)') 'POINTING CORRECTIONS'
	WRITE(6,'(A)') '  ant AXIS     1       2       3       4       5'//
	1			'       6       7      8      9'
	DO I=1,10
	  WRITE(6,'(2x,I2,X,A,X,F9.2,3F8.2,5f7.2)') I,'Azim',(APC(J,I),J=1,9)
	  WRITE(6,'(2x,I2,X,A,X,F9.2,3F8.2,4f7.2)') I,'Elev',(EPC(J,I),J=1,8)
	END DO
	RETURN
	END
c
c STAT_INTERFEROMETER
c: display
c+
	SUBROUTINE STAT_INTERFEROMETER
c
c  STAT_INTERFEROMETER:	Print interferometer quantities for status.
c
c  ARGUMENTS:
c--
c	feb 90 wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	real*8 antpos(nants,3)
	character*80 error
	real*8 delay(nants),fof1(nants),fof2(nants)

	WRITE(6,'(a)') 'INTERFEROMETER'
	WRITE(6,'(A)') '  ant       X             Y           Z         '//
	1	'    DELAY  FOF1  FOF2 '
	CALL COMGETD('ANTPOS',ANTPOS,3*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('DELAY',DELAY,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('FOF1',FOF1,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('FOF2',FOF2,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)

	DO I=1,10
	  WRITE(6,'(2x,I2,3F14.6,3F9.3)')
	1	I,(ANTPOS(I,J),J=1,3),DELAY(I),FOF1(I),FOF2(I)
	END DO

	RETURN
	END
c
c STAT_POINT
c: display
	SUBROUTINE STAT_POINT

C  STAT_POINT:	get the pointing information from common and output 
c			it for STATUS
c
c  ARGUMENTS:
c--
c	FEB 90  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	include '../inc/ephem.inc'
	record /asource/s	
	character error*80
	integer itrack(nants),ants
	real*8 a1,a2,azim(nants),elev(nants)
	character*8 source
	character*14 ang1,ang2,angles

	CALL COMGETI('ITRACK',ITRACK,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	IMRA=0
	IMRA2=0
	IMAZ=0
	  CALL COMGETD('AZIM',AZIM,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETD('ELEV',ELEV,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	DO I=1,10
	  IF ((ITRACK(I).AND.2).NE.0) IMRA=IMRA*10 + I
	  IF ((ITRACK(I).AND.1).NE.0) IMAZ=IMAZ*10 + I
	  IF ((ITRACK(I).AND.4096).NE.0) IMRA2=IMRA2*10 + I
	END DO
	IF (IMRA.GT.0) THEN
	  ANTS = IMRA
	  LAB1 = ' RA:'
	  LAB2 = 'DEC:'
	  CALL COMGETD('RA',A1,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETD('DEC',A2,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  ANG1 = ANGLES(A1*12.D0/DPI)
	  ANG2 = ANGLES(A2*180.D0/DPI)
	  CALL COMGETI('SORCDATA',S,24,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  SOURCE = s.name
	ELSE IF (IMAZ.GT.0) THEN
	  ANTS = IMAZ
	  LAB1 = ' AZ:'
	  LAB2 = ' EL:'
	  ANG1 = ANGLES(AZIM(I))
	  ANG2 = ANGLES(ELEV(I))
	  SOURCE = '[AZ/EL]'

	ELSE
	  ANTS = 0
	  LAB1 = ' '
	  LAB2 = ' '
	  ANG1 = ' '
	  ANG2 = ' '
	  SOURCE = ' '
	END IF
	WRITE(6,'(A,A,2X,A,A,2X,A,A,A,I9,A)')
	1	'SOURCE: ',source,lab1,ang1,lab2,ang2,'   [',ants,']'
	IF(IMRA2.GT.0) THEN
	  CALL COMGETI('STARDATA',S,24,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  ANG1 = ANGLES(S.RA0*12.D0/DPI)
	  ANG2 = ANGLES(S.DEC0*180.D0/DPI)
	  WRITE(6,'(A,A,2X,A,A,2X,A,A,A,I9,A)')
	1	'SOURCE2: ',s.name,'RA:',ang1,
	2	  ' DEC:',ang2,'   [',imra2,']'
	END IF
	RETURN
	END
c
c STAT_FREQ
c: display
	SUBROUTINE STAT_FREQ

C  STAT_FREQ:	get the frequency information from common and output 
c			it for STATUS
c
c  ARGUMENTS:
c--
c	jan 90  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	character error*80
	real*8 freq,fdop,xfreq,refmhz
	integer mharm,nharm

	CALL COMGETD('FREQ',FREQ,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('FDOP',FDOP,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('XFREQ',XFREQ,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('REFMHZ',REFMHZ,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('MHARM',MHARM,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('NHARM',NHARM,1,ERROR)
	   CALL ERROR_CHECK(ERROR)

	WRITE(6,'(A,F11.6,A,F11.6,A,F10.6,A,F12.7,A,2I3)')
	1	'FREQ:',FREQ,'  Fdop:',FDOP,'  XFreq:',xfreq,
	2		'  Refmhz:',REFMHZ
	WRITE(6,'(A,2I3)')
	1	'HARMONICS:',NHARM,MHARM
	RETURN
	END

c
c STAT_CORR
c: display
	SUBROUTINE STAT_CORR

C  STAT_CORR:	get the correlator information from common and output 
c			it for status
c
c  ARGUMENTS:
c--
c	jan 90  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	character error*80
	real*4 corf(4),corbw(4)
	integer cormode,coropt
	character*7 type

	CALL COMGETR('CORF',CORF,4,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETR('CORBW',CORBW,4,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('CORMODE',CORMODE,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('COROPT',COROPT,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	IF (COROPT.EQ.0) THEN
	  TYPE = '<CROSS>'
	ELSE IF (COROPT.EQ.1) THEN
	  TYPE = '<AUTO>'
	ELSE
	  TYPE = ' '
	END IF

	WRITE(6,'(A,4f8.2,a,i1,a,a)')
	1	'CORFS:',corf,' Mode:',cormode,'  ',type
	WRITE(6,'(A,4f8.2)') 'CORBW:',corbw
	RETURN
	END

c
c STAT_MESSAGES
c: display
	SUBROUTINE STAT_MESSAGES

C  STAT_MESSAGES:	get the program and message information from common
C			and output it for status
c
c  ARGUMENTS:
c--
c	jan 90  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	character*80 message(7)
	integer messeq
	character*64 program
	character*80 error

	WRITE(6,*)
	CALL COMGETI('MESSEQ',MESSEQ,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETA('PROGRAM',PROGRAM,64,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETA('MESSAGE',MESSAGE,560,ERROR)
	   CALL ERROR_CHECK(ERROR)

	WRITE(6,'(A,A)') 'PROGRAM: ',program
	DO LINE = 1,7
	   write(6,'(a)') MESSAGE(LINE)
	END DO
	RETURN
	END

c
c STAT_POINTING
c: display
	SUBROUTINE STAT_POINTING

C  STAT_POINTING:	get the antenna information from common and output 
c			it for STATUS
c
c  ARGUMENTS:
c--
c	jan 90  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	integer atlimit,slewflag
	real*4 axiserr(2,nants),axisrms(2,nants)
	real*8 axisin(2,nants)
	integer axisnum,itrack(nants)
	real*4 apc(9,nants),epc(8,nants)
	character*80 error
	character*5 staname(nants)
	logical first /.true./
	integer  ant, jax
	character*5 message(2)

	real*4 offset(2) /0.,184320./
	integer mask(9,2)
	data mask/'1'x,'2'x,'4'x,'8'x,'10'x,'20'x,'40'x,'80'x,
	1	'1000'x,'2000'x,'4000'x,'8000'x,'10000'x,'20000'x,
	2	 '40000'x,'80000'x,'100000'x,'200000'x/

	IF (FIRST) THEN
	  FIRST = .FALSE.
	  CALL COMGETR('APC',APC,9*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETR('EPC',EPC,8*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	END IF
	CALL COMGETI('ATLIMIT',ATLIMIT,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('SLEWFLAG',SLEWFLAG,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('AXISNUM',AXISNUM,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('AXISIN',AXISIN,2*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETR('AXISERR',AXISERR,2*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETR('AXISRMS',AXISRMS,2*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETA('STANAME',STANAME,60,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('ITRACK',ITRACK,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)

	WRITE(6,'(a)') 'ANTENNAS'
	WRITE(6,'(A)') 
	1  '  ant STAT   Azim   Err  RMS  Limslew Elev    Err  RMS Limslew'

	DO ANT=1,10
	 DO JAX=1,2
	  IF(ITRACK(ANT).eq.0) THEN
C		CALL TELEMIN2(TENCODERV(ANT,JAX),NEWAX,ERROR)
C		CALL ENCODERS(NEWAX)
C		AXISIN(ANT,JAX) = (FLOAT(NEWAX)+OFFSET(JAX))/117341.7564
C		AXISERR(ANT,JAX) = 0.
C		AXISRMS(ANT,JAX) = 0.
	  END IF

	  MESSAGE(JAX) = '    '
	  IF((ITRACK(ANT).AND.7).EQ.1) THEN
		MESSAGE(JAX) = 'AzEl'
	  ELSE IF((ITRACK(ANT).AND.7).EQ.2) THEN
		MESSAGE(JAX) = 'RaDc'
	  END IF
	  IF((SLEWFLAG.AND.MASK(ANT,JAX)).NE.0) THEN
	    MESSAGE(JAX) = 'Slew'
	  END IF
	  IF((ATLIMIT.AND.MASK(ANT,JAX)).NE.0) MESSAGE(JAX) = 'LIMT'

	 END DO
	    WRITE(6,'(2x,I2,2x,A,X,F7.3,x,2I4,2X,a,2x,F7.3,X,2i4,x,a)',
	1							IOSTAT=IS)
	1	ANT,STANAME(ANT),AXISIN(1,ANT)-APC(1,ANT)/60.,
	2	INT(AXISERR(1,ANT)),INT(AXISRMS(1,ANT)),MESSAGE(1),
	3	AXISIN(2,ANT)-EPC(1,ANT)/60.,INT(AXISERR(2,ANT)),
	4	INT(AXISRMS(2,ANT)),MESSAGE(2)
	END DO

	RETURN
	END

	SUBROUTINE ERROR_CHECK(ERROR)
	character*(*) error

	IF (ERROR(1:2).NE.'OK') WRITE(6,'(A,A)') '  error: ',ERROR(1:70)
	RETURN
	END	
