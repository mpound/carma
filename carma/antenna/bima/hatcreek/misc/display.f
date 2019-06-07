c= DISPLAY   display system state
c: basic
c+
	PROGRAM DISPLAY
C  	DISPLAY: displays the state of the interferometer system on a
c	dedicated terminal screen using CURSES in a socially acceptable
c	manner.
c@  once
c	T or t means display fills screen only one time and exits
c--
c	redone dec 89  wh
c	9 ant version oct 90 wh
c	unix-> dec 90 wh (uses curses from 5lib)
c	aug 93 wh - get rid of watch on program line
c	nov 93 wh -stomp out watchmes
c	nov 93 wh - add PROJECT
c	nov 93 wh - sense when control is dead
c	feb 94 wh - add once through keyword once=T
c	may 94 wh - make weather facts work
c	aug 94 wh - add in ant 2,8,9
c	aug 96 wh - add ant A
c   sep 96 rp - change code to use mmlockstat, etc to allow for cm receivers
c	may 97 wh -  more ant A
c	jun 97 wh - new ant 1
c----------------------------------------------------------c
	include '../inc/constants.inc'
	external initscr	!$pragma C ( initscr )
	external refresh	!$pragma C ( refresh )
	external beep		!$pragma C ( beep )

c  --  the 7 windows are: time,weather,point,freq,corr,ants,program
	integer iheight(8) /  1,  1,    6,    1,   1,  11,    9,  1/
	integer iwidth(8)  / 38, 39,   18,   79,  79,  61,   80, 80/
	integer irow(8)    /  0,  0,    7,    1,   2,   4,   15, 15/
	integer icol(8)    /  0, 40,    0,    0,   0,  17,    0,  0/

	character error*80
	integer ticks
	real*8 utin,lstin
	character*10 once
	logical loop /.true./

	call keyini
	call keya('once',once,'F')
	call keyfin

c  --  start by initing screen --
	call initscr
	call mbusopen(error)
	if (error(1:2).ne.'OK') call write_display(error)
c	call lockexit  

c  --  loop--set up buffer/ fill in various lines/ output/ wait--
	ITER = 0
	DO WHILE (loop)
	 if((once(1:1).eq.'T').or.(once(1:1).eq.'t')) loop=.false.
	 ITER = ITER+1

c	--only refresh display if 1 sec has elapsed--
	 CALL GET_TIMES(TICKS,2,ID,IM,IY,UTIN,LSTIN,ERROR)
	   CALL ERROR_CHECK(ERROR)
	 NEXTTIME = 100 - mod(ticks,100)
	 CALL LINE_TIME(IROW(1),ICOL(1),IWIDTH(1),UTIN,LSTIN)
	 if (mod(iter,3).eq.1) CALL LINE_WEATHER(IROW(2),ICOL(2),IWIDTH(2))
	 if (mod(iter,2).eq.1) CALL LINE_POINT(IROW(3),ICOL(3),IWIDTH(3))
	 if ((iter.eq.1).or.(mod(iter,3).eq.2))
	1	 CALL LINE_FREQ(IROW(4),ICOL(4),IWIDTH(4))
	 if (mod(iter,3).eq.1) CALL LINE_CORR(IROW(5),ICOL(5),IWIDTH(5))
c	 if ((mod(iter,8).eq.2).or.(mod(iter,8).eq.3))
c	1		 CALL LINE_WATCH(IROW(8),ICOL(8),IWIDTH(8))
	 CALL LINE_ANT(IROW(6),ICOL(6),IWIDTH(6))
	 if (mod(iter,2).eq.1) CALL LINE_PROG(IROW(7),ICOL(7),IWIDTH(7))

	    ITER=MOD(ITER,24)

	  CALL COMGETI('BEEPS',IB,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  IF((ERROR(1:2).EQ.'OK').AND.(IB.GT.0)) THEN
	   CALL BEEP
	   IB=MAX(MIN(IB-1,5),0)
	   CALL COMPUTI('BEEPS',IB,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  END IF

C  --  output whole buffer--
	  CALL REFRESH

C  --  wait short interval and repeat
	  CALL WAIT_TICKS(NEXTTIME)
	END DO
	END
c
c LINE_TIME
c: display
	SUBROUTINE LINE_TIME(IROW,ICOL,IWIDTH,UTIN,LSTIN)

C  LINE_TIME:	convert the times and output for display
c
c  ARGUMENTS:
c  IROW (integer,input)	Row for subwindow
c  ICOL (integer,input)	Column for subwindow
c  IWIDTH (integer,input)	Width for subwindow
c  UTIN (dp,input)	Universal Time
c  LSTIN (dp,input)	Local siderial time
c--
c	dec 89  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	external move	!$pragma C ( move )
	external addstr	!$pragma C ( addstr )
	external attrset	!$pragma C ( attrset )
	external deleteln !$pragma C (deleteln )
	real*8 utin,lstin
	character buffer*38, ut*14,lst*14,date*8,angles*14
	character blanks*42 /' '/
	equivalence (buffer,blanks)

	UT = ANGLES(UTIN*12.D0/DPI)
	LST = ANGLES(LSTIN*12.D0/DPI)
	CALL DATEX(DATE)
	WRITE(BUFFER,'(A,A,A,A,2X,A)',ERR=99) 
	1		'UT: ',UT(1:8),' LST: ',LST(1:8),DATE
c99	CALL ATTRSET(%val('10000000'o))
99	CALL MOVE(%val(IROW),%val(ICOL))
	CALL ADDSTR(BUFFER)
	RETURN
	END
c
c LINE_WEATHER
c: display
	SUBROUTINE LINE_WEATHER(IROW,ICOL,IWIDTH)

C  LINE_WEATHER:	get the weather information and output for display
c
c  ARGUMENTS:
c  IROW (integer,input)	Row for subwindow
c  ICOL (integer,input)	Column for subwindow
c  IWIDTH (integer,input)	Width for subwindow
c--
c	dec 89  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	external move	!$pragma C ( move )
	external addstr	!$pragma C ( addstr )
	external deleteln !$pragma C (deleteln )
	real airtemp
	character buffer*40
	character*80 error
	character*40 blanks /' '/
	equivalence (buffer,blanks)

C	 --GET THE WEATHER VALUES--
	    CALL COMGETR('WINDMPH',windmph, 1,error) 
	    CALL COMGETR('WINDDIR',winddir, 1,error) 
	    CALL COMGETR('AIRTEMP',airtemp, 1,error) 
c	    CALL COMGETR('PRESSMB',pressmb, 1,error) 
	    CALL COMGETR('PRECIPMM',precipmm, 1,error) 
	    CALL COMGETR('DEWPOINT',dewpoint, 1,error) 
	    CALL COMGETR('RELHUMID',relhumid, 1,error) 

	buffer = ' ' 

C	 --FORMAT LINE AND OUTPUT TO BUFFER--
	 WRITE(BUFFER,100,ERR=99) int(AIRTEMP),int(PRECIPMM),int(RELHUMID),
	1		int(WINDMPH),int(WINDDIR)
100	 FORMAT('T:',i3,'C  H2O:',i3,
	1	'mm ',i3.2,'%  Wind:',i2,'mph ',i3)
99	CALL MOVE(%val(IROW),%val(ICOL))
	buffer(40:40) = char(0)
	CALL ADDSTR(BUFFER)

	RETURN
	END
	
c
c LINE_POINT
c: display
	SUBROUTINE LINE_POINT(IROW,ICOL,IWIDTH)

C  LINE_POINT:	get the pointing information from common and output 
c			it for display
c
c  ARGUMENTS:
c  IROW (integer,input)	Row for subwindow
c  ICOL (integer,input)	Column for subwindow
c  IWIDTH (integer,input)	Width for subwindow
c--
c	dec 89  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	include '../inc/ephem.inc'
	record /asource/s
	external move	!$pragma C ( move )
	external addstr	!$pragma C ( addstr )
	external deleteln !$pragma C (deleteln )
	character buffer*30,error*80
	integer itrack(nants)
	real*8 azims(nants),elevs(nants),ra,dec,dra,ddec
	character*14 ang1,ang2,angles
	character*8 source
	character*12 project
	character*12 STRINGRA,STRINGAZ,STRINGANTS
	character*1 ANTSYM(12) 
	1	/'1','2','3','4','5','6','7','8','9','A','B','C'/
	CALL COMGETI('ITRACK',ITRACK,NANTS,ERROR)
	   CALL ERROR_CHECK(ERROR)

	IMAZ1 = 0
	STRINGAZ = ''
	STRINGRA = ''
	DO I=1,nants
	 if((itrack(i).and.4096).eq.0) then
	  IF ((ITRACK(I).AND.2).NE.0) THEN
		STRINGRA = STRINGRA(1:len1(stringra))//ANTSYM(I)
	  ELSE IF ((ITRACK(I).AND.1).NE.0) then
		STRINGAZ = STRINGAZ(1:len1(stringaz))//ANTSYM(I)
		if(imaz1.eq.0) imaz1 = i
	  end if
	 end if
	END DO
	IF (STRINGRA.ne.'') THEN
	  STRINGANTS = STRINGRA
	  LAB1 = ' RA: '
	  LAB2 = 'DEC: '
	  CALL COMGETI('SORCDATA',S,24,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETD('RA',RA,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETD('DEC',DEC,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  ANG1 = ANGLES(RA*12.D0/DPI)
	  ANG2 = ANGLES(DEC*180.D0/DPI)
	  CALL COMGETD('DRA',DRA,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETD('DDEC',DDEC,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  IF(ABS(S.RA2000-RA).LT..1) THEN
	    SOURCE = S.NAME
	  ELSE
	    SOURCE = '[RA/DEC]'
	  END IF
	ELSE IF (STRINGAZ.ne.'') THEN
	  STRINGANTS = STRINGAZ
	  LAB1 = ' AZ: '
	  LAB2 = ' EL: '
	  CALL COMGETD('AZIM',AZIMS,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETD('ELEV',ELEVS,nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  ANG1 = ANGLES(AZIMS(imaz1))
	  ANG2 = ANGLES(ELEVS(imaz1))
	  SOURCE = '[AZ/EL]'

	ELSE
	  STRINGANTS = ' '
	  LAB1 = ' '
	  LAB2 = ' '
	  ANG1 = ' '
	  ANG2 = ' '
	  SOURCE = ' '
	END IF

	CALL COMGETA('PROJECT',PROJECT,12,ERROR)
	   CALL ERROR_CHECK(ERROR)

	WRITE(BUFFER,'(A,A10)',ERR=99) 'ANT: ',STRINGANTS
99	CALL MOVE(%val(IROW),%val(ICOL))
	CALL ADDSTR(BUFFER(:iwidth))

	WRITE(BUFFER,'(A,A)',ERR=98) 'SRC: ',SOURCE
98	CALL MOVE(%val(IROW+1),%val(ICOL))
	CALL ADDSTR(BUFFER(:iwidth))

	WRITE(BUFFER,'(A,A)',ERR=97) LAB1,ANG1
97	CALL MOVE(%val(IROW+2),%val(ICOL))
	CALL ADDSTR(BUFFER(:iwidth))

	WRITE(BUFFER,'(A,A)',ERR=96) LAB2,ANG2
96	CALL MOVE(%val(IROW+3),%val(ICOL))
	CALL ADDSTR(BUFFER(:iwidth))
	IF(STRINGRA.ne.'') THEN
	  WRITE(BUFFER,'(A,F9.3)',ERR=95) 'dRA: ',DRA
95	  CALL MOVE(%val(IROW+4),%val(ICOL))
	  CALL ADDSTR(BUFFER(:iwidth))
	  WRITE(BUFFER,'(A,F9.3)',ERR=94) 'dDC: ',DDEC
94	  CALL MOVE(%val(IROW+5),%val(ICOL))
	  CALL ADDSTR(BUFFER(:iwidth))
	ELSE
	  BUFFER = ' '
	  CALL MOVE(%val(IROW+4),%val(ICOL))
	  CALL ADDSTR(BUFFER(:iwidth))
	  CALL MOVE(%val(IROW+5),%val(ICOL))
	  CALL ADDSTR(BUFFER(:iwidth))
	END IF

	WRITE(BUFFER,'(A,A)',ERR=93) ' PJ:',PROJECT
93	CALL MOVE(%val(IROW+6),%val(ICOL))
	CALL ADDSTR(BUFFER(:IWIDTH))

	RETURN
	END
	
c
c LINE_FREQ
c: display
	SUBROUTINE LINE_FREQ(IROW,ICOL,IWIDTH)

C  LINE_FREQ:	get the frequency information from common and output 
c			it for display
c
c  ARGUMENTS:
c  IROW (integer,input)	Row for subwindow
c  ICOL (integer,input)	Column for subwindow
c  IWIDTH (integer,input)	Width for subwindow
c--
c	jan 90  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	external move	!$pragma C ( move )
	external addstr	!$pragma C ( addstr )
	external deleteln !$pragma C (deleteln )
	character buffer*80,error*80
	character*84 blanks /' '/
	equivalence (buffer,blanks)
	real*8 freq,iffreq,xfreq,refmhz

	buffer = ' '
	CALL COMGETD('FREQ',FREQ,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('IFFREQ',IFFREQ,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('XFREQ',XFREQ,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETD('REFMHZ',REFMHZ,1,ERROR)
	   CALL ERROR_CHECK(ERROR)

	WRITE(BUFFER,'(A,F13.8,A,F10.4,A,F13.8,A,F13.6)',ERR=99)
	1	'FREQ: ', FREQ,' IFFREQ:',IFFREQ,' XFREQ:',XFREQ,' REF:',REFMHZ
99	CALL MOVE(%val(IROW),%val(ICOL))
	buffer(80:80) = char(0)
	CALL ADDSTR(BUFFER)
	RETURN
	END
	
c
c LINE_CORR
c: display
	SUBROUTINE LINE_CORR(IROW,ICOL,IWIDTH)

C  LINE_CORR:	get the correlator information from common and output 
c			it for display
c
c  ARGUMENTS:
c  IROW (integer,input)	Row for subwindow
c  ICOL (integer,input)	Column for subwindow
c  IWIDTH (integer,input)	Width for subwindow
c--
c	jan 90  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	external move	!$pragma C ( move )
	external addstr	!$pragma C ( addstr )
	external deleteln !$pragma C (deleteln )
	character buffer*80,error*80
	character blanks*84 /' '/
	equivalence (buffer,blanks)
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

	WRITE(BUFFER,'(A,4F7.2,A,2F7.2,A,I3,3X,A)',ERR=99) 
	1	'CORFS:',CORF,' BAND:',CORBW(1),corbw(2),' MODE:',CORMODE,TYPE
99	CALL MOVE(%val(IROW),%val(ICOL))
	buffer(80:80) = char(0)
	CALL ADDSTR(BUFFER)
	RETURN
	END
c
c LINE_ANT
c: display
	SUBROUTINE LINE_ANT(IROW,ICOL,IWIDTH)

C  LINE_ANT:	get the antenna information from common and output 
c			it for display
c
c  ARGUMENTS:
c  IROW (integer,input)	Row for subwindow
c  ICOL (integer,input)	Column for subwindow
c  IWIDTH (integer,input)	Width for subwindow
c--
c	jan 90  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	external move	!$pragma C ( move )
	external addstr	!$pragma C ( addstr )
	external deleteln !$pragma C (deleteln )
	external clrtoeol !$pragma C (clrtoeol)
	character buffer*61
	integer atlimit,slewflag,antinuse
	real*4 axiserr(2,nants),axisrms(2,nants)
	integer axisnum,itrack(nants),resoff(2,nants)
	real*4 systempav(nants)
	double precision axisin(2,nants)
	real apc(9,nants),epc(8,nants)
	character*80 error
	character*60 staname
	double precision encoder
	logical first /.true./, select
	integer  ant, jax
	character*3 locks /'   '/
	character*5 message(2)
	integer bits,tpeek,antinuse,alive,lastalive/0/,dead/0/

	integer*4 mask(2,10)
	data mask/'1'x,'2'x,'4'x,'8'x,'10'x,'20'x,'40'x,'80'x,
	1	'100'x,'200'x,'400'x,'800'x,'1000'x,'2000'X,
	2	'4000'X,'8000'X,'10000'X,'20000'X,'40000'x,'80000'x/
	integer*4 cmask(2) /8,16/
	integer lxladd(10) 
	1	/'5f'x,'5f'x,'1f'x,'5f'x,'5f'x,'5f'x,'5f'x,'5f'x,'5f'x,'5f'x/
	integer lmadd(10) 
	1	/'6e'x,'6e'x,'5f'x,'6e'x,'6e'x,'6e'x,'6e'x,'6e'x,'6e'x,'6e'x/
	integer lm(10) /1,1,2,1,1,1,1,1,1,1/

	IF (FIRST) THEN
	  FIRST = .FALSE.
	  CALL COMGETR('APC',APC,9*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETR('EPC',EPC,8*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	  CALL COMGETI('RESOFF',RESOFF,2*nants,ERROR)
	   CALL ERROR_CHECK(ERROR)
	END IF
	CALL COMGETI('ATLIMIT',ATLIMIT,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('SLEWFLAG',SLEWFLAG,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('AXISNUM',AXISNUM,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('ANTINUSE',ANTINUSE,1,ERROR)
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
	CALL COMGETR('SYSTEMPAV',SYSTEMPAV,12,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('ANTINUSE',ANTINUSE,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETI('ALIVE',ALIVE,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	IF(ALIVE.NE.LASTALIVE) THEN
	  DEAD = 0
	ELSE
	  DEAD = DEAD+1
	END IF
	LASTALIVE = ALIVE

	BUFFER=' ANT     AZIM TRACK   ERR  RMS  ELEV TRACK  ERR  RMS LOCK TSYS'
	CALL MOVE(%val(IROW),%val(ICOL))
	call clrtoeol
	buffer(60:60) = char(0)
	CALL ADDSTR(BUFFER)

	DO ANT=1,10
	 DO JAX=1,2 
	  IF(ITRACK(ANT).EQ.0) THEN
	    AXISIN(JAX,ANT) = ENCODER(ANT,JAX-1,RESOFF(JAX,ANT),ERROR)
c	    if(error(1:2).ne.'OK'.and. select(ant,antinuse))
c	1				 call write_display(error)
	    AXISERR(JAX,ANT) = 0.
	    AXISRMS(JAX,ANT) = 0.
	  END IF

	  MESSAGE(JAX) = '    '
	  IF((ITRACK(ANT).AND.3).EQ.1) THEN
		MESSAGE(JAX) = 'AzEl'
	  ELSE IF((ITRACK(ANT).AND.3).EQ.2) THEN
		MESSAGE(JAX) = 'Trak'
	  ELSE IF((ITRACK(ANT).AND.24).EQ.CMASK(JAX)) THEN
		MESSAGE(JAX) = 'CVel'
	  END IF
	  IF((SLEWFLAG.AND.MASK(JAX,ANT)).NE.0) THEN
		MESSAGE(JAX) = 'Slew'
	  END IF
	 IF((ATLIMIT.AND.MASK(JAX,ANT)).NE.0) MESSAGE(JAX) = 'LIMT'
	 END DO
	 IF(DEAD.GT.10) THEN
		MESSAGE(1) = 'xxxx'
		MESSAGE(2) = 'xxxx'
	 END IF
	 IF(.NOT.SELECT(ANT,ANTINUSE)) THEN
		MESSAGE(1) = 'OFF '
		MESSAGE(2) = 'OFF '
	 END IF
	 bits = tpeek(ant,LXLADD(ANT),1,error)
	 if((bits.and.2).gt.0) locks(2:2) = 'L'
	 if((bits.and.2).eq.0) locks(2:2) = '-'
	 if((bits.and.4).gt.0) locks(1:1) = 'L'
	 if((bits.and.4).eq.0) locks(1:1) = '-'

c	 bits = tpeek(ant,LMADD(ANT),1,error)
c	 if((bits.and.LM(ant)).gt.0) locks(3:3) = 'L'
c	 if((bits.and.LM(ant)).eq.0) locks(3:3) = '-'

	  if (mmlockstat(ant).gt.0) locks(3:3) = 'L'
	  if (mmlockstat(ant).eq.0) locks(3:3) = '-'

	    i1 = (ant-1)*5+1
	    i2 = i1+5
	    if(systempav(ant).gt.9999.) systempav(ant)=9999.
	    WRITE(BUFFER,'(Z1,x,A5,F8.2,x,a,i4,f5.1,F7.2,X,A,I4,f5.1,
	1	X,a,i5)', ERR=99) 
	1	ANT,STANAME(i1:i2),AXISIN(1,ANT)-APC(1,ANT)/60.,MESSAGE(1),
	2	INT(AXISERR(1,ANT)),AXISRMS(1,ANT),
	3	AXISIN(2,ANT)-EPC(1,ANT)/60.,MESSAGE(2),INT(AXISERR(2,ANT)),
	4	AXISRMS(2,ANT),LOCKS,INT(SYSTEMPAV(ANT))

99	CALL MOVE(%val(IROW+ant),%val(ICOL))
c	if (ant.eq.3) buffer = '3 '
	CALL ADDSTR(BUFFER(:IWIDTH))
	END DO
	IF (DEAD.GT.10) THEN
	  CALL MOVE(%val(IROW-1),%val(ICOL))
	  CALL ADDSTR('*** CONTROL has died, Please restart it.***')
	ELSE
	  CALL MOVE(%val(IROW-1),%val(ICOL))
	  CALL ADDSTR('                                           ')
	END IF
	RETURN
	END
c
c LINE_PROG
c: display
	SUBROUTINE LINE_PROG(IROW,ICOL,IWIDTH)

C  LINE_PROG:	get the program and message information from common and output 
c			it for display
c
c  ARGUMENTS:
c  IROW (integer,input)	Row for subwindow
c  ICOL (integer,input)	Column for subwindow
c  IWIDTH (integer,input)	Width for subwindow
c--
c	jan 90  wh
c----------------------------------------------------------------c
	include '../inc/constants.inc'
	external move	!$pragma C ( move )
	external addstr	!$pragma C ( addstr )
	external deleteln !$pragma C (deleteln )
	character*80 message(7),mtemp
	integer messeq,oldmesseq
	character*64 program
	character*80 error,buffer

	CALL COMGETI('MESSEQ',MESSEQ,1,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETA('MESSAGE',MESSAGE,7*80,ERROR)
	   CALL ERROR_CHECK(ERROR)
	CALL COMGETA('PROGRAM',PROGRAM,64,ERROR)
	   CALL ERROR_CHECK(ERROR)

	BUFFER =' PROGRAM: '//PROGRAM
	CALL MOVE(%val(IROW),%val(ICOL))
	CALL ADDSTR(BUFFER(:IWIDTH))

	IF (MESSEQ.NE.OLDMESSEQ) THEN
	  OLDMESSEQ = MESSEQ
	  DO LINE = 1,7
	    CALL MOVE(%val(IROW+LINE),%val(ICOL))
	    CALL DELETELN()
	    mtemp = message(line)
	    mtemp(80:80) = char(0)
	    CALL ADDSTR(Mtemp)
	  END DO
	END IF
	RETURN
	END
c
c LINE_WATCH
c: display
c	SUBROUTINE LINE_WATCH(IROW,ICOL,IWIDTH)

C  LINE_WATCH:	get WATCHMES from common and output 
c			it to display
c
c  ARGUMENTS:
c  IROW (integer,input)	Row for subwindow
c  ICOL (integer,input)	Column for subwindow
c  IWIDTH (integer,input)	Width for subwindow
c--
c	jan 90  wh
c----------------------------------------------------------------c
c	include '../inc/constants.inc'
c	external move	!$pragma C ( move )
c	external addstr	!$pragma C ( addstr )
c	character*80 message
c	character*16 lines
c	character*20 blanks /' '/
c	equivalence (lines,blanks)
c	character*80 error
c

c	CALL COMGETA('WATCHMES',MESSAGE,80,ERROR)
c	   CALL ERROR_CHECK(ERROR)
c	IF(MESSAGE(7:8).NE.'  ') THEN
c	    CALL MOVE(%val(IROW),%val(ICOL))
c	    CALL ADDSTR(MESSAGE)
c	END IF
c	RETURN
c	END

	SUBROUTINE ERROR_CHECK(ERROR)
	character*80 error

	IF (ERROR(1:2).NE.'OK') THEN
	 IF(ERROR(1:4).NE.'TPEE') CALL WRITE_DISPLAY('DISPLAY:'//ERROR(1:70))
	END IF
	RETURN
	END	

