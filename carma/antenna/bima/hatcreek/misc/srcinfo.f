c= SRCINFO - Calculates the current LST and elevation of source.
	PROGRAM SRCINFO
c: observing
c+
c	srcinfo calculates the current LST and elevation of source.
c	If the source is a planet, srcinfo  also calculates the 
c	predicted flux for each baseline. It is used by command files 
c	(cf. maserp).
c
c@ source
c	Source name [8 characters] from catalog or planet
c
c@ epoch
c	Julian Day number of observation (0 means present)
c
c@ catalog
c	Catalog name ($HATCAT/j2000.cat is assumed)
c
c@ elevlim
c	Elevation limit (deg) is only really needed when srcinfo
c	is called from command files to determine if observing can be done.
c
c@ fluxmin
c	The percentage of the total planet flux necessary for pointing.
c	15 percent will force the first maximum of the bessel function
c 
c@ jylimit
c	The minimum flux (JY) necessary for pointing in addition to 
c	the percentage minimum described above (only for planets)
c
c@ env
c	Enviroment variable to output:
c	 srcinfo outputs the 
c	 if "Y" then writes a file srcinfo.csh which can be 
c	 sourced to produce the following environmental variables: 
c	       1) source elevation (deg) to  ELEV
c	       2) predicted planet fluxes to 
c	          variables JY1, JY2,...,JY9 (one for each baseline)
c		  if flux is not known (MERCURY) or source is not a 
c		  planet then JY1=JY2=JY3=...=9999
c	       3) LST time in HHMM format to CSLST
c
c	  WARNING: may be dangerous to use "Y" if a command file 
c		currently running uses these global variables.
c--
C	JC FEB 87 ,added rise and set time stuff JC JUL 87
C	added percent of total flux limit (for planets) jJC jul87
C	--fixed format for Sun and Moon diameters;  set flux to 0 if
C	  TB is 0 (Sun, Moon, Mercury)   --JB, Sept. 1988
C	WH SEP 89  DBLE(RA1950) ADDED
C	WH NOV 89 keyini added
c	unix->  nov 90 wh
C----------------------------------------------------------------------------
	character*14 angles,a1,a2
	character*1 global
	character*80 error
	include '../inc/constants.inc'
	include '../inc/ephem.inc'
	record /asource/ s
	real*4 plflx/9999./
	real*8 am /.004363323/   ! 1 minute of time in radians
	real*8 epoch
	real*4 secs /206264.8/
	real*4 degs /57.29577/
	real*8 lo,lst,u_ns(nants),v_ns(nants),frequp,freqlow,ut
	integer ticks
	real*4 flxmax,flxave
	integer*4 numflx
	real*4 j1xbyx,jy_lim
	integer*4 month,day,year
	character*14 elevstring
	character*14 cslststring
	character*12 src
	character*50 cat

	type *,' srcinfo - find current position of a source'
	call mbusopen(error)
	 if(error(1:2).ne.'OK') type *,error
	call keyini
	call keya('source',src,' ')
	call keya('catalog',cat,' ')
	call keyr('elevlim',el_lim,10.)
	call keyr('fluxmin',percent_res,10.)
	call keyr('jylimit',jy_lim,0.)
	call keyd('epoch',epoch,0.d0)
	call keya('globals',global,'n')
	call keyfin
	if(global.eq.'y') open(16,file='srcinfo.csh',status='unknown')
	
	CALL GET_TIMES(TICKS,2,MONTH,DAY,YEAR,UT,LST,ERROR)
		IF(ERROR(1:2).NE.'OK') TYPE *,'srcinfo:'//ERROR

	CALL GET_SOURCE_NOLOCK(SRC,CAT,EPOCH,S,ERROR)
	IF(ERROR(1:2).NE.'OK') THEN
			TYPE *,'srcinfo:'//ERROR
			IED=90
			GOTO 999
	ELSE

C  --  compute elevation
	  ELEV = ASIN(SIN(S.DEC0)*0.656+COS(S.DEC0)*COS(LST-S.RA0)*0.7547)
	  elow = (pi/180.)*el_lim
	  acosarg=(sin(elow)-sin(S.dec0)*0.656)/(cos(S.dec0)*.7547)
	  if (acosarg.gt..9999907) then 
		acosarg=.9999907
	  else if (acosarg.lt.-.9999907) then
		acosarg=-.9999907
	  endif
	  rise=S.ra0-acos(acosarg)
	  set=S.ra0+acos(acosarg)
	  if (rise.lt.0.0) rise = tupi + rise

	  TYPE *
	  TYPE *,' Source: ',S.NAME,'   [',CAT(1:LNBLNK(CAT)),']'
	  if (epoch.ne.0.d0) TYPE *,'  Computed for epoch:',epoch
	  IF (ACOSARG.EQ..9999907) THEN
		   type *,' source never rises to ',el_lim,' degrees'
	  ELSE IF(ACOSARG.EQ.-.9999907) THEN
		   type *,' source never sets below ',el_lim,' degrees'
	  ELSE
		   a1 = ANGLES(RISE*12.D0/DPI)
		   a2 = ANGLES(SET*12.D0/DPI)
		   TYPE 996,a1,a2,el_lim
996		   format(1x,' Rises at LST = ',A,' sets at ',A,
	2	   ' (elev limit ',f5.0,' Deg)')
	  ENDIF
	  a1 = ANGLES(LST*12.D0/DPI)
	  a2 = ANGLES(ELEV*180.D0/DPI)
	  TYPE 992,a1,a2
992	   FORMAT (1x,' Present LST = ',A,6X,'source elevation: ',A)
	  TYPE 995,S.VELOC + S.VREST + S.VSUN
995	   FORMAT(1x,' Doppler velocity: ',F7.1)
	  IF(S.PLMAJ.EQ.0.) THEN
		   a1 = ANGLES(S.RA2000*12.D0/DPI)
		   a2 = ANGLES(S.DEC2000*180.D0/DPI)
		   TYPE 990,a1,a2
	  END IF
990	   FORMAT('  J2000    RA: ',A,5X,'  DEC: ',A)

	  a1 = ANGLES(S.RA0*12.D0/DPI)
	  a2 = ANGLES(S.DEC0*180.D0/DPI)
	  TYPE 991,a1,a2
991	   FORMAT('  Present  RA: ',A,5x,'  DEC: ',A)

	  IF(S.PLMAJ.NE.0.) THEN
		a1 = ANGLES(S.RA1*12.D0/DPI*AM)
		a2 = ANGLES(S.DEC1*180.D0/DPI*AM)
		TYPE 998,a1,a2
		TYPE *
		TYPE 993,S.PLMAJ*SECS,S.PLMIN*SECS,S.PLANGLE
		TYPE 994,S.PLTB,S.PLFLUXCR
	  END IF

	END IF

993	FORMAT('  Planet major/minor axes: ',F8.1,'" ',F8.1,'"',
	1	' Inclin: ',F6.1,' degrees')
994	FORMAT('              temperature: ',F6.1,
	1	' K      flux: ',F6.1,' Jy')
998	FORMAT('  per min dRA: ',A,5x,' dDEC: ',A)

	IF (S.PLMAJ.NE.0.) THEN

	  CALL COMGETD('LO1',LO,1,ERROR)
	   CALL WRITE_ERROR('srcinfo',ERROR)
	  FREQUP=LO+1.5
	  FREQLOW=LO-1.5
	  if(S.PLTB.GT.0.) then
	    FLXUPR=S.PLMAJ*S.PLMIN*1158.1*(FREQUP)**3/
	1   		(EXP(.04801*(FREQUP)/S.PLTB)-1.)
	    FLXLWR=S.PLMAJ*S.PLMIN*1158.1*(FREQLOW)**3/
	1 	(EXP(.04801*(FREQLOW)/S.PLTB)-1.)
	  else		!fix-up for sun,moon,mercury, where PLTB=0.
	    FLXUPR=0.
	    FLXLWR=0.
	  endif

	  type *,'  Antenna   Max Flux  Ave Flux  '
  	  CALL CALCUNSVNS(LST,U_NS,V_NS,ERROR)
	   CALL WRITE_ERROR('srcinfo',ERROR)
          COSI=COS(S.PLANGLE)
          SINI=SIN(S.PLANGLE)
	  DO I=1,NANTS
	    FLXMAX=0.
	    FLXAVE=0.
	    NUMFLX=0
	    DO J=1,NANTS
	     IF(I.NE.J) THEN
		UB = U_NS(J)-U_NS(I)
		VB = V_NS(J)-V_NS(I)
		BETA_NS=DPI*SQRT((S.PLMAJ*(UB*COSI-VB*SINI))**2
	1   		+(S.PLMIN*(UB*SINI+VB*COSI))**2)
		PLFLX=ABS(J1XBYX(BETA_NS*FREQUP))*FLXUPR
	1		   +ABS(J1XBYX(BETA_NS*FREQLOW))*FLXLWR
		IF(PLFLX.GT.FLXMAX) FLXMAX = PLFLX
		FLXAVE = FLXAVE+PLFLX
		IF (PLFLX.GT..5D0) NUMFLX = NUMFLX+1
	     END IF
	    END DO
	    TYPE *,'  ',I,'  ',FLXMAX,FLXAVE/(NANTS-1)
	    IF(GLOBAL.EQ.'y') 	WRITE(16,'(A,I1,2x,I4.4)') 
	1					'setenv JY',I,INT(FLXAVE)
	  END DO
	END IF

c		assign global variables CLST & ELEV

999	IF(GLOBAL.NE.'y') STOP

	ELEVSTRING = ANGLES(ELEV*180.D0/DPI)
	CSLSTSTRING = ANGLES(LST*12.D0/DPI)
	WRITE(16,'(A,A)')'setenv ELEV ',ELEVSTRING(1:index(elevstring,':')-1)
	WRITE(16,'(A,A)')'setenv CSLST ',CSLSTSTRING(1:2)//CSLSTSTRING(4:5)

	STOP
	END

C-------------------------------------------------------------C
	SUBROUTINE GET_SOURCE_NOLOCK(SOURCE,CATALOG,PRESENT_EPOCH,S,
	1	ERROR)
C  SENDS MESSAGE TO EPHEMERIS TO PRECESS A SOURCE WITHOUT LOCKS
C   THIS VERSION DOES NOT FILL IN COMMON VARIABLES
C  WH  JAN 87
C-------------------------------------------------------------C
	include	 '../inc/ephem.inc'
	record /asource/ s
	include '../inc/constants.inc'
	character*(*) source,catalog
	real*8 present_epoch,freq,TJD
	integer ticks(2)
	integer*4 mon,day,year
	real*8 ut,lst, julian
	character*80 error

C	--NO ACTION IF SOURCE=TEST
	IF(SOURCE(1:4).EQ.'TEST' .OR. SOURCE(1:4).EQ.'test') THEN
	  ERROR='OK'
	  S.NAME = 'TEST'
	  S.VELOC=0.
	  S.VSUN=0.
	  S.VREST=0.
	  S.PLMAJ=0.
	  S.PLMIN=0.
	  S.PLANGLE=0.
	  S.RA2000=0.d0
	  S.DEC2000=0.d0
	  S.RA0=0.D0
	  S.RA1=0.D0
	  S.DEC0=0.D0
	  S.DEC1=0.D0
	ELSE
C	  --otherwise get position from ephemeris routines--
	  CALL COMGETD('FREQ',FREQ,1,ERROR)
	   IF (FREQ.LT.50.D0) FREQ = 90.D0
	  IF (PRESENT_EPOCH.EQ.0.D0) THEN
	    CALL GET_TIMES(TICKS,1,MON,DAY,YEAR,UT,LST,ERROR)
		TJD = JULIAN(DAY,MON,YEAR,UT)
	  ELSE
		TJD = PRESENT_EPOCH
	  END IF
	  CALL EPHEMERIS(source,catalog,TJD,FREQ,S,ERROR)

	END IF

	RETURN
	END


