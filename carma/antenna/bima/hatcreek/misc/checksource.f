c*
      PROGRAM CHECKSOURCE
c= CHECKSOURCE - calculates the current LST and elevation of source.
c: observing
c+
c	CHECKSOURCE calculates the current LST and elevation of source.
c	If the source is a planet, CHECKSOURCE  also calculates the 
c	predicted flux for each baseline. It is useful for command files 
c	(cf. POINTING.CSH) and checking catalog inputs.
c	For planets, the Geocentric apparent position replaces the
c	J2000 position in the miriad header; this can be compared
c	with the Nautical Almanac if you remember that epoch needs
c	to be adjusted by deltaT from page K9 to obtain dynamical
c	time used by the NA.
c	Note: Naif saves the last source/catalog found so when looking
c	up a source twice in succession, the second time repeats the
c	first. Look up a different source to force a new catalog lookup. 
c@ source
c	Source name [8 characters] from catalog or planet
c	Other objects on the naif approved list can be accessed
c	as _xxxxxx where xxxxxx is their naif number.
c
c@ epoch
c	Julian Day number of observation (0 means present time)
c	Note 1: A format like 'YYMMMDD:[HH:MM...' is converted
c	to a julian day number using subroutine DAYJUL. 
c       (compatible with the output from "date -u +%y%h%d:%H:%M:%S")
c	Note 2: A number -1000 to 1000 means delta days from present.
c	Default uses value from COMMON.
c
c@ freq
c	Observing frequency in GHz. Default=0.d0 uses value from COMMON.
c
c@ catalog
c	Catalog name; first catalog is searched (this is a path, not
c	just a filename). If it is not found then the filename is
c	stripped off and $HATCAT/filename is searched.
c	If blank, $HATCAT/j2000.cat is assumed.
c
c@ elevlim
c	Elevation limit (deg) is only really needed when checksource
c	is called from command files to determine if observing can be done.
c@ setme
c	Setting this causes checksource to only output the
c	value that is desired; this allows the shell command:
c	set=`checksource ... setme=xxx` to work.
c	Current values can be:
c	 visible	y or n	 (is the source above elevlim)
c	 azel		azim(deg) elev(deg)
c        riset          rise and set in format "HH:MM:SS.SSS HH:MM:SS.SSS"
c       Note: there is no minimum match on these options.
c
c--
C	JC FEB 87 ,added rise and set time stuff JC JUL 87
C	WH NOV 89 keyini added
c	unix->  nov 90 wh
c	setme added mar 93
c     added setme=azel to print out (az,el) in radians        jun 93   - pjt
c	2aug93 wh - match changes in get_source
c	27aug93 wh - add exit status
c	30aug  wh - fix bug from 27aug
c	16sep  wh - fix units for dRA,dDEC
c       17oct94 pjt - added setme=riset for HAPLOT program
c                     added option to enter epoch=YYMMMDD etc. format 
c          also changed indentations and tabs for some improved readability
c   26jul95 mchw changed format for 8 ants.
c       nov95 wh   9 ants
c	17jun97 rp	10 ants
c   28jul97 mchw print vsource,  vrest,  vsun
c   30jul97 mchw set min freq to 0. GHz in getsource.
c   31jul97 mchw Allow user input for frequency.
c   31oct97 mchw Calculate visibility flux at freq, not at (usb+lsb)/2
C----------------------------------------------------------------------------
      character*14 angles,a1,a2
      character*10 setme
      character*80 error
      include '../inc/constants.inc'
      include '../inc/ephem.inc'
      record /asource/ s
      real*4 plflx/9999./
      real*8 am /.004363323/   ! 1 minute of time in radians
      real*8 epoch,tjd
      real*4 secs /206264.8/
      real*4 degs /57.29577/
      real*8 lst,u_ns(nants),v_ns(nants),ut,freq
      integer ticks
      real*4 j1xbyx,az
      integer*4 month,day,year
      character*12 src
      character*100 cat
      character*9 splflx(nants),sdis(nants)

      call mbusopen(error)
      if(error(1:2).ne.'OK') goto 99
      call keyini
      call keya('source',src,' ')
      call keya('catalog',cat,' ')
      call keyr('elevlim',el_lim,10.)
      call keyd('freq',freq,0.d0)
      call keyepoch('epoch',epoch,0.d0)
      call keya('setme',setme,'n')
      call keyfin
	
      CALL GET_TIMES(TICKS,2,MONTH,DAY,YEAR,UT,LST,ERROR)
      IF(ERROR(1:2).NE.'OK') GOTO 99

      CALL GET_SOURCE_NOLOCK(SRC,CAT,EPOCH,S,freq,TJD,ERROR)
      IF(ERROR(1:2).NE.'OK') GOTO 99

C  --  compute elevation
      ELEV = ASIN(SIN(S.DEC0)*0.65365
	1	  + COS(S.DEC0)*COS(LST-S.RA0)*0.756797)
      elow = (pi/180.)*el_lim
      acosarg=(sin(elow)-sin(S.dec0)*0.65365)/(cos(S.dec0)*.756797)
      if (acosarg.gt..9999907) then 
         acosarg=.9999907
      else if (acosarg.lt.-.9999907) then
         acosarg=-.9999907
      endif
      rise=S.ra0-acos(acosarg)
      set=S.ra0+acos(acosarg)
      if (rise.lt.0.0) rise = tupi + rise

c  --	handle some setme= cases; Uses 'GOTO 99' to escape from program
      IF(SETME(1:7).EQ.'visible') THEN
	   IF(ELEV.GT.ELOW) THEN
		WRITE(*,'(''y'')')
	   ELSE
		WRITE(*,'(''n'')')
	   END IF
	   GOTO 99
      ELSE IF (SETME(1:4).EQ.'azel') THEN
          az = (LST-S.RA0)*DEGS
          IF(az.LT.-180.0) az = az + 360.0
          IF(az.GT.180.0) az = az - 360.0
          TYPE *,az,ELEV*DEGS
          GOTO 99
      END IF

      IF (setme(1:5).EQ.'riset') THEN
c        Doesn't handle both circumpolar cases yet....
c	 but should output all 0's (rise=set=0)
         a1 = ANGLES(RISE*12.D0/DPI)
         a2 = ANGLES(SET*12.D0/DPI)
         WRITE('(A,'' '',A)') a1,a2
         GOTO 99
      ELSE
	  TYPE *
	  TYPE *,' Source: ',S.NAME,'   [',CAT(1:LNBLNK(CAT)),']'
	  TYPE *,'  Computed for julian date:',tjd
	  IF (ACOSARG.EQ..9999907) THEN
		   type *,' source never rises to ',el_lim,' degrees'
	  ELSE IF(ACOSARG.EQ.-.9999907) THEN
		   type *,' source never sets below ',el_lim,' degrees'
	  ELSE
		   a1 = ANGLES(RISE*12.D0/DPI)
		   a2 = ANGLES(SET*12.D0/DPI)
		   TYPE 996,a1,a2,el_lim
996		   FORMAT(1x,' Rises at LST = ',A,' sets at ',A,
     *                       ' (elev limit ',f5.0,' Deg)')
          ENDIF
      ENDIF
        
      a1 = ANGLES(LST*12.D0/DPI)
      a2 = ANGLES(ELEV*180.D0/DPI)
      TYPE 992,a1,a2
992   FORMAT (1x,' Present LST = ',A,6X,'source elevation: ',A)
      TYPE 995,S.VELOC + S.VREST - S.VSUN, S.VELOC , S.VREST , S.VSUN
995   FORMAT(1x,' Doppler velocity: ',F8.2,
     *              '  vsource:',f8.2,'  vrest:',f8.2,'  vsun:',f8.2)
c      IF(S.PLMAJ.EQ.0.) THEN
	   a1 = ANGLES(S.RA2000*12.D0/DPI)
	   a2 = ANGLES(S.DEC2000*180.D0/DPI)
	   TYPE 990,a1,a2
c      END IF
990   FORMAT('  J2000    RA: ',A,5X,'  DEC: ',A)

      a1 = ANGLES(S.RA0*12.D0/DPI)
      a2 = ANGLES(S.DEC0*180.D0/DPI)
      TYPE 991,a1,a2
991   FORMAT('  Present  RA: ',A,5x,'  DEC: ',A)

c  --	convert rad/tick to  hr/deg  per minute of time
      a1 = ANGLES(S.RA1*12.D0/DPI*6000.d0)
      a2 = ANGLES(S.DEC1*180.D0/DPI*6000.d0)
      TYPE 998,a1,a2
998   FORMAT('  per min dRA: ',A,5x,' dDEC: ',A)

      IF(S.PLMAJ.NE.0.) THEN
		TYPE *
		TYPE 993,S.PLMAJ*SECS,S.PLMIN*SECS,S.PLANGLE
		TYPE 994,S.PLTB,S.PLFLUXCR,FREQ
      END IF

993   FORMAT('  Planet major/minor axes: ',F10.3,'" ',F10.3,'"',
     *	' Inclin: ',F6.1,' degrees')
994   FORMAT('              temperature: ',F6.1,
     *	' K      flux: ',F12.3,' Jy at',F12.3,' GHz')

      IF (S.PLMAJ.NE.0.) THEN
	   if(S.PLTB.GT.0.) then
	    FLUX=2.*S.PLMAJ*S.PLMIN*1158.1*(FREQ)**3/
     *   		(EXP(.04801*(FREQ)/S.PLTB)-1.)
	   else		!fix-up for sun,moon,mercury, where PLTB=0.
	    FLUX=0.
	   endif

	   type *
	   type *,'ANT      FLUX \\ -SQRT(U*U+V*V) '
	   TYPE *,'       1        2        3        4        5'//
     *     	'        6        7        8        9       10'
  	   CALL CALCUNSVNS(LST,U_NS,V_NS,ERROR)
	   CALL WRITE_ERROR('CHECKSOURCE',ERROR)
          COSI=COS(S.PLANGLE)
          SINI=SIN(S.PLANGLE)
	   DO I=1,10
	    DO J=1,10
	     IF(I.NE.J) THEN
		   UB = U_NS(J)-U_NS(I)
		   VB = V_NS(J)-V_NS(I)
		   BETA_NS=DPI*SQRT((S.PLMAJ*(UB*COSI-VB*SINI))**2
     *	   		+(S.PLMIN*(UB*SINI+VB*COSI))**2)
		   PLFLX=ABS(J1XBYX(SNGL(BETA_NS*FREQ)))*FLUX
		   WRITE(SPLFLX(J),'(F9.2)') PLFLX
		   WRITE(SDIS(J),'(F9.2)') -SQRT(UB*UB+VB*VB)

	     ELSE
		   SPLFLX(J) = ' '
		   SDIS(J) = ' '
	     END IF
	    END DO
		TYPE '(I2,10A)',I,(SPLFLX(K),K=1,I-1),'         ',
	1					(SDIS(K),K=I+1,10)
	   END DO
      END IF

c -- "GOTO 99" : the all-including escape from this program

99    if (error(1:2).ne.'OK') call bug('f',error)
      END

C-------------------------------------------------------------C
	SUBROUTINE GET_SOURCE_NOLOCK(SOURCE,CATALOG,PRESENT_EPOCH,S,
	1	freq,TJD,ERROR)
C  SENDS MESSAGE TO EPHEMERIS TO PRECESS A SOURCE WITHOUT LOCKS
C   THIS VERSION DOES NOT FILL IN COMMON VARIABLES
C  WH  JAN 87
C-------------------------------------------------------------C
	include '../inc/ephem.inc'
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
	  if(freq.eq.0.d0)CALL COMGETD('FREQ',FREQ,1,ERROR)
	   IF (FREQ.LT.0.D0) FREQ = 90.D0
	  IF (ABS(PRESENT_EPOCH).LT.1000.D0) THEN
	    CALL GET_TIMES(TICKS,1,MON,DAY,YEAR,UT,LST,ERROR)
		TJD = JULIAN(DAY,MON,YEAR,UT) + PRESENT_EPOCH
	  ELSE
		TJD = PRESENT_EPOCH
	  END IF

	  CALL EPHEMERIS(source,catalog(:len1(catalog)),TJD,FREQ,S,ERROR)

	END IF

	RETURN
	END

c***********************************************************************
      SUBROUTINE keyepoch(key,epoch, default)
c
      CHARACTER key*(*)
      DOUBLE PRECISION epoch, default
c
      CHARACTER*30 estr
      LOGICAL ok
      INTEGER len1
         
      CALL keya(key,estr,' ')
      CALL lcase(estr)
         
c       check if ':' or month (vowels 'aeuo' will do it) seems to appear
c       in string, if so, try the dayjul conversion, else assume it's
c       already a julian day...

      ok = INDEX(estr,':').GT.0 .OR.
     *     INDEX(estr,'a').GT.0 .OR.
     *     INDEX(estr,'e').GT.0 .OR.
     *     INDEX(estr,'u').GT.0 .OR.
     *     INDEX(estr,'o').GT.0
      
      IF (ok) THEN
         CALL dayjul(estr,epoch)
      ELSE
         IF (estr.EQ.' ') THEN
            epoch = default
         ELSE
            CALL atodf(estr(1:len1(estr)),epoch,ok)
            IF (.NOT.ok) CALL bug('w','(atodf) Decoding '//estr)
         ENDIF
      ENDIF
      END

