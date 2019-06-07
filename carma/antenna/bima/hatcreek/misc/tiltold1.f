c= TILT - Rotates the antenna in azimuth and measures the tilt.
	PROGRAM TILT
c: utility
c+
c	TILT rotates the antenna in azimuth and measures the
c	antenna tilt at intervals.  The tilt can be 
c	plotted and the	shim thicknesses to level the antenna
c	are calculated.
c
c@ ants
c	Antenna selection flag:
c	 * means use all nants antennas
c	 xyz means use antennas x y and z
c	 *-xyz means use all antennas except x y and z
c@ meter
c	Tiltmeter used: 1 or 2. (Default 1).
c@ azim
c	Starting azimuth. The antenna is rotated from AZIM to -AZIM
c	Default=-120 degrees.
c@ elev
c	Elevation (degees) during measurement. Default=10 for snow.
c@ pterm
c	Terminal PGPLOT device. Default is no terminal plot.
c@ phard
c	Hardcopy PGPLOT device. Default is no Hardcopy plot.
c--
c  History:
c    jan 92 wh
c    nov 92 wh   change to atod
c    01jan93 mchw  replaced dir by starting azimuth. In-code doc.
c    aug93 wh   set exit status
c    feb94 sz added 4 lines to test for ant 3 and add +90 to aztilt(1)
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='TILT (version 1.1 27-aug-93)')
	parameter (numpts=17)
	include '../inc/constants.inc'
	integer ants
	logical select
	real azstart,elevation,azstep
	character*20 pterm,phard
	character*80 error,mess
	integer itrack(nants)
	double precision azims(nants),elevs(nants)
	character*8 direc(-1:1)/'westward','nowhere','eastward'/
 	real shims(nants),shimn(nants),shimw(nants)
	real sincos(numpts,nants),calc(numpts,nants),tp(2,nants)
	real aztilt(2)/90,0/		! azimuths of tiltmeters
c
c  start by getting arguments.
c
	call keyini
	call keyants(ants,1,error)
	if(error(1:2).ne.'OK') type *,error
	if(error(1:3).eq.'Too') goto 99
	call keyi('meter',meter,1)
	call keyr('azim',azstart,-120.)
	call keyr('elev',elevation,10.)
	call keya('pterm',pterm,' ')
	call keya('phard',phard,' ')
	call keyfin
c
c Check inputs.
c
	if(meter.lt.1 .or. meter.gt.2) then
		error = 'meter must be 1 or 2'
		goto 99
	endif
	azstep = -2.*azstart/(numpts-1)
	print *,'azstep=',azstep
	if(azstep.eq.0.)stop
	if(.true.)goto 1234
c
	call mbusopen(error)
	 if(error(1:2).ne.'OK') goto 99

c --- announce and lock up resources to be used ---
	call output(version)
	write(mess,'(a,a,z4,a,f5.0,a)')
	1	' (reading  ','  antennas:',ants,
	2	  ' at ',elevation,' DEG elevation)'
	call announce(mess,error)
	IF(error(1:2).ne.'OK') GOTO 99

	call lock_up(ants,'l','lockant',' ',' ',' ',error)
	IF(error(1:2).ne.'OK') GOTO 99

C --- setup the motion ---
	CALL COMGETI('ITRACK',ITRACK,nants,ERROR)
	 IF(ERROR(1:2).NE.'OK') GOTO 99
	CALL COMGETD('AZIM',AZIMS,nants,ERROR)
	 IF(ERROR(1:2).NE.'OK') GOTO 99
	CALL COMGETD('ELEV',ELEVS,nants,ERROR)
	 IF(ERROR(1:2).NE.'OK') GOTO 99
c
c  Select AZ/EL pointing.
c	
	DO  I=1,NANTS
	 IF(SELECT(I,ANTS)) THEN
	  ITRACK(I) = 1
	  ELEVS(I) = ELEVATION
	  AZIMS(I) = AZIMUTH
	 END IF
	END DO
	CALL COMPUTI('ITRACK',ITRACK,nants,ERROR)
	 IF(ERROR(1:2).NE.'OK') GOTO 99
	CALL WAIT_TICKS(100)
	CALL COMPUTD('AZIM',AZIMS,nants,ERROR)
	 IF(ERROR(1:2).NE.'OK') GOTO 99
	CALL COMPUTD('ELEV',ELEVS,nants,ERROR)
	 IF(ERROR(1:2).NE.'OK') GOTO 99

c --- measure tilt at AZSTEP degree intervals in azimuth ---
	CALL TILTMEASURE(ANTS,METER,SINCOS,azstart,azstep,ERROR)
	 IF(ERROR(1:2).NE.'OK') GOTO 99

c --- least squares fit and calculate shims for antennas ---
1234	continue
	CALL TILT_SHIMS
     *		(ANTS,METER,AZTILT,SINCOS,CALC,azstart,azstep,
     *			SHIMW,SHIMN,SHIMS,tp)

	IF(PTERM.NE.' ') call tilt_graph(ants,aztilt,sincos,calc,
     *	  azstart,azstep,PTERM,shimw,shimn,shims,tp)
	IF(PHARD.NE.' ') call tilt_graph(ants,aztilt,sincos,calc,
     *	  azstart,azstep,PHARD,shimw,shimn,shims,tp)
c
c  Finish up.
c
99	if (error(1:2).ne.'OK') call bug('f',error)
c	i=ieee_flags('clear','exception','all',out)	
	END

c-----------------------------------------------------------C
	SUBROUTINE TILTMEASURE(ANTS,METER,SINCOS,azstart,azstep,ERROR)
c	measure tilt at intervals of azstep in azimuth
c----------------------------------------------------------C
	include '../inc/constants.inc'
	parameter (numpts=17)
	parameter (nsamp=60)
	real azstart,azstep
	character*80 error
	real sincos(numpts,nants)
	logical select
	real arcmin(nsamp),ave(nants),atod
	integer ants
	double precision azims(nants)
	integer ant
	character units*4,name*16
	integer meter,tiltadd(2) / 1092,1093 /

	type 798
798	format(' ant  azim  ave  rms    ---------tiltmeter samples ('')------'
	1 '--------')

	CALL COMGETD('AZIM',AZIMS,nants,ERROR)
	  IF(ERROR(1:2).NE.'OK') RETURN
	DO K=1, NUMPTS
	  DO  ANT=1,nants
	    IF(SELECT(ANT,ANTS)) AZIMS(ANT) = azstart+azstep*(K-1)
	  END DO
	  CALL COMPUTD('AZIM',AZIMS,nants,ERROR)
	    IF(ERROR(1:2).NE.'OK') RETURN

	  CALL WAIT_TICKS(200)		! wait 2 seconds

c --- wait for end of slew ---	
c --- note: tilt only waits for slew flags ---
	  CALL WAIT_FOR_ANTS(ANTS,14,ERROR)
	  IF(ERROR(1:2).NE.'OK') RETURN
	  CALL WAIT_TICKS(1000)

C --- read the tiltmeters ---
	  DO ANT=1,nants
	    IF(SELECT(ANT,ANTS)) THEN
	      do m=1,nsamp
 	        ARCMIN(M) = ATOD(ANT,TILTADD(METER),iraw,units,name,ERROR)
		CALL WRITE_ERROR('TILT',ERROR)
		CALL WAIT_TICKS(4)
	      enddo
	      call averms(nsamp,arcmin,av,rms)
	      SINCOS(K,ANT) = -av * 60.	! change sign arcsecs
	      rms = rms*60.
		if(nsamp.eq.8)then
		  print 800,ANT,azims(ANT),ARCMIN,sincos(k,ANT)
		else
		  print 890,ant,azims(ant),sincos(k,ant),rms
		  do i=1,nsamp,10
		    print 880,(arcmin(i-1+j),j=1,10)
		  enddo
		endif
	    END IF
	  END DO
	END DO
800	format(' ',i2,x,f6.0,x,8F7.3,F9.2)
880	format(10f7.3)
890	format(' ',i2,x,f6.0,x,2F9.2)

	DO ANT=1,nants
	  IF(SELECT(ANT,ANTS)) THEN
	    AVE(ANT)=0.
	    DO K=1,NUMPTS
		AVE(ANT)=AVE(ANT)+SINCOS(K,ANT)
	    ENDDO
	    AVE(ANT)=AVE(ANT)/(NUMPTS)
	    DO K=1,NUMPTS
		SINCOS(K,ANT)=SINCOS(K,ANT)-AVE(ANT)
	    ENDDO
	  ELSE
	    AVE(ANT) = 0.
	  ENDIF
	ENDDO
	print *,'Average readings(") ',ave
	END
C------------------------------------------------------------------C
	SUBROUTINE TILT_SHIMS(ANTS,METER,AZTILT,TILT,CALC,
     *	  azstart,azstep,shimw,shimn,shims,tp)
c	least squares fit to tilt and calculate shims for antennas
C-------------------------------------------------------------------C
	parameter  (NUMPTS=17)
	include '../inc/constants.inc'
	integer ants,meter,ant
	real azstart,azstep
	real shimw(nants),shimn(nants),shims(nants)
	logical select
	character*120 line
	common/point/ptype(numpts,nants)
	real aztilt(2),ang(numpts),tilt(numpts,nants),calc(numpts,nants)
	integer*4 ikeep(numpts)
	real rmat(3,3),v(3),ans(3),relat(3),row(3),resid(numpts),tp(2,nants)

	DTR = PI / 180.		! convert degrees to radians

	type *
	WRITE(LINE,'(A)')
	1  ' TILTMETER MEASUREMENT   SINE/COSINE FIT'
	TYPE *,LINE
	call logit(line)
	WRITE(LINE,100)
100	FORMAT('Antenna       Sine  Cosine
	1   Shim West   Shim South  Shim North   Azimuth of tiltmeter')
	call logit(line)
	TYPE *,LINE
	
      DO  ANT=1,nants
	IF(SELECT(ANT,ANTS)) THEN

c --- least squares fit for antenna tilt ---
          ITER = 0
	  DO K = 1,numpts			
	    IKEEP(K) = 1			!Init good point array
	    PTYPE(K,ANT) = 43.			!Set ptype array
	  ENDDO
10	  DO  M=1,3
	    V(M)=0.
	    DO  N=1,3
	     RMAT(M,N)=0.
	    ENDDO
	  ENDDO
	   DO  K=1,numpts
	      IF (IKEEP(K).EQ.1) THEN		!Only let in good points
	        ANG(K) =  (azstart + (K-1)*azstep)*dtr
	        ROW(1) = SIN(ANG(K))
	        ROW(2) = COS(ANG(K))
	        ROW(3) = 1.
	        DO M = 1,3
	          V(M) = V(M) + TILT(K,ANT) * ROW(M)
	          DO N = 1,3
	            RMAT(M,N) = RMAT(M,N) + ROW(M) * ROW(N)
	          ENDDO
	        ENDDO
	       ENDIF
	    ENDDO

	    CALL INVERT(3,RMAT,V,ANS,RELAT)

	    AV = 0.
	    RMS = 0.
	    ICOUNT = 0
	    DO  K=1,numpts
	      CALC(K,ANT) = ANS(1)*SIN(ANG(K)) + ANS(2)*COS(ANG(K)) + ANS(3)
	      RESID(K) = TILT(K,ANT) - CALC(K,ANT)
	      IF (IKEEP(K).EQ.1) THEN		!Only let in good points
	        AV = AV + RESID(K)
	        RMS = RMS + RESID(K)*RESID(K)
		ICOUNT = ICOUNT + 1
	      ENDIF
	    ENDDO
	    AV = AV / (ICOUNT)
	    RMS = SQRT(RMS/(ICOUNT) - AV*AV)

c --- delete bad points and go back to try again --- (TWICE)
	    IF (ITER.EQ.2) GO TO 105
	    ITER = ITER+1
	    NUMBADPTS=0
	    DO K = 1, NUMPTS
	      IF(ABS(RESID(K)) .GT. 2.*RMS) THEN	!only keep good points
		IKEEP(K) = 0
		PTYPE(K,I) = 41.			!change ptype for plot
		NUMBADPTS=NUMBADPTS+1
	      ENDIF
	    ENDDO
	    IF((ITER.LT.3.).AND.(NUMBADPTS.GT.0)) GO TO 10
	    
c --- calculate shims to level antennas ** needs changefor new ants---
105	    IF(ANT.eq.3) AZTILT(1) = AZTILT(1)+90.0
            S =  COS(AZTILT(1)*DTR)*ANS(1)+SIN(AZTILT(1)*DTR) * ANS(2)
	    C = -SIN(AZTILT(1)*DTR)*ANS(1)+COS(AZTILT(1)*DTR) * ANS(2)
	    IF(ANT.eq.3) AZTILT(1) = AZTILT(1)-90.0
	    SHIMW(ANT) = 135.*S*4.8481368E-6
	    SHIMN(ANT) = 78.*C*4.8481368E-6
	    SHIMS(ANT) =-78.*C*4.8481368E-6

c --- set lowest shim to 0 (shims all must be +ve.) ---
	    VMIN=AMIN1(SHIMW(ANT),SHIMN(ANT))
	    VMIN=AMIN1(SHIMS(ANT),VMIN)
	    SHIMW(ANT)=SHIMW(ANT)-VMIN
	    SHIMN(ANT)=SHIMN(ANT)-VMIN
	    SHIMS(ANT)=SHIMS(ANT)-VMIN

c --- calculate theta (direction) and phi (magnitude) of tilt ---
c --- antenna 3 is tiltmeter is different by 90 deg!  Zhou/Forster
	    IF(ANT.eq.3) AZTILT(1) = AZTILT(1)+90.0
	    theta = atan2(ans(1),ans(2))/dtr + aztilt(1) + 180.
	    if(theta.gt.360.) theta=theta-360.
	    if(theta.lt.-360.) theta=theta+360.
	    phi   = sqrt(ans(1)*ans(1) + ans(2)*ans(2))
	    tp(1,ANT) = theta	! tilt direction in degrees
	    tp(2,ANT) = phi	! tilt magnitude in arcsecs

	    WRITE(LINE,110)ANT,S,C,SHIMW(ANT),SHIMS(ANT),SHIMN(ANT),AZTILT(1)
110	    FORMAT(' ',I2,3X,2X,2F10.5,2X,3F10.4,5X,F6.0)
	    IF(ANT.eq.3) AZTILT(1) = AZTILT(1)-90.0
	    call logit(line)
	    TYPE *,LINE(1:80)
	  ENDIF
	ENDDO

	J=1
c	WRITE(LINE,120)
c120	FORMAT(20X,'DATA  FIT AND ERROR FOR TILT   (ARCSEC)')
c	WRITE(LINE,121)
c121	FORMAT(' ANGLE',5X,' ANTENNA 1',15X,'  ANTENNA 2',15X
c	2,'ANTENNA 3')
c	type *,line(:80)
c	DO K=1,numpts
c	 WRITE(LINE,122)ANG(K)*180./PI,(TILT(K,I),CALC(K,I),
c	1 (TILT(K,I)-CALC(K,I)),I=1,3)
c122	 FORMAT(X,F6.0,9F8.1)
c	 call logit(line)
c	 type *,line(:80)
c	ENDDO
	END
c----------------------------------------------------------------------------C
	SUBROUTINE TILT_GRAPH(ANTS,AZTILT,SINCOS,CALC,azstart,azstep,
	1	PLOTTER, SHIMW, SHIMN, SHIMS,tp)
c  Plot	the output arrays for tiltmeter
c----------------------------------------------------------------------------c
	include '../inc/constants.inc'
	parameter (numpts=17)
	real azstart,azstep
	real shimw(nants),shimn(nants),shims(nants)
c 	character*50 buf
	integer ants
	common/point/ptype(numpts,nants)
	real sincos(numpts,nants),calc(numpts,nants),aztilt(2)
	character*(*) plotter
	character*60 title
	real x(numpts),tp(2,nants)
	
	call pgbegin(0,plotter,3,3)
	call pgsch(1.6)
	call pgask(.false.)
	DO I=1,nants
	    SMAX=-1000.
	    SMIN=1000.
	    DO K=1,NUMPTS
		X(K) =  azstart+azstep*(K-1)
		SMAX=MAX(SMAX,SINCOS(K,I))
		SMIN=MIN(SMIN,SINCOS(K,I))
	    ENDDO
	    IF((SMAX.NE.0.).AND.(SMIN.NE.0.)) THEN
	     call pgenv(-abs(1.1*azstart),abs(1.1*azstart),
     *		SMIN-0.1*(smax-smin),SMAX+0.1*(smax-smin),0,1)
	     write(title,'(a,i2,a,f7.4,a,f7.4,a,f7.4)')
	1    'Ant:',i,' shims: W ',shimw(i),' N ',shimn(i),' S ',shims(i)
	     call pglabel('Azim','Arcsecs',title)
	     CALL pgPOINT(numpts,X,SINCOS(1,I),9)	    
	     CALL pgline(numpts,X,CALC(1,I))
	     IF(azstart.gt.0.) CALL pgmtext('B',-.8,1.,1.,'westward')
	     IF(azstart.lt.0.) CALL pgmtext('B',-.8,1.,1.,'eastward')
	    ENDIF
	ENDDO
	call pgsch(4.)
	call pgiden
	call pgend

c		theta = tp(1,i)			! tilt azimuth in degrees
c		phi   = tp(2,i)			! tilt angle in arcsec
c		j = (i-1)*2
c		ct = cos(theta/57.3)
c		st = sin(theta/57.3)
c		b  = 246000.*sin(phi/(3600*57.3))  
c		b1 = b*0.67*ct
c		b2 = b*st			! baseline in 10000*ns
c		b3 = b*0.74*ct
c		apc4 = -phi*st/60.		! pointing in arcminutes
c		apc5 =  phi*ct/60.
c
c	   WRITE(BUF,'(x,I1,2X,F4.0,F6.0,2X,2F5.1,2x,3f6.0)')
c	1		I,phi,theta,apc4,apc5,b1,b2,b3
	END		
C------------------------------------------------------------------C
	subroutine averms(nsamp,x,ave,rms)
c  calculate ave and rms for x(nsamp)
c
	integer nsamp
	real x(nsamp),ave,rms,sum,squ
c
	sum = 0.
	squ = 0.
	do i=1,nsamp
	  sum = sum + x(i)
	  squ = squ + x(i)*x(i)
	enddo
	ave = sum/nsamp
	rms = squ/nsamp - ave*ave
	if(rms.gt.0.)then
	  rms = sqrt(rms)
	else
	  rms = 0.
	endif
	end
