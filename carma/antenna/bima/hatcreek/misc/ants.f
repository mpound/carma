c*
	PROGRAM ANTS
c= ANTS - manually point the antennas.
c& wh
c: observing
c+
c	ANTS is a simple command used to manually point the antennas.
c
c@ ants
c	Antenna flag selects antenna combination:
c	 * means use all nants antennas
c	 xyz means use antennas x y and z
c	 *-xyz means use all antennas except x y and z
c
c@ star
c	This is a source that is put into the 2nd source 
c	common slot.
c@ source
c	This is coded as follows:
c	 If 'test' or 'TEST', point to the transmitter.
c	 If alpha, point to source in catalog. def: evenstar.16
c	 If missing, use ra-dec or az-el
c@ catalog
c	Catalog for source and star. def:evenstar.16
c@ ra
c	The RA to point to if source is missing
c	(either hr:min:sec.ss or hr.frac)
c
c@ dec
c	The DEC (either deg:min:sec.ss or deg.frac)
c
c@ az
c	The azimuth to point at if source and 
c	RA are missing. (deg)
c
c@ el
c	The elevation to use with az. (deg)
c
c@ options
c	 If "wait", ants won't exit until the antenna arrives at position
c	 If "halt", ALL ants are stopped by setting all ITRACKs to 0
c	 If "open", open the TV flaps
c
c@ trakopt
c	Tracking option, the default is C but for meaningful pointing
c	you need to enter either CH for radio or CT for optical.
c		trakopt = C  ADD CORRECTIONS	256
C		trakopt = S  SOUTH MODE EL<90	512
c		trakopt = N  North Mode El>90	64
c		trakopt = B  Both mode 0<el<180 --
c		trakopt = +  Add delaz/delel	1024
c		trakopt = H  Add horn offsets	32
c		trakopt = T  Add TV offsets	2048
c		trakopt = R  Add RA/DEC offsets 128
c		trakopt = K  copy dazimlas to dazim &
c			     change to +

c--
c	unix-> jun 91  wh
c	dec 92 tvflap
c	jan 93 trakopt
c	jun 97 keytrack instead of keytrak
c---------------------------------------------------------------------c
	implicit none
	integer j,len1
	character version*(*)
	parameter(version='ANTS (version 1.4 6-jun-97)')
	include '../inc/ephem.inc'
	record /asource/ s
	include '../inc/constants.inc'
	logical select
	character*8 trakopt
	character error*80
	real*8 dazim(NANTS),delev(NANTS)
	real*8 azim,elev,azims(NANTS),elevs(NANTS)
	integer antflag, shutoff(NANTS) /NANTS*0/
	character*20 rastring,decstring,source,star,azstring,elstring
	character*30 catalog
	character*8 action,options
	real*8 xstrlflt

	call mbusopen(error)
	if(error(1:2).ne.'OK') goto 99
	CALL ANNOUNCE(version,ERROR)
	IF(ERROR(1:2).NE.'OK') GOTO 99

c	--now begin by reading arguments--
	call keyini
	call keyants(antflag,1,error)
	 if(error(1:2).ne.'OK') type *,error
	 if(error(1:3).eq.'Too') goto 99
	call keya('source',source,' ')
	call keya('star',star,' ')
	call keya('catalog',catalog,'evenstar.16')
	call keya('ra',rastring,'999')
	call keya('dec',decstring,'999')
	call keya('az',azstring,'999')
	call keya('el',elstring,'999')
	call keya('options',options,' ')
	call lcase(options)
c  --	can we do this?
	call test_lock('LOCKANT',antflag,'C',error)
	  if(error(1:2).ne.'OK') goto 99
	call keytrack(antflag,trakopt,'C',error)
	 if(error(1:2).ne.'OK') goto 99
	call keyfin

c	-- TV flaps --
	if(options.eq.'open') then
	  call tvflap(antflag,'OPEN',error)
	else
	  call tvflap(antflag,'CLOSE',error)
	end if
	if(error(1:2).ne.'OK') type *,error

c  --	emergency stop
	if (options.eq.'halt') then
		call computi('ITRACK',shutoff,nants,error)
		if(error(1:2).ne.'OK') goto 99
		error = 'All ITRACKS set to 0'
		goto 99
	end if


c	--fabricate angles--
	if(source(1:1).ne.' ') then
		CALL TEST_LOCK('LOCKSRC',ANTFLAG,'C',ERROR)
		  if(error(1:2).ne.'OK') goto 99
		CALL GET_SOURCE(SOURCE,catalog,0.d0,S,ERROR)
		IF(ERROR(1:2).NE.'OK') GOTO 99
	else if(star(1:1).ne.' ') then
		CALL GET_STAR(STAR,catalog,0.d0,S,ERROR)
		IF(ERROR(1:2).NE.'OK') GOTO 99
		TRAKOPT = TRAKOPT(1:len1(trakopt))//'2'
	else if (rastring(1:3).ne.'999' .and. decstring(1:3).ne.'999') then
		CALL TEST_LOCK('LOCKSRC',ANTFLAG,'C',ERROR)
		  if(error(1:2).ne.'OK') goto 99
		action = 'RA/DEC'
	  	S.RA0= XSTRLFLT(RASTRING,ERROR) *DPI/12.D0
	   	 IF(ERROR(1:2).NE.'OK') GOTO 99
	  	S.DEC0= XSTRLFLT(DECSTRING,ERROR)  *DPI/180.D0
	  	 IF(ERROR(1:2).NE.'OK') GOTO 99
		s.tick = 0
		s.ra1 = 0.
		s.dec1 = 0.
		s.name = action
		call computi('SORCDATA',s,24,error)
		 if(error(1:2).ne.'OK') goto 99
	else if (azstring(1:3).ne.'999' .and. elstring(1:3).ne.'999') then
		action = 'AZ/EL'
		s.name = action
		AZIM= XSTRLFLT(AZSTRING,ERROR)
	  	 IF(ERROR(1:2).NE.'OK') GOTO 99
	  	ELEV= XSTRLFLT(ELSTRING,ERROR)
	  	 IF(ERROR(1:2).NE.'OK') GOTO 99
		call comgetd('AZIM',azims,nants,error)
	         if(error(1:2).ne.'OK') type *,error
		call comgetd('ELEV',elevs,nants,error)
	         if(error(1:2).ne.'OK') type *,error
	else
		error = 'invalid combination of source-ra-az'
		goto 99
	end if

	IF (ACTION.EQ.'AZ/EL') then
	  DO J=1,nants
	   IF(SELECT(J,ANTFLAG)) THEN
	      azims(j) = azim
	      elevs(j) = elev
	   end if
	  end do
	  call computd('AZIM',azims,nants,error)
	         if(error(1:2).ne.'OK') type *,error
	  call computd('ELEV',elevs,nants,error)
	         if(error(1:2).ne.'OK') type *,error
	END IF

	call comgetd('DAZIM',dazim,nants,error)
	        if(error(1:2).ne.'OK') type *,error
	call comgetd('DELEV',delev,nants,error)
	        if(error(1:2).ne.'OK') type *,error
	DO J=1,nants
	      IF(SELECT(J,ANTFLAG)) THEN
	        DAZIM(j) = 0.
	        DELEV(j) = 0.
	      end if
	end do
	call computd('DAZIM',dazim,nants,error)
	         if(error(1:2).ne.'OK') type *,error
	call computd('DELEV',delev,nants,error)
	         if(error(1:2).ne.'OK') type *,error

c  --	begin_point fills in itrack, and azim,elev if 'test'
c  --	 or 'az/el'
	CALL BEGIN_POINT(ANTFLAG,TRAKOPT,S,ERROR)

	if(options(1:4).eq.'wait') then
		call wait_ticks(100)
		call wait_for_ants(antflag,14,error)
	end if

99	TYPE *,ERROR
	END
