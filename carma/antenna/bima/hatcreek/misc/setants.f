c*
	PROGRAM SETANTS
c& wh
c= SETANTS - Set the antennas to be used.
c: observing
c+
c	SETANTS fills in the antennas in use word in 
c	the common block (antinuse). Antennas can also
c	be turned off by CONTROL if they stop moving.
c	This is to guard against error conditions such
c	as collisions or antennas on hardware pointing
c	limits. (Ant 3 azimuth also stalls randomly).
c	SETANTS can be used to turn an antenna back
c	on after CONTROL has turned it off; if the
c	antenna is on a limit, be sure to give it an
c	ANTS command to drive off the limit, or the
c	antenna will be turned off again.
c
c	WARNING: when antennas are turned off, CONTROL
c	does not read in their positions; be sure 
c	that closely spaced antennas are stowed in a
c	safe position before turning them OFF.
c
c@ ants
c	Antenna selection flag:
c	 * means use all nants antennas
c	 xyz means use antennas x y and z
c	 *-xyz means use all antennas except x y and z
c
c@ to
c	ON or OFF
c--
c  sep 92 wh	
c  aug 93 wh   add exit status
c---------------------------------------------------------------------c
	character*(*) version
	parameter(version='SETANTS (version 1.1 27-aug-93)')
	include '../inc/constants.inc'
	character*80 error
	integer ants,ant,antinuse
	logical select
	double precision azim,elev,encoder
	integer resoff(2,nants),itrack(nants),slewflag
	real axiserr(2,nants)
	character*4 option,state
	character*3 possible(2) /'ON ','OFF'/
	character*40 explanation(2) /
	1 'Point these antennas',
	4 'Ignore these antennas'/
	integer slewmask(nants) /3,'c'x,'30'x,'c0'x,'300'x,'c00'x,'3000'x,
	1 'c000'x,'30000'x,'c0000'x,'300000'x,'c00000'x/


 	call output(version)
	call mbusopen(error)
	 if(error(1:2).ne.'OK') goto 99

	call comgeti('ANTINUSE',antinuse,1,error)
	  if(error(1:2).ne.'OK') goto 99
	call comgeti('RESOFF',resoff,2*nants,error)
	  if(error(1:2).ne.'OK') goto 99
	call comgeti('ITRACK',itrack,nants,error)
	  if(error(1:2).ne.'OK') goto 99
	call comgeti('SLEWFLAG',slewflag,1,error)
	  if(error(1:2).ne.'OK') goto 99
	call comgetr('AXISERR',axiserr,2*nants,error)
	  if(error(1:2).ne.'OK') goto 99

	call keyini
	call keyants(ants,0,error)
	call keya('to',option,' ')
	call ucase(option)
	call keyfin

	type *,'Summary of Antenna disposition:'
	type *,' (Close spaced antennas must be pointed in a '
	type *,'  safe direction before being turned off)'
	mask = 1
	do ant=1,nants
	  mask = mask*2
	  if(select(ant,ants)) then
		if(option(1:2).eq.'ON') antinuse = antinuse .or. mask
		if(option(1:3).eq.'OFF') antinuse = antinuse .and.(.not.mask)
	  end if
	  azim = encoder(ant,0,resoff(1,ant),error)
	  elev = encoder(ant,1,resoff(2,ant),error)
	  if((antinuse.and.mask) .gt. 0) state = 'ON'
	  if((antinuse.and.mask) .eq. 0) then
		state = 'OFF'
		itrack(ant) = 0
		axiserr(1,ant) = 0.
		axiserr(2,ant) = 0.
		slewflag = slewflag .and. (.not.slewmask(ant))
	  end if
	  
	  type *,'Ant ',ant,' is turned ',state,' and points to ',
	1				int(azim),int(elev)
	end do
	call computi('ANTINUSE',antinuse,1,error)
	call computi('ITRACK',itrack,nants,error)
	call computi('SLEWFLAG',slewflag,1,error)
	call computr('AXISERR',axiserr,2*nants,error)

99	if (error(1:2).ne.'OK') call bug('f',error)
	end
