c*
	PROGRAM RESOFF
c& wh
c= RESOFF - measure the resolver offset
c: misc
c+
c	RESOFF measures the offsets for the resolvers of the
c	specified antennas and puts the results into the common
c	block.  The antennas have to be at a known position which
c	is input to the program.
c
c@ ants
c	Antenna flag specifies which antennas to measure.
c	 * means use all nants antennas
c	 xyz means use antennas x y and z
c	 *-xyz means use all antennas except x y and z
c
c@ degrees
c	The Azimuth and Elevation (deg) for the specified antenna[s].
c--
c	may 91 wh
c	may 92 wh resoff.ascii
c	jul 93 wh new version of keyants
c	jul 95 wh 12 ant version
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='RESOFF(version 1.2 6-jul-95)')
	include '../inc/constants.inc'
	integer degrees(2),ants,select,resoff(2,12),fres(2,12),encoderoff
	character*80 error,rlabel
	character*50 rfile

	type *,version
	call mbusopen(error)
	 if(error(1:2).ne.'OK') type *,error
	call keyini
	call keyants(ants,1,error)
	if(error(1:2).ne.'OK') type *,error
	call keyi('degrees',degrees(1),0)
	call keyi('degrees',degrees(2),0)
	call keyfin
	if((mod(degrees(1),2).ne.0) .or. (mod(degrees(2),2).ne.0)) then
		type *,'DEGREES must be even, try again'
		stop
	end if
	call comgeti('RESOFF',resoff,24,error)
	 if(error(1:2).ne.'OK') type *,error

	  call filepath('HATTAB','resoff','ascii',rfile)
	  open(1,file=rfile(:len1(rfile)),status='unknown')
	  read(1,'(a)') rlabel
1	  read(1,*,end=2) i,fres(1,i),fres(2,i)
	  goto 1
2	  continue

	do i=1,nants
	  if(select(i,ants) .and. error(1:2).eq.'OK') then
	    resoff(1,i) = encoderoff(i,0,degrees(1),error)
	    fres(1,i) = resoff(1,i)
	    if (error(1:2).ne.'OK') type *,error

	    resoff(2,i) = encoderoff(i,1,degrees(2),error)
	    fres(2,i) = resoff(2,i)
	    if (error(1:2).ne.'OK') type *,error
	  else if((fres(1,i).eq.0.) .and. (fres(2,i).eq.0.)) then
	     fres(1,i) = resoff(1,i)
	     fres(2,i) = resoff(2,i)
	  end if
	end do

	if(error(1:2).eq.'OK') then
	  type *,'Updating common block'
	  call computi('RESOFF',resoff,24,error)
	  if(error(1:2).ne.'OK') type *,error
	  type *, 'Current Offset values are:'
	  type 999,'azim: ',(resoff(1,i),i=1,10)
	  type 999,'elev: ',(resoff(2,i),i=1,10)
999	  format(a,10i7)
	type *,'Updating file resoff.ascii'
	write(*,'(a,a)') 'label:',rlabel
	rewind 1
	write(1,'(a)') rlabel
	do i=1,nants
	    write(1,'(i4,2i10)') i,fres(1,i),fres(2,i)
	end do
	close(1)
	type *,'WARNING: you must kill/rerun CONTROL'
	end if
	end
