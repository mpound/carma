      subroutine plotpt

c Heavily modified 1OCT92 by DPW 
c
c  Routine to plot various data entries versus each other.
c  Uses PGPLOT routines.
c					        STM  11 Jul 1985

	include 'point.inc'

	real	azomin, azomax, elomin, elomax	!Y-axis plot limits
c	integer	istat, lib$spawn	!dummy for invoking lib$spawn( )

c	sameplots = ask('Use the same plot parameters as last time?'
c	1						,.true.,.true.,0)
	sameplots = .false.

c  Determine plot bounds. Autoscale.

	minyspan = 1.	!arcminutes
	call BOUNDS( npts, azo, wtaz, minyspan, 100., -100., 100.
	1				, azomin, azomax,' AZoffset',0 )
	call BOUNDS( npts, elo, wtel, minyspan, 100., -100., 100.
	1				, elomin, elomax,' ELoffset',0 )

20	call hdr( 'PLOT', 4 )

	azelplots = .true.
	timeplots = .true.

c  get plot device. nominally for plots to screec "soft"
	screen = ask('Send plots to screen?',screen,.not.sameplots,logu) 
        WRITE(*,*) "TEUBEN debug: ",screen
	azelplots = .false.
	if( screen ) then
           device = soft
c          azelplots = ask('Plot offsets vs. AZ and EL?',azelplots
c	1						,.not.sameplots,logu)
	   if( azelplots ) then
		call AZELPLT( device, azomin, azomax, elomin, elomax, .true.)
	   endif

c	timeplots = ask('Plot offsets vs. time?',timeplots,.not.sameplots,logu)
	   if( timeplots ) then 
		call TIMEPLT( device, azomin, azomax, elomin, elomax, .false. )
	   endif

	   call PGEND

c            pjt: added these to hardcopy too...
        else
           WRITE(*,*) "TEUBEN debug"
           device=hard
           call AZELPLT( device, azomin, azomax, elomin, elomax, .true.)
	   call PGEND
	endif	!end of screen plots

c  get plot device. nominally for plots to post script file "hard"
	hardc = ask('Make hardcopy of plots?',hardc,.not.sameplots,logu) 
c	device = textin('Send plots to',hard,.not.sameplots,logu) 
	if( hardc ) then
		device = hard
c pjt
                azelplots = .TRUE.
                timeplots = .FALSE.

c	azelplots = ask('Plot offsets vs. AZ and EL?',azelplots
c	1						,.not.sameplots,logu)
	   if( azelplots ) then
		call AZELPLT( device, azomin, azomax, elomin, elomax, .false. )
c send plots to printer
		call pgend
c		istat = lib$spawn('print point.ps')
	   endif

c	timeplots = ask('Plot offsets vs. time?',timeplots,.not.sameplots,logu)
	   if( timeplots ) then 
		call TIMEPLT( device, azomin, azomax, elomin, elomax, .false. )
c send plots to printer
		call pgend
c		istat = lib$spawn('print point.ps')
	   endif

	endif	!end of hardcopy plots

999	return
	end	


c------------------------------------------------
	subroutine AZELPLT( device, azomin, azomax, elomin, elomax, bigscreen )

c plot offsets vs. AZ and EL
c add plotting of wobble curve when appropriate

	include 'point.inc'

	logical	bigscreen	!=.true. if plots are to be put on terminal
	real	azomin, azomax, elomin, elomax	!Y-axis plot limits
	real	azmin, azmax, elmin, elmax	!X-axis plot limits
	character   xlbl*80, ylbl*80
	integer	ncurve	!number of data points for wobble curve
	real	xc(10000), yc(10000)	!wobble curve data
	real	dcurve	!step size for wobble curve
	real	xyzfunc, xyzfunc2	!external functions

	nxpage = 2

	if( bigscreen ) then
		nypage = 2
	else
		nypage = 3
	endif

c  Initialize plot.
	call pgbegin( 0, device, nxpage, nypage )

c  Draw plots.

c  AZO versus AZ plot.
	minxspan = 2.	!degrees
	call BOUNDS( npts, az, wtaz, minxspan, 45., -90., 360.
	1				, azmin, azmax,' AZ range',0 )
	call BOUNDS( npts, el, wtel, minxspan, 30., 0., 90.
	1				, elmin, elmax,' EL range',0 )
	call pgenv( azmin, azmax, azomin, azomax, 0, 0 )
	xlbl = 'Azimuth [degrees]'
       	ylbl = 'Azimuth offset [arcmin]'
	call pglabel( xlbl, ylbl, ' ' )
	call PGSCH( 1.8 )
	call PGTEXT(azmin,azomax+.02, toplbl)
	do i = 1, npts
		if( wtaz(i) .gt. 0. ) then
			call PGSCH( bigdot*wtaz(i) )
			call PGPOINT( 1, az(i), azo(i), 17 )
		endif
	enddo
	call PGSCH( 1. )

c add best fit wobble curve if wobble fit was done and the AZ, ZA is small
	if( wobble .and. (azmax-azmin) .le. 45. ) then
c determine the number of data points for the curve from the AZ range
		ncurve = 12 * nint( (azmax-azmin)*512./360.)
		dcurve = (azmax-azmin)/(ncurve-1)
		do i = 1, ncurve
			xc(i) = azmin + (i-1)*dcurve
			yc(i) = 0.
			do k = 1, 8
c use average el point
			   yc(i) = yc(i) + wobc(k)*xyzfunc( 8+k, xc(i),
	1						(elmin+elmax)/2. )
			enddo
		enddo
		call pgline( ncurve, xc, yc ) 
	endif	!end of wobble curve

c  AZO versus EL plot.

	call pgenv( elmin, elmax, azomin, azomax, 0, 0 )
	xlbl = 'Elevation [degrees]'
       	ylbl = 'Azimuth offset [arcmin]'
       	call pglabel( xlbl, ylbl, ' ' )
	do i = 1, npts
		if( wtaz(i) .gt. 0. ) then
			call PGSCH( bigdot*wtaz(i) )
			call PGPOINT( 1, el(i), azo(i), 17 )
		endif
	enddo
	call PGSCH( 1. )

c  ELO versus AZ plot.

	call pgenv( azmin, azmax, elomin, elomax, 0, 0 )
	xlbl = 'Azimuth [degrees]'
       	ylbl = 'Elevation offset [arcmin]'
       	call pglabel( xlbl, ylbl, ' ' )
	call PGSCH( 1.3 )
	call PGTEXT(azmin,elomax+.02, toplbl2)
	call PGSCH( 1. )
	do i = 1, npts
		if( wtel(i) .gt. 0. ) then
			call PGSCH( bigdot*wtel(i) )
			call PGPOINT( 1, az(i), elo(i), 17 )
		endif
	enddo
	call PGSCH( 1. )

c  ELO versus EL plot.

	call pgenv( elmin, elmax, elomin, elomax, 0, 0 )
	xlbl = 'Elevation [degrees]'
       	ylbl = 'Elevation offset [arcmin]'
       	call pglabel( xlbl, ylbl, ' ' )
	do i = 1, npts
		if( wtel(i) .gt. 0. ) then
			call PGSCH( bigdot*wtel(i) )
			call PGPOINT( 1, el(i), elo(i), 17 )
		endif
	enddo
	call PGSCH( 1. )

c add best fit wobble curve if wobble fit was done
	if( wobble .and. (elmax-elmin) .le. 45. ) then
c determine the number of data points for the curve from the AZ range
		ncurve = 12 * nint( (elmax-elmin)*512./360.)
		dcurve = (elmax-elmin)/(ncurve-1)
		do i = 1, ncurve
			xc(i) = elmin + (i-1)*dcurve
			yc(i) = 0.
			do k = 1, 8
			   yc(i) = yc(i) + wobc(k)*xyzfunc2( 8+k, xc(i), xc(i) )
			enddo
		enddo
		call pgline( ncurve, xc, yc ) 
	endif	!end of wobble curve

c now make hairball plot
	if( bigscreen ) then	!start new screen
		call PGEND
c  Initialize plot.
		call PGBEGIN( 0, device, 1, 1 )
		call pgenv( -1., 1., -1., 1., 1, -2 )
		call PGSCH(1.)
	else	!fit onto bottom 1/3 of azel page
c set up plot for lower 1/3 of the page
		call pgenv( -1., 1., -1., 1., 1, -2 )
		call PGSCH(1.)
	endif

	call HAIRBALL( device )

c	call PGIDEN
	return
	end


c-----------------------------------------------------------------------------
	subroutine TIMEPLT( device, azomin, azomax, elomin, elomax, bigscreen )

C Plot data vs. time and time of day

	include 'point.inc'

	real	x(2), y(2)	!line endpoints
	integer	ip		!index of last valid point
	logical	bigscreen	!=.true. if plots are to be put on terminal
	real	azomin, azomax, elomin, elomax	!Y-axis plot limits
	real	daymin, daymax	!X-axis time plot limits 
	character   xlbl*80, ylbl*80
	real	diurn(maxpts)	!time of day

	nxpage = 1
	if( bigscreen ) then
		nypage = 2
	else
		nypage = 4
	endif

c  Initialize plot.
	call pgend
	call pgbegin( 0, device, nxpage, nypage )

c  Azimuth offset vs. time plot.
	minxspan = .01	!14.4 minutes
	call BOUNDS( npts, mjd1, wtaz, minxspan, 100000., 0., 100000.
	1				, daymin, daymax,'Day range',0 )

c	call PGBBUF
	call pgenv( daymin, daymax, azomin, azomax, 0, 0 )
	write( xlbl,'('' Time Past '',a,'' 0 UT [days]'')'
	1						,iostat=ioer) ddmmmyy
	ylbl = 'Azimuth offset [arcmin]'
	call pglabel( xlbl, ylbl, ' ' )
	call PGSCH( 2.4 )
	call PGTEXT(daymin,azomax+.02, toplbl)
	call PGSCH( 1. )
c	call PGEBUF
c	call PGBBUF
	ip = 0
	do i = 1, npts
		if( wtaz(i) .gt. 0. ) then
			call PGSCH( bigdot*wtaz(i) )
			call PGPOINT( 1, mjd1(i), azo(i), 17 )
			if( ip .gt. 0 ) then	!draw connecting line
				x(1) = mjd1(ip)
				y(1) = azo(ip)
				x(2) = mjd1(i)
				y(2) = azo(i)
				call PGLINE( 2, x, y)
			endif
			ip = i
		endif
	enddo
	call PGSCH( 1. )

c  Elevation offset vs. time plot.

c	call PGBBUF
	call pgenv( daymin, daymax, elomin, elomax, 0, 0 )
	ylbl = 'Elevation offset [arcmin]'
	call pglabel( xlbl, ylbl, ' ' )
c	call PGEBUF
c	call PGBBUF
	ip = 0
	do i = 1, npts
		if( wtel(i) .gt. 0. ) then
			call PGSCH( bigdot*wtel(i) )
			call PGPOINT( 1, mjd1(i), elo(i), 17 )
			if( ip .gt. 0 ) then	!draw connecting line
				x(1) = mjd1(ip)
				y(1) = elo(ip)
				x(2) = mjd1(i)
				y(2) = elo(i)
				call PGLINE( 2, x, y)
			endif
			ip = i
		endif
	enddo
	call PGSCH( 1. )

c	return
c	end

c get MJD time modulus 1
	do i = 1, npts
		diurn(i) = mod( mjd1(i), 1. )
	enddo

c  Azimuth offset vs. time of day
	call pgenv( 0., 1., azomin, azomax, 0, 0 )
	write( xlbl,'('' Time of Day [days]'')',iostat=ioer)
	ylbl = 'Azimuth offset [arcmin]'
	call pglabel( xlbl, ylbl, ' ' )
	call PGSCH( 1.6 )
	call PGTEXT(0.,azomax+.02, toplbl2)
	call PGSCH( 1. )
	do i = 1, npts
		if( wtaz(i) .gt. 0. ) then
			call PGSCH( bigdot*wtaz(i) )
			call PGPOINT( 1, diurn(i), azo(i), 17 )
		endif
	enddo
	call PGSCH( 1. )

c  Elevation offset vs. time plot.

	call pgenv( 0., 1., elomin, elomax, 0, 0 )
	ylbl = 'Elevation offset [arcmin]'
	call pglabel( xlbl, ylbl, ' ' )
	do i = 1, npts
		if( wtaz(i) .gt. 0. ) then
			call PGSCH( bigdot*wtaz(i) )
			call PGPOINT( 1, diurn(i), elo(i), 17 )
		endif
	enddo

	return
	end



c------------------------------------------------------------------------------
	subroutine HAIRBALL( device )

c Vector plot of pointing errors with the hemisphere mapped to a disk

	include 'point.inc'

	real	x(2), y(2)	!x,y coord. of az,el
c	real	x2(maxpts), y2(maxpts)	!x,y coord. of az+azo,el+elo
	real	x1, y1	!center of circle to be drawn
	real	vlen	!radius of circle
	external	fx, fy	!functions to generate circles

	common   /circle/ x1, y1, vlen

c10	gain = value('gain for converting errors to vectors'
c	1					,gain,.not.sameplots,logu)
10	gain = value('gain for converting errors to vectors'
	1					,1.,.false.,logu)

C	call PGEND
c	call PGBEGIN( 0, device, 1, 1 )
c	call pgask(.false.)

c  Initialize plot.
c	call PGSCH(1.)
c	call pgenv( -1., 1., -1., 1., 1, -2 )
c set up plot for lower half of the page
c	call PGVPORT(.1,1.1,.1,.4)
c	call PGWNAD(-1.,1.,-1.,1.)
c put in compass headings
	call PGMTEXT('LV',1.,.5,.5,'E')
	call PGMTEXT('RV',1.,.5,.5,'W')
	call PGMTEXT('T',1.,.5,.5,'N')
	call PGMTEXT('B',1.,.5,.5,'S')      

c draw border circle
	x1 = 0.
	y1 = 0.
	vlen = 1.
	call pgfunt( fx, fy, 360, 0.0, 360.0, 1 )

c draw 1. arcmin diameter error circle
	x1 = .8
	y1 = -.8
	vlen = .1
	write(label,'(1x,f4.2,'' arcmin radius.'')',iostat=ioer) vlen/gain
	call pgfunt( fx, fy, 360, 0.0, 360.0, 1 )
	call PGSCH(.8)
	call PGTEXT(.5,-1., label )
	call PGTEXT(.5,1., 'Error Vector Plot')
c	call PGTEXT(-1.,1.05, toplbl)
c	call PGTEXT(-1.,1.01, toplbl2)

c convert az,el to x,y and azo,elo to dx,dy and plot point and error line
	do i = 1, npts
	   	call AZELTOXY(az(i),el(i),azo(i),elo(i),gain,x(1),y(1),x(2),y(2))
		if( wtaz(i) .gt. 0. .or. wtel(i) .gt. 0. ) then
			call PGSCH( bigdot*(wtel(i)+wtaz(i))/2. )
			call PGPOINT( 1, x(1), y(1), 17 )
		   	call PGLINE( 2, x, y)
		endif
	enddo
	call PGSCH(1.)

c	yes = ask('Try different error gain?',.false.,.not.sameplots,logu)
c	if( yes ) goto 10

	return
	end



c------------------------------------------------------------------------------
	subroutine AZELTOXY(az,el,azo,elo,gain,x1,y1,x2,y2)

c this routine does the coordinate transformation from az,el to x,y
c plus the end of the error vector

	implicit none

c--------------INPUT---------------------
	real	az, el	!altaz coordinates [degrees], az=0 is up => north
			!with increasing az going counter clockwise
	real	azo, elo	!errors [arcmin]
	real	gain	!gain to apply to errors for plotting

c--------------OUTPUT---------------------
	real	x1, y1	!cartesian coordinates, inside unit circle
			!North=0,1 East=-1,0
	real	x2, y2	!end of error vector

	x1 = -sind(az) * (1. - el/90.)
	y1 =  cosd(az) * (1. - el/90.)
c	write(*,'('' az,el'',2f8.2,'' => x,y '',2f8.2)') az,el,x,y

	x2 = x1 + gain * (  elo*sind(az) -azo*cosd(az) )
	y2 = y1 + gain * ( -elo*cosd(az) -azo*sind(az) )

	return
	end


c-----------------------------------------------------------------------------
      function FX(t)
c function to be used by PGFUNT to draw circles or ellipses
c	real	fx
      common   /circle/ x1, y1, vlen
      fx = x1 + vlen* cosd( t )
      return
      end


c-----------------------------------------------------------------------------
      function FY(t)
c function to be used by PGFUNT to draw circles or ellipses
c	real	fy
      common   /circle/ x1, y1, vlen
      fy = y1 + vlen* sind( t )
      return
      end
