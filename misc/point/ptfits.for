      subroutine fitcon( ant, rxn )

c Major rewrite started 24SEP92 by DPW

c This routine deals with one telescope and receiver at a time.
c It can handle a data series in which the pointing constants used during
c the observations are changes. The pointing constants for each data point
c are recorded in the input data file and kept with the data.

c First:
c An incremental (old constants not removed) fit is done for a variety of
c fitting term combinations so you can get a feel for the significance of
c the various terms.

c Second:
c You can edit out bad data points
c one at a time. This can be repeated as many times as you like. The
c removed data points are restored if you process this antenna again.

c Third:
c The actual terms to fit are selected, either the defaults or same as 
c last time or a new set. Once selected, the old pointing constants used
c for the data for the selected terms are removed. Unselected terms are not
c removed. Thus only the coefficients for the fitted terms are written and
c will need to entered into the telescopes.

c Fourth:
c The best fit is subtracted from the data to give you a final working set for
c plotting, etc. The best fit coefficients can be written to the
c PNT_CONST.ddmmmyy file. This knows about RADIO vs. OPTICAL data.

c Fifth
c A wobble fit is performed but is not subtracted.

c  The basic fitting routine is
c  FITXYZZ which is modified from my generic routine
c  enguser:[dpw.other.for.util]FITXYZ

	include 'point.inc'

	double precision mjdtime !default MJD time
	character*7	coef(8)	!text array containing constants or blankd
	real	cown(8)		!fitting constants entered from keyboard
	integer	cmap(8)		!fit to all constants list
	integer	ichngcons	!number of times pointing constants changed
	character*26	quest	!text for question
	logical	removed(maxterms)	!=.true. if old term already removed
	integer	numfit	!number of unremoved points being fit
	real	azot, elot	!temporary data offset
	real	sigmas	!deviation of worst point
	real	azormsinit, elormsinit	!initial RMS's
	integer	icount	!number of non-zero weighted points

c  Header screen.

      call hdr( 'FITTING' , 7 )

c30	write(*,'(//,'' Fitting pointing data for antenna #'',i2,/)'
c     +		, iostat=ioer) ant
c	write(logu,'(//,'' Fitting pointing data for antenna #'',i2,/)'
c     +		, iostat=ioer) ant

c initialize remov array
	do i = 1, icant(ant, rxn)
		remov(i) = .false.
	enddo

c load data without removing pointing constants
c set error threshold
	ploterr = value('data error threshold',.25,.false.,logu)
100	call LOADFIT( ant, rxn, .FALSE., remov, ichngcons, numfit )

c-----------------------------
c go through a preset list of terms to fit to see what combinations of
c constants give significant improvements

200	write(*,'(//,'' Results for incremental fit to '',i4,'' data points'')'
	1		,iostat=ioer) numfit
	if( logu .gt. 0 ) write(logu,'(//
	1	,'' Results for incremental fit to '',i4,'' data points'')'
	2		,iostat=ioer) numfit

	call CONSHEAD(radio, logu, ichngcons)	!print header for constants

c get initial RMS's
	azormsinit = WTRMS(npts, azo, wtaz, icount)
	elormsinit = WTRMS(npts, elo, wtel, icount)
c write out line of blank constants and RMS's
	call WRITECONS(0,imap,c,azormsinit,elormsinit,logu, ' Before')

c	if( radio ) then	!then just work on X1, X2 and X3
	if( radio ) then	!change to work on all of the constants
c	   do k = 1, 2
c set up nvar and imap from master table
c		nvar = raster(0,k)
c		do i = 1, nvar
c			imap(i) = raster(i,k)
c		enddo
	   do k = 1, 9
c set up nvar and imap from master table
		nvar = master(0,k)
		do i = 1, nvar
			imap(i) = master(i,k)
		enddo

c  Solve for weighted best fit
		call fitxyzz( npts,az,el,azo,elo,wtaz,wtel, nvar, imap
	1	, c,rmsbeforeaz,rmsbeforeel,rmsafteraz,rmsafterel, .false.
	2	, 0, iworstaz, iworstel, worstaz, worstel )	!no output

c write results
		call WRITECONS(nvar,imap,c,rmsafteraz,rmsafterel,logu
	1					,' Delta ')
	
	   enddo

	else	!work on all constants for optical dat
	   do k = 1, 9
c set up nvar and imap from master table
		nvar = master(0,k)
		do i = 1, nvar
			imap(i) = master(i,k)
		enddo

c  Solve for weighted best fit
		call fitxyzz( npts,az,el,azo,elo,wtaz,wtel, nvar, imap
	1	, c,rmsbeforeaz,rmsbeforeel,rmsafteraz,rmsafterel, .false.
	2	, 0, iworstaz, iworstel, worstaz, worstel )	!no output

c write results
		call WRITECONS(nvar,imap,c,rmsafteraz,rmsafterel,logu
	1					,' Delta ')
	
	   enddo

	endif
c----------------------------------------
c try different radio error threshold
c	if( radio ) then
c	   yes = ask('Change data error threshold?',.false.,.true.,logu)
c	   if( yes ) then
c		ploterr = value('data error threshold',ploterr,.true.,logu)
c		goto 100
c	   endif
c	endif

c----------------------------------------

c---------------------------------------------
c filter data
	yes = ask('Remove "bad" data?',.false.,.true.,logu)
	if( yes ) then
		do i = 1, nvar
			imap(i) = master(i,9)
		enddo

221		threshold = value('selection threshold [sigma]',3.5,.true.,logu)

		write(*,'(/,'' REMOVED DATA POINTS'',/
	1	''  +days    AZ     EL     AZO    ELO  AZwt ELwt   date  '')')
		if( logu .gt. 0 ) then
		write(logu,'(/,'' REMOVED DATA POINTS'',/
	1	''  +days    AZ     EL     AZO    ELO  AZwt ELwt   date  '')')
		endif

c restore previously removed data
		do i = 1, npts
			remov(i) = .false.
		enddo
		call LOADFIT( ant, rxn, .FALSE., remov, ichngcons, numfit )

c  Solve for weighted best fit
222		call fitxyzz( npts,az,el,azo,elo,wtaz,wtel, nvar, imap
	1	, c,rmsbeforeaz,rmsbeforeel,rmsafteraz,rmsafterel, .false.
	2	, 0, iworstaz, iworstel, worstaz, worstel )	!no output

c look at worst point from last fit to all terms and see if it is to be removed
	   if( worstaz .gt. worstel ) then
		sigmas = worstaz/rmsafteraz        
		i = iworstaz
	   else
		sigmas = worstel/rmsafterel
		i = iworstel
	   endif

	   if( sigmas .gt. threshold ) then   
		mjdtime = dble( mjd1(i) )
		length = mjd_to_abstime_fortran(time1,mjdtime,3)   !3=>.001sec
	   	write(*,'(   f8.4,  f7.2,  f7.2,  f7.3,  f7.3,f5.2,f5.2,2x,a20)'
	1	,iostat=ioer)
	2	  mjd1(i), az(i), el(i),azo(i),elo(i),wtaz(i),wtel(i),time1
	if( logu .gt. 0 )   
	1	write(logu,'(f8.4,  f7.2,  f7.2,  f7.3,  f7.3,f5.2,f5.2,2x,a20)'
	1	,iostat=ioer)
	2	  mjd1(i), az(i), el(i),azo(i),elo(i),wtaz(i),wtel(i),time1

		wtaz(i) = 0.
		wtel(i) = 0.
		numfit = numfit - 1
		remov( loadmap(i) ) = .true.	!removed = .true.
		goto 222
	   else	!write out current fit rms and ask for repeat
		call WRITECONS(nvar,imap,c,rmsafteraz,rmsafterel,logu
	1					,' Result')
		yes = ask('Try different threshold?',.false.,.true.,logu)
		if( yes ) goto 221
	   endif

	endif	!for removing bad points

c	if( sigmas .gt. threshold ) then   !see if it is to be removed from fit
c temporarily subtract fit from worst point
c		azot = azo(i)
c		elot = elo(i)
c		call subpnt(az(i),el(i),azot,elot,nvar,imap,c)
c		write(*,'(/,'' The worst point is '',f5.1,'' sigma'')'
c	1			,iostat=ioer) sigmas
c		if( logu .gt. 0 )
c	1	write(logu,'(/,'' The worst point is '',f5.1,'' sigma'')'
c	2			,iostat=ioer) sigmas
c	write(*,'(
c	1	''  +days    AZ     EL     AZO    ELO  AZwt ELwt   date  '')')
c	if( logu .gt. 0 )	write(logu,'(
c	1	''  +days    AZ     EL     AZO    ELO  AZwt ELwt   date  '')')
c	   write(*,'(   f8.4,  f7.2,  f7.2,  f7.3,  f7.3,f5.2,f5.2,2x,     a7)'
c	1	,iostat=ioer)
c	2	  mjd1(i), az(i), el(i),azot,elot,wtaz(i),wtel(i),ddmmmyy
c	if( logu .gt. 0 )   
c	1	write(logu,'(f8.4,  f7.2,  f7.2,  f7.3,  f7.3,f5.2,f5.2,2x, a7)'
c	1	,iostat=ioer)
c	2	  mjd1(i), az(i), el(i),azot,elot,wtaz(i),wtel(i),ddmmmyy
c
c		yes = ask('Remove this point from fit?',.false.,.true.,logu)
c		if( yes ) then	!set weight to zero and start over
c			wtaz(i) = 0.
c			wtel(i) = 0.
c			numfit = numfit - 1
c			remov( loadmap(i) ) = .true.	!removed = .true.
c			goto 200
c		endif

c	endif

c done editing out bad data	

c------------------------------

c now do the desired specific fit
	write(*,'(//,'' Select constants to vary - '')')
	write(logu,'(//,'' Select constants to vary - '')')

c  Choose variables to vary.

	sameterms = ask('Fit to same terms as last time?'
     &					,sameterms,.true.,12)

235	nvar = 0

        do i=1, maxterms

		if( .not.sameterms ) then
		   write(quest,'('' Fit '',a20,''?'')',iostat=ioer) funclabel(i)
		   if( i .eq. 2 ) then	!don't fit elevation offset E2, since
					!it is redundant with X2(i=7) EL collim.
			term(i) = .false.
			term(i) = ask(quest,term(i),.false.,0)	!no log writing
		   else
			term(i) = ask(quest,term(i),.not.sameterms,0)
		   endif
		endif

		if( term(i) ) then
			if( i .le. 5 .and. radio ) then
			   write(*,'('' ------------- WARNING fitting Es''
	1			,'' to RADIO data--------------------'')')
			   if( logu .gt. 0 )
	1		   write(logu,'('' ------------- WARNING fitting Es''
	2			,'' to RADIO data--------------------'')')
			endif
			nvar = nvar + 1
	  		imap( nvar ) = i
		endif

	enddo
	if( .not.sameterms ) then
		yes = ask('Fit the above terms?',.true.,.true.,logu)
		if( .not.yes ) goto 235
	endif

	if ( nvar .eq. 0 ) then

		write(*,'(/,'' ------No terms to fit to.-------------'',/)')
		if( logu .gt. 0 ) write(logu,'(/,'' No terms to fit to.'',/)')

c------------------------------
c subtract a user input set of constants if wanted
		yes = ask('Subtract your own set of constants?'
	1						,.false.,.true.,logu)
		if( yes ) then

c set up nvar and imap from last entry in master table, i.e. 7 terms
			nvar = master(0,9)
			do i = 1, nvar
				imap(i) = master(i,9)
c defualt to last set of fitted constants
				cown(imap(i)) = c(i)
			enddo
c fill in E2 as zero
			cown(2) = 0.

c get new constants
			nvar = 8
			do k = 1, nvar
			    cown(k) = value(funclabel(k),cown(k),.true.,logu)
			    cmap(k) = k
			enddo

	call SUBFUNC( npts,az,el,azo,elo,wtaz,wtel, nvar,cmap
	1	, cown,rmsbeforeaz,rmsbeforeel,rmsafteraz,rmsafterel,.false.,0 )
	call CONSHEAD(radio, logu, ichngcons)	!print header for constants
	call WRITECONS(0,cmap,c,azormsinit,elormsinit,logu, ' Before')
	call WRITECONS(nvar,cmap,cown,rmsafteraz,rmsafterel,logu, ' After')

		endif

		go to 999
	endif

c list the terms being fit
 
c500	write(*,'(/)')
c	write(logu,'(/)')
c	write( *, '('' Fitting #'',i2,''; '',a20)', iostat=ioer)
c     +			( (imap( k ), funclabel(imap(k)) ), k=1, nvar )
c	write(logu, '('' Fitting #'',i2,''; '',a20)', iostat=ioer)
c     +			( (imap( k ), funclabel(imap(k)) ), k=1, nvar )

c  Ready to go.
c Null out constants

c determine if old values for the constants being fitted are to be removed 
c from the data
c nvar and imap contain the info about what is being fitted, and thus about
c which constants to remove

	depoint = ask('Remove the old constants from the data?'
     +						,.true.,.false.,logu)

c load data
c	   do i = 1, 8
c		removed(i) = .false.
c		c(i) = 0.
c	   enddo

	call LOADFIT( ant, rxn, depoint, remov, ichngcons, numfit )

	write(*,'(//,'' New pointing constants determined by fit to ''
	1	,i4,'' data points'')',iostat=ioer) numfit
	if( logu .gt. 0 )
	1    write(logu,'(//,'' New pointing constants determined by fit to ''
	1	,i4,'' data points'')',iostat=ioer) numfit
                                
	call CONSHEAD(radio, logu, ichngcons)	!print header for constants

c use last set of pointing constants for first line of results, i.e. no fit
	do k = 1, 8
		cmap(k) = k
	enddo
	call WRITECONS(8,cmap,cold,azormsinit,elormsinit,logu, ' Old   ')

c  Solve for weighted best fit
	call fitxyzz( npts,az,el,azo,elo,wtaz,wtel, nvar, imap
	1	, c,rmsbeforeaz,rmsbeforeel,rmsafteraz,rmsafterel, .false.
	2	, 0, iworstaz, iworstel, worstaz, worstel )	!no output

c write results
c	call WRITECONS(nvar,imap,c,rmsafteraz,rmsafterel,logu,' New ', toplbl2)
	call WRITECONS(nvar,imap,c,rmsafteraz,rmsafterel,logu,' New   ')

c write 2nd title line for plots
c fill with blanks
	do i = 1, 8
		coef(i) = '       '
	enddo

c fill fitted constants
	do i = 1, nvar
		write(coef(imap(i)),'(f7.3)',iostat=ioer ) c(i)
	enddo

c write output
	write(toplbl2,'(1x,''New'',	 '' E1='',a7
	2				'', E2='',a7
	3				'', E3='',a7
	4				'', E4='',a7
	5				'', E5='',a7
	6				'', X1='',a7
	7				'', X2='',a7
	8				'', X3='',a7
	9	'' =>'',f6.3,1x,f6.3,'' RMS'')',iostat=ioer) 
	1	(coef(i), i=1,8), rmsafteraz, rmsafterel
	
c try different constants
	write(*,'(/)')
	write(logu,'(/)')
c	yes = ask('Try different set of fitting terms?'
c	1					,.false.,.not.sameterms,logu)
c	if( yes ) then
c		sameterms = .false.
c		goto 235
c	endif

c check to see if best fit is to be removed from the data
	subfit = ask('Subtract best fit from the data?'
	1					,.true.,.false.,logu)

	if( subfit ) call SUBFUNC( npts,az,el,azo,elo,wtaz,wtel, nvar,imap
	1	, c,rmsbeforeaz,rmsbeforeel,rmsafteraz,rmsafterel,.false.,0 )
	
c write to constants file
	if( depoint ) then	!old constants have been removed, write new ones
	   write(label,'(''Write coefficients to '',a20)',iostat=ioer) constfile
	   yes = ask(label,.false.,.true.,logu)
	   if( yes ) then
		do i = 1, nvar
		   k = imap(i)

		   if( k .le. 5 ) then
			if( radio ) then
			   write(*,'('' ------------- WARNING fitting Es''
	1			,'' to RADIO data--------------------'')')
			   write(logu,'('' ------------- WARNING fitting Es''
	1			,'' to RADIO data--------------------'')')
			endif
			write(*,'('' PNT_CONST'',i1,'' E'',i1,''='',f7.3)'
	1					,iostat=ioer ) ant, k, c(i)
			if( logu .gt. 0 )
	1		   write(logu,'('' PNT_CONST'',i1,'' E'',i1,''='',f7.3)'
	2					,iostat=ioer ) ant, k, c(i)
			write(constu,'('' PNT_CONST'',i1,'' E'',i1,''='',f7.3)'
	2					,iostat=ioer ) ant, k, c(i)
		   endif

		   if( k .gt. 5 .and. k .le. 8 .and. radio ) then
			write(*,'('' PNT_CONST'',i1,'' R'',i1,''='',f7.3)'
	1					,iostat=ioer ) ant, (k-5), c(i)
			if( logu .gt. 0 )
	1		   write(logu,'('' PNT_CONST'',i1,'' R'',i1,''='',f7.3)'
	2					,iostat=ioer ) ant, (k-5), c(i)
			write(constu,'('' PNT_CONST'',i1,'' R'',i1,''='',f7.3)'
	2					,iostat=ioer ) ant, (k-5), c(i)
		   endif

		   if( k .gt. 5 .and. k .le. 8 .and. .not.radio ) then
			write(*,'('' PNT_CONST'',i1,'' O'',i1,''='',f7.3)'
	1					,iostat=ioer ) ant, (k-5), c(i)
			if( logu .gt. 0 )
	1		   write(logu,'('' PNT_CONST'',i1,'' O'',i1,''='',f7.3)'
	2					,iostat=ioer ) ant, (k-5), c(i)
			write(constu,'('' PNT_CONST'',i1,'' O'',i1,''='',f7.3)'
	2					,iostat=ioer ) ant, (k-5), c(i)
		   endif
			
		enddo

	   endif
	endif

c----------------------------------------------------------------------------
c determine if wobble fit should be done
c	if( .not.radio .and. numfit .gt. (nvar+4) ) then
	if( numfit .gt. (nvar+4) ) then	!change to always fit for wobble
		wobble = .true. 
	else
		wobble = .false.
	endif

	wobble = ask('Fit data to wobble terms?',wobble,.false.,logu)
	if( wobble ) then	!fit 256 and 512 theta terms
		do i = 1, 8
			imap(i) = 8 + i                
		enddo
		nvar = 8

c  Solve for weighted best fit
		call fitxyzz( npts,az,el,azo,elo,wtaz,wtel, nvar, imap
	1	, wobc,rmsbeforeaz,rmsbeforeel,rmsafteraz,rmsafterel, .false.
	2	, 0, iworstaz, iworstel, worstaz, worstel )	!no output

c write output header
		write(*,'(/,8x,'' C256AZ S256AZ C256EL S256EL C512AZ ''
	1	,''S512AZ C512EL S512EL    AZrms  ELrms '')',iostat=ioer)
		if( logu .gt. 0 )
	1      	   write(logu,'(/,8x,'' C256AZ S256AZ C256EL S256EL C512AZ ''
	2	,''S512AZ C512EL S512EL    AZrms  ELrms '')',iostat=ioer)

c write results after resetting imap parameters to put coef. in correct places
		do i = 1, 8                            
			imap(i) = i                
		enddo
		call WRITECONS(nvar,imap,wobc,rmsafteraz,rmsafterel,logu
	1							,' Wob ')

c analyze wobble fit
		write(*,'(/,'' Azimuth WOBBLE analysis'')')
		if( logu .gt.0 ) write(logu,'(/,'' Azimuth WOBBLE analysis'')')
		call WOBBLED( wobc(1), wobc(2), wobc(5), wobc(6), logu )

		write(*,'(/,'' Elevation WOBBLE analysis'')')
		if( logu .gt.0 ) write(logu,'(/,'' Elevation WOBBLE analysis'')')
		call WOBBLED( wobc(3), wobc(4), wobc(7), wobc(8), logu )

	endif	!fit wobble
                                  
999   return
      end


c-----------------------------------------------------------------------------
	subroutine WEIGHT(ir,azwt,elwt)

c Calculate the weighting to use for the data

	include 'point.inc'

	azwt = 1.
	elwt = 1.

c the data ir.azer and ir.eler are the estimated error for the data in arcmin
c these are converted to weights for use in fitting radio data
c 1 is the highest weight and corresponds to .001 error

c change weighting limit from .001 to .02
c	if( ir.azer .ge. .001 ) azwt = .001/ir.azer
c	if( ir.eler .ge. .001 ) elwt = .001/ir.eler
	if( ir.azer .ge. .02 ) azwt = .02/ir.azer
	if( ir.eler .ge. .02 ) elwt = .02/ir.eler
	if( ir.azer .gt. ploterr ) azwt = 0.
	if( ir.eler .gt. ploterr ) elwt = 0.

	return
	end


c-----------------------------------------------------------------------------
	subroutine CONSHEAD(radio, logu, ichngcons)

c routine to printer header for points constants

	implicit none

c----------------INPUT-------------------
	integer	logu	!unit number for LOG file
	logical	radio	!=.true. if these are radio pointing constants
	integer	ichngcons	!number of times the pointing constants
				!were changed during data collection

c----------------OUTPUT-----------------
c	write to screen and LOG file

c---------------WORKNING VARIABLES---------
	integer	ioer	!I/O status
	

	if( ichngcons .gt. 0 ) then	!write warning
		write(*,'('' ---------------WARNING----------------''
	1	,/,'' The pointing constants changed'',i3
	2	,'' times while this data was obtained.'')'
	3	,iostat=ioer) ichngcons
		if( logu .gt. 0 )
	1		write(logu,'('' ---------------WARNING----------------''
	1		,/,'' The pointing constants changed'',i3
	2		,'' times while collecting this data.'')'
	3		,iostat=ioer) ichngcons
	endif

	if( radio ) then

       	   write(*,'(/,8x,''    E1     E2     E3     E4     E5     ''
	2	,''R1     R2     R3     AZrms  ELrms '')',iostat=ioer)
	   if( logu .gt. 0 )
	1	write(logu,'(/,8x,''    E1     E2     E3     E4     E5     ''
	2	,''R1     R2     R3     AZrms  ELrms '')',iostat=ioer)

	else	!optical

       	   write(*,'(/,8x,''    E1     E2     E3     E4     E5     ''
	2	,''O1     O2     O3     AZrms  ELrms '')',iostat=ioer)
       	   if( logu .gt. 0 )
	1	 write(logu,'(/,8x,''    E1     E2     E3     E4     E5     ''
	2	,''O1     O2     O3     AZrms  ELrms '')',iostat=ioer)

	endif

	return
	end


c-------------------------------------------------------------------------
	subroutine WRITECONS(nvar,imap,c,azrms,elrms,logu, lab)

c write out fitted constants and RMS's in correct position on line

	implicit none

c--------------------INPUT------------------------------------
	integer	nvar	!number terms being fit
	integer	imap(*)	!map of which constants to fit
	real	c(*)	!fitted constants
	real	azrms, elrms	!RMS's
	integer	logu	!unit number for LOG file
	character*(*)	lab	!line label

c-------------------OUTPUT------------------------------------
C write line of constants and RMS's to screen and LOG file

c--------------WORKING VARIABLES-----------------------------
	character*7	coef(8)	!text array containing constants or blankd
	integer	i	!index
	integer	ioer	!I/O status

c fill with blanks
	do i = 1, 8
		coef(i) = '       '
	enddo

c fill fitted constants
	do i = 1, nvar
		write(coef(imap(i)),'(f7.3)',iostat=ioer ) c(i)
	enddo

c write output
	write(*,'(1x,a7, 8a7,'' =>'',f6.3,1x,f6.3)',iostat=ioer) 
	1	lab, (coef(i), i=1,8), azrms, elrms
	if( logu .gt. 0 ) 
	1	write(logu,'(1x,a7, 8a7,'' =>'',f6.3,1x,f6.3)',iostat=ioer) 
	1	lab, (coef(i), i=1,8), azrms, elrms

	return
	end


c---------------------------------------------------------------------------
	subroutine LOADFIT( ant, rxn, depoint, remov, ichngcons, numfit )

c This routine loads the data into the arrays for fitting.
c Only non-zero weighted data is loaded.
c The data can be corrected for the old pointing constants.

	include 'point.inc'

c------------------INPUT-----------------------
c	ant	!antenna number
c	logical	depoint		!=.true. if pointing constants are to be removed
c	logical	remov(*)	!=.true. if point is to be removed

c-------------------OUTPUT---------------------
c loaded arrays of data
	integer	ichngcons	!number of changes in pointing constants during
				!data collection
	integer	numfit		!number of valid data points for the fit

c-----------------WORKING VARIABLES------------
	integer	ichngold	!previous number of changes in pointing constants 

	npts = 0
	ichngcons = 0
	numfit = 0

	write(*,'('' History of pointing constants'')',iostat=ioer)
	if( logu .gt. 0 ) 
	1	write(logu,'('' History of pointing constants'')',iostat=ioer)

	do i = 1, icant(ant,rxn)
		ir = ptdat(i,ant,rxn)
		call weight(ir,azwt,elwt)
		if( azwt .gt. 0. .or. elwt .gt. 0. ) then
			npts = npts + 1
			loadmap(npts) = i

c deal with removed points selected from before
			if( remov(i) ) then
				azwt = 0.
				elwt = 0.
			else
				numfit = numfit + 1
			endif

			wtaz(npts) = azwt
			wtel(npts) = elwt
			mjd(npts) = ir.mjd
			az(npts) = ir.az
			el(npts) = ir.el
			azo(npts) = ir.azo
			elo(npts) = ir.elo

c check for changes in the pointing constants, i.e. not constant
			ichngold = ichngcons
			do k = 1, 8
				if( npts .eq. 1 ) then	!first point
					cold(k) = ir.con(k)
				else	!after first point, check for changes
					if( cold(k) .ne. ir.con(k) ) then
						cold(k) = ir.con(k)
						ichngcons = ichngcons + 1
					endif
				endif
			enddo

c write out pointing constants when they change
			if( ichngcons .gt. ichngold .or. npts .eq. 1 ) then
c	integer	mjd_to_abstime_fortran	!function to convert MJD to people time
			   length = mjd_to_abstime_fortran(time3,mjd(npts),0) 
		write(*,'(1x,a20, 8f7.3)',iostat=ioer) time3, cold 
			   if( logu .gt. 0 ) 
	1	write(logu,'(1x,a20, 8f7.3)',iostat=ioer) time3, cold 
			endif

			if( depoint ) then	
c remove the old pointing terms selected by "imap" from the data
			   do k = 1, nvar
				if( imap(k) .le. 8 ) then
					c(k) = -ir.con(imap(k))	!-(-)=+
				else	!wobble terms not in data file
					c(k) = 0.
				endif
			   enddo
		  call subpnt(az(npts),el(npts),azo(npts),elo(npts),nvar,imap,c)
			endif

		endif
	enddo

c convert double precision MJD to single precision with zero at the 
c the beginning of the time span
	call MJDPLUS(npts,mjd,mjd1,ddmmmyy)

	return
	end


c-----------------------------------------------------------------------------
	subroutine WOBBLED( c256, s256, c512, s512, logu )

c evaluate amplitudes phases and excursion of wobble fit

	implicit none

	real	c256, s256, c512, s512	!wobble coefficients
	real	phase256, phase512	!wobble phases
	real	amp256, amp 512		!wobble amplitudes
	real	wob			!wobble curve 
	real	wmax, wmin		!wobble excuraion
	real	wobptop			!peak to peak excursion
	integer	i			!loop index
	integer	ioer			!iostat flag
	integer	logu			!unit # for log file
	real	atan3d			!function for arctan(x,y)

	phase256 = atan3d( s256, c256 )
	amp256 = sqrt( c256**2 + s256**2 )
	phase512 = atan3d( s512, c512 )
	amp512 = sqrt( c512**2 + s512**2 )

	wmax = 0.
	wmin = 0.
	do i = 0, 90
		wob = c256*cosd(4.*i) + s256*sind(4.*i)
	1		+ c512*cosd(8.*i) + s512*sind(8.*i)
		wmax = max( wmax, wob )
		wmin = min( wmin, wob )
	enddo

	wobptop = wmax - wmin

c write results
	write(*,'('' WOBBLE = '',f5.3,''*COS(256*theta+'',f5.0,'')''
	1	,'' + '',f5.3,''*COS(512*theta+'',f5.0,'')'',/
	2	,'' peak-to-peak WOBBLE = '',f6.3,'' arcmin'')'
	3	, iostat=ioer ) amp256, phase256, amp512, phase512, wobptop
	if( logu .gt. 0 )
	1	write(logu,'('' WOBBLE = '',f5.3,''*COS(256*theta+'',f5.0,'')''
	1	,'' + '',f5.3,''*COS(512*theta+'',f5.0,'')'',/
	2	,'' peak-to-peak WOBBLE = '',f6.3,'' arcmin'')'
	3	, iostat=ioer ) amp256, phase256, amp512, phase512, wobptop

	return
	end
