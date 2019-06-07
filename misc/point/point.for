c
      program point
c
c--------------------------------------------------------------------
c
c		MM INTERFEROMETER POINTING PACKAGE
c
c--------------------------------------------------------------------
c
c  Program to calculate and plot the various pointing parameters.
c  Uses pointing file output by the 10m - millimeter interferometer.
c  Handles both optical and radio pointing data.  Allows the
c  user to display and analyze data. 
c
c  Fortran unit 11 is used for I/O of data and unit 12 is used for
c  the logfile POINT.LOG.
c
c					Steven T. Myers  7/85
c
c  Major modifications to handle multiple telescopes. 
c                                       David Woody Oct. 92
c  Minor fortran dialect issues, and C interface changes, for the CARMA
c  version to run on linux
c                                       Peter Teuben, June 2005
c    "peter's ported pointing package points pretty proudly"
c
c--------------------------------------------------------------------
c
c  The program is modular and the various subroutines are contained
c  in separate "modules" or source code files.
c
c	Module Name	Routine			Function
c      -------------   ---------    --------------------------------
c
c	POINT		POINT		main program
c			TOPSCREEN	program header block
c			HDR		write page header
c			CLEAR		clear screen
c			TIMEHEADER	time stamp and range
c			SETSELECT	set up data selection criterion
c
c	PTUTIL		ASK		get logical answer to question
c			TEXTIN		get text input
c			GETMJDTIME	get time
c			MJDPLUS		convert MJD to ddmmmyy+dd.ddd
c			IVALUE		get an integer
c			VALUE		get a real number
c			BOUNDS		determine bounds for a plot
c
c	PTLOAD		LOADPNT		load pointing file
c			OPENREAD	open input data file
c			SELECT		decide if data is in range
c
c	FITXYZZ		FITXYZZ		generalized fitting routines
c			SUBFUNC		subtract function from data set
C			SUBPNT		subtract function from data point
C			FUNCLABEL	label for function terms
C    			XYZFUNC 	first function
C			XYZFUNC2 	second function
c			WTRMS		calculate weighted RMS
C
c	PTFITS		FITCON		fit pointing constants
c			WEIGHT		determine weight for a data point
c			CONSHEAD	write coefficient header
c			WRITECONS	write out coefficients
c			WOBBLED		analyse wobble fit
c
c	PTPLOT		PLOTPT		plot offsets versus time
c			AZEXPLT		plot AZO's and ELO's vs. AZ and EL
c			TIMEPLT		plot AZO's and ELO's vs. time
c			DIURNALPLT	plot AZO's and ELO's vs. time of day
c			HAIRBALL	vector plot inside circle of errors
c			AZELTOXY	generate vectors for HAIRBALL
c			FX, FY		cosd and sind for making PGPLOT circles
c
c---------------------------------------------------------------------
c
c  A set of "common" variables is used to transfer data and status
c  information between routines.
c
c	Program set parameter :
c
c		MAXPTS		maximum array size (1000) per antenna
c
c
c---------------------------------------------------------------------
c
c Program version and documentation update.
c
c					STM   09 Sep 1985
c					STM   11 Dec 1985
c
c New formats and constants.
c
c                                       STM   08 Sep 1986
c
c Major revision started 24SEP92 by DPW
c
c---------------------------------------------------------------------
c
c
	include 'point.inc'
        include 'pointdata.inc'

	logu = 12

	call clear
	write(*,'(//,'' -----------------WELCOME to POINT/CARMA-------------'',//
	1	,'' This program analyses Optical and Radio ''
	2	,''pointing data in CARMA offset format.'',//)')
	write(logu,'(//,'' WELCOME to POINT/CARMA'',//
	1	,'' This program analyses Optical and Radio ''
	2	,''pointing data in CARMA offset format.'',//)')

c  Open logfile.

	open( logu, file='point.log')
	write(*,'('' Opening LOG file POINT.LOG'')',iostat=ioer)

c
c  Write date and time to log.
	call TIMEHEADER( 1, 1 )

c open pointing constants file
	mjdstart = scan_abstime_fortran(' ')
	length = mjd_to_abstime_fortran(time3,mjdstart,0)	!0=>0.sec
	ddmmmyy = time3
	constfile = 'PNT_CONST.'// ddmmmyy
	open( constu, file=constfile)
	write(*,'('' Opening  pointing  constants file ''
	1	,a, '' on unit'',i3)',iostat=ioer) constfile, constu
	write(logu,'('' Opening  pointing  constants file ''
	1	,a, '' on unit'',i3)',iostat=ioer) constfile, constu

c  Main screen.
10      continue
c	call topscreen

c  First load data.
	
20	call hdr( 'LOAD', 4 )

	call loadpnt

c determine which antennas to fit
	do ant = 1, maxants
	   do rxn = 1, 2
		if( icant(ant,rxn) .ge. 1 ) then     !check to see for analysis
			if( logu .gt. 0 ) then
				write(logu,'(a)') formfeed
		write(logu,'(''   ANALYSIS FOR ANTENNA #'',i2,'' RX #'',i1
	1				,/,'' ____________________________'',/)'
	1				,iostat=ioer) ant, rxn
			endif
		write(*,'(//,i6,'' points found for AN#'',i1,'' RX#'',i1)'
     +			,iostat=ioer) icant(ant,rxn), ant, rxn
			if( logu .gt. 0 )
     +		write(logu,'(//,i6,'' points found for AN#'',i1,'' RX#'',i1)'
     +			,iostat=ioer) icant(ant,rxn), ant, rxn
		yes = ask('Analyze pointing data for this antenna and receiver?'
     +						,.true.,.true.,logu)
			if( yes ) then
				call clear
				threshold = 3.	!bad data threshold
111	continue
				call TIMEHEADER( ant, rxn )
				call fitcon(ant, rxn)

				yes = ask('List this data?',.false.,.true.,logu)
				if( yes ) then
	write(*,'(
	1	''  +days     AZ     EL     AZO    ELO  AZwt ELwt   date  '')')
	write(logu,'(
	1	''  +days     AZ     EL     AZO    ELO  AZwt ELwt   date  '')')

					do i = 1, npts
	write(*,'(   f8.5,  f7.2,  f7.2,  f7.3,  f7.3,f5.2,f5.2,2x,     a7)'
	1	,iostat=ioer)
	2	  mjd1(i), az(i), el(i),azo(i),elo(i),wtaz(i),wtel(i),ddmmmyy
	write(logu,'(f8.5,  f7.2,  f7.2,  f7.3,  f7.3,f5.2,f5.2,2x,     a7)'
	1	,iostat=ioer)
	2	  mjd1(i), az(i), el(i),azo(i),elo(i),wtaz(i),wtel(i),ddmmmyy
					enddo

				endif

c				yes = ask('Plot this data?',.true.,.true.,logu)
c				if( yes ) then
					call TIMEHEADER( ant, rxn )
					call PLOTPT
c				endif

			yes2 = ask('Try again on this antenna and receiver?'
     +							,.false.,.true.,logu)
				if( yes2 ) then
		write(*,'(//,i6,'' points found for AN#'',i1,'' RX#'',i1)'
     +			,iostat=ioer) icant(ant,rxn), ant, rxn
			if( logu .gt. 0 )
     +		write(logu,'(//,i6,'' points found for AN#'',i1,'' RX#'',i1)'
     +			,iostat=ioer) icant(ant,rxn), ant, rxn
					threshold = 
	1	value('Bad point threshold [stand. dev.]'
	2						,threshold,.true.,logu)
					goto 111
				endif

			endif
		endif
	   enddo	!end of RX1 and 2
	enddo	!end of icant(ant,rxn)
 
	yes = ask('Start over again?',.true.,.true.,logu)
	if( yes ) goto 10 

c  Spawn DCL phantom process.
c
c         istat = dclspawn()
c
c            call pgadvance
c            call pgend

         close( logu )	!log file
         close( constu )	!pointing constants file

	stop
	end


c----------------------------------------------------------------------------
	subroutine SETSELECT

c set up data selection criterion
c
c and write to plot header

	include 'point.inc'

	character*6	opendev	!='OPEN"' if a plot device is open

c default plot device
        soft = '/xw'
c20	soft = textin('Screen PGPLOT Device name',soft,.true.,.true.,logu)
20	soft = textin('Screen PGPLOT Device name',soft,.true.,logu)
	if( soft .eq. '?' ) then
		call PGLDEV	!get possible device types
		goto 20
	endif

c test this device
	call pgbegin( 0, soft, 1, 1 )
	call PGQINF( 'STATE', opendev, length )
	if( opendev .ne. 'OPEN' ) goto 20
	call pgend

40	write(*,'('' Hardcopy postscript plots are written to '',a40
	1	,/,'' and spooled to the default PRINT queue.'')'
	2	,iostat=ioer) hard

	hard = '/vps'
c	hard = textin('Hardcopy written to and PRINTed ',hard,.false.,.true.,logu)
	hard = textin('Hardcopy written to and PRINTed ',hard,.true.,logu)
	if( hard .eq. '?' ) then
		call PGLDEV	!get possible device types
		goto 40
	endif
c test this device
	call pgbegin( 0, hard, 1, 1 )
	call PGQINF( 'STATE', opendev, length )
	if( opendev .ne. 'OPEN' ) goto 40
	call pgend

100	mjdstart = getmjdtime('start time',mjdstart,time1,.true.,logu)
	mjdstop  = getmjdtime(' stop time',mjdstop ,time2,.true.,logu)

	if( mjdstart .gt. mjdstop ) then
		write(*,*) ' Stop is before Start.'
		write(logu,*) ' Stop is before Start.'
		goto 100
	endif

	yes = ask('Do you want to enter different times?',.false.,.true.,logu)
	if( yes ) goto 100

	utstart = 0.0
	utend	= 24.0
	yes = ask('Limit UT range?',.false.,.true.,logu)

	if( yes ) then
		utstart = value('Starting UT [hrs]',utstart,.true.,logu) 
		utend =   value('Ending   UT [hrs]',utend,  .true.,logu) 
c convert UT hrs to decimal fraction of a day for use with MJD
		utstart = utstart/24.
		if( utstart .lt. 0. .or. utstart .gt. 1. ) utstart = 0.
		utend = utend/24.
		if( utend .lt. 0. .or. utend .gt. 1. ) utend = 1.
	endif

	call TIMEHEADER( 1, 1 )

	depoint = .true.
	subfit = .true.

	write(*,*) header            
	write(logu,*) header

	return
	end


c------------------------------------------------------------------------------
	SUBROUTINE TOPSCREEN
C
C Routine to produce beginning screen.
C
	INTEGER ISTAT
C
C Clear screen.
C
	CALL CLEAR
C
C Generate header.
C
	TYPE 10
 10	FORMAT(T20,'      OWENS VALLEY RADIO OBSERVATORY'/
     +         T20,'MILLIMETER INTERFEROMETER POINTING PACKAGE'//
     +	       T5,70('-')//T5,'After prompt enter desired command.'/
     +         T5,'Enter "H" for help.'/T5,'Record of session is',
     +	          ' in logfile POINT.LOG .'/)
C
	RETURN
	END


c-------------------------------------------------------------------------------
      subroutine HDR( string, len )
c
c  Routine to print screen header.
c
      include 'point.inc'
c
      character   string*16
      integer*4   istat, len

c      call clear

c  Generate header.
c
      type 1101, string( 1:len ), file
 1101 format( t5, 'POINT <', a<len>, '>', t34,
     +   'Current file: ', a // t5, 70( '-' ) / )
c
      return
      end


c-------------------------------------------------------------------------------
	SUBROUTINE CLEAR
C
C Subroutine to clear screen and home cursor on VT terminal.
C
C						25 July 1985 BJC
	BYTE BARRAY(4)
	CHARACTER*1 CARRAY(4)
	EQUIVALENCE( BARRAY, CARRAY )
C
C Form escape sequence for screen clear.
C
	BARRAY(1) = 27		!Decimal value of ESC character
	CARRAY(2) = '['
	CARRAY(3) = '2'
	CARRAY(4) = 'J'
C
C Write sequence to device.
C
cpjt	WRITE( 6, 10 ) CARRAY
 10	FORMAT( 1X, 4A1 )
C
C Form escape sequence for cursor home.
C
	CARRAY(3) = 'f'
C
C Write sequence to device.
C
cpjt	WRITE( 6, 20 ) CARRAY(1), CARRAY(2), CARRAY(3)
 20	FORMAT( 1X, 3A1 )
C
	RETURN
	END


c-----------------------------------------------------------------------------
	subroutine TIMEHEADER( ant, rxn )

c write time stamped header for LOG file and plots

	include 'point.inc'

	character	cdate*9, ctime*8	!time stamp 

c  get date and time to log.

c      write( logu, '( t30, a9 / )' ) 'POINT.LOG'
      call date( cdate )
      call time( ctime )
      timestamp = ctime // '   ' // cdate
c      write( *, '('' It is now '', a20 )' ) timestamp
c      write( logu, '('' The time is '', a20 )' ) timestamp

c convert MJD selection time to people time
	length = mjd_to_abstime_fortran( time1, mjdstart, 0)
	length = mjd_to_abstime_fortran( time2, mjdstop, 0)

c write plot header
	if( radio ) then
		write(header,'(1x,a20,'' RADIO '',a20,'' - '', a20)'
	1		,iostat=ioer) timestamp, time1, time2
		write(toplbl,'(a68,'' AN'',i1,'' RX'',i1)',iostat=ioer) 
	1							header,ant,rxn
	else
		write(header,'(1x,a20,'' OPTIC '',a20,'' - '', a20)'
	1		,iostat=ioer) timestamp, time1, time2
		write(toplbl,'(a68,'' AN'',i1,'' OPT'')',iostat=ioer) header,ant
	endif

	write(*,'(//,a,//)',iostat=ioer) toplbl
	write(logu,'(//,a,//)',iostat=ioer) toplbl

	return
	end
