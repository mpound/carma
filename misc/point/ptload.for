
	subroutine LOADPNT

c Start modifications to handle new file format and change in pointing
c constant functions, DPW 23SEP92.
c A logical function "SELECT" will be used to select the valid data

c Format change 07MAY93, MJD>49114.75
c
c  Written by S.T.Myers          19 Jun 1985
c  Last revised                  22 Jul 1985
c  Revised to new format          1 Sep 1986
c  Revised to new sign convention 6 Jul 2005   (Peter Teuben)
c
c  Routine to load pointing data from input file into common block
c  PTARRAYS.
c
c  Logical Unit 11 is used for input while unit 12 is used for output
c  to the LOGFILE.


	include 'point.inc'

	character*46	line1	!numerical part of first line for decoding
	integer		mjd45	!last two digits of ir.mjd time
	character*100	inline	!full data file input line
	character*2	rx	!receiver number prefix "RX"
	character*80	line2	!numerical part of first line for decoding

c  Open file .

10	write(*,'(//)')
	call OPENREAD( 11, infile )

	file = infile
	call hdr( 'LOADING', 6 )

c  Write filename to logfile.

      write( 12, 1110 ) file
 1110 format( / ' Using pointing file ', a32 / )

c set up selection criterion
	call SETSELECT

c initialize counters
	ivalid = 0
	do ant = 1, maxants
		icant(ant,1) = 0
		icant(ant,2) = 0
	enddo

c  Read data.

	write(*,'('' Reading data file.'')')
	write(12,'('' Reading data file.'')')

100	continue
c return point for reading data records

	read( 11, '(a)',iostat=ioer,end=1000) inline
c	write( *, '(1x,a)',iostat=ioer) inline
	if( ioer .ne. 0 ) then
		write(*,'('' Error reading data file after'',/,a)') inline
		write(logu,'('' Error reading data file after'',/,a)') inline
		goto 100
	endif

c check for "MM" as first two characters
	read( inline, '(a2)',iostat=ioer) mm
	if( mm .ne. 'MM' ) then
	  write(*,'('' MM characters not found where expected after MJD'',f9.3)'
     +			,iostat=ioer) ir.mjd
	 write(logu,'('' MM characters not found where expected after MJD'',f9.3)'
     +			,iostat=ioer) ir.mjd
		goto 100	!try next record
	endif

c look for format change when RX# was added
	read( inline, '(4x, a2)',iostat=ioer) rx

c------------------------------
	if( rx .eq. 'RX' .or. rx .eq. 'OP' ) then  !new format as of ~24FEB93
c read first half of record;
c ant, RX#, source, time, position, observed offsets and noise

	   if( rx .eq. 'OP' ) then	!optical data, set ir.rx=1
		read( inline, '(a2,i1,5x,a12,1x,a7,a)', iostat=ioer, end=1000
	1	, err=98 ) mm, ir.ant, ir.src, ir.date, line2
		ir.rx = 1
	   else	!radio data
		read( inline, '(a2,i1,3x,i1,1x,a12,1x,a7,a)', iostat=ioer
	1	, end=1000, err=98 ) mm, ir.ant, ir.rx, ir.src, ir.date, line2
	   endif

c get rest of fields using free format
		read( line2, *, iostat=ioer) ir.mjd, ir.az, ir.el
	1				,ir.azo, ir.azer, ir.elo, ir.eler

c-----------------------------------
	else	!old format not containing RX# or OPT
c deal with change in format before mjd=48937. by reading and decoding
	read( inline, '(a2,i1,1x,a12,1x,a7,f10.3,a)', iostat=ioer, end=1000
	1	, err=98 ) mm, ir.ant, ir.src, ir.date, ir.mjd, line1
c the last 2 digits of ir.date for mjd's after 48937. are the first two
c characters in line1. This is because the radio data before this time
c was written as degreesx57.296
	if( ioer .ne. 0 ) then
		write(*,'('' Error decoding "INLINE" '',/,a)') inline
		write(logu,'('' Error decoding "INLINE" '',/,a)') inline
		goto 100
	endif

c decode numerical data from line1 depending upon when it was written
	if( ir.mjd .lt. 48937. ) then

		decode(42, 1120, line1, iostat=ioer )
	1		ir.az, ir.el, ir.azo, ir.azer, ir.elo, ir.eler
1120	format( 	 f9.1,  f7.1,   f8.2,    f5.2,   f8.2,    f5.2)
c correct az el data for early radio data
		if( radio ) then
			ir.az = ir.az*3.141592654/180.
			ir.el = ir.el*3.141592654/180.
		endif

	else
		decode(42, 1121, line1, iostat=ioer )
	1		mjd45, ir.az, ir.el, ir.azo, ir.azer, ir.elo, ir.eler
1121	format( 	   i2,  f8.2,  f8.2,   f7.2,    f5.2,   f7.2,    f5.2)
c mjd45 is the two extra digits added to the mjd date for mjd. greater 48937.
		ir.mjd = ir.mjd + mjd45/1.e+5

	endif

	ir.rx = 1

	endif	!RX/OP format check
c-----------------------------------
c	write( *, '(1x,a2,i1,1x,i1,1x,a12,1x,a7,f15.5,/,6f10.3)'
c     +		, iostat=ioer ) mm, ir.ant, ir.rx, ir.src, ir.date, ir.mjd
c     +		, ir.az, ir.el, ir.azo, ir.azer, ir.elo, ir.eler
c	write( logu, '(1x,a2,i1,1x,i1,1x,a12,1x,a7,f15.5,/,6f10.3)'
c     +		, iostat=ioer ) mm, ir.ant, ir.rx, ir.src, ir.date, ir.mjd
c     +		, ir.az, ir.el, ir.azo, ir.azer, ir.elo, ir.eler


c read second half of record; pointing constants in use when the data was taken
	read( 11, 1220, end=1000, err=99 )    ir.pad, (ir.con(i), i=1,8)
1220	format(                             10x,a4,             8f8.3)
c	write( *, 1220, iostat=ioer )    ir.pad, (ir.con(i), i=1,8)
c	write( 12, 1220, iostat=ioer )    ir.pad, (ir.con(i), i=1,8)

	ivalid = ivalid + 1

c check for data taken after MJD=49114.75, and RX (radio) data
c if so, read extra line
	if( ir.mjd .gt. 49114.75 .and. rx .eq. 'RX') read(11, *, iostat=ioer, end=1000 ) inline

c check selection criterion using the function select
	if( .not. select(ir) ) goto 100

c store data into arrays
	icant(ir.ant,ir.rx) = icant(ir.ant,ir.rx) + 1

	ptdat(icant(ir.ant,ir.rx),ir.ant, ir.rx) = ir

	goto 100	!go read next record
	
1000	continue
c done reading the input file
	write(*,'(i6,'' valid records read.'')',iostat=ioer) ivalid
	write(12,'(i6,'' valid records read.'')',iostat=ioer) ivalid
	do ant = 1, maxants
	   do rxn = 1, 2
		write(*,'(i6,'' selected for antenna #'',i1,'' RX'',i1)'
     +			,iostat=ioer) icant(ant,rxn), ant, rxn
		if( logu .gt. 0 )
     +		write(logu,'(i6,'' selected for antenna #'',i1,'' RX'',i1)'
     +			,iostat=ioer) icant(ant,rxn), ant, rxn

	   enddo
	enddo

c  Finished reading input file

      close(11)	!input file

      return

c  In case of read error ...

98	continue
	write(*,'('' Input file read error in 1st half at MJD'',f9.3)'
     +			,iostat=ioer) ir.mjd
	write(logu,'('' Input file read error in 1st half at MJD'',f9.3)'
     +			,iostat=ioer) ir.mjd
	goto 100	!try next record

99	continue
	write(*,'('' Input file read error in 2nd half after MJD'',f9.3)'
     +			,iostat=ioer) ir.mjd
	write(logu,'('' Input file read error in 2nd half after MJD'',f9.3)'
     +			,iostat=ioer) ir.mjd
	goto 100	!try next record

	end	!end of LOADPNT routine


c---------------------------------------------------------------------------
	function SELECT(ir)

c this routine decides if the data is in the selected range
c or has reasonable errors
 
	include 'point.inc'

	select = .true.
	
	if( ir.mjd .lt. mjdstart .or. ir.mjd .gt. mjdstop ) select = .false.

c require both axes to have finite errors
	if( ir.azer .gt. .3 .or. ir.eler .gt. .3 ) select = .false.
	xxx = dble( 1. )
	yyy = dmod( ir.mjd, xxx )	!modulus
	fday = yyy
	if( fday .lt. utstart .or. fday .gt. utend ) select = .false.

	end


c----------------------------------------------------------------------------
	SUBROUTINE OPENREAD( LUN, INFILE )

C Subroutine to OPEN input file on unit LUN

	include 'point.inc'

c check for default file type
10	yes = ask('Analyze normal optical pointing file?',
     *            .true.,.true.,logu)
	if( yes ) then	!open default optical file
		infile = 'optical.data'
		radio = .false.
	else
	     yes = ask('Analyze normal radio pointing file?',
     *                 .true.,.true.,logu)
		if( yes ) then	!open default optical file
			infile = 'radio.data'
			radio = .true.
		else
C Prompt user for filename.
			infile = textin('pointing data filename'
     &				,infile,.true.,logu)
			radio = ask('Is this a radio pointing file'
	1			,.false.,.true.,logu)
		endif
	endif

c set up selection criterion
c set up default times as yesterday and today
	time2 = '  '
	mjdstart = scan_abstime_fortran(time2)
	mjdstop = mjdstart + 1.
	sameterms = .true.

c set default fitting terms
	if( radio ) then

		term(1) = .false.
		term(2) = .false.
		term(3) = .false.
		term(4) = .false.
		term(5) = .false.
		term(6) = .true.	!R1, AZ offset
		term(7) = .true.	!R2, EL offset
		term(8) = .false.
c set default time span to 10 days
		mjdstart = mjdstart - 9

	else	!optical point

		term(1) = .true.	!E1, AZ encoder zero
		term(2) = .false.	!E2, EL encoder zero
		term(3) = .true.	!E3, axis non-orthogonality
		term(4) = .true.	!E4, North-South axis
		term(5) = .true.	!E5, East-West axis
		term(6) = .true.	!O1, AZ offset
		term(7) = .true.	!O2, EL offset
		term(8) = .true.	!O3, EL sag

	endif

c	OPEN(LUN, FILE=infile, CARRIAGECONTROL='LIST',
c	1	ERR=999,READONLY, SHARED, STATUS='OLD')

	OPEN(LUN, FILE=infile,ERR=999,READONLY, STATUS='OLD')

	return

999	write(*,*) ' -- Error detected during OPEN -- try again '
	goto 10

	END
