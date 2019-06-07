	character*(*) function TEXTIN(label,default,get,logu)

c This routine gets text input in response to "LABEL" and returns them
c as "TEXTIN". 
c On input "ANSWER" is the default response.

	implicit none

	character
     &	label*(*)	!text for question being asked
     &	,default*(*)	!default text from calling program
     &	,answer*79	!input from keyboard

	integer
     &	nchar	!number of characters entered from keyboard
     &	,logu	!log file unit number, 0 means don't write to log 

	logical
     &	get	!= .true. if a response or default is to be gotten from keyboard
c		!= .false. if only the "label" and answer are to be printed

	if (get) then
c get input from keyboard

1		write(*,10,err=1000) label, default
10	format (1x,'Enter ',a,', <CR> =',/,1x,a)
		read(*,20,err=1,end=9999) nchar, answer
20	format(q,a79)

		if ( nchar .eq. 0 ) then
			textin = default
		else	
			textin = answer
		endif
c write to LOG file
		if( logu .gt. 0 ) write (logu,22,err=25) label, textin

	else
c just output default text

		textin = default
1000		write (*,22,err=25) label, textin
22	format (1x,'The text for ',a,' is ',/,1x,a)
c write to LOG file
c		if( logu .gt. 0 ) write (logu,22,err=25) label, textin

	endif

25	return

c end of file input from keyboard, abort program
9999	stop

	end


c-----------------------------------------------------------------------------
	logical function ASK(label,default,get,unout)

c This routine gets YES/NO answers to questions "LABEL" and returns them
c as "ANSWER". 
c On input "ANSWER" is the default response.
c ON output "ANSWER" is the response.

	implicit none

	character
     &	label*(*)	!text for question being asked
     &	,char	
     &	,YN*1	!"N" or "Y"

	integer
     &	nchar	!number of characters entered from keyboard
     &	,unout	!output unit number for log file, =0 => no log file

	logical
     &	answer	!default response to question
     &	,get	!= .true. if a response or default is to be gotten from keyboard
		!= .false. if only the "label" and answer are to be printed
     &	,default	!input default answer

	answer = default

	if (get) then
c get input from keyboard

		YN = 'N'
		if(answer) YN = 'Y'

1		write(*,'(  1x , a, '' : '', a1)',err=1000) label, YN
		write(*,'(''+'', a, '' : '', $)',err=1000) label
		read(*,20,err=1,end=9999) nchar, char
20	format(q,a1)

		if ( nchar .eq. 0 ) goto 2000
	
		if ( char .eq. 'Y' .or. char .eq. 'y' ) then
			answer = .true.
			goto 2000
		endif

		if ( char .eq. 'N' .or. char .eq. 'n' ) then
			answer = .false.
			goto 2000
		endif

		goto 1	!not a valid response, try again

	else
c just output answer to the question

		YN = 'N'
		if(answer) YN = 'Y'
1000		write (*,22,err=2000) label, YN
22	format (1x, a, ' The answer is ',a1,'.')

	endif

2000	ask = answer

	if ( unout .gt. 0 ) then
c write to LOG file if new input is requested
		YN = 'N'
		if(answer) YN = 'Y'
		write (unout,22,err=25) label, YN
	endif

25	return

c end of file input from keyboard, abort program
9999	stop

	end

c-------------------------------------------------------------------
	function GETMJDTIME(label,default,ddmmmyy,get,logu)

c This routine takes in VAX format person time in the format
c DDMMMYY:hh:mm:s.s and returns MJD time.
c the default input time is in MJD
c "label" is the text to the question asking for the time

c The output getmjdtime = double precision MJD time

	implicit none

	double precision	default	!default MJD time
	double precision	getmjdtime	!returned MJD time
	character	label*(*)	!text for question being asked
	integer		nchar	!number of characters entered from keyboard
	integer	logu	!log file unit number, 0 means don't write to log 
	logical	get	!= .true. if a response or default is to be gotten 
			!from the keyboard; = .false. if only the "label" 
			!and answer are to be printed
	character*20	time1, time2, ddmmmyy	!people time DDMMMYY:hh:mm:s.s
	character*1	charin(20), charold(20)	!
	integer	length	!length of time character string
	integer	i	!index
	integer	ioer, ioer1, ioer2	!I/O error flags
	integer	mjd_to_abstime_fortran	!function to convert to people time
	double precision	scan_abstime_fortran	!function to convert
					!people time to MJD

c convert default MJD time to DDMMMYY:hh:mm:s.s
	length = mjd_to_abstime_fortran(time1,default,3)	!3=>.001sec

	if( get ) then
c get input from keyboard

1		write(*,10,iostat=ioer) label, time1
10	format (1x,'Enter ',a,', <CR>=',a,' ; ',$)
		read(*,20,err=1,end=9999) nchar, time2
20	format(q,a)

		if ( nchar .eq. 0 ) then	!=<CR>
			getmjdtime = default	!MJD
			time2 = time1 
		else	!decode text string for time

			if( nchar .le. 7 ) then
c use default for the remaining part
			   decode(nchar,'(<nchar>a1)'
	1				,time2,iostat=ioer) charin
			   do i = nchar+1,20
				charin(i) = ' '
			   enddo
			   decode(20,'(20a1)'
	1				,time1,iostat=ioer1) charold
			   write(time2,'(20a1)',iostat=ioer2)
	1		        (charin(i),i=1,nchar),(charold(i),i=nchar+1,20)
c		      	     if( ioer .ne. 0 .or. ioer1 .ne. 0 
c	1						.or. ioer2 .ne. 0) then
c				write(*,'('' nchar='', i3,2x, 20a1, 1x, 20a1
c	1				, 1x, a20)',iostat=ioer), nchar
c	2		        	,(charin(i),i=1,20),(charold(i),i=1,20)
c	3				,time2
c				goto 1
c			     endif
			endif

			getmjdtime = scan_abstime_fortran(time2)
			if( getmjdtime .le. 0 ) then
		write(*,'('' Problem with time conversion for;'',a)'
     &				,iostat=ioer) time2
		write(logu,'('' Problem with time conversion for;'',a)'
     &				,iostat=ioer) time2
				goto 1
			endif
			length = mjd_to_abstime_fortran(time2,getmjdtime,3)
		endif
	endif

	ddmmmyy = time2
c write to screen and LOG file
	write (*,'(''+'',a,'' = '',a,'' ='',f14.7,'' MJD'')',iostat=ioer) 
     &					label, ddmmmyy, getmjdtime
	if( logu .gt. 0 ) 
     &	write (logu,'(1x,a,'' = '',a,'' ='',f14.7,'' MJD'')',iostat=ioer) 
     &					label, ddmmmyy, getmjdtime

25	return

c end of file input from keyboard, abort program
9999	stop
	end



C------------------------------------------------------------------------------
	subroutine MJDPLUS(npts,mjd,mjd1,ddmmmyy)

c find integer MJD time nearest the center of the time span and subtract
c it from all times to get a single precision number 
c and people time string DDMMMYY

c started 29SEP92 DPW

	implicit none

c --------------INPUTS-----------
	integer			npts	!number of data points
	double precision	mjd(*)	!full MJD time

c---------------OUTPUTS----------
	real			mjd1(*)	!single precision (MJD - ddmmmyy)
	character*7		ddmmmyy	!people data for center of range

c---------WORKING VARIABLES------
	double precision	mjdsmall	!smallest data point
	double precision	mjdbegin	!first integer date
	double precision	mjddiff		!temporary difference
	integer			i		!index
	integer			length		!length of time string
	integer	mjd_to_abstime_fortran	!function to convert to people time
	integer			ioer	!I/O status
	character*20	string	!character string for time


	mjdsmall = mjd(1)

	do i = 1, npts
		if( mjd(i) .lt. mjdsmall ) mjdsmall = mjd(i)
	enddo

	mjdbegin = int( mjdsmall )
	
	length = mjd_to_abstime_fortran(string,mjdbegin,0)	!0=>0.sec

	ddmmmyy = string

	do i = 1, npts
		mjddiff = mjd(i) - mjdbegin
		mjd1(i) = mjddiff
c		write(*,'('' MJD ='',f14.3,'' = '',a,'' +'',f8.3,''days'')'
c	1			,iostat=ioer) mjd(i), ddmmmyy, mjd1(i)
	enddo

	return
	end


c-----------------------------------------------------------------------------
	subroutine BOUNDS( npts, x, wt, minspan, threshold, defmin, defmax
	1			, xmin, xmax, label, logu )

c routine to find reasonable plot limts

	implicit none

c-------------INPUTS------------------------------
	integer	npts	!number of points in data array x
	real	x(*)	!input data array
	real	wt(*)	!input data weight array
	real	minspan	!minimum allowed span
	real	threshold	!go to defaults if larger than threshold
	real	defmin, defmax	!default boundaries
	character*(*)	label	!label for parameter being bounded
	integer	logu	!LOG file device number. 0=> no log written

c-------------OUTPUT------------------------------
	real	xmin, xmax	!chosen limits

c---------WORKING VARIABLES-----------------------
	integer	i	!index
	real	x1, x2	!temporary variables
	integer	ioer	!I/O status

	x1 = x(1)
	x2 = x(1)
	do i = 1, npts
		if( wt(i) .gt. 0. ) then
			x1 = min( x1, x(i) )
			x2 = max( x2, x(i) )
		endif
	enddo

c check for span less than minspan
	if( (x2-x1) .lt. minspan ) then	!force xmax-xmin=minspan
		xmin = ( x1 + x2 - minspan ) / 2. 
		xmax = ( x1 + x2 + minspan ) / 2. 
	else	! add 10% to actual span to keep points on the plot
		xmin = x1 - .05*(x2-x1)
		xmax = x2 + .05*(x2-x1)
	endif

c check for span larger than threshold
	if( (x2-x1) .gt. threshold ) then	!use defualts
		xmin = defmin
		xmax = defmax
	endif

	if( logu .gt. 0 ) then
		write(*,'('' Bounds for '',a,'' are '',f12.3,'' to '',f12.3)'
	1					,iostat=ioer) label, xmin, xmax
		write(logu,'('' Bounds for '',a,'' are '',f12.3,'' to '',f12.3)'
	1					,iostat=ioer) label, xmin, xmax
	endif

	return
	end


c------------------------------------------------------------------------------
	real function VALUE(label,data,get,logu)

c This routine gets a single real value from the keyboard,
c "0" or <CR> returns the default VALUE = "data".
c Or it just prints the "label" and value = "data".
c It writes to unit logu and to the terminal.

	implicit none

	integer	logu	!LOG file unit number
	character
     &	label*(*)	!text desciption of variable
     &	,inline*20		!input line from keyboard
	
	real
     &	data	!default value on input
     &	,temp	!data read in from keyboard

	integer
     &	nchar	!number of characters entered from keyboard

	logical
     &	get	!= .true. if a value or default is to be gotten from keyboard
c		!= .false. if only the "label" and "data" are to be printed

	value = data

	if (get) then
c get input from keyboard

1		write (*,10,err=1000) label, data
10	format (1x,'Enter ',a,'(<CR>=',1pg13.6e2,') : ',$)
		read (*, 20, err=1, end=9999) nchar, temp
20	format(q,f20.0)
		if ( nchar .ne. 0 ) value = temp
c write to LOG file
		if( logu .gt. 0 ) write (logu,22,err=25) label, value

	else
c just output the value of "data"

1000		write (*,22,err=25) label, value
22	format (1x,'The value of ',a,' is ',1pg13.6e2)
c write to LOG file
		if(logu .gt. 0 ) write (logu,22,err=25) label, value

	endif

25		return

c end of file input from keyboard, abort program
9999	stop

	end


c------------------------------------------------------------------------------
	integer function IVALUE(label,data,get, logu)

c This routine gets a single integer value from the keyboard,
c "0" or <CR> returns the default IVALUE = "data".
c Or it just prints the "label" and ivalue = "data".
c It writes to unit logu and to the terminal.

	implicit none

	integer	logu	!LOG file unit number
	character
     &	label*(*)	!text desciption of variable
     &	,inline*20		!input line from keyboard
	
	integer
     &	data	!default value on input
     &	,temp	!data read in from keyboard

	integer
     &	nchar	!number of characters entered from keyboard

	logical
     &	get	!= .true. if a value or default is to be gotten from keyboard
c		!= .false. if only the "label" and "data" are to be printed

	ivalue = data

	if (get) then
c get input from keyboard

1		write (*,10,err=1000) label, data
10	format (1x,'Enter ',a,'(<CR>=',i8,') : ',$)
		read (*, 20, err=1, end=9999) nchar, temp
20	format(q,i)
		if ( nchar .ne. 0 ) ivalue = temp
c write to LOG file
		if( logu .gt. 0 ) write (logu,22,err=25) label, ivalue
22	format (1x,'The value of ',a,' is ',i8)

	else
c just output the value of "data"

1000		write (*,22,err=25) label, ivalue
c write to LOG file
		if( logu .gt. 0 ) write (logu,22,err=25) label, ivalue

	endif

25		return

c end of file input from keyboard, abort program
9999	stop

	end
