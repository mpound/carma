c*
	PROGRAM UTSYMBOL
c& wh
c= UTSYMBOL  furnish UT as a symbol
c: observing
c+
c
C	UTSYMBOL reads the ut and types out 
c	the UT as hhmm . It is used in the shell command:
c	set UT=`utsymbol` to set the shell symbol UT.
c--
c	unix-> nov 90 wh
c	aug93 wh  add exit status
c       20jul95 jm changed type statement to output call.
C----------------------------------------------------------------C
	include '../inc/constants.inc'
	real*8 ut
	integer tick
	character*4 utstring
	character*14 a,angles
	character*80 error
	integer*4 month,day,year

	CALL MBUSOPEN(ERROR)
	 if(error(1:2).ne.'OK') goto 99
	CALL GET_TIMES(TICK,2,MONTH,DAY,YEAR,UT,LST,ERROR)
	 if(error(1:2).ne.'OK') goto 99

	A = ANGLES(UT*12.D0/DPI)
	UTSTRING = A(1:2)//A(4:5)

	call output(utstring)
99	if (error(1:2).ne.'OK') call bug('f',error)

	END
