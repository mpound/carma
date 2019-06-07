c*
	PROGRAM LSTSYMBOL
c= LSTSYMBOL - reads the lst and writes out the lst as hhmm.
c: observing
c+
C	LSTSYMBOL reads the lst and writes out
c	the lst as hhmm. Use it with the shell
c	command "set LST=`lstsymbol`" to set
c	the shell symbol LST.
c--
c	unix-> nov 90 wh
c	aug93 wh  add exit status
c       20jul95 jm changed type statement to output call.
C----------------------------------------------------------------C
	include '../inc/constants.inc'
	real*8 ut,lst
	integer tick
	character*4 lststring
	character*14 a,angles
	character*80 error
	integer*4 month,day,year

	CALL MBUSOPEN(ERROR)
	 if(error(1:2).ne.'OK') goto 99
	CALL GET_TIMES(TICK,2,MONTH,DAY,YEAR,UT,LST,ERROR)
	 if(error(1:2).ne.'OK') goto 99

	A = ANGLES(LST*12.D0/DPI)
	LSTSTRING = A(1:2)//A(4:5)

	call output(lststring)
99	if (error(1:2).ne.'OK') call bug('f',error)
	END
