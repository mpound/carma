c
c* DEADMAN
c: alarm
c+
	PROGRAM DEADMAN

c	DEADMAN: toggles the alarm bit of the lab/dorm alarm system;
c	if 'ALARMS[4:8]='ring', the alarm is not toggled.
c--
c	may 94 wh  
c------------------------------------------------------------c
	character*80 error
	character*10 state
	integer bits,peek,newbit

c	CALL LOCKEXIT
	CALL MBUSOPEN(ERROR)
	 if(error(1:2).ne.'OK') type *,error

	DO WHILE(.true.)
	  CALL COMGETA('ALARMS',state,8,error)

	  IF((STATE(1:4).eq.'DISA').or.(STATE(1:8).eq.'ENAB_off')) THEN
	    if (newbit.eq.0) then
		newbit = '8000'x
	    else
		newbit = 0
	    end if
	  ELSE
	  IF(STATE(1:8).EQ.'ENABring') THEN 
	    newbit = 0
	  END IF
	  END IF
	  BITS = PEEK('E80C'X)
	  BITS = BITS.AND.'7fff'X
	  BITS = BITS.OR.NEWBIT
	  CALL POKE('E80C'X,BITS)
	  CALL WAIT_TICKS(500)
	END DO
	END

