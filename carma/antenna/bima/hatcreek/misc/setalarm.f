c*
	PROGRAM SETALARM
c& wh
c= setalarm - Enable the dorm alarm.
c: misc
c+
c	setalarm Enables or Disables the DORM ALARM
c	software switch which is contained in the
c	common block variable ALARMS. A value of
c	ENABLED results in the dorm alarm being
c	turned on for alarm conditions in watch or
c	wait_for_ants.  A  value of DISABLED will
c	block the alarm. Both of these options
c	turn the alarm circuit off; the option 
c	TEST rings the alarm.
c	This new version works with the program deadman
c	which toggles the alarm interface every 10 secs.
c	This insures that the alarm rings if the computer
c	hangs.
c@ to
c	ON or OFF or TEST
c--
c  sep 92 wh	
c  aug 93 wh   set exit status
c  sep 93 wh   add TEST and bit setting
c  nov 93 wh   invert bit
c  aug 94 wh	change flags for deadman
c---------------------------------------------------------------------c
	character*(*) version
	parameter(version='setalarm (version 2.0 10-aug-94)')
	character*80 error
	character*10 option

 	call output(version)
	call mbusopen(error)
	 if(error(1:2).ne.'OK') goto 99

	call keyini
	call keya('to',option,' ')
	call ucase(option)
	call keyfin
	if (option(1:2).eq.'ON') then
	 call computa('ALARMS','ENAB_off',8,error)
	 type *,'Alarm is ENABLED'

	else if (option(1:3).eq.'OFF') then
	 call computa('ALARMS','DISA_off',8,error)
	  type *,'Alarm is DISABLED'
	else if (option(1:4).eq.'TEST') then
	  type *,'Alarm is turned on for testing'
	 call computa('ALARMS','ENABring',8,error)
	else
	  call bug('f',' ON or OFF or TEST please')
	end if
99	if (error(1:2).ne.'OK') call bug('f',error)
	end
