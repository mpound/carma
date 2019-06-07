c*
	PROGRAM SETWALSH
c& wh
c= SETWALSH - Fills in the walshing option words in COMMON. 
c: observing
c+
c	SETwalsh fills in the walshing option words in 
c	the common block (walants and wbants). This determines 
c	what patterns delay sends to the antenna walshing 
c	registers and also how the correlator sets the 
c	dewalshing registers. Note that the wideband correlator
c	is now walshed by baselines 36,37,38 and so can
c	coexist with the digital correlator.
c
c@ to
c	The value of the walshing option:
c	 on= turn on walshing
c	 off= turn  off walshing
c
c@ ants
c	Antenna flag describes which of the IFs the analog correlator
c	is plumbed to.  This information is passed to WBANTS.
c	Note that the wide band correlator baseline 1 must correspond
c	to the lowest numbered antenna, etc.
c--
c  17feb93  wh
c  aug 93  wh    add exit status
c---------------------------------------------------------------------c
	include '../inc/constants.inc'
	character*(*) version
	parameter(version='SETWALSH (version 2.1 27-aug-93)')
	character*10 option
	character*80 error
	integer walants(24) /
	1	216,98,143,150,140,213,55,255,254,180,214,63,
	2	62,138,86,225,227,19,228,245,94,14,104,231 /
	integer wbants(3)
	integer zeroes(24) /24*0/,scrambled(24) /24*0/,ants
	logical select

 	call output(version)

	call keyini
	call keya('to',option,'-')
	call ucase(option)
	call keyants(ants,0,error)
	 if(error(1:2).ne.'OK') type *,error
	call keyfin

	if(option(1:2).eq.'ON') then
	  call computi('WALANTS',walants,24,error)
	else if(option(1:3).eq.'OFF') then
	  call computi('WALANTS',zeroes,24,error)
	else
	  error = 'Illegal value of to>>'//option
	end if
	if (ants.gt.0) then
	  j = 1
	  do i= 1,nants
	    if(select(i,ants)) then
	      if (j.eq.4) then
		error = 'Too many antennas selected in ants'
		goto 99
	      end if
	      wbants(j) = i
	      j = j+1
	    end if
	  end do	    
	  call computi('WBANTS',wbants,3,error)
	end if

99	if (error(1:2).ne.'OK') call bug('f',error)
	end
