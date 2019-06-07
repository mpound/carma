	PROGRAM BIT
c= BIT - loads telemetry registers from a file.
c: test
c& wh
c+
C  BIT:	loads telemetry registers from a file that is formatted:
c		telemetry_address(hex)   contents(hex)  nbytes(int)
c
c  ARGUMENTS:
c  ANTS		antenna flag
c  HEX		telemetry address
c  BIT		up to 8 bit numbers (n,m,etc) negative means turn off
c--
c	dec 92 wh
c	jul 93 wh - new version of keyants
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='BIT (version 1.1 26-jul-93)')
	include '../inc/constants.inc'
	integer ants,tpeek,ant
	character*80 error
	logical select
	integer inbits,outbits,slot,bit(8)
	character*10 shex,sbit
	logical turnon(8)

	call mbusopen(error)
	if (error(1:2).ne.'OK') goto 99
	call output(version)
	call keyini
	call keyants(ants,1,error)
	if(error(1:2).ne.'OK') type *,error
	call keya('hex',shex,'0')
	read(shex,'(z)') slot
	do i=1,8
	  call keya('bit',sbit,'999')
	  if(sbit(1:1).eq.'-') then
	    turnon(i) = .false.
	  else
	    turnon(i) = .true.
	  end if
	  read(sbit,'(i)') bit(i)
	  bit(i) = abs(bit(i))
	end do
	call keyfin

	if(slot.eq.0) then
	  error = 'telemetry address not entered'
	  goto 99
	else if (bit(1).eq.999) then
	  error = 'bit position not entered'
	  goto 99
	end if

	do ant=1,nants
	 if(select(ant,ants)) then
	   inbits = tpeek(ant,slot,1,error)
	    if(error(1:2).ne.'OK') type *,error
	   outbits = inbits
	   do i=1,8
	    if(bit(i).ne.999) then
	     k = 2**bit(i)
	     if(turnon(i)) then
		outbits = outbits .or. k
	     else
		outbits = outbits .and. (.not.k)
	     end if
	    end if
	   end do
	   call tpoke(ant,slot,1,outbits,error)
	   if(error(1:2).ne.'OK') type *,error
	   type 100,'Ant:',ant,' Hex:',slot,' Old:',inbits,' New:',outbits
100	   format(x,a,i2,a,z4,a,z4,a,z4)
	 end if
	end do
99	type *,error
	end

