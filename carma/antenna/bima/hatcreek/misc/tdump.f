c*
	PROGRAM TDUMP
c& wh
c= TDUMP  display selected telemetry registers
c: test
c+
c	TDUMP displays selected slots from the telemetry
c	for all the antennas selected by ants.  The 
c	slots are a variable number of bytes wide and are
c	displayed in hex as well as converted to another
c	format (2's comp/Unsigned/Bits/Velocity) that is
c	easier to interpret.  All telemetry slots consist
c	of an outgoing register and an incoming register;
c	many outgoing words have different definitions
c	for the incoming bits and it is also possible to
c	read back the bits that have been sent out by
c	prepending a - to the address.
c
c@ ants
c	Antenna flag.
c
c@ hex
c	1-6 hex byte address numbers that will be displayed.
c	(Separate multiple values with commas.) The following
c	is a partial listing of possible addresses; the file
c	$HATTABS/tdump.ascii contains the addresses other than
c	AtoD addresses which are found in $HATTABS/atod.ascii.
c	For AtoD slots, use the value of INDEX from atod.ascii.
c
c	 HEX		CONTENTS
c	 20		Azim Resolver in
c	 22		Elev Resolver in
c	 24		Focus Resolver in
c	 2C		Azim Velocity status (- for out)
c	 2E		Elev Velocity status (- for out)
c	 30		Azim Inductosyn Cos in
c	 32		Azim Inductosyn Sin in
c	 34		Elev Inductosyn Cos in
c	 36		Elev Inductosyn Sin in
c	 -3C		Freq Offset control out
c	 -3D		Freq Offset 1 Frequency out
c	 -40		Freq Offset 1 walsh out
c	 -41		Freq Offset 2 Frequency out
c	 -44		Freq Offset 2 walsh out
c	 -45		Freq Offset 1 Phase out
c	 -47		Freq Offset 2 Phase out
c	 -50 to -6F	Motor driver cards
c	 70 to 13A	Analog Inputs
c	13A		Antenna Status bits
c	1000-1104	AtoD slots.
c
c@ secs
c	Repeat interval in seconds (0 or missing = no repeat)
c--
c	apr 92 wh
c	nov 92 wh - use atod to provide natural units
c	aug 93 wh - zero telem error regis each time
c	dec 93 wh - don't check for antennas
c	oct 97 wh - allow ant=0; the LAB
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='TDUMP (version 1.3 15-oct-97)')
	include '../inc/constants.inc'
	integer slot(6),sadd(6),ants,in,tpeek
	character*80 error
	character*20 shex(6)
	character*40 contents
	character*3 unit
	real analog
	logical select
	integer inorout(6),ant
	real atod
	integer nbytes(6)
	character*1 form(2,6),type
	character*2 direc(2) /'-O','-I'/
	character*16 explain(6)
	integer negfill(4) /'ffffff00'x,'ffff0000'x,'ff000000'x,0/
	integer negbit(4)  /'80'x,'8000'x,'800000'x,'80000000'x/
	call output(version)
	call keyini
	call keyants(ants,0,error)
	if(error(1:3).eq.'Too') goto 99
	call keya('hex',shex(1),'0')
	call keya('hex',shex(2),'0')
	call keya('hex',shex(3),'0')
	call keya('hex',shex(4),'0')
	call keya('hex',shex(5),'0')
	call keya('hex',shex(6),'0')
	call keyr('secs',secs,0.)
	call keyfin
	do i=1,6
	  if(shex(i)(1:1).ne.'0') then
		if(shex(i)(1:1).ne.'-') then
		  read(shex(i),'(z)') slot(i)
		  if(slot(i).ge.'1000'x) read(shex(i),'(i)') slot(i)
		  sadd(i) = slot(i)
		  inorout(i) = 2
		else
		  read(shex(i)(2:),'(z)') slot(i)
		  sadd(i) = slot(i)-256
		  inorout(i) = 1
		end if
	  end if
	end do

	call read_definition_file(6,slot,nbytes,explain,form,error)
	if (error(1:2).ne.'OK') goto 99

	call mbusopen(error)
	if(error(1:2).ne.'OK') goto 99
	
	do ant=0,nants
c	 if(select(ant,ants)) call atodcal(ant,error)
c	  if(error(1:2).ne.'OK') goto 99
	end do	

	type *,' ant Tadd    label             hex         contents   '
	type *,'--------------------------------------------------'
1	do i=1,6
	 if(slot(i).gt.0) then
	   do ant=0,nants
	     if(select(ant,ants)) then
		type = form(inorout(i),i)
		if(type.ne.'a') then
		  in = tpeek(ant,sadd(i),nbytes(i),error)
		  if(error(1:2).ne.'OK') type *,slot(i),error(1:40)
		end if
	        if(type.eq.'a') then
		  analog = ATOD(ANT,sadd(i),in,unit,explain(i),ERROR)
		  if(error(1:2).ne.'OK') type *,error
		  write(contents,'(F9.3,x,a3)') analog,unit
	        else if (type.eq.'-') then
		   contents = 'not allowed'
		else if (type.eq.'U') then
		   write(contents,'(I8)') in
		else if (type.eq.'V') then
		   j = in .and. 'fff'x
		   if ((in.and.'1000'x).gt.0)  j = -j
		   write(contents,'(I8)') j
		else if (type.eq.'P') then
		   write(contents,'(F9.5)') in / 65536.
		else if (type.eq.'F') then
		   if ((in.and.negbit(nbytes(i))).ne.0) in=in.or.negfill(nbytes(i))
	 	   write(contents,'(F11.5)') in * 4.76837158203e-4
		else if (type.eq.'2') then
		   if ((in.and.negbit(nbytes(i))).ne.0) in=in.or.negfill(nbytes(i))
		   write(contents,'(I8)') in
		else if (type.eq.'B') then
		   contents = ' '
		   mask = 2**(8*nbytes(i)-1)
		   jj = 1
		   do j=1,nbytes(i)*8
		     if((mask.and.in) .ne.0) then
		        contents(jj:jj) = '1'
		     else
			contents(jj:jj) = '0'
		     end if
		     if(mod(j,8).eq.0) then
			jj = jj+1
			contents(jj:jj) = ' '
		     end if
		     jj = jj+1
		     mask = mask/2
		   end do
		end if
	      if(type.ne.'a') then
	       type 100,ant,slot(i),direc(inorout(i)),explain(i)(:len1(explain(i))),in,contents
100	       format(x,i2,' 0x',z3.3,a,' (',a,') 0x',z8.8,' =',a)
	      else
	       type 101,ant,slot(i),direc(inorout(i)),explain(i)(:len1(explain(i))),in,contents
101	       format(x,i2,'  ',i4.4,a,' (',a,') 0x',z8.8,' =',a)
	      end if
	     call tpoke(ant,'13e'x+256,1,0,error)
	     call tpoke(ant,'13f'x+256,1,0,error)
	     end if
	   end do
	 end if
	end do
	type *
	if (secs.eq.0.) stop
	call wait_ticks(int(secs*100.))
	goto 1

99	if (error(1:2).ne.'OK') call bug('f',error)
	end

	subroutine read_definition_file(n,slot,nbytes,explain,form,error)
	integer n,slot(n),nbytes(n)
	character*16 explain(n),expl
	character*40 name
	character*80 error
	character*1 form(2,n)
	integer add,nb
	character*1 fin,fout
	character*80 line
	
	call filepath('HATTAB','tdump','ascii',name)
	open(1,file=name,status='old',form='formatted',iostat=is,err=99)
	read(1,*)
	read(1,*)

	do i=1,n
	  if(slot(i).ge.1000) then
	    nbytes(i) = 2
	    form(1,i) = 'a'
	    form(2,i) = 'a'
	  else
	    nbytes(i) = 2
	    explain(i) = 'unknown'
	    form(1,i) = 'u'
	    form(2,i) = 'u'
	  end if
	end do

1	read(1,'(a)',end=10) line
	read(line,'(z3,i6,7x,a16,8x,a1,x,a1)',iostat=is,err=98)
	1	  add,nb,expl,fin,fout
	 do i=1,n
	   if(abs(slot(i)).eq.add) then
		nbytes(i) = nb
		explain(i) = expl
		form(1,i) = fin
		form(2,i) = fout
	  end if		
	 end do
	goto 1
10	error='OK'
	return

98	write(error,'(a,i3)') 'can''t read tdump.ascii:',is
	call perror('error reading tdump.ascii')
	return 
99	write(error,'(a,i3)') 'can''t open tdump.ascii:',is
	call perror('error opening tdump.ascii')
	return
	end
