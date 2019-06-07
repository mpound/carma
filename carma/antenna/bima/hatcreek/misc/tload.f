c*
	PROGRAM TLOAD
c& wh
c= TLOAD load selected telemetry registers
c: test
c+
c	TLOAD loads telemetry registers from a file of
c	the form:    taddress, tcontents on each
c	line.  taddress and tcontents are in hex.
c	Any line in the file beginning with ! is ignored.
c	Look in the file: $HATTAB/tdump.ascii for possible
c	telemetry addresses.
c
c@ ants
c	Antenna selection flag:
c	 * means use all nants antennas
c	 xyz means use antennas x y and z
c	 *-xyz means use all antennas except x y and z
c
c@ from
c	Name of the file containing the stuff to load.
c--
c	aug 92 wh
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='TLOAD (version 1.0 19-aug-92)')
	integer ants,tpeek,ant
	character*80 error,line
	character*40 filename
	logical select
	integer inbits

	integer tadd(50),outbits(50)
	integer nbytes(50)
	character*1 form(2,50)
	character*16 explain(50)

	call output(version)
	call keyini
	call keyants(ants,0,error)
	if(error(1:3).eq.'Too') goto 99
	call keya('from',filename,' ')
	call keyfin

	open(1,file=filename,iostat=is,status='old')
	if (is.ne.0) then
	  type *,'Can''t open ',filename,' (',is,')'
	  stop
	end if
	call mbusopen(error)
	type *,'mbusopen..',error(1:50)

	type *,' ant Tadd     explain     hex  [from=',filename(:len1(filename)),']'
	type *,'-----------------------------------------------'
	n=1
1	read(1,'(a)',iostat=iss) line
	 if(iss.lt.0) goto 2
	 if(line(1:1).eq.'!') goto 1
	 read(line,'(z8,z8)',iostat=is) tadd(n),outbits(n)
	  if(is.ne.0) then
	   write(error,'(a,i3,a)') 'Error reading file ',filename,' (',is,')'
	   goto 99
	  end if
	 n = n+1
	goto 1
2	n = n-1

	call read_definition_file(n,tadd,nbytes,explain,form,error)
	if (error(1:2).ne.'OK') goto 99

	do i=1,n
	 do ant=1,9
	  if(select(ant,ants)) then
	    call tpoke(ant,tadd(i),nbytes(i),outbits(i),error)
	     if(error(1:2).ne.'OK') goto 99
	    inbits = tpeek(ant,tadd(i)-256,nbytes(i),error)
	     if(error(1:2).ne.'OK') goto 99
	    type '(i2,'' 0x'',z4.4,x,a,'' 0x'',z4.4)',ant,tadd(i),explain(i),inbits
	  end if
	 end do
	end do

99	type *,error
	end

c  --	lookup further information about each telemetry address used --
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
	  nbytes(i) = 0
	  explain(i) = 'unknown'
	  form(1,i) = '-'
	  form(2,i) = '-'
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

