c*
	PROGRAM VLACALS
c& wh
c= VLACALS convert the VLA calibrators file into a list
c: observing
c+
C	VLACALS processes the vla calibrators list gotten from
c	vlais@zia.nrao.edu into a list that can be
c	read by CATALOG.
c
c@ input
c	Input file.  (default:vlacals)
c
c@ output
c	Output file. (default:vlapared)
c
c@ flux
c	Threshold flux (janskys) at 2 cm.
c--
c  jan 92 wh
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='VLACALS (version 1.0 17-jan-92)')
	character*80 line
	character*50 in,out
	character*12 alternate,name
	character*15 rastring,dcstring
	real flux

	call output(version)
	call keyini
	call keya('input',in,'vlacals')
	call keya('output',out,'vlapared')
	call keyr('flux',flux,2.)
	call keyfin

	open(1,file=in,status='old',form='formatted',err=98)
	open(2,file=out,status='unknown',form='formatted',err=99)

10	read(1,'(a)',end=90) line
	  if(line(1:2).eq.'--' .or. line(1:2).eq.'==') goto 10
	  if(line(1:3).eq.'IAU' .or. line(1:4).eq.'BAND') goto 10
	  if(line(13:16).eq.'2000') then
	    rastring = line(21:33)
	    rastring(index(rastring,'h'):index(rastring,'h')) = ':'
	    rastring(index(rastring,'m'):index(rastring,'m')) = ':'
	    dcstring = line(36:48)
	    dcstring(index(dcstring,'d'):index(dcstring,'d')) = ':'
	    dcstring(index(dcstring,''''):index(dcstring,'''')) = ':'
	    name = line(:8)
	    alternate = ' '
	  else if (line(:9) .eq. 'ALTERNATE') then
	    alternate = line(18:28)
	  else if (line(3:5).eq.'2cm') then
	    read(line,'(25x,f5.2)') flux2
	    if (flux2.ge.flux) then
		type 95,name,rastring,dcstring,flux2
95		format(x,a,x,a,x,a,f5.2)
		if(alternate.ne.' ') type 95,alternate,rastring,dcstring,flux2
	    end if
	  end if
	goto 10

90	type *,'end of input file'		
	stop
c	 CAT,SNAME,HRA,MRA,SRA,DDEC,MDEC,SDEC,PRA,PDEC,V
c       (A1,3x,A6,3X,i2,X,I2,X,F4.1,I3,X,i2,X,F2.0,F6.3,F5.2,17X,F3.1)


98	type *,'Can''t open '//in
	stop
99	type *,'Can''t open '//out
	stop
	end
