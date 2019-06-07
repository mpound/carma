c*
	PROGRAM JOGGER
c& wh
c= JOGGER - offsets the antenna position using pgplot cursor.
c: observing
c+
c	JOGGER offsets the antenna using pgplot cursor control; 
c	it is most often used to match up the optical and radio
c	pointing using the transmitter.
c
c	The settings of dazim and delev are controlled by marking
c	points on the graph using the mouse. Selecting a point outside
c	the grid does not actually select the point but causes the 
c	grid scale to be doubled. Typing "e" will 
c	stop the program. (This program is often used in conjunction
c	with ants; note that ants sets dazim and delev to 0 when it
c	starts). (loadcom is often use to enter the measured pointing
c	offsets in the permanent database; note that control only reads
c	apc/epc on startup).
c
c@ source
c	Source name; "test" is default.
c@ ants
c	Antenna selection flag:
c	 one antenna only
c@ trakopt
c	Tracking option: 'N' to force elevation greater than 90 deg
c	'S' to force elevation less than 90 degrees. Default is to
c	observe sources with declination .gt. latitude with elevation
c	.gt. 90 degrees. 'K' adds the current dazimlas/delevlas to
c	the pointing offsets. This program requires + which is added to
c	the user options to set the antenna tracking mode.
c@ device
c	PGPLOT plotting device to be used for cursor window.
c--
c  feb 92 wh
c  add trakopt=k  jul 93 wh
c  fix keytrak -> keytrack  jun 97
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='JOGGER (version 1.3 7-jun-97)')
	include '../inc/constants.inc'
	include '../inc/ephem.inc'
	record /asource/ s
	character*25 pgdev
	integer pgstat,ants
	character*1 char,ans
	character*80 error
	character*50 line /' '/
	real x,y
	double precision dazim(nants),delev(nants)
	double precision dazimlas(nants),delevlas(nants)
	real azmin /-5./,azmax /5./,elmin /-5./,elmax /5./
	character*8 trakopt,source*10
	logical select

	call output(version)
	type *
	type *,'CONTROL must be running'
	type *,'Use TRAKOPT to select Horn or Tv option if desired'
	type *,'Move mouse on grid and push button '
	type *,'Type "e" to stop'

	call keyini
	call keya('source',source,'test')
	call keyants(ants,1,error)
	 if(error(1:2).ne.'OK') goto 99
	do i=1,nants
	  if(select(i,ants)) jant = i
	end do
	type *,'Jogging antenna ',jant
	call keytrack(ants,trakopt,'C+',error)
	 if(error(1:2).ne.'OK') goto 99
	call keya('device',pgdev,'/xwindow')
	call keyfin

c  --	set up pointing
	CALL GET_SOURCE(SOURCE,' ',0.d0,S,ERROR)
	 IF(error(1:2).NE.'OK') GOTO 99
	CALL BEGIN_POINT(ANTS,TRAKOPT,S,ERROR)
	 IF(error(1:2).NE.'OK') GOTO 99

c  --	handle trakopt=k
	call comgetd('DAZIMLAS',dazimlas,nants,error)
	 if(error(1:2).ne.'OK') goto 99
	call comgetd('DELEVLAS',delevlas,nants,error)
	 if(error(1:2).ne.'OK') goto 99
	call comgetd('DAZIM',dazim,nants,error)
	 if(error(1:2).ne.'OK') goto 99
	call comgetd('DELEV',delev,nants,error)
	 if(error(1:2).ne.'OK') goto 99
	do i=1,nants
	  if(select(i,ants)) then
	    if(index(trakopt,'K').gt.0) then
		dazim(i) = dazimlas(i)
		delev(i) = delevlas(i)
	    else
		dazim(i) = 0.
		delev(i) = 0.
	    end if
	  end if
	end do
	    
	 pgstat = pgbegin(0,pgdev,1,1)
	 call pgask(.false.)
	 call pgenv(azmin,azmax,elmin,elmax,0,2)
	 call pgmtext('B',2.,.5,.5,'Azim (arcmin)')
	 call pgmtext('L',2.,.5,.5,'Elev (arcmin)')

	char = ' '
	do while (char.ne.'e')
	  call pgcurse(xnew,ynew,char)
	  if (char.ne.'e') then
		x = xnew
		y = ynew
	  end if
	  if (x.le.azmin  .or. x.ge.azmax .or. y.le.elmin .or. y.ge.elmax) then
	    azmin = azmin *2.
	    azmax = azmax *2.
	    elmin = elmin *2.
	    elmax = elmax *2.
	    call pgenv(azmin,azmax,elmin,elmax,0,2)
	  else
	    call pgrect(x-.05,x+.05,y-.05,y+.05)
	    if(index(trakopt,'K').gt.0) then
	      dazim(jant) = x + dazimlas(jant)
	      delev(jant) = y + delevlas(jant)
	    else
	      dazim(jant) = x
	      delev(jant) = y
	    end if
	    call computd('DAZIM',dazim,nants,error)
	     if(error(1:2).ne.'OK') type *,error
	    call computd('DELEV',delev,nants,error)
	     if(error(1:2).ne.'OK') type *,error
	    call pgsci(0)
	    call pgmtext('T',1.,.3,0.,line)
	    call pgsci(1)
	    write(line,'(a,f8.2,a,f8.2)')'dAZ:',dazim(jant),' dEL:',delev(jant)
	    call pgmtext('T',1.,.3,0.,line)
	  end if
	end do
	  call pgend
	  type *,line
	  type *,'Should I update DAZIMLAS/DELEVLAS?<y/n>'
	  accept 998,ans
998	  format(a)
	  if(ans.eq.'y') then
		type *,'DAZIMLAS(',jant,') =',dazim(jant)
		type *,'DELEVLAS(',jant,') =',delev(jant)
		dazimlas(jant) = dazim(jant)
		delevlas(jant) = delev(jant)
		call computd('DAZIMLAS',dazimlas,nants,error)
		 if(error(1:2).ne.'OK') goto 99
		call computd('DELEVLAS',delevlas,nants,error)
	  end if
99	type *,error
	end
