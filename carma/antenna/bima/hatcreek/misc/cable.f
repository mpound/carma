c*
	program cable
c
c=  CABLE	- measure the lengths of the IF cables
c: observing
c& wh
c+
c	CABLE measures the length of the IF cables by stepping the
c	frequency with the synthesizer in the range 1100-1260 MHz
c	and using the linelength equipment to measure the phase
c	of the returning signal.  The phases are extended through
c	2 pi and then a straight line fit is done to the equation:
c	Phase(turns) = A x freq(GHz) + B; Cable length(ns) =  A/2.
c   Results are written to common variable "cable" and to file
c	HATLOG/cable.
c	
c@ freqs
c	Frequency range and step size (MHz). The range
c	of permissible values are 1100 -> 1260 MHz.
c	Defaults are 1180,1220,1 which is used to get
c	a standard cable length for cable length tracking.
c
c@ itime
c	Integration time in seconds.  Default is 1.
c
c@ ants
c	Antenna flag; the correlator setup routines are run for
c	these antennas.
c
c@ log
c	If a log file name is specified, data will be written to
c	file(s) "log.x" where x=ant.  Default - no log file written.
c	Records have a format of:
c	 freq  raw-turns extended-turns amplitude error-turns
c
c@ save
c	"y" or "s" to copy measured values from common to file
c		$HATTAB/cable following the measurement (default)
c	"restore" copies values from $HATTAB/cable to common, skips
c		the measurement
c	"n" is to make the measurement and write the results
c		to common, but not save them to the file 
c--
c  2aug93  wh
c  20sep93 wh  no more caboff
c  12oct93 wh  save/restore
c  jan96 wh   cables arent all same length anymore
c  mar96 rp   restore 1360 nsec as estimated length
c  nov96 rp	  initial estimate of cable length (needed to extend the phase
c			  past 1 turn) is taken from common, not from variable clns
c  may99 wh   new linelength hardware
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='CABLE (version 2.0 17-may-99)')
	include '../inc/constants.inc'
	include '../inc/exports.inc'
	
	integer ant
	integer ants
	real*8 fstart,fstop,fincr,fmhz,f
	real*8 fcal /0.d0/
	character*80 error
c  --   estimated cable length in nanosecs; as of 7 Nov 96, common value of
c		cable is used as the initial guess
	real*8 p1(nants),psave(nants),cc(nants),ss(nants)
	real*4	db	/13./
	integer wflag /0/
	real*8 amp(nants)
	integer antprime
	real*4 inttime
	integer intticks
	character*4 antc
	character*40 log
	parameter (nmax=4000)
	real*4 sig(nmax),p(nmax,nants),pext(nmax,nants)
	real hpdb
	double precision cable(nants),refmhz,fq(nmax)
	logical select
	character save*12

c  --	input the parameters
	call keyini
	call keyd('freqs',fstart,1180.d0)
	call keyd('freqs',fstop,1220.d0)
	call keyd('freqs',fincr,1.d0)
	call keyr('itime',inttime,1)
	call keyants(ants,0,error)
	  if(error(1:2).ne.'OK') goto 99
	call keya('log',log,' ')
	call keya('save',save,'S')
	call ucase(save)
	call keyfin
	if (save(1:1).eq.'Y') save = 'S'
	do i=nants,1,-1
	  if (select(i,ants)) antprime = i
	end do
	intticks = 100*inttime + 100

	call mbusopen(error)
	  if (error(1:2).ne.'OK') goto 99

C  --	announce and lock up resources to be used--
c	CALL ANNOUNCE(version,ERROR)
c	IF(ERROR(1:2).NE.'OK') goto 99

c  --	if save=restore, just restore the cable values
	if(save(1:7).eq.'RESTORE') then
	  call cablesave(save,error)
	  go to 99
	end if

	call comgetd('CABLE',cable,nants,error)
	 if(error(1:2).ne.'OK') goto 99
	call comgetd('REFMHZ',refmhz,1,error)
	 if(error(1:2).ne.'OK') goto 99


c  --	put a header on the printouts
	type *,' MHz         1  phase(turns) 3      4      5      6      7'//
	1	'      8      9     A'

	ifreq = 0
	do fmhz = fstart,fstop,fincr
		ifreq = ifreq+1
		fq(ifreq) = fmhz/1000.d0
		sig(ifreq) = 1.

	
		f = fmhz
		call hp8662_set(f,13.,error)
c		call racal_set(f,19.,error) 
		if (error(1:2).ne.'OK') goto 99
		call wait_ticks(50)

		call computr('INTTIME',inttime,1,error)
		call wait_ticks(intticks)
		call comgetd('CABPHASE',p1,12,error)

c -- temporary: p1(7) should be 0.003 if NI card is working right; ignore point in fit otherwise
		if (abs(p1(7)-0.003).gt.0.00002) sig(ifreq)=100.

		do ant=1,nants
		 if(select(ant,ants)) then
		  p(ifreq,ant) = p1(ant)
		 else
		  p(ifreq,ant) = 0.
		 end if
		end do

		do ant=1,nants
		 if(select(ant,ants)) then
		  if (fmhz.eq.fstart) then
		      pext(ifreq,ant) = p(ifreq,ant)
		  else
c  --		   estimated phase step
		   phstep = 2.*cable(ant)*fincr/1000.
		   guess = psave(ant)+phstep
		   nguess = guess
		   if ((amod(guess,1.)-p(ifreq,ant)).gt.0.5) 
	1					nguess=nguess+1
		   if ((amod(guess,1.)-p(ifreq,ant)).lt.-.5)
	1					nguess=nguess-1
		   pext(ifreq,ant)=p(ifreq,ant)+nguess
		  endif
		  psave(ant)=pext(ifreq,ant)
		 end if
		end do

		type '(f10.4,10f8.4)', fmhz,(p(ifreq,i),i=1,10)
	enddo

c  --	get initial phase at 1200mhz (fill in cabphase)
c	call cablephase(ants,error)

	type *,'number of points =',ifreq

	mwt=0
	do ant=1,nants
	 if(select(ant,ants)) then
	   call fit(fq,pext(1,ant),ifreq,sig,mwt,a,b,siga,sigb,chi2,q)
		! numerical recipes least squares fit to straight line
	   cable(ant) = b/2.
	   type 100,ant, cable(ant), sigb/2.
100	   format('Ant ',i2,' has cable length =',f10.4,' (',f6.4,') nsec')

c  --	build a file if a filename was provided (note: use access=append so
c	that data won't be lost in case file already exists)
c
	   if (log(1:1).ne.' ') then
	    write(antc,'(''.'',z1)') ant
	    open (2,file=log(:len1(log))//antc,access='append',form='formatted')

	    do i=1,ifreq
		pfit = a + b*fq(i)
		presid = pfit-pext(i,ant)
		pline = pext(1,ant) + 
	1	 (fq(i)-fq(1))/(fq(ifreq)-fq(1))*(pext(ifreq,ant)-pext(1,ant))
		pflat = pline - pext(i,ant)
		write (2,102) 1000.*fq(i),p(i,ant),pext(i,ant),pfit,
	1					presid,pflat
102		format(f10.5,5f12.6)
	    enddo
	    close(2)
	   end if
	  end if
	end do
	call computd('CABLE',cable,nants,error)
	 if (error(1:2).ne.'OK') goto 99

c  --	if save=s, save the cable values to $HATTAB/cable
	if(save(1:1).eq.'S') call cablesave(save,error)

99	if (error(1:2).ne.'OK') call bug('f',error)
	end

c  --	----------------------------------------------------
c  --	Subroutine to save/restore the cable values to file
c  if save="save", copies values from common to file $HATTAB/cable
c  if save="restore", copies values from file to common
c
	SUBROUTINE CABLESAVE(ACTION,ERROR)
	include '../inc/constants.inc'
	character action*12, error*80,cablepath*100, date*24
	double precision phases(nants), cable(nants)

	call filepath('HATTAB','cable',' ',cablepath)
	if(action(1:7).eq.'RESTORE') then
	  open(11,file=cablepath(1:len1(cablepath)),status='OLD',
	1	iostat=is)
	  if(iostat.ne.0) then
	     write(error,'(a,a,a,i3,a)')'Unable to open ',
	1	cablepath(1:len1(cablepath)),' (error:',is,')'
	     return
	  end if
	  read(11,*)
	  read(11,*) date
	  type *,'Restoring CABLE and CABPHASE to values at time ',date
	  read(11,fmt=100,iostat=is) (cable(i),i=1,nants)
	  read(11,fmt=101,iostat=iss) (phases(i),i=1,nants)
	  if((is.ne.0).or.(iss.ne.0)) then
		write(error,'(a,2i3,a)')'Unable to read cable file (error:',is,iss,')'
		call perror('Error=')
		return
	  end if
	  call computd('CABPHASE',phases,nants,error)
	   if(error(1:2).ne.'OK') return
	  call computd('CABLE',cable,nants,error)
	else if(action(1:1).eq.'S') then
	  call comgetd('CABPHASE',phases,nants,error)
	   if(error(1:2).ne.'OK') return
	  call comgetd('CABLE',cable,nants,error)
	   if(error(1:2).ne.'OK') return
	  open(11,file=cablepath(1:len1(cablepath)),status='UNKNOWN',
	1	iostat=is)
	  if(iostat.ne.0) then
	     write(error,'(a,a,a,i3,a)')'Unable to open ',
	1	cablepath(1:len1(cablepath)),' (error:',is,')'
	     call perror('Error=')
	     return
	  end if
	  write(11,*)'CABLE contains: TIME, CABLE, CABPHASE (written by CABLE)'
	  call fdate(date)
	  write(11,*)'''', date,''''
	  write(11,fmt=100,iostat=is) (cable(i),i=1,nants)
	  write(11,fmt=101,iostat=iss) (phases(i),i=1,nants)
	  if((is.ne.0).or.(iss.ne.0)) then
		write(error,'(a,2i3,a)')'Unable to read cable file (error:',is,iss,')'
	        call perror('Error=')
		return
	  end if
	end if
	close(11)
	return
100	format(<nants>(f11.5))
101	format(<nants>(f10.6))
	end

c  --	this is the software tools least square fit routine
      SUBROUTINE FIT(X,Y,NDATA,SIG,MWT,A,B,SIGA,SIGB,CHI2,Q)
	double precision x,sx,sy,st2,ss,sxoss,t
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA)
      SX=0.
      SY=0.
      ST2=0.
      B=0.
      IF(MWT.NE.0) THEN
        SS=0.
        DO 11 I=1,NDATA
          WT=1./(SIG(I)**2)
          SS=SS+WT
          SX=SX+X(I)*WT
          SY=SY+Y(I)*WT
11      CONTINUE
      ELSE
        DO 12 I=1,NDATA
          SX=SX+X(I)
          SY=SY+Y(I)
12      CONTINUE
        SS=FLOAT(NDATA)
      ENDIF
      SXOSS=SX/SS
      IF(MWT.NE.0) THEN
        DO 13 I=1,NDATA
          T=(X(I)-SXOSS)/SIG(I)
          ST2=ST2+T*T
          B=B+T*Y(I)/SIG(I)
13      CONTINUE
      ELSE
        DO 14 I=1,NDATA
          T=X(I)-SXOSS
          ST2=ST2+T*T
          B=B+T*Y(I)
14      CONTINUE
      ENDIF
      B=B/ST2
      A=(SY-SX*B)/SS
      SIGA=SQRT((1.+SX*SX/(SS*ST2))/SS)
      SIGB=SQRT(1./ST2)
      CHI2=0.
      IF(MWT.EQ.0) THEN
        DO 15 I=1,NDATA
          CHI2=CHI2+(Y(I)-A-B*X(I))**2
15      CONTINUE
        Q=1.
        SIGDAT=SQRT(CHI2/(NDATA-2))
        SIGA=SIGA*SIGDAT
        SIGB=SIGB*SIGDAT
      ELSE
        DO 16 I=1,NDATA
          CHI2=CHI2+((Y(I)-A-B*X(I))/SIG(I))**2
16      CONTINUE
        Q=GAMMQ(0.5*(NDATA-2),0.5*CHI2)
      ENDIF
      RETURN
      END

