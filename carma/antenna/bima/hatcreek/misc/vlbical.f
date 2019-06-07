c***********************************************************************
	program VLBICAL
	implicit none
c
c= VLBICAL - compute expected Ta/Tsys for phased-up vlbi scan
c& rp
c: uv analysis
c+
c  
c@ visp
c   "preobs" uv data file; only the source name and gains are
c	accessed
c
c@ vism
c	"midobs" uv data file; only the vlbiwgts and system temps
c	are accessed
c
c@ log
c	The list output file name.  The default is the terminal.
c
c@ window
c	Correlator window to be used for system temperature.  This
c	should match the frequency band actually written onto the
c	VLBI tape.	
c--
c
c  History:
c    mchw  14oct94 on-line task to fit phaselo1 and phaselo2.
c    mchw  24oct94 Add on old values of phaselo1 and phaselo2.
c    rp adapted phasem code to vlbical
c	 rp    7 oct 96 change dimensions to accomodate 18 wideband tsys values
c		   wideal(ant)=ta/tsys**2 instead of (ta/tsys)**2
c------------------------------------------------------------------------

c	include '$MIRINC/maxdim.h'
	integer MAXANT
	integer MAXWIDE
	integer MAXCHAN
	parameter (MAXWIDE=18)
	parameter (MAXANT=12)
	parameter (MAXCHAN=2048)

	character version*(*),visp*64,vism*64,log*64,line*80, error*80, type*1
	parameter(version='version 1.1 27-Apr-95')
	character*80 abuf,snamepre*20,snamemid*20
	integer tvis,nants,nsols,item,length,iants,nspec,nread,iant,nwide
	real wgt(MAXANT)
	double precision preamble(4)
	integer nreadmax /MAXCHAN/
	real amp(MAXANT),ampl(MAXANT)
	real sumamps,sumtsys,ta,tsys
	integer iwindow /2/
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	
	logical updated
	double precision interval
	integer refant,iostat,j
	real phi(MAXANT),usb(MAXANT),lsb(MAXANT)
	real phaselo1(MAXANT),phaselo2(MAXANT)
	real systemps(MAXANT*MAXWIDE)
	real wideal(MAXANT),widealsum
c
c  Externals.
c
	character itoaf*8
	integer uvscan
	integer len1
c
c  Get the input parameters.
c
	call output('VLBICAL: '//version)
	call keyini
	call keyf('visp',visp,' ')
	call keyf('vism',vism,' ')
	call keya('log',log,' ')
	call keyi('window',iwindow,2)
	call keyfin
c
c  Check that the inputs make sense.
c
	if(visp.eq.' ') call bug('f','Input visibility file is missing')
c
c  Open the output log file.
c
	call LogOpen(log,'q')
	call LogWrit('Gains for '//visp)
c
c  get the gains from the PREOBS visibility file
c
	call uvopen(tvis,visp,'old')
	call uvread(tvis,preamble,data,flags,nreadmax,nread)
	call rdhdd(tvis,'interval',interval,0.d0)
	call rdhdi(tvis,'ngains',nants,0)
	call rdhdi(tvis,'nsols',nSols,0)
	call output('Number of gains: '//itoaf(nants))
	call output('Number of solution intervals: '//itoaf(nSols))
	if(nants*nSols.eq.0) call bug('f','No gains to fit')
	call haccess(tvis,item,'gains','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening gains item')
	  call bugno('f',iostat)
	endif
	call getvis(item,refant,nants,nsols,phi,amp)
	call hdaccess(item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing output gains item')
	  call bugno('f',iostat)
	endif
	call uvgetvra(tvis,'source',snamepre)
	type *,snamepre
	call uvclose(tvis)
c
c  get the system temps from the MIDOBS visibility file
c
	call uvopen(tvis,vism,'old')
	call uvread(tvis,preamble,data,flags,nreadmax,nread)
	call uvrdvri(tvis,'nwide',nwide,0)
	if (nwide.ne.0) then
		call uvgetvrr(tvis,'wsystemp',systemps,nants*nwide)
	else
		call bug('f','nwide=0, cannot get systemps')
	endif
	call uvgetvra(tvis,'source',snamemid)
c	type *,snamemid
	type *, systemps
c
c  get the weights from the MIDOBS visibility file

	call haccess(tvis,item,'vlbiwgts','read',iostat)
	if(iostat.ne.0)then
		call bug('w','Error opening vlbiwgts item')
	else
		do while (iostat.eq.0)
			call hreada(item,abuf,iostat)
			if (iostat.eq.0) then
				read (abuf,*) iant,wgt(iant)
c				type *, iant,wgt(iant)
			endif
		enddo	
		call hdaccess(item,iostat)
		if(iostat.ne.0)then
			call bug('w','Error closing output gains item')
			call bugno('f',iostat)
		endif
	endif
	call uvclose(tvis)
c
c  compute Ta/Tsys

	sumamps = 0.
	sumtsys = 0.
	widealsum=0.
	do iant=1,nants
		if (wgt(iant).gt.0) then
			ampl(iant) = sqrt(wgt(iant))/amp(iant)
			sumamps = sumamps + sqrt(wgt(iant))/amp(iant)
			tsys = systemps(MAXANT*(iwindow+1)+iant)
			sumtsys = sumtsys + wgt(iant)*tsys
			ta = 1./(amp(iant)**2)
			wideal(iant) = ta/(tsys**2)
			widealsum = widealsum + wideal(iant)
		else
			wideal(iant)=0.
			ampl(iant) = 0.
		endif
	enddo

	type *
	type *, ' ant     amp     Ta   Tsys    Ta/Tsys   wgt    idealwgt'
	do iant=1,nants
		wideal(iant) = wideal(iant)/widealsum
		if (wgt(iant).gt.0.001) then
			tsys = systemps(MAXANT*(iwindow+1)+iant)
			ta = 1./(amp(iant)**2)
			type 100, iant,amp(iant),ta,tsys,ta/tsys,wgt(iant),wideal(iant)
100			format(i4,f9.2,f8.3,f7.0,e10.2,f7.3,3x,'(',f5.3,')')
		endif
	enddo
c
c  print the weighted amplitudes

	type *
	type 101, (ampl(iant),iant=1,9) 
101	format('AMPS',3x,9f7.3)
	j = len1(visp)
	type 102, visp(j-8:j-2),sumamps*sumamps,sumtsys,sumamps*sumamps/sumtsys,snamepre,snamemid
102	format('SUM ',a,2x,f8.3,f7.0,e10.2,3x,a,a)
	


c
c	call uvgetvri(tvis,'nants',iants,1)
c
	call LogClose
c	call hclose(tvis,iostat)
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine getvis(item,refant,nants,nsols,phi,aveamp)
	implicit none
	integer item,nants,nsols
	integer MAXANT
	parameter (MAXANT=12)
	integer refant,iostat,i,j,k,offset
	character line*80
	integer header(2),nants,nsols,item,offset
	double precision dtime
	complex gains(MAXANT),ref, avgains(MAXANT)
	real SumAmp(MAXANT),AveAmp(MAXANT),RmsAmp(MAXANT),SumPhi(MAXANT)
	real SumWts(MAXANT),Amp(MAXANT),Phi(MAXANT),RmsPhi(MAXANT)
	real AvePhi(MAXANT)
c
	offset = 0
	call hreadi(item,header,offset,8,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error reading gains item')
	  call bugno('f',iostat)
	endif
	offset = 8
c
c  Initialize some statistics.
c
	do j=1,nants
	  avgains(j) = (0.,0.)
	  SumAmp(j) = 0.
	  RmsAmp(j) = 0.
	  SumPhi(j) = 0.
	  RmsPhi(j) = 0.
	  SumWts(j) = 0.
	enddo
c
c  Read the gains and write out amplitude and phase relative to reference
c  antenna. Accumulate statistics.
c
	if(refant.le.0.or.refant.gt.nants)refant=0
	do k=1,nsols
	  call hreadd(item,dtime,offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0) call hreadr(item,gains,offset,8*nants,
     *								iostat)
	  if(iostat.ne.0)then
	    call bug('w','I/O error while reading gains')
	    call bugno('f',iostat)
	  endif
	  if(refant.ne.0)ref = gains(refant)
	  do j=1,nants
	    if(refant.ne.0.and.abs(gains(refant)).ne.0.)then
	      gains(j) = gains(j)/ref
	    endif
	    avgains(j) = avgains(j) + gains(j)
	    call amphase(gains(j),amp(j),phi(j))
	    SumAmp(j) = SumAmp(j) + amp(j)
	    SumPhi(j) = SumPhi(j) + phi(j)
	    RmsAmp(j) = RmsAmp(j) + amp(j)*amp(j)
	    RmsPhi(j) = RmsPhi(j) + phi(j)*phi(j)
	    SumWts(j) = SumWts(j) + 1.
	  enddo
c	  write(line,100) dtime-2444239.5d0,(amp(i),nint(phi(i)),
c     *							i=1,nchan)
c100  	  format(f13.7,' ',6(f8.3,i5))
c	  length = 13 + 1 + nchan*(8+5)
c	  call LogWrit(line(1:length))
	  offset = offset + 8*nants
    	enddo
c
c  Write out some statistics for the gains.
c
	do j=1,nants
	  if(SumWts(j).ne.0.)then
	    avgains(j) = avgains(j)/SumWts(j)
	    call amphase(avgains(j),amp(j),phi(j))
	    AveAmp(j) = SumAmp(j)/SumWts(j)
	    AvePhi(j) = SumPhi(j)/SumWts(j)
	    RmsAmp(j) = sqrt(max(RmsAmp(j)/SumWts(j)-AveAmp(j)**2,0.))
	    RmsPhi(j) = sqrt(max(RmsPhi(j)/SumWts(j)-AvePhi(j)**2,0.))
	  endif
	enddo
c
	do j=1,nants,6
	  k = min(j+5,nants)
	  write(line,110) (Amp(i),nint(Phi(i)),i=j,k)
110  	  format(3x,'Vector Average',3x,6(f6.3,i4))
c	  call LogWrit(line)
	  write(line,115) (AveAmp(i),nint(AvePhi(i)),i=j,k)
115  	  format(3x,'Scalar Average',3x,6(f6.3,i4))
c	  call LogWrit(line)
	  write(line,120) (RmsAmp(i),nint(RmsPhi(i)),i=j,k)
120  	  format(5x,'Scalar Rms',5x,6(f6.3,i4))
c	  call LogWrit(line)

	enddo
	end
