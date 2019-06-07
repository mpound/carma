c***********************************************************************
	program PHASEM
	implicit none
c
c= PHASEM - Phase the array using the gains derived from selfcal.
c& mchw
c: uv analysis
c+
c	PHASEM phases the array using the gains derived from selfcal
c	for MIRIAD uv-data. The array can be phased for both sidebands
c	of LO1 by running the procedure PHASECAL which makes items
c	lsbgains and gains in the uv-data.
c	The mean and rms of the gains are also calculated.
c@ vis
c	The input uv-data. Only the gains item needs to be
c	present in the uv-data. If the lsbgains item is also present then
c	phaselo1=(usb+lsb)/2, phaselo2=(usb-lsb)/2, else phaselo2 = 0.
c
c@ log
c	The list output file name.  The default is the terminal.
c
c@ refant
c	The phase of this antenna is set to 0. The antenna gains are 
c	relative to the reference antenna.
c	The default is determined by selfcal.
c--
c
c  History:
c    mchw  14oct94 on-line task to fit phaselo1 and phaselo2.
c    mchw  24oct94 Add on old values of phaselo1 and phaselo2.
c    mchw  26jul95 Format for 12 antennas.
c    mchw  03apr98 add phase for antennas 789A
c------------------------------------------------------------------------
	integer MAXANT
	parameter (MAXANT=12)
	character version*(*),vis*64,log*64,line*80, error*80, type*1
	parameter(version='version with add-on phases 03-APR-98')
	integer tvis,nants,nsols,item,length
	logical updated
	double precision interval
	integer refant,iostat,j
	real phi(MAXANT),usb(MAXANT),lsb(MAXANT)
	real phaselo1(MAXANT),phaselo2(MAXANT), off(MAXANT)
	data off/0.,0.,0.,0.,0.,0.,180.,180.,180.,180.,0.,0./
c
c  Externals.
c
	character itoaf*8
	integer uvscan
c
c  Get the input parameters.
c
	call output('PHASEM: '//version)
	call keyini
	call keyf('vis',vis,' ')
	call keya('log',log,' ')
	call keyi('refant',refant,0)
	call keyfin
c
c  Check that the inputs make sense.
c
	if(vis.eq.' ') call bug('f','Input visibility file is missing')
c
c  Open the output log file.
c
	call LogOpen(log,'q')
	call LogWrit('Gains for '//vis)
c
c  Open the uvdata file containing the gain solutions.
c		We do not use uvopen since vartable may not be present.
c 
c	call hopen(tvis,vis,'old',iostat)
c	  if(iostat.ne.0)then
c	    call bug('w','Error opening input file '//vis)
c	    call bugno('f',iostat)
c	  endif
	call uvopen(tvis,vis,'old')
c
c  Read some header information for the gains file.
c
	call rdhdd(tvis,'interval',interval,0.d0)
	call rdhdi(tvis,'ngains',nants,0)
	call rdhdi(tvis,'nsols',nSols,0)
	call output('Number of gains: '//itoaf(nants))
	call output('Number of solution intervals: '//itoaf(nSols))
	if(nants*nSols.eq.0) call bug('f','No gains to fit')
c
c  Look for the gains item
c
	call haccess(tvis,item,'gains','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening gains item')
	  call bugno('f',iostat)
	endif

	call getvis(item,refant,nants,nsols,phi)
c
c  Set usb phase.
c
	do j=1,nants
	  usb(j) = Phi(j)
	enddo
c
c  Close gains item
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing output gains item')
	  call bugno('f',iostat)
	endif
c
c  Look for the lsbgains item
c
	call haccess(tvis,item,'lsbgains','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening lsbgains item')
	else
	  call getvis(item,refant,nants,nsols,phi)
	  call hdaccess(item,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error closing output gains item')
	    call bugno('f',iostat)
	  endif
	endif
c
c  Set lsb phase.
c
	do j=1,nants
	  lsb(j) = Phi(j)
	enddo
c
c  Set phase LO1 & LO2
c
	if(uvscan(tvis,'time').eq.0)then
	call uvprobvr(tvis,'phaselo1',type,length,updated)
	if(type.eq.'r')then
	  call uvgetvrr(tvis,'phaselo1',phaselo1,nants)
	else
	  do j=1,nants
	    phaselo1(j) = 0.
	  enddo
	endif
c
	call uvprobvr(tvis,'phaselo2',type,length,updated)
	if(type.eq.'r')then
	  call uvgetvrr(tvis,'phaselo2',phaselo2,nants)
	else
	  do j=1,nants
	    phaselo2(j) = 0.
	  enddo
	endif
	endif
c
	  write(line,'(a,12f5.2)') 'OLD PHASELO1:  ',phaselo1
	  call LogWrit(line)
	  write(line,'(a,12f5.2)') 'OLD PHASELO2:  ',phaselo2
	  call LogWrit(line)
	do j=1,nants
	  phaselo1(j) = mod((phaselo1(j) + (usb(j)+lsb(j))/2./360.) ,1.)
     +	+ off(j)/360.
	  phaselo2(j) = mod((phaselo2(j) + (usb(j)-lsb(j))/2./360.) ,1.)
	enddo
	  write(line,'(a,12f5.2)') 'NEW PHASELO1:  ',phaselo1
	  call LogWrit(line)
	  write(line,'(a,12f5.2)') 'NEW PHASELO2:  ',phaselo2
	  call LogWrit(line)
	call computr('PHASELO1',phaselo1,nants,error)
	call computr('PHASELO2',phaselo2,nants,error)
c
	call LogClose
c	call hclose(tvis,iostat)
	call uvclose(tvis)
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine getvis(item,refant,nants,nsols,phi)
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
	  call LogWrit(line)
	  write(line,115) (AveAmp(i),nint(AvePhi(i)),i=j,k)
115  	  format(3x,'Scalar Average',3x,6(f6.3,i4))
	  call LogWrit(line)
	  write(line,120) (RmsAmp(i),nint(RmsPhi(i)),i=j,k)
120  	  format(5x,'Scalar Rms',5x,6(f6.3,i4))
	  call LogWrit(line)
	enddo
	end
