c********1*********2*********3*********4*********5*********6*********7**
c*
	program setcorr
c
c=  SETCORR	- input the correlator settings
c: observing
c& wh
c+
c	SETCORR changes the parameters relating to the correlator
c	setup.  The new values are loaded into common and the correlator
c	registers.  The corfs are adjusted for doppler tracking using
c	the last value of the doppler velocity that is in common.
c	The user specifies cormode, corf, and corbw, which are used to
c	determine the correlator configuration, shiftrate, and multiplexing.
c@ cormode
c	cormode determines the correlator configuration and number of
c	spectral windows. The default is cormode=8 with the maximum bandwidth.
c	The following table gives the bandwidth and the number of channels
c	in each correlator window. The four bandwidths bw1, bw2, bw3, bw4
c	may be one of 6.25/12.5/25/50/100 MHz. Multiplexing is used for a
c	bandwidth 50 or 100 MHz, and reduces the number of channels in the
c	window by a factor 2 or 4 respectively.
c
c				Correlator Modes.
c	-----------------------------------------------------------------
c		corf1		corf2		corf3		corf4	
c	cormode lsb1  usb1    lsb2   usb2    lsb3   usb3    lsb4  usb4
c	-----------------------------------------------------------------
c 	 1   bw1/1024 -       -     -          -      -      -     -
c	 2   bw1/512  -       -    bw2/512     -      -      -     -
c	 3   bw1/512  -       -    bw2/256     -      -      -    bw4/256
c	 4   bw1/256  -       -    bw2/256  bw3/256   -      -    bw4/256
c	 5   bw1/512  -     100/32 100/32      -      -     100/32 100/32
c	 6   bw1/256  -     100/32 100/32   bw3/256   -     100/32 100/32
c	 7    25/256  -     100/32 100/32   100/32  100/32  100/32 100/32
c	 8  *bw1/128 100/32 100/32 100/32   100/32  100/32  100/32 100/32
c	-----------------------------------------------------------------
c	* For cormode=8, bw1 is restricted to 25, 50, or 100 MHz.
c	For cross-correlation, the windows are repeated in each sideband
c	of the first LO, so there are nspect=2*cormode spectral windows.
c	The lsb windows are followed by the usb windows.
c	For auto-correlation there are nspect=cormode spectral windows
c	with twice as many channels in each double sideband window.
c@ corf
c	Correlator LO frequencies. Four numbers in range 90 to 900 MHz.
c	Default=200,400,600,800. Using cormode=8 and corbw=100 gives
c	the maximum bandwidth contiguous bandwidth from 100 to 900 MHz.
c	Insert unused values. e.g. for cormode=3 include a value for corf(3)
c@ corbw
c	Correlator filter widths. Up to 4 values depending on the cormode.
c	6.25/12.5/25/50/100 MHz. The default is 100,100,100,100. 
c	The pairs corbw1,3 and corbw2,4 use the same shift rate. 100/50/25
c	can be paired, but corbw1,3 and corbw2,4 should be the same for 
c	smaller bandwidths. Insert unused or constant values. e.g. for
c	cormode=6 corbw=25,100,50,100
c@ corspec
c	Select which correlator spectral windows are written into uv-data. 
c	corspec has nspect values: 1 or 0, to write, or omit each
c	spectral window respectively.  e.g. for cormode=5 cross-correlation,
c	corspec=1,0,0,0,0,1,0,0,1,1 writes lsb window 1 and usb windows 1,4,5
c	The default is to write all spectral windows.  The wideband average
c	of each window is always written (excluding flagged channels).
c	The wideband currently written is the average lsb, average usb,
c	followed by nspect=2*cormode averages for each spectral window.
c@ ants
c	Antenna flag; the correlator setup routines are run for
c	these antennas. The default, ants=0, sets the correlator parameters
c	and exits without running the correlator settup software.
c@ corbits
c	 Number of bits used to digitize the signal. [1 or 2]. Default=2
c@ restfqs
c	Rest frequencies used to calculate velocities for each spectral window.
c	The spectral windows are defined by the correlator setup.
c	There are nspect values in GHz, corresponding to the order defined
c	by cormode, not just the ones selected for writing by corspec.
c	Default: the rest frequency is set equal to
c	the observing frequency in any windows for which restfqs=0.
c@ maxcorr
c	Two values to specify the maximum correlation coeficient. The spectral
c	window is flagged if the measured correlation is greater than these
c	values. The 1st value applies to the whole correlation function.
c	Default	value=1. The 2nd value applies outside the central 10% of the 
c	correlation function, and defaults to the 1st value. The appropriate
c	values depend on the source flux density and system noise.
c	For a source flux density, S Jy, the expected correlation is 
c			S / (Jy/K x Tsys) = 2 x 10-5 x S,
c	for an antenna gain of 140 Jy/K and Tsys=300 K.
c	For Gaussian noise the rms correlation is
c			dT / Tsys = 1/sqrt(2 x corbw x inttime) = 10-5 for
c	corbw=100 MHz and inttime=50 sec.
c	Set maxcorr greater than the expected correlation by at least
c	5-6 x rms to avoid flagging random noise.
c	For a continuum source, the correlation function falls rapidly so a
c	smaller value for maxcorr can be set outside the central 10%.
c	For spectral line emission use the default maxcorr(2) = maxcorr(1).
c	E.g. for a 10 Jy quasar maxcorr=2e-3 will flag data which is ten times
c	the expected correlation, or 20x the expected noise.
c@ coptions
c	Several processing options may be given separated by commas:
c       auto	  Auto-correlation.  Default = Cross-correlation.
c       hanning	  Hanning smoothing reduces the resolution and suppresses
c		          ringing due to sharp edges. Default is no Hanning.
c       nocal	  No antenna temperature calibration. Default = Yes.
c       nopass    No on-line passband calibration. Default = Yes.
c       nocable	  No cable length calibration. Default = Yes.
c@ passfile
c	Cache file for passband (created in the project directory)
c	If this is named and contains data, it will be used for the
c	passband. Use different names for different correlator setups.
c	If it is missing, passbands will be measured for each source.
c	note: common variable PRESET = Y overrides this variable.
c@ conffile
c	Cache file for correlator setups (created in the project directory)
c	If this is named and contains data, it will be used for the
c	basic correlator settings. Use different names for the different
c	correlator setups.  If it is missing, new correlator settings will
c	be loaded from common and detector tweaking will occurr.
c	note: common variable CRESET = Y overrides this variable.
c@ tsysfile
c	Cache file for TSYS/Gains (created in the project directory)
c	If this is named and contains data, it will be used for the
c	Calibration values. Use different names for the different
c	correlator setups.  If it is missing, The TSYS measurement
c	will be remeasured.
c	note: common variable TRESET = Y overrides this variable.
c--
c  History:
c  oct 91 wh	
c  23feb93 mchw  Calculate default cormpx and corshift from corbw.
c  26feb93 mchw  Check user inputs for consistency.
c  26apr93  wh   Add setting of corbits
c  27aug93 wh    set exit status
c  04nov93 mchw  New correlator modes.
c  24feb94 mchw  Testing new correlator modes.
c  24mar94 mchw  Double number of channels for auto-correlation modes.
c  29mar94 mchw  Better doc for ants=0, and corbw; corrected table.
c  06jul94 mchw  revised doc; no multiplex for shift < 50 MHz.
c  21jul94 mchw  Added hanning keyword.
c  25jul94 mchw  Set bw(2) = bw(1) for mode 1
c  15aug94 mchw  Added restfqs keyword. Removed version 3 from name.
c  09sep94 mchw  Change keyword to "cormode". doc for auto-correlation.
c  19oct94 wh	 set cabyn='Y' 
c  03nov94 mchw  Added maxcorr, and processing options with defaults.
c  20dec95 wh    add passfile and conffile
c  12feb96 mchw  Fixed bug in message about cabyn.
c  16feb96 wh    TSYSFILE
c---------------------------------------------------------------------c
	character version*(*)
	parameter(version='SETCORR (version 3.23 02-AUG-96)')
	integer NUMCHAN,NUMWIN,MAXWIN
	parameter (NUMWIN=8,MAXWIN=16)
	include '../inc/constants.inc'
	include '../inc/exports.inc'
	integer		mode
	integer		coropt, corbits
	integer		cormpx(NUMWIN)
	real 		corfin(4)	! input corfs, before doppler shifts
	real		bw(4)
	real		corf(4)		! actual corfs, after doppler shifts
	character*1 hanning,caplcal,cpasscal,cabyn
	character*80 	error, mess
	real		lo2			! 2nd lo freq
	real*8		beta		! doppler correction factor v/c
	integer c3bits(NUMWIN), c3mpx(NUMWIN), c3samp(NUMWIN), c3mode(2)
	integer nspect,nschan(MAXWIN),ischan(MAXWIN),corspec(MAXWIN)
	double precision sfreq(MAXWIN),sdf(MAXWIN),restfreq(16),freq
	real corshift(2), veldop, maxcorr(2)
	integer ants, i, j, nchan, ncorspec
	character*20 exopt(2) /'Crosscorrelation ','Autocorrelation '/
	data ischan/MAXWIN*0/,c3samp/NUMWIN*0/,c3mode/2*0/,corspec/MAXWIN*1/
	data nschan/MAXWIN*0/,c3bits/NUMWIN*0/,c3mpx/NUMWIN*0/
	character*80 conffile,passfile,tsysfile
c
c  Input the arguments
c
	call mbusopen(error)
	 if(error(1:2).ne.'OK') goto 99
	call announce(version,error)
	 if(error(1:2).ne.'OK') goto 99

	call keyini
        call keyi('cormode',mode,8)
	call keyr('corf',corfin(1),200.)
	call keyr('corf',corfin(2),400.)
	call keyr('corf',corfin(3),600.)
	call keyr('corf',corfin(4),800.)
	call keyr('corbw',bw(1),100.)
	call keyr('corbw',bw(2),100.)
	call keyr('corbw',bw(3),100.)
	call keyr('corbw',bw(4),100.)
	call keyi('corbits',corbits,2)
	call mkeyi('corspec',corspec,MAXWIN,ncorspec)
	call keyants(ants,0,error)
	do i = 1,16
	  call keyd('restfqs',restfreq(i),0.d0)
	enddo
	call keyr('maxcorr',maxcorr(1),1.)
	call keyr('maxcorr',maxcorr(2),maxcorr(1))
	call GetOpt(coropt,hanning,caplcal,cpasscal,cabyn)
	call keya('conffile',conffile,' ')
	call keya('passfile',passfile,' ')
	call keya('tsysfile',tsysfile,' ')
	call keyfin
c
c  Check input parameters.
c
	if(mode.lt.1 .or. mode.gt.8) then
	  error = 'Illegal value for CORMODE'
	  goto 99
	endif
c
	if(coropt.eq.0) then
	  NUMCHAN = 1024
	else if(coropt.eq.1) then
	  NUMCHAN = 2048
	else
	  error = 'Illegal value for COROPT'
	  goto 99
	endif
c
c  Check corfs and corbw.
c
	do i=1,4
	  if(corfin(i).lt.90. .or. corfin(i).gt.900.) then
	   write(error,'(a,i2,a)') 'Illegal value for CORF(',i,')'
	   goto 99
	  endif
	  if(bw(i).ne.100. .and. bw(i).ne.50. .and. 
	1	bw(i).ne.25. .and. bw(i).ne.12.5 .and.
	2		bw(i).ne.6.25) then
	   write(error,'(a,i2,a)') 'Illegal value for CORBW(',i,')'
	   goto 99
	  endif
	enddo
c
c  Check modes and fix unused bw's for shiftrate calculation.
c
	if(mode.eq.1) bw(2) = bw(1)
	if(mode.eq.1 .or. mode.eq.2) bw(4) = bw(2)
	if(mode.eq.1 .or. mode.eq.2 .or. mode.eq.3 .or. mode.eq.5)
     *      bw(3) = bw(1)
	if(mode.eq.5 .and.(bw(2).ne.100.d0 .or. bw(4).ne.100.d0))then
	    write(error,'(a)') 'setting corbw 2 & 4 to 100 MHz'
	    bw(2) = 100.d0
	    bw(4) = 100.d0
	else if(mode.eq.6) then
	  if(bw(2).ne.100.d0 .or. bw(4).ne.100.d0)then
	    write(error,'(a)') 'setting corbw 2 & 4 to 100 MHz'
	    call bug('w','setting corbw 2 & 4 to 100 MHz')
	    bw(2) = 100.d0
	    bw(4) = 100.d0
	  endif
	else if(mode.eq.7) then
	  if(bw(1).ne.25.d0)then
	    write(error,'(a)') 'setting corbw 1 to 25 MHz'
	    bw(1) = 25.d0
	  endif
	  if(bw(2).ne.100.d0 .or. bw(3).ne.100.d0
     *					 .or. bw(4).ne.100.d0)then
	    write(error,'(a)') 'setting corbw 2, 3 & 4 to 100 MHz'
	    bw(2) = 100.d0
	    bw(3) = 100.d0
	    bw(4) = 100.d0
	  endif
	else if(mode.eq.8) then
	  if(bw(1).ne.25.d0 .and. bw(1).ne.50.d0
     *					 .and. bw(1).ne.100.d0)then
	    write(error,'(a)') 'bw(1) must be 25, 50 or 100 MHz'
	    goto 99
	  endif
	  if(bw(2).ne.100.d0 .or. bw(3).ne.100.d0
     *					 .or. bw(4).ne.100.d0)then
	    write(error,'(a)') 'setting corbw 2, 3 & 4 to 100 MHz'
	    bw(2) = 100.d0
	    bw(3) = 100.d0
	    bw(4) = 100.d0
	  endif
	endif
c	
c  set values for cormpx
c
	do i=1,4
	  if(bw(i).eq.100.0d0) then
	    cormpx(i) = 4
	  else if(bw(i).eq.50.0d0) then 
	    cormpx(i) = 2
	  else
	    cormpx(i) = 1
	  endif
	enddo
c
c  Set default values for corshift
c
	do i=1,2
	   if(bw(i).ge.25..or.bw(i+2).ge.25.)then
	      corshift(i) = 50.
	   else if(bw(i).eq.12.5.or.bw(i+2).eq.12.5)then
	      corshift(i) = 25.
	   else
		  corshift(i) = 12.5
	   endif
c
c  Check for under and over-sampling.
c
	  do j=0,2,2
	    if(bw(i+j).gt.cormpx(i+j)*corshift(i)/2.)then
	  	  write(error,'(a,i2,a)')'corbw(',i+j,') is undersampled'
		  goto 99
	    endif
	    if(bw(i+j).lt.cormpx(i+j)*corshift(i)/2.)then
	  	  write(error,'(a,i2,a)')'corbw(',i+j,') is oversampled'
		  call write_error('setcorr',error)
	    endif
	  enddo
	enddo
c
c  compute the number of channels in each window
c
	if(mode.eq.1)then
	  nschan(1)=NUMCHAN/cormpx(1)
	else if(mode.eq.2)then
	  nschan(1)=NUMCHAN/2./cormpx(1)
	  nschan(2)=NUMCHAN/2./cormpx(2)
	else if(mode.eq.3)then
	  nschan(1)=NUMCHAN/2./cormpx(1)
	  nschan(2)=NUMCHAN/4./cormpx(2)
	  nschan(3)=NUMCHAN/4./cormpx(4)
	else if(mode.eq.4)then
	  do i=1,4
	    nschan(i)=NUMCHAN/4./cormpx(i)
	  enddo
	else if(mode.eq.5)then
	  nschan(1)=NUMCHAN/2./cormpx(1)
	  do i=2,5
	    nschan(i)=NUMCHAN/8./4.
	  enddo
	else if(mode.eq.6)then
	  nschan(1)=NUMCHAN/4./cormpx(1)
	  nschan(2)=NUMCHAN/8./4.
	  nschan(3)=NUMCHAN/8./4.
	  nschan(4)=NUMCHAN/4./cormpx(3)
	  nschan(5)=NUMCHAN/8./4.
	  nschan(6)=NUMCHAN/8./4.
	else if(mode.eq.7)then
	  nschan(1)=NUMCHAN/4./1.
	  do i=2,7
	    nschan(i)=NUMCHAN/8./4.
	  enddo
	else if(mode.eq.8)then
	  nschan(1)=NUMCHAN/8./cormpx(1)
	  do i=2,8
	    nschan(i)=NUMCHAN/8./4.
	  enddo
	endif
c
c  number of channels in each sideband of LO1
c
	nchan = 0
	do i=1,mode
	  nchan = nchan + nschan(i)
	enddo
c
c  Set the correlator common variables
c
	if(mode.eq.1)then
	  c3mode(1) = 1
	  c3mode(2) = 1
	  c3samp(1) = 1
	  c3mpx(1) = cormpx(1)
	else if(mode.eq.2)then
	  c3mode(1) = 2
	  c3mode(2) = 2
	  c3samp(1) = 1
	  c3samp(2) = 4
	  c3mpx(1) = cormpx(1)
	  c3mpx(2) = cormpx(2)
	else if(mode.eq.3)then
	  c3mode(1) = 2
	  c3mode(2) = 3
	  c3samp(1) = 1
	  c3samp(2) = 4
	  c3samp(3) = 8
	  c3mpx(1) = cormpx(1)
	  c3mpx(2) = cormpx(2)
	  c3mpx(3) = cormpx(4)
	else if(mode.eq.4)then
	  c3mode(1) = 3
	  c3mode(2) = 3
	  c3samp(1) = 1
	  c3samp(2) = 4
	  c3samp(3) = 5
	  c3samp(4) = 8
	  do i=1,4
	    c3mpx(i) = cormpx(i)
	  enddo
	else if(mode.eq.5)then
	  c3mode(1) = 2
	  c3mode(2) = 4
	  c3samp(1) = 1
	  c3samp(2) = 3
	  c3samp(3) = 4
	  c3samp(4) = 7
	  c3samp(5) = 8
	  c3mpx(1) = cormpx(1)
	  do i=2,5
	    c3mpx(i) = 4
	  enddo
	else if(mode.eq.6)then
	  c3mode(1) = 3
	  c3mode(2) = 4
	  c3samp(1) = 1
	  c3samp(2) = 3
	  c3samp(3) = 4
	  c3samp(4) = 5
	  c3samp(5) = 7
	  c3samp(6) = 8
	  c3mpx(1) = cormpx(1)
	  c3mpx(2) = 4
	  c3mpx(3) = 4
	  c3mpx(4) = cormpx(3)
	  c3mpx(5) = 4
	  c3mpx(6) = 4
	else if(mode.eq.7)then
	  c3mode(1) = 4
	  c3mode(2) = 4
	  c3samp(1) = 1
	  c3samp(2) = 3
	  c3samp(3) = 4
	  c3samp(4) = 5
	  c3samp(5) = 6
	  c3samp(6) = 7
	  c3samp(7) = 8
	  c3mpx(1) = cormpx(1)
	  do i=2,7
	    c3mpx(i) = 4
	  enddo
	else if(mode.eq.8)then
	  c3mode(1) = 4
	  c3mode(2) = 4
	  do i=1,8
	    c3samp(i) = i
	  enddo
	  c3mpx(1) = cormpx(1)
	  do i=2,8
	    c3mpx(i) = 4
	  enddo
	endif	
c
	do i=1,mode
	  c3bits(i) = corbits
	enddo
c
c  lock up the correlator; if locked, print error messg and quit
c
	call lock_up(0,'L','LOCKCOR',' ',' ',' ',error)
	 if (error(1:2).ne.'OK') goto 99
c
c  copy correlator configuration into common
c
	call computi('CORMODE',mode,1,error)
		call write_error('setcorr',error)
	call computi('COROPT',coropt,1,error)
			! note: some programs override this value of icoption
		call write_error('setcorr',error)
	call computi('CORBITS',2,1,error)
	call computa('HAN',hanning,1,error)
	call computa('CPASSCAL',cpasscal,1,error)
	call computa('CAPLCAL',caplcal,1,error)
	call computa('C3MCAL','N',1,error)
	call computr('CORSHIFT',corshift,2,error)
	call computi('C3NSPECT',mode,1,error)
	call computi('C3NCHAN',nchan,1,error)
	call computi('C3NSCHAN',nschan,NUMWIN,error)
	call computi('C3MPX',c3mpx,NUMWIN,error)
	call computi('C3SAMP',c3samp,NUMWIN,error)
	call computi('C3BITS',c3bits,NUMWIN,error)
	call computi('C3MODE',c3mode,2,error)
	call computa('CABYN',cabyn,1,error)
	call computr('MAXCORR',maxcorr,2,error)
	call computa('CONFFILE',conffile,80,error)
	call computa('PASSFILE',passfile,80,error)
	call computa('TSYSFILE',tsysfile,80,error)
	call computi('CORSPEC',corspec,MAXWIN,error)
c
	type *,'INPUT VALUES FOR CORRELATOR'
	write(mess,'(a,i2,a,a,a)')
	1	'COROPT= ',coropt,' (',exopt(coropt+1),')'
	type *,mess
	call logit(mess)
	write(mess,'(a,i2,a,a,a)')'CORMODE= ',mode,
     *				' (number of spectra in one sideband of LO1)'
	type *,mess
	call logit(mess)
	if(hanning.eq.'Y')then
	 write(mess,'(a)')'HAN= Y (Hanning smoothing)'
	else
	 write(mess,'(a)')'HAN= N (No Hanning smoothing)'
	endif
	type *,mess
	call logit(mess)
	write(mess,'(a,i2,a)')'CORBITS= ',corbits, ' (bits in sampler)'
	type *,mess
	call logit(mess)
	if(cpasscal.eq.'Y')then
	 write(mess,'(a)')'CPASSCAL= Y (Passband correction applied)'
	else
	 write(mess,'(a)')'CPASSCAL= N (No Passband correction applied)'
	endif
	type *,mess
	call logit(mess)
	if(caplcal.eq.'Y')then
	 write(mess,'(a)')'CAPLCAL= Y (Calibration applied)'
	else
	 write(mess,'(a)')'CAPLCAL= N (No Calibration applied)'
	endif
	type *,mess
	call logit(mess)
	if(cabyn.eq.'Y')then
	 write(mess,'(a)')'CABYN= Y (Cable length calibration applied)'
	else
	 write(mess,'(a)')'CABYN= N (No Cable length calibration applied)'
	endif
	type *,mess
	call logit(mess)
	write(mess,'(a,2f6.1,a)')'CORSHIFT= ',corshift,' (shiftrates<MHz>)'
	type *,mess
	call logit(mess)
	write(mess,'(a,4f10.4,a)')'CORFS= ',corfin,' (lo3<MHz>)'
	type *,mess
	call logit(mess)
	write(mess,'(a,4f10.2,a)')'CORBW= ',bw,' (bandwidths<MHz>)'
	type *,mess
	call logit(mess)
	write(mess,'(a,16i1,a)')'CORSPEC= ',corspec,' (windows written)'
	type *,mess
	call logit(mess)
 
c -- corfs are doppler shifted using last value of veldop
	call comgetr('VELDOP',veldop,1,error)
		call write_error('setcorr',error)
	call comgetd('LO2',lo2,1,error)
		call write_error('setcorr',error)
	beta = veldop/c				! doppler factor

c -- this code copied from set_synth
	do i=1,4
	    corf(i)=corfin(i)-beta*(lo2+corfin(i))

	    if ( (corf(i).lt.90.) .or. (corf(i).gt. 900.) ) then
		write(error,703) i
703	    	format ('    WARNING: CORF(',I1,
	1		') out of range after doppler shift')
		call logit(error)
		call output(error)
	    endif
	enddo

	type *,' Sample CORF doppler correction for VELDOP=',veldop
	type 700, corfin
700	format (' Before: ',4F11.6)
	type 702, corf
702	format (' After:  ',4F11.6)

	call computr('CORFIN',corfin,4,error)
		call write_error('setcorr',error)
	call computr('CORF',corf,4,error)
		call write_error('setcorr',error)


	call computr('CORBW',bw,4,error)
		call write_error('setcorr',error)

c -- turn on all IFS & setup correlator if ants>0
	if(ants.ne.0) then
	  call ifamps(ants,'0DB',error)
	  type *,'0 dB atten in 2nd IF boxes'
	  call computi('ANTS',ants,1,error)
	   call write_error('setcorr',error)

c  --	initialize correlator with new setup
	 if (.not.CORA_COMSET()) then
	  CALL CORA_MSG(ERROR)
	  GOTO 99
	 end if
	end if

c -- compute windows and put in common
	CALL CORACOM

c -- input windows and type
	call comgeti('nspect',nspect,1,error)
	call comgeti('nschan',nschan,MAXWIN,error)
	call comgeti('ischan',ischan,MAXWIN,error)
	call comgetd('sfreq',sfreq,MAXWIN,error)
	call comgetd('sdf',sdf,MAXWIN,error)

c -- write out restfreqs for each window.
	CALL COMGETD('FREQ',FREQ,1,ERROR)
	 CALL WRITE_ERROR('FREQ',ERROR)
	do i=1,16
	  if((restfreq(i).lt.50.d0).or.(restfreq(i).gt.300.d0))
	1				 		restfreq(i)=freq
	enddo
	call computd('RESTFREQ',restfreq,16,error)
	   CALL WRITE_ERROR('FREQ',ERROR)

	type *
	type *,'	SPECTRAL WINDOWS DESCRIPTION'
	type *,'window  ISCHAN  NSCHAN     SFREQ         SDF '
	do i=1,nspect
	  type 90,i,ischan(i),nschan(i),sfreq(i),sdf(i)
90	  format(x,i3,4x,i4,4x,i4,4x,f12.6,4x,e9.3)
	end do

99	call ieee_retrospective
	if (error(1:2).ne.'OK') call bug('f',error)
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine GetOpt(coropt,hanning,caplcal,cpasscal,cabyn)
c
	implicit none
	integer coropt
	character*1 hanning,caplcal,cpasscal,cabyn
c
c  Determine extra processing options.
c
c  Output:
c    coropt		crosscorrelation=0; autocorrelation=1
c    hanning	hanning smoothing [Y/N]
c    caplcal	apply antenna temperature calibration [Y/N]
c    cpasscal	apply on-line passband calibration [Y/N]
c    cabyn		do cable length calibration [Y/N]
c-----------------------------------------------------------------------
	integer nopt
	parameter(nopt=5)
	logical present(nopt)
	character opts(nopt)*9
	data opts/'auto     ','hanning  ','nocal    ',
     *		  'nopass   ','nocable  '/
	call options('coptions',opts,present,nopt)
c
c  Set Defaults.
c
	coropt   = 0
	hanning  = 'N'
	caplcal  = 'Y'
	cpasscal = 'Y'
	cabyn    = 'Y'
c
c  Set options.
c
	if(present(1)) coropt   = 1
	if(present(2)) hanning  = 'Y'
	if(present(3)) caplcal  = 'N'
	if(present(4)) cpasscal = 'N'
	if(present(5)) cabyn    = 'N'
	end
c************************************************************************
c  History:
c    rjs    nov89 Original version.
c    rjs  25jan90 Minor documentation improvement.
c    nebk 18may92 Add KEYMATCH
c    nebk 03jun92 Give user more help in KEYMATCH if they bomb and
c                 restrict number of possible KEYA calls to MAXOUT
c                 so successive KEYMATCH calls with the same KEYWORD 
c                 can be used
c    rjs  02jul92 Insensitive to case. Input must be in lower case.
c
c********1*********2*********3*********4*********5*********6*********7*c
cc*Options -- Get command line options.
cc:user-input
cc+
	subroutine Options(key,opts,present,nopt)
c
	implicit none
	character key*(*)
	integer nopt
	character opts(nopt)*(*)
	logical present(nopt)
c
c  Get options from the command line, and return to the caller those
c  options that are present. For example, assume the task keyword is
c  "options", and that possible options are, say, display,movie,fiddle, etc
c  then this examines the command line:
c    task options=fiddle
c  and returns indicating that "fiddle" was present, but "display" and
c  "movie" were absent.
c  This will allow the user to abbreviate options to uniqueness, and will
c  generate an error if there is an ambiguous option.
c
c  Inputs:
c    key	The task keyword to use.
c    opts	An array of possible option values. These should be in lower
c		case.
c    nopt	The number of possible options.
c  Output:
c    present	This indicates whether the option was present.
c--
c------------------------------------------------------------------------
	character string*16
	integer l,i,iopt
c
c  Externals.
c
	integer len1
        character*80 umsg
c
c  Initialise the options to indicate that none are present.
c
	do i=1,nopt
	  present(i) = .false.
	enddo
c
c  Search the task parameters.
c
	call keya(key,string,' ')
	dowhile(string.ne.' ')
	  l = len1(string)
	  call lcase(string(1:l))
          umsg = 'Unrecognised option '//string
	  if(l.gt.len(opts(1)))
     *	    call bug('f',umsg)
	  iopt = 0
	  do i=1,nopt
	    if(string(1:l).eq.opts(i)(1:l))then
              umsg = 'Ambiguous option '//string
	      if(iopt.ne.0)
     *		call bug('f',umsg)
	      iopt = i
	    endif
	  enddo
          umsg = 'Unrecognised option '//string
	  if(iopt.eq.0)
     *	    call bug('f',umsg)
	  present(iopt) = .true.
	  call keya(key,string,' ')
	enddo
	end
c************************************************************************
cc*KeyMatch -- Get, with minimum match, command line options.
cc:user-input
cc+
      subroutine keymatch (key, ntype, types, maxout, out, nout)
      implicit none
c
      integer ntype, nout, maxout
      character*(*) types(ntype), out(maxout), key
c
c     Get a list of inputs for one keyword from the user and expand them
c     for minimum match compared to a list of possible inputs.
c     This will generate an error if there is an ambiguous option.
c
c  Inputs:
c    key	The task keyword to use.
c    type	An array of possible input values. These should be in lower
c               case.
c    ntype	The number of possible input types
c    maxout     Maximum number of output values
c  Output:
c    out        The expanded keyword list.  Will be all blank
c               if no values given
c    nout       The number of output values in out.
c--
c------------------------------------------------------------------------
	character string*16
	integer l, i, iopt, j
	integer len1
        character*130 umsg
c-----------------------------------------------------------------------
c
c  Initialize
c
      do i = 1, maxout
        out(i) = ' '
      end do
      nout = 0
c
c Search input for known type
c
      call keya(key,string,' ')
      dowhile(string.ne.' ' .and. nout.lt.maxout)
        l = len1(string)
        call lcase(string(1:l))
c
        iopt = 0
        do i = 1, ntype
          if(string(1:l).eq.types(i)(1:l))then
            if(iopt.ne.0) then
              umsg = string(1:len1(string))//' is ambiguous for '//
     +               'keyword '//key//'.  Choose from'
              call output (umsg)
              do j = 1, ntype
                umsg = '   '//types(j)
                call output (umsg)
              end do
              call bug('f', ' ')
            end if
            iopt = i
          endif
        enddo
        umsg = string(1:l)//' is unrecognised for keyword '//
     +         key//'. Choose from'
        if(iopt.eq.0) then
          call output (umsg)
          do j = 1, ntype
            umsg = '   '//types(j)
            call output (umsg)
          end do
          call bug('f', ' ')
        end if
c
        nout = nout + 1
        out(nout) = types(iopt)
c
        if (nout.lt.maxout) call keya(key,string,' ')
      enddo
c
      end
