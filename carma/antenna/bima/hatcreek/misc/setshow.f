c*
	PROGRAM SETSHOW
c& wh
c= SETSHOW - Controls the operation of the show subroutine
c: observing
c+
c	SETSHOW controls the operation of the show subroutine
c	that displays data taken by observation programs.
c
c@ mode
c	 If 's', display spectra
c	 If 'l', display linetype vs. time
c
c@ select
c	To select the baselines to be displayed in 
c	spectra mode, 
c	 select=antennae(a,b,c)[(d,e)]
c	 (This copies the miriad defs.)
c
c@ accum
c	If 't', show the accumulated spectra, otherwise
c	show the instantaneous spectra.
c
c@ spectra
c	 Describes the spectra that is captured for mode 's'.
c	  LINETYPE,START,WIDTH,STEP
c	  LINETYPE can take the value: 'channel'
c	  WIDTH is the number of channels to average per line.
c	  START is the first channel number.
c	  STEP is the channel increment between points.
c	  A maximum of 256 lines are captured and displayed.
c	  (If spectra is changed during an observation, 
c	  accumulation is restarted.)
c
c@ line
c	Describes the line that is captured for mode 'l'.
c	 LINETYPE,START,WIDTH
c	 LINETYPE can take the values: 'channel' or 'wide'
c
c	 If start=0 and LINETYPE isn't 'wide', wide(1) is captured.
c--
c  oct 91 wh	
c---------------------------------------------------------------------c
	character*(*) version
	parameter(version='SETSHOW (version 1.0 25-oct-91)')
	character*80 error
	character*150 showit
	character*1 a,m
	character*30 ants,linetype,spectratype
	character*30 line,spect
	integer break

 	call output(version)
	call keyini
	call keya('mode',m,'l')
	call keya('accum',a,'t')
	call keya('line',linetype,'wide')
	call keyi('line',nfirst,1)
	call keyi('line',nchan,1)
	call keya('spectra',spectratype,'channel')
	call keyi('spectra',nsfirst,1)
	call keyi('spectra',nschan,1)
	call keyi('spectra',nsstep,1)
	call keya('select',ants,'antennae(1,2,3,4)')
	call keyfin

	if(m.ne.'l' .and. m.ne.'s') then
	  type *,'SETSHOW: mode=',m,' is illegal'
	  stop
	else if(a.ne.'t' .and. a.ne.'f') then
	  type *,'SETSHOW: accum=',a,' is illegal'
	  stop
	else if(linetype.ne.'wide' .and. linetype.ne.'channel') then
	  type *,'SETSHOW: linetype=',linetype,' is illegal'
	  stop
	else if(nchan.lt.1 .or. nchan.gt.2048) then
	  type *,'SETSHOW: line_n=',nchan,' is illegal'
	  stop 
	else if(nfirst.lt.1 .or. nfirst.gt.2048) then
	  type *,'SETSHOW: line_first=',nfirst,' is illegal'
	  stop
	else if(spectratype.ne.'channel') then
	  type *,'SETSHOW: spectra_type=',spectratype,' is illegal'
	  stop
	else if(nschan.lt.1 .or. nschan.gt.99) then
	  type *,'SETSHOW: spectra_n=',nschan,' is illegal'
	  stop
	else if(nsfirst.lt.1 .or. nsfirst.gt.2000) then
	  type *,'SETSHOW: spectra_first=',nsfirst,' is illegal'
	  stop
	else if(nsstep.lt.1 .or. nsstep.gt.99) then
	  type *,'SETSHOW: spectra_step=',nsstep,' is illegal'
	  stop
	end if
	write(line,'(a,a,a,i4.4,a,i4.4,a)')
	1  'line=',linetype(1:len1(linetype)),',',nfirst,',',nchan,' '

	write(spect,'(a,a,a,i4.4,a,i2.2,a,i2.2,a)')
	1  'spectra=',spectratype(1:len1(spectratype)),',',nsfirst,',',nschan,
	1	',',nsstep,' '

	write(showit,'(a,a,a,a,a,a)')
	2  'mode=',m,' accum=',a,' select=',ants(1:len1(ants))

	showit = showit(:len1(showit))//' '//line(:len1(line))//' '//
	1			spect(:len1(spect))
	call computa('SHOWCOM',showit,150,error)
	type *,'Output line:'
	break = index(showit(70:150),' ')-1
	type *,showit(1:70+break)
	type *,showit(70+break:len1(showit))
	print *,error
	end


