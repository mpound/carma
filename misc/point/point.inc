c data types, structures and common blocks for the POINTS routines
c see pointdata.inc for the data// style statements

	implicit none

	double precision xxx, yyy	!temporary variables
	real	fday	!fraction of mjd day
	logical	badfound	!=.true. if a bad point has been removed
	real	bigdot		!size of biggest dot for plots
	real	ploterr		!threshhold to use for selecting data to be plotted
			!data with azer or eler above "ploterr" is rejected
			!for fitting and plotting
	character	formfeed/12/	!ASCII formfeed
	real	gain		!for converting error to vectors 
				!for hair ball plot
	real	threshold	!bad data threshold [standard deviations]
	real	WTRMS	!function to return weighted RMS
	character*80	label	!general purpose label
	real	rmsbeforeaz	!rms before fitting
	real	rmsbeforeel	!rms before fitting
	real	rmsafteraz	!rms residual after fitting
	real	rmsafterel	!rms residual after fitting
	real	worstaz	!deviation of worst point
	real	worstel	!deviation of worst point
	integer	iworstaz	!index of worst point
	integer	iworstel	!index of worst point
	character*80	toplbl	!label for top of plots
	character*120	toplbl2	!second line for top of plots
	character*80	junk	!junk line
	character*73	header	!time stamp + type + time range
	character*20	timestamp	!today's date and time, time stamp
	double precision	mjdstart, mjdstop	!mjd times
	double precision	getmjdtime	!function to get time
	character*20		time1, time2, time3	!people time
	integer			length			!lenght of time string
	integer	mjd_to_abstime_fortran	!function to convert MJD to people time
	double precision scan_abstime_fortran	!convert people time to MJD

c data structure for pointing data
	structure /pntrec/
		integer		ant	!antenna number
		integer		rx	!receiver #, ie. 1 or 2
		character*4	pad	!location of the antenna
		character*12	src	!source name
		character*7	date	!date
		double precision	mjd	!Julean date
		real		az	!azimuth
		real		el	!elevation
		real		azo	!azimuth offset
		real		azer	!error or noise or S/N in azo
		real		elo	!elevation offset
		real		eler	!error or noise or S/N in elo
 		real		con(8)	!pointing constants in use
	end structure                                 

	record /pntrec/ ir	!single input data record

	integer	maxants	!maximum number of antennas to deal with
	parameter( maxants=6 )
	integer	maxpts	!maximum number of data points per antenna
	parameter( maxpts=10000)
	integer	maxterms	!maximum of fitting terms
	parameter( maxterms = 8)

	record	/pntrec/ ptdat(maxpts,maxants,2)	!selected data array
				! (data points, antenna #, receiver # )
	real	c(maxterms)	!fitted constants
	real	wobc(8)	!fitted wobble constants
	real	cold(maxterms)	!pointing constants used for data
	integer	imap(maxterms)	!map of which constants to fit
	integer	imapsave(maxterms)	!saved imap
	integer	icant(maxants,2)	!current count of number of data points
				!selected for each antenna and receiver
	logical		radio	!=.true. if this is radio pointing
	integer		ioer	!IO error status
	integer		i,j,k,l	!general indices  
	integer		ant	!antenna number   
	integer		rxn	!receiver number
	integer		lun	!input file unit #
	integer		logu		!output log unit #
	integer		constu		!output pointing constants unit #
	character*2	mm	!"MM" for marking the beginning of a record
	character*60	filename	!      
	character*60	file		!      
	character*60	infile		!           
	character*60	outfile		!      
	character*60	constfile	!file for pointing constants
	character*79	textin	!junction to get text string
	logical		ivalid	!number of valid records read in
	logical		select	!function to determine if record meets
				!selection criterion
	logical		yes, yes2	!answer to a question
	logical		ask	!function to ask question                                              
	integer		ivalue	!function to get an integer
	real		value	!function to get a real number
	integer		nvar, nvarsave	!number of variables being fit
	logical	term(maxterms)	!indicates if term was fitted last time around
	character*20	funclabel	!returns name of term being fit
	logical	screen		!=.true. if writing to teminal screen is wanted
	logical		depoint	!=.true. if old pointing terms are to removed
				!from the data
	logical		same		!=.true. if same as last time
	logical		sameterms	!=.true. if same terms are to be fit
	logical		sameplots	!=.true. if same plots are to be made
	logical		azelplots	!=.true. to plot offset vs. AZ and EL		logical		subfit	!=.true. if fitted terms are to be subtracted
	logical		timeplots 	!=.true. to plot offset vs. time 
	logical		diurnal	 	!=.true. to plot offset vs. time of day
	logical		hair		!=.true. to make vector hairball plot
	logical		hardc		!=.true. to make hard copy of plots
	logical		subfit		!=.true. if fitted terms are to be subtracted
	logical		wobble		!=.true. if wobble fit is to be done
	character*32	device		!plot device for PGPLOT
	character*32	soft		!screen plot device for PGPLOT
	character*32	hard		!hardcopy device for PGPLOT

c	integer     icode
	integer	npts	!number of data points
	double precision	mjd(maxpts)	!time MJD
	real			mjd1(maxpts)	!MJD time minus NINT of middle
	character*7	ddmmmyy	!beginning of time range
	real	az(maxpts)	!table of azimuth values
	real	el(maxpts)	!table of elevation values
	real 	azo(maxpts)	!table of azo values to be fit
			!also output of residuals from the fit
	real	elo(maxpts)	!table of elo values to be fit
			!also output of residuals from the fit
	real	wtaz(maxpts)	!table of data point weights
	real	azwt		!single point AZ weight
	real	wtel(maxpts)	!table of data point weights
	real	elwt		!single point EL weight
	integer	loadmap(maxpts)	!address of where point came from
	logical	remov(maxpts)	!=.true. if point has been edited out
	integer	nxpage, nypage	!number of plots per page
	real	minyspan	!minimum allowed Y-axis span
	real	minxspan	!minimum allowed X-axis span

	integer	master(0:10,16)	!set of combination of fitting optical terms
				!master(0,i) = number of variables
				!master(j,i) = function number
c	the 'data' statements can only be included one in ANSI compilers,
c	'vax' compiler was more liberal	

	integer	raster(0:10,16) !set of combination of fitting radio terms
				!raster(0,i) = number of variables
				!raster(j,i) = function number

	real	utstart, utend	!UT selection window

	common	/ptarrays/ file, ptdat, icant, logu, term, imap, nvar
     &		,mjdstart, mjdstop, sameterms, subfit
     &		,npts, mjd1, az, el, azo, elo, wtaz, wtel
     &		,sameplots, azelplots, timeplots, timestamp
     &		,toplbl, radio, header, ddmmmyy
     &		,rmsbeforeaz, rmsbeforeel, rmsafteraz, rmsafterel
     &		,worstaz, worstel, iworstaz, iworstel, c, constu, constfile
     &		,loadmap, cold, soft, hard, threshold, gain, hair
     &		,screen, hardc, wobble, wobc, ploterr, master, raster, bigdot
     &		,toplbl2, utstart, utend

