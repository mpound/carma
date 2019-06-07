	subroutine fitxyzz( imax,x,y,z,z2,wt,wt2, nvar,flist
	1	, c,rmsbefore,rmsbefore2,rmsafter,rmsafter2
	2	, screen, logu, iworst, iworst2, worst, worst2 )

c modified from FITXYZ to handle fitting azo and elo pointing data,
c i.e. two "z" surfaces as a function az,el
c this essentially just doubles all of the z related functions

c Routine to fit z to functions of x and y 
c with weighting of individual data points.

c Started 05APR91 by DPW

	implicit	none

c input data
	integer	logu	!unit for writing LOg file
	integer	imax	!number of data points
	real	x(*)	!table of x-coordinate values
	real	y(*)	!table of z-coordinate values
	real	z(*)	!table of z values to be fit
			!also output of residuals from the fit
	real	z2(*)	!table of z values to be fit
			!also output of residuals from the fit
	real	wt(*)	!table of data point weights
	real	wt2(*)	!table of data point weights

	integer	nvar	!number of functions to be fit
	integer	flist(*)	!list of which functions to fit to
	logical	screen	!=.true. if writing to teminal screen is wanted

c output data
c	real	z(*)	!table of z values to be fit
			!also output of residuals from the fit
c	real	z2(*)	!table of z values to be fit
			!also output of residuals from the fit
	real	zd	!current data value
	real	zd2	!current data value
	real	rmsbefore	!rms before fitting
	real	rmsbefore2	!rms before fitting
	real	rmsafter	!rms residual after fitting
	real	rmsafter2	!rms residual after fitting
	real	rms
	real	WTRMS	!function to calculate weighted RMS

c working parameters
	integer	icount, count	!count of non-zero weighted points
	integer	icount2	!count of non-zero weighted points
	real	sum2	!sum of squares
	real	sum22	!sum of squares
	real	worst	!deviation of worst point
	real	worst2	!deviation of worst point
	integer	iworst	!index of worst point
	integer	iworst2	!index of worst point
	real	wtsum	!sum of data weights
	real	wtsum2	!sum of data weights
	integer	idata	!data index, 1-imax
	integer	i, j, k, l	!matrix indices, 1-nvar
	integer	nterm	!maximum number of terms to be fit
	parameter( nterm=10 )
	real	w(nterm)	!AZO function values
	real	w2(nterm)	!ELO function values
	real	col(nterm)	!data column matrix
	real	mat(nterm,nterm)	!function matrix
	real	matinv(nterm,nterm)	!inverted function matrix
	integer	idgt	!number of accurate digit
	real	junk(100)	!working space for matrix inversion routine
	real	c(nterm)	!fitted constants
	integer	ier	!error flag from matrix inversion
	real	xyzfunc	!function to calculate the az for (x,y)
	real	xyzfunc2	!function to calculate the el for (x,y)
c	external	xyzfunc	!external function to return z as a function
c				!of function number, x and y
	character*20	funclabel	!function returning a text string
					!describing the function
	integer	iostat, ioerr	!IO error flags

	if( nvar .le. 0 ) then
	    if( screen ) 
	1  write(*,*,iostat=ioerr) ' No variables to be fitted, NVAR=', nvar
	    return
	endif

C Zero matrix and column and variables
	sum2 = 0.
	sum22 = 0.
	wtsum = 0.
	wtsum2 = 0.
	icount = 0.
	icount2 = 0.

	do i=1,nvar
		c(i) = 0.
		col(i) = 0.
		do j=1,nvar
			mat(i,j) = 0.
		enddo
	enddo

c start fitting procedure

	if( screen ) write(*,5,iostat=ioerr) imax, nvar
	if( logu .gt. 0 ) write(logu,5,iostat=ioerr) imax, nvar
5	format(' Fitting ', i6, ' data points to ', i2, ' variables' )
c	if( screen ) write(*,6,iostat=ioerr) (x(i),y(i),z(i),i=1,imax)
c	if( logu .gt. 0 )write(logu,6,iostat=ioerr) (x(i),y(i),z(i),i=1,imax)
c6	format(' X,Y,Z',3(1pg13.6))

c cycle through the data points
	do idata = 1, imax
		do i = 1, nvar
			w(i) = xyzfunc( flist(i), x(idata), y(idata) )
			w2(i) = xyzfunc2( flist(i), x(idata), y(idata) )
		enddo

		if( wt(idata) .gt. 0. ) then
			sum2 = sum2 + wt(idata)*z(idata)**2
			icount = icount + 1
			wtsum = wtsum + wt(idata)
		endif
		if( wt2(idata) .gt. 0. ) then
			sum22 = sum22 + wt2(idata)*z2(idata)**2
			icount2 = icount2 + 1
			wtsum2 = wtsum2 + wt2(idata)
		endif

c	write(*,101, iostat=ioerr) idata, x(idata), y(idata), z(idata)
c	1		, w(1), w(2), w(3), wt(idata) 
c101	format( i4, 'xyz', 3(1pg10.3), ' f=', 3(1pg10.3), ' wt=', 1pg10.3)

c Construct matrix and column loaded with the chi-squared equations
c derived from partial derivatives w.r.t. to the coefficients.
		do k=1,nvar
			col(k) = col(k) + wt(idata)*z(idata)*w(k)
     +					+ wt2(idata)*z2(idata)*w2(k)
			do l=1,nvar
				mat(k,l) = mat(k,l) + wt(idata)*w(k)*w(l)
     +						+ wt2(idata)*w2(k)*w2(l)
			enddo
		enddo
	enddo			!cycle through the data

	if( screen ) write(*,*,iostat=ioerr)
	1	icount, ' and', icount2, ' non-zero weighted points were found.'
	if( logu .gt. 0 ) write(logu,*,iostat=ioerr) 
	1	icount, ' and', icount2, ' non-zero weighted points were found.'

c calc rms before fitting
	if( wtsum .gt. 0. ) then
		rmsbefore = sqrt( sum2/wtsum )
		rmsbefore2 = sqrt( sum22/wtsum2 )
		if( screen ) write(*,'('' RMS before fitting; AZ='',f8.3
	1		, '', and EL='', f8.3, '' arcmin'')',iostat=ioerr) 
	2			rmsbefore, rmsbefore2

		if( logu .gt. 0 ) write(logu,'('' RMS before fitting; AZ='',f8.3
	1		, '', and EL='', f8.3, '' arcmin'')',iostat=ioerr) 
	2			rmsbefore, rmsbefore2
	else
		rmsbefore = sum2
		if( screen ) write(*,10,iostat=ioerr) wtsum, sum2
	if( logu .gt. 0 ) write(logu,10,iostat=ioerr) wtsum, sum2
10	format(' Before fitting, net weighting is ', e10.3, '<=0'
	1		,', sum**2=', e10.3 ) 
	endif

c check new WTRMS function
	rms = WTRMS(imax, z, wt, count)
	if( rms .ne. rmsbefore ) write(*,'('' RMS calc. discrepancy'',2pg13.3)'
	1		,iostat=ioerr) rms, rmsafter

c	write(*,*) ' matrix', mat
c	write(*,*) ' column', col

c check for enough data
	if( icount .lt. nvar .or. icount2 .lt. nvar ) then	!not enough
		write(*,'('' Not enough data to fit.'')')
		if( logu .gt. 0 ) write(logu,'('' Not enough data to fit.'')')
		do k = 1, nvar
			c(k) = 0.
		enddo
		goto 500
	endif

c----------------------------------------------------------------------------
c invert fitting matrix using IMSL routine LINV2F
	idgt = 0
	call linv2f(mat,nvar,nterm,matinv,idgt,junk,ier)	

	if( idgt .lt. 5 .or. screen )	!less than 6 digit accuracy
	1		write(*,'(i3,''  digit matrix inversion accuracy''
	1					,'' (routine FITXYZ)'')') idgt
	if( idgt .lt. 5 .or. logu .gt. 0 ) 
	1		write(logu,'(i3,''  digit matrix inversion accuracy''
	1					,'' (routine FITXYZ)'')') idgt
	if (ier .ne. 0) then
		write(*, '('' Error in inverting matrix (routine FITXYZ)'')')
		write(3, '('' Error in inverting matrix (routine FITXYZ)'')')
		if( ier .eq. 129 ) then
			write(*,'('' Matrix is singular''
	1						,'' (routine FITXYZ'')')
			write(3,'('' Matrix is singular''
	1						,'' (routine FITXYZ'')')
		endif
		if( ier .eq. 34 ) then
	write(*,'('' Less than '',i2,''  digit matrix inversion accuracy''
	1					,'' (routine FITXYZ'')') idgt
	write(3,'('' Less than '',i2,''  digit matrix inversion accuracy''
	1					,'' (routine FITXYZ'')') idgt
		endif
		if( ier .eq. 131 ) then
	write(*,'('' Too ill conditioned for iterative improvement''
	1					,'' (routine FITXYZ'')')
	write(3,'('' Too ill conditioned for iterative improvement''
	1					,'' (routine FITXYZ'')')
		endif
	endif
	
c multiply column vector by inverse of matrix to get solutions
	do k=1,nvar
		do l=1,nvar
			c(k) = c(k) + matinv(k,l)*col(l)
		enddo
	enddo

c write out the value for the fitted constants
500	if( screen ) write(*,21,iostat=ioerr) 
	1			 ( c(k), funclabel(flist(k)), k=1,nvar )
	if( logu .gt. 0 ) write(logu,21,iostat=ioerr) 
	1			( c(k), funclabel(flist(k)), k=1,nvar )
21	format( ' The best fit function is',/
	1	,1x, f8.3, 1x, a,/
	2	,1x, f8.3, 1x, a,/
	3	,1x, f8.3, 1x, a,/
	4	,1x, f8.3, 1x, a,/
	5	,1x, f8.3, 1x, a,/
	6	,1x, f8.3, 1x, a,/
	7	,1x, f8.3, 1x, a,/
	8	,1x, f8.3, 1x, a,/
	9	,1x, f8.3, 1x, a,/
	1	,1x, f8.3, 1x, a )
c	1	,1x, 1pg13.6, 1x, a,/
c	2	,1x, 1pg13.6, 1x, a,/
c	3	,1x, 1pg13.6, 1x, a,/
c	4	,1x, 1pg13.6, 1x, a,/
c	5	,1x, 1pg13.6, 1x, a,/
c	6	,1x, 1pg13.6, 1x, a,/
c	7	,1x, 1pg13.6, 1x, a,/
c	8	,1x, 1pg13.6, 1x, a,/
c	9	,1x, 1pg13.6, 1x, a,/
c	1	,1x, 1pg13.6, 1x, a )

c Calculate the RMS after fitting
	sum2 = 0.
	sum22 = 0.
	wtsum = 0.
	wtsum2 = 0.
	worst = 0.
	worst2 = 0.
	iworst = 1
	iworst2 = 1

	do idata = 1, imax
		zd = z(idata)
		if( wt(idata) .gt. 0. ) then
			do i = 1, nvar
			  zd = zd - c(i)*xyzfunc( flist(i), x(idata), y(idata) )
			enddo
			sum2 = sum2 + wt(idata)*zd**2
			wtsum = wtsum + wt(idata)
			if( abs(zd) .gt. worst ) then
				worst = abs(zd)
				iworst = idata
			endif
		endif
		zd2 = z2(idata)
		if( wt2(idata) .gt. 0. ) then
			do i = 1, nvar
		zd2 = zd2 - c(i)*xyzfunc2( flist(i), x(idata), y(idata) )
			enddo
			sum22 = sum22 + wt2(idata)*zd2**2
			wtsum2 = wtsum2 + wt2(idata)
			if( abs(zd2) .gt. worst2 ) then
				worst2 = abs(zd2)
				iworst2 = idata
			endif
		endif
	enddo


c Calc. rms after fitting
	if( wtsum .gt. 0. ) then
		rmsafter = sqrt( sum2/wtsum )
		if(screen ) write(*,'('' After fitting, AZrms='',f8.3
	1	      , '', Worst error='',f8.3)',iostat=ioerr) ,rmsafter, worst
		if( logu .gt. 0 ) write(logu,'('' After fitting, AZrms='',f8.3
	2	      , '', Worst error='',f8.3)',iostat=ioerr) ,rmsafter, worst
	else
		rmsafter = sum2
		if( screen ) write(*,11,iostat=ioerr) wtsum, sum2
		if( logu .gt. 0 ) write(logu,11,iostat=ioerr) wtsum, sum2
11	format(' Net weighting is ', e10.3, '<=0'
	1		,', sum**2=', e10.3 ) 
	endif

	if( wtsum2 .gt. 0. ) then
		rmsafter2 = sqrt( sum22/wtsum2 )
		if(screen ) write(*,'('' After fitting, ELrms='',f8.3
	1	    , '', Worst error='',f8.3)',iostat=ioerr) ,rmsafter2, worst2
		if( logu .gt. 0 ) write(logu,'('' After fitting, ELrms='',f8.3
	2	    , '', Worst error='',f8.3)',iostat=ioerr) ,rmsafter2, worst2
	else
		rmsafter2 = sum22
		if( screen ) write(*,11,iostat=ioerr) wtsum2, sum22
		if( logu .gt. 0 ) write(logu,11,iostat=ioerr) wtsum2, sum22
	endif

	return
	end

c------------------------------------------------------------------------------
	subroutine SUBFUNC( imax,x,y,z,z2,wt,wt2, nvar,flist
	1	, c,rmsbefore,rmsbefore2,rmsafter,rmsafter2,screen, logu )

C Subtract calculated function from data

	implicit	none
                                                          
c input data
	integer	logu	!unit number for logfile
	integer	imax	!number of data points
	real	x(*)	!table of x-coordinate values
	real	y(*)	!table of z-coordinate values
       	real	z(*)	!table of z values to be fit
			!also output of residuals from the fit
       	real	z2(*)	!table of z values to be fit
			!also output of residuals from the fit
	real	wt(*)	!table of data point weights
	real	wt2(*)	!table of data point weights
	integer	nvar	!number of functions to be fit
	integer	flist(*)	!list of which functions to fit to
	logical	screen	!=.true. if writing to teminal screen is wanted

c output data
c	real	z(*)	!table of z values to be fit
			!also output of residuals from the fit
	real	rmsbefore	!rms before fitting
	real	rmsbefore2	!rms before fitting
	real	rmsafter	!rms residual after fitting
	real	rmsafter2	!rms residual after fitting

c working parameters
	integer	icount	!count of non-zero weighted points
	integer	icount2	!count of non-zero weighted points
	real	sum2	!sum of squares
	real	sum22	!sum of squares
	real	wtsum	!sum of data weights
	real	wtsum2	!sum of data weights
	integer	idata	!data index, 1-imax
	integer	i, j, k, l	!matrix indices, 1-nvar
	real	c(*)	!fitted constants
	real	xyzfunc	!function to calculate the z for (x,y)
	real	xyzfunc2	!function to calculate the z for (x,y)
c	external	xyzfunc	!external function to return z as a function
c				!of function number, x and y
	character*20	funclabel	!function returning a text string
					!describing the function
	integer	iostat, ioerr	!IO error flags

c write out the value for the function coefficients
	if( screen ) write(*,21,iostat=ioerr) 
	1			 ( c(k), funclabel(flist(k)), k=1,nvar )
	if( logu .gt. 0 ) write(logu,21,iostat=ioerr) 
	1			( c(k), funclabel(flist(k)), k=1,nvar )
21	format( ' Subtracting the function ',/
	1	, 1pg13.6, 1x, a,/
	2	, 1pg13.6, 1x, a,/
	3	, 1pg13.6, 1x, a,/
	4	, 1pg13.6, 1x, a,/
	5	, 1pg13.6, 1x, a,/
	6	, 1pg13.6, 1x, a,/
	7	, 1pg13.6, 1x, a,/
	8	, 1pg13.6, 1x, a,/
	9	, 1pg13.6, 1x, a,/
	1	, 1pg13.6, 1x, a )

	sum2 = 0.
	sum22 = 0.
	wtsum = 0.
	wtsum2 = 0.

	do idata = 1, imax
		call SUBPNT(x(idata),y(idata),z(idata),z2(idata),nvar,flist,c)
		sum2 = sum2 + wt(idata)*z(idata)**2
		sum22 = sum22 + wt2(idata)*z2(idata)**2
		wtsum = wtsum + wt(idata)
		wtsum2 = wtsum2 + wt2(idata)
	enddo


c Calc. rms after fitting
	if( wtsum .gt. 0. ) then
		rmsafter = sqrt( sum2/wtsum )
		if(screen ) write(*,*,iostat=ioerr) 
	1			' After subtracting function, RMS=',rmsafter
		if( logu .gt. 0 ) write(logu,*,iostat=ioerr) 
	1			' After subtracting function, RMS=',rmsafter
	else
		rmsafter = sum2
		if( screen ) write(*,11,iostat=ioerr) wtsum, sum2
		if( logu .gt. 0 ) write(logu,11,iostat=ioerr) wtsum, sum2
11	format(' Net weighting is ', e10.3, '<=0'
	1		,', sum**2=', e10.3 ) 
	endif

	if( wtsum2 .gt. 0. ) then
		rmsafter2 = sqrt( sum22/wtsum2 )
		if(screen ) write(*,*,iostat=ioerr) 
	1			' After subtracting function, RMS=',rmsafter2
		if( logu .gt. 0 ) write(logu,*,iostat=ioerr) 
	1			' After subtracting function, RMS=',rmsafter2
	else
		rmsafter2 = sum22
		if( screen ) write(*,11,iostat=ioerr) wtsum2, sum22
		if( logu .gt. 0 ) write(logu,11,iostat=ioerr) wtsum2, sum22
	endif

	return
	end


c-----------------------------------------------------------------------------
	subroutine SUBPNT(x,y,z,z2,nvar,flist,c)

c subtract a function from a single data point

	implicit none

	real	x, y	!input independent parameters
	real	z, z2	!measured (dependent) data
	integer	nvar	!number of function terms used
	integer	flist(*)	!numerical list of terms
	real	c(*)	!function coefficients
	integer	i	!index of term number
	real	xyzfunc	!function for first datum z
	real	xyzfunc2	!function for second datum z2

	do i = 1, nvar
	    z = z - c(i)*xyzfunc( flist(i), x, y )
	    z2 = z2 - c(i)*xyzfunc2( flist(i), x, y )
	enddo

	return
	end	


c--------------------------------------------------------------------------
	character*(*) function funclabel(nvar)

c This function returns the character string describing the funcxzy(nvar)

	implicit	none

	character*20	label(16)/
c				  12345678901234567890
	1			 'AZ encoder zero   E1'
	2			,'EL encoder zero   E2'
	3			,'AZ/EL non-orthog. E3'
	4			,'N-S axis/position E4'
	5			,'E-W axis/position E5'
	6			,'AZ collimation    X1'
	7			,'EL collimation    X2'
	8			,'Elevation sag     X3'
	9			,'wobble cos(256*AZ)  '
	1			,'wobble sin(256*AZ)  '
	1			,'wobble cos(256*EL)  '
	2			,'wobble sin(256*EL)  '
	3			,'wobble cos(512*AZ)  '
	4			,'wobble sin(512*AZ)  '
	5			,'wobble cos(512*EL)  '
	6			,'wobble sin(512*EL)  '/

	integer	nvar	!function number

	funclabel = label(nvar)

	end

c-----------------------------------------------------------------------------
	real	function xyzfunc( nvar, az, el )

c on the sky azimuth collimation functions
c i.e. multiply encoder errors by cosd(el)
	
	implicit	none

c	real	xyzfunc	!calculated value
	integer	nvar	!number of function to be calculated
	real	az	!azimuth [degrees]
	real	el	!elevation [degrees]

c these are the functions to fit the on the sky collimation error data
	xyzfunc = 1.	!constant for default

	if( nvar .eq. 1 ) then	!E1
		xyzfunc = cosd(el)	!AZ encoder zero
		return
	endif

	if( nvar .eq. 2 ) then	!E2
		xyzfunc = 0.	!no EL offset effect on AZ
		return
	endif

	if( nvar .eq. 3 ) then	!E3
		xyzfunc = sind(el)	!non-orthogonal AZ and EL axes
		return
	endif

	if( nvar .eq. 4 ) then	!E4
		xyzfunc = -sind(az)*sind(el)	!North-South axis error
		return
	endif

	if( nvar .eq. 5 ) then	!E5
		xyzfunc = -cosd(az)*sind(el)	!East-West axis error
		return
	endif

	if( nvar .eq. 6 ) then	!X1
		xyzfunc = 1.	!AZ collimation error
		return
	endif

	if( nvar .eq. 7 ) then	!X2
		xyzfunc = 0.	!no AZ effect from EL collimation error
		return
	endif

	if( nvar .eq. 8 ) then	!X3
		xyzfunc = 0.	!no AZ effect from EL sag error
		return
	endif

	if( nvar .eq. 9 ) then
		xyzfunc = cosd(256*az)*cosd(el)	!256 theta encoder error
		return
	endif

	if( nvar .eq. 10 ) then
		xyzfunc = sind(256*az)*cosd(el)	!256 theta encoder error
		return
	endif

	if( nvar .eq. 11 ) then
		xyzfunc = 0.
		return
	endif

	if( nvar .eq. 12 ) then
		xyzfunc = 0.
		return
	endif

	if( nvar .eq. 13 ) then
		xyzfunc = cosd(512*az)*cosd(el)	!512 theta encoder error
		return
	endif

	if( nvar .eq. 14 ) then
		xyzfunc = sind(512*az)*cosd(el)	!512 theta encoder error
		return
	endif

	if( nvar .eq. 15 ) then
		xyzfunc = 0.
		return
	endif

	if( nvar .eq. 16 ) then
		xyzfunc = 0.
		return
	endif



	end


c-----------------------------------------------------------------------------
	real	function xyzfunc2( nvar, az, el )

c on the sky elevation collimation functions
	
	implicit	none

c	real	xyzfunc2	!calculated value
	integer	nvar	!number of function to be calculated
	real	az	!azimuth [degrees]
	real	el	!elevation [degrees]

c these are the functions to fit the on the sky collimation error data
	xyzfunc2 = 1.	!constant for default

	if( nvar .eq. 1 ) then	!E1
		xyzfunc2 = 0.	!no AZ offset effect of EL
		return
	endif

	if( nvar .eq. 2 ) then	!E2
		xyzfunc2 = 1.	!EL offset
		return
	endif

	if( nvar .eq. 3 ) then	!E3
		xyzfunc2 = 0.	!no non-orthogonal axes effect on EL
		return
	endif

	if( nvar .eq. 4 ) then	!E4
		xyzfunc2 = -cosd(az)	!North-South axis error
		return
	endif

	if( nvar .eq. 5 ) then	!E5
		xyzfunc2 = sind(az)	!East-West axis error
		return
	endif

	if( nvar .eq. 6 ) then	!X1
		xyzfunc2 = 0.	!no effect form AZ collimation error
		return
	endif

	if( nvar .eq. 7 ) then	!X2
		xyzfunc2 = 1.	!EL collimation error, same as nvar=2
		return
	endif

	if( nvar .eq. 8 ) then	!X3
		xyzfunc2 = cosd(el)	!EL sag error
		return
	endif

	if( nvar .eq. 9 ) then
		xyzfunc2 = 0.
		return
	endif

	if( nvar .eq. 10 ) then
		xyzfunc2 = 0.
		return
	endif

	if( nvar .eq. 11 ) then
		xyzfunc2 = cosd(256*el)	!256 theta encoder error
		return
	endif

	if( nvar .eq. 12 ) then
		xyzfunc2 = sind(256*el)	!256 theta encoder error
		return
	endif

	if( nvar .eq. 13 ) then
		xyzfunc2 = 0.
		return
	endif

	if( nvar .eq. 14 ) then
		xyzfunc2 = 0.
		return
	endif

	if( nvar .eq. 15 ) then
		xyzfunc2 = cosd(512*el)	!512 theta encoder error
		return
	endif

	if( nvar .eq. 16 ) then
		xyzfunc2 = sind(512*el)	!512 theta encoder error
		return
	endif

	end


c-----------------------------------------------------------------------------
	function WTRMS(npts,y,wt,icount)

c This routine calculates the weighted RMS for a data set
c It does not subtract the average

	implicit none

c----------------INPUTS--------------------
	integer	npts	!number of input data points
	real	y(*)	!input data array
	real	wt(*)	!weighting to be applied to data

c----------------OUTPUTS--------------------
	integer	icount	!number of non-zero weighted points
	real	WTRMS	!weighted RMS of data set

c----------------WORKING VARIABLES--------------------
	integer	i	!index
	real	wtsum	!sum of data weights
	real	sum2	!weighted sum of squares of data


c initialize sums
	icount = 0
	wtsum = 0
	sum2 = 0
	do i = 1, npts
		if( wt(i) .gt. 0. ) then
			icount = icount + 1
			wtsum = wtsum + wt(i)
			sum2 = sum2 + wt(i)*y(i)**2
		endif
	enddo

	if( wtsum .gt. 0. ) then
		wtrms = sqrt( sum2/wtsum )
	else	!no data
		wtrms = -1.
	endif

	return
	end
