c these are the data statements that belong to data.inc
c in ANSI compilers they can only occur once 
c June 2005:    VAX code ported to Linux 		Peter Teuben -    
c
	data (master(i,1),i=0,2)/2,             6, 7   /  !            X1,X2
	data (master(i,2),i=0,2)/2, 1,             7   /  !E1,            X2
	data (master(i,3),i=0,3)/3,    3,          7, 8/  !   E3,         X2,X3
	data (master(i,4),i=0,3)/3, 1,          6, 7   /  !E1,         X1,X2
	data (master(i,5),i=0,4)/4, 1, 3,       6, 7   /  !E1,E3,      X1,X2
	data (master(i,6),i=0,5)/5,       4, 5, 6, 7, 8/  !      E4,E5,X1,X2,X3
	data (master(i,7),i=0,5)/5,    3, 4, 5, 6, 7   /  !   E3,E4,E5,X1,X2
	data (master(i,8),i=0,6)/6, 1,    4, 5, 6, 7, 8/  !E1,   E4,E5,X1,X2,X3
	data (master(i,9),i=0,7)/7, 1, 3, 4, 5, 6, 7, 8/  !E1,E3,E4,E5,X1,X2,X3

	data (raster(i,1),i=0,2)/2,             6, 7   /  !            X1,X2
	data (raster(i,2),i=0,3)/3,             6, 7, 8/  !            X1,X2,X3
c
	data	bigdot/2./	!size of biggest dot for plots
	data	ploterr/.5/ !threshhold to use for selecting data to be plotted	
	data	gain/2./	!for converting error to vectors 

c
	data		logu/12/	!output log unit #
	data		constu/13/	!output pointing constants unit #
c
	data	screen/.true./	!=.true. if writing to teminal screen is wanted
c
	data		same/.true./	!=.true. if same as last time
	data		sameterms/.true./ !=.true. if same terms are to be fit
	data		sameplots/.true./ !=.true. if same plots are to be made
	data		azelplots/.true./ !=.true. to plot offset vs. AZ and EL
	data		timeplots/.true./ !=.true. to plot offset vs. time 
	data		diurnal	/.true./  !=.true. to plot offset vs. time of day
	data		hair/.true./	!=.true. to make vector hairball plot
	data		hardc/.true./	!=.true. to make hard copy of plots
	data		subfit/.true./  !=.true. if fitted terms are to be subtracted
	data		wobble/.false./	!=.true. if wobble fit is to be done

