c*
	PROGRAM CATALOG
c& wh
c= CATALOG - builds and maintains source catalogs.  
c: program
c+
c	CATALOG builds and maintains source catalogs.  
c	The standard repository catalogs reside in directory $HATCAT.
c	The information entered for each source is:
c	   Name (8 chars)
c	   Position (j2000 only) (use MIRIAD program j2000 to convert)
c	   Proper Motion in RA and DEC
c	   Velocity (LSR).
c
c@ catalog
c	The name of the catalog file; the catalog will be created if it 
c       doesn't exist after you are asked to affirm the creation. No default.
c       Note: the standard repositories are in $HATCAT and $UCAT.
c
c@ infile
c	The name of an ascii file of source positions to be read into
c	the current catalog.  The "O" style input records look like this:
c	 CAT,SNAME,HRA,MRA,SRA,DDEC,MDEC,SDEC,PRA,PDEC,V
c	  (A1,3x,A6,3X,i2,X,I2,X,F4.1,I3,X,i2,X,F2.0,F6.3,F5.2,17X,F3.1)
c	 which is the format of a listing of bright stars.
c	The "V" style input record is from the vla calibrators file
c	which is obtained from vlais@zia.nrao.edu using commands:
c	vlais - vla - cal - csou and mailing the results.
c
c@ log
c	The name of a file to accept the listing of the catalog (in
c	ascending RA order). If left blank, CATALOG asks for user
c	input of source positions and velocities, and the listing does
c	not happen.  To list on the terminal, use /dev/tty.
c
c@ initials
c	The observer's initials (up to 4 chars) to keep track of where
c	catalog positions come from. This is required if you're modifying
c       or creating a file.
c
c--
c	nov 89 wh
c	unix-> nov 90 wh
c	jul 93 wh - various changes: keywords,formats,catalog name
c	jul 93 pjt - minor cosmetic changes in error handling and
c                    opening new catalog
c	jun 97 wh - ask before creating a catalog
c
c Bugs:
c   new catalogs on which one tries to use log= and initials= are
c   created empty
c----------------------------------------------------------------c
	character version*(*)
	parameter(version='CATALOG (version 1.3 30-jun-97)')
	include '../inc/catalog.inc'
	character*80 error
	character*80 infile,listfile
	character catname*80, initials*4

	call output(version)
	call keyini
	call keyf('catalog',catname,' ')
	call keyf('infile',infile,' ')
	call keyf('log',listfile,' ')
	call keya('initials',initials,' ')
	call keyfin

	IF (CATNAME.EQ.' ') CALL BUG('f',
     *		'No catalog specified (catalog=)')
	CALL OPEN_CATALOG(CATNAME,INITIALS,ERROR)
	IF(error(1:2).ne.'OK') CALL BUG('f',ERROR)

	IF(INFILE.NE.' ') THEN
	   IF(INITIALS.NE.' ') THEN
              CALL GET_FROM_FILE(INFILE,INITIALS,ERROR)
              IF(ERROR(1:2).NE.'OK') CALL BUG('f',ERROR)
	   ELSE
              CALL BUG('f','Initials needed (initials=)')
	   END IF
	END IF

	IF(LISTFILE.NE.' ') THEN
	   CALL LIST_CATALOG(CATNAME,LISTFILE,ERROR)
           IF(ERROR(1:2).NE.'OK') CALL BUG('f',ERROR)
        ELSE
          IF(INITIALS.NE.' ') THEN
            ERROR = 'OK'
	    DO WHILE (ERROR(1:2).EQ.'OK')
	      CALL PROCESS_SOURCE(CATNAME,INITIALS,ERROR)
	    END DO
	  ELSE
	    CALL BUG('f','Initials needed (initials=) to process source')
	  END IF
        ENDIF

	END
c
c  OPEN_CATALOG
c: observing
c+
	SUBROUTINE OPEN_CATALOG(CATALOG,INITIALS,ERROR)
c
	character*(*) catalog,initials,error
c
c  OPEN_CATALOG:	Opens the catalog file for
c			read/write and returns error.
c
c  ARGUMENTS:
c  CATALOG (char,input)	Name of a catalog file.
c  INITIALS (char,input)Guy who wants to edit it
c  ERROR (char,output)	Status return
c--
c	nov 89 wh
c----------------------------------------------------------------c
	include '../inc/catalog.inc'
	record /catalog_first/ head
	character*80 filename,gerror
	character*10 desire
        integer len1

	filename = catalog
	lf = len1(filename)

	CLOSE (12)
	OPEN (unit=12, file=filename(1:lf), status='old',
	2	access='direct', form='unformatted', recl=80,  iostat=is)
	IF(IS.NE.0) THEN
          IF(INITIALS.NE.' ') THEN
	     type '(a,a,a,$)','I am about to create '//filename(:lf)//
	1	' IS THIS WHAT YOU WANT??(yes/no)'
	     accept '(a)',desire
	     call ucase(desire)
	     if(desire(1:3).ne.'YES') return
	     OPEN (unit=12, file=filename(1:lf), status='NEW',
	2	access='direct', form='unformatted', recl=80,  iostat=is)
             IF (is.eq.0) THEN
	        HEAD.IEND = 1
	        WRITE(12'1) HEAD
             ENDIF
          ELSE
             ERROR='Need initials to create a new catalog'
             return
          ENDIF
	END IF
	error = 'OK'
	if(is.ne.0) error = filename(1:lf)//' '//GERROR()
	return
	end
c
c  GET_FROM_FILE
c: observing
c+
	SUBROUTINE GET_FROM_FILE(FILE,INITIALS,ERROR)
c
c  GET_FROM_FILE:	Read various types of ascii files and put the 
c			sources into a catalog.
C		File format "O" is used for Jerrys optical list: 
c	 CAT,SNAME,HRA,MRA,SRA,DDEC,MDEC,SDEC,PRA,PDEC,V
c       (A1,3x,A6,X,i2,X,I2,X,F5.2,I3,X,i2,X,F2.0,F6.3,F5.2,17X,F3.1)
c
c		File format "V" is used for the vla list obtained from
c		vlais@zia.nrao.edu (type vla & cal & csou)
c
c  ARGUMENTS:
c  FILE (char,input)		Name of the input file
C  INITIALS(char*4,input)	Observer's initials
c  ERROR (char,output)		Status return
c--
c	nov 89 wh
c----------------------------------------------------------------c
	character*(*) file,initials
	include '../inc/catalog.inc'
	record /catalog_entry/ entry
	real*8 thera,thedec
	integer hra,mra,ddec,mdec
	real*8 sra,sdec,pra,pdec,v
	character*6 sname
	character*8 upper
	character*1 cat,which
	character*80 error,line
	character*12 alternate
	character*15 rastring,dcstring
	double precision xstrlflt

	OPEN (11,FILE=FILE,STATUS='OLD',FORM='FORMATTED',IOSTAT=IS)
	IF (IS.NE.0) THEN
		ERROR = 'Can''t open ascii file '//file(1:20)
		RETURN
	END IF

	type '(a,$)','Which type of input file (O or V)(Optical or VLA)?'
	accept '(a)',which
	call ucase(which)

c  --	Optical catalog input section (output of MESH)
	if(which.eq.'O') then
	  ISIN = 0
	  DO WHILE (ISIN.EQ.0)
	    READ(11,100,IOSTAT=ISIN) CAT,SNAME,HRA,MRA,SRA,DDEC,MDEC,SDEC,
     .	     PRA,PDEC,V
	    THERA = HRA + MRA/60.D0 + SRA/3600.D0
	    THEDEC= SIGN(DBLE(ABS(DDEC)+ MDEC/60.D0+ SDEC/3600.D0), DBLE(DDEC))
100	    FORMAT(A1,3x,A6,X,i2,X,I2,X,F5.2,I3,X,i2,X,F2.0,F6.3,F5.2,
     .			17X,F3.1)
	    UPPER = CAT//SNAME
	    CALL UCASE(UPPER)
	    ENTRY.NAME   = UPPER
	    ENTRY.RA     = THERA
	    ENTRY.PMRA   = PRA
	    ENTRY.DEC    = THEDEC
	    ENTRY.PMDEC  = PDEC
	    ENTRY.VEL    = V
	    ENTRY.EPOCH  = 2000.
	    ENTRY.OBSERVED = 0.
	    ENTRY.COMMENTS = file
	    ENTRY.INITIALS = INITIALS
	    CALL CATFIND(CAT//SNAME,'ENTER',ENTRY,ERROR)
	    IF (ERROR(1:2).NE.'OK')  TYPE *,CAT//SNAME,' error ',error
	  END DO
	  ERROR = 'OK'

c  --	VLA calibrators input section
	else if (which.eq.'V') then
	  type '(a,$)','What is the minimum flux desired at 2cm (J)?'
	  accept *,fluxmin
10	  read(11,'(a)',end=90) line
	    if(line(1:2).eq.'--' .or. line(1:2).eq.'==') goto 10
	    if(line(1:3).eq.'IAU' .or. line(1:4).eq.'BAND') goto 10
	    if(line(13:16).eq.'2000') then
	      rastring = line(21:33)
	      rastring(index(rastring,'h'):index(rastring,'h')) = ':'
	      rastring(index(rastring,'m'):index(rastring,'m')) = ':'
	      ENTRY.RA     = XSTRLFLT(rastring,error)
		if(error(1:2).ne.'OK') type *,line,error
	      ENTRY.PMRA   = 0.
	      dcstring = line(36:48)
	      dcstring(index(dcstring,'d'):index(dcstring,'d')) = ':'
	      dcstring(index(dcstring,''''):index(dcstring,'''')) = ':'
	      ENTRY.DEC    = XSTRLFLT(dcstring,error)
		if(error(1:2).ne.'OK') type *,line(:30),error
	      ENTRY.PMDEC  = 0.
	      ENTRY.NAME   = line(:8)
	      ENTRY.VEL    = 0.
	      ENTRY.EPOCH  = 2000.
	      ENTRY.OBSERVED = 0.
	      ENTRY.INITIALS = INITIALS
	      alternate = ' '
	    else if (line(:9) .eq. 'ALTERNATE') then
	      alternate = line(18:28)
	    else if (line(3:5).eq.'2cm') then
	      read(line,'(25x,f5.2)') flux2
	      if (flux2.ge.fluxmin) then
	        ENTRY.COMMENTS = '2cm FLUX: '//line(26:31)
	       CALL CATFIND(ENTRY.NAME,'ENTER',ENTRY,ERROR)
	       IF (ERROR(1:2).NE.'OK')  TYPE *,ENTRY.NAME,' error ',error
	       if(alternate.ne.' ') then
		ENTRY.NAME = alternate
	        CALL CATFIND(ENTRY.NAME,'ENTER',ENTRY,ERROR)
	        IF (ERROR(1:2).NE.'OK')  TYPE *,ENTRY.NAME,' error ',error
	       end if
	      end if
	    end if
	  goto 10

90	  ERROR = 'OK'
	else
	  error = 'Bad option selected'
	end if
	CLOSE(11)
	type *,error
	RETURN
	END
c
c  PROCESS_SOURCE
c: observing
c+
	SUBROUTINE PROCESS_SOURCE(CATNAME,INITIALS,ERROR)
c
c  PROCESS_SOURCE:	Prompts for the values for 1 source and writes it
c			into the catalog. At each input, 5 actions are
c			possible:  number = new value, CR = use current value,
c			delete = delete the source, abort = exit with no action
c
c  ARGUMENTS:
c  CATNAME (char,input)		Catalog name
C  INITIALS (char*4,input)	Users initials
c  ERROR (char,output)		Status return
c--
c	nov 89 wh
c----------------------------------------------------------------c
	character*(*) error,initials,catname
	include '../inc/catalog.inc'
	record /catalog_entry/ entry
	character*8 source
	character*24 string
	character*14 angles,a1
	real*8 xstrlflt
	logical exists,cr,delete,abort,exit,list

	TYPE *,'Enter/Manipulate a catalog entry:'
	TYPE *,'	At each prompt, you may type:'
	TYPE *,'		<RETURN>	use current value'
	TYPE *,'		delete 		Delete the source'
	TYPE *,'		abort		Abort work on this source'
	TYPE *,'		list		List the catalog'
	TYPE *,'		CTRL/D		EXIT from loop'

5	IS = 0
	TYPE 102,'SOURCE Name (char*8)','  '
	CALL STRINGIN(SOURCE,DELETE,CR,ABORT,LIST,EXIT)
	 CALL UCASE(SOURCE)
	 IF(DELETE) GOTO 91
	 IF(CR) GOTO 91
	 IF(.NOT.LIST) GOTO 6
	    CALL LIST_CATALOG(CATNAME,'/dev/tty',ERROR)
	    GO TO 5
6	 IF(ABORT .OR. EXIT) GOTO 91

	CALL CATFIND(SOURCE,'G',ENTRY,ERROR)
	IF(ERROR(1:2).NE.'OK') GOTO 10
	exists = .true.
	GOTO 20

10	ENTRY.NAME   = SOURCE
	ENTRY.RA     = 0.D0
	ENTRY.PMRA   = 0.D0
	ENTRY.DEC    = 0.D0
	ENTRY.PMDEC  = 0.D0
	ENTRY.VEL    = 0.
	ENTRY.EPOCH  = 2000.
	ENTRY.OBSERVED = 0.
	ENTRY.COMMENTS = ' '
	ENTRY.INITIALS = INITIALS
	exists = .false.

20	a1 = ANGLES(ENTRY.RA)
	TYPE 102,'Right Ascension(J2000)(h:m:s)',a1
102	FORMAT(A,' [',A,']',T50,'<',$)
	CALL STRINGIN(STRING,DELETE,CR,ABORT,LIST,EXIT)
	 IF(DELETE) GOTO 90
	 IF(CR) GOTO 21
	 IF(ABORT .OR. EXIT .OR. LIST) GOTO 91
	ENTRY.RA = XSTRLFLT(STRING,ERROR)
	 IF (ERROR(1:2).NE.'OK') GOTO 20

21	TYPE 103,'PM for RA (sec per year)',ENTRY.PMRA
103	FORMAT(A,' [',F10.3,']',T50,'<',$)
	CALL STRINGIN(STRING,DELETE,CR,ABORT,LIST,EXIT)
	 IF(DELETE) GOTO 90
	 IF(CR) GOTO 22
	 IF(ABORT .OR. EXIT .OR. LIST) GOTO 91
	ENTRY.PMRA = XSTRLFLT(STRING,ERROR)
	 IF (ERROR(1:2).NE.'OK') GOTO 21

22	a1 = ANGLES(ENTRY.DEC)
	TYPE 102,'Declination(J2000)(d:m:s)',a1
	CALL STRINGIN(STRING,DELETE,CR,ABORT,LIST,EXIT)
	 IF(DELETE) GOTO 90
	 IF(CR) GOTO 23
	 IF(ABORT .OR. EXIT .OR. LIST) GOTO 91
	ENTRY.DEC = XSTRLFLT(STRING,ERROR)
	 IF (ERROR(1:2).NE.'OK') GOTO 22

23	TYPE 103,'PM for DEC (" per year)',ENTRY.PMDEC
	CALL STRINGIN(STRING,DELETE,CR,ABORT,LIST,EXIT)
	 IF(DELETE) GOTO 90
	 IF(CR) GOTO 24
	 IF(ABORT .OR. EXIT .OR. LIST) GOTO 91
	ENTRY.PMDEC = XSTRLFLT(STRING,ERROR)
	 IF (ERROR(1:2).NE.'OK') GOTO 23

24	TYPE 103,'LSR Velocity (km/sec)',ENTRY.VEL
	CALL STRINGIN(STRING,DELETE,CR,ABORT,LIST,EXIT)
	 IF(DELETE) GOTO 90
	 IF(CR) GOTO 25
	 IF(ABORT .OR. EXIT .OR. LIST) GOTO 91
	ENTRY.VEL = XSTRLFLT(STRING,ERROR)
	 IF (ERROR(1:2).NE.'OK') GOTO 24

25	TYPE 103,'Observed EPOCH (year)',ENTRY.OBSERVED
	CALL STRINGIN(STRING,DELETE,CR,ABORT,LIST,EXIT)
	 IF(DELETE) GOTO 90
	 IF(CR) GOTO 26
	 IF(ABORT .OR. EXIT .OR. LIST) GOTO 91
	ENTRY.OBSERVED = XSTRLFLT(STRING,ERROR)
	 IF (ERROR(1:2).NE.'OK') GOTO 25

26	TYPE 102,'COMMENTS',ENTRY.COMMENTS
	CALL STRINGIN(STRING,DELETE,CR,ABORT,LIST,EXIT)
	 IF(DELETE) GOTO 90
	 IF(CR) GOTO 27
	 IF(ABORT .OR. EXIT .OR. LIST) GOTO 91
	ENTRY.COMMENTS = STRING(1:24)

27	IF(EXISTS) THEN
	  CALL CATFIND(SOURCE,'CHANGE',ENTRY,ERROR)
	ELSE
	  CALL CATFIND(SOURCE,'ENTER',ENTRY,ERROR)
	END IF
	GOTO 91

90	CALL CATFIND(SOURCE,'DELETE',ENTRY,ERROR)

91	IF(EXIT) ERROR = 'END OF INPUT'
	RETURN
	END

	SUBROUTINE STRINGIN(STRING,DELETE,CR,ABORT,LIST,EXIT)
	character*(*) string
	logical delete,cr,abort,exit,list

	DELETE = .FALSE.
	CR = .FALSE.
	ABORT = .FALSE.
	LIST = .FALSE.
	EXIT = .FALSE.

	READ(5,'(Q,A)',END=90) K,STRING
	IF(K.EQ.0) THEN
		CR = .TRUE.
	ELSE IF((INDEX(STRING,'DELETE').GT.0) 
	1		.OR. (INDEX(STRING,'delete').gt.0)) THEN
		DELETE = .TRUE.
	ELSE IF((INDEX(STRING,'ABORT').GT.0)
	1		 .OR. (INDEX(STRING,'abort').gt.0)) THEN
		ABORT = .TRUE.
	ELSE IF((INDEX(STRING,'LIST').GT.0)
	1		 .OR. (INDEX(STRING,'list').gt.0)) THEN
		LIST = .TRUE.
	END IF
	RETURN
90	EXIT = .TRUE.
	RETURN
	END
c
c  LIST_CATALOG
c: observing
c+
	SUBROUTINE LIST_CATALOG(CATNAME,LIST,ERROR)
c
c  LIST_CATALOG:	Make an ascii listing of the current catalog in RA
c			order.
c
c
c  ARGUMENTS:
c  CATNAME (char,input)	Catalog name
c  LIST (char,input)	Name of the listing file
c  ERROR (char,output)	Status return
c--
c	nov 89 wh
c----------------------------------------------------------------c
	character*(*) list,error,catname
	
	character*130 line
	include '../inc/catalog.inc'
	record /catalog_entry/ entries(10000), saveit
	record /catalog_first/ head

	TYPE *,' Open Listing file '//list
	OPEN (13,FILE=LIST,STATUS='UNKNOWN',IOSTAT=IS)
	IF (IS.NE.0) THEN
		ERROR = 'Can''t open listing file '//list(1:20)
		RETURN
	END IF

c  -- read in the entire catalog, note the fake '***' user since read-only
        CALL OPEN_CATALOG(CATNAME,'***',ERROR)
	IF (ERROR(1:2).NE.'OK') RETURN

	ERROR = 'OK'
	READ(12'1) HEAD
	DO I=2,HEAD.IEND
	  READ(12'I) ENTRIES(I-1)
	END DO
	NUM = I-2

c  -- sort it by RA
	DO I=1,NUM-1
	    JLO=I
	  DO J=I+1,NUM
	   IF(ENTRIES(J).RA .LT. ENTRIES(JLO).RA) JLO = J
	  END DO
	  IF(JLO.NE.I) THEN
		SAVEIT = ENTRIES(I)
		ENTRIES(I) = ENTRIES(JLO)
		ENTRIES(JLO) = SAVEIT
	  END IF
	END DO

	IF(LIST(1:8).EQ.'/dev/tty') THEN
	  NL=78
	 WRITE(13,'(A,a)') 'Source     RA (2000)    PMra(s)    DEC (2000)   '//
	1	'PMdec(")    Veloc      Date'
	ELSE
	  NL=130
	 WRITE(13,'(A,a)') 'Source     RA (2000)    PMra(s)    DEC (2000)   '//
	1	'PMdec(")    Veloc      Date         Comments'
	END IF
	DO I=1,NUM
	  CALL FORMAT(ENTRIES(I),LINE)
	  WRITE(13,'(A)',IOSTAT=IS) LINE(1:NL)
	  IF(IS.NE.0) THEN
		ERROR = 'Can''t write listing file '//list(1:20)
		RETURN
	  END IF
	END DO
	CLOSE(13)
	RETURN
	END
c
c  FORMAT
c: observing
c+
	SUBROUTINE FORMAT(REC,LINE)
c
c  FORMAT:	Formats a catalog record as an ascii line for printing.
c
c  ARGUMENTS:
c  REC (record,input)	The catalog record
c  LINE (char,output)	The output line
c--
c	nov 89 wh
c----------------------------------------------------------------c
	include '../inc/catalog.inc'
	record /catalog_entry/ rec
	character*(*) line
	character*14 angles,ara,adec

	ara = Angles(rec.ra)
	adec = Angles(rec.dec)
	write(line,'(a,x,a,a,f7.3,a,A,a,f7.3,a,f8.1,a,f6.1,a,a,a,a)',iostat=is)
	1  rec.name,ara,'(',rec.pmra,') ',adec,
	2    '(',rec.pmdec,') ',rec.vel,' km/s ',rec.observed,' ',
	3      rec.comments,'-',rec.initials

	RETURN
	END
