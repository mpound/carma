c*
	PROGRAM COMMENT
c& wh
c= COMMENT - adds comment lines to the log.date file.
c: misc
c+
c	COMMENT adds comment lines to the log.date file.
c@ is
c	The comment; if blank, COMMENT prompts for the comment lines.
c   Type control D to exit.
c	If the comment following IS contains blanks, it must be quoted(").
c
c--
c	unix-> jun 91  wh
c---------------------------------------------------------------------c
	integer lk(40)
	character*80 commnt(40)
	character*70 argv,command
	logical keyprsnt

C	--LOOK AT COMMAND LINE FOR ANY WORD
	call keyini
	command = ' '
	ic = 1
	argv = ' '
	do while (keyprsnt('is'))
	  call keya('is',argv,'.')
	  ia = lnblnk(argv)
	  command(ic:ic+ia+1) = argv(:ia)//' '
	  ic = ic+ia+1
	end do
	call keyfin	

C	--output the comments--
	IF(ic .gt. 1) THEN
	  CALL LOGIT(COMMAND(:IC))
	ELSE
	  K=1
	  L=1
	  DO WHILE (K.GT.0)
	   TYPE 100,'comment>'
100	   FORMAT(A,$)
	   READ(5,101,END=10)COMMNT(L)
101	   FORMAT(A)
	   LK(L)=K
	   IF(K.GT.0)L=L+1
	  END DO

10	  DO I=1,L	  
	    CALL LOGIT(COMMNT(I)(:lnblnk(commnt(i))))
	  END DO
	END IF
	END
