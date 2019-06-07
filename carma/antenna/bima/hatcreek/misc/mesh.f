c*
	PROGRAM MESH
c& wh
c= MESH - trims the bright star list for each segment of the sky. 
c: misc
c+
c
c	MESH trims the bright star list down to one star
c	in each segment of the sky. The inputs are obtained
c	interactively.
c
c--
c	unix-> jun 91  wh
c---------------------------------------------------------------------c
	real*4 dclo(100),dchi(100),ralo(100,100),rahi(100,100)
	integer*4 nzones(100),ndc,winner
	real*8 sra(6000),sdec(6000)
	real*4 spmr(6000),spmd(6000),vmag(6000)
	character*50 infile,outfile
	character*10 sname(6000),aname,cat*3
	integer ddec,mdec,hra,mra
	character*14 angles,a1,a2

c	--input the number of zones--
	TYPE *,'MESH PRUNES THE MASTER STAR LIST TO PROVIDE THE BRIGHTEST'
	TYPE *,' STAR IN EACH SQUARE ZONE OVER THE SKY'
	TYPE *,' ENTER THE NUMBER OF ZONES AROUND THE EQUATOR:'
	ACCEPT *,NZ
	TYPE *,'ENTER THE INPUT FILE NAME:'
	ACCEPT 110,INFILE
110	format(a)
	TYPE *,'ENTER THE OUTPUT FILE NAME:'
	ACCEPT 110,OUTFILE

	OPEN(1,NAME=INFILE,STATUS='OLD',ERR=50)
	GOTO 51
50	TYPE *,'INPUT FILE ERROR'
	STOP
51	OPEN(2,NAME=OUTFILE,STATUS='unknown')

	NZ=(NZ+2)/4*4
	TYPE *,' NUMBER OF ZONES CORRECTED TO:',NZ

	NDC=NZ/2
	DCSTEP=180./NDC
	TYPE *,'DECLINATION SIZE=',DCSTEP

	DO I=1,NDC
	  DCHI(I)=90.-(I-1)*DCSTEP
	  DCLO(I)=DCHI(I)-DCSTEP
	  NZONES(I)=MAX(1.,NZ*COSD(MIN(ABS(DCHI(I)),ABS(DCLO(I)))))
	  RASTEP=24./NZONES(I)
	  TYPE *,NZONES(I),' ZONES AT DECLINATION=',(DCHI(I)+DCLO(I))/2.,RASTEP

	  DO J=1,NZONES(I)
	    RALO(I,J)=(J-1)*RASTEP
	    RAHI(I,J)=RALO(I,J)+RASTEP
	  END DO
	END DO


	I=1
10	  READ(1,100,END=12) CAT,ANAME,HRA,MRA,ScRA,DDEC,MDEC,ScDEC,
     .	   SPMR(I),SPMD(I),VMAG(I)
	  SNAME(I) = CAT//ANAME
	  SRA(I) = HRA + MRA/60.D0 + ScRA/3600.D0
	  SDEC(I)= SIGN(DBLE(ABS(DDEC)+ MDEC/60.D0+ ScDEC/3600.D0), DBLE(DDEC))
100	  FORMAT
     .	      (A3,x,A6,3X,i2,X,I2,X,F4.1,I3,X,i2,X,F2.0,F6.3,F5.2,
     .		17X,F3.1)
	I=I+1
	GOTO 10
12	NIN=I-1

C	--PUT THE BRIGHTEST INTO EACH ZONE--
	DO IDC=1,NDC

	  DO IRA=1,NZONES(IDC)
	   VMAGMAX=10.
	   WINNER=0
	   DO I=1,NIN
	    IF(VMAG(I).LT.VMAGMAX) THEN
		IF((SRA(I).GT.RALO(IDC,IRA))
	1	   .AND.(SRA(I).LT.RAHI(IDC,IRA))
	2	    .AND.(SDEC(I).GT.DCLO(IDC))
	3	     .AND.(SDEC(I).LT.DCHI(IDC))) THEN
			VMAGMAX=VMAG(I)
			WINNER=I
		END IF
	    END IF
	   END DO
	   I=WINNER
	  a1 = ANGLES(SRA(I))
	  a2 = angles(sdec(i))
	  WRITE(2,101) SNAME(I)(1:3),SNAME(I)(4:10),a1,a2,
     .	   	SPMR(I),SPMD(I),VMAG(I)
101	  FORMAT(A3,x,A6,x,a11,x,a8,F6.3,F5.2,17X,F3.1)

	   TYPE *,'WINNER FOR ',IRA,IDC, ' IS ',I,x,a1,a2,VMAG(I)

	  END DO
	END DO


	end
