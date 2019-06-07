/** WATCH	-  inspect values periodically				*/
/*: misc								*/
/*+
	PROGRAM WATCH
	WATCH monitors variables and thermister values.
	The list of things to monitor is in file:
	$HATTAB/watch.ascii and WATCH produces a file
	containing columns of values suitable for plotting
	using WIP that is named $HATWATCH/watchn.ddmmmyy.
	  The format for watch.ascii is:

	 analog ANT ADD [N]    ["alarm"] "Label"   LL L H HH
	 common NAME [SUB]   ["alarm"] "Label"   LL L H HH
	 time  ["alarm"] MAX 
	 antbits ANT ["alarm"]
	 Where:  ANT is an antenna number
	  ADD is a hex telemetry address or index number
	  N is the telemetry nbytes (def is 2)
	  "alarm" causes the alarm to be sounded
		if the value goes outside the range LL-HH
	  "Label" is a user supplied label for plots 
	  NAME is a common block name
	  SUB is a common block subsrcript (def is 1)
	  LL & HH are alarm limits
	  L & H are warning limits
	  MAX is the maximum time-ticks error allowed

	The output file contains a header line, a line of
	column headers, and data lines that contain the
	modified julian time in column 1 and the requested
	quantities in columns 2-last.

	WATCH is started by typing:   rwatch node and opens
	a window on node.  It also writes a file: $HATWATCH/current
	that contains the last set of warning messages.			*/
/*@ minutes								*/
/*	Time between samples in minutes. (default is 5)
									*/
/*--	1.3 - wh - separate file for each antenna
	1.4 - wh - activate alarm subroutine
	2.0 - wh - version for private window(also write current file)
	2.1 - wh - name of watch file is:  watchn.date
	2.2 - wh - error has to occur twice in succession for alarm
	2.3 - wh - put common block + dtime in ant 0
	2.4 - wh - get time from gps receiver	
	2.5 - wh - renormalize times
		   put back .H/.L functionality				*/
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <math.h>
#define TRUE 1
#define FALSE 0
#define TSIZE 100
#define NANTS 10
struct watchme {
char	type[8] ;	/* "analog" or "common" */
char	ant[12]     ;	/* antenna number input  */
int	antflag ;	/* antenna flag for analog/antbits 1 for common */
int	add    ;	/* telemetry address for analog */
int	nbytes ;	/* telemetry size for analog */
char	ctype[2] ;	/* common block variable type for common */
char	name[16] ;	/* common block variable name for common */
int	sub    ;	/* common block variable subscript for common */
char	label[20] ;	/* label for warnings, etc.  */
double	L,H    ;	/* Warning limits  */
double	LL,HH	;	/* Alarm limits  */
int	alarmon ;	/* alarm if TRUE */
int	nerrors[12] ;	/* count errors  */
} ;
struct watchme  W[TSIZE] ;
FILE *file, *current ;
static char *blanks = "                                                           ";
int messout(int, char *, char *) ;
int readlims(struct watchme W[TSIZE]) ;
int watchmes(char *, int , char *) ;
char *gettok(int *status) ;
double julian_(int *,int *,int *,double *) ;
float atod_(int *,int *,int *,char [4],char [16],char [80]) ;

main(int argc, char *argv[])
{
  int i, ticks, dticks, two = 2 ;
  int mon, day, year, w, ticker ;
  char aname[16] ;
  double ut, lst, julday ;
  double  value ;
  int status, powoff, three = 3, tstatus = 0x13a ;
  int ints[TSIZE], raw ;
  float reals[TSIZE], minutes ;
  double doubles[TSIZE] ;
  char units[4], currentfile[80] ;
  char mess[80], allmess[80] ;
  char valueline[1000], vstring[20], headerline[12][1000] ;
  float delta, precis, five = 5. ;
  char wfreq[20], wtrans[20], wwait[20], error[80], antnum[6], dummy[80] ;
  int ant ;

	if(argc>1) sscanf(argv[1],"minutes=%f",&minutes) ;
	else minutes = 5. ;
	dticks = (int)(minutes*6000.) ;

	printf("WATCH (version 2.7 15-oct-97)     minutes=%.2f\n",minutes) ;
	mbusopen_(error) ;
	if (strncmp(error,"OK",2) != 0) printf("mbusopen: %s\n",error) ;

	if(readlims(W) < 0) return ;

/*  --	open files for all antennas & print headers   */
	for (ant=0; ant<=NANTS; ant++) {
	    if(messout(ant,"# WATCH is restarting","#") < 0) return ;
	    strcpy(headerline[ant],"#days") ;
	    for (i=0; strncmp(W[i].type,"done",4) != 0; i++) {
	      if(select_(&ant,&(W[i].antflag))) {
	 	strcat(headerline[ant]," ") ;
		strcat(headerline[ant],W[i].label) ;
	      }
	      else  if ((ant == 0) && (strncmp(W[i].type,"common",6) == 0)) {
	 	strcat(headerline[ant]," ") ;
		strcat(headerline[ant],W[i].label) ;
	      }
	    }
	    messout(ant,headerline[ant],"#") ;
	}
	  strcpy(currentfile,getenv("HATWATCH")) ;
	  strcat(currentfile,"/current") ;

/*  --	INSPECTION LOOP    */
	for (;;) {
	  get_times_(&ticks,&two,&mon,&day,&year,&ut,&lst,error) ;
	  if (strncmp(error,"OK",2) != 0)  watchmes(error,1,dummy) ;
		
	  julday = julian_(&day,&mon,&year,&ut) - 2450000. ;
	  current = fopen(currentfile,"w") ;
	  sprintf(mess,"------------------------------- JD = %f",julday) ;
	  fprintf(current,"%s\n",mess) ;
	  watchmes(mess,1,error) ;

/*  --	LOOP THROUGH ANTS, BUILDING RECORD FOR EACH AND OUTPUT  */
	  for (ant=0; ant<=NANTS; ant++) {
	  sprintf(valueline,"%f  ",julday) ;
	  powoff = 0 ;
	  for (i=0;strncmp(W[i].type,"done",4) != 0; i++) {
	   if(((ant==0)&&(strncmp(W[i].type,"common",6)==0))
		 || ((select_(&ant,&(W[i].antflag)))&&(powoff == 0))) {

/*  --	    first, check for power off in cabin  */
/*	     if(ant>0) status = tpeek_(&ant,&tstatus,&three,error) ;
	     else status = 0x800000 ;
	    if((status & 0x800000) == 0) {
	      if(powoff == 0) {
		powoff = 1 ;
		sprintf(mess,"|Ant %d:CABIN POWER OFF",ant) ;
		watchmes(mess,0,error) ;
		fprintf(current,"%s\n",mess) ;
	      }
	    }
	    else {  */

	     if (strncmp(W[i].type,"analog",6) == 0) {
	      value = atod_(&ant,&W[i].add,&raw,units,aname,error) ;
	      if(strncmp(error,"OK",2) != 0) watchmes(error,0,dummy) ;
	     }
	     else if ((ant==0)&&(strncmp(W[i].type,"common",6) == 0)) {
	      if(strncmp(W[i].ctype,"I",1) == 0) {
		comgeti_(W[i].name,ints,&W[i].nbytes,error) ;
		value = (double)ints[W[i].sub-1] ;
	      }
	      else if(strncmp(W[i].ctype,"R",1) == 0) {
		comgetr_(W[i].name,reals,&W[i].nbytes,error) ;
		value = (double)reals[W[i].sub-1] ;
	      }
	      else if(strncmp(W[i].ctype,"D",1) == 0) {
		comgetd_(W[i].name,doubles,&W[i].nbytes,error) ;
		value = doubles[W[i].sub-1] ;
	      }
	     }
	     else if ((ant==0)&&(strncmp(W[i].type,"time",4) == 0)) {
	      watchgps_(&delta,&precis,&ticker,error) ; 
	      value = delta*100. ;
	     }
	     else if ((ant>0)&&(strncmp(W[i].type,"antbits",7) == 0)) {
	      watchstatus(ant,ut,allmess,error) ;
	      if(strncmp(error,"OK",2) !=0) watchmes(error,0,dummy) ;
	      if(strcmp(allmess,"") != 0) {
		fprintf(current,"|%s\n",allmess) ;
		watchmes(allmess,0,error) ;
	      }
	      value = 0. ;
             }
	     if(fabs(value) > 100000.) value = -999. ;
	     if(value > W[i].H || value < W[i].L) {
		sprintf(mess,"%s.%1d = %.6g legal range: %9f - %9f",
		  W[i].label,ant,value,W[i].L,W[i].H) ;
		fprintf(current,"|%s\n",mess) ;
	        watchmes(mess,0,error) ;
	     }
	     if(value > W[i].HH || value < W[i].LL) {
		W[i].nerrors[ant-1]++ ;
		if((W[i].alarmon)&&(W[i].nerrors[ant-1] > 2))
		     alarm_(mess,error) ;
	     }
	     else W[i].nerrors[ant-1] = 0 ;

	     if(fabs(value) > 10000.) w = 0 ;
	     else if(fabs(value) > 1000.) w = 1 ;
	     else if(fabs(value) > 100.) w = 2 ;
	     else if(fabs(value) > 10.) w = 3 ;
	     else  w = 4 ;
	     sprintf(vstring,"%.*f ",w,value) ;
	     strcat(valueline,vstring) ;
	/*  }  */
	   }
	  }
	  messout(ant,valueline,headerline[ant]) ;
	  }
	  fclose(current) ;
	  watchmes(mess,-1,error) ;
	  wait_ticks_(&dticks) ;
	}
}
/* READLIMS	- read in limits file to fill structure			*/
/*: watch								*/
/*+
	int readlims(struct watchme *W) ; 
   
	readlims reads in the file of watch limits to place them
	in structure W.

  ARGUMENTS:
  W(watchme *,output)	Structure of limits
	jul 92  wh
									*/
/*--									*/
int readlims(struct watchme W[100])
{
  char filepath[40] ;
  int nextw = -1, status, i ;
  char token[80], tok[80], tokant[12]  ;
  char error[80], addst[20] ;
  static char *alarmess[2] = {" ", "alarm"} ;

	strcpy(filepath,getenv("HATTAB")) ;
	strcat(filepath,"/watch.ascii") ;

/*  --	open-read-close the file  */
	file = fopen(filepath,"r") ;
	if (file == NULL) {
		perror("WATCH: can't open watch.ascii: ") ;
		return -1 ;
	}
	printf(" TYPE  sub/ant-add  Label          LL L H HH\n") ;
	do {
	 strcpy(token,gettok(&status)) ;
	 if (status != -1) {
	  if (strncmp(token,"!",1) == 0) 
		while (status == 0) gettok(&status) ;
	  else if (strncmp(token,"analog",6) == 0) {
		nextw++ ;
	 	strcpy(W[nextw].type,"analog") ;
		strncpy(tokant,gettok(&status),12) ;
		strcpy(W[nextw].ant,tokant) ;
		pants_(tokant,&W[nextw].antflag,strlen(tokant)) ;
		strcpy(addst,gettok(&status)) ;
		W[nextw].add = strtol(addst,(char **)NULL,16) ;
		if(W[nextw].add > 4095)
		  W[nextw].add = strtol(addst,(char **)NULL,10) ;
		strcpy(tok,gettok(&status)) ;
		if(isdigit(tok[0])) {
		  W[nextw].nbytes = atoi(tok) ;
		  strcpy(tok,gettok(&status)) ;
		}
		else  W[nextw].nbytes = 2 ;
		if (strncmp(tok,"alarm",5) == 0) {
		  W[nextw].alarmon = TRUE ;
		  strcpy(tok,gettok(&status)) ;
		}
		else  W[nextw].alarmon = FALSE ;
		strcpy(W[nextw].label,tok) ;
		W[nextw].LL = atof(gettok(&status)) ;
	        if(status != 0) break ;
		W[nextw].L = atof(gettok(&status)) ;
	        if(status != 0) break ;
		W[nextw].H = atof(gettok(&status)) ;
	        if(status != 0) break ;
		W[nextw].HH = atof(gettok(&status)) ;
		while (status == 0)  gettok(&status) ;
		printf("%s %s -%6s   %s   %5f<%5f<%5f<%5f   %s\n",
		 W[nextw].type,W[nextw].ant,addst,
		  W[nextw].label,W[nextw].LL,W[nextw].L,W[nextw].H,W[nextw].HH,
		   alarmess[W[nextw].alarmon]) ;
	  }
	  else if (strncmp(token,"common",6) == 0) {
		nextw++ ;
	 	strcpy(W[nextw].type,"common") ;
		W[nextw].antflag = 1 ;
		strcpy(tok,gettok(&status)) ;
		for (i=0;i<11;i++) W[nextw].name[i]=(char)toupper((int)tok[i]) ;
		comprobe_(W[nextw].name,W[nextw].ctype,&W[nextw].nbytes,error) ;
		strcpy(tok,gettok(&status)) ;
		if(isdigit(tok[0])) {
		  W[nextw].sub = atoi(tok) ;
		  strcpy(tok,gettok(&status)) ;
		}
		else  W[nextw].sub = 1 ;
		if (strncmp(tok,"alarm",5) == 0) {
		  W[nextw].alarmon = TRUE ;
		  strcpy(tok,gettok(&status)) ;
		}
		else  W[nextw].alarmon = FALSE ;
		strcpy(W[nextw].label,tok) ;
		W[nextw].LL = atof(gettok(&status)) ;
	        if(status != 0) break ;
		W[nextw].L = atof(gettok(&status)) ;
	        if(status != 0) break ;
		W[nextw].H = atof(gettok(&status)) ;
	        if(status != 0) break ;
		W[nextw].HH = atof(gettok(&status)) ;
		while (status == 0)  gettok(&status) ;
		printf("%s %s[%3d]   %s   %5f<%5f<%5f<%5f   %s\n",
		 W[nextw].type,W[nextw].name,W[nextw].sub,
		  W[nextw].label,W[nextw].LL,W[nextw].L,W[nextw].H,W[nextw].HH,
		   alarmess[W[nextw].alarmon]) ;
	  }
	  else if (strncmp(token,"time",4) == 0) {
		nextw++ ;
	 	strcpy(W[nextw].type,"time") ;
		strcpy(W[nextw].label,"Ticks") ;
		W[nextw].antflag = 1 ;
		strcpy(tok,gettok(&status)) ;
		if (strncmp(tok,"alarm",5) == 0) {
		  W[nextw].alarmon = TRUE ;
		  strcpy(tok,gettok(&status)) ;
		}
		else  W[nextw].alarmon = FALSE ;
		W[nextw].HH = atof(tok) ;
		W[nextw].H = W[nextw].HH ;
		W[nextw].LL = -W[nextw].HH ;
		W[nextw].L = W[nextw].LL ;
		while (status == 0)  gettok(&status) ;
		printf("%s             %s                    %5.0f<%5.0f %s\n",
		 W[nextw].type,W[nextw].label,W[nextw].LL,W[nextw].HH,
		   alarmess[W[nextw].alarmon]) ;
	  }
	  else if (strncmp(token,"antbits",7) == 0) {
		nextw++ ;
	 	strcpy(W[nextw].type,"antbits") ;
		strcpy(W[nextw].label,"Status") ;
		strncpy(tokant,gettok(&status),12) ;
		strcpy(W[nextw].ant,tokant) ;
		pants_(tokant,&W[nextw].antflag,strlen(tokant)) ;
		strcpy(tok,gettok(&status)) ;
		if (strncmp(tok,"alarm",5) == 0) {
		  W[nextw].alarmon = TRUE ;
		  strcpy(tok,gettok(&status)) ;
		}
		else  W[nextw].alarmon = FALSE ;
		W[nextw].HH = 100. ;
		W[nextw].H = 100. ;
		W[nextw].LL = -100. ;
		W[nextw].L = -100. ;
		while (status == 0)  gettok(&status) ;
		printf("%s   %s        %s                     %s\n",
		 W[nextw].type,W[nextw].ant,W[nextw].label,
		   alarmess[W[nextw].alarmon]) ;
	  }
	  for (i=0; i<=NANTS; i++) W[nextw].nerrors[i] = 0 ;
	 }
	} while (status != -1) ;
	strcpy(W[++nextw].type,"done") ;
	fclose(file) ;
	return 1 ;
}
/*  --	GET THE NEXT TOKEN FOR PROCESSING (bounded by blanks and endoflines)*/
/*  --	status is 0 normally; 1 for endofline;  -1 for endoffile  */
/*  --	the file is opened externally to this routine  */
char *gettok(int *status)
{
  char token[20] ;
  char *p = token ;
  int c ;

	p = token ;
	*status = 0 ;

/*  --	throw out leading blanks and tabs   */	
	while ((c = fgetc(file)) == ' ' || c == '\t' && c !=EOF && c !='\n') ;
	if (c == EOF) {*status = -1; return token; }
	if (c == '\n') {*status = 1; return token; }
	*p++ = c ;
/*  --	build token until next blank or tab  */
	while ((c = fgetc(file)) != ' ' && c != '\t' && c !=EOF && c !='\n') 
	  *p++ = c;
	*p++ = '\0' ;
	if (c == EOF) *status = -1;
	if (c == '\n') *status = 1;
	return token ;
}

/* MESSOUT	- write out log message to file				*/
/*: logger								*/
/*+
	int messout(antenna,text of message,header message) ;
   
	messout receives the messages from logger and formats them
	with a timestamp and carriage return them writes them into
	the appropriate file $HATLOG/watchn.date.  The file is opened 
	for each line.

  ARGUMENTS:
  ANT(int,input)	Antenna number
  TEXT(char ,input)	Message text 
  HEAD(char,input)	Header message for beginning of day
	sep 91  wh
									*/
/*--									*/

int messout(int ant, char *text, char *head)
{
  char filename[14] ;
  static char lastfilename[12][14] = 
	{" "," "," "," "," "," "," "," "," "," "," "," "} ;
  char path[60] ;
  char *ng ;
  time_t tp ;
  struct tm *tim ;
  FILE *file ;
  int n ;
  char antstring[3] ;

/*  --	generate the time-stamp and path name */
	tp = time(NULL) ;
	tim = gmtime(&tp) ;
	n = strftime(filename,13,".%d%h%y\0",tim) ;
	filename[3] = (char)tolower((int)filename[3]) ;
	ng = getenv("HATWATCH") ;
	if (ng == NULL) {
		perror("WATCH: no HATWATCH:") ;
		return -1 ;
	}
	strcpy(path,ng) ;
	strcat(path,"/watch") ;
	sprintf(antstring,"%1x",ant) ;
	strcat(path,antstring) ;
	strcat(path,filename) ;

/*  --	open-write-close the file  */
	file = fopen(path,"a") ;
	if (file == NULL) {
		perror("WATCH: can't open watchfile: ") ;
		return -1 ;
	}

/*  --	add column header to beginning of new file  */
	if(strcmp(filename,lastfilename[ant]) !=0)
	  fprintf(file,"%s\n",head) ;
	strcpy(lastfilename[ant],filename) ;

	fprintf(file,"%s\n",text) ;
	fclose(file) ;
	return 0 ;
}			


/* WATCHMES - put watch mess into common				*/
/*: watch								*/
/*+
	int watchmes(mess,erase,error) ;
   
	watchmes puts messages into the WATCHME section of common.

  ARGUMENTS:
  MESS(char,input)	A Status messages
  ERASE(int,input)	1=start messages,-1=put into common, 0=justamess
  ERROR(char ,output)	Status return
	aug 92  wh
									*/
/*--									*/

int watchmes(char *mess, int erase, char *error)
{
  static char watchme[12][80] ;
  int i, d960 = 960 ;
  static int line ;
	strncpy(error,"OK",79) ;
	if(erase == -1) {
	  computa_("WATCHME",watchme,&d960,error) ;
	}
	else {
	 if (erase == 1) {
		line = 0 ;
	        for (i=line;i<12;i++) strncpy(watchme[i]," ",79) ;
	 }
	 strncpy(watchme[line],mess,79) ;
	 line++ ; if(line == 12) line = 11 ;
	}
}
/* WATCHSTATUS - get antenna status					*/
/*: watch								*/
/*+
	int watchstatus(antenna,ut,allmess,error) ;
   
	watchstatus checks out the antenna status bits on the
	telemetry and reports the first problem in error.

  ARGUMENTS:
  ANTENNA(int,input)	Antenna number
  UT (double,input)	Universal time in radians
  ALLMESS(char,output)	Status messages
  ERROR(char ,output)	Status return
	aug 92  wh
									*/
/*--									*/

int watchstatus(int ant, double ut, char *allmess, char *error)
{
  static char *telerror[7] = {
   "Data overflow", "Parity error", "Transmit check", "Invalid ending",
     "Mid-bit transition loss", "New word data overflow", "Receiver disabled" } ;
  int teleerr = 0x13e, one = 1, allones = 0, teleadd = 0x13f ;

  int i, status, addr, three = 3, tstatus = 0x13a ;
  int isgood[23] = {0,0,0,0,0,-1,0,0, 1,1,1,1,1,-1,1,0, -1,-1,-1,-1,-1,-1,-1} ;
  static char *mess[23] = {  
   "EL Limit, ", "SERVICE, ", "AZ ULT Limit, ", "KEY OFF, ",
    "LOW Water Pressure, ", "", "AZ Limit, ", "EL ULT Limit, ", 
     "AZ Temp HI, ", "TV Flap Fault, ", "EL Temp HI, ", "Receiver Temp HI, ",
      "Cabin Temp HI, ", "", "ANT Drive MANUAL, ", "COLLISION!, ",
       "", "", "", "", "", "Power Override, ", "TV Flap Open, " } ;


/* --	now check for telemetry errors  */
	status = tpeek_(&ant,&teleerr,&one,error) ;
	addr = tpeek_(&ant,&teleadd,&one,error) ;
	tpoke_(&ant,&teleadd,&one,&allones,error) ;
	if  (strncmp(error,"OK",2) != 0) write_display_(error) ;
	if (addr != 0) {
	 for (i=0; i<8; i++) {
	  if((status & 1<<i) != 0) {
	     sprintf(error,"Ant %d: Telem-out %s at %X ",ant,telerror[i],addr) ;
	     return ;
	  }
	 }
	}
/* --	lastly check all of the status bits  */
	strcpy(allmess,"") ;
	status = tpeek_(&ant,&tstatus,&three,error) ;
	if  (strncmp(error,"OK",2) != 0) write_display_(error) ;
	for (i=0; i<22; i++) {
	 if(isgood[i] != -1) {
	  if((status & 1<<i) != isgood[i]<<i) {
		if(strcmp(allmess,"") == 0) sprintf(allmess,"Ant %d:",ant) ;
		if((int)strlen(allmess) < 79-(int)strlen(mess[i])) strcat(allmess,mess[i]) ;
	  }
	 }
	}
	if (((status&0x400000) == 0x400000) && ((ut < .8)||(ut > 3.4))) {
		if(strcmp(allmess,"") == 0) sprintf(allmess,"Ant %d:",ant) ;
		if((int)strlen(allmess) < 79-(int)strlen(mess[22])) 
					strcat(allmess,mess[22]) ;
	}
}

