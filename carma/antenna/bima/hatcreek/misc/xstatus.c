/** XSTATUS	- interactive display of the interferometer status	*/
/*: misc								*/
/*+
	XSTATUS
   
	XSTATUS is an interactive program that displays various
	quantities of the telescopes, interferometer, receiver,
	and other subsystems.
	The information fills a standard 80*24 screen and is updated
        regularly (once a second).

        This program has currently no parameters, but the following
        can be thought of:

        TEMPLATE: template description of screen (see e.g. $HATCAT/atod.ascii)
	
        
/*--								  	*/
/* History								*/
/*    nov 92 wh							        */
/*    jul 93 pjt combined all these *status and display programs        */
/*    aug 93 wh  fork off display progs instead of subr			*/
/*    dec 93 wh  add wstatus 						*/
/*    jun 95 wh  add precipmm 						*/
/*    mar 98 wh  check for runaway forking				*/
#define NANTS    10
#define VERSION  "16Mar98"

#include <string.h>
#include <unistd.h>
#include <curses.h>
#include <ctype.h>
#include <time.h>
#include <sys/time.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>

#define MODE_NULL     0
#define MODE_RSTATUS  1
#define MODE_ASTATUS  2
#define MODE_ISTATUS  3
#define MODE_DISPLAY  4
#define MODE_TEMPLATE 5
#define MODE_INPUT    6
#define MODE_WSTATUS  7
#define MODE_PSTATUS  8
int spawn(char *,char *) ;
void stepon() ;
char *antstring(int) ;
void startup() ;
void forktidy() ;

static int DPID ;
static int COUNTER = 5 ;
static struct timeval TIME ;
static long LASTTIME = 0 ;


main(int argc, char *argv[])
{
    int ants, ant, wait_time=100, quit=0, c, i, mode;
    char error[80], me[12], units[4], name[16], arg[30] ;
    bool need_to_wait = FALSE;
    int  bad(), getants(int, int), iskey(char*, char*), usage();
    char *keyval(char*);
    char whom[40] ;
    char antstr[12] ;

    printf("XSTATUS (version  %s)\n",VERSION) ;
/*    mbusopen_(error) ;
    if (strncmp(error,"OK",2) != 0) {
        printf("mbusopen: %s\n",error) ;
	exit(1) ;
    }   */

    initscr() ;             /* startup curses */
/*    cbreak(); */		/* put into interactive mode */
    noecho(); 

    mode =  MODE_NULL;
    ant = 1;                /* current antenna if ant= active */
    ants = 0;               /* ants flag if ants= active */
    startup();  /* used at startup */
	
    do {                    /* interaction loop until interactive quit */
	  if (DPID != 0) stepon() ;
	  switch (mode) {
	    case MODE_RSTATUS: 	if (ant==3) DPID=spawn("rstatus3","ant=3");
	                        else {
				 sprintf(arg,"ant=%s",antstr) ;
				 DPID=spawn("rstatusn",arg);
				}
	                        break;
	    case MODE_ISTATUS: 	sprintf(arg,"ants=%s",antstr) ;
				DPID=spawn("istatus",arg);
	                        break;
	    case MODE_ASTATUS: 	sprintf(arg,"ants=%s",antstr) ;
				DPID=spawn("astatus",arg);
	                        break;
	    case MODE_WSTATUS: 	DPID=spawn("wstatus",arg);
	                        break;
            case MODE_DISPLAY: 	DPID=spawn("display",NULL);
                                break;
            case MODE_PSTATUS: 	DPID=spawn("showprecip",NULL) ;
                                break;
            case MODE_TEMPLATE:		/*	template(); */
            			break;
	    case MODE_NULL:
	    			break;
	    default:
	    			break;
	  }
	  refresh();

	  c = getch();   /* get new char */
	   if (DPID != 0) stepon() ;
	   switch (c) {
	    case ERR: break;        /* continue */

	              /* information */
	              
	    case '?': clear(); refresh(); help();
		      clear(); refresh();
		      mode = MODE_INPUT;  break;
            case 's': clear(); refresh(); show_status(mode,ant,ants);
		      clear(); refresh();
		      mode = MODE_INPUT;  break;

                      /* toggle to a mode */
            
            case 'r': clear(); refresh();
		      mode = MODE_RSTATUS;  break;
            case 'i': clear(); refresh();
                      mode = MODE_ISTATUS; break;
            case 'z': clear(); refresh();
                      mode = MODE_ASTATUS; break;
            case 'w': clear(); refresh();
                      mode = MODE_WSTATUS; break;
            case 'd': clear(); refresh();
                      mode = MODE_DISPLAY; break;
            case 'p': clear(); refresh();
                      mode = MODE_PSTATUS; break;
            case 't': clear(); refresh();
                      mode = MODE_TEMPLATE; break;

                      /* add / delete antenna from the list */
                      
	    case '+': ants = getants(c, ants); break;
	    case '-': ants = getants(c, ants); break;
	    case '*': ants = getants(c, ants); break;

	              /* select single antenna */
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
                      ant = c - '0';        /* convert digit to 1..9 */
                      ants = getants(c,0x00);   /* reset ants with 1 ant */
    		      clear(); refresh();
		      mode = MODE_INPUT;  break;
	    case 'a':
	    case 'b':
	    case 'c':
	    case 'A':
	    case 'B':
	    case 'C':
		      ant = tolower(c) - 'a' + 10 ;
		      ants = getants(tolower(c),0x00);
		      clear(); refresh();
		      mode = MODE_INPUT;  break;
	    case 'q': quit = 1;
	              break;
	    default:
                      bad();        /* don't break, try again */
	   }
	   strcpy(antstr,antstring(ants)) ;  /* convert bits to characters */
    }  while (!quit);
    endwin();
}

/*
 *
 */

static FILE *flog = NULL;

debug(char *mesg)
{
#if 0
    /* doesn't seem to work    ... */
  /*  nocbreak(); echo(); */
    printf("\"]2;%s\"",mesg);
/*    cbreak(); noecho(); */
#endif
#if 0
    if (flog==NULL) flog=fopen("xstatus.log","r");
    fprintf(flog,"%s\n",mesg);
    fflush(flog);
#endif
    move(23,0);    addstr(mesg);
}






#define SHOW(a) move(row++,0); sprintf(mesg,a); addstr(mesg);

help()
{
    int row=0;
    char mesg[80];

    SHOW("_________________________ Experimental Xstatus ___________");
    SHOW(VERSION);
    SHOW(" ");
    SHOW("Commands: ");
    SHOW(" ?  This help                         r  toggle to RSTATUS");
    SHOW(" +I Add an antenna to list            z  toggle to ASTATUS");
    SHOW(" -I Remove antenna from list          i  toggle to ISTATUS");
    SHOW(" I  Switch to show single antenna     d  toggle to DISPLAY");
    SHOW(" *  Add all (valid) antennae		w  toggle to WSTATUS");
    SHOW(" s  Status of program (debug)");
    SHOW(" q  Quit");
    SHOW("Notation");
    SHOW("     I       a digit [1-9] denoting an antenna");
    SHOW("Hit any key to continue to previous mode");
    getch();    
}

void startup()
{
    int row=0;
    char mesg[80];

    SHOW("Select a mode:");
    SHOW(" r    RSTATUS");
    SHOW(" z    ASTATUS");
    SHOW(" i    ISTATUS");
    SHOW(" w    WSTATUS");
    SHOW(" d    DISPLAY");
    SHOW(" p    Precipmm");
    SHOW(" ?    more help");
    SHOW(" ");
    SHOW("(These are toggles, and one can always toggle to another mode)");
    SHOW("Enter selection: ");
}

show_status(int mode, int ant, int ants)
{
    int row=0, c;
    char mesg[80];

    SHOW("================ STATUS =========================");
    switch (mode) {
        case MODE_NULL:     SHOW("   null");     break;
        case MODE_RSTATUS:  SHOW("   rstatus");  break;
        case MODE_ASTATUS:  SHOW("   astatus");  break;
        case MODE_ISTATUS:  SHOW("   istatus");  break;
        case MODE_WSTATUS:  SHOW("   wstatus");  break;
        case MODE_TEMPLATE: SHOW("   template"); break;
        default:            SHOW("   unknown");  break;
    }
    sprintf(mesg,"ant=%d ants=%d (0x%x)",ant,ants,ants);
    SHOW(mesg);
    getch();    
}


int bad()
{
    int c, row=0;
    char mesg[80];

    move(15,10); addstr("### Bad selection, try again or '?' for help");
    c = getch();    
    return c;
}

static bool present[NANTS];

int getants(int c, int ants)
{
    char ca, mesg[80];
    int i, n ;
    int valid_antenna();

    if (c=='*') {
    	for (i=0; i<10; i++)
    	    present[i] = valid_antenna(i+1) ? TRUE : FALSE;
    } else if (isdigit(c)) {
        i = c - '0';
        if (valid_antenna(i)) {
    	    for (i=0; i<NANTS; i++)
    	        present[i] = FALSE;
    	    i = c - '0';
    	    present[i-1] = TRUE;
    	}
    } else if (islower(c)) {
        n = c - 'a' +10 ;
        if (valid_antenna(n)) {
    	    for (i=0; i<NANTS; i++)
    	        present[i] = FALSE;
    	    present[n-1] = TRUE;
    	}
    } else if (isupper(c)) {
        n = c - 'A' +10 ;
        if (valid_antenna(n)) {
    	    for (i=0; i<NANTS; i++)
    	        present[i] = FALSE;
    	    present[n-1] = TRUE;
    	}
    } else if (c=='+' || c=='-') {
        move(23,0); addstr("Now which antenna:[1..a]");
	refresh() ;
        ca = getch();    
        i = ca - '0';	            /* selected antenna  i = 1..NANTS */
	if (valid_antenna(i))
            present[i-1] = (c=='+' ? TRUE : FALSE);
        else {
            move(23,0);
            sprintf(mesg,"### Warning: invalid antenna number %d",i);
            addstr(mesg);
	    refresh();
        }
    } else {
        move(23,0);
        sprintf(mesg,"### Warning: Bad getants option %s",c);
        addstr(mesg);
	refresh();
    }
    /*
     * something is fishy here, Wilson seems to set bit i+1 for antenna i
     * where i=1..NANTS; that seems a bit strange ...
     */
    ants = 0x00;                            /* reset ants */
    for (i=0; i<NANTS; i++)
        if (present[i]) ants |= (1<<(i+1));  /* set bits for each present ant */
    return ants;
}

/*
 * VALID_ANTENNA:
 *      i = 1..12 is valid
 */
int valid_antenna(int i)
{
    if (i<1 || i>12) return 0;
    return 1;
}


/*
 * ISKEY, KEYVAL: part of my 'key=val' based YAPCLP,
 *
 *   has very little error trapping for badly formed arguments key=val
 */

int iskey(char *arg,char *key)
{
    int klen=strlen(key);
    
    return (strncmp(arg,key,klen)==0 && arg[klen] == '=') ;
}

char *keyval(char *arg)
{
    char *v, *eq ;

    eq = strchr(arg,'=');           /* find the '=' */
    if (eq == NULL) return NULL;    /* make sure it's there */
    eq++;                           /* position after the '=' */
    v = (char *)malloc(strlen(eq)); 
    strcpy(v,eq);
    return v;
}

/* usage()
{
    printf("Default keywords with values are:\n\n");
    printf("   delay=1          Delay in seconds between screen updates\n");

    exit(0);
}  */
/*
	spawn subroutine returns the process ID for kill
*/
int spawn(char *program,char *arg)
{
    char bin[80] ;
    int dpid ;

/* --	Check that forks aren't occurring too quickly, allow only 5 per sec */
	if ( !COUNTER-- )
	{
	  COUNTER = 5 ;
	  gettimeofday(&TIME,0) ;
	  if (TIME.tv_sec == LASTTIME)
	  {
	    printf("Exiting from runaway fork loop\n") ;
	    exit(1) ;
	  }
	  else  LASTTIME = TIME.tv_sec ;
	}

/* --	now fork another process and exec off program  */
	switch(dpid=fork()) {
	case -1:
	   printf("Can't fork status process\n") ;
	   return 0;
	case 0:
	   strcpy(bin,getenv("HATBIN")) ;
	   strcat(bin,"/") ;
	   strcat(bin,program) ;
	   execlp(bin,program,arg,NULL) ;
	   printf("Can't exec process %s\n",bin) ;
	   return 0;
	default:
	   break ;
	}
	return dpid ;
}
/*
	stepon kills a process and waits around to make sure
*/
void stepon()
{
  int i, endstat ;
  char error[80] ;
/*  --  send the stop signal  -- */
	if( DPID != 0 ) 
	{
	  if(kill(DPID,SIGKILL) == -1)
	  {
		sprintf(error,"errno = %d",errno) ;
		perror("stepon/kill") ;
	  }
	  if(wait(&endstat) != DPID) perror("stepon/wait") ;
	  DPID = 0 ;
	}
}
/*
	antstring converts bits to character string
*/
char *antstring(int ants)
{
  int i,j=0 ;
  char string[20] ;

  for (i=1;i<=9;i++) {
	if(((1<<i)&ants) != 0) string[j++] = (char)(i + (int)'0') ;
  }
  for (i=10;i<=15;i++) {
	if(((1<<i)&ants) != 0) string[j++] = (char)(i -10 + (int)'a') ;	
  }
  string[j] = 0 ;
  return string ;
}
