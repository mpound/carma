/** WSTATUS	- display watch error messages				*/
/*: misc								*/
/*+
	WSTATUS 
   
	WSTATUS displays 12 lines of error messages generated
	by the watch program and stored in common.
									*/
/*--	dec 93 wh							*/
#include "hatlib.h"
#include <string.h>
#include <unistd.h>
#include <curses.h>

main()
{
  char watchme[12][80] ;
  char error[80], mess[80] ;
  int i, d960 = 960, d500 = 500 ;
	initscr() ;

	sprintf(mess,"  WATCH ERROR MESSAGES:") ;
	move(0,10) ; addstr(mess) ;
	for (;;) {
	 comgeta_("WATCHME",watchme[0],&d960,error) ;
 	 for(i=0;i<12;i++) {
	   move(i+2,0) ; 
	   addstr(watchme[i]) ;
	 }
	 move(14,0) ;
	 addstr(error) ;
	 refresh() ;
	 wait_ticks_(&d500) ;
	}
}
