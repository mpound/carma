/** ISTATUS	- display all aspects of interferometer registers	*/
/*: misc								*/
/*+
	ISTATUS ANTS=xxx
   
	ISTATUS displays all many of the accessible quantities that 
	are connected to the interferometer calculations.
	The information fills a screen and is updated each second.  
	An antenna selection parameter allows elimation
	of data from unused antennas.  Some of the displayed data
	comes from the interferometer hardware, but some comes from the
	common block and is updated by the DELAY subprocess, so this
	will only be valid if an observing program is active.
	The quantities displayed are:
	 RA2000  and current RA   (hours)
	 DEC2000 and current DEC  (hours)
	 HAdot and DECdot  
	 Tau, Taudot, Taudotdot are the delay and derivatives  (ns)
	 Delay  is Tau corrected for Delay0 and cable length   (ns)
	 Bits   is the integer sent to the interface
	 FOF1  is Taudot*LO1	(Hz)
	 FOF2  is Taudot*LO2	(Hz)
	 FOFx(r) are the values read back from the registers and converted
	 PHASEx  are the FOF phases read back from the registers  (turns)
	 WALSHx  are the Walsh values read back from the registers
	 POWER   is the total power
	Most of the quantities above are only updated at the beginning
	of integrations.						*/
/*@ ants								*/
/*	Antenna selection flag:
	 * means use all nants antennas
	 xyz means use antennas x y and z
	 *-xyz means use all antennas except x y and z			*/
/*--	nov 92 wh								*/
#include "constants.h"
#define SIDERIAL 0x2000
#define TRANSMITTER 0x4000
#define WALSH  0x8000
#include <string.h>
#include <unistd.h>
#include <curses.h>
#include <time.h>
#include <sys/time.h>
#include "hatlib.h"
struct asource so ;
char mess[80], error[80] ;
struct timeval tp ;
struct timezone tzp ;
float atod_(int *ant,int *mpx,int *raw,char units[4],char name[16],char error[80]) ;

main(int argc, char *argv[])
{
  int ants, onesec = 100, nants = NANTS, ant ;
  char antstring[20] ;
  int itrack[NANTS], it, raw ;
  char error[80], me[12], units[4], name[16] ;
  float totpow ;
  int d1094 = 1094 ;
  int x3dm = 0x3d-256, d24 = 24, d0 = 0, d1 = 1, d2 = 2, d3 = 3 ;
  int v, x41m = 0x41-256, x44m = 0x44-256, x47m = 0x47-256, x45m = 0x45-256 ;
  int x40m = 0x40-256, xe4 = 0xe4 ;
  char sname[10] ;
  double ra2000, dec2000, pi = 3.141592654 ;
  double obsra,obsdec,tau[NANTS],taudot[NANTS],taudd[NANTS] ;
  double delay[NANTS], fof1[NANTS], fof2[NANTS], f ;
  int valued[NANTS] ;

	if(argc>1) sscanf(argv[1],"ants=%s",antstring) ;
	else {
	   printf("USAGE: astatus ants=xxx \n") ;
	   exit(0) ;
	}
	pants_(antstring,&ants,strlen(antstring)) ;

	printf("ISTATUS (version 1.3 20-jan-98)\n") ;
/*	lockexit_() ; */
	mbusopen_(error) ;
	if (strncmp(error,"OK",2) != 0) {
		printf("mbusopen: %s\n",error) ;
		exit() ;
	}
	initscr() ;

	do {
	  gettimeofday(&tp,&tzp) ;
	  sprintf(mess," INTERFEROMETER STATUS:  %s  ",ctime(&tp.tv_sec)) ;
	  move(0,10) ; addstr(mess) ;

/*  --	SOURCE		*/
	  comgeti_("SORCDATA",(int *)&so,&d24,error) ;
	  ra2000 = so.ra2000*12./pi ;
	  dec2000 = so.dec2000*180./pi ;
	  comgetd_("OBSRA",&obsra,&d1,error) ;
	  comgetd_("OBSDEC",&obsdec,&d1,error) ;
	  obsra = obsra*12./pi ;
	  obsdec = obsdec*12./pi ;
	  strncpy(sname,so.name,8) ;
	  sprintf(mess,\
	    "SOURCE: %10s  RA2000: % .5lf (% .5lf)  DEC2000: % .5lf (% .5lf) ",
		sname,ra2000,obsra,dec2000,obsdec) ;
	  move(1,0) ; addstr(mess) ; 

	  move(2,0) ; 
	  addstr("ANTS:         1          2         3         4         5  "\
	   "       6         7        8         9       10") ;

/*  --	ITRACK  		*/
	  comgeti_("ITRACK",itrack,&nants,error) ;
	  strcpy(mess,"ITRACK:   ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		it = itrack[ant-1]&0xf000 ;
		if (it == SIDERIAL)  strcpy(me," SIDR") ;
		else if (it == TRANSMITTER) strcpy(me," TEST") ;
		else strcpy(me,"   ") ;
		if ((it&WALSH) != 0) strcat(me,"+w  ") ;
		else strcat(me,"    ") ;
		strncat(mess,me,8) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(3,0) ; addstr(mess) ;
/*  --	TAU			*/
	  comgetd_("TAU",tau,&nants,error) ;
	  strcpy(mess,"TAU:    ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		sprintf(me,"% 9.4lf ",tau[ant-1]) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(4,0) ; addstr(mess) ;
/*  --	TAUDOT			*/
	  comgetd_("TAUDOT",taudot,&nants,error) ;
	  strcpy(mess,"TAU.:   ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		sprintf(me,"% 9.6lf ",taudot[ant-1]) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(5,0) ; addstr(mess) ;
/*  --	TAUDD			*/
	  comgetd_("TAUDD",taudd,&nants,error) ;
	  strcpy(mess,"TAU..:  ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		sprintf(me,"% 9.6lf ",taudd[ant-1]) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(6,0) ; addstr(mess) ;
/*  --	DELAY		*/
	  comgetd_("DELAY",delay,&nants,error) ;
	  strcpy(mess,"DELAY:  ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		sprintf(me,"% 9.4lf ",delay[ant-1]) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(7,0) ; addstr(mess) ;
/*  --	BITS		*/
	  comgeti_("DWIRES",valued,&nants,error) ;
	  strcpy(mess,"DBITS:  ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		sprintf(me,"% 9X ",valued[ant-1]) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(8,0) ; addstr(mess) ;
/*  --	FOF1		*/
	  comgetd_("FOF1",fof1,&nants,error) ;
	  strcpy(mess,"FOF1:   ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		sprintf(me,"% 9.4lf ",fof1[ant-1]) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(9,0) ; addstr(mess) ;
/*  --	FOF1(register)		*/
	  strcpy(mess,"FOF1(r): ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		v = tpeek_(&ant,&x3dm,&d3,error) ;
		f = (double)v / 2097.152 ;
		sprintf(me,"% 9.4lf ",f) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(10,0) ; addstr(mess) ;
/*  --	FOF2		*/
	  comgetd_("FOF2",fof2,&nants,error) ;
	  strcpy(mess,"FOF2:   ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		sprintf(me,"% 9.4lf ",fof2[ant-1]) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(11,0) ; addstr(mess) ;
/*  --	FOF2(register)		*/
	  strcpy(mess,"FOF2(r): ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		v = tpeek_(&ant,&x41m,&d3,error) ;
		f = (double)v / 2097.152 ;
		sprintf(me,"% 9.4lf ",f) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(12,0) ; addstr(mess) ;
/*  --	FOF1(phase)		*/
	  strcpy(mess,"PHASE1: ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		v = tpeek_(&ant,&x45m,&d2,error) ;
		f = (double)(v>>4) /4096. ;
		sprintf(me,"% 9.4lf ",f) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(13,0) ; addstr(mess) ;
/*  --	FOF2(phase)		*/
	  strcpy(mess,"PHASE2: ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		v = tpeek_(&ant,&x47m,&d2,error) ;
		f = (double)(v>>4) /4096. ;
		sprintf(me,"% 9.4lf ",f) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(14,0) ; addstr(mess) ;
/*  --	WALSH1		*/
	  strcpy(mess,"WALSH1: ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		v = tpeek_(&ant,&x40m,&d1,error) ;
		sprintf(me,"% 9d ",v) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(15,0) ; addstr(mess) ;
/*  --	WALSH2		*/
	  strcpy(mess,"WALSH2: ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
		v = tpeek_(&ant,&x44m,&d1,error) ;
		sprintf(me,"% 9d ",v) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(16,0) ; addstr(mess) ;

/*  --	TOTAL POW		*/
	  strcpy(mess,"POWER:  ") ;
	  for(ant=1;ant<=10;ant++) {
	    if(select_(&ant,&ants)) {
	        totpow = atod_(&ant,&d1094,&raw,units,name,error) ;
		sprintf(me,"% 9.4f ",totpow) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"        ") ;
	  }
	  move(17,0) ; addstr(mess) ;

	  refresh() ;
	  wait_ticks_(&onesec) ;
	}  while (1) ;
}

