/** ASTATUS	- display antenna status			*/
/*: misc								*/
/*+
	ASTATUS ANTS=xxx
   
	ASTATUS displays all many of the quantities that are connected
	to antennas.  The information fills a screen and is updated
	each second.  An antenna selection parameter allows elimation
	of data from unused antennas.  Some of the displayed data
	comes from the antenna hardware, but other quantities come 
	from the common block and is updated by the CONTROL program, 
	so these will not be valid if CONTROL is not active.
	NOTE: The time displayed is the unix system time which is
	not the same as the multibus time displayed by DISPLAY.

	The quantities displayed are:
	 ITRACK    decoded   AZ=Azim/Elev     RA=RA/DEC tracking
			     VA=Cons Vel AZ   VE=Cons Vel EL
			     h=horn offsets   r=ra/dec offsets
			     c=pointing offsets  +=Az/El offsets
	 RA2000	 is Right Ascension   (hours)
	 DEC2000 is Declination   (deg)
	 AZIM, ELEV are read from the antennas   (deg)
	 AZERR,ELERR are from CONTROL  (arcsec)
	 AZVEL,ELVEL are from the antennas   (-3000<0<3000)
	 AZSTATUS,ELSTATUS decode SLEWFLAG,ATLIMIT   
	 FOCUS is from the antennas    (mm)
	 TILT is from the antennas     (arcmin)
	 POWER is from the antennas    (volts)
	 FAULTS decodes the antenna status registers:
		SERVICE=?
		AZ LIM,AZ ULIM  Azimuth limits
		EL LIM,EL ULIM  Elevation limits
	        KEY OFF    Means the key in the ant control box
		WATER P   is water pressure low
		AZM HOT   means the AZIM drive box is hot
		ELV HOT   means the ELEV drive box is hot
		REC HOT   means the reciever is hot
		CAB HOT   means the cabin is hot
		TV FLAP   means the flap is open during the day
		BANG!!    means collision
		POW OFF   means antenna power is off									*/
/*@ ants								*/
/*	Antenna selection flag:
	 * means use all nants antennas
	 xyz means use antennas x y and z
	 *-xyz means use all antennas except x y and z			*/
/*--									*/
#include <string.h>
#include <unistd.h>
#include <curses.h>
#include <time.h>
#include <sys/time.h>
#include "hatlib.h"
#include "control.h"
/* #include "ephem.h" */
struct asource so ;
char mess[80], error[80] ;
struct timeval tp ;
struct timezone tzp ;
float atod_(int *ant,int *mpx,int *raw,char units[4],char name[16],char error[80]) ;
char *getatod(int *ant,int *mpx) ;

main(int argc, char *argv[])
{
  int ants, onesec = 100, nants = NANTS, ant ;
  char antstring[20] ;
  float vp15,vm15,v5 ;
  int d1000 = 1000, d1001 = 1001, d1002 = 1002, d1092 = 1092, d1100 = 1100 ;
  int d1094 = 1094 ;
  int  x98 = 0x98, x2c = 0x2c, x2e = 0x2e ;
  int itrack[NANTS], it, raw ;
  char polcode, error[80], me[8], units[4], name[16] ;
  float totpow, axiserr[NANTS][2] ;
  double a ;
  int xe4 = 0xe4, d24 = 24, d18 = 18,d0 = 0, d1 = 1, resoff[NANTS][2], d2 = 2 ;
  int slewflag, atlimit, v, vv, f, x13a = 0x13a, d3 = 3 ;
  float foc, tilt ;
  int ifoc ;
  int x24 = 0x24 ;
  double ra2000, dec2000, pi = 3.141592654 ;
  float focus[NANTS] ;

	if(argc>1) sscanf(argv[1],"ants=%s",antstring) ;
	else {
	   printf("USAGE: astatus ants=xxx \n") ;
	   exit(0) ;
	}
	pants_(antstring,&ants,strlen(antstring)) ;
	if (ants == 0) {
	   printf("Illegal value for ants\n") ;
	   exit(0) ;
	}
	printf("ASTATUS (version 1.2 7-aug-96)\n") ;
/*	lockexit_() ;  */
	mbusopen_(error) ;
	if (strncmp(error,"OK",2) != 0) {
		printf("mbusopen: %s\n",error) ;
		exit() ;
	}
	comgeti_("RESOFF",resoff[0],&d24,error) ;
	initscr() ;

	do {
	  gettimeofday(&tp,&tzp) ;
	  sprintf(mess," ANTENNA STATUS:  %s  ",ctime(&tp.tv_sec)) ;
	  move(0,10) ; addstr(mess) ;
	  move(1,0) ; 
	  addstr("ANTS:    --1--  --2--  --3--  --4--  --5--  --6--  --7--"
		 "  --8--  --9--  --A--") ;

/*  --	POWER SUPPLIES	*/
	  strcpy(mess,"RVR PWR:") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(ant == 3) strcat(mess,"       ") ;
		else if (select_(&ant,&ants)) {
			v5 = atod_(&ant,&d1001,&raw,units,name,error);
			vp15 = atod_(&ant,&d1000,&raw,units,name,error);
			vm15 = atod_(&ant,&d1002,&raw,units,name,error);
			if ((v5 > 4.8) && (v5 < 5.2) && (vp15 > 14.6) && (vp15 < 15.2) && \
				(vm15 < -14.8) && (vm15 > -15.2)) strcat(mess,"    OK ");
			else strcat(mess,"  BAD  ");
		}
	    else strcat(mess,"       ") ;
	  }
	  move(2,0) ; addstr(mess) ;

/*	--	POLARIZATION FRAME */

	  strcpy(mess,"POL:   ");
	  for (ant=1; ant<=NANTS; ant++) {
		if (select_(&ant,&ants)) {
			polpos_(&ant,&polcode);
			sprintf(me,"     %c ",polcode);
			strncat(mess,me,7);
		}
		else strcat(mess,"       ");
	  }
	  move(3,0) ; addstr(mess) ;

/*  --	ITRACK  		*/
	  comgeti_("ITRACK",itrack,&nants,error) ;
	  strcpy(mess,"ITRACK: ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		it = itrack[ant-1]&0x1f ;
		if (it == AZEL)  strcpy(me," AZ") ;
		else if (it == RADEC) strcpy(me," RA") ;
		else if ((it&0x18) == CONSVELAZ) strcpy(me," VA") ;
		else if ((it&0x18) == CONSVELEL)strcpy(me," VE") ;
		else strcpy(me,"   ") ;
		it = itrack[ant-1]&0x3fe0 ;
		if ((it&0x7e0) != 0) strcat(me,".") ;
		if ((it&HORNOFF) != 0) strcat(me,"h") ;
		if ((it&ADDRA) != 0) strcat(me,"r") ;
		if ((it&CORRECT) != 0) strcat(me,"c") ;
		if ((it&ELOVER90) != 0) strcat(me,"n") ;
		if ((it&ELUNDER90) != 0) strcat(me,"s") ;
		if ((it&OFFSETS) != 0) strcat(me,"+") ;
		strncat(me,"       ",7) ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(6,0) ; addstr(mess) ;
/*  --	SOURCE		*/
	  comgeti_("SORCDATA",(int *)&so,&d24,error) ;
	  ra2000 = so.ra2000*12./pi ;
	  dec2000 = so.dec2000*180./pi ;
	  so.name[7] = 0 ;
	  sprintf(mess,\
	    "SOURCE: %s  RA2000: % .5lf  DEC2000: % .5lf  ",
		so.name,ra2000,dec2000) ;
	  move(23,0) ; addstr(mess) ; 
/*  --	AZIM		*/
	  strcpy(mess,"AZIM:  ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		a = encoder_(&ant,&d0,&resoff[ant-1][0],error) ;
		sprintf(me,"%7.2lf",a) ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(7,0) ; addstr(mess) ;
/*  --	AZERR		*/
	  comgetr_("AXISERR",axiserr[0],&d24,error) ;
	  strcpy(mess,"AZerr: ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		sprintf(me,"%7.1lf",axiserr[ant-1][0]) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(8,0) ; addstr(mess) ;
/*  --	AZVEL		*/
	  strcpy(mess,"AZvel: ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		v = tpeek_(&ant,&x2c,&d2,error) ;
		vv = v & 0xfff ;
		if ((v&0x100) != 0) vv = -vv ;
		sprintf(me,"%7d",vv) ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(9,0) ; addstr(mess) ;
/*  --	AZSTATUS		*/
	  comgeti_("SLEWFLAG",&slewflag,&d1,error) ;
	  comgeti_("ATLIMIT",&atlimit,&d1,error) ;
	  strcpy(mess,"AZstat: ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		if((atlimit & 1<<(ant-1)*2)!=0) strcpy(me," LIMIT ") ;
		else if((slewflag & 1<<(ant-1)*2)!=0) strcpy(me," SLEW  ") ;
		else strcpy(me,"       ") ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(10,0) ; addstr(mess) ;

/*  --	ELEV		*/
	  strcpy(mess,"ELEV:  ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		a = encoder_(&ant,&d1,&resoff[ant-1][1],error) ;
		sprintf(me,"%7.2lf",a) ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(12,0) ; addstr(mess) ;
/*  --	ELERR		*/
	  strcpy(mess,"ELerr: ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		sprintf(me,"%7.1lf",axiserr[ant-1][1]) ;
		strcat(mess,me) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(13,0) ; addstr(mess) ;
/*  --	ELVEL		*/
	  strcpy(mess,"ELvel: ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		v = tpeek_(&ant,&x2e,&d2,error) ;
		vv = v & 0xfff ;
		if ((v&0x100) != 0) vv = -vv ;
		sprintf(me,"%7d",vv) ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(14,0) ; addstr(mess) ;
/*  --	ELSTATUS		*/
	  strcpy(mess,"ELstat:  ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		if((atlimit & 2<<(ant-1)*2)!=0) strcpy(me,"LIMIT") ;
		else if((slewflag & 2<<(ant-1)*2)!=0) strcpy(me,"SLEW") ;
		else strcpy(me,"       ") ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(15,0) ; addstr(mess) ;
/*  --	FOCUS			*/
	  strcpy(mess,"FOCUS: ") ;
	  getfocus_(&ants,focus,error) ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
	      sprintf(me,"%7.3f",focus[ant-1]) ;
	      strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(17,0) ; addstr(mess) ;

/*  --	TILT			*/
	  strcpy(mess,"TILT:  ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		tilt = atod_(&ant,&d1092,&raw,units,name,error) ;
		sprintf(me,"%7.2f",tilt) ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(18,0) ; addstr(mess) ;

/*  --	TOTAL POW		*/
	  strcpy(mess,"POWER: ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
	        totpow = atod_(&ant,&d1094,&raw,units,name,error) ;
		sprintf(me,"%7.4f",totpow) ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(19,0) ; addstr(mess) ;
/*  --	FAULTS			*/
	  strcpy(mess,"FAULTS: ") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		f = tpeek_(&ant,&x13a,&d3,error) ;
		if ((f&0x1) != 0) strcpy(me,"EL LIM ") ;
		else if ((f&0x2) != 0) strcpy(me,"SERVICE") ;
		else if ((f&0x4) != 0) strcpy(me,"AZ ULIM") ;
		else if ((f&0x8) != 0) strcpy(me,"KEY OFF") ;
		else if ((f&0x10) != 0) strcpy(me,"WATER P") ;
		else if ((f&0x40) != 0) strcpy(me,"AZ LIM ") ;
		else if ((f&0x80) != 0) strcpy(me,"EL ULIM") ;
		else if ((f&0x100) == 0) strcpy(me,"AZM HOT") ;
		else if ((f&0x200) == 0) strcpy(me,"TV FLAP") ;
		else if ((f&0x400) == 0) strcpy(me,"ELV HOT") ;
		else if ((f&0x800) == 0) strcpy(me,"REC HOT") ;
		else if ((f&0x1000) == 0) strcpy(me,"CAB HOT") ;
		else if ((f&0x8000) != 0) strcpy(me,"BANG!! ") ;
		else if ((f&0x800000) == 0) strcpy(me,"POW OFF") ;
		else strcpy(me,"   --  ") ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(20,0) ; addstr(mess) ;

/*  --	TV			*/
	  strcpy(mess,"TV FLAP:") ;
	  for(ant=1;ant<=NANTS;ant++) {
	    if(select_(&ant,&ants)) {
		f = tpeek_(&ant,&x13a,&d1,error) ;
		if ((f&0x40) != 0) strcpy(me,"TVOPEN ") ;
		else strcpy(me,"TVSHUT ") ;
		strncat(mess,me,7) ;
	    }
	    else strcat(mess,"       ") ;
	  }
	  move(21,0) ; addstr(mess) ;

	  refresh() ;
	  wait_ticks_(&onesec) ;
	}  while (1) ;
}
/*  --	read atod and check error status   */
char *getatod(int *ant,int *mpx)
{
  float t ;
  char error[80] ;
  static char results[8] ;
  int raw ;
  char units[4], name[16] ;

	t = atod_(ant,mpx,&raw,units,name,error) ;
	if (strncmp(error,"OK",2) == 0) 
	  sprintf(results,"%7.2f",t) ;
	else
	  strcpy(results,"BAD    ") ;
	return results ;
}


