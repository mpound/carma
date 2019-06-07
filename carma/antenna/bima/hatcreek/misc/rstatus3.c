/** Rstatus3	- display all aspects of receiver			*/
/*: basic								*/
/*+
	RSTATUS3 ANT=xxx
   
	RSTATUS3 presents a status/parameter display for the selected
	receiver.

  ARGUMENTS:
  ANT(int,input)	Antenna number (1-12)
	oct 92  wh
									*/
/*--									*/
#include <string.h>
#include <unistd.h>
#include "constants.h"
#include <curses.h>
#include <time.h>
#include <sys/time.h>
#include "hatlib.h"
char mess[80], error[80] ;
struct timeval tp ;
struct timezone tzp ;
char *therm(int *ant,int *mpx) ;
char  t1[15], t2[15], t3[15], t4[15], t5[15], t6[15] ;

main(int argc, char *argv[])
{
  int atodraw;
  char units[4],name[16];
  int d1003 = 1003,d1004 = 1004, d1012 = 1012, d1021 = 1021, d1009 = 1009 ;
  int d1019 = 1019, d1015 = 1015 ;
  int ant, onesec = 82 ;
  float temp;

  int mmoscb,mmbckb,attenb,mxrbckb,cavity ;
  int xf0 = 0xf0, xf8 = 0xf8, xf4 = 0xf4, xf6 = 0xf6, xf2 = 0xf2 ;

  float lbanderr,lbandif ;
  int bitsin, x5e = 0x5e, x86 = 0x86, x82 = 0x82, two = 2, x5b = 0x5b ;
  char lock[5],chop[5],polar[5],flap[5] ;

  float xbanderr, xbandif, yigout, yigmon, xbandmon ;
  int x8e = 0x8e, x8a = 0x8a, x12m = 0x12-256, x1e = 0x1e ;
  int x7e = 0x7e, x5bm = 0x5b-256 ;

  float mbanderr, mbandif,mloopg ;
  int x8c = 0x8c,  x88 = 0x88, x16m = 0x16-256 ;

  float mvop, mvopb ;
  char band, refok[4], sweepon[4] ;
  int  x14m = 0x14-256, xda = 0xda ;
  static char bands[4] = { 'A', 'B', 'C', 'D'} ;

  char ifon[4], atten[5], echoref[4] ;
  float totpow ;
  int xd0 = 0xd0 ;

  float echo,echomon,echoa,echob ;
  int x52m = 0x52-256, x11c = 0x11c, one = 1 ;
  int b78, x3cm = 0x3c-256 ;
  char s78[4], string78[4][4] = {"ON","OFF","REV","OFF" } ;


  float htrmon ;
  int xd6 = 0xd6, x9c = 0x9c ;

  int d1082 = 1082, d1083 = 1083, d1084 = 1084, d1085 = 1085 ;

  float htrout, shotbias ;
  char loop[7] ;
  int x54m = 0x54-256, x56m = 0x56-256, xb2 = 0xb2, xca = 0xca ;

  float sismxr, sismxv, sismxc ;

  float spress, rpress ;
  int x78 = 0x78, xa8 = 0xa8 ;
  int x92 = 0x92 ;
  float modout, modmon ;
  int x50m = 0x50-256, xea = 0xea ;
  char enab[10] ;

	if(argc>1) sscanf(argv[1],"ant=%x",&ant) ;
	else {
	   printf("USAGE: rstatus3 ant=xxx \n") ;
	   exit(0) ;
	}
	if (ant==0) {
	  printf("Illegal value 0 for antenna\n");
	  exit(0) ;
	}

	printf("RSTATUS3 (version 1.1 16-mar-95)\n") ;
	if (ant !=1 && ant!=3) {
	  printf("RSTATUS is the correct program for this antenna\n") ;
	  exit(0) ;
	}
	else if (ant==1) {
	  printf("RSTATUS1 is the correct program for this antenna\n");
	  exit(0) ;
	}
/*	lockexit_() ;  */
	mbusopen_(error) ;
	if (strncmp(error,"OK",2) != 0) {
		printf("mbusopen: %s\n",error) ;
		exit() ;
	}

	initscr() ;
	do {
	  gettimeofday(&tp,&tzp) ;
	  sprintf(mess," RECEIVER STATUS:  ant=%d  %s  ",ant,ctime(&tp.tv_sec)) ;
	  move(0,10) ; addstr(mess) ;

/*  --	MOTORS:		*/
	  mmoscb = resolvin_(&ant,&xf0,error) ;
	  mmbckb = resolvin_(&ant,&xf8,error) ;
	  attenb = resolvin_(&ant,&xf4,error) ;
	  mxrbckb = resolvin_(&ant,&xf6,error) ;
	  cavity = resolvin_(&ant,&xf2,error) ;
	  sprintf(mess,"MOTORS: MMosc B= % d  MMbck B= % d  Atten B= % d "\
			" Mxrbck B= % d   ", mmoscb,mmbckb,attenb,mxrbckb) ;
	  move(3,0) ; addstr(mess) ;
	  bitsin = tpeek_(&ant,&x5e,&one,error) ;
	  if((bitsin&0x4) != 0) strcpy(enab,"Enabled") ;  else strcpy(enab,"Disabled") ;
	  sprintf(mess,"MOTORS: Cavity B= % d   TUNE: %s  ",cavity,enab) ;
	  move(4,0) ; addstr(mess) ;
/*  --	L BAND: 	*/
	  bitsin = tpeek_(&ant,&x1e,&two,error) ;
	  lbanderr = (float)atodin_(&ant,&x86,error) /2690.5 -5. ;
	  lbandif = (float)atodin_(&ant,&x82,error) /2690.5 ;
	  if((bitsin&0x4) != 0) strcpy(lock,"Lock") ;  else strcpy(lock,"BAD ") ;
	  sprintf(mess,"LBAND: Status= %s  Err volts= % .2f  IF Level= %.2f   ",
			lock,lbanderr,lbandif) ;
	  move(5,0) ; addstr(mess) ;

/*  --	X BAND:		*/
	  yigout = ((float)(tpeek_(&ant,&x12m,&two,error)) * 4.5/2048.) +8. ;
	  xbanderr = (float)atodin_(&ant,&x8e,error) /2690.5 - 4.1814 ;
	  xbandif = (float)atodin_(&ant,&x8a,error) /2690.5;
	  yigmon = (float)atodin_(&ant,&x7e,error) /26905.*4.5 + 8. ;
	  if((bitsin&0x2) != 0) strcpy(lock,"Lock") ;  else strcpy(lock,"BAD ") ;
	  sprintf(mess,
	  "XBAND: Status= %s  Err volts= % .2f  IF Level= % .2f "\
		" YIG tune= % .2f (% .2f)   ",lock,xbanderr,xbandif,yigout,yigmon) ;
	  move(6,0) ; addstr(mess) ;

/*  --	MM BAND		*/
	  bitsin = tpeek_(&ant,&x5e,&two,error) ;
	  mbanderr = (float)atodin_(&ant,&x8c,error) /2690.5 -5. ;
	  mbandif = (float)atodin_(&ant,&x88,error) /2690.5 ;
	  if((bitsin&0x2) != 0) strcpy(lock,"Lock") ;  else strcpy(lock,"BAD ") ;
	  mloopg = (float)(tpeek_(&ant,&x16m,&two,error)) * 10./2048. ;
	  sprintf(mess,"MM:    Status= %s  Err volts= % .2f  IF Level= % .2f "\
		    " Loop Gain= % .2f   ",lock,mbanderr,mbandif,mloopg) ;
	  move(7,0) ; addstr(mess) ;

	  if ((bitsin&0x4) != 0) strcpy(refok,"OK") ;  else  strcpy(refok,"BAD") ;
	  if ((bitsin&0x8) !=0) strcpy(echoref,"OK") ; else strcpy(echoref,"BAD") ;
	  bitsin = tpeek_(&ant,&x5bm,&two,error) ;
	  if ((bitsin&0x10) ==0) strcpy(sweepon,"ON") ; else strcpy(sweepon,"OFF") ;
	  mvop = ((float)(tpeek_(&ant,&x14m,&two,error)) * 2.222/2048.) + 7.5 ;
	  mvopb = (float)atodin_(&ant,&xda,error) /3276.8 ;
	  sprintf(mess,"MM:   Lock Ref= %s  Sweep= %s   "\
	      "Lock VOP=%.2f (% .2f)   ",refok,sweepon,mvop,mvopb) ;
	  move(9,0) ; addstr(mess) ;

/*  --	IF		*/
	  if ((bitsin&0x4) !=0) strcpy(ifon,"ON") ; else strcpy(ifon,"OFF") ;
	  if ((bitsin&0x8) !=0) strcpy(atten,"0DB") ; else strcpy(atten,"10DB") ;
	  totpow = (float)atodin_(&ant,&xd0,error) /3276.8 ;
	  strcpy(t1,therm(&ant,&d1012)) ;
	  sprintf(mess,"IF:   2nd IF= %s 2nd IF Atten= %s  Power= % .4f" \
		"  Tamb=%s   ",
		    ifon,atten,totpow,t1) ;
	  move(11,0) ; addstr(mess) ;

/*  --	IF TEMPS	*/
	  htrmon = (float)atodin_(&ant,&xd6,error) /3276.8 ;
	  strcpy(t1,therm(&ant,&d1021)) ;	
	  strcpy(t2,therm(&ant,&d1009)) ;
	  sprintf(mess,"TEMPS: Heater= % .2f  T plate= %s T 2ndIF= %s "\
		"   ",htrmon,t1,t2) ;
	  move(13,0) ; addstr(mess) ;

/*  --	ECHO		*/
	  b78 = tpeek_(&ant,&x3cm,&one,error) ;
	  strcpy(s78,string78[(int)(b78&3)]) ;
	  echo = (float)(tpeek_(&ant,&x50m,&two,error)) * 5./2048. ;
	  echoa = (float)(tpeek_(&ant,&x52m,&two,error)) * 5./2048. ;
	  echob = (float)(tpeek_(&ant,&x54m,&two,error)) * 5./2048. ;
	  sprintf(mess,"ECHO: 78HZ= %s Ref= %s Level= %.2f   Bal A= %.2f " \
			" Bal B= %.2f   ",s78,echoref,echo,echoa,echob) ;
	  move(15,0) ; addstr(mess) ;
/*  --  flap   */
	  bitsin = tpeek_(&ant,&x5e,&two,error) ;
	  strcpy(flap,"BAD") ;
	  if ((bitsin&0x200) !=0) strcpy(flap,"SKY") ; 
	  if ((bitsin&0x100) !=0) strcpy(flap,"AMB") ; 
	  strcpy(polar,"BAD") ;
	  if ((bitsin&0x40) ==0) strcpy(polar,"CW") ;
	  if ((bitsin&0x80) ==0) strcpy(polar,"CCW") ;
	  sprintf(mess,"FLAP: polarization= %s flap= %s   ",
			polar,flap) ;
	  move(16,0) ; addstr(mess) ;

/*  --	DEWAR		*/
	  strcpy(t1,therm(&ant,&d1082)) ;
	  strcpy(t2,therm(&ant,&d1083)) ;
	  strcpy(t3,therm(&ant,&d1084)) ;
	  strcpy(t4,therm(&ant,&d1085)) ;
	  sprintf(mess,"DEWAR: sensor 1= %s  2= %s  3= %s  4 = %s   ",t1,t2,t3,t4) ;
	  move(17,0) ; addstr(mess) ;

	  shotbias =  (float)atodin_(&ant,&xca,error)/3276.8/8. -1.2 ;
	  sprintf(mess,"DEWAR:  Schottky bias= % .3f   ",shotbias) ;
	  move(19,0) ; addstr(mess) ;


/*  --	HELIUM		*/
	  spress =  atod_(&ant,&d1004,&atodraw,units,name,error) ;
	  rpress =  atod_(&ant,&d1003,&atodraw,units,name,error) ;
	  sprintf(mess,"HELIUM: Supply pressure= %.0f  Return pressure= %.0f   ",
				spress,rpress) ;
	  move(22,0) ; addstr(mess) ;
/*
	  strcpy(t1,therm(&ant,&d1019)) ;
	  strcpy(t2,therm(&ant,&d1015)) ;
	  sprintf(mess,"HELIUM: T inlet= %s  T gas dis= %s   ",t1,t2) ;
	  move(23,0) ; addstr(mess) ;
*/

	  refresh() ;
	  wait_ticks_(&onesec) ;
	}  while (1) ;
}
/*  --	read a thermistor and check error status   */
char *therm(int *ant,int *indx)
{
  float t ;
  char error[80] ;
  static char results[15], units[4], name[16] ;
  int raw ;

	t = atod_(ant,indx,&raw,units,name,error) ;
	if (strncmp(error,"OK",2) == 0) 
	  sprintf(results,"%.2f",t) ;
	else
	  strcpy(results,"BAD") ;
	return results ;
}

