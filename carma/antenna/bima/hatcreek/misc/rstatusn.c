/** RSTATUSN	- display all aspects of receiver			*/
/*: basic								*/
/*+
	RSTATUS ANT=xxx

	RSTATUS displays all quantities that pertain to an individual
	receiver.  The information fills a screen and is updated
	each second.  Several quantities such as loopgain are shown
	as both the value sent out and the value read back from the
	receiver which is in parenthesis; these should match.
	If thermistors or certain other quantities send back 
	illegal values (v>4.45), the value is reported as BAD.
	All of the reported values have been converted to the
	units that are used on the meters mounted on the equipment.	*/

/*@	 ant								*/
/*	The number of the antenna to monitor.
									*/
/*--	jul 93 wh - switch to using atod()				*/
#include <string.h>
#include <unistd.h>
#define NANTS 10 ;
#include <curses.h>
#include <time.h>
#include <sys/time.h>
#include "hatlib.h"
char mess[80], error[80] ;
struct timeval tp ;
struct timezone tzp ;
char *therm(int *ant,int *mpx) ;
char  t1[15], t2[15], t3[15], t4[15], t5[15], t6[15], tbc[15] ;

main(int argc, char *argv[])
{
  int ant, onesec = 100 ;

  int raw ;
  char units[4], name[16] ;
  float v15,vm15,v5,pol ;
  char polcode;
  int d1000 = 1000, d1002 = 1002, d1001 = 1001, d1013 = 1013, d1081 = 1081 ;

  float refbc,mmoscb,mmbckb,attenb,mxrbckb ;
  int d1075 =  1075, d1072 =  1072, d1054 =  1054, d1076 =  1076 ;

  float refad,mmosca,mmbcka,attend ;
  int d1074 =  1074, d1071 =  1071, d1056 =  1056;

  float lbanderr,lbandif ;
  int bitsin, x5e = 0x5e, d1008 =  1008, d1007 =  1007, two = 2, x5b = 0x5b ;
  char lock[5] ;

  float xbanderr, xbandif, yigout, xbandmon ;
  int d1098 =  1098, d1097 =  1097, x52m = 0x52-256, d1099 =  1099, x6e = 0x6e ;

  float mbanderr, mbandif,mloopg,mloopmon,phnoisemon ;
  int d1069 =  1069, d1068 =  1068, x66m = 0x66-256, d1070 =  1070, d1016 = 1016 ;

  float mvop, mvopb, mvopa ;
  char band, refok[4], sweepon[4], oscon[4] ;
  int x6b = 0x6b, x64m = 0x64-256, d1051 =  1051, d1050=1050 ;
  static char bands[4] = { 'A', 'B', 'C', 'D'} ;

  char ifon[4], atten[5], echoref[4] ;
  float totpow ;
  int d1094 =  1094 ;

  float echo,echomon,echoa,echob ;
  int x6dm = 0x6d-256, d1058 =  1058, x6em = -146, x6fm = 0x6f-256, one = 1 ;
  int b78, x3cm = 0x3c-256 ;
  char s78[4], string78[4][4] = {"ON","OFF","REV","OFF" } ;


  float htrmon,htrmon_stg2,htrmon_stg3 ;
  int d1080 =  1080, d1021 =  1021, d1009 =  1009, x9c = 0x9c ;

  int d1091 =1091;
  int d1082 =1082, d1083 =1083, d1084 =1084, d1085 =1085, d1086 = 1086, d1087 = 1087 ;

  float htrout_stg2,htrout_stg3, shotbias ;
  char loop[2],sisunits[3] ;
  int x54m = 0x54-256, x56m = 0x56-256, d1005 =  1005, d1048 =  1048 ;
  int x62m = 0x62-256;

  float sismxr, sismxv, sismxc ;

  float spress, rpress ;
  int d1004 =1004, d1003 =1003, d1019 = 1019, d1015 =1015, d1017 =1017, d1020 =1020 ;
  int d1006 =  1006, d1049 =  1049 ;
  int d1057 =  1057, d1012 =  1012 ;
  float calib ;
  float modout, modmon,modaout,modamon ;
  int x50m = 0x50-256, d1078 =  1078, x60m = 0x60-256, d1077 = 1077 ;
  int d1073 = 1073, d1088 = 1088;
  char enab50, enab60;
  char enab10;
  int x18 = 0x18;
  int x58 = 0x58, x68 = 0x68 ;

  double lo1,oscfreq,xfreq,refmhz ;
  int mharm,nharm;
  char sant[2] ;
  
	if(argc>1) sscanf(argv[1],"ant=%x",&ant) ;
	else {
	   printf("USAGE: rstatus ant=xxx \n") ;
	   exit(0) ;
	}
	if (ant == 0) {
	     printf("Illegal value 0 for ant\n") ;
	     exit(0) ;
	}
	printf("RSTATUS (version 1.1 30apr97)\n") ;
	if (ant ==3) { 
	  printf("RSTATUS3 is the correct program for this antenna\n") ;
	  exit(0) ;
	}
/*
	lockexit_() ;
*/
	mbusopen_(error) ;
	if (strncmp(error,"OK",2) != 0) {
		printf("mbusopen: %s\n",error) ;
		exit(1) ;
	}

	initscr() ;
	do {
	  gettimeofday(&tp,&tzp) ;
	  sprintf(mess," RECEIVER STATUS:  ant=%X  %s  ",ant,ctime(&tp.tv_sec)) ;
	  move(0,10) ; addstr(mess) ;

/*  --	COMMON BLOCK	*/
	  comgetd_("LO1",&lo1,&one,error) ;
	  comgetd_("oscfreq",&oscfreq,&one,error) ;
	  comgeti_("mharm",&mharm,&one,error);
	  comgetd_("xfreq",&xfreq,&one,error);
	  comgeti_("nharm",&nharm,&one,error);	
	  comgetd_("REFMHZ",&refmhz,&one,error) ;
	  sprintf(mess,"FREQS: LO1=%7.3f   Oscfreq=%7.3f  Mharm= %d  Xfreq=%6.3f  Synth=%.6lf   ", \
		lo1,oscfreq,mharm,xfreq,refmhz) ;
	  move(1,0) ; addstr(mess) ;


/*  --	POWER SUPPLIES	*/
	  v15 = atod_(&ant,&d1000,&raw,units,name,error) ;
	  vm15 = atod_(&ant,&d1002,&raw,units,name,error) ;
	  v5 = atod_(&ant,&d1001,&raw,units,name,error) ;
	  enab10 = (tpeek_(&ant,&x18,&one,error) & 0x0010) ? 'Y' : 'N' ;
	  enab50 = (tpeek_(&ant,&x58,&one,error) & 0x0010) ? 'Y' : 'N' ;
	  enab60 = (tpeek_(&ant,&x68,&one,error) & 0x0010) ? 'Y' : 'N' ;
	  sprintf(mess,"PWR:   +15= %4.1f  -15= %4.1f  +5= %3.1f    Tune enable " \
		"card 0x10 %c, 0x50 %c, 0x60 %c", v15,vm15,v5,enab10,enab50,enab60) ;
	  move(2,0) ; addstr(mess) ;

/*  --	band B motors		*/
	  refbc = atod_(&ant,&d1073,&raw,units,name,error) ;
	  modout = (float)((tpeek_(&ant,&x50m,&two,error))-1024) * 350./1024. ;
	  modmon = atod_(&ant,&d1078,&raw,units,name,error) ;
	  mmoscb = atod_(&ant,&d1075,&raw,units,name,error) ;
	  mmbckb = atod_(&ant,&d1072,&raw,units,name,error) ;
	  attenb = atod_(&ant,&d1054,&raw,units,name,error) ;
	  mxrbckb = atod_(&ant,&d1076,&raw,units,name,error) ;
	  mvopb = atod_(&ant,&d1051,&raw,units,name,error) ;
	  if (ant == 1) {
	  	sprintf(mess,"LO-B:  Vop=%4.2f  Ref=%5.0f  MMosc=%5.0f  MMbck=%5.0f  Atten=%5.0f "\
			" Mxrbck=%5.0f ", mvopb,refbc,mmoscb,mmbckb,attenb,mxrbckb) ;
	  }
	  else {
	    sprintf(mess,"LO-B:  Vop=%4.2f  Ref=%5.0f  MMosc=%5.0f  MMbck=%5.0f  Atten=%5.0f "\
			" Mod=%3.0f(%4.0f)", mvopb,refbc,mmoscb,mmbckb,attenb,modout,modmon) ;
	  }
	  move(3,0) ; addstr(mess) ;

/*  --	band A/D motors		*/
	  refad = atod_(&ant,&d1088,&raw,units,name,error) ;
	  mmosca = atod_(&ant,&d1074,&raw,units,name,error) ;
	  mmbcka = atod_(&ant,&d1071,&raw,units,name,error) ;
	  attend = atod_(&ant,&d1056,&raw,units,name,error) ;
	  modaout = (float)((tpeek_(&ant,&x60m,&two,error))-1024) * 350./1024. ;
	  modamon = atod_(&ant,&d1077,&raw,units,name,error) ;
	  mvopa = atod_(&ant,&d1050,&raw,units,name,error) ;
	  sprintf(mess,"LO-D:  Vop=%4.2f  Ref=%5.0f  MMosc=%5.0f  MMbck=%5.0f  Atten=%5.0f "\
			" Mod=%3.0f(%4.0f)", mvopa,refad,mmosca,mmbcka,attend,modaout,modamon) ;
	  move(4,0) ; addstr(mess) ;

/*  --	L BAND: 	*/
	  bitsin = tpeek_(&ant,&x5e,&two,error) ;
	  lbanderr = atod_(&ant,&d1008,&raw,units,name,error) ;
	  lbandif = atod_(&ant,&d1007,&raw,units,name,error) ;
	  if((bitsin&0x4) != 0) strcpy(lock,"Lock") ;  else strcpy(lock,"BAD ") ;
	  sprintf(mess,"L LCK: Status= %s  Err volts= % .2f  IF Level= %.2f   ",
			lock,lbanderr,lbandif) ;
	  move(6,0) ; addstr(mess) ;

/*  --	X BAND:		*/
	  yigout = ((float)(tpeek_(&ant,&x52m,&two,error)) * 4.5/2048.) +8. ;
/*
	  xbanderr = atod_(&ant,&d1098,&raw,units,name,error) ;
*/
	  xbanderr = xerrv_(&ant);
	  xbandif = atod_(&ant,&d1097,&raw,units,name,error) ;
	  xbandmon = atod_(&ant,&d1099,&raw,units,name,error) ;
	  if((bitsin&0x2) != 0) strcpy(lock,"Lock") ;  else strcpy(lock,"BAD ") ;
	  sprintf(mess,
	  "X LCK: Status= %s  Err volts= % .2f  IF Level= %.2f "\
		" YIG tune= %.2f (%.2f)   ",lock,xbanderr,xbandif,yigout,xbandmon) ;
	  move(7,0) ; addstr(mess) ;

/*  --	MM BAND		*/
	  bitsin = tpeek_(&ant,&x6e,&two,error) ;
	  mbanderr = atod_(&ant,&d1069,&raw,units,name,error)  ;
	  mbandif = atod_(&ant,&d1068,&raw,units,name,error) ;
	  if((bitsin&0x100) != 0) strcpy(lock,"Lock") ;  else strcpy(lock,"BAD ") ;
	  mloopg = (float)(tpeek_(&ant,&x66m,&two,error)) * 10./2048. ;
	  mloopmon = atod_(&ant,&d1070,&raw,units,name,error) ;
	  sprintf(mess,"MMLCK: Status= %s  Err volts= % .2f  IF Level= %.2f "\
		    " Loop Gain= %.2f (%.2f)   ",lock,mbanderr,mbandif,\
			mloopg,mloopmon) ;
	  move(8,0) ; addstr(mess) ;

	  if ((bitsin&0x200) != 0) strcpy(refok,"OK") ;  else  strcpy(refok,"BAD") ;
	  if ((bitsin&0x400) !=0) strcpy(echoref,"OK") ; else strcpy(echoref,"BAD") ;
	  bitsin = tpeek_(&ant,&x6b,&two,error) ;
	  if ((bitsin&0x80) ==0) strcpy(sweepon,"ON") ; else strcpy(sweepon,"OFF") ;
	  if ((bitsin&0x40) != 0) strcpy(oscon,"ON"); else strcpy(oscon,"OFF") ;
	  band = bands[(int)((bitsin&0x000c)>>2) ] ;
	  mvop = ((float)(tpeek_(&ant,&x64m,&two,error)) * 2.222/2048.) + 7.5 ;
	  mvopb = atod_(&ant,&d1051,&raw,units,name,error) ;
	  phnoisemon = atod_(&ant,&d1016,&raw,units,name,error) ;
	  sprintf(mess,"MMLCK: Band= %c  70_MHz_Ref= %s  Sweep= %s  Osc= %s  "\
	      " Phnoise= %.2f  Vop= %.2f   ",band,refok,sweepon,oscon,phnoisemon,mvop) ;
	  move(10,0) ; addstr(mess) ;

/*  --	IF		*/
	  band = bands[(int)(bitsin&0x3) ] ;
	  if ((bitsin&0x20) !=0) strcpy(ifon,"ON") ; else strcpy(ifon,"OFF") ;
/*
	  if ((bitsin&0x10) !=0) strcpy(atten,"0DB") ; else strcpy(atten,"13DB") ;
*/
	  totpow = atod_(&ant,&d1094,&raw,units,name,error) ;
	  calib = atod_(&ant,&d1057,&raw,units,name,error) ;
	  pol = atod_(&ant,&d1081,&raw,units,name,error) ;
	  polpos_(&ant,&polcode);
	  sprintf(mess,"2ndIF: Band= %c  Amp= %s  Atten= %2d dB  Power=%7.4f" \
		"  Cal= %5.0f  Pol= %5.0f (%c)",
		band,ifon,ifdb_(&ant),totpow,calib,pol,polcode) ;
	  move(11,0) ; addstr(mess) ;

/*  --	TEMPS	*/
	  htrmon = atod_(&ant,&d1080,&raw,units,name,error) ;
	  strcpy(t1,therm(&ant,&d1021)) ;
	  strcpy(t2,therm(&ant,&d1009)) ;
	  strcpy(t4,therm(&ant,&d1013)) ;
	  strcpy(t5,therm(&ant,&d1012)) ;
	  sprintf(mess,"TEMPS: Htr=%4.2f  Tplate=%s  T2ndIF=%s "\
		" Tbc=%s  Tamb=%s",htrmon,t1,t2,t4,t5) ;
	  move(13,0) ; addstr(mess) ;

/*  --	ECHO		*/
	  b78 = tpeek_(&ant,&x3cm,&one,error) ;
	  strcpy(s78,string78[(int)(b78&3)]) ;
	  echo = (float)(tpeek_(&ant,&x6dm,&one,error)) * 5./128. ;
	  echomon = atod_(&ant,&d1058,&raw,units,name,error) ;
	  echob = (float)(tpeek_(&ant,&x6em,&one,error)) * 5./128. ;
	  echoa = (float)(tpeek_(&ant,&x6fm,&one,error)) * 5./128. ;
/* wiring reversed on 8 bit DACs, bal B is 6E, bal A is 6F on all
antennas - niranjan thatte jul 27, 93 */
	  sprintf(mess,"ECHO:  Input_level=%s  78_Hz=%s  Level=%4.2f (%4.2f)  Bal_A=%4.2f " \
			" Bal_B=%4.2f   ",echoref,s78,echo,echomon,echoa,echob) ;
	  move(15,0) ; addstr(mess) ;

/*  --	DEWAR		*/
	  strcpy(t1,therm(&ant,&d1082)) ;
	  strcpy(t2,therm(&ant,&d1083)) ;
	  strcpy(t3,therm(&ant,&d1084)) ;
	  strcpy(t4,therm(&ant,&d1085)) ;
	  strcpy(t5,therm(&ant,&d1086)) ;
	  sprintf(mess,"DEWAR: sensor 1= %s  2= %s  3= %s  4= %s  5= %s      ",
		  t1,t2,t3,t4,t5) ;
	  move(17,0) ; addstr(mess) ;

	  htrout_stg2 = (float)(tpeek_(&ant,&x62m,&two,error)) * 12.1/2048. ;
	  htrmon_stg2 =  atod_(&ant,&d1091,&raw,units,name,error)  ;/* ma */
	  htrout_stg3 = (float)(tpeek_(&ant,&x54m,&two,error)) * 12.1/2048. ;
	  htrmon_stg3 =  atod_(&ant,&d1005,&raw,units,name,error)  ;/* ma */
	  sprintf(mess,"DEWAR: stage 2 heater = %5.2f V, %4.1f mA    stage 3 heater = "\
		"%5.2f V, %4.1f mA ",htrout_stg2,htrmon_stg2,htrout_stg3,htrmon_stg3) ;

	  move(18,0) ; addstr(mess) ;

	  band = bands[(int)(bitsin&0x3) ] ;

/*
	  if (ant==1) {
	  	shotbias =  atod_(&ant,&d1048,&raw,units,name,error) ;
	  	sprintf(mess,"Schottky bias = %.3f V                                     ", \
			shotbias) ;
	  }

	  else {
*/
	  	bitsin = tpeek_(&ant,&x5b,&two,error) ;
	  	sismxr = (float)(tpeek_(&ant,&x56m,&two,error)) * 20./2048. ; /* commanded bias */
	  	if((bitsin & 0x0014) == 0x0004) {
			strcpy(loop,"v") ; 
			strcpy(sisunits,"mV");
	  	}
	  	else if ((bitsin & 0x0014) == 0x0010) {
			strcpy(loop,"o");
			strcpy(sisunits,"mV");
	  	}
	  	else {
			strcpy(loop,"i") ;
			strcpy(sisunits,"uA");
			sismxr *= 10.;
			if (ant != 7) sismxr *= 497./200.;
	  	}
	  	sismxv =  atod_(&ant,&d1049,&raw,units,name,error) ; /* mV  */
	  	sismxc =  atod_(&ant,&d1006,&raw,units,name,error) ; /* uA  */
	  	sprintf(mess,"SIS:   band %c commanded bias = %6.2f %s, mode %s; readback %5.2f mV, %6.2f uA  ", \
			band,sismxr,sisunits,loop,sismxv,sismxc) ;

	  move(20,0) ; addstr(mess) ;

/*  --	HELIUM		*/
	  spress =  atod_(&ant,&d1004,&raw,units,name,error) ;
	  rpress =  atod_(&ant,&d1003,&raw,units,name,error) ;
	  sprintf(mess,"HELIUM: Supply pressure= %.0f  Return pressure= %.0f   ",
				spress,rpress) ;
	  move(22,0) ; addstr(mess) ;

	  strcpy(t1,therm(&ant,&d1019)) ;
	  strcpy(t2,therm(&ant,&d1015)) ;
	  strcpy(t3,therm(&ant,&d1017)) ;
	  strcpy(t4,therm(&ant,&d1020)) ;
	  sprintf(mess,"HELIUM: T inlet= %s  T gas dis= %s  T heat exch= %s "\
		" T sump= %s   ",t1,t2,t3,t4) ;
	  move(23,0) ; addstr(mess) ;

	  refresh() ;  
	  wait_ticks_(&onesec) ;
	}  while (1) ;
}
/*  --	read a thermistor and check error status   */
char *therm(int *ant,int *mpx)
{
  float t ;
  char error[80] ;
  static char results[15], blank[2] = " " ;
  char hold[15] ;
  int raw ;
  char units[4], name[16] ;

	t = atod_(ant,mpx,&raw,units,name,error) ;
	if (strncmp(error,"OK",2) == 0) 
	  sprintf(results,"%5.2f",t) ;
	else
	  strcpy(results,"BAD") ;
	return results ;
}

