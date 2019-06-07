/** DELAY	-	subprocess that steps the interferometer	*/
/*: basic								*/
/*+
	DELAY
	DELAY is a setuid root subprocess that is forked in subroutine 
	begin_delay to handle all of the interferometer calculations, 
	phase resetting and register stepping during an integration.
	It is killed at the end of the integration in subroutine
	end_delay. Delay gets all of its inputs from common
	variables and puts all outputs back into common or 
	hardware registers.  Delay has several new features over
	the Vax version which include:  Including the cable length
	errors of individual cables in the calculation of which
	delay setting to use and keeping track of the integrated
	gain errors caused by switching cables in and out in
	various parts of the I/F spectrum.

									*/
/*--	24feb wh - change sign of fof1 calc
	24may wh - new table of wire lengths from jack	
	21jun wh - change sign of antmiss 23jun-unchange sign	
	27jul wh - put back reading walants from common
	21sep wh - expand delaytim array to 9x9, prime array with init vals
	27sep wh - read in lanalevel each sec
	23nov wh - fix bug in delaysteptime
	03feb wh - dydiag=5 (print lanalevels)	 
	26aug wh - include weather info in refraction calc
	25apr wh - setup_transmitter bug fix	
	27aug wh - set up exit cleanup of locks	
	27sep wh - read in powers using atod; 2&8 use vf card 
	20nov wh - 2,4,5,6,7,8,9 use vf card	
	7dec  wh - fix bug in WIRE length for ant 9	
	26jan wh - add long delay bits stuff back in 
	13feb wh - bigger time-on-delay tab	
	16sep wh - put all bits in time-on-delay tab  
	6nov  wh - switch over to fiber again ant6 <> ant7 
	8nov  wh - catch instance where ants go off opposite ends of delay 
	12nov wh - change the amount of delay0 shift when off the end
	14nov wh - try to fix endless loop when ant 1 gets to limit 
	13jan97 rp - undefine FIBER to go back to B-array 
	7apr97wh - add in delay 10s wire lengths  
	5jun97wh - sample corr power ads just like front end power 
	22sep97wh - changes for band D
	20jan98 jrf - updates for a-array (wirelengths & maxdelay) 
	11dec98 jrf - re-arranged top 3 bit wires for new ants/stns setup 
	22apr99 wh  - read ovens/powers from subreflector 
	21jul99 wh  - remove corr atod read - add cable phase read */

/* removed 26Mar98 - rp
   removed 28jan99 - jrf
#define FIBER   define this symbol to return to the fiber cables 
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <math.h>
#include <sys/types.h>
#include "hatlib.h"
#include <errno.h>
#include <sys/priocntl.h>
#include <sys/rtpriocntl.h>
#include <sys/tspriocntl.h>
#include <sys/mman.h>

#define TRUE 1
#define FALSE 0
#define FOF 1
#define DLY 0
#define POW 2
#define END 3
#define NANTS 12
#define SIDERIAL 0x2000
#define TRANSMITTER 0x4000
#define SIDorTRANS 0x6000
/*
 *	Constant tables used by delay 
 */
static float FOFTOBITS = 2097.152 ;

/*
 *	Global variables
 */
static struct asource S ;
static double UT,LST,FREQ ;
static double RA,DEC,HA,SINH,COSH,SIND,COSD ;
static double HDOT, DDOT ;
static double CABLE[NANTS], ELEV[NANTS], ELEVTRUE[NANTS], LO1, LO2, LO1BT ;
static double TAU[NANTS],TAUDOT[NANTS],TAUDOTDOT[NANTS] ;
static double DELAY[NANTS], FOF1[NANTS], FOF2[NANTS] ;
static float   DELAY0[NANTS], DELAYTIM[NANTS][19], DELAY0D[NANTS] ;
static float PHASEM1[NANTS], PHASEM2[NANTS] ;
static float TPOWER[NANTS], TPOWERA[NANTS],TPOWERB[NANTS] ;
static float PHASELO1[NANTS], PHASELO2[NANTS],T300K[NANTS], T400K[NANTS] ;
static int ITRACK[NANTS], DBITS[NANTS] ;
static double ANTPOS[3][NANTS], ANTMISS[NANTS] ;
static int BADDELAY, BADRESET, NTPOWER ;
static int RESET, DYDIAG, COROPT ;
static int WALANTS[2][12] ;
static double COSLAT[NANTS], SINLAT[NANTS] ;
static float INTTIME ;
static double LASTDELAY0FIX ;
static char BAND[2] ;
#include "telemadd.h"
#ifdef FIBER
/*
 *   Table of maximum delay wire length for each antenna
 */
	/* the following table is valid as of 20 Jan 98 
  static double MOSTDELAY[10] = {7998.,7998.,7998.,7990.,7989.,3991.,7988.,
                               7987.,3995.,3998.} ; */

	/* the following table is valid as of 13 Dec 98 */
  static double MOSTDELAY[10] = {7998.,7998.,7998.,7990.,7988.,3995.,3998.,
                               7987.,3991.,7989.} ;
#else
  static double MOSTDELAY[10] = {998.,998.,998.,998.,998.,998.,998.,
                               998.,998.,998.} ; 
#endif

typedef struct astep {		/*--Table holds all parameters to step the registers ---*/
 int imfoff;			/*    =foff flag			*/
 int ticks;			/*    =current time to step				*/
 int reset;			/*    =reset time (ticks)				*/
 int lasticks;			/*    =last time stepped				*/
 int ant;			/*    =antenna number for telem				*/
 int teladd;			/*    =address for telem				*/
 double current;		/*    =floating value of register (delay)		*/
 double stepsize;		/*    =step size and sign	  (next delay)		*/
 double dtime;			/*    =step interval (secs)	  (taudot)		*/
 double ddtime;			/*    =step interval derivative   (taudotdot)		*/
 struct astep *next;		/*    =linked list pointer				*/
 }  ASTEP ;
static ASTEP *T ;
static ASTEP *TT ;
static int inuse(int,int) ;
static void andsoforth() ;
static void set_delay(int, int) ;
static void setup_siderial(int,double,double) ;
static void  setup_transmitter(int) ;
static void add_to_table(int imfoff,int nextstep,int reset,int ant,int reg,
	double current,	double stepsize,double steptime,double dd) ;
static double half_frac(double val,double direc) ;
static void check_delay_limits(int ant1,double delay) ;
static void get_from_common(int nants) ;
static int delaybits(int ant,double delay,double taudot) ;
static double bitsdelay(int ant,int bits) ;
static void timeonbit(int ant,int bits,float sec) ;
static int delaysteptime(int ant,double delay,int bits,int *bitsa,double taudot) ;
static void report(int) ;
static void calc_refract(double *,double *) ;
void main (int *argc,char *argv[])
{
 char error[80] ;
 int nants = NANTS ;
 int nants8 = NANTS*8 ;
 int which,i,j ;
 int alltimes = 2 ;
 int mon,day,year,ant,limit ;
 double tjd, julian_(), dummy = 0., refraction, arefraction ;
 int one = 1 , inbit, add_ticks, five = 5, sizes = 24 ;
 int zero = 0, ants, abit ;
 int ticksleft, ticksnow, ticks, resetbit = 0x81, resetoff = 0x1 ;
 int treset_back = TRESET -256 ;

 pcparms_t     pcparms;

	mbusopen_(error) ;
	 if (strncmp(error,"OK",2) != 0) 
			{ printf("DELAY: %s\n",error) ;exit(1) ;}
	
/*
 *	Do some fancy system stuff
 *	program must be suid root to up priority and lock pages
	*/

        pcparms.pc_cid = 3;
	((rtparms_t *)pcparms.pc_clparms)->rt_pri = 1 ;
	((rtparms_t *)pcparms.pc_clparms)->rt_tqsecs = 0 ;
	((rtparms_t *)pcparms.pc_clparms)->rt_tqnsecs = RT_TQINF ;
	          
	if((priocntl(P_PID, P_MYID, PC_SETPARMS, (caddr_t)&pcparms)) == -1)
	      perror ("priority");
	          
	 if(mlockall(MCL_CURRENT || MCL_FUTURE) < 0) perror("mlockall fails") ;

/*
 *	Get a bunch of stuff out of common
 */
	get_from_common(nants) ;
	calc_refract(&refraction,&arefraction) ;

/*
 *	Grand loop is repeated only if reset pulse fails 
 */
	do 
	{
	 BADRESET = 0 ;

/*
 *	Start off by getting the time 
 */
	 get_times_(&ticks,&alltimes,&mon,&day,&year,&UT,&LST,error) ;
	 if (strncmp(error,"OK",2) != 0) 
			{ printf("DELAY: %s\n",error) ;exit(1) ;}

/*
 *	Calculate the impending golden reset time 
 *	(the next mbus tick divisible by 32 at least 10 ticks in future)
 */
	 RESET = (((ticks + 10 + 32) /32) *32) ;
	 add_ticks = RESET - ticks ;
	 UT += add_ticks * 7.272205216e-7 ;
	 LST += add_ticks * 7.29211574e-7 ;

	 tjd = julian_(&day, &mon, &year, &UT) ;

/*
 *	Now pluck the source data out of common and extrapolate
 *	 the position to time=RESET
 */
	 comgeti_("SORCDATA",(int *)&S, &sizes, error) ;
	 if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error); exit(1);}
	 if (strncmp(S.name,"TEST",4) != 0)
	 {
	  if (S.ra0 == 0. && S.dec0 == 0.) 
		{printf("DELAY: no source in common\n"); exit(1);}

	  RA = S.ra0 + (RESET-S.tick)*S.ra1 ;
	  DEC = S.dec0 + (RESET-S.tick)*S.dec1 ;
	  HA = LST - RA ;
	  COSH = cos(HA) ;
	  SINH = sin(HA) ;
	  COSD = cos(DEC) ; 
	  SIND = sin(DEC) ;
	  computd_("OBSRA",&RA,&one,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}
	  computd_("OBSDEC",&DEC,&one,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}

/*
 *	Derivitives of HA and DEC  
 */
	  HDOT = 7.272205216e-5 * (1.002737894 - S.ra1) ;
	  DDOT = 7.272205216e-5 * S.dec1 ;
	  computd_("HDOT",&HDOT,&one,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}
	  computd_("DDOT",&DDOT,&one,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}
	 }
/*
 *	Loop through the antennas, filling in the structure Step 
 *	 (BADDELAY is set if delay has run off the end/ forcing 
 *		complete recalculation with all DELAY0s adjusted)
 */
	 limit = 100 ;
	 do 
	 {
	  T = NULL ;
	  ants = 0 ;
	  abit = 2 ;
	  BADDELAY = 0 ;
	  LASTDELAY0FIX = 0. ; /* used in check_delay_limits */

	  for (ant=1; ant<=nants; ant++) 
	  {
	    which = ITRACK[ant-1] & (SIDERIAL | TRANSMITTER) ;
	    if(COROPT != 1) 
	    {
	      if(which == SIDERIAL) setup_siderial(ant,refraction,arefraction) ;
	      if(which == TRANSMITTER) setup_transmitter(ant) ;
	    }
	    if((which==SIDERIAL)||(which==TRANSMITTER)) ants += abit ;
	    abit <<= 1 ;
	  }
	  if( BADDELAY ) wait_ticks_(&one) ; /* let hatlogger run */
	 }
	 while (BADDELAY && --limit ) ;
	 if (T == NULL) {printf("DELAY: no antennas selected\n"); exit(1);}

/*
 *	Setup the total power measurement 
 */
	 computi_("NTPOWER",&zero,&one,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}
	 for(i=0;i<nants;++i) { 
	  TPOWER[i]=0.; TPOWERA[i]=0.; TPOWERB[i]=0.; T300K[i]=0.; T400K[i]=0.;}
	 computr_("TPOWER",TPOWER,&nants,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}
	 computr_("TPOWERA",TPOWERA,&nants,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}
	 computr_("TPOWERB",TPOWERB,&nants,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}
	 computr_("T300K",T300K,&nants,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}
	 computr_("T400K",T400K,&nants,error) ;
	  if (strncmp(error,"OK",2) != 0) {printf("DELAY: %s\n",error);exit(1);}
	 add_to_table(POW,RESET+16,RESET,nants,ants,dummy,dummy,32.,dummy) ;
/*
 *	Setup stuff to happen at the end of integration (approx)
 */
	 add_to_table(END,RESET+(int)(INTTIME*100.),RESET,
					nants,ants,dummy,dummy,100000.,dummy) ;

/*	 TT = T ;     --this is a debug piece of code--
	 while (TT != NULL) 
	 {
		printf("%d %d ||  %d\n",TT->ant,TT->teladd,TT->ticks-RESET);
		TT = TT->next ;
	 }   */
	 computd_("TAU",TAU,&nants,error) ;
	 computd_("TAUDOT",TAUDOT,&nants,error) ;
	 computd_("TAUDD",TAUDOTDOT,&nants,error) ;
	 computd_("DELAY",DELAY,&nants,error) ;
	 computd_("FOF1",FOF1,&nants,error) ;
	 computd_("FOF2",FOF2,&nants,error) ;
	 computi_("DWIRES",DBITS,&nants,error) ;

/*
 *	Diagnostic printouts 
 */
	 if(DYDIAG >0)
	 {
	   if((DYDIAG ==1) ||(DYDIAG==2))
	   {
	     printf("UT:    %lf hours\n",UT/M_PI*12.) ;
	     printf
	     ("ANT:    1          3          4          5          6          7\n") ;
	   }
	   if(DYDIAG == 2)
	   {
	     printf("TAU:   %10f %10f %10f %10f %10f %10f\n",
		TAU[0],TAU[2],TAU[3],TAU[4],TAU[5],TAU[6]) ;
	     printf("TAUDOT: %10f %10f %10f %10f %10f %10f\n",
		TAUDOT[0],TAUDOT[2],TAUDOT[3],TAUDOT[4],TAUDOT[5],TAUDOT[6]) ;
	   }
	   if((DYDIAG ==1) ||(DYDIAG == 2))
	   {
	     printf("DELAY: %10f %10f %10f %10f %10f %10f\n",
		DELAY[0],DELAY[2],DELAY[3],DELAY[4],DELAY[5],DELAY[6]) ;
	     printf("DBITS: %10d %10d %10d %10d %10d %10d\n",
		DBITS[0],DBITS[2],DBITS[3],DBITS[4],DBITS[5],DBITS[6]) ;
	     printf("FOF1:  %10f %10f %10f %10f %10f %10f\n",
		FOF1[0],FOF1[2],FOF1[3],FOF1[4],FOF1[5],FOF1[6]) ;
	     printf("FOF2:  %10f %10f %10f %10f %10f %10f\n",
		FOF2[0],FOF2[2],FOF2[3],FOF2[4],FOF2[5],FOF2[6]) ;
	   }
	   if(DYDIAG > 7) report(RESET) ;
	 }
/*
 *	Short circuit the reset if coropt==1 (autocorrelation) 
 */
	 if(COROPT != 1)
	 {

/* --	wait until 5 ticks before reset time, then arm reset  */
	   get_times_(&ticksnow,&one,&mon,&day,&year,&UT,&LST,error) ;
	   ticksleft = RESET - ticksnow -5 ;
	   if(ticksleft <= 0)
	   {
		 BADRESET = 1 ;
		 printf("DELAY: overshot reset by %d ticks\n",-ticksleft) ;
	   }
	   if(ticksleft > 0)  wait_ticks_(&ticksleft) ;
	   for (ant=1; ant<=nants; ant++) 
	     if(ITRACK[ant-1] & SIDorTRANS) tpoke_(&ant,&TRESET,&one,&resetbit,error) ;

	   get_times_(&ticksnow,&one,&mon,&day,&year,&UT,&LST,error) ;
/* --	reset MUST be armet by 2 ticks before the time   */
	   if(ticksnow > RESET-3)
	   {
		BADRESET = 1 ; 
		sprintf(error,"DELAY: missed reset by %d ticks",
			ticksnow-(RESET-2)) ;
		logit_(error) ;
		printf("%s\n",error) ;
	   }
/* --	check for bits set   */
	   for (ant=1; ant<=nants; ant++) 
	     if(ITRACK[ant-1] & SIDorTRANS) 
	     {
		inbit = tpeek_(&ant,&treset_back,&one,error) ;
		if( (inbit&resetbit) != resetbit ) 
		{
		  BADRESET = 1 ;
		  sprintf(error,"DELAY: reset bit missing for ant %d",ant) ;
		  logit_(error) ;
		  printf("%s\n",error) ;
	        }
	     }
/* --	wait until after the RESET tick, then turn off the reset  */
	   ticksleft = RESET+3 - ticksnow ;
	   if (ticksleft > 0) wait_ticks_(&ticksleft) ;
	   for (ant=1; ant<=nants; ant++) 
	     if(ITRACK[ant-1] & SIDorTRANS) tpoke_(&ant,&TRESET,&one,&resetoff,error) ;
	 }
	}
	while (BADRESET) ;

/* --	all that's left to to is run the stepper subroutine until killed   */
	andsoforth() ;
}
/*-----------------------------------------------------*/
/* --	setup the stepping table for a siderial source */

void setup_siderial(int ant,double refraction,double arefraction)
{ 
  double taudot3,value1,value2 ;
  double step1,step2,tstep1,tstep2,dtstep1 ;
  int firstepd, firstep1, firstep2, ivdnext ;
  double turns, persec,tstepd,pturns1,pturns2, elevrad ;
  int phasefof1, phasefof2, ivalued, ivalue1, ivalue2 ;
  int one = 1, two = 2, three = 3, zero = 0, i ;
  int vdlo,vdhi ;
  char error[80] ;
  float seconbit, secleft ;

	i = ant-1 ;
	elevrad = ELEVTRUE[i]/57.295779513 ;
	TAU[i] = -(ANTPOS[0][i]*COSD*COSH - ANTPOS[1][i]*COSD*SINH 
		+ ANTPOS[2][i]*SIND) + ANTMISS[i]*cos(elevrad) ;

/*    (the first correction is for differential refraction; the
        second corection is for antennas at different heights) 
       (the calculations come from  Thompson,moran,swenson as
        interpreted by Welch) */
        if(ELEV[i] != 0.) 
	{
          TAU[i] += TAU[i] * refraction/(pow(sin(elevrad),2.)) ;
          TAU[i] -= arefraction/sin(elevrad) *
                  (ANTPOS[0][i] * COSLAT[i] + ANTPOS[2][i] * SINLAT[i]) ;
        }

	TAUDOT[i] = -(HDOT*(-ANTPOS[0][i]*COSD*SINH -ANTPOS[1][i]*COSD*COSH)
		+DDOT*(-ANTPOS[0][i]*SIND*COSH +ANTPOS[1][i]*SIND*SINH 
			+ANTPOS[2][i]*COSD));
	TAUDOTDOT[i] = -(HDOT*HDOT*(-ANTPOS[0][i]*COSD*COSH 
		+ANTPOS[1][i]*COSD*SINH)) ;
	taudot3 = -(HDOT*HDOT*HDOT*(ANTPOS[0][i]*COSD*SINH 
		+ANTPOS[1][i]*COSD*COSH)) ;
/*	DELAY[i] = DELAY0[i] -CABLE[i] -TAU[i] ;*/
	DELAY[i] = DELAY0[i] -TAU[i] ;
	if(strncmp(BAND,"D",1) == 0) DELAY[i] +=DELAY0D[i] ;
	check_delay_limits(i,DELAY[i]) ;
	 if(BADDELAY) return ;

	FOF1[i] = -LO1BT * TAUDOT[i] + 31.25 ;
	FOF2[i] = -LO2 * TAUDOT[i] + 3.125 ;
	ivalued = delaybits(ant,DELAY[i],TAUDOT[i]) ;
	DBITS[i] = ivalued ;
	value1 = (FOF1[i]*FOFTOBITS) ;
	ivalue1 = (int)value1 ;
	value2 = (FOF2[i]*FOFTOBITS) ;
	ivalue2 = (int)value2 ;
	step1 = (TAUDOTDOT[i]<0.) ? 1. : -1. ;
	step2 = step1 ;
	tstep1 = (fabs(TAUDOTDOT[i]) < 1.e-10 ) ? 
		1.e8 : 100./(FOFTOBITS*LO1BT*fabs(TAUDOTDOT[i])) ;
	if (tstep1 > 1.e8) tstep1 = 1.e8 ;
	dtstep1= (fabs(TAUDOTDOT[i]) < 1.e-10) ? 
		0. :tstep1*taudot3/(FOFTOBITS*LO1BT*TAUDOTDOT[i]*TAUDOTDOT[i]) ;
	tstep2 = tstep1 * LO1BT / LO2 ;
	firstepd = RESET -16 + delaysteptime(ant,DELAY[i],ivalued,&ivdnext,TAUDOT[i]) ;
	if (firstepd < RESET) 
	{
	  ivalued = ivdnext ;
	  firstepd += delaysteptime(ant,DELAY[i],ivalued,&ivdnext,TAUDOT[i]) ;
	}
	/* moved here sep 96  */
	seconbit = (float)(firstepd-RESET)/100. ;
	secleft = (INTTIME > seconbit) ? seconbit : INTTIME ;
	timeonbit(ant,ivalued,secleft) ;
	firstep1 = (fabs(TAUDOTDOT[i]) < 1.e-10) ?
	  RESET+100000 : RESET + (int)(tstep1 * half_frac(value1,-TAUDOTDOT[i])) ;
	firstep2 = (fabs(TAUDOTDOT[i]) < 1.e-10) ?
	  RESET+100000 : RESET + (int)(tstep2 * half_frac(value2,-TAUDOTDOT[i])) ;
/*	pturns1 = modf(-LO1BT*TAU[i]+PHASELO1[i]+PHASEM1[i],&turns) ;*/
	pturns1 = modf(-LO1BT*TAU[i]+PHASELO1[i],&turns) ;
	phasefof1 = ((int)(4096.* pturns1)) << 4;
/*	pturns2 = modf(-LO2*TAU[i]+PHASELO2[i]+PHASEM2[i],&turns) ;*/
	pturns2 = modf(-LO2*TAU[i]+PHASELO2[i],&turns) ;
	phasefof2 = ((int)(4096.* pturns2)) << 4 ;
	DELAY[i] -= TAUDOT[i]*(firstepd-RESET)/100. ;
	add_to_table
	   (DLY,firstepd,RESET,i+1,0,DELAY[i],(float)ivdnext,
		TAUDOT[i],TAUDOTDOT[i]);
	add_to_table(FOF,firstep1,RESET,i+1,TFOF1,
		value1+step1*(firstep1-RESET)/tstep1,step1,tstep1,dtstep1);
	add_to_table(FOF,firstep2,RESET,i+1,TFOF2,
		value2+step2*(firstep2-RESET)/tstep2,step2,tstep2,0.);
	  if(ant == 3) tpoke_(&ant,&TLEV1,&two,&zero,error);
	  else    tpoke_(&ant,&TLEV2,&one,&zero,error) ;
	  set_delay(ant,ivalued) ;
	  tpoke_(&ant,&TFOF1,&three,&ivalue1, error) ;
	  tpoke_(&ant,&TFOF2,&three,&ivalue2, error) ;
	  tpoke_(&ant,&TPFOF1,&two,&phasefof1,error) ;
	  tpoke_(&ant,&TPFOF2,&two,&phasefof2,error) ;
	  tpoke_(&ant,&TWALSH1,&one,&WALANTS[0][i],error) ;
	  tpoke_(&ant,&TWALSH2,&one,&WALANTS[1][i],error) ;
	tstepd = 1./(33.*fabs(TAUDOT[i])) ;
}
/*
 *	Setup the stepping table for the remote transmitter
 */

void setup_transmitter(int ant)
{
  double turns, elev ;
  int  phasefof1, phasefof2 ;
  int  ivalued,  ivalue1 = 0, ivalue2 = 0 ;
  int one = 1, two = 2, three = 3, zero = 0, i ;
  int vdlo, vdhi ;
  char error[80] ;

	i = ant-1 ;
	TAU[i] = -.97 * ANTPOS[1][i] + ANTMISS[i]*cos(ELEVTRUE[i]/57.29578) ;
/*	DELAY[i] = DELAY0[i] -CABLE[i] -TAU[i] ;*/
	DELAY[i] = DELAY0[i]  -TAU[i] ;
	if(strncmp(BAND,"D",1) == 0) DELAY[i] +=DELAY0D[i] ;
	ivalued = delaybits(ant,DELAY[i],1.) ;
	DBITS[i] = ivalued ;
	timeonbit(ant,ivalued,INTTIME) ;
	TAUDOT[i] = 0. ;
	TAUDOTDOT[i] = 0. ;
	FOF1[i] = 45. ;
	FOF2[i] = 5. ;
/*	phasefof1 = ((int)(4096. * modf(PHASELO1[i]+PHASEM1[i],&turns)))<<4 ;*/
	phasefof1 = ((int)(4096. * modf(PHASELO1[i],&turns)))<<4 ;
/*	phasefof2 = ((int)(4096. * modf(PHASELO2[i]-PHASEM2[i],&turns)))<<4 ;*/
	phasefof2 = ((int)(4096. * modf(PHASELO2[i],&turns)))<<4 ;
	add_to_table(DLY,RESET+100000,RESET,i+1,0,DELAY[i],ivalued,0.,0.) ;
	add_to_table(FOF,RESET+100000,RESET,i+1,TFOF1,0.,1.,1000000.,0.);
	add_to_table(FOF,RESET+100000,RESET,i+1,TFOF2,0.,1.,1000000.,0.);
	  if(ant == 3) tpoke_(&ant,&TLEV1,&two,&zero,error);
	  else    tpoke_(&ant,&TLEV2,&one,&zero,error) ;
	  set_delay(ant,ivalued) ;

	  tpoke_(&ant,&TFOF1,&three,&ivalue1, error) ;
	  tpoke_(&ant,&TFOF2,&three,&ivalue2, error) ;
	  tpoke_(&ant,&TPFOF1,&two,&phasefof1,error) ;
	  tpoke_(&ant,&TPFOF2,&two,&phasefof2,error) ;
	  tpoke_(&ant,&TWALSH1,&one,&WALANTS[0][i],error) ;
	  tpoke_(&ant,&TWALSH2,&one,&WALANTS[1][i],error) ;
}
/*
 *	Step the registers at the right time
 *	(also keep track of the time each of the longest 6 delay wires
 *	 was in and write this to common each 100 ticks.)	   
 */
void andsoforth()
{
  ASTEP *juststepped, *previous, *t ;
  char error[80] ;
  int intval,time = 0, ticks, nextval, ant, ant1 ;
  int ticksonly = 0,mon,day,year, raw ;
  double dummy,dummy2,seconds ;
  int d228 = 228, three = 3, one = 1, nants = NANTS, nants8 = nants*8 ;
  char who[6], units[4], name[16] ;
  float secsremain ;
  int d1106=1106, d1094=1094, d1107=1107, d1108=1108, d1064=1064, d1066=1066 ;
/*
 *	Endless loop until killed by observing program  
 */	
	while ((time-RESET)<40000)
	{
	  get_times_(&time,&ticksonly,&mon,&day,&year,&dummy,&dummy2,error) ;

/*	T is a pointer to the first item in the stepping table
 * 	which is a linked list of stepping events.  The processing
 *	required is to check the first item to see if it's time
 *	has come (or past) and then perform the calulation and step
 *	that is signifies.  This top item is then shuffled down in
 *	the table to a new position for it's new step time and the
 *	new top item checked.  When we find an item whose time has
 *	not come, we set up a wait until that time and sleep.
 */

	  while (T->ticks <= time)
	  {
	     if((DYDIAG == 9)&&(T->imfoff != POW)) report(time) ;
/*
 *	Check for timing error 
 */
	     if((T->ticks<(time-4))&&(T->imfoff != POW))
	     {
		  if(T->teladd == TFOF1 ) strcpy(who,"FOFF1") ;
		  else if(T->teladd == TFOF2 ) strcpy(who,"FOFF2") ;
	  	  else  strcpy(who,"DELAY") ;
		  printf("Missed %d %s step by %d ticks at %d\n",
		     T->ant,who,time- T->ticks,time-RESET) ;
	     }
/*
 *	Foff register stepping here   (these are stepped 1 unit each time)
 */
	     if(T->imfoff == FOF)
	     {
		intval = (int)(T->current + .5) ;
		tpoke_(&(T->ant),&(T->teladd),&three,&intval,error) ;
		T->current += T->stepsize ;
		T->lasticks = T->ticks ;
		T->ticks += T->dtime ;
		T->dtime += T->ddtime ;
	     }

/*
 *	Delay register stepping  (variable stepsize accomodated) 
 */
	     else if(T->imfoff == DLY)
	     {
	 	intval = ((int)(T->stepsize)) & 0x3ffff ;
		set_delay(T->ant,intval) ;

		T->lasticks = T->ticks ;
		T->ticks += delaysteptime
			(T->ant,T->current,intval,&nextval,T->dtime);
/*	Kludge to prevent endless looping   */
		if(T->ticks <= T->lasticks) T->ticks = T->lasticks +1 ;
		T->stepsize = (float)nextval ;
		seconds = (T->ticks - T->lasticks) / 100. ;
		T->current -= T->dtime * seconds ;
		T->dtime += T->ddtime * seconds ;
		/* truncate seconds to end of integration and
			don't even add bits in if beyond end of int  */
		secsremain = INTTIME - (float)(time-RESET)/100. ;
		seconds = (secsremain > seconds) ? seconds : secsremain ;
		if (secsremain > 0.) timeonbit(T->ant,intval,seconds) ;
	     }
/*
 *	Power measurement here (T->ant = nants, T->teladd = ants) 
 */
	     else if (T->imfoff == POW) {
		NTPOWER++ ;
		for(ant=1;ant<=T->ant;ant++)
	        {
		  if(inuse(ant,T->teladd)!=0)
		  {
		    ant1 = ant-1 ;
		    if(ant == 3) 
		     TPOWER[ant1] += atod_(&ant,&d1094,&raw,units,name,error) ;
		    else 
		     TPOWER[ant1] += atod_(&ant,&d1106,&raw,units,name,error) ;
		     if (strncmp(error,"OK",2) != 0) printf("DELAY: %s\n",error) ;
		    if(ant == 6) {
		     TPOWERA[ant1] += atod_(&ant,&d1107,&raw,units,name,error) ;
		     TPOWERB[ant1] += atod_(&ant,&d1108,&raw,units,name,error) ;
		     T300K[ant1] += atod_(&ant,&d1064,&raw,units,name,error) ;
		     T400K[ant1] += atod_(&ant,&d1066,&raw,units,name,error) ;
		    }
		  }
		}
		computi_("NTPOWER",&NTPOWER,&one,error) ;
		computr_("TPOWER",TPOWER,&nants,error) ;
		computr_("TPOWERA",TPOWERA,&nants,error) ;
		computr_("TPOWERB",TPOWERB,&nants,error) ;
		computr_("T300K",T300K,&nants,error) ;
		computr_("T400K",T400K,&nants,error) ;
		T->lasticks = T->ticks ;
		T->ticks += T->dtime ;
	     }
	     else if (T->imfoff == END) {
		computr_("DELAYTIM",DELAYTIM[0],&d228,error) ;
		T->lasticks = T->ticks ;
		T->ticks += T->dtime ;
	     }

/*
 *	Shuffle record to a new place based on its new time  
 */
	     juststepped = T ;
	     previous = juststepped ;
	     t = juststepped->next ;
/*
 *	Should justepped remain the first record? 
 */
	   if(juststepped->ticks > t->ticks)
	   {
/*
 *	Look for record with time greater than juststepped  
 */
	     while ((t->next != NULL) && (t->ticks < juststepped->ticks))
	     {
		previous = t ;
		t = t->next ;
	     }
/*
 *	If we get to last record, put juststepped after it  
 */
	     if (t->ticks < juststepped->ticks)
	     {
		t->next = juststepped ;
		T = juststepped->next ;
		juststepped->next = NULL ;
	     }
/*
 *	if not after last record, put juststepped before current record 
 */
	     else
	     {
		previous->next = juststepped ;
		T = juststepped->next ;
		juststepped->next = t ;
	     }
	   }
	  }
/*
 *	Now calculate how many ticks until the top item needs stepping  
 */
	  get_times_(&time,&ticksonly,&mon,&day,&year,&dummy,&dummy2,error) ;
	  ticks = T->ticks - time ;
	  if(ticks > 0) wait_ticks_(&ticks) ; 
	}
}
/*
 *	REPORT: printout the state of the stepping database
 *		note: report makes the next step 3 ticks late. 
 */
void report(int time)
{
  ASTEP  *t ;
  char who[6] ;

	printf("State of the STEP table at TIME= %d\n",time-RESET) ;
	printf("TICKS   ANT    WHO     CURRENT   STEPSIZE   dTIME\n") ;
	t = T ;
	for (t=T;t!=0;t=t->next)
	{
	  if(t->imfoff != POW) 
	  {
	   if(t->teladd == TFOF1 ) strcpy(who,"FOFF1") ;
	   else if(t->teladd == TFOF2 ) strcpy(who,"FOFF2") ;
	   else  strcpy(who,"DELAY") ;
	   printf("%d   %d   %s   %lf   %lf  %lf\n",
		t->ticks-RESET,t->ant,who,t->current,t->stepsize,t->dtime) ;	  
	  }
	}
	printf("---------------------------------------------\n") ;
}
/*
 *	Add values to the stepping table			   
 */

void add_to_table(int imfoff,int nextstep,int reset,int ant,int reg,double current,
	double stepsize,double steptime,double dd)
{
  ASTEP *new, *temp, *previous ;

	new = (ASTEP *)malloc(sizeof(ASTEP)) ;
	new->imfoff = imfoff ;
	new->ticks = nextstep ;
	new->reset = reset ;
	new->lasticks = reset ;
	new->ant = ant ;
	new->teladd = reg ;
	new->current = current ;
	new->stepsize = stepsize ;
	new->dtime = steptime ;
	new->ddtime = dd ;
	new->next = NULL ;

/*
 *	No entries in table  
 */
	if (T == (ASTEP *)NULL) T = new ;

/*
 *	Candidate earlier than first entry  
 */
	else if (new->ticks < T->ticks)
	{
	  new->next = T ;
	  T = new ;
	}
	else
	{
	  temp = T ;
	  previous = T ;
	  while ((temp->next != (ASTEP *)NULL) && (temp->ticks <= new->ticks))
	  {
		previous = temp ;
		temp = temp->next ;
	  }

/*
 *	Candidate later than last entry  
 */
	  if (temp->next == (ASTEP *)NULL) temp->next = new ;
/*
 *	Candidate in middle of table  
 */
	  else	{ new->next = temp ; previous->next = new ; }

	}
}
/*
 *	Compute the fraction of a step until .5 is reached	   
 */

double half_frac(double val,double direc)
{
  double fr ;
	if (direc < 0.)  fr = (val-.5) - floor(val-.5) ;
	else		 fr = ceil(val+.5) -(val+.5) ;
	return fr ;
}
/*
 *	Test the delay value for limits 2 - 1998/7998 and adjust all delay0s
 *      so the out-of-bounds delay becomes 10 inside the limit 
 */

void check_delay_limits(int ant1, double delay)
{

  double dcor ;
  int i ;
  char error[80] ;
  int nants = NANTS ;

	dcor = 0. ;
	if(delay < 3.) dcor = delay - 7. ;
        if(delay > (MOSTDELAY[ant1]-3.)) dcor = delay-MOSTDELAY[ant1]+7. ;
	if(dcor != 0.)
	{
	  if((dcor>0. && LASTDELAY0FIX<0.) || (dcor<0. && LASTDELAY0FIX>0.))
	  {
	     sprintf(error,"Uncorrectable delay range for ant %d",ant1+1);
	     printf("%s\n",error) ;
	     logit_(error) ;
	  }
	  else
	  {	
	     sprintf(error,
		"Delay off the end, changing all DELAY0s by %f\n",dcor) ;
	     printf("%s\n",error) ;
	     logit_(error) ;
	     for (i=0; i<NANTS; i++) DELAY0[i] -= dcor ;
	     computr_("DELAY0",DELAY0,&nants,error) ;
	     BADDELAY = 1 ;
	     LASTDELAY0FIX = dcor ;
	  }
	}
}

/*
 *	Gather up common block parameters that we need
 */
void get_from_common(int nants)
{
  char error[80] ;
  int i,j,one = 1, nants3, twentyfour = 24 ;

	comgetr_("INTTIME",&INTTIME,&one,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetd_("ELEV",ELEV,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetd_("ELEVTRUE",ELEVTRUE,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetd_("ANTMISS",ANTMISS,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetr_("DELAY0",DELAY0,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
/*	comgetd_("CABLE",CABLE,&nants,error);
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetr_("PHASEM1",PHASEM1,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetr_("PHASEM2",PHASEM2,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}*/
	comgetd_("LO1",&LO1,&one,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgeta_("BAND",BAND,&one,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}

	if (strncmp(BAND,"D",1) == 0)
		LO1BT = LO1/3. ;
	else
		LO1BT = LO1 ;

	comgeti_("COROPT",&COROPT,&one,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
        comgetd_("COSLAT",COSLAT,&nants,error) ;
        if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ; exit(1) ;}
        comgetd_("SINLAT",SINLAT,&nants,error) ;
        if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ; exit(1) ;}
	comgetd_("LO2",&LO2,&one,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetr_("DELAY0D",DELAY0D,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetr_("PHASELO1",PHASELO1,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetr_("PHASELO2",PHASELO2,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgeti_("ITRACK",ITRACK,&nants,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	nants3 = 3 * nants ;
	comgetd_("ANTPOS",ANTPOS[0],&nants3,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgetd_("FREQ",&FREQ,&one,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgeti_("DYDIAG",&DYDIAG,&one,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	comgeti_("WALANTS",WALANTS[0],&twentyfour,error) ;
	if (strncmp(error,"OK",2) != 0) { printf("DELAY: %s\n",error) ;	exit(1) ;}
	for (j=0;j<=18;j++)
	  for (i=0;i<NANTS;i++)   DELAYTIM[i][j] = 0. ;
}


/*
 *	Table of wire lengths for the delay boxes (ns)	
 *	note: measured values for ants 2,8,9 added 8nov95 - jrf 
 *        fiber lengths as of 9feb96  by wjw
 */

/* this wirelist was used in Jan 96

static double WIRE[10][18] = {
  0.,0.,999.904,500.040, 250.015, 124.992, 62.506, 31.261, 15.618, 7.818, 
	3.902, 1.950, .976, .495, .237, .124, .059, .029,
  0.,0.,999.717,500.023, 250.026, 125.003, 62.515, 31.270, 15.619, 7.829, 
	3.906, 1.960,.996, .492, .257, .125, .060, .037,
  0.,0.,999.478,500.029, 250.016, 125.028, 62.513, 31.237, 15.628, 7.816, 
	3.900, 1.949,.971, .489, .241, .118, .070, .035,
  3999.189,1999.707,999.878,500.063, 250.021, 125.003, 62.509, 31.255, 15.626,7.817, 
	3.910, 1.946,.973, .486, .236, .127, .063, .035,
  3999.370,1999.552,999.858,499.907, 250.021, 125.021, 62.511, 31.251, 15.638,7.819,
	3.897, 1.949,.973, .485, .237, .123, .062, .032,
  3998.221,1998.551,999.868,500.032, 250.028, 125.017, 62.507, 31.263, 15.637,7.817, 
	3.903, 1.945,.978, .493, .242, .120, .061, .036,
  3997.294,1999.768,999.881,500.022, 250.005, 125.004, 62.505, 31.250, 15.625,7.818, 
	3.902, 1.951,.969, .480, .236, .124, .061, .034,
  3999.003,1999.400,999.812,500.000, 250.019, 125.021, 62.522, 31.258, 15.628,7.809, 
	3.906, 1.951,.981, .493, .242, .118, .065, .040,
  0.,0.,999.971,188.030, 453.890, 226.981, 113.501, 56.758, 28.364, 14.179, 
	7.093, 3.542, 1.731, 0.884, 0.448, 0.224, 0.112, 0.060,
  0., 0., 0., 500.049, 250.038, 125.003, 62.512, 31.252, 15.605, 7.828, 
	3.901, 1.937,.974, .480, .249, .127, .064, .031
} ;

 * this wirelist is valid beginning 09Nov96(ant 10: 7apr97): 

static double WIRE[10][18] = {
  3999.003, 1999.400, 999.812, 500.040, 250.015, 124.992, 62.506, 31.261, 15.618, 7.818, 
	3.902, 1.950, .976, .495, .237, .124, .059, .029,
  0., 0., 999.717, 500.023, 250.026, 125.003, 62.515, 31.270, 15.619, 7.829, 
	3.906, 1.960,.996, .492, .257, .125, .060, .037,
  0., 0., 999.478, 500.029, 250.016, 125.028, 62.513, 31.237, 15.628, 7.816, 
	3.900, 1.949,.971, .489, .241, .118, .070, .035,
  3999.189, 1999.707, 999.878, 500.063, 250.021, 125.003, 62.509, 31.255, 15.626,7.817, 
	3.910, 1.946,.973, .486, .236, .127, .063, .035,
  3999.370, 1999.552, 999.858, 499.907, 250.021, 125.021, 62.511, 31.251, 15.638,7.819,
	3.897, 1.949,.973, .485, .237, .123, .062, .032,
  0., 0., 999.904, 500.032, 250.028, 125.017, 62.507, 31.263, 15.637,7.817, 
	3.903, 1.945,.978, .493, .242, .120, .061, .036,
  3997.294, 1999.768, 999.881, 500.022, 250.005, 125.004, 62.505, 31.250, 15.625,7.818, 
	3.902, 1.951,.969, .480, .236, .124, .061, .034,
  3998.221, 1998.551, 999.868, 500.000, 250.019, 125.021, 62.522, 31.258, 15.628,7.809, 
	3.906, 1.951,.981, .493, .242, .118, .065, .040,
  0., 0., 999.971, 188.030, 453.890, 226.981, 113.501, 56.758, 28.364, 14.179, 
	7.093, 3.542, 1.731, 0.884, 0.448, 0.224, 0.112, 0.060,
  0., 0., 0., 500.049, 250.038, 125.003, 62.512, 31.252, 15.605, 7.828, 
	3.901, 1.937,.974, .480, .249, .127, .064, .031
} ;

 * this wirelist is valid beginning 18jan98 (must keep track of stns/ants ): 
 * 1-10 -> 500e,475w,580n,4320n,2220e,1740n,1500e,3270n,740w,2700n

static double WIRE[10][18] = {
  3998.204, 1998.535, 999.857, 500.040, 250.015, 124.992, 62.506, 31.261, 15.618, 7.818, 
	3.902, 1.950, .976, .495, .237, .124, .059, .029,
  3997.268, 1999.747, 999.861, 500.023, 250.026, 125.003, 62.515, 31.270, 15.619, 7.829, 
	3.906, 1.960,.996, .492, .257, .125, .060, .037,
  3998.952, 1999.366, 999.786, 500.029, 250.016, 125.028, 62.513, 31.237, 15.628, 7.816, 
	3.900, 1.949,.971, .489, .241, .118, .070, .035,
  3999.306, 1999.511, 999.826, 500.063, 250.021, 125.003, 62.509, 31.255, 15.626,7.817, 
	3.910, 1.946,.973, .486, .236, .127, .063, .035,
  3999.881, 1999.939, 999.963, 499.907, 250.021, 125.021, 62.511, 31.251, 15.638,7.819,
	3.897, 1.949,.973, .485, .237, .123, .062, .032,
  0., 1999.005, 999.948, 500.032, 250.028, 125.017, 62.507, 31.263, 15.637,7.817, 
	3.903, 1.945,.978, .493, .242, .120, .061, .036,
  3999.162, 1999.680, 999.854, 500.022, 250.005, 125.004, 62.505, 31.250, 15.625,7.818, 
	3.902, 1.951,.969, .480, .236, .124, .061, .034,
  3999.356, 1999.867, 999.901, 500.000, 250.019, 125.021, 62.522, 31.258, 15.628,7.809, 
	3.906, 1.951,.981, .493, .242, .118, .065, .040,
  0., 1999.843, 999.456, 188.030, 453.890, 226.981, 113.501, 56.758, 28.364, 14.179, 
	7.093, 3.542, 1.731, 0.884, 0.448, 0.224, 0.112, 0.060,
  0., 1999.875, 999.879, 500.049, 250.038, 125.003, 62.512, 31.252, 15.605, 7.828, 
	3.901, 1.937,.974, .480, .249, .127, .064, .031
} ;
 * this wirelist is valid beginning 11dec98 (must keep track of stns/ants ): 
 * 1-10 -> 580n,475w,475e,4320n,1500e,740w,2700n,3270n,1740n,2220e	*/

static double WIRE[10][18] = {
  3998.204, 1998.535, 999.857, 500.040, 250.015, 124.992, 62.506, 31.261, 15.618, 7.818, 
	3.902, 1.950, .976, .495, .237, .124, .059, .029,
  3997.268, 1999.747, 999.861, 500.023, 250.026, 125.003, 62.515, 31.270, 15.619, 7.829, 
	3.906, 1.960,.996, .492, .257, .125, .060, .037,
  3998.952, 1999.366, 999.786, 500.029, 250.016, 125.028, 62.513, 31.237, 15.628, 7.816, 
	3.900, 1.949,.971, .489, .241, .118, .070, .035,
  3999.306, 1999.511, 999.826, 500.063, 250.021, 125.003, 62.509, 31.255, 15.626,7.817, 
	3.910, 1.946,.973, .486, .236, .127, .063, .035,
  3999.162, 1999.680, 999.854, 499.907, 250.021, 125.021, 62.511, 31.251, 15.638,7.819,
	3.897, 1.949,.973, .485, .237, .123, .062, .032,
  0., 1999.843, 999.456,  500.032, 250.028, 125.017, 62.507, 31.263, 15.637,7.817, 
	3.903, 1.945,.978, .493, .242, .120, .061, .036,
  0., 1999.875, 999.879, 500.022, 250.005, 125.004, 62.505, 31.250, 15.625,7.818, 
	3.902, 1.951,.969, .480, .236, .124, .061, .034,
  3999.356, 1999.867, 999.901, 500.000, 250.019, 125.021, 62.522, 31.258, 15.628,7.809, 
	3.906, 1.951,.981, .493, .242, .118, .065, .040,
  0., 1999.005, 999.948, 188.030, 453.890, 226.981, 113.501, 56.758, 28.364, 14.179, 
	7.093, 3.542, 1.731, 0.884, 0.448, 0.224, 0.112, 0.060,
  3999.881, 1999.939, 999.963, 500.049, 250.038, 125.003, 62.512, 31.252, 15.605, 7.828, 
	3.901, 1.937,.974, .480, .249, .127, .064, .031
} ;


/** DELAYBITS	- convert a delay(ns) to bits				*/
/*: basic								*/
/*+
	int DELAYBITS(ANT,DELAY,TAUDOT)

  DELAYBITS:	Find the bits necessary to represent a given delay
		value on an antenna's delay box by successive 
		approximation. (Uses the global table of cable lengths).
		Adds half of the smallest bit to the test delay to
 		achieve rounding.  (Caching the value of the
		partial result lowers the calculations per result from
		18 to 6.) (Note: the function may be double valued; we
		choose the bit representation with the most short bits
		turned on when ascending and with the longer bits on
		when decending.)

  ARGUMENTS:
  ANT (int,input)		Antenna number
  DELAY(double,input)		delay in ns.
  DELAYBITS(int,output)		equivalent bit pattern 
  TAUDOT(double,input)		used for direction of movement(delay moves
				 in the opposite direction to tau)
									*/
  static int bits13[10] = {0,0,0,0,0,0,0,0,0,0} ;
  static double wire13[10] = {0,0,0,0,0,0,0,0,0,0} ;
/*  These need changing for different wire setups:
	bigbit is the bit value of the longest wire
	18 - bigi   is the number of bits
	maskoff masks the output before sending to the interface   */
/*  static bigbit[10] = {0x20000,0x20000,0x20000,0x20000,0x20000,0x10000,0x20000,
                      0x20000,0x10000,0x10000} ;
  static bigi[10] = {0,0,0,0,0,1,0,0,1,1} ;
  static maskoff[10] = {0x3ffff,0x3ffff,0x3ffff,0x3ffff,0x3ffff,0x1ffff,0x3ffff,
                      0x3ffff,0x1ffff,0x1ffff} ; */

/* new bigbit values valid 13dec98 */
  static bigbit[10] = {0x20000,0x20000,0x20000,0x20000,0x20000,0x10000,0x10000,
                      0x20000,0x10000,0x20000} ;
  static bigi[10] = {0,0,0,0,0,1,1,0,1,0} ;
  static maskoff[10] = {0x3ffff,0x3ffff,0x3ffff,0x3ffff,0x3ffff,0x1ffff,0x1ffff,
                      0x3ffff,0x1ffff,0x3ffff} ;

int delaybits(int ant,double delay,double taudot)
{
  double testdelay ;
  int bits, i, testbit, ant1, istart ;
  double delayplus ;

	ant1 = ant-1 ;
	delayplus = delay + WIRE[ant1][17] * .5 ;

/*
 *	Will the values cached in wire13/bits13 be useful?
 *	If Yes, start with bit 14 
 */
	if(delayplus>wire13[ant1] && delayplus<(wire13[ant1]+.45))
	{
		istart = 14 ;
		testdelay = wire13[ant1] ;
		bits = bits13[ant1] ;
		testbit = 0x8 ;
	}
/*
 *	No, so start with the biggest wire 
 */
	else
	{
		istart = bigi[ant1] ;
		testdelay = 0. ;
		bits = 0 ;
		testbit=bigbit[ant1] ;
	}
/*
 *	Add in each wire in turn until the answer matches the input
 *	to within eps ns.; build bit pattern answer in bits 
 */
	for(i=istart; i<18; i++,testbit >>=1)
	{
	  if(testdelay + WIRE[ant1][i] < delayplus)
	  {
	    testdelay += WIRE[ant1][i] ;
	    bits |= testbit ;
	  }
	  if (i == 13) {bits13[ant1] = bits; wire13[ant1]=testdelay;}
	}
/*	printf("%d <-- %f  (%d)\n",bits,delay,ant) ;*/
	return (bits & 0x3ffff) ;
}
/** BITSDELAY	- convert a bits to delay(ns)				*/
/*: basic								*/
/*+
	double BITSDELAY(ANT,BITS)

  BITSDELAY:	Find the delay resulting from a given bit 
		value on an antenna's delay box. (Uses the 
		global table of cable lengths).

  ARGUMENTS:
  ANT (int,input)		Antenna number
  BITS(int,input)		bit pattern
  BITSDELAY(double,output)	equivalent delay

									*/
/*--									*/
double bitsdelay(int ant,int bits)
{
  double testdelay ;
  int i, testbit, ant1,istart ;

	ant1 = ant-1 ;
	if((bits&0xfffffff0) == bits13[ant1])
	{
		istart = 14 ;
		testdelay = wire13[ant1] ;
		testbit = 0x8 ;
	}
	else
	{
		istart = bigi[ant1] ;
		testdelay = 0. ;
		testbit=bigbit[ant1] ;
	}
	for(i=istart; i<18; i++,testbit >>=1) 
	  if((testbit & bits) != 0)  testdelay += WIRE[ant1][i] ;

/*	printf("%d --> %f  (%d)\n",bits,testdelay,ant) ; */
	return testdelay ;
}
/** TIMEONBIT	- count up seconds that each bit was on			*/
/*: basic								*/
/*+
	void TIMEONBIT(ANT,BITS,SEC)

  TIMEONBIT:	Add SEC to DELAYTIM[ant-1][abit] where abit
		is the most signicant 18 bits of delay converted
		to an integer: bit 18 -> 1, etc., also add SEC
		to DELAYTIM[ant-1][0].

  ARGUMENTS:
  ANT (int,input)		Antenna number
  BITS(int,input)		bit pattern
  SEC(real,input)		Seconds for this step

									*/
/*--									*/
void timeonbit(int ant,int bits,float sec)
{
  int i, testbit[18] = 
    {0x20000,0x10000,0x8000,0x4000,0x2000,0x1000,
	0x800,0x400,0x200,0x100,0x80,0x40,0x20,0x10,0x8,0x4,0x2,0x1} ;
  int ant1 ;

	ant1 = ant-1 ;
	DELAYTIM[ant1][0] += sec ;
	for(i=0; i<18; i++) 
	  if(testbit[i] & bits)  DELAYTIM[ant1][i+1] += sec ;

	return ;
}


/** DELAYSTEPTIME	- calc the secs until the next delay step       */
/*: basic								*/
/*+
	int DELAYSTEPTIME(ANT,DELAY,BITS,BITSAFTER,TAUDOT)

  DELAYSTEPTIME:	Calculate the seconds until the next delay step
			time.  Start with the current delay (and bits)
			and calculate the time until halfway to the delay
			represented by bits+1 or bits-1. (Uses the global 
			table of cable lengths). (If stepping the delay
			results in a delay change in the wrong direction
			(caused by a larger cable being shorter than the
			sum of all lesser cables), the bit-value is 
			stepped again).

  ARGUMENTS:
  ANT (int,input)		Antenna number
  DELAY(double,input)		current delay in ns.
  BITS(int,input)		current bit pattern
  BITSAFTER(int,output)		next bit pattern
  TAUDOT(double,input)		rate of change of tau
  DELAYSTEPTIME(int,output)	Ticks until delay should be stepped
									*/
/*--									*/
int delaysteptime(int ant,double delay,int bits,int *bitsafter,double taudot)
{
  double nextdelay, currdelay, seconds ;
  int nextbits, ticks ;

	if (fabs(taudot) < 1.e-6)
	{
	  *bitsafter = bits ;
	  return 9999. ;
	}
	currdelay = bitsdelay(ant,bits) ;
	nextbits = bits ;
	do 
	{
	  nextbits = (taudot<0.) ? nextbits+1 : nextbits-1 ;
	  nextdelay = bitsdelay(ant,nextbits) ;
	} while ( (nextdelay-currdelay)*taudot > 0.) ;
	seconds = fabs( ((nextdelay+currdelay)/2.-delay) /taudot) ;
	ticks = (int)(100.*seconds+.5) ;
	*bitsafter = nextbits ;
	return ticks ;
}
/*
 *	Calc_refract - calculate the correct refraction constant based
 *		on the weather instrument inputs  
 */ 
void calc_refract(double *refraction,double *arefraction)
{
  float pressmb, airtemp, relhumid ;
  int d1 = 1 ;
  double es, kelvin ;
  char error[80] ;
	comgetr_("PRESSMB",&pressmb,&d1,error) ;
	if(strncmp(error,"OK",2) != 0)  pressmb = 1000. ;
	if((pressmb> 1100.) || (pressmb< 700.)) pressmb = 1000. ;
	comgetr_("AIRTEMP",&airtemp,&d1,error) ;
	if(strncmp(error,"OK",2) != 0)  airtemp = 14. ;
	if((airtemp< -50.) || (airtemp> 50.)) airtemp = 14. ;
	kelvin = airtemp+273.2 ;
	comgetr_("RELHUMID",&relhumid,&d1,error) ;
	if(strncmp(error,"OK",2) != 0)  relhumid = 50. ;
	if(relhumid< 0.)  relhumid = 2. ;
	if(relhumid>100.) relhumid = 98. ;

	es = 6.105 * 
	     exp((25.22*(kelvin-273.2)/kelvin) - (5.31*log(kelvin/273.2))) ;
	es = es * (double)relhumid /100. ;

	*refraction = 1.e-6 / 6.28318530718 *
	  ((.53*(double)pressmb/kelvin) + (915. *es /(kelvin*kelvin))) ;
        *arefraction = 1.e-6  *
          ((77.6*(double)pressmb/kelvin) + (3.73e5 *es /(kelvin*kelvin))) ;
}
/*
 *	Output the delay value to the antenna and calculate the 
 *	Gain boost necessary because of the fiber switches and output
 */
void set_delay(int ant, int ivalued) 
{
  int vdlo,vdhi,i ;

	i = ant-1 ;
	vdlo = ivalued & 0x7fff ;
	vdhi = (ivalued & 0x38000) >> 15 ;
	poke_(&LODELAY[i],&vdlo) ;
#ifdef FIBER
	poke_(&HIDELAY[i],&vdhi) ;
#endif
}

/*
 *	Mimic the fortran select function  
 */
int inuse(int ant,int ants)
{
  if ((ants & 1<<ant) > 0)  return 1 ;
  else return 0 ;
}
