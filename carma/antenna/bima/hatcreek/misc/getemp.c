#include "../inc/ugpib.h"
#include <errno.h>
#include <stdlib.h>
#include "hatlib.h"
#define NBUF 16

static int devh = 0 ;
static char dvm[] = {"dev13"} ;

main () 

{
	char buffer[NBUF]; 
	char out[30];
	int i;
	int n600 = 600;
	int n200 = 200;
	float ohmsmin,ohmsmax;
	float temp,tempmin,tempmax;
	FILE *fp;

/*  --	open the channel if necessary    */
	if (devh == 0) {
	  devh = ibfind( dvm );
	  if( devh < 0 ) {
		printf("%s/n","channel open error");
		return 1;
	  } 	
	  ibclr( devh );
	  ibllo( devh );
	}

	while (1) {

	ibwrt(devh,"*CLS",4);
	ibwrt(devh,"CONF:FRES 1.e6",14);
	ibwrt(devh,"SYST:BEEP:STAT OFF",18);
	ibwrt(devh,"CALC:FUNC NULL",14);
	ibwrt(devh,"CALC:FUNC AVER",14);
	ibwrt(devh,"CALC:STAT ON",12);
	ibwrt(devh,"FRES:NPLC 1",11);
	ibwrt(devh,"TRIG:SOUR IMM",13);
	ibwrt(devh,"SAMP:COUN 120",13);
	ibwrt(devh,"INIT",4);
	wait_ticks_(&n600);
	ibwrt(devh,"CALC:AVER:MIN?",14);
	ibrd(devh,buffer,16);
	ohmsmin = atof(buffer);
	tempmin = t_ge25088_(&ohmsmin);
	
	ibwrt(devh,"CALC:AVER:MAX?",14);
	ibrd(devh,buffer,16);
	ohmsmax = atof(buffer);
	tempmax = t_ge25088_(&ohmsmax);
	temp = (tempmin + tempmax)/2.;
/*
	ibwrt(devh,"CALC:AVER:COUN?",15);
	ibrd(devh,buffer,16);
	printf("%s\n",buffer);
*/

	fp = fopen("getemp.dat","a");
	fprintf(fp,"%6.3f %6.3f  %6.3f\n",tempmin,tempmax,temp);
	fclose(fp);
 
	sprintf(out,"%s %6.3f %s","DISP:TEXT \"",temp,"K\"");
	ibwrt(devh,out,21);
	ibwrt(devh,"SYST:BEEP",9);
	}
	ibloc(devh);
	return 0;
}
