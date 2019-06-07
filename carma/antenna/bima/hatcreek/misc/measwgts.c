/* measwgts.c - measure relative weights of different antennas in
	phased-up VLBI port signal; uses HP DVM to measure a voltage,
	which may be output of detector diode or of HP power meter;
	turns off all IF's, then measures one at a time, normalizes
	result, and types out answer (redirect to file if you want */

#include "../inc/ugpib.h"
#include "../inc/hatlib.h"
#include <stdlib.h>
#include <stdio.h>
#define NANTS 10

static int devh = 0;
static char dvm[] = {"dev6"};
float getvolts();

int main(int argc, char *argv[])
{
	int ant,ants;
	char error[80];
	int one=1;
	int ten=100;
	int addr[10] ={0x6c,0x6c,0x5c,0x6c,0x6c,0x6c,0x6c,0x6c,0x6c,0x6c} ;
	int mask[10] ={0x20,0x20, 0x4,0x20,0x20,0x20,0x20,0x20,0x20,0x20} ;
	int off[10]  ={0, 0, 0, 0, 0, 0, 0, 0, 0, 0} ;
	int save[10];
	float p[10],ptot,wgt;
	float p0;	
	int peekaddr;

/*
 	if (argc < 2) {
		printf("Usage: measwgts ants\n");
		return 1;
	}

	pants_(argv[1],&ants,strlen(argv[1]));
	if (ants == 0) {
		printf("illegal value for ants\n") ;
		return 1;
    	}
*/

	ants = 2046;

/*	open the multibus so that the program can control IF on/off switches */

	mbusopen_(error); 
	if ((strcmp(error,"OK")) != '\0') {
		printf("%s\n",error);
		return 1;
	}

/* begin by measuring total power with all IFs turned off */

	for (ant=1; ant<=NANTS; ant++) {
		if (select_(&ant,&ants) != 0) {
			peekaddr = addr[ant-1]-256;
			save[ant-1] = tpeek_(&ant,&peekaddr,&one,error);
			setbits_(&ant,&addr[ant-1],&one,&mask[ant-1],&off[ant-1],error) ;
		}
	}
	wait_ticks_(&ten);
	p0 = getvolts();

/* now restore each IF in turn to its original state, meas pwr, turn off */

	ptot = 0.;
	for (ant=1; ant<=NANTS; ant++) {
		if (select_(&ant,&ants) != 0) {
			tpoke_(&ant,&addr[ant-1],&one,&save[ant-1],error);
			wait_ticks_(&ten);
			p[ant-1] = getvolts() - p0;
			setbits_(&ant,&addr[ant-1],&one,&mask[ant-1],&off[ant-1],error) ;
		}
		else {
			p[ant-1] = 0.;

		}
		ptot += p[ant-1];
	}

/* return all IF's to original states, type out normalized weights */

	printf(" 0   0.00   %9.5f\n",p0);

	for (ant=1; ant<=NANTS; ant++) {
		if (select_(&ant,&ants) != 0) {
			tpoke_(&ant,&addr[ant-1],&one,&save[ant-1],error);
		}
		wgt = p[ant-1]/ptot;
		if (wgt < 0.) {
			wgt = 0.000001;
		}
		printf(" %d   %6.4f  %9.5f\n",ant,wgt,p[ant-1]);
	}

	return 0;
}

/* subroutine getvolts reads out voltage from HP DVM over IEEE interface */

float getvolts()
{
	char buffer[16];
	if (devh == 0) {
		devh = ibfind(dvm);
		if (devh < 0) {
			printf("%s\n","channel open error");
			return 1;
		}
		ibclr(devh);
		ibllo(devh);
	}

	ibwrt(devh,"*CLS",4);
	ibwrt(devh,"MEAS:VOLT:DC?",13);
	ibrd(devh,buffer,16);
	return atof(buffer);
}

