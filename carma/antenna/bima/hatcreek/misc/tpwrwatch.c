/* tpwrwatch.c - check up on Wilson's code */

#include <stdio.h>
#include "hatlib.h"
#include "telemadd.h"
#include <time.h>

int main (int argc, char *argv[])
{
	int mon,day,year,ticks0,ticks,ant;
	int one = 1;
	int two = 2;
	int num = 12;
	double ut,lst;
	char error[80],filename[50];
	FILE *fp;
	float tpower[12];
	int dumpcount,last,ntpower;

	strcpy(filename,"tpx.dat");
	fp = fopen(filename,"a");

	mbusopen_(error);
	if (strcmp(error,"OK") != 0) {
		printf("MBUSOPEN:%s\n",error);
		return 1;
	}

/* begin endless loop */

	dumpcount=0;
	last=10000;
	get_times_(&ticks0,&one,&mon,&day,&year,&ut,&lst,error);

	while (1) {

/* read cable lengths from common, determine if any have changed */

		comgeti_("ntpower",&ntpower,&one,error);
		if (ntpower != last) {
			comgetr_("tpower",tpower,&num,error);
			get_times_(&ticks,&one,&mon,&day,&year,&ut,&lst,error);
			fprintf(fp,"%5d %2d %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n", \
				(ticks-ticks0),ntpower,tpower[0],tpower[1],tpower[2],tpower[3],tpower[4], \
				tpower[5],tpower[6],tpower[7],tpower[8]);
			last = ntpower;
		}
		dumpcount++;
		dumpcount %= 20;
		if (dumpcount == 0) fflush(fp);
	    wait_ticks_(&one);
	}
}

