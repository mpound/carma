/* cablewatch.c  records cable lengths and elevations */
/* 01 jan 95 - read airtemp from common; record delay for ants 6,7,
	since cable lengths are measured through fiberoptic delay lines */
/* 23may97 - updated for 10 antennas.  jrf */

#include <stdio.h>
#include "hatlib.h"
#include "telemadd.h"
#include <time.h>
#define nants 12

/* typecast the various declarations */
double julian_(int *, int *, int *, double *);
float diode_(int *, int *, float *, char []);

int main (int argc, char *argv[])
{

	int mon,day,year,ticks,ant,raw,newmeas;
	float temp;
	int one = 1;
	int two = 2;
	int n1042 = 1042;
	int five = 5;
	int num = nants;
	int wait = 3000;
	double ut,lst,julday,elev[nants],cable[nants] ;
	double lastcable[nants] = {0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.};
	char units[4],name[16],error[80],filename[50];
	FILE *fp;


	strcpy(filename,getenv("OBS"));
	strcat(filename,"/cable") ;
	strcat(filename,"/cablelog.dat");

	mbusopen_(error);
	if (strcmp(error,"OK") != 0) {
		printf("MBUSOPEN:%s\n",error);
		return 1;
	}

/* begin endless loop */

	while (1) {

/* read cable lengths from common, determine if any have changed */

		comgetd_("cable",cable,&num,error);
		newmeas = 0;
		for (ant=0; ant<10; ant++) {
			if (fabs(cable[ant]-lastcable[ant]) > 0.00001) {
				newmeas = 1;
			}
			lastcable[ant] = cable[ant];
		}

/* read out elev, temp, time and write to file only if new measurement exists */

		if (newmeas == 1) {	
			get_times_(&ticks,&two,&mon,&day,&year,&ut,&lst,error);
			julday = julian_(&day,&mon,&year,&ut) - 2449000;
			comgetd_("elev",elev,&num,error);
			comgetr_("airtemp",&temp,&one,error);
/*
			temp= atod_(&five,&n1042,&raw,units,name,error);
*/					
			fp = fopen(filename,"a");
			fprintf(fp,"%.4f %6.2f %6.1f  %.4f %.4f %.4f %.4f %.4f %.4f %.4f %.4f %.4f %.4f\n", \
				julday,temp,elev[4],cable[0],cable[1],cable[2],cable[3],cable[4], \
				cable[5],cable[6],cable[7],cable[8],cable[9]);
					/* note: subscripts run from 0-9 for ants 1-10 */
			fclose(fp);
		}
	    wait_ticks_(&wait);
	}
}

