/** PHASETRACK - realtime atmospheric phase correction */
/*: misc								*/
/*+
Uses total power detectors to correct for atmospheric phase
fluctuations.  Runs independently of observing and delay-stepping
programs.

*/
/*--	May 96  RP								*/
	
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "hatlib.h"
#include "telemadd.h"
#include <time.h>

#define NANTS 12

int main (int argc, char *argv[])
{

	float vfacum[NANTS],tpwr[NANTS],tpwr0[NANTS];
	float factor;
	float vfin;
	float phase;
	int iphase;
	float delayphase[NANTS];		/* LO1 phase offset sent out by delay program (degrees) */
	float tpwrtok[NANTS];			/* conversion factor from v/f volts to Kelvin */
	float ncycle[NANTS] = {6,6,6,6,6,6,6,6,6,6,6,6};
									/* compute new total pwr every 0.32 * ncycle secs */
	int nacum[NANTS];				/* number of vf readings currently summed in vfacum */
	int iphaseflag;
	int ilastphase[NANTS];			/* last phase to appear in the phase register */
	float atmophase[NANTS];			/* atmospheric phase correction in degrees */

	int mpx_tpwr[NANTS] = {1094, 1106, 1094, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106};
									/* using atod on ants 1 and 3 because there is no v/f */
	int zerodb,lastzerodb[NANTS];	
	int reset_reference;
	int x6b = 0x6b;
	int phase_read_addr = 0x45 - 256;
		/* must trick tpeek into reading outgoing register */
	int phase_write_addr = 0x45;
		/* offset phase for LO1 */
	double turns;
	int n32 = 32;

	int mon,day,year,ticks;
    double ut,lst,julday;
	int raw;
	char units[4],name[16],error[80];
	int ants,ant,nt,iapply;
	int twelve = 12;
	int zero = 0;
	int one = 1;
	int two = 2;

	if (argc != 4) {
		printf("Usage: phasetrack ants factor(degrees/K) apply(0\1)\n");
		return 1;
	}

	pants_(argv[1],&ants,strlen(argv[1]));
		/* get antenna flag */
	factor = atof(argv[2]);
	iapply = atoi(argv[3]);
/*
	if (iapply == 0) printf("\nwriting phase correction to common variable ATMOPHASE," \
		" but not actually applying it\n\n");
*/
/* open the multibus */

	mbusopen_(error);
	if (strcmp(error,"OK") != 0) {
		printf("MBUSOPEN:%s\n",error);
		return 1;
	}

	reset_reference = 1;
		/* will be set to zero at start of an integration */

	for (ant=1; ant<=NANTS; ant++) {
		nt = ant-1;
		vfacum[nt] = 0.;
		nacum[nt] = 0;
		tpwr[nt] = tpwr0[nt] = 0.;
		atmophase[nt] = 0.;
		ilastphase[nt] = 0xfff0 & tpeek_(&ant,&phase_read_addr,&two,error);
	}

/* begin big loop; exit if IPHASEFLAG = -1 */

	while (1) {
		
/* retrieve diagnostic level and setpoints from common */

		comgeti_("iphasetrack",&iphaseflag,&one,error);
			/* quit if -1, reset reference on next integration if +1, immediately if +2 */

		if (iphaseflag < 0) {
			for (nt=0; nt < NANTS; nt++) {
				atmophase[nt] = 0;
			}
			computr_("atmophase",atmophase,&twelve,error);
			computi_("iphasetrack",&zero,&one,error);
				/* rezero the phaseflag so that program can be restarted easily */
			return 0;
		}
		else if (iphaseflag > 0) {
			if (iphaseflag==1) reset_reference = 1;
			for (nt=0; nt < NANTS; nt++) {
				atmophase[nt] = 0;
				tpwr0[nt] = tpwr[nt];
			}
			computi_("iphasetrack",&zero,&one,error);
		}

		comgetr_("tpwrtok",tpwrtok,&twelve,error);
			/* conversion factor from total pwr volts to degrees Kelvin */

/* loop through antennas, add total pwrs to sum */

		for (ant=1; ant<NANTS; ant++) {
			if (select_(&ant,&ants)) {	
				nt = ant-1;		/* subscript for arrays, runs 0-11 */
				zerodb = tpeek_(&ant,&x6b,&two,error) & 0x10;
				vfin = atod_(&ant,&mpx_tpwr[nt],&raw,units,name,error);
/*
				if (zerodb && lastzerodb[nt]) {
					vfacum[nt] += vfin;
					nacum[nt]++;
				}
				lastzerodb[nt] = zerodb;
*/
				vfacum[nt] += vfin;
				nacum[nt]++;
				if (nacum[nt] >= ncycle[nt]) {
					tpwr[nt] = tpwrtok[nt] * vfacum[nt]/nacum[nt];
					vfacum[nt] = 0.;
					nacum[nt] = 0;
					if ((reset_reference) || (tpwr0[nt]==0.)) tpwr0[nt] = tpwr[nt];
						/* keep updating reference total pwr until reset flag is cleared */
					atmophase[nt] = factor * (tpwr[nt]-tpwr0[nt]);
/*
					printf("tpwr %6.4f, tpwr0 %6.4f, reset_ref %1d, atmophase %6.1f\n", \
						tpwr[nt],tpwr0[nt],reset_reference,atmophase[nt] );
*/
				}
			}
		}

		computr_("atmophase",atmophase,&twelve,error);
			/* store the current atmospheric phase corrections in common each time */

		for (ant=1; ant<NANTS; ant++) {
			if (select_(&ant,&ants)) {	
				nt = ant-1;
				iphase = 0xfff0 & tpeek_(&ant,&phase_read_addr,&two,error);
				if (iphase != ilastphase[nt]) {
					delayphase[nt] = 360.* (iphase >> 4)/4096.;
						/* if an unexpected value appears, it must have been put there
						   by delay subroutine, indicating start of new integration */
					reset_reference = 0;
						/* if reset_reference flag was set, clear it now; reference value
						   will be the total power measured just as this integration began */
					ilastphase[nt] = iphase;
				}
				phase = modf((double)((delayphase[nt]+atmophase[nt])/360.),&turns);
/*
				printf("iphase %d, delayphase %6.1f, final phase %6.1f\n", \
					iphase,delayphase[nt],360.*phase );
*/
				iphase = (int)(phase*4096.) << 4;
				if ((iapply == 1) && (iphase != ilastphase[nt])) {
					ilastphase[nt] = iphase;
					tpoke_(&ant,&phase_write_addr,&two,&iphase,error);
						/* overwrite the phase in the telem register; new phase will be
						   applied to LO1 within 0.01 sec */
				}
			}
		}

		wait_ticks_(&n32);
	}
}
