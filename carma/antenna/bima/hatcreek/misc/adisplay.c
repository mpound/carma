/* adisplay.c test program to display many address of the a/d card.
the program continuously displays n addresses from the atod, thus
allowing monitoring of the status of many different processes. the
n addresses have to be consecutive 
	
	niranjan thatte july 92 - based on atod_n.c 		*/

#include "hatlib.h"
#include "telemadd.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{

	int raw[320],i,j,k,ant,wait,one,start_add,number,address;
	double volts[320],avg,rms;
	char error[80],ans[3];

	if (argc != 4) {
		printf("Usage: adisplay ant start_add(d/o/h) number(<33)\n");
		return 1;
	}
	wait = 200; /* wait time between display updates */
	ant = atoi(argv[1]); one = 1;
	sscanf(argv[2],"%i",&start_add); number = atoi(argv[3]);
	if (number%2 != 0) {
		number++;
	}
	printf("Display counts or voltages? (c/v)");
	scanf("%s",ans);
	printf("\n"); /* make sure the display starts on a new line */
	mbusopen_(error); /* open the multibus to read/write */
	if ((strcmp(error,"OK")) != '\0') {
		printf("%s\n",error);
		return(1);
	}
/*
	atodcal_(&ant,error); 
	if ((strcmp(error,"OK")) != '\0') {
		printf("%s\n",error);
		return(1);
	}
*/
	printf("Starting address is %x (hex)\n\n",start_add);
/* now keep on grinding until interrupted */
	printf("\t Average      Rms\t   Average      Rms\n");
	while (1) {
		for (i=0;i<10;i++) {
			for (j=0;j<number;j++) {
				k = (j)*10 + i;
				address = (j*2)+start_add;
				raw[k] = atodin_(&ant,&address,error);
				volts[k] = raw[k]*4.516129/32768.0;
			}
			wait_ticks_(&one);
		}
		if ((ans[0] == 'c') || (ans[0] == 'C')) {
			for (j=0;j<number;j++) {
				avg = 0;
				for (i=(j*10);i<((j*10)+10);i++) {
					avg += raw[i];
				}
				avg/=10.0;
				rms = 0;
				for (i=(j*10);i<((j*10)+10);i++) {
					rms += (raw[i]-avg)*(raw[i]-avg);
				}
				rms/=10.0;
				rms = sqrt(rms);
				if (j%2 != 0) {
					printf("         %8.1f   %6.2f      \n",avg,rms);
				}
				else {
					printf("        %8.1f   %6.2f",avg,rms);
				}	
			}
		}
		else if ((ans[0] == 'v') || (ans[0] == 'V')) {
			for (j=0;j<number;j++) {
				avg = 0;
				for (i=(j*10);i<((j*10)+10);i++) {
					avg += volts[i];
				}
				avg/=10.0;
				rms = 0;
				for (i=(j*10);i<((j*10)+10);i++) {
					rms += (volts[i]-avg)*(volts[i]-avg);
				}
				rms/=10.0;
				rms = sqrt(rms);
				if (j%2 != 0) {
					printf("     %9.6f   %9.6f      \n",avg,rms);
				}
				else {
					printf("        %9.6f  %9.6f",avg,rms);
				}	
			}
		}
		else {
			printf("Illegal input, please try again\n");
		}
		wait_ticks_(&wait);
		printf("\033\133");
		printf("%d",number/2);
		printf("\101");
	}
	return(0);
}
