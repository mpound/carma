/* adcheck.c program to check the status of the a/d convertor. This
program runs through all the 100 inputs on a given antenna and if
any of the inputs is equal to 32768, it prints out the number of
the address location(s). useful for diagnosing if the a/d card
is saturated */
	
/*	niranjan thatte nov 92 - based on adisplay.c 		*/

/*	dick plambeck jan 94; check for 32767 or higher because
	doug's atod on ant 1 saturates at 32767 */

#include "hatlib.h"
#include "telemadd.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{

	int raw[1000],i,j,k,ant,one,start_add,number,address,isbad;
	double avg;
	char error[80];
	one = 1;
	start_add = 112;
	number = 100;
	isbad = 0;

	if (argc != 2) {
		printf("Usage: adcheck ant(hex) \n");
		return 1;
	}
	sscanf(argv[1],"%x",&ant);

	mbusopen_(error); 
	if ((strcmp(error,"OK")) != '\0') {
		printf("%s\n",error);
		return(1);
	}

	for (i=0;i<10;i++) {
		for (j=0;j<number;j++) {
			k = (j)*10 + i;
			address = (j*2)+start_add;
			raw[k] = atodin_(&ant,&address,error);
		}
		wait_ticks_(&one);
	}
	for (j=0;j<number;j++) {
		avg = 0;
		for (i=(j*10);i<((j*10)+10);i++) {
			avg += raw[i];
		}
		avg/=10.0;
		if ((avg >= 32767.0) || (avg <= -32767.0)) {
			printf("Address (decimal) %d has value %8.1f\n",((j*2)+start_add),avg);
			isbad = 1;
		}
	}
	if (isbad == 0) {
		printf("No saturated addresses detected\n");
	}
	return 0;
}
