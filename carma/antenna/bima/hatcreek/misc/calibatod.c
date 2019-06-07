/* calibatod.c program which calls atodcal in order to calibrate
the atod card...only to be used for the new antennas...on the
retrofits, the card calibrates itself automatically when power
it turned on - you hope! */

/* niranjan thatte oct 92 */

#include "hatlib.h"
#include <string.h>

int main(int argc, char *argv[])
{
	int ant;
	char error[80];

	if (argc != 2) {
		printf("Usage: calibatod ant(hex)\n");
		return 1;
	}

	sscanf(argv[1],"%x",&ant);
	mbusopen_(error);
	atodcal_(&ant,error);
	if ((strcmp(error,"OK")) != '\0') {
		printf("%s\n",error);
		return 1;
	}
	return 0;
}

