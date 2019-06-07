#include "slalib.h"
#include "slamac.h"
void slaCd2tf ( int ndp, float days, char *sign, int ihmsf[4] )
/*
**  - - - - - - - - -
**   s l a C d 2 t f
**  - - - - - - - - -
**
**  Convert an interval in days into hours, minutes, seconds.
**
**  (single precision)
**
**  Given:
**     ndp       int      number of decimal places of seconds
**     days      float    interval in days
**
**  Returned:
**     sign      char*    '+' or '-'
**     ihmsf     int[4]   hours, minutes, seconds, fraction
**
**  Called:  slaDd2tf
**
**  Last revision:   11 December 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/* Use double version */
   slaDd2tf ( ndp, (double) days, sign, ihmsf );
}
