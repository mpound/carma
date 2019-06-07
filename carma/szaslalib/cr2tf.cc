#include "slalib.h"
#include "slamac.h"
void slaCr2tf ( int ndp, float angle, char *sign, int ihmsf[4] )
/*
**  - - - - - - - - -
**   s l a C r 2 t f
**  - - - - - - - - -
**
**  Convert an angle in radians into hours, minutes, seconds.
**
**  (single precision)
**
**  Given:
**     ndp       int      number of decimal places of seconds
**     angle     float    angle in radians
**
**  Returned:
**     sign      char*    '+' or '-'
**     ihmsf     int(4)   hours, minutes, seconds, fraction
**
**  Called:
**     slaDd2tf
**
**  Defined in slamac.h:  D2PI
**
**  Last revision:   18 November 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/* Scale then use days to h,m,s routine */
   slaDd2tf ( ndp, (double) angle / D2PI, sign, ihmsf );
}
