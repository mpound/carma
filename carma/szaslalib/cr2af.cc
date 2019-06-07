#include "slalib.h"
#include "slamac.h"
void slaCr2af ( int ndp, float angle, char *sign, int idmsf[4] )
/*
**  - - - - - - - - -
**   s l a C r 2 a f
**  - - - - - - - - -
**
**  Convert an angle in radians into degrees, arcminutes, arcseconds.
**
**  (single precision)
**
**  Given:
**     ndp       int      number of decimal places of arcseconds
**     angle     float    angle in radians
**
**  Returned:
**     sign      *char    '+' or '-'
**     idmsf     int[4]   degrees, arcminutes, arcseconds, fraction
**
**  Called:
**     slaDd2tf
**
**  Defined in slamac.h:  D15B29
**
**  Last revision:   18 November 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/* Scale then use days to h,m,s routine */
   slaDd2tf ( ndp, (double) angle * D15B2P, sign, idmsf );
}
