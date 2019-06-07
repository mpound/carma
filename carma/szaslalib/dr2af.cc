#include "slalib.h"
#include "slamac.h"
void slaDr2af ( int ndp, double angle, char *sign, int idmsf[4] )
/*
**  - - - - - - - - -
**   s l a D r 2 a f
**  - - - - - - - - -
**
**  Convert an angle in radians to degrees, arcminutes, arcseconds.
**
**  (double precision)
**
**  Given:
**     ndp       int          number of decimal places of arcseconds
**     angle     double       angle in radians
**
**  Returned:
**     sign      char*        '+' or '-'
**     idmsf     int[4]       degrees, arcminutes, arcseconds, fraction
**
**  Called:
**     slaDd2tf
**
**  Defined in slamac.h:  D15B2P
**
**  Last revision:   19 November 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/* Scale then use days to h,m,s routine */
   slaDd2tf ( ndp, angle * D15B2P, sign, idmsf );
}
