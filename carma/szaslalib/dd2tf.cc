#include "slalib.h"
#include "slamac.h"
void slaDd2tf ( int ndp, double days, char *sign, int ihmsf[4] )
/*
**  - - - - - - - - -
**   s l a D d 2 t f
**  - - - - - - - - -
**
**  Convert an interval in days into hours, minutes, seconds.
**
**  (double precision)
**
**  Given:
**     ndp       int      number of decimal places of seconds
**     days      double   interval in days
**
**  Returned:
**     *sign     char     '+' or '-'
**     ihmsf     int[4]   hours, minutes, seconds, fraction
**
**  Last revision:   31 August 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define D2S 86400.0    /* Days to seconds */

{
   double rs, rm, rh, a, ah, am, as, af;

/* Handle sign */
   *sign = (char) ( ( days < 0.0 ) ?  '-' : '+' );

/* Field units in terms of least significant figure */
   rs = pow ( 10.0, (double) gmax ( ndp, 0 ) );
   rs = dint ( rs );
   rm = rs * 60.0;
   rh = rm * 60.0;

/* Round interval and express in smallest units required */
   a = rs * D2S * fabs ( days );
   a = dnint ( a );

/* Separate into fields */
   ah = a / rh;
   ah = dint ( ah );
   a  = a - ah * rh;
   am = a / rm;
   am = dint ( am );
   a  = a - am * rm;
   as = a / rs;
   as = dint ( as );
   af = a - as * rs;

/* Return results */
   ihmsf[0] = (int) ah;
   ihmsf[1] = (int) am;
   ihmsf[2] = (int) as;
   ihmsf[3] = (int) af;
}
