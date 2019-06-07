#include "slalib.h"
#include "slamac.h"
void slaDjcal ( int ndp, double djm, int iymdf[4], int *j )
/*
**  - - - - - - - - -
**   s l a D j c a l
**  - - - - - - - - -
**
**  Modified Julian Date to Gregorian calendar, expressed
**  in a form convenient for formatting messages (namely
**  rounded to a specified precision, and with the fields
**  stored in a single array).
**
**  Given:
**     ndp      int       number of decimal places of days in fraction
**     djm      double    Modified Julian Date (JD-2400000.5)
**
**  Returned:
**     iymdf    int[4]    year, month, day, fraction in Gregorian calendar
**     *j       long      status:  nonzero = out of range
**
**  Any date after 4701BC March 1 is accepted.
**
**  Large ndp values risk internal overflows.  It is typically safe
**  to use up to ndp=4.
**
**  The algorithm is derived from that of Hatcher 1984 (QJRAS 25, 53-55).
**
**  Defined in slamac.h:  dmod
**
**  Last revision:   19 March 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double fd, df, f, d;
   long jd, n4, nd10;

/* Validate */
   if ( ( djm <= -2395520.0 ) || ( djm >= 1.0e9 ) ) {
      *j = - 1;
      return;
   } else {

   /* Denominator of fraction */
      fd = pow ( 10.0, (double) gmax ( ndp, 0 ) );
      fd = dnint ( fd );

   /* Round date and express in units of fraction */
      df = djm * fd;
      df = dnint ( df );

   /* Separate day and fraction */
      f = dmod ( df, fd );
      if ( f < 0.0 ) f += fd;
      d = ( df - f ) / fd;

   /* Express day in Gregorian calendar */
      jd = (long) dnint ( d ) + 2400001L;
      n4 = 4L * ( jd + ( ( 2L * ( ( 4L * jd - 17918L ) / 146097L)
                                       * 3L ) / 4L + 1L ) / 2L - 37L );
      nd10 = 10L * ( ( ( n4 - 237L ) % 1461L ) / 4L ) + 5L;
      iymdf[0] = (int) ( ( n4 / 1461L ) - 4712L );
      iymdf[1] = (int) ( ( ( nd10 / 306L + 2L ) % 12L ) + 1L );
      iymdf[2] = (int) ( ( nd10 % 306L ) / 10L + 1L );
      iymdf[3] = (int) dnint ( f );
      *j = 0;
   }
}
