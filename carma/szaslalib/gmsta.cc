#include "slalib.h"
#include "slamac.h"
double slaGmsta ( double date, double ut1 )
/*
**  - - - - - - - - -
**   s l a G m s t a
**  - - - - - - - - -
**
**  Conversion from Universal Time to Sidereal Time with
**  rounding errors minimized.
**
**  (double precision)
**
**  Given:
*     date   double     UT1 date (MJD: integer part of JD-2400000.5))
**    ut1    double     UT1 time (fraction of a day)
**
**  The result is the Greenwich Mean Sidereal Time (double
**  precision, radians).
**
**  There is no restriction on how the UT is apportioned between the
**  date and ut1 arguments.  Either of the two arguments could, for
**  example, be zero and the entire date+time supplied in the other.
**  However, the routine is designed to deliver maximum accuracy when
**  the date argument is a whole number and ut1 lies in the range
**  0 to 1.
**
**  See also the routine slaGmst, which accepts the UT1 as a single
**  argument.  Compared with slaGmst, the extra numerical precision
**  delivered by the present routine is unlikely to be important in
**  an absolute sense, but may be useful when critically comparing
**  algorithms and in applications where two sidereal times close
**  together are differenced.
**
**  Called:  slaDranrm
**
**  Defined in slamac.h:  DS2R, dmod
**
**  Last revision:   19 March 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double f, d, t;

/* Fractional part of date (if any) */
   f = dmod ( date, 1.0 );

/* Days from fundamental epoch J2000 to 0h UT on this date */
   d = date - 51544.5;

/* Julian centuries from fundamental epoch J2000 to this UT */
   t = ( d + ut1 ) / 36525.0;

/* GMST at this UT */
   return slaDranrm ( DS2R * ( 24110.54841
                             + 86636.555367909 * ut1
                             + 236.555367909 * d
                             + 86400 * f
                             + ( 0.093104 - 6.2e-6 * t ) * t * t ) );
}
