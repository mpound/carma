#include "slalib.h"
#include "slamac.h"
double slaZd ( double ha, double dec, double phi )
/*
**  - - - - - -
**   s l a Z d
**  - - - - - -
**
**  HA, Dec to Zenith Distance.
**
**  (double precision)
**
**  Given:
**     ha     double     Hour Angle in radians
**     dec    double     declination in radians
**     phi    double     observatory latitude in radians
**
**  The result is in the range 0 to pi.
**
**  Notes:
**
**  1)  The latitude must be geodetic.  In critical applications,
**      corrections for polar motion should be applied.
**
**  2)  In some applications it will be important to specify the
**      correct type of hour angle and declination in order to
**      produce the required type of zenith distance.  In particular,
**      it may be important to distinguish between the zenith distance
**      as affected by refraction, which would require the "observed"
**      HA,Dec, and the zenith distance in vacuo, which would require
**      the "topocentric" HA,Dec.  If the effects of diurnal aberration
**      can be neglected, the "apparent" HA,Dec may be used instead of
**      the topocentric HA,Dec.
**
**  3)  No range checking of arguments is done.
**
**  4)  In applications which involve many zenith distance calculations,
**      rather than calling the present routine it will be more efficient
**      to use inline code, having previously computed fixed terms such
**      as sine and cosine of latitude, and perhaps sine and cosine of
**      declination.
**
**  Last revision:   4 April 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double sh, ch, sd, cd, sp, cp, x, y, z;

   sh = sin ( ha );
   ch = cos ( ha );
   sd = sin ( dec );
   cd = cos ( dec );
   sp = sin ( phi );
   cp = cos ( phi );

   x = ch * cd * sp - sd * cp;
   y = sh * cd;
   z = ch * cd * cp + sd * sp;

   return atan2 ( sqrt ( x * x + y * y ), z );
}
