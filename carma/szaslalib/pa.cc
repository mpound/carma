#include "slalib.h"
#include "slamac.h"
double slaPa ( double ha, double dec, double phi )
/*+
**  - - - - - -
**   s l a P a
**  - - - - - -
**
**  HA, Dec to Parallactic Angle.
**
**  (double precision)
**
**  Given:
**     ha     d     hour angle in radians (geocentric apparent)
**     dec    d     declination in radians (geocentric apparent)
**     phi    d     observatory latitude in radians (geodetic)
**
**  The result is in the range -pi to +pi
**
**  Notes:
**
**  1)  The parallactic angle at a point in the sky is the position
**      angle of the vertical, i.e. the angle between the direction to
**      the pole and to the zenith.  In precise applications care must
**      be taken only to use geocentric apparent HA,Dec and to consider
**      separately the effects of atmospheric refraction and telescope
**      mount errors.
**
**  2)  At the pole a zero result is returned.
**
**  Last revision:   16 August 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double cp, cqsz, sqsz;

   cp = cos ( phi );
   sqsz = cp * sin ( ha );
   cqsz = sin ( phi ) * cos ( dec) - cp * sin ( dec) * cos ( ha );
   return ( ( sqsz != 0.0 || cqsz != 0.0 ) ? atan2 ( sqsz, cqsz ) : 0.0 );
}
