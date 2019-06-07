#include "slalib.h"
#include "slamac.h"
void slaDh2e ( double az, double el, double phi, double *ha, double *dec )
/*
**  - - - - - - - -
**   s l a D h 2 e
**  - - - - - - - -
**
**  Horizon to equatorial coordinates:  Az,El to HA,Dec
**
**  (double precision)
**
**  Given:
**     az          double       azimuth
**     el          double       elevation
**     phi         double       observatory latitude
**
**  Returned:
**     *ha         double       hour angle
**     *dec        double       declination
**
**  Notes:
**
**  1)  All the arguments are angles in radians.
**
**  2)  The sign convention for azimuth is north zero, east +pi/2.
**
**  3)  HA is returned in the range +/-pi.  Declination is returned
**      in the range +/-pi/2.
**
**  4)  The is latitude is (in principle) geodetic.  In critical
**      applications, corrections for polar motion should be applied.
**
**  5)  In some applications it will be important to specify the
**      correct type of elevation in order to produce the required
**      type of HA,Dec.  In particular, it may be important to
**      distinguish between the elevation as affected by refraction,
**      which will yield the "observed" HA,Dec, and the elevation
**      in vacuo, which will yield the "topocentric" HA,Dec.  If the
**      effects of diurnal aberration can be neglected, the
**      topocentric HA,Dec may be used as an approximation to the
**      "apparent" HA,Dec.
**
**  6)  No range checking of arguments is done.
**
**  7)  In applications which involve many such calculations, rather
**      than calling the present routine it will be more efficient to
**      use inline code, having previously computed fixed terms such
**      as sine and cosine of latitude.
**
**  Last revision:   21 February 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double sa, ca, se, ce, sp, cp, x, y, z, r;

/* Useful trig functions */
   sa = sin ( az );
   ca = cos ( az );
   se = sin ( el );
   ce = cos ( el );
   sp = sin ( phi );
   cp = cos ( phi );

/* HA,Dec as x,y,z */
   x = - ca * ce * sp + se * cp;
   y = - sa * ce;
   z = ca * ce * cp + se * sp;

/* To spherical */
   r = sqrt ( x * x + y * y );
   *ha = ( r == 0.0 ) ? 0.0 : atan2 ( y, x ) ;
   *dec = atan2 ( z, r );
}
