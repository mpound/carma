#include "slalib.h"
#include "slamac.h"
void slaE2h ( float ha, float dec, float phi, float *az, float *el )
/*
**  - - - - - - -
**   s l a E 2 h
**  - - - - - - -
**
**  Equatorial to horizon coordinates:  HA,Dec to Az,El
**
**  (single precision)
**
**  Given:
**     ha          float       hour angle
**     dec         float       declination
**     phi         float       observatory latitude
**
**  Returned:
**     *az         float       azimuth
**     *el         float       elevation
**
**  Notes:
**
**  1)  All the arguments are angles in radians.
**
**  2)  Azimuth is returned in the range 0-2pi;  north is zero,
**      and east is +pi/2.  Elevation is returned in the range
**      +/-pi/2.
**
**  3)  The latitude must be geodetic.  In critical applications,
**      corrections for polar motion should be applied.
**
**  4)  In some applications it will be important to specify the
**      correct type of hour angle and declination in order to
**      produce the required type of azimuth and elevation.  In
**      particular, it may be important to distinguish between
**      elevation as affected by refraction, which would
**      require the "observed" HA,Dec, and the elevation
**      in vacuo, which would require the "topocentric" HA,Dec.
**      If the effects of diurnal aberration can be neglected, the
**      "apparent" HA,Dec may be used instead of the topocentric
**      HA,Dec.
**
**  5)  No range checking of arguments is carried out.
**
**  6)  In applications which involve many such calculations, rather
**      than calling the present routine it will be more efficient to
**      use inline code, having previously computed fixed terms such
**      as sine and cosine of latitude, and (for tracking a star)
**      sine and cosine of declination.
**
**  Defined in slamac.h:  D2PI
**
**  Last revision:   10 July 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   float sh, ch, sd, cd, sp, cp, x, y, z, r, a;

/* Useful trig functions */
   sh = (float) sin ( ha );
   ch = (float) cos ( ha );
   sd = (float) sin ( dec );
   cd = (float) cos ( dec );
   sp = (float) sin ( phi );
   cp = (float) cos ( phi );

/* Az,El as x,y,z */
   x = - ch * cd * sp + sd * cp;
   y = - sh * cd;
   z = ch * cd * cp + sd * sp;

/* To spherical */
   r = (float) sqrt ( x * x + y * y );
   a = ( r == 0.0f ) ? 0.0f : (float) atan2 ( y, x ) ;
   *az = ( a < 0.0f ) ? (float) ( (double) a + D2PI ) : a;
   *el = (float) atan2 ( z, r );
}
