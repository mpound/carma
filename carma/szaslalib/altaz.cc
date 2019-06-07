#include "slalib.h"
#include "slamac.h"
void slaAltaz ( double ha, double dec, double phi,
                double *az, double *azd, double *azdd,
                double *el, double *eld, double *eldd,
                double *pa, double *pad, double *padd )
/*
**  - - - - - - - - -
**   s l a A l t a z
**  - - - - - - - - -
**
**  Positions, velocities and accelerations for an altazimuth
**  telescope mount.
**
**  (double precision)
**
**  Given:
**     ha          double      hour angle
**     dec         double      declination
**     phi         double      latitude
**
**  Returned:
**     *az         double      azimuth
**     *azd        double         "    velocity
**     *azdd       double         "    acceleration
**     *el         double      elevation
**     *eld        double          "     velocity
**     *eldd       double          "     acceleration
**     *pa         double      parallactic angle
**     *pad        double          "      "   velocity
**     *padd       double          "      "   acceleration
**
**  Notes:
**
**  1)  Natural units are used throughout.  HA, DEC, PHI, AZ, EL
**      and ZD are in radians.  The velocities and accelerations
**      assume constant declination and constant rate of change of
**      hour angle (as for tracking a star);  the units of AZD, ELD
**      and PAD are radians per radian of HA, while the units of AZDD,
**      ELDD and PADD are radians per radian of HA squared.  To
**      convert into practical degree- and second-based units:
**
**        angles * 360/2pi -> degrees
**        velocities * (2pi/86400)*(360/2pi) -> degree/sec
**        accelerations * ((2pi/86400)**2)*(360/2pi) -> degree/sec/sec
**
**      Note that the seconds here are sidereal rather than SI.  One
**      sidereal second is about 0.99727 SI seconds.
**
**      The velocity and acceleration factors assume the sidereal
**      tracking case.  Their respective numerical values are (exactly)
**      1/240 and (approximately) 1/3300236.9.
**
**  2)  Azimuth is returned in the range 0-2pi;  north is zero,
**      and east is +pi/2.  Elevation and parallactic angle are
**      returned in the range +/-pi/2.  Position angle is +ve
**      for a star west of the meridian and is the angle NP-star-zenith.
**
**  3)  The latitude is geodetic as opposed to geocentric.  The
**      hour angle and declination are topocentric.  Refraction and
**      deficiencies in the telescope mounting are ignored.  The
**      purpose of the routine is to give the general form of the
**      quantities.  The details of a real telescope could profoundly
**      change the results, especially close to the zenith.
**
**  4)  No range checking of arguments is carried out.
**
**  5)  In applications which involve many such calculations, rather
**      than calling the present routine it will be more efficient to
**      use inline code, having previously computed fixed terms such
**      as sine and cosine of latitude, and (for tracking a star)
**      sine and cosine of declination.
**
**  Defined in slamac.h:  DPI, D2PI
**
**  Last revision:   14 March 1997
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define TINY 1e-30   /* Zone of avoidance around zenith/nadir */

{
   double sh, ch, sd, cd, sp, cp, chcd, sdcp, x, y, z, rsq, r, a, e,
          c, s, q, qd, ad, ed, edr, add, edd, qdd;

/* Useful functions */
   sh = sin ( ha );
   ch = cos ( ha );
   sd = sin ( dec );
   cd = cos ( dec );
   sp = sin ( phi );
   cp = cos ( phi );
   chcd = ch * cd;
   sdcp = sd * cp;
   x = - chcd * sp + sdcp;
   y = - sh * cd;
   z = chcd * cp + sd * sp;
   rsq = x * x + y * y;
   r = sqrt ( rsq );

/* Azimuth and elevation */
   if ( rsq == 0.0 ) {
      a = 0.0;
   } else {
      a = atan2 ( y, x );
   }
   if ( a < 0.0 ) a += D2PI;
   e = atan2 ( z, r );

/* Parallactic angle */
   c = cd * sp - ch * sdcp;
   s = sh * cp;
   if ( c * c + s * s > 0.0 ) {
      q = atan2 ( s, c );
   } else {
      q = DPI - ha;
   }

/* Velocities and accelerations (clamped at zenith/nadir) */
   if ( rsq < TINY ) {
      rsq = TINY;
      r = sqrt ( rsq );
   }
   qd = - x * cp / rsq;
   ad = sp + z * qd;
   ed = cp * y / r;
   edr = ed / r;
   add = edr * ( z * sp + ( 2.0 - rsq ) * qd );
   edd = - r * qd * ad;
   qdd = edr * ( sp + 2.0 * z * qd );

/* Results */
   *az = a;
   *azd = ad;
   *azdd = add;
   *el = e;
   *eld = ed;
   *eldd = edd;
   *pa = q;
   *pad = qd;
   *padd = qdd;
}
