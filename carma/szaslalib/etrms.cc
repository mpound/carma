#include "slalib.h"
#include "slamac.h"
void slaEtrms ( double ep, double ev[3] )
/*
**  - - - - - - - - -
**   s l a E t r m s
**  - - - - - - - - -
**
**  Compute the e-terms (elliptic component of annual aberration)
**  vector.
**
**  (double precision)
**
**  Given:
**     ep      double      Besselian epoch
**
**  Returned:
**     ev      double[3]   e-terms as (dx,dy,dz)
**
**  References:
**
**     1  Smith, C.A. et al, 1989.  "The transformation of astrometric
**        catalog systems to the equinox J2000.0".  Astron.J. 97, 265.
**
**     2  Yallop, B.D. et al, 1989.  "Transformation of mean star places
**        from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space".
**        Astron.J. 97, 274.
**
**  Note the use of the J2000 aberration constant (20.49552 arcsec).
**  This is a reflection of the fact that the e-terms embodied in
**  existing star catalogues were computed from a variety of
**  aberration constants.  Rather than adopting one of the old
**  constants the latest value is used here.
**
**  Defined in slamac.h:  DAS2R
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double t, e, e0, p, ek, cp;

/* Julian centuries since B1950 */
   t = ( ep - 1950.0 ) * 1.00002135903e-2;

/* Eccentricity */
   e = 0.01673011 - ( 0.00004193 + 0.000000126 * t ) * t;

/* Mean obliquity */
   e0 = ( 84404.836 -
              ( 46.8495 + ( 0.00319 + 0.00181 * t ) * t ) * t ) * DAS2R;

/* Mean longitude of perihelion */
   p = ( 1015489.951 +
              ( 6190.67 + ( 1.65 + 0.012 * t ) * t ) * t ) * DAS2R;

/* E-terms */
  ek = e * 20.49552 * DAS2R;
  cp = cos ( p );
  ev[0] = ek * sin ( p );
  ev[1] = -ek * cp * cos ( e0 );
  ev[2] = -ek * cp * sin ( e0 );
}
