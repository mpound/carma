#include "slalib.h"
#include "slamac.h"
double slaDsep ( double a1, double b1, double a2, double b2 )
/*
**  - - - - - - - -
**   s l a D s e p
**  - - - - - - - -
**
**  Angle between two points on a sphere.
**
**  (double precision)
**
**  Given:
**     a1,b1    double    spherical coordinates of one point
**     a2,b2    double    spherical coordinates of the other point
**
**  (The spherical coordinates are RA,dec, long,lat etc, in radians.)
**
**  The result is the angle, in radians, between the two points.  It
**  is always positive.
**
**  Called:  slaDcs2c
**
**  Last revision:   5 October 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i;
   double d, v1[3], v2[3], s2, c2;

/* Convert coordinates from spherical to Cartesian */
   slaDcs2c ( a1, b1, v1 );
   slaDcs2c ( a2, b2, v2 );

/* Modulus squared of half the difference vector */
   s2 = 0.0;
   for ( i = 0; i < 3; i++ ) {
      d = v1[i] - v2[i];
      s2 += d * d;
   }
   s2 /= 4.0;

/* Angle between the vectors */
   c2 = 1.0 - s2;
   return 2.0 * atan2 ( sqrt ( s2 ), sqrt ( gmax ( 0.0, c2 )));
}
