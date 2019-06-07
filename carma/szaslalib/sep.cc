#include "slalib.h"
#include "slamac.h"
float slaSep ( float a1, float b1, float a2, float b2 )
/*
**  - - - - - - -
**   s l a S e p
**  - - - - - - -
**
**  Angle between two points on a sphere.
**
**  (single precision)
**
**  Given:
**     a1,b1    float    spherical coordinates of one point
**     a2,b2    float    spherical coordinates of the other point
**
**  (The spherical coordinates are RA,Dec, long,lat etc, in radians.)
**
**  The result is the angle, in radians, between the two points.  It
**  is always positive.
**
**  Called:  slaCs2c
**
**  Last revision:   5 October 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i;
   float d, v1[3], v2[3], s2, c2;

/* Convert coordinates from spherical to Cartesian */
   slaCs2c ( a1, b1, v1 );
   slaCs2c ( a2, b2, v2 );

/* Modulus squared of half the difference vector */
   s2 = 0.0f;
   for ( i = 0; i < 3; i++ ) {
      d = v1[i] - v2[i];
      s2 += d * d;
   }
   s2 /= 4.0f;

/* Angle between the vectors */
   c2 = 1.0f - s2;
   return (float)
          ( 2.0 * atan2 ( sqrt ( s2 ), sqrt ( gmax ( 0.0f, c2 ) ) ) );
}
