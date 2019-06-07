#include "slalib.h"
#include "slamac.h"
void slaCc62s ( float v[6],
                float *a, float *b, float *r,
                float *ad, float *bd, float *rd )
/*
**  - - - - - - - - -
**   s l a C c 6 2 s
**  - - - - - - - - -
**
**  Conversion of position & velocity in Cartesian coordinates
**  to spherical coordinates.
**
**  (single precision)
**
**  Given:
**     v     float[6]   Cartesian position & velocity vector
**
**  Returned:
**     *a    float      longitude (radians)
**     *b    float      latitude (radians)
**     *r    float      radial coordinate
**     *ad   float      longitude derivative (radians per unit time)
**     *bd   float      latitude derivative (radians per unit time)
**     *rd   float      radial derivative
**
**  Last revision:   28 April 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double x, y, z, xd, yd, zd, rxy2, rxy, r2, xyp, dr;


/* Components of position/velocity vector. */
   x = v[0];
   y = v[1];
   z = v[2];
   xd = v[3];
   yd = v[4];
   zd = v[5];

/* Component of R in XY plane squared. */
   rxy2 = x * x + y * y;

/* Modulus squared, with protection against null vector. */
   if ( ( r2 = rxy2 + z * z ) == 0.0 ) {
      x = xd;
      y = yd;
      z = zd;
      rxy2 = x * x + y * y;
      r2 = rxy2 + z * z;
   }

/* Position and velocity in spherical coordinates. */
   rxy = sqrt ( rxy2 );
   xyp = x * xd + y * yd;
   if ( rxy2 != 0.0 ) {
      *a = (float) atan2 ( y, x );
      *b = (float) atan2 ( z, rxy );
      *ad = (float) ( ( x * yd - y * xd ) / rxy2 );
      *bd = (float) ( ( zd * rxy2 - z * xyp ) / ( r2 * rxy ) );
   } else {
      *a = 0.0f;
      *b = (float) ( ( z != 0.0 ) ? atan2 ( z, rxy ) : 0.0 );
      *ad = 0.0f;
      *bd = 0.0f;
   }
   *r = (float) ( dr = sqrt ( r2 ) );
   *rd = (float) ( ( dr != 0.0 ) ? ( xyp + z * zd ) / dr : 0.0 );
}
