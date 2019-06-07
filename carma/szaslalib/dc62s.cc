#include "slalib.h"
#include "slamac.h"
void slaDc62s ( double v[6], double *a, double *b, double *r,
                double *ad, double *bd, double *rd )
/*
**  - - - - - - - - -
**   s l a D c 6 2 s
**  - - - - - - - - -
**
**  Conversion of position & velocity in Cartesian coordinates
**  to spherical coordinates.
**
**  (double precision)
**
**  Given:
**     v     double[6]  Cartesian position & velocity vector
**
**  Returned:
**     *a    double     longitude (radians)
**     *b    double     latitude (radians)
**     *r    double     radial coordinate
**     *ad   double     longitude derivative (radians per unit time)
**     *bd   double     latitude derivative (radians per unit time)
**     *rd   double     radial derivative
**
**  Last revision:   28 April 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double x, y, z, xd, yd, zd, rxy2, rxy, r2, xyp;


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
      *a = atan2 ( y, x );
      *b = atan2 ( z, rxy );
      *ad = ( x * yd - y * xd ) / rxy2;
      *bd = ( zd * rxy2 - z * xyp ) / ( r2 * rxy );
   } else {
      *a = 0.0;
      *b = ( z != 0.0 ) ? atan2 ( z, rxy ) : 0.0;
      *ad = 0.0;
      *bd = 0.0;
   }
   *rd = ( ( *r = sqrt ( r2 ) ) != 0.0 ) ? ( xyp + z * zd ) / *r : 0.0;
}
