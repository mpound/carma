#include "slalib.h"
#include "slamac.h"
void slaDtp2v ( double xi, double eta, double v0[3], double v[3] )
/*
**  - - - - - - - - -
**   s l a D t p 2 v
**  - - - - - - - - -
**
**  Given the tangent-plane coordinates of a star and the direction
**  cosines of the tangent point, determine the direction cosines
**  of the star.
**
**  (double precision)
**
**  Given:
**     xi,eta    double      tangent plane coordinates of star
**     v0        double[3]   direction cosines of tangent point
**
**  Returned:
**     v         double[3]   direction cosines of star
**
**  Notes:
**
**  1  If vector v0 is not of unit length, the returned vector v will
**     be wrong.
**
**  2  If vector v0 points at a pole, the returned vector v will be
**     based on the arbitrary assumption that the RA of the tangent
**     point is zero.
**
**  3  This routine is the Cartesian equivalent of the routine slaDtp2s.
**
**  Last revision:   12 February 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double x, y, z, f, r;


   x = v0[0];
   y = v0[1];
   z = v0[2];
   f = sqrt ( 1.0 + xi * xi + eta * eta );
   r = sqrt ( x * x + y * y );
   if ( r == 0.0 ) {
      r = 1e-20;
      x = r;
   }
   v[0] = ( x - ( xi * y + eta * x * z ) / r ) / f;
   v[1] = ( y + ( xi * x - eta * y * z ) / r ) / f;
   v[2] = ( z + eta * r ) / f;
}
