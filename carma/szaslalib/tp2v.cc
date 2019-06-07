#include "slalib.h"
#include "slamac.h"
void slaTp2v ( float xi, float eta, float v0[3], float v[3] )
/*
**  - - - - - - - -
**   s l a T p 2 v
**  - - - - - - - -
**
**  Given the tangent-plane coordinates of a star and the direction
**  cosines of the tangent point, determine the direction cosines
**  of the star.
**
**  (single precision)
**
**  Given:
**     xi,eta    float      tangent plane coordinates of star
**     v0        float[3]   direction cosines of tangent point
**
**  Returned:
**     v         float[3]   direction cosines of star
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
**  3  This routine is the Cartesian equivalent of the routine slaTp2s.
**
**  Last revision:   11 February 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   float x, y, z, f, r;


   x = v0[0];
   y = v0[1];
   z = v0[2];
   f = (float) sqrt ( (double) ( 1.0f + xi * xi + eta * eta ) );
   r = (float) sqrt ( (double) ( x * x + y * y ) );
   if ( r == 0.0f ) {
      r = 1e-20f;
      x = r;
   }
   v[0] = ( x - ( xi * y + eta * x * z ) / r ) / f;
   v[1] = ( y + ( xi * x - eta * y * z ) / r ) / f;
   v[2] = ( z + eta * r ) / f;
}
