#include "slalib.h"
#include "slamac.h"
void slaV2tp ( float v[3], float v0[3], float *xi, float *eta, int *j )
/*
**  - - - - - - - -
**   s l a V 2 t p
**  - - - - - - - -
**
**  Given the direction cosines of a star and of the tangent point,
**  determine the star's tangent-plane coordinates.
**
**  (single precision)
**
**  Given:
**     v         float[3]    direction cosines of star
**     v0        float[3]    direction cosines of tangent point
**
**  Returned:
**     *xi,*eta  float       tangent plane coordinates of star
**     j         int         status:   0  =  OK
**                                     1  =  error, star too far from axis
**                                     2  =  error, antistar on tangent plane
**                                     3  =  error, antistar too far from axis
**
**  Notes:
**
**  1  If vector v0 is not of unit length, or if vector v is of zero
**     length, the results will be wrong.
**
**  2  If v0 points at a pole, the returned xi,eta will be based on the
**     arbitrary assumption that the RA of the tangent point is zero.
**
**  3  This routine is the Cartesian equivalent of the routine slaS2tp.
**
**  Last revision:   27 November 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
#define TINY 1e-6f
{
   float x, y, z, x0, y0, z0, r2, r, w, d;


   x = v[0];
   y = v[1];
   z = v[2];
   x0 = v0[0];
   y0 = v0[1];
   z0 = v0[2];
   r2 = x0 * x0 + y0 * y0;
   r = (float) sqrt ( (double) r2 );
   if ( r == 0.0f ) {
      r = 1e-20f;
      x0 = r;
   }
   w = x * x0 + y * y0;
   d = w + z * z0;
   if ( d > TINY ) {
      *j = 0;
   } else  if ( d >= 0.0f ) {
      *j = 1;
      d = TINY;
   } else if ( d > -TINY ) {
      *j = 2;
      d = -TINY;
   } else {
      *j = 3;
   }
   d *= r;
   *xi = ( y * x0 - x * y0 ) / d;
   *eta = ( z * r2 - z0 * w ) / d;
}
