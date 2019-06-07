#include "slalib.h"
#include "slamac.h"
void slaTpv2c ( float xi, float eta, float v[3], float v01[3],
                                                 float v02[3], int *n )
/*
**  - - - - - - - - -
**   s l a T p v 2 c
**  - - - - - - - - -
**
**  Given the tangent-plane coordinates of a star and its direction
**  cosines, determine the direction cosines of the tangent-point.
**
**  (single precision)
**
**  Given:
**     xi,eta    float        tangent plane coordinates of star
**     v         float[3]     direction cosines of star
**
**  Returned:
**     v01       float[3]     direction cosines of TP, solution 1
**     v02       float[3]     direction cosines of TP, solution 2
**     *n        int          number of solutions:
**                             0 = no solutions returned (note 2)
**                             1 = only the first solution is useful (note 3)
**                             2 = both solutions are useful (note 3)
**
**  Notes:
**
**  1  The vector v must be of unit length or the result will be wrong.
**
**  2  Cases where there is no solution can only arise near the poles.
**     For example, it is clearly impossible for a star at the pole
**     itself to have a non-zero xi value, and hence it is meaningless
**     to ask where the tangent point would have to be.
**
**  3  Also near the poles, cases can arise where there are two useful
**     solutions.  The argument n indicates whether the second of the
**     two solutions returned is useful;  n=1 indicates only one useful
**     solution, the usual case.  Under these circumstances, the second
**     solution can be regarded as valid if the vector v02 is interpreted
**     as the "over-the-pole" case.
**
**  4  This routine is the Cartesian equivalent of the routine slaTps2c.
**
**  Last revision:   5 June 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   float x, y, z, rxy2, xi2, eta2p1, sdf, r2, r, c;


   x = v[0];
   y = v[1];
   z = v[2];
   rxy2 = x * x + y * y;
   xi2 = xi * xi;
   eta2p1 = eta*eta + 1.0f;
   sdf = z * (float) sqrt ( (double) ( xi2 + eta2p1 ) );
   r2 = rxy2 * eta2p1 - z * z * xi2;
   if ( r2 > 0.0f ) {
      r = (float) sqrt( (double) r2 );
      c = ( sdf * eta + r ) /
              ( eta2p1 * (float) sqrt ( (double) ( rxy2 * ( r2 + xi2 ) ) ) );
      v01[0] = c * ( x * r + y * xi );
      v01[1] = c * ( y * r - x * xi );
      v01[2] = ( sdf - eta * r ) / eta2p1;
      r = - r;
      c = ( sdf * eta + r ) /
              ( eta2p1 * (float) sqrt ( (double) ( rxy2 * ( r2 + xi2 ) ) ) );
      v02[0] = c * ( x * r + y * xi );
      v02[1] = c * ( y * r - x * xi );
      v02[2] = ( sdf - eta * r ) / eta2p1;
      *n = ( fabs ( sdf ) < 1.0f ) ? 1 : 2;
   } else {
      *n = 0;
   }
}
