#include "slalib.h"
#include "slamac.h"
void slaTp2s ( float xi, float eta, float raz, float decz,
               float *ra, float *dec )
/*
**  - - - - - - - -
**   s l a T p 2 s
**  - - - - - - - -
**
**  Transform tangent plane coordinates into spherical.
**
**  (single precision)
**
**  Given:
**     xi,eta      float  tangent plane rectangular coordinates
**     raz,decz    float  spherical coordinates of tangent point
**
**  Returned:
**     *ra,*dec    float  spherical coordinates (0-2pi,+/-pi/2)
**
**  Called:        slaRanorm
**
**  Last revision:   10 July 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   float sdecz, cdecz, denom, radif;

   sdecz = (float) sin ( decz );
   cdecz = (float) cos ( decz );

   denom = cdecz - eta * sdecz;
   radif = (float) atan2 ( xi, denom );

   *ra = slaRanorm ( radif + raz );
   *dec = (float) atan2 ( sdecz + eta * cdecz ,
                          sqrt ( xi * xi + denom * denom ) );
}
