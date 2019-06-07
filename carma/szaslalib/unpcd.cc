#include "slalib.h"
#include "slamac.h"
void slaUnpcd ( double disco, double *x, double *y )
/*
**  - - - - - - - - -
**   s l a U n p c d
**  - - - - - - - - -
**
**  Remove pincushion/barrel distortion from a distorted [x,y]
**  to give tangent-plane [x,y].
**
**  Given:
**     disco    double      pincushion/barrel distortion coefficient
**     x,y      double      distorted coordinates
**
**  Returned:
**     *x,*y    double      tangent-plane coordinates
**
**  Notes:
**
**   1)  The distortion is of the form rp = r*(1 + c*r**2), where r is
**       the radial distance from the tangent point, c is the disco
**       argument, and rp is the radial distance in the presence of
**       the distortion.
**
**   2)  For pincushion distortion, c is +ve;
**       For barrel distortion, c is -ve.
**
**   3)  For x,y in "radians" - units of one projection radius,
**       which in the case of a photograph is the focal length of
**       the camera - the following disco values apply:
**
**           geometry          disco
**
**           astrograph         0.0
**           schmidt           -0.3333
**           AAT PF doublet  +147.069
**           AAT PF triplet  +178.585
**           AAT f/8          +21.20
**           JKT f/8          +13.32
**
**    4)  The present routine is an approximate inverse to the
**        companion routine slaPcd, obtained from two iterations
**        of Newton's method.  The mismatch between the slaPcd and
**        slaUnpcd routines is negligible for astrometric applications;
**        To reach 1 milliarcsec at the edge of the AAT triplet or
**        Schmidt field would require field diameters of 2.4 degrees
**        and 42 degrees respectively.
**
**  Last revision:   1 August 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double cr2, a, cr2a2, f;

   cr2 = disco * ( *x * *x + *y * *y );
   a = ( 2.0 * cr2 + 1.0 ) / ( 3.0 * cr2 + 1.0 );
   cr2a2 = cr2 * a * a;
   f = ( 2.0 * cr2a2 * a + 1.0 ) / ( 3.0 * cr2a2 + 1.0 );
   *x *= f;
   *y *= f;
}
