#include "slalib.h"
#include "slamac.h"
void slaPcd ( double disco, double *x, double *y )
/*
**  - - - - - - -
**   s l a P c d
**  - - - - - - -
**
**  Apply pincushion/barrel distortion to a tangent-plane [x,y].
**
**  Given:
**     disco    double      pincushion/barrel distortion coefficient
**     x,y      double      tangent-plane coordinates
**
**  Returned:
**     *x,*y    double      distorted coordinates
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
**   3)  For x,y in units of one projection radius (in the case of
**       a photographic plate, the focal length), the following
**       disco values apply:
**
**           geometry          disco
**
**           astrograph         0.0
**           schmidt           -0.3333
**           AAT pf doublet  +147.069
**           AAT pf triplet  +178.585
**           AAT f/8          +21.20
**           JKT f/8          +13.32
**
**    4)  There is a companion routine, slaUnpcd, which performs
**        an approximately inverse operation.
**
**  Last revision:   15 July 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  double f;

  f =  1.0 + disco * ( *x * *x + *y * *y );
  *x *= f;
  *y *= f;
}
