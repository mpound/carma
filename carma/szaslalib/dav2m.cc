#include "slalib.h"
#include "slamac.h"
void slaDav2m ( double axvec[3], double rmat[3][3] )
/*
**  - - - - - - - - -
**   s l a D a v 2 m
**  - - - - - - - - -
**
**  Form the rotation matrix corresponding to a given axial vector.
**
**  (double precision)
**
**  A rotation matrix describes a rotation about some arbitrary axis.
**  The axis is called the Euler axis, and the angle through which the
**  reference frame rotates is called the Euler angle.  The axial
**  vector supplied to this routine has the same direction as the
**  Euler axis, and its magnitude is the Euler angle in radians.
**
**  Given:
**    axvec  double[3]     axial vector (radians)
**
**  Returned:
**    rmat   double[3][3]  rotation matrix
**
**  If axvec is null, the unit matrix is returned.
**
**  The reference frame rotates clockwise as seen looking along
**  the axial vector from the origin.
**
**  Last revision:   25 July 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  double x, y, z, phi, s, c, w;

/* Euler angle - magnitude of axial vector - and functions */
   x   = axvec[0];
   y   = axvec[1];
   z   = axvec[2];
   phi = sqrt ( x * x + y * y + z * z );
   s   = sin ( phi );
   c   = cos ( phi );
   w   = 1.0 - c;

/* Euler axis - direction of axial vector (perhaps null) */
   if ( phi != 0.0 ) {
      x = x / phi;
      y = y / phi;
      z = z / phi;
   }

/* Compute the rotation matrix */
   rmat[0][0] = x * x * w + c;
   rmat[0][1] = x * y * w + z * s;
   rmat[0][2] = x * z * w - y * s;
   rmat[1][0] = x * y * w - z * s;
   rmat[1][1] = y * y * w + c;
   rmat[1][2] = y * z * w + x * s;
   rmat[2][0] = x * z * w + y * s;
   rmat[2][1] = y * z * w - x * s;
   rmat[2][2] = z * z * w + c;
}
