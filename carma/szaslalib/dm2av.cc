#include "slalib.h"
#include "slamac.h"
void slaDm2av ( double rmat[3][3], double axvec[3] )
/*
**  - - - - - - - - -
**   s l a D m 2 a v
**  - - - - - - - - -
**
**  From a rotation matrix, determine the corresponding axial vector.
**
**  (double precision)
**
**  A rotation matrix describes a rotation about some arbitrary axis.
**  The axis is called the Euler axis, and the angle through which the
**  reference frame rotates is called the Euler angle.  The axial
**  vector returned by this routine has the same direction as the
**  Euler axis, and its magnitude is the Euler angle in radians.  (The
**  magnitude and direction can be separated by means of the routine
**  slaDvn.)
**
**  Given:
**    rmat   double[3][3]   rotation matrix
**
**  Returned:
**    axvec  double[3]      axial vector (radians)
**
**  The reference frame rotates clockwise as seen looking along
**  the axial vector from the origin.
**
**  If rmat is null, so is the result.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double x, y, z, s2, c2, phi, f;

   x = rmat[1][2] - rmat[2][1];
   y = rmat[2][0] - rmat[0][2];
   z = rmat[0][1] - rmat[1][0];
   s2 = sqrt ( x * x + y * y + z * z );
   if ( s2 != 0.0 )
   {
      c2 = ( rmat[0][0] + rmat[1][1] + rmat[2][2] - 1.0 );
      phi = atan2 ( s2 / 2.0, c2 / 2.0 );
      f = phi / s2;
      axvec[0] = x * f;
      axvec[1] = y * f;
      axvec[2] = z * f;
   } else {
      axvec[0] = 0.0;
      axvec[1] = 0.0;
      axvec[2] = 0.0;
   }
}
