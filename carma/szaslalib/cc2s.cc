#include "slalib.h"
#include "slamac.h"
void slaCc2s ( float v[3], float *a, float *b )
/*
**  - - - - - - - -
**   s l a C c 2 s
**  - - - - - - - -
**
**  Direction cosines to spherical coordinates.
**
**  (single precision)
**
**  Given:
**     v       float[3]   x,y,z vector
**
**  Returned:
**     *a,*b   float      spherical coordinates in radians
**
**  The spherical coordinates are longitude (+ve anticlockwise
**  looking from the +ve latitude pole) and latitude.  The
**  Cartesian coordinates are right handed, with the x axis
**  at zero longitude and latitude, and the z axis at the
**  +ve latitude pole.
**
**  If v is null, zero a and b are returned.
**  At either pole, zero a is returned.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double x, y, z, r;

   x = (double) v[0];
   y = (double) v[1];
   z = (double) v[2];
   r = sqrt ( x * x + y * y );

   *a = ( r == 0.0 ) ? 0.0f : (float) atan2 ( y, x );
   *b = ( z == 0.0 ) ? 0.0f : (float) atan2 ( z, r );
}
