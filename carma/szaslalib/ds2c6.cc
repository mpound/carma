#include "slalib.h"
#include "slamac.h"
void slaDs2c6 ( double a, double b, double r, double ad,
                double bd, double rd, double v[6] )
/*
**  - - - - - - - - -
**   s l a D s 2 c 6
**  - - - - - - - - -
**
**  Conversion of position & velocity in spherical coordinates
**  to Cartesian coordinates.
**
**  (double precision)
**
**  Given:
**     a     double      longitude (radians)
**     b     double      latitude (radians)
**     r     double      radial coordinate
**     ad    double      longitude derivative (radians per unit time)
**     bd    double      latitude derivative (radians per unit time)
**     rd    double      radial derivative
**
**  Returned:
**     v     double[6]   Cartesian position & velocity vector
**
**  Last revision:   16 November 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double sa, ca, sb, cb, rcb, x, y, rbd, w;

/* Useful functions */
   sa = sin ( a );
   ca = cos ( a );
   sb = sin ( b );
   cb = cos ( b );
   rcb = r * cb;
   x = rcb * ca;
   y = rcb * sa;
   rbd = r * bd;
   w = rbd * sb - cb * rd;

/* Position */
   v[0] = x;
   v[1] = y;
   v[2] = r * sb;

/* Velocity */
   v[3] = -y * ad - w * ca;
   v[4] = x * ad - w * sa;
   v[5] = rbd * cb + sb * rd;
}
