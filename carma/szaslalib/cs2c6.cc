#include "slalib.h"
#include "slamac.h"
void slaCs2c6 ( float a, float b, float r, float ad,
                float bd, float rd, float v[6] )
/*
**  - - - - - - - - -
**   s l a C s 2 c 6
**  - - - - - - - - -
**
**  Conversion of position & velocity in spherical coordinates
**  to Cartesian coordinates.
**
**  (single precision)
**
**  Given:
**     a     float      longitude (radians)
**     b     float      latitude (radians)
**     r     float      radial coordinate
**     ad    float      longitude derivative (radians per unit time)
**     bd    float      latitude derivative (radians per unit time)
**     rd    float      radial derivative
**
**  Returned:
**     v     float(6)   Cartesian position & velocity vector
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double da, db;
   float sa, ca, sb, cb, rcb, x, y, rbd, w;

/* Useful functions */
   da = (double) a;
   db = (double) b;
   sa = (float) sin ( da );
   ca = (float) cos ( da );
   sb = (float) sin ( db );
   cb = (float) cos ( db );
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
   v[3] = - y * ad - w * ca;
   v[4] = x * ad - w * sa;
   v[5] = rbd * cb + sb * rd;
}
