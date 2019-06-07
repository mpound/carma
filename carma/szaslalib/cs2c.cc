#include "slalib.h"
#include "slamac.h"
void slaCs2c ( float a, float b, float v[3] )
/*
**  - - - - - - - -
**   s l a C s 2 c
**  - - - - - - - -
**
**  Spherical coordinates to direction cosines.
**
**  (single precision)
**
**  Given:
**     a,b      float     spherical coordinates in radians
**                        (RA,Dec), (long,lat) etc
**
**  Returned:
**     v        float[3]  x,y,z unit vector
**
**  The spherical coordinates are longitude (+ve anticlockwise
**  looking from the +ve latitude pole) and latitude.  The
**  Cartesian coordinates are right handed, with the x axis
**  at zero longitude and latitude, and the z axis at the
**  +ve latitude pole.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   float cosb;

   cosb = (float) cos ( b );
   v[0] = (float) cos ( a ) * cosb;
   v[1] = (float) sin ( a ) * cosb;
   v[2] = (float) sin ( b );
}
