#include "slalib.h"
#include "slamac.h"
float slaRvlg ( float r2000, float d2000 )
/*
**  - - - - - - - -
**   s l a R v l g
**  - - - - - - - -
**
**  Velocity component in a given direction due to the combination
**  of the rotation of the Galaxy and the motion of the Galaxy
**  relative to the mean motion of the local group.
**
**  (single precision)
**
**  Given:
**     r2000,d2000   float    J2000.0 mean RA,Dec (radians)
**
**  Result:
**     Component of solar motion in direction r2000,d2000 (km/s)
**
**  Sign convention:
**     The result is +ve when the Sun is receding from the
**     given point on the sky.
**
**  Reference:
**     IAU trans 1976, 168, p201.
**
**  Called:
**     slaCs2c, slaVdv
**
**  Last revision:   15 July 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/*
**  Solar velocity due to galactic rotation and translation
**
**  speed = 300 km/s
**
**  apex  = l2,b2  90deg, 0deg
**        = RA,dec  21 12 01.1  +48 19 47  J2000.0
**
**  This is expressed in the form of a J2000.0 x,y,z vector:
**
**      va(1) = x = -speed*cos(ra)*cos(dec)
**      va(2) = y = -speed*sin(ra)*cos(dec)
**      va(3) = z = -speed*sin(dec)
*/
   static float va[3] = { -148.23284f, 133.44888f, -224.09467f };
   float vb[3];

/* Convert given J2000 RA,dec to x,y,z */
   slaCs2c ( r2000, d2000, vb );

/* Compute dot product with solar motion vector */
   return slaVdv ( va, vb);
}
