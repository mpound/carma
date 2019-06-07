#include "slalib.h"
#include "slamac.h"
float slaRvgalc ( float r2000, float d2000 )
/*
**  - - - - - - - - - -
**   s l a R v g a l c
**  - - - - - - - - - -
**
**  Velocity component in a given direction due to the rotation
**  of the Galaxy.
**
**  (single precision)
**
**  Given:
**     r2000,d2000   float    J2000.0 mean RA,Dec (radians)
**
**  Result:
**     Component of dynamical LSR motion in direction r2000,d2000 (km/s)
**
**  Sign convention:
**     The result is +ve when the dynamical LSR is receding from the
**     given point on the sky.
**
**  Called:
**     slaCs2c, slaVdv
**
**  Note:  The Local Standard of Rest used here is a point in the
**         vicinity of the Sun which is in a circular orbit around
**         the Galactic centre.  Sometimes called the "dynamical" LSR,
**         it is not to be confused with a "kinematical" LSR, which
**         is the mean standard of rest of star catalogues or stellar
**         populations.
**
**  Reference:  The orbital speed of 220 km/s used here comes from
**              Kerr & Lynden-Bell (1986), MNRAS, 221, p1023.
**
**  Last revision:   23 March 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/*
**
**  LSR velocity due to Galactic rotation
**
**  Speed = 220 km/s
**
**  Apex  = L2,B2  90deg, 0deg
**        = RA,Dec  21 12 01.1  +48 19 47  J2000.0
**
**  This is expressed in the form of a J2000.0 x,y,z vector:
**
**      va(1) = x = -speed*cos(ra)*cos(dec)
**      va(2) = y = -speed*sin(ra)*cos(dec)
**      va(3) = z = -speed*sin(dec)
*/
   static float va[3] = { -108.70408f, 97.86251f, -164.33610f };
   float vb[3];

/* Convert given J2000 RA,dec to x,y,z */
   slaCs2c ( r2000, d2000, vb );

/* Compute dot product with LSR motion vector */
   return slaVdv ( va, vb );
}
