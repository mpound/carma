#include "slalib.h"
#include "slamac.h"
float slaRvlsrd ( float r2000, float d2000 )
/*
**  - - - - - - - - - -
**   s l a R v l s r d
**  - - - - - - - - - -
**
**  Velocity component in a given direction due to the Sun's
**  motion with respect to the dynamical Local Standard of Rest.
**
**  (single precision)
**
**  Given:
**     r2000,d2000   float    J2000.0 mean RA,Dec (radians)
**
**  Result:
**     Component of "peculiar" solar motion in direction R2000,D2000 (km/s)
**
**  Sign convention:
**     The result is +ve when the Sun is receding from the given point on
**     the sky.
**
**  Note:  The Local Standard of Rest used here is the "dynamical" LSR,
**         a point in the vicinity of the Sun which is in a circular
**         orbit around the Galactic centre.  The Sun's motion with
**         respect to the dynamical LSR is called the "peculiar" solar
**         motion.
**
**         There is another type of LSR, called a "kinematical" LSR.  A
**         kinematical LSR is the mean standard of rest of specified star
**         catalogues or stellar populations, and several slightly
**         different kinematical LSRs are in use.  The Sun's motion with
**         respect to an agreed kinematical LSR is known as the "standard"
**         solar motion.  To obtain a radial velocity correction with
**         respect to an adopted kinematical LSR use the routine slaRvlsrk.
**
**  Reference:  Delhaye (1965), in "Stars and Stellar Systems", vol 5, p73.
**
**  Called:  slaCs2c, slaVdv
**
**  Last revision:   11 March 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/*
**  Peculiar solar motion from Delhaye 1965: in Galactic Cartesian
**  coordinates (+9,+12,+7) km/s.  This corresponds to about 16.6 km/s
**  towards Galactic coordinates L2 = 53 deg, B2 = +25 deg, or RA,Dec
**  17 49 58.7 +28 07 04 J2000.
**
**  The solar motion is expressed here in the form of a J2000.0
**  equatorial Cartesian vector:
**
**      va(1) = x = -speed*cos(ra)*cos(dec)
**      va(2) = y = -speed*sin(ra)*cos(dec)
**      va(3) = z = -speed*sin(dec)
*/
   static float va[3] = { 0.63823f, 14.58542f, -7.80116f };
   float vb[3];

/* Convert given J2000 RA,dec to x,y,z */
   slaCs2c ( r2000, d2000, vb );

/* Compute dot product with solar motion vector */
   return slaVdv ( va, vb );
}
