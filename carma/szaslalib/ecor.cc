#include "slalib.h"
#include "slamac.h"
void slaEcor ( float rm, float dm, int iy, int id, float fd,
               float *rv, float *tl )
/*
**  - - - - - - - -
**   s l a E c o r
**  - - - - - - - -
**
**  Component of Earth orbit velocity and heliocentric
**  light time in a given direction.
**
**  (single precision)
**
**  Given:
**     rm,dm    float    mean RA,Dec of date (radians)
**     iy       int      year
**     id       int      day in year (1 = Jan 1st)
**     fd       float    fraction of day
**
**  Returned:
**     *rv      float    component of Earth orbital velocity (km/sec)
**     *tl      float    component of heliocentric light time (sec)
**
**  Notes:
**
**  1  The date and time is TDB (loosely ET) in a Julian calendar
**     which has been aligned to the ordinary Gregorian
**     calendar for the interval 1900 March 1 to 2100 February 28.
**     The year and day can be obtained by calling slaCalyd or
**     slaClyd.
**
**  2  Sign convention:
**
**     The velocity component is +ve when the Earth is receding from
**     the given point on the sky.  The light time component is +ve
**     when the Earth lies between the Sun and the given point on
**     the sky.
**
**  3  Accuracy:
**
**     The velocity component is usually within 0.004 km/s of the
**     correct value and is never in error by more than 0.007 km/s.
**     The error in light time correction is about 0.03s at worst,
**     but is usually better than 0.01s. For applications requiring
**     higher accuracy, see the slaEvp routine.
**
**  Called:  slaEarth, slaCs2c, slaVdv
**
**  Last revision:   24 November 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define AUKM  1.4959787066e8f   /* AU to km (1985 Almanac) */
#define AUSEC 499.0047837f      /* AU to light sec */

{
   float pv[6], v[3];

/* Sun:Earth position & velocity vector */
   slaEarth ( iy, id, fd, pv );

/* Star position vector */
   slaCs2c ( rm, dm, v );

/* Velocity component */
   *rv = - AUKM * slaVdv ( &pv[3], v );

/* Light time component */
   *tl = AUSEC * slaVdv ( pv, v );
}
