#include "slalib.h"
#include "slamac.h"
void slaEarth ( int iy, int id, float fd, float pv[6] )
/*
**  - - - - - - - - -
**   s l a E a r t h
**  - - - - - - - - -
**
**  Approximate heliocentric position and velocity of the Earth.
**
**  (single precision)
**
**  Given:
**     iy       int        year
**     id       int        day in year (1 = Jan 1st)
**     fd       float      fraction of day
**
**  Returned:
**     pv       float[6]   Earth position & velocity vector
**
**  Notes:
**
**  1  The date and time is TDB (loosely ET) in a Julian calendar
**     which has been aligned to the ordinary Gregorian
**     calendar for the interval 1900 March 1 to 2100 February 28.
**     The year and day can be obtained by calling slaCalyd or
**     slaClyd.
**
**  2  The Earth heliocentric 6-vector is mean equator and equinox
**     of date.  Position part, PV(1-3), is in AU;  velocity part,
**     PV(4-6), is in AU/sec.
**
**  3  Max/RMS errors 1950-2050:
**       13/5 E-5 AU = 19200/7600 km in position
**       47/26 E-10 AU/s = 0.0070/0.0039 km/s in speed
**
**  4  More precise results are obtainable with the routine slaEvp.
**
**  Defined in slamac.h:  D2PI, dmod
**
**  Last revision:   25 April 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define SPEED 1.9913e-7f    /* Mean orbital speed of Earth, AU/s */
#define REMB  3.12e-5f      /* Mean Earth:EMB distance, AU       */
#define SEMB  8.31e-11f     /* Mean Earth:EMB speed, AU/s        */

{
   float yi, yf, t, elm, gam, em, elt, eps0,
         e, esq, v, r, elmm, coselt, sineps, coseps, w1, w2, selmm, celmm;
   int iy4;

/* Whole years & fraction of year, and years since J1900.0 */
   yi = (float) ( iy - 1900 );
   iy4 = iy >= 0 ? iy % 4 : 3 - ( - iy - 1 ) % 4;
   yf = ( (float) ( 4 * ( id - 1 / ( iy4 + 1 ) )
                    - iy4 - 2 ) + 4.0f * fd ) / 1461.0f;
   t = yi + yf;

/* Geometric mean longitude of Sun */
/* (cf 4.881627938+6.283319509911*t mod 2pi) */
   elm = (float) dmod ( 4.881628
                        + D2PI * ( (double) yf ) + 0.00013420 * ( (double) t ),
                        D2PI );

/* Mean longitude of perihelion */
   gam = 4.908230f + 3.0005e-4f * t;

/* Mean anomaly */
   em = elm - gam;

/* Mean obliquity */
   eps0 = 0.40931975f - 2.27e-6f * t;

/* Eccentricity */
   e = 0.016751f - 4.2e-7f * t;
   esq = (float) ( e * e );

/* True anomaly */
   v = em + 2.0f * e * (float) sin ( (double) em )
          + 1.25f * esq * (float) sin ( 2.0 * (double) em );

/* True ecliptic longitude */
   elt = v + gam;

/* True distance */
   r = ( 1.0f - esq ) / ( 1.0f + e * (float) cos ( (double) v ) );

/* Moon's mean longitude */
   elmm = (float) dmod ( ( 4.72 + 83.9971 * ( (double) t ) ), D2PI );

/* Useful functions */
   coselt = (float) cos ( (double) elt );
   sineps = (float) sin ( (double) eps0 );
   coseps = (float) cos ( (double) eps0 );
   w1 = -r * (float) sin ( (double) elt );
   w2 = -SPEED * ( coselt + e * (float) cos ( (double) gam ) );
   selmm = (float) sin ( (double) elmm );
   celmm = (float) cos ( (double) elmm );

/* Earth position and velocity */
   pv[0] = - r * coselt - REMB * celmm;
   pv[1] = ( w1 - REMB * selmm ) * coseps;
   pv[2] = w1 * sineps;
   pv[3] = SPEED * ( (float) sin ( (double) elt ) +
                 e * (float) sin ( (double) gam ) ) + SEMB * selmm;
   pv[4] = ( w2 - SEMB * celmm ) * coseps;
   pv[5] = w2 * sineps;
}
