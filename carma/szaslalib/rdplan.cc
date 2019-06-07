#include "slalib.h"
#include "slamac.h"
void slaRdplan ( double date, int np, double elong, double phi,
                 double *ra, double *dec, double *diam )
/*
**  - - - - - - - - - -
**   s l a R d p l a n
**  - - - - - - - - - -
**
**  Approximate topocentric apparent RA,Dec of a planet, and its
**  angular diameter.
**
**  Given:
**     date        double     MJD of observation (JD - 2400000.5)
**     np          int        planet: 1 = Mercury
**                                    2 = Venus
**                                    3 = Moon
**                                    4 = Mars
**                                    5 = Jupiter
**                                    6 = Saturn
**                                    7 = Uranus
**                                    8 = Neptune
**                                 else = Sun
**     elong,phi   double     observer's east longitude and geodetic
**                                                  latitude (radians)
**
**  Returned:
**     ra,dec      double     RA, Dec (topocentric apparent, radians)
**     diam        double     angular diameter (equatorial, radians)
**
**  Notes:
**
**  1  The date is in a dynamical timescale (TDB, formerly ET) and is
**     in the form of a Modified Julian Date (JD-2400000.5).  For all
**     practical purposes, TT can be used instead of TDB, and for many
**     applications UT will do (except for the Moon).
**
**  2  The longitude and latitude allow correction for geocentric
**     parallax.  This is a major effect for the Moon, but in the
**     context of the limited accuracy of the present routine its
**     effect on planetary positions is small (negligible for the
**     outer planets).  Geocentric positions can be generated by
**     calls to the routines slaEvp, slaDmoon and slaPlanet.
**
**  3  The direction accuracy (arcsec, 1000-3000AD) is of order:
**
**            Sun              5
**            Mercury          2
**            Venus           10
**            Moon            30
**            Mars            50
**            Jupiter         90
**            Saturn          90
**            Uranus          90
**            Neptune         10
**
**     The angular diameter accuracy is about 0.4% for the Moon,
**     and 0.01% or better for the Sun and planets.
**
**  Called: slaGmst, slaDt, slaEpj, slaDmoon, slaPvobs, slaPrenut,
**          slaPlanet, slaDmxv, slaDcc2s, slaDranrm
**
**  Last revision:   5 March 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define AUKM 1.49597870e8    /* AU in km */

{
   int ip, j, i;
   double stl, vgm[6], v[6], rmat[3][3], vse[6], vsg[6], vsp[6],
          vgo[6], dx, dy, dz, r, tl;

/* Equatorial radii (km) */
   static double eqrau[9] = {
      696000.0,          /* Sun     */
      2439.7,            /* Mercury */
      6051.9,            /* Venus   */
      1738.0,            /* Moon    */
      3397.0,            /* Mars    */
      71492.0,           /* Jupiter */
      60268.0,           /* Saturn  */
      25559.0,           /* Uranus  */
      24764.0            /* Neptune */
   };



/* Classify NP */
   ip = ( np >= 1 && np <= 8 ) ? np : 0;

/* Approximate local ST */
   stl = slaGmst ( date - slaDt ( slaEpj ( date ) ) / 86400.0 ) + elong;

/* Geocentre to Moon (mean of date) */
   slaDmoon ( date, v );

/* Nutation, to true of date */
   slaNut ( date, rmat );
   slaDmxv ( rmat, &v[0], &vgm[0] );
   slaDmxv ( rmat, &v[3], &vgm[3] );

/* Moon? */
   if ( ip == 3 ) {

   /* Yes: geocentre to Moon (true of date) */
      for ( i = 0; i <= 5; i++ ) v[i] = vgm[i];

   } else {

   /* No: precession/nutation matrix, J2000 to date */
      slaPrenut ( 2000.0, date, rmat );

   /* Sun to Earth-Moon Barycentre (J2000) */
      slaPlanet ( date, 3, v, &j );

   /* Precession and nutation to date */
      slaDmxv ( rmat, &v[0], &vse[0] );
      slaDmxv ( rmat, &v[3], &vse[3] );

   /* Sun to geocentre */
      for ( i = 0; i <= 5; i++ ) vsg[i] = vse[i] - 0.012150581 * vgm[i];

   /* Sun? */
      if ( ip == 0 ) {

      /* Yes: geocentre to Sun */
         for ( i = 0; i <= 5; i++ ) v[i] = - vsg[i];

      } else {

      /* No: Sun to Planet */
         slaPlanet ( date, ip, v, &j );

      /* Precession and nutation to date */
         slaDmxv ( rmat, &v[0], &vsp[0] );
         slaDmxv ( rmat, &v[3], &vsp[3] );

      /* Geocentre to planet */
         for ( i = 0; i <= 5; i++ ) v[i] = vsp[i] - vsg[i];
      }
   }

/* Refer to origin at the observer */
   slaPvobs ( phi, 0.0, stl, vgo );
   for ( i = 0; i <= 5; i++ ) v[i] -= vgo[i];

/* Geometric distance (AU) */
   dx = v[0];
   dy = v[1];
   dz = v[2];
   r = sqrt ( dx * dx + dy * dy + dz * dz );

/* Light time (sec) */
   tl = 499.004782 * r;

/* Correct position for planetary aberration */
   for ( i = 0; i <= 2; i++ ) v[i] -= tl * v[i+3];

/* To RA,Dec */
   slaDcc2s ( v, ra, dec );
   *ra = slaDranrm ( *ra );

/* Angular diameter (radians) */
   *diam = 2.0 * asin ( eqrau[ip] / ( r * AUKM ) );
}