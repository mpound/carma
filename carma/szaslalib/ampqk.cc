#include "slalib.h"
#include "slamac.h"
void slaAmpqk ( double ra, double da, double amprms[21],
                double *rm, double *dm )
/*
**  - - - - - - - - -
**   s l a A m p q k
**  - - - - - - - - -
**
**  Convert star RA,Dec from geocentric apparent to mean place.
**
**  The mean coordinate system is the post IAU 1976 system,
**  loosely called FK5.
**
**  Use of this routine is appropriate when efficiency is important
**  and where many star positions are all to be transformed for
**  one epoch and equinox.  The star-independent parameters can be
**  obtained by calling the slaMappa routine.
**
**  Given:
**     ra       double      apparent RA (radians)
**     da       double      apparent Dec (radians)
**
**     amprms   double[21]  star-independent mean-to-apparent parameters:
**
**       (0)      time interval for proper motion (Julian years)
**       (1-3)    barycentric position of the Earth (AU)
**       (4-6)    heliocentric direction of the Earth (unit vector)
**       (7)      (grav rad Sun)*2/(Sun-Earth distance)
**       (8-10)   abv: barycentric Earth velocity in units of c
**       (11)     sqrt(1-v*v) where v=modulus(abv)
**       (12-20)  precession/nutation (3,3) matrix
**
**  Returned:
**     *rm      double      mean RA (radians)
**     *dm      double      mean Dec (radians)
**
**  References:
**     1984 Astronomical Almanac, pp B39-B41.
**     (also Lederle & Schwan, Astron. Astrophys. 134, 1-6, 1984)
**
**  Notes:
**
**  1)  The accuracy is limited by the routine slaEvp, called
**      by slaMappa, which computes the Earth position and
**      velocity using the methods of Stumpff.  The maximum
**      error is about 0.3 milliarcsecond.
**
**  2)  Iterative techniques are used for the aberration and
**      light deflection corrections so that the routines
**      slaAmp (or slaAmpqk) and slaMap (or slaMapqk) are
**      accurate inverses;  even at the edge of the Sun's disc
**      the discrepancy is only about 1 nanoarcsecond.
**
**  Called:  slaDcs2c, slaDimxv, slaDvdv, slaDvn, slaDcc2s,
**           slaDranrm
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double gr2e;    /* (grav rad Sun)*2/(Sun-Earth distance) */
   double ab1;     /* sqrt(1-v*v) where v=modulus of Earth vel */
   double ehn[3];  /* Earth position wrt Sun (unit vector, FK5) */
   double abv[3];  /* Earth velocity wrt SSB (c, FK5) */
   double p[3], p1[3], p2[3], p3[3];  /* work vectors */
   double ab1p1, p1dv, p1dvp1, w, pde, pdep1;
   int i, j;

/* Unpack some of the parameters */
   gr2e = amprms[7];
   ab1  = amprms[11];
   for ( i = 0; i < 3; i++ ) {
      ehn[i] = amprms[i + 4];
      abv[i] = amprms[i + 8];
   }

/* Apparent RA,Dec to Cartesian */
   slaDcs2c ( ra, da, p3 );

/* Precession and nutation */
   slaDimxv ( (double(*)[3]) &amprms[12], p3, p2 );

/* Aberration */
   ab1p1 = ab1 + 1.0;
   for ( i = 0; i < 3; i++ ) {
      p1[i] = p2[i];
   }
   for ( j = 0; j < 2; j++ ) {
      p1dv = slaDvdv ( p1, abv );
      p1dvp1 = 1.0 + p1dv;
      w = 1.0 + p1dv / ab1p1;
      for ( i = 0; i < 3; i++ ) {
         p1[i] = ( p1dvp1 * p2[i] - w * abv[i] ) / ab1;
      }
      slaDvn ( p1, p3, &w );
      for ( i = 0; i < 3; i++ ) {
         p1[i] = p3[i];
      }
   }

/* Light deflection */
   for ( i = 0; i < 3; i++ ) {
      p[i] = p1[i];
   }
   for ( j = 0; j < 5; j++ ) {
      pde = slaDvdv ( p, ehn );
      pdep1 = 1.0 + pde;
      w = pdep1 - gr2e * pde;
      for ( i = 0; i < 3; i++ ) {
         p[i] = ( pdep1 * p1[i] - gr2e * ehn[i] ) / w;
      }
      slaDvn ( p, p2, &w );
      for ( i = 0; i < 3; i++ ) {
         p[i] = p2[i];
      }
   }

/* Mean RA,Dec */
   slaDcc2s ( p, rm, dm );
   *rm = slaDranrm ( *rm );
}
