#include "slalib.h"
#include "slamac.h"
void slaMapqkz ( double rm, double dm, double amprms[21],
                 double *ra, double *da )
/*
**  - - - - - - - - - -
**   s l a M a p q k z
**  - - - - - - - - - -
**
**  Quick mean to apparent place:  transform a star RA,dec from
**  mean place to geocentric apparent place, given the
**  star-independent parameters, and assuming zero parallax
**  and proper motion.
**
**  Use of this routine is appropriate when efficiency is important
**  and where many star positions, all with parallax and proper
**  motion either zero or already allowed for, and all referred to
**  the same equator and equinox, are to be transformed for one
**  epoch.  The star-independent parameters can be obtained by
**  calling the slaMappa routine.
**
**  The corresponding routine for the case of non-zero parallax
**  and proper motion is slaMapqk.
**
**  The reference frames and timescales used are post IAU 1976.
**
**  Given:
**     rm,dm    double      mean RA,dec (rad)
**     amprms   double[21]  star-independent mean-to-apparent parameters:
**
**       (0-3)    not used
**       (4-6)    heliocentric direction of the Earth (unit vector)
**       (7)      (grav rad Sun)*2/(Sun-Earth distance)
**       (8-10)   abv: barycentric Earth velocity in units of c
**       (11)     sqrt(1-v**2) where v=modulus(abv)
**       (12-20)  precession/nutation (3,3) matrix
**
**  Returned:
**     *ra,*da  double      apparent RA,dec (rad)
**
**  References:
**     1984 Astronomical Almanac, pp B39-B41.
**     (also Lederle & Schwan, Astron. Astrophys. 134,
**      1-6, 1984)
**
**  Notes:
**
**    1)  The vectors amprms(1-3) and amprms(4-6) are referred to the
**        mean equinox and equator of epoch eq.
**
**    2)  Strictly speaking, the routine is not valid for solar-system
**        sources, though the error will usually be extremely small.
**        However, to prevent gross errors in the case where the
**        position of the Sun is specified, the gravitational
**        deflection term is restrained within about 920 arcsec of the
**        centre of the Sun's disc.  The term has a maximum value of
**        about 1.85 arcsec at this radius, and decreases to zero as
**        the centre of the disc is approached.
**
**  Called:
**     slaDcs2c       spherical to Cartesian
**     slaDvdv        dot product
**     slaDmxv        matrix x vector
**     slaDcc2s       Cartesian to spherical
**     slaDranrm      normalize angle 0-2pi
**
**  Last revision:   12 June 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   long i;
   double gr2e, ab1, ehn[3], abv[3], p[3], pde, pdep1,
          w, p1[3], p1dv, p1dvp1, p2[3], p3[3];


/* Unpack scalar and vector parameters */
   gr2e = amprms[7];
   ab1 = amprms[11];
   for ( i = 0; i < 3; i++ ) {
      ehn[i] = amprms[i+4];
      abv[i] = amprms[i+8];
   }

/* Spherical to x,y,z */
   slaDcs2c ( rm, dm, p );

/* Light deflection */
   pde = slaDvdv ( p, ehn );
   pdep1 = pde + 1.0;
   w = gr2e / gmax ( pdep1, 1e-5 );
   for ( i = 0; i < 3; i++ ) {
      p1[i] = p[i] + w * ( ehn[i] - pde * p[i] );
   }

/* Aberration */
   p1dv = slaDvdv ( p1, abv );
   p1dvp1 = p1dv + 1.0;
   w = 1.0 + p1dv / ( ab1 + 1.0 );
   for ( i = 0; i < 3; i++ ) {
      p2[i] = ( ( ab1 * p1[i] ) + ( w * abv[i] ) ) / p1dvp1;
   }

/* Precession and nutation */
   slaDmxv ( (double(*)[3]) &amprms[12], p2, p3 );

/* Geocentric apparent RA,dec */
   slaDcc2s ( p3, ra, da );
   *ra = slaDranrm ( *ra );
}
