#include "slalib.h"
#include "slamac.h"
void slaAddet ( double rm, double dm, double eq, double *rc, double *dc )
/*
**  - - - - - - - - -
**   s l a A d d e t
**  - - - - - - - - -
**
**  Add the e-terms (elliptic component of annual aberration) to a
**  pre IAU 1976 mean place to conform to the old catalogue convention.
**
**  Given:
**     rm,dm     double     RA,Dec (radians) without e-terms
**     eq        double     Besselian epoch of mean equator and equinox
**
**  Returned:
**     *rc,*dc   double     RA,dec (radians) with e-terms included
**
**  Called:
**     slaEtrms, slaDcs2c, slaDcc2s, slaDranrm, slaDrange
**
**  Explanation:
**     Most star positions from pre-1984 optical catalogues (or
**     derived from astrometry using such stars) embody the
**     e-terms.  If it is necessary to convert a formal mean
**     place (for example a pulsar timing position) to one
**     consistent with such a star catalogue, then the RA,Dec
**     should be adjusted using this routine.
**
**  Reference:
**     Explanatory Supplement to the Astronomical Almanac,
**     ed P.K.Seidelmann (1992), page 169.
**
**  Last revision:   25 July 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double a[3];    /* Elliptic components of annual aberration vector */
   double v[3];    /* Cartesian equivalant of RA,Dec */
   int i;


/* E-terms vector */
   slaEtrms ( eq, a );

/* Spherical to Cartesian */
   slaDcs2c ( rm, dm, v );

/* Include the e-terms */
   for ( i=0; i < 3; i++ ) {
      v[i] += a[i];
   }

/* Cartesian to spherical */
   slaDcc2s ( v, rc, dc );

/* Bring RA into conventional range */
   *rc = slaDranrm ( *rc );
}
