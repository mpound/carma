#include "slalib.h"
#include "slamac.h"
void slaSubet ( double rc, double dc, double eq, double *rm, double *dm )
/*
**  - - - - - - - - -
**   s l a S u b e t
**  - - - - - - - - -
**
**  Remove the e-terms (elliptic component of annual aberration)
**  from a pre IAU 1976 catalogue RA,Dec to give a mean place.
**
**  (double precision)
**
**  Given:
**     rc,dc     double     RA,Dec (radians) with e-terms included
**     eq        double     Besselian epoch of mean equator and equinox
**
**  Returned:
**     *rm,*dm   double     RA,Dec (radians) without e-terms
**
**  Called:
**     slaEtrms, slaDcs2c, sla,dvdv, slaDcc2s, slaDranrm
**
**  Explanation:
**     Most star positions from pre-1984 optical catalogues (or
**     derived from astrometry using such stars) embody the
**     e-terms.  This routine converts such a position to a
**     formal mean place (allowing, for example, comparison with a
**     pulsar timing position).
**
**  Reference:
**     Explanatory Supplement to the Astronomical Ephemeris,
**     section 2D, page 48.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double a[3], v[3], f;

   int i;

/* E-terms */
   slaEtrms ( eq, a );

/* Spherical to Cartesian */
   slaDcs2c ( rc, dc, v );

/* Include the e-terms */
   f = 1.0 + slaDvdv (v, a);
   for ( i = 0; i < 3; i++ ) {
      v[i] = f * v[i] - a[i];
   }

/* Cartesian to spherical */
   slaDcc2s ( v, rm, dm );

/* Bring RA into conventional range */
   *rm = slaDranrm ( *rm );
}
