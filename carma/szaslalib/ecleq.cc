#include "slalib.h"
#include "slamac.h"
void slaEcleq ( double dl, double db, double date,
                double *dr, double *dd)
/*
**  - - - - - - - - -
**   s l a E c l e q
**  - - - - - - - - -
**
**  Transformation from ecliptic coordinates to J2000.0
**  equatorial coordinates.
**
**  (double precision)
**
**  Given:
**     dl,db       double      ecliptic longitude and latitude
**                             (mean of date, IAU 1980 theory, radians)
**     date        double      TDB (loosely ET) as Modified Julian Date
**                                              (JD-2400000.5)
**  Returned:
**     *dr,*dd     double      J2000.0 mean RA,Dec (radians)
**
**  Called:
**     slaDcs2c, slaEcmat, slaDimxv, slaPrec, slaEpj, slaDcc2s,
**     slaDranrm, slaDrange
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double rmat[3][3], v1[3], v2[3];

/* Spherical to Cartesian */
   slaDcs2c ( dl, db, v1 );

/* Ecliptic to equatorial */
   slaEcmat ( date, rmat );
   slaDimxv ( rmat, v1, v2 );

/* Mean of date to J2000 */
   slaPrec ( 2000.0, slaEpj ( date ), rmat );
   slaDimxv ( rmat, v2, v1 );

/* Cartesian to spherical */
   slaDcc2s ( v1, dr, dd );

/* Express in conventional ranges */
   *dr = slaDranrm ( *dr );
   *dd = slaDrange ( *dd );
}
