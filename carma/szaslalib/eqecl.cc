#include "slalib.h"
#include "slamac.h"
void slaEqecl ( double dr, double dd, double date,
                double *dl, double *db )
/*
**  - - - - - - - - -
**   s l a E q e c l
**  - - - - - - - - -
**
**  Transformation from J2000.0 equatorial coordinates to
**  ecliptic coordinates.
**
**  (double precision)
**
**  Given:
**     dr,dd       double      J2000.0 mean RA,Dec (radians)
**     date        double      TDB (loosely ET) as Modified Julian Date
**                                              (JD-2400000.5)
**  Returned:
**     *dl,*db     double      ecliptic longitude and latitude
**                             (mean of date, IAU 1980 theory, radians)
**
**
**  Called:
**     slaDcs2c, slaPrec, slaEpj, slaDmxv, slaEcmat, slaDcc2s,
**     slaDranrm, slaDrange
**
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double rmat[3][3], v1[3], v2[3];

/* Spherical to Cartesian */
   slaDcs2c ( dr, dd, v1 );

/* Mean J2000 to mean of date */
   slaPrec ( 2000.0, slaEpj ( date ), rmat );
   slaDmxv ( rmat, v1, v2 );

/* Equatorial to ecliptic */
   slaEcmat ( date, rmat );
   slaDmxv ( rmat, v2, v1 );

/* Cartesian to spherical */
   slaDcc2s ( v1, dl, db );

/* Express in conventional ranges */
   *dl = slaDranrm ( *dl );
   *db = slaDrange ( *db );
}
