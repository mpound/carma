#include "slalib.h"
#include "slamac.h"
void slaEqgal ( double dr, double dd, double *dl, double *db )
/*
**  - - - - - - - - -
**   s l a E q g a l
**  - - - - - - - - -
**
**  Transformation from J2000.0 equatorial coordinates to
**  IAU 1958 Galactic coordinates.
**
**  (double precision)
**
**  Given:
**     dr,dd       double       J2000.0 RA,Dec
**
**  Returned:
**     *dl,*db     double       Galactic longitude and latitude l2,b2
**
**  (all arguments are radians)
**
**  Called:
**     slaDcs2c, slaDmxv, slaDcc2s, slaDranrm, slaDrange
**
**  Note:
**     The equatorial coordinates are J2000.0.  Use the routine
**     slaEg50 if conversion from B1950.0 'FK4' coordinates is
**     required.
**
**  Reference:
**     Blaauw et al, Mon.Not.R.astron.Soc.,121,123 (1960)
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double v1[3], v2[3];

/*
**  l2,b2 system of Galactic coordinates
**
**  p = 192.25       RA of Galactic north pole (mean B1950.0)
**  q =  62.6        inclination of Galactic to mean B1950.0 equator
**  r =  33          longitude of ascending node
**
**  p,q,r are degrees
**
**  Equatorial to Galactic rotation matrix (J2000.0), obtained by
**  applying the standard FK4 to FK5 transformation, for inertially
**  zero proper motion, to the columns of the B1950 equatorial to
**  Galactic rotation matrix:
*/
   static double rmat[3][3];

   rmat[0][0] = -0.054875539726;
   rmat[0][1] = -0.873437108010;
   rmat[0][2] = -0.483834985808;
   rmat[1][0] =  0.494109453312;
   rmat[1][1] = -0.444829589425;
   rmat[1][2] =  0.746982251810;
   rmat[2][0] = -0.867666135858;
   rmat[2][1] = -0.198076386122;
   rmat[2][2] =  0.455983795705;

/* Spherical to Cartesian */
   slaDcs2c ( dr, dd, v1 );

/* Equatorial to Galactic */
   slaDmxv ( rmat, v1, v2 );

/* Cartesian to spherical */
   slaDcc2s ( v2, dl, db );

/* Express in conventional ranges */
   *dl = slaDranrm ( *dl );
   *db = slaDrange ( *db );
}
