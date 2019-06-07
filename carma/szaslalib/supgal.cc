#include "slalib.h"
#include "slamac.h"
void slaSupgal ( double dsl, double dsb, double *dl, double *db )
/*
**  - - - - - - - - - -
**   s l a S u p g a l
**  - - - - - - - - - -
**
**  Transformation from De Vaucouleurs supergalactic coordinates
**  to IAU 1958 Galactic coordinates.
**
**  (double precision)
**
**  Given:
**     dsl,dsb     double      supergalactic longitude and latitude
**
**  Returned:
**     *dl,*db     double      Galactic longitude and latitude l2,b2
**
**  (all arguments are radians)
**
**  Called:
**     slaDcs2c, slaDimxv, slaDcc2s, slaDranrm, slaDrange
**
**  References:
**
**     De Vaucouleurs, De Vaucouleurs, & Corwin, Second Reference
**     Catalogue of Bright Galaxies, U. Texas, page 8.
**
**     Systems & Applied Sciences Corp., Documentation for the
**     machine-readable version of the above catalogue,
**     contract NAS 5-26490.
**
**    (These two references give different values for the Galactic
**     longitude of the supergalactic origin.  Both are wrong;  the
**     correct value is l2=137.37.)
**
**  Last revision:   8 December 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double v1[3], v2[3];
/*
**  System of supergalactic coordinates:
**
**    sgl   sgb        l2     b2      (deg)
**     -    +90      47.37  +6.32
**     0     0         -      0
**
**  Galactic to supergalactic rotation matrix:
*/
   static double rmat[3][3] =
   {
      { -0.735742574804,  0.677261296414,  0.0            },
      { -0.074553778365, -0.080991471307,  0.993922590400 },
      {  0.673145302109,  0.731271165817,  0.110081262225 }
   };

/* Spherical to Cartesian */
   slaDcs2c ( dsl, dsb, v1 );

/* Supergalactic to Galactic */
   slaDimxv ( rmat, v1, v2 );

/* Cartesian to spherical */
   slaDcc2s ( v2, dl, db );

/* Express in conventional ranges */
   *dl = slaDranrm ( *dl );
   *db = slaDrange ( *db );
}
