#include "slalib.h"
#include "slamac.h"
void slaEg50 ( double dr, double dd, double *dl, double *db )
/*
**  - - - - - - - -
**   s l a E g 5 0
**  - - - - - - - -
**
**  Transformation from B1950.0 'FK4' equatorial coordinates to
**  IAU 1958 Galactic coordinates.
**
**  (double precision)
**
**  Given:
**     dr,dd       double       B1950.0 'FK4' RA,dec
**
**  Returned:
**     *dl,*db     double       Galactic longitude and latitude l2,b2
**
**  (all arguments are radians)
**
**  Called:
**     slaDcs2c, slaDmxv, slaDcc2s, slaSubet, slaDranrm, slaDrange
**
**  Note:
**     The equatorial coordinates are B1950.0 'FK4'.  Use the
**     routine slaEqgal if conversion from J2000.0 coordinates
**     is required.
**
**  Reference:
**     Blaauw et al, Mon.Not.R.astron.Soc.,121,123 (1960)
**
**  Last revision:   16 November 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double v1[3], v2[3], r, d;

/*
** l2,b2 system of Galactic coordinates
**
** p = 192.25       RA of Galactic north pole (mean B1950.0)
** q =  62.6        inclination of Galactic to mean B1950.0 equator
** r =  33          longitude of ascending node
**
** p,q,r are degrees
**
** Equatorial to Galactic rotation matrix
**
** The Euler angles are p, q, 90-r, about the z then y then
** z axes.
**
**        +cp.cq.sr-sp.cr     +sp.cq.sr+cp.cr     -sq.sr
**
**        -cp.cq.cr-sp.sr     -sp.cq.cr+cp.sr     +sq.cr
**
**        +cp.sq              +sp.sq              +cq
*/

   static double rmat[3][3];

   rmat[0][0] = -0.066988739415;
   rmat[0][1] = -0.872755765852;
   rmat[0][2] = -0.483538914632;
   rmat[1][0] =  0.492728466075;
   rmat[1][1] = -0.450346958020;
   rmat[1][2] =  0.744584633283;
   rmat[2][0] = -0.867600811151;
   rmat[2][1] = -0.188374601723;
   rmat[2][2] =  0.460199784784;


/* Remove e-terms */
   slaSubet ( dr, dd, 1950.0, &r, &d );

/* Spherical to Cartesian */
   slaDcs2c ( r, d, v1 );

/* Rotate to Galactic */
   slaDmxv ( rmat, v1, v2 );

/* Cartesian to spherical */
   slaDcc2s ( v2, dl, db );

/* Express angles in conventional ranges */
   *dl = slaDranrm ( *dl );
   *db = slaDrange ( *db );
}
