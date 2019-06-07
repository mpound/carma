#include "slalib.h"
#include "slamac.h"
void slaGe50 ( double dl, double db, double *dr, double *dd )
/*
**  - - - - - - - -
**   s l a G e 5 0
**  - - - - - - - -
**
**  Transformation from IAU 1958 Galactic coordinates to
**  B1950.0 'FK4' equatorial coordinates.
**
**  (double precision)
**
**  Given:
**     dl,db       double       Galactic longitude and latitude l2,b2
**
**  Returned:
**     *dr,*dd     double       B1950.0 'FK4' RA,Dec
**
**  (all arguments are radians)
**
**  Called:
**     slaDcs2c, slaDimxv, slaDcc2s, slaAddet, slaDranrm, slaDrange
**
**  Note:
**     The equatorial coordinates are B1950.0 'FK4'.  Use the
**     routine slaGaleq if conversion to J2000.0 coordinates
**     is required.
**
**  Reference:
**     Blaauw et al, Mon.Not.R.astron.Soc.,121,123 (1960)
**
**  Last revision:   8 December 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double v1[3], v2[3], r, d, re, de;
/*
**  l2,b2 system of Galactic coordinates
**
**  p = 192.25       RA of Galactic north pole (mean B1950.0)
**  q =  62.6        inclination of Galactic to mean B1950.0 equator
**  r =  33          longitude of ascending node
**
**  p,q,r are degrees
**
**  Equatorial to Galactic rotation matrix
**
**  The Euler angles are p, q, 90-r, about the z then y then
**  z axes.
**
**         +cp.cq.sr-sp.cr     +sp.cq.sr+cp.cr     -sq.sr
**
**         -cp.cq.cr-sp.sr     -sp.cq.cr+cp.sr     +sq.cr
**
**         +cp.sq              +sp.sq              +cq
*/
   static double rmat[3][3] =
   {
      { -0.066988739415, -0.872755765852, -0.483538914632 },
      {  0.492728466075, -0.450346958020,  0.744584633283 },
      { -0.867600811151, -0.188374601723,  0.460199784784 }
   };


/* Spherical to Cartesian */
   slaDcs2c ( dl, db, v1 );

/* Rotate to mean B1950.0 */
   slaDimxv ( rmat, v1, v2 );

/* Cartesian to spherical */
   slaDcc2s ( v2, &r, &d );

/* Introduce e-terms */
   slaAddet ( r, d, 1950.0, &re, &de );

/* Express in conventional ranges */
   *dr = slaDranrm ( re );
   *dd = slaDrange ( de );
}
