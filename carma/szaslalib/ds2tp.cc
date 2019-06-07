#include "slalib.h"
#include "slamac.h"
void slaDs2tp ( double ra, double dec, double raz, double decz,
                double *xi, double *eta, int *j )
/*
**  - - - - - - - - -
**   s l a D s 2 t p
**  - - - - - - - - -
**
**  Projection of spherical coordinates onto tangent plane
**  ('gnomonic' projection - 'standard coordinates').
**
**  (double precision)
**
**  Given:
**     ra,dec      double   spherical coordinates of point to be projected
**     raz,decz    double   spherical coordinates of tangent point
**
**  Returned:
**     *xi,*eta    double   rectangular coordinates on tangent plane
**     *j          int      status:   0 = OK, star on tangent plane
**                                    1 = error, star too far from axis
**                                    2 = error, antistar on tangent plane
**                                    3 = error, antistar too far from axis
**
**  Last revision:   18 July 1996
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
#define TINY 1e-6
{
   double sdecz, sdec, cdecz, cdec, radif, sradif, cradif, denom;


/* Trig functions */
   sdecz = sin ( decz );
   sdec = sin ( dec );
   cdecz = cos ( decz );
   cdec = cos ( dec );
   radif = ra - raz;
   sradif = sin ( radif );
   cradif = cos ( radif );

/* Reciprocal of star vector length to tangent plane */
   denom = sdec * sdecz + cdec * cdecz * cradif;

/* Handle vectors too far from axis */
   if ( denom > TINY ) {
      *j = 0;
   } else if ( denom >= 0.0 ) {
      *j = 1;
      denom = TINY;
   } else if ( denom > -TINY ) {
      *j = 2;
      denom = -TINY;
   } else {
      *j = 3;
   }

/* Compute tangent plane coordinates (even in dubious cases) */
   *xi = cdec * sradif / denom;
   *eta = ( sdec * cdecz - cdec * sdecz * cradif ) / denom;
}
