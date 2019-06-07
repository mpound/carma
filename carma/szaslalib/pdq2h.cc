#include "slalib.h"
#include "slamac.h"
void slaPdq2h ( double p, double d, double q,
                double *h1, int *j1, double *h2, int *j2 )
/*
**  - - - - - - - - -
**   s l a P d q 2 h
**  - - - - - - - - -
**
**  Hour Angle corresponding to a given parallactic angle
**
**  (double precision)
**
**  Given:
**     p           double      latitude
**     d           double      declination
**     q           double      parallactic angle
**
**  Returned:
**     *h1         double      hour angle:  first solution if any
**     *j1         int         flag: 0 = solution 1 is valid
**     *h2         double      hour angle:  first solution if any
**     *j2         int         flag: 0 = solution 2 is valid
**
**  Called:  slaDrange
**
**  Defined in slamac.h:  DPI, DPIBY2
**
**  Last revision:   24 November 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define TINY 1e-12   /* Zone of avoidance around critical angles */

{
   double pn, qn, dn, sq, cq, sqsd, qt, qb, hpt, t;

/* Preset status flags to OK */
   *j1 = 0;
   *j2 = 0;

/* Adjust latitude, azimuth, parallactic angle to avoid critical values */
   pn = slaDrange ( p );
   if ( fabs ( fabs ( pn ) - DPIBY2 ) < TINY ) {
      pn -= dsign ( TINY, pn);
   } else if ( fabs ( pn ) < TINY ) {
      pn = TINY;
   }
   qn = slaDrange ( q );
   if ( fabs ( fabs ( qn ) - DPI ) < TINY ) {
      qn -= dsign ( TINY, qn );
   } else if ( fabs ( qn ) < TINY ) {
      qn = TINY;
   }
   dn = slaDrange ( d );
   if ( fabs ( fabs ( dn ) - fabs ( p ) ) < TINY ) {
      dn -= dsign ( TINY, dn );
   } else if ( fabs ( fabs ( dn ) - DPIBY2 ) < TINY ) {
      dn -= dsign ( TINY, dn );
   } else if ( fabs ( dn ) < TINY ) {
      dn = TINY;
   }

/* Useful functions */
   sq = sin ( qn );
   cq = cos ( qn );
   sqsd = sq * sin ( dn );

/* Quotient giving sin(h+t) */
   qt = sin ( pn ) * sq * cos ( dn );
   qb = cos ( pn ) * sqrt ( cq * cq + sqsd * sqsd );

/* Any solutions? */
   if ( fabs ( qt ) <= qb ) {

   /* Yes: find h+t and t */
      hpt = asin ( qt / qb );
      t = atan2 ( sqsd, cq );

   /* The two solutions */
      *h1 = slaDrange ( hpt - t );
      *h2 = slaDrange ( - hpt - ( t + DPI ) );

   /* Reject if h and Q different signs */
      if ( *h1 * qn < 0.0 ) *j1 = - 1;
      if ( *h2 * qn < 0.0 ) *j2 = - 1;
   } else {
      *j1 = - 1;
      *j2 = - 1;
   }
}
