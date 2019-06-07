#include "slalib.h"
#include "slamac.h"
void slaTps2c ( float xi, float eta, float ra, float dec,
                float *raz1, float *decz1,
                float *raz2, float *decz2, int *n )
/*
**  - - - - - - - - -
**   s l a T p s 2 c
**  - - - - - - - - -
**
**  From the tangent plane coordinates of a star of known RA,Dec,
**  determine the RA,Dec of the tangent point.
**
**  (single precision)
**
**  Given:
**     xi,eta        float   tangent plane rectangular coordinates
**     ra,dec        float   spherical coordinates
**
**  Returned:
**     *raz1,*decz1  float   spherical coordinates of TP, solution 1
**     *raz2,*decz2  float   spherical coordinates of TP, solution 2
**     *n            int     number of solutions:
**                            0 = no solutions returned (note 2)
**                            1 = only the first solution is useful (note 3)
**                            2 = both solutions are useful (note 3)
**
**  Notes:
**
**  1  The raz1 and raz2 values are returned in the range 0-2pi.
**
**  2  Cases where there is no solution can only arise near the poles.
**     For example, it is clearly impossible for a star at the pole
**     itself to have a non-zero xi value, and hence it is
**     meaningless to ask where the tangent point would have to be
**     to bring about this combination of xi and dec.
**
**  3  Also near the poles, cases can arise where there are two useful
**     solutions.  The argument n indicates whether the second of the
**     two solutions returned is useful;  n=1 indicates only one useful
**     solution, the usual case;  under these circumstances, the second
**     solution corresponds to the "over-the-pole" case, and this is
**     reflected in the values of raz2 and decz2 which are returned.
**
**  4  The decz1 and decz2 values are returned in the range +/-pi, but
**     in the usual, non-pole-crossing, case, the range is +/-pi/2.
**
**  5  This routine is the spherical equivalent of the routine slaTpv2c.
**
**  Called:  slaRanorm
**
**  Last revision:   5 June 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  float x2, y2, sd, cd, sdf, r2, r, s, c;

  x2 = xi * xi;
  y2 = eta * eta;
  sd = (float) sin ( (double) dec );
  cd = (float) cos ( (double) dec );
  sdf = sd * (float) sqrt ( (double) ( 1.0f + x2 + y2 ) );
  r2 = cd * cd * ( 1.0f + y2 ) - sd * sd * x2;
  if ( r2 >= 0.0f ) {
     r = (float) sqrt ( (double) r2 );
     s = sdf - eta * r;
     c = sdf * eta + r;
     if ( xi == 0.0f && r == 0.0f ) {
        r = 1.0f;
     }
     *raz1 = slaRanorm ( ra - (float) atan2 ( (double) xi, (double) r ) );
     *decz1 = (float) atan2 ( (double) s, (double) c );
     r = -r;
     s = sdf - eta * r;
     c = sdf * eta + r;
     *raz2 = slaRanorm ( ra - (float) atan2 ( (double) xi, (double) r ) );
     *decz2 = (float) atan2 ( (double) s, (double) c );
     *n = ( fabs ( (double) sdf ) < 1.0 ) ? 1 : 2;
  } else {
     *n = 0;
  }
}
