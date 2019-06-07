#include "slalib.h"
#include "slamac.h"
void slaDtps2c ( double xi, double eta, double ra, double dec,
                 double *raz1, double *decz1,
                 double *raz2, double *decz2, int *n )
/*
**  - - - - - - - - - -
**   s l a D t p s 2 c
**  - - - - - - - - - -
**
**  From the tangent plane coordinates of a star of known RA,Dec,
**  determine the RA,Dec of the tangent point.
**
**  (double precision)
**
**  Given:
**     xi,eta        double  tangent plane rectangular coordinates
**     ra,dec        double  spherical coordinates
**
**  Returned:
**     *raz1,*decz1  double  spherical coordinates of TP, solution 1
**     *raz2,*decz2  double  spherical coordinates of TP, solution 2
**     *n            int     number of solutions:
**                            0 = no solutions returned (note 2)
**                            1 = only the first solution is useful (note 3)
**                            2 = both solutions are useful (note 3)
**
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
**  5  This routine is the spherical equivalent of the routine slaDtpv2c.
**
**  Called:  slaDranrm
**
**  Last revision:   5 June 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  double x2, y2, sd, cd, sdf, r2, r, s, c;

  x2 = xi * xi;
  y2 = eta * eta;
  sd = sin ( dec );
  cd = cos ( dec );
  sdf = sd * sqrt ( 1.0 + x2 + y2 );
  r2 = cd * cd * ( 1.0 + y2 ) - sd * sd * x2;
  if ( r2 >= 0.0 ) {
     r = sqrt ( r2 );
     s = sdf - eta * r;
     c = sdf * eta + r;
     if ( xi == 0.0 && r == 0.0 ) {
        r = 1.0;
     }
     *raz1 = slaDranrm ( ra - atan2 ( xi, r ) );
     *decz1 = atan2 ( s, c );
     r = -r;
     s = sdf - eta * r;
     c = sdf * eta + r;
     *raz2 = slaDranrm ( ra - atan2 ( xi, r ) );
     *decz2 = atan2 ( s, c );
     *n = ( fabs ( sdf ) < 1.0 ) ? 1 : 2;
  } else {
     *n = 0;
  }
}
