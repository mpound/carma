#include "slalib.h"
#include "slamac.h"
void slaInvf ( double fwds[6], double bkwds[6], int *j )
/*
**  - - - - - - - -
**   s l a I n v f
**  - - - - - - - -
**
**  Invert a linear model of the type produced by the slaFitxy routine.
**
**  Given:
**     fwds    double[6]      model coefficients
**
**  Returned:
**     bkwds   double[6]      inverse model
**     *j      int            status:  0 = OK, -1 = no inverse
**
**  The models relate two sets of [x,y] coordinates as follows.
**  Naming the elements of fwds:
**
**     fwds[0] = a
**     fwds[1] = b
**     fwds[2] = c
**     fwds[3] = d
**     fwds[4] = e
**     fwds[5] = f
**
**  Where two sets of coordinates [x1,y1] and [x2,y1] are related
**  thus:
**
**     x2 = a + b*x1 + c*y1
**     y2 = d + e*x1 + f*y1
**
**  The present routine generates a new set of coefficients:
**
**     bkwds[0] = p
**     bkwds[1] = q
**     bkwds[2] = r
**     bkwds[3] = s
**     bkwds[4] = t
**     bkwds[5] = u
**
**  Such that:
**
**     x1 = p + q*x2 + r*y2
**     y1 = s + t*x2 + u*y2
**
**  Two successive calls to slaInvf will thus deliver a set
**  of coefficients equal to the starting values.
**
**  See also slaFitxy, slaPxy, slaXy2xy, slaDcmpf
**
**  Last revision:   30 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double a, b, c, d, e, f, det;

   a = fwds[0];
   b = fwds[1];
   c = fwds[2];
   d = fwds[3];
   e = fwds[4];
   f = fwds[5];
   det = b * f - c * e;

   if ( det != 0.0 ) {
      bkwds[0] = ( c * d - a * f ) / det;
      bkwds[1] = f / det;
      bkwds[2] = - c / det;
      bkwds[3] = ( a * e - b * d ) / det;
      bkwds[4] = - e / det;
      bkwds[5] = b / det;
      *j = 0;
   } else {
      *j = -1;
   }
}
