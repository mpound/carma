#include "slalib.h"
#include "slamac.h"
void slaXy2xy ( double xc1, double yc1, double coeffs[6],
                double *xc2, double *yc2 )
/*
**  - - - - - - - - -
**   s l a X y 2 x y
**  - - - - - - - - -
**
**  Transform one [x,y] into another using a linear model of the type
**  produced by the slaFitxy routine.
**
**  Given:
**     xc1      double        x-coordinate
**     yc1      double        y-coordinate
**     coeffs   double[6]     transformation coefficients (see note)
**
**  Returned:
**     *xc2     double        x-coordinate
**     *yc2     double        y-coordinate
**
**  The model relates two sets of [x,y] coordinates as follows.
**  Naming the elements of coeffs:
**
**     coeffs[0] = a
**     coeffs[1] = b
**     coeffs[2] = c
**     coeffs[3] = d
**     coeffs[4] = e
**     coeffs[5] = f
**
**  the present routine performs the transformation:
**
**     xc2 = a + b*xc1 + c*yc1
**     yc2 = d + e*xc1 + f*yc1
**
**  See also slaFitxy, slaPxy, slaInvf, slaDcmpf.
**
**  Last revision:   5 December 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   *xc2 = coeffs[0] + coeffs[1] * xc1 + coeffs[2] * yc1;
   *yc2 = coeffs[3] + coeffs[4] * xc1 + coeffs[5] * yc1;
}
