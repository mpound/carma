#include "slalib.h"
#include "slamac.h"
void slaPxy ( int np, double xye[][2], double xym[][2],
              double coeffs[6], double xyp[][2],
              double *xrms, double *yrms, double *rrms )
/*
**  - - - - - - -
**   s l a P x y
**  - - - - - - -
**
**  Given arrays of "expected" and "measured" [x,y] coordinates, and a
**  linear model relating them (as produced by slaFitxy), compute
**  the array of "predicted" coordinates and the rms residuals.
**
**  Given:
**     np      int            number of samples
**     xye     double[np]     expected [x,y] for each sample
**     xym     double[np]     measured [x,y] for each sample
**     coeffs  double[6]      coefficients of model (see below)
**
**  Returned:
**     xyp     double[np]     predicted [x,y] for each sample
**     *xrms   double         RMS in x
**     *yrms   double         RMS in y
**     *rrms   double         total RMS (vector sum of xrms and yrms)
**
**  The model is supplied in the array coeffs.  Naming the
**  elements of coeff as follows:
**
**     coeffs[0] = a
**     coeffs[1] = b
**     coeffs[2] = c
**     coeffs[3] = d
**     coeffs[4] = e
**     coeffs[5] = f
**
**  The model is applied thus:
**
**     xp = a + b*xm + c*ym
**     yp = d + e*xm + f*ym
**
**  The residuals are (xp-xe) and (yp-ye).
**
**  If np is less than or equal to zero, no coordinates are
**  transformed, and the rms residuals are all zero.
**
**  See also slaFitxy, slaInvf, slaXy2xy, slaDcmpf
**
**  Called:  slaXy2xy
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i;
   double sdx2, sdy2, xp, yp, dx, dy, dx2, dy2, p;

/* Initialize summations */
   sdx2 = 0.0;
   sdy2 = 0.0;

/* Loop by sample */
   for ( i = 0; i < np; i++ ) {

   /*  Transform "measured" [x,y] to "predicted" [x,y] */
       slaXy2xy ( xym[i][0], xym[i][1], coeffs, &xp, &yp );
       xyp[i][0] = xp;
       xyp[i][1] = yp;

   /*  Compute residuals in x and y, and update summations */
       dx = xye[i][0] - xp;
       dy = xye[i][1] - yp;
       dx2 = dx * dx;
       dy2 = dy * dy;
       sdx2 = sdx2 + dx2;
       sdy2 = sdy2 + dy2;

   /*  Next sample */
   }

/* Compute RMS values */
   p = (double) gmax ( 1.0, np );
   *xrms = sqrt ( sdx2 / p );
   *yrms = sqrt ( sdy2 / p );
   *rrms = sqrt ( *xrms * *xrms + *yrms * *yrms );
}
