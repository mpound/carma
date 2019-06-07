#include "slalib.h"
#include "slamac.h"
void slaDcmpf ( double coeffs[6],
                double *xz, double *yz, double *xs,
                double *ys, double *perp, double *orient )
/*
**  - - - - - - - - -
**   s l a D c m p f
**  - - - - - - - - -
**
**  Decompose an [x,y] linear fit into its constituent parameters:
**  zero points, scales, nonperpendicularity and orientation.
**
**  Given:
**     coeffs    double[6]     transformation coefficients (see note)
**
**  Returned:
**     *xz       double        x zero point
**     *yz       double        y zero point
**     *xs       double        x scale
**     *ys       double        y scale
**     *perp     double        nonperpendicularity (radians)
**     *orient   double        orientation (radians)
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
**  The model transforms coordinates [x1,y1] into coordinates
**  [x2,y2] as follows:
**
**     x2 = a + b*x1 + c*y1
**     y2 = d + e*x1 + f*y1
**
**  The transformation can be decomposed into four steps:
**
**     1)  Zero points:
**
**             x' = xz + x1
**             y' = yz + y1
**
**     2)  Scales:
**
**             x'' = xs*x'
**             y'' = ys*y'
**
**     3)  Nonperpendicularity:
**
**             x''' = cos(perp/2)*x'' + sin(perp/2)*y''
**             y''' = sin(perp/2)*x'' + cos(perp/2)*y''
**
**     4)  Orientation:
**
**             x2 = cos(orient)*x''' + sin(orient)*y'''
**             y2 =-sin(orient)*y''' + cos(orient)*y'''
**
**  See also slaFitxy, slaPxy, slaInvf, slaXy2xy
**
**  Last revision:   22 September 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  double a, b, c, d, e, f, rb2e2, rc2f2, xsc, ysc;

  double p, ws, wc;

  double vor;
  double hp, shp, chp, sor, cor, det, x0, y0;

/* Copy the six coefficients */
   a = coeffs[0];
   b = coeffs[1];
   c = coeffs[2];
   d = coeffs[3];
   e = coeffs[4];
   f = coeffs[5];

/* Scales */
   rb2e2 = sqrt ( b * b + e * e );
   rc2f2 = sqrt ( c * c + f * f );
   if ( ( b * f - c * e ) >= 0.0 )
     xsc = rb2e2;
   else {
     b = -b;
     c = -c;
     xsc = -rb2e2;
   }
   ysc = rc2f2;

/* Non-perpendicularity */
   p = ( ( c != 0.0 || f != 0.0 ) ? atan2 ( c, f ) : 0.0 ) +
       ( ( e != 0.0 || b != 0.0 ) ? atan2 ( e, b ) : 0.0 );

/* Orientation */
   ws = ( c * rb2e2 ) - ( e * rc2f2 );
   wc = ( b * rc2f2 ) + ( f * rb2e2 );
   vor = ( ws != 0.0 || wc != 0.0 ) ? atan2 ( ws, wc ) : 0.0;

/* Zero corrections */
   hp = p / 2.0;
   shp = sin ( hp );
   chp = cos ( hp );
   sor = sin ( vor );
   cor = cos ( vor );
   det = xsc * ysc * ( chp + shp ) * ( chp - shp );
   if ( fabs ( det ) > 0.0 ) {
     x0 = ysc * ( a * ( ( chp * cor ) - ( shp * sor ) )
                - d * ( ( chp * sor ) + ( shp * cor ) ) ) / det;
     y0 = xsc * ( a * ( ( chp * sor ) - ( shp * cor ) )
                + d * ( ( chp * cor ) + ( shp * sor ) ) ) / det;
   }
   else {
     x0 = 0.0;
     y0 = 0.0;
   }

/* Results */
   *xz = x0;
   *yz = y0;
   *xs = xsc;
   *ys = ysc;
   *perp = p;
   *orient = vor;
}
