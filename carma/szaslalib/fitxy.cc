#include "slalib.h"
#include "slamac.h"
void slaFitxy ( int itype, int np,
                double xye[][2], double xym[][2], double coeffs[6],
                int *j )
/*
**  - - - - - - - - -
**   s l a F i t x y
**  - - - - - - - - -
**
**  Fit a linear model to relate two sets of [x,y] coordinates.
**
**  Given:
**     itype    int            type of model: 4 or 6 (note 1)
**     np       int            number of samples (note 2)
**     xye      double[np][2]  expected [x,y] for each sample
**     xym      double[np][2]  measured [x,y] for each sample
**
**  Returned:
**     coeffs   double[6]      coefficients of model (note 3)
**     *j       int            status:  0 = OK
**                                     -1 = illegal itype
**                                     -2 = insufficient data
**                                     -3 = singular solution
**
**  Notes:
**
**    1)  itype, which must be either 4 or 6, selects the type of model
**        fitted.  Both allowed itype values produce a model coeffs which
**        consists of six coefficients, namely the zero points and, for
**        each of xe and ye, the coefficient of xm and ym.  For itype=6,
**        all six coefficients are independent, modelling squash and shear
**        as well as origin, scale, and orientation.  However, itype=4
**        selects the "solid body rotation" option;  the model coeffs
**        still consists of the same six coefficients, but now two of
**        them are used twice (appropriately signed).  Origin, scale
**        and orientation are still modelled, but not squash or shear -
**        the units of x and y have to be the same.
**
**    2)  For nc=4, np must be at least 2.  For nc=6, np must be at
**        least 3.
**
**    3)  The model is returned in the array coeffs.  Naming the
**        elements of coeffs as follows:
**
**                    coeffs[0] = a
**                    coeffs[1] = b
**                    coeffs[2] = c
**                    coeffs[3] = d
**                    coeffs[4] = e
**                    coeffs[5] = f
**
**        The model is:
**
**              xe = a + b*xm + c*ym
**              ye = d + e*xm + f*ym
**
**        For the "solid body rotation" option (itype=4), the
**        magnitudes of b and f, and of c and e, are equal.  The
**        signs of these coefficients depend on whether there is a
**        sign reversal between xe,ye and xm,ym.  Fits are performed
**        with and without a sign reversal and the best one chosen.
**
**    4)  Error status values j=-1 and -2 leave coeffs unchanged;
**        If j=-3 coeffs may have been changed.
**
**  See also slaPxy, slaInvf, slaXy2xy, slaDcmpf
**
**  Called:  slaDmat, slaDmxv
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i, jstat;
   int iw[4];
   int nsol;
   double p, sxe, sxexm, sxeym, sye, syeym, syexm, sxm,
          sym, sxmxm, sxmym, symym, xe, ye,
          xm, ym, v[4], dm3[3][3], dm4[4][4], det,
          sgn, sxxyy, sxyyx, sx2y2, a, b, c, d,
          sdr2, xr, yr, aold, bold, cold, dold, sold;

/* Preset the status */
   *j = 0;

/* Float the number of samples */
   p = (double) np;

/* Check itype */
   if ( itype == 6 ) {

/*
** Six-coefficient linear model
** ----------------------------
*/

   /* Check enough samples */
      if ( np >= 3 ) {

   /* Form summations */
         sxe = 0.0;
         sxexm = 0.0;
         sxeym = 0.0;
         sye = 0.0;
         syeym = 0.0;
         syexm = 0.0;
         sxm = 0.0;
         sym = 0.0;
         sxmxm = 0.0;
         sxmym = 0.0;
         symym = 0.0;

         for ( i = 0; i < np; i++ ) {
            xe = xye[i][0];
            ye = xye[i][1];
            xm = xym[i][0];
            ym = xym[i][1];
            sxe = sxe + xe;
            sxexm = sxexm + xe * xm;
            sxeym = sxeym + xe * ym;
            sye = sye + ye;
            syeym = syeym + ye * ym;
            syexm = syexm + ye * xm;
            sxm = sxm + xm;
            sym = sym + ym;
            sxmxm = sxmxm + xm * xm;
            sxmym = sxmym + xm * ym;
            symym = symym + ym * ym;
         }

      /* Solve for a,b,c in  xe = a + b*xm + c*ym */
         v[0] = sxe;
         v[1] = sxexm;
         v[2] = sxeym;
         dm3[0][0] = p;
         dm3[0][1] = sxm;
         dm3[0][2] = sym;
         dm3[1][0] = sxm;
         dm3[1][1] = sxmxm;
         dm3[1][2] = sxmym;
         dm3[2][0] = sym;
         dm3[2][1] = sxmym;
         dm3[2][2] = symym;
         slaDmat ( 3, dm3[0], v, &det, &jstat, iw);
         if (jstat == 0) {
            for ( i = 0; i < 3; i++ ) {
               coeffs[i] = v[i];
            }

         /* Solve for d,e,f in  ye = d + e*xm + f*ym */
            v[0] = sye;
            v[1] = syexm;
            v[2] = syeym;
            slaDmxv ( dm3, v, &coeffs[3] );
         } else {

         /* No 6-coefficient solution possible */
            *j = -3;
         }
      } else {

      /* Insufficient data for 6-coefficient fit */
         *j = -2;
      }
   } else if ( itype == 4 ) {

   /*
   ** Four-coefficient solid body rotation model
   ** ------------------------------------------
   */

   /* Check enough samples */
      if ( np >= 2 ) {

      /* Try two solutions, first without then with flip in x */
         for ( nsol = 1; nsol <= 2; nsol++ ) {
            sgn = ( nsol == 1 ) ? 1.0 : -1.0;

         /* Form summations*/
            sxe = 0.0;
            sxxyy = 0.0;
            sxyyx = 0.0;
            sye = 0.0;
            sxm = 0.0;
            sym = 0.0;
            sx2y2 = 0.0;
            for ( i = 0; i < np; i++ ) {
               xe = xye[i][0] * sgn;
               ye = xye[i][1];
               xm = xym[i][0];
               ym = xym[i][1];
               sxe = sxe + xe;
               sxxyy = sxxyy + xe * xm + ye * ym;
               sxyyx = sxyyx + xe * ym - ye * xm;
               sye = sye + ye;
               sxm = sxm + xm;
               sym = sym + ym;
               sx2y2 = sx2y2 + xm * xm + ym * ym;
            }

         /*
         ** Solve for a,b,c,d in:  +/- xe = a + b*xm - c*ym
         **                          + ye = d + c*xm + b*ym
         */
            v[0] = sxe;
            v[1] = sxxyy;
            v[2] = sxyyx;
            v[3] = sye;
            dm4[0][0] = p;
            dm4[0][1] = sxm;
            dm4[0][2] = -sym;
            dm4[0][3] = 0.0;
            dm4[1][0] = sxm;
            dm4[1][1] = sx2y2;
            dm4[1][2] = 0.0;
            dm4[1][3] = sym;
            dm4[2][0] = sym;
            dm4[2][1] = 0.0;
            dm4[2][2] = -sx2y2;
            dm4[2][3] = -sxm;
            dm4[3][0] = 0.0;
            dm4[3][1] = sym;
            dm4[3][2] = sxm;
            dm4[3][3] = p;
            slaDmat ( 4, dm4[0], v, &det, &jstat, iw );
            if ( jstat == 0 ) {
               a = v[0];
               b = v[1];
               c = v[2];
               d = v[3];

            /* Determine sum of radial errors squared */
               sdr2 = 0.0;
               for ( i = 0; i < np; i++ ) {
                  xm = xym[i][0];
                  ym = xym[i][1];
                  xr = a + b * xm - c * ym - xye[i][0] * sgn;
                  yr = d + c * xm + b * ym- xye[i][1];
                  sdr2 = sdr2 + xr * xr + yr * yr;
               }
            } else {

            /* Singular: set flag */
               sdr2 = -1.0;
            }

         /* If first pass and non-singular, save variables */
            if ( nsol == 1 && jstat == 0 ) {
               aold = a;
               bold = b;
               cold = c;
               dold = d;
               sold = sdr2;
            }
         }

      /* Pick the best of the two solutions */
         if ( sold >= 0.0 && sold <= sdr2 ) {
            coeffs[0] = aold;
            coeffs[1] = bold;
            coeffs[2] = -cold;
            coeffs[3] = dold;
            coeffs[4] = cold;
            coeffs[5] = bold;
         } else if ( jstat == 0 ) {
            coeffs[0] = -a;
            coeffs[1] = -b;
            coeffs[2] = c;
            coeffs[3] = d;
            coeffs[4] = c;
            coeffs[5] = b;
         } else {

         /* No 4-coefficient fit possible */
            *j = -3;
         }
      } else {

      /* Insufficient data for 4-coefficient fit */
         *j = -2;
      }
   } else {

   /* Illegal itype - not 4 or 6 */
      *j = -1;
   }
}
