#include "slalib.h"
#include "slamac.h"
void slaSvdcov ( int n, int np, int nc, double *w, double *v,
                 double *work, double *cvm )
/*
**  - - - - - - - - - -
**   s l a S v d c o v
**  - - - - - - - - - -
**
**  From the w and v matrices from the SVD factorization of a matrix
**  (as obtained from the slaSvd routine), obtain the covariance matrix.
**
**  (double precision)
**
**  Given:
**     n      int            number of rows and columns in matrices w and v
**     np     int            first dimension of array containing matrix v
**     nc     int            first dimension of array to receive cvm
**     *w     double[n]      nxn diagonal matrix w (diagonal elements only)
**     *v     double[np][np] array containing nxn orthogonal matrix v
**
**  Returned:
**     *work  double[n]      workspace
**     *cvm   double[nc][nc] array to receive covariance matrix
**
**  Reference:
**     Numerical Recipes, Section 14.3.
**
**  Example call (note handling of "adjustable dimension" 2D arrays):
**
**    double w[NP], v[NP][NP], work[NP], c[NC][NC];
**    int n;
**     :
**    slaSvdcov ( n, NP, NC, w, (double *) v, work, (double *) c );
**
**  Last revision:   20 February 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i, j, k;
   double s;
   double *vi, *vj;
   double *cvmi, *cvmj;


   for ( i = 0; i < n; i++ ) {
      s = w[i];
      if ( s != 0.0 )
         work[i] = 1.0 / ( s * s );
      else
         work[i] = 0.0;
   }
   for ( i = 0, vi = v, cvmi = cvm;
         i < n;
         i++, vi += np, cvmi += nc ) {
      for ( j = 0, vj = v, cvmj = cvm;
            j <= i;
            j++, vj += np, cvmj += nc ) {
         s = 0.0;
         for ( k = 0; k < n; k++ ) {
            s += vi[k] * vj[k] * work[k];
         }
         cvmi[j] = s;
         cvmj[i] = s;
      }
   }
}
