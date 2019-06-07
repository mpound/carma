#include "slalib.h"
#include "slamac.h"
void slaSmat ( int n, float *a, float *y, float *d, int *jf, int *iw )
/*
**  - - - - - - - -
**   s l a S m a t
**  - - - - - - - -
**
**  Matrix inversion & solution of simultaneous equations.
**
**  (single precision)
**
**  For the set of n simultaneous equations in n unknowns:
**     a.y = x
**
**  where:
**     a is a non-singular n x n matrix
**     y is the vector of n unknowns
**     x is the known vector
**
**  slaSmat computes:
**     the inverse of matrix a
**     the determinant of matrix a
**     the vector of n unknowns
**
**  Arguments:
**
**     symbol  type dimension           before              after
**
**       n      long                 no. of unknowns       unchanged
**       *a     float  [n][n]            matrix             inverse
**       *y     float   [n]              vector            solution
**       *d     float                      -              determinant
**    >  *jf    long                       -            singularity flag
**       *iw    long    [n]                -               workspace
**
**
**    > jf is the singularity flag.  If the matrix is non-singular,
**      jf=0 is returned.  If the matrix is singular, jf=-1 & d=0.0 are
**      returned.  In the latter case, the contents of array a on return
**      are undefined.
**
**  Algorithm:
**     Gaussian elimination with partial pivoting.
**
**  Speed:
**     Very fast.
**
**  Accuracy:
**     Fairly accurate - errors 1 to 4 times those of routines optimized
**     for accuracy.
**
**  Example call (note handling of "adjustable dimension" 2D array):
**
**     float a[MP][MP], v[MP], d;
**     int j, iw[MP];
**      :
**     slaSmat ( n, (float *) a, v, &d, &j, iw );
**
**  Last revision:   20 February 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define TINY 1e-20f

{
   int k, imx, i, j, ki;
   float amx, t, yk;

/* Pointers to beginnings of rows in matrix a[n][n] */

   float  *ak,      /* row k                        */
          *ai,      /* row i                        */
          *aimx;    /* row imx                      */

   *jf = 0;
   *d = 1.0f;

   for ( k = 0, ak = a; k < n; k++, ak += n ) {
      amx = (float) fabs ( (double) ak[k] );
      imx = k;
      aimx = ak;
      if ( k != n ) {
         for ( i = k + 1, ai = ak + n; i < n; i++, ai += n ) {
            t = (float) fabs ( (double) ai[k] );
            if ( t > amx ) {
               amx = t;
               imx = i;
               aimx = ai;
            }
         }
      }
      if ( amx < TINY ) {
         *jf = -1;
      } else {
         if ( imx != k ) {
            for ( j = 0; j < n; j++ ) {
               t = ak[j];
               ak[j] = aimx[j];
               aimx[j] = t;
            }
            t = y[k];
            y[k] = y[imx];
            y[imx] = t;
            *d = - *d;
         }
         iw[k] = imx;
         *d *= ak[k];
         if ( fabs ( *d ) < TINY ) {
            *jf = -1;
         } else {
            ak[k] = 1.0f / ak[k];
            for ( j = 0; j < n; j++ ) {
               if ( j != k ) {
                  ak[j] *= ak[k];
               }
            }
            yk = y[k] * ak[k];
            y[k] = yk;
            for ( i = 0, ai = a; i < n; i++ , ai += n ) {
               if ( i != k ) {
                  for ( j = 0; j < n; j++ ) {
                     if ( j != k ) {
                        ai[j] -= ai[k] * ak[j];
                     }
                  }
                  y[i] -= ai[k] * yk;
               }
            }
            for ( i = 0, ai = a; i < n; i++ , ai += n ) {
               if ( i != k )
                  ai[k] *= - ak[k];
            }
         }
      }
   }
   if ( *jf != 0 ) {
      *d = 0.0f;
   } else {
      for ( k = n;  k-- > 0; ) {
         ki = iw[k];
         if ( k != ki ) {
            for ( i = 0, ai = a; i < n; i++ , ai += n ) {
               t = ai[k];
               ai[k] = ai[ki];
               ai[ki] = t;
            }
         }
      }
   }
}
