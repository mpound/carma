#include "slalib.h"
#include "slamac.h"
void slaSvdsol ( int m, int n, int mp, int np, double *b, double *u,
                 double *w, double *v, double *work, double *x )
/*
**  - - - - - - - - - -
**   s l a S v d s o l
**  - - - - - - - - - -
**
**  From a given vector and the SVD of a matrix (as obtained from
**  the slaSvd routine), obtain the solution vector.
**
**  (double precision)
**
**  This routine solves the equation:
**
**     a . x = b
**
**  where:
**
**     a   is a given m (rows) x n (columns) matrix, where m.ge.n
**     x   is the n-vector we wish to find
**     b   is a given m-vector
**
**  By means of the singular value decomposition method (SVD).  In
**  this method, the matrix a is first factorized (for example by
**  the routine slaSvd) into the following components:
**
**     a = u x w x vt
**
**  where:
**
**     a   is the m (rows) x n (columns) matrix
**     u   is an m x n column-orthogonal matrix
**     w   is an n x n diagonal matrix with w(i,i).ge.0
**     vt  is the transpose of an nxn orthogonal matrix
**
**     Note that m and n, above, are the logical dimensions of the
**     matrices and vectors concerned, which can be located in
**     arrays of larger physical dimensions mp and np.
**
**  The solution is found from the expression:
**
**     x = v . [diag(1/wj)] . ( transpose(u) . b )
**
**  Notes:
**
**  1)  If matrix a is square, and if the diagonal matrix w is not
**      adjusted, the method is equivalent to conventional solution
**      of simultaneous equations.
**
**  2)  If m>n, the result is a least-squares fit.
**
**  3)  If the solution is poorly determined, this shows up in the
**      SVD factorization as very small or zero wj values.  Where
**      a wj value is small but non-zero it can be set to zero to
**      avoid ill effects.  The present routine detects such zero
**      wj values and produces a sensible solution, with highly
**      correlated terms kept under control rather than being allowed
**      to elope to infinity, and with meaningful values for the
**      other terms.
**
**  Given:
**     m,n    int            numbers of rows and columns in matrix a
**     mp,np  int            physical dimensions of array containing matrix a
**     *b     double[m]      known vector b
**     *u     double[mp][np] array containing mxn matrix u
**     *w     double[n]      nxn diagonal matrix w (diagonal elements only)
**     *v     double[np][np] array containing nxn orthogonal matrix v
**
**  Returned:
**     *work  double[n]      workspace
**     *x     double[n]      unknown vector x
**
**  Note:  If the relative sizes of m, n, mp and np are inconsistent,
**         the vector x is returned unaltered.  This condition should
**         have been detected when the SVD was performed using slaSvd.
**
**  Reference:
**     Numerical Recipes, Section 2.9.
**
**  Example call (note handling of "adjustable dimension" 2D arrays):
**
**    double a[MP][NP], w[NP], v[NP][NP], work[NP], b[MP], x[NP];
**    int m, n;
**     :
**    slaSvdsol ( m, n, MP, NP, b, (double *) a, w, (double *) v, work, x );
**
**  Last revision:   20 February 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int j, i, jj;
   double s;
   double *ui;
   double *vj;

/* Check that the matrix is the right size and shape */
   if ( m >= n && m <= mp && n <= np ) {

   /* Calculate [diag(1/wj)] . transpose(u) . b (or zero for zero wj) */
      for ( j = 0; j < n; j++ ) {
         s = 0.0;
         if ( w[j] != 0.0 ) {
            for ( i = 0, ui = u;
                  i < m;
                  i++, ui += np ) {
               s += ui[j] * b[i];
            }
            s /= w[j];
         }
         work[j] = s;
      }

   /* Multiply by matrix v to get result */
      for ( j = 0, vj = v;
            j < n;
            j++, vj += np ) {
         s = 0.0;
         for ( jj = 0; jj < n; jj++ ) {
            s += vj[jj] * work[jj];
         }
         x[j] = s;
      }
   }
}
