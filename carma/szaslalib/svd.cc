#include "slalib.h"
#include "slamac.h"
double rms ( double a, double b );
void slaSvd ( int m, int n, int mp, int np, double *a, double *w,
              double *v, double *work, int *jstat )
/*
**  - - - - - - -
**   s l a S v d
**  - - - - - - -
**
**  Singular value decomposition.
**
**  (double precision)
**
**  This routine expresses a given matrix a as the product of
**  three matrices u, w, v:
**
**     a = u x w x vt
**
**  where:
**
**     a   is any m (rows) x n (columns) matrix, where m >= n
**     u   is an m x n column-orthogonal matrix
**     w   is an n x n diagonal matrix with w(i,i) >= 0
**     vt  is the transpose of an n x n orthogonal matrix
**
**     Note that m and n, above, are the logical dimensions of the
**     matrices and vectors concerned, which can be located in
**     arrays of larger physical dimensions, given by mp and np.
**
**  Given:
**     m,n    int            numbers of rows and columns in matrix a
**     mp,np  int            physical dimensions of the array containing a
**     a      double[mp][np] array containing m x n matrix a
**
**  Returned:
**     *a     double[mp][np] array containing m x n column-orthogonal matrix u
**     *w     double[n]      n x n diagonal matrix w (diagonal elements only)
**     *v     double[np][np] array containing n x n orthogonal matrix v
**     *work  double[n]      workspace
**     *jstat int            0 = OK
**                          -1 = the a array is the wrong shape
**                          >0 = 1 + index of w for which convergence failed.
**
**     (n.b. v contains matrix v, not the transpose of matrix v)
**
**  References:
**     The algorithm is an adaptation of the routine SVD in the EISPACK
**     library (Garbow et al 1977, Eispack guide extension, Springer
**     Verlag), which is a Fortran 66 implementation of the Algol
**     routine SVD of Wilkinson & Reinsch 1971 (Handbook for Automatic
**     Computation, vol 2, Ed Bauer et al, Springer Verlag).  For the
**     non-specialist, probably the clearest general account of the use
**     of SVD in least squares problems is given in Numerical Recipes
**     (Press et al 1986, Cambridge University Press).
**
**  From slamac.h:  TRUE, FALSE
**
**  Example call (note handling of "adjustable dimension" 2D arrays):
**
**    double a[MP][NP], w[NP], v[NP][NP], work[NP];
**    int m, n, j;
**     :
**    slaSvd ( m, n, MP, NP, (double *) a, w, (double *) v, work, &j );
**
**  Last revision:   20 February 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

/* Maximum number of iterations in QR phase */
#define ITMAX 30
{

   int i, k, l, j, k1, its, l1, i1, cancel;
   double g, scale, an, s, x, f, h, cn, c, y, z;
   double *ai, *aj, *ak;
   double *vi, *vj, *vk;

/* Check that the matrix is the right size and shape */
   if ( m < n || m > mp || n > np ) {
      *jstat = -1;
   } else {
      *jstat = 0;

   /* Householder reduction to bidiagonal form */
      g = 0.0;
      scale = 0.0;
      an = 0.0;
      for ( i = 0, ai = a; i < n; i++, ai += np ) {
         l = i + 1;
         work[i] = scale * g;
         g = 0.0;
         s = 0.0;
         scale = 0.0;
         if ( i < m ) {
            for ( k = i, ak = a; k < m; k++, ak += np ) {
               scale += fabs ( ak[i] );
            }
            if ( scale != 0.0 ) {
               for ( k = i, ak = ai; k < m; k++, ak += np ) {
                  x = ak[i] / scale;
                  ak[i] = x;
                  s += x * x;
               }
               f = ai[i];
               g = - dsign ( sqrt ( s ), f );
               h = f * g - s;
               ai[i] = f - g;
               if ( i != n - 1 ) {
                  for ( j = l; j < n; j++ ) {
                     s = 0.0;
                     for ( k = i, ak = ai; k < m; k++, ak += np ) {
                        s += ak[i] * ak[j];
                     }
                     f = s / h;
                     for ( k = i, ak = ai; k < m; k++, ak += np ) {
                        ak[j] += f * ak[i];
                     }
                  }
               }
               for ( k = i, ak = ai; k < m; k++, ak += np ) {
                  ak[i] *= scale;
               }
            }
         }
         w[i] = scale * g;
         g = 0.0;
         s = 0.0;
         scale = 0.0;
         if ( i < m && i != n - 1 ) {
            for ( k = l;  k < n;  k++ ) {
               scale += fabs ( ai[k] );
            }
            if ( scale != 0.0 ) {
               for ( k = l; k < n; k++ ) {
                  x = ai[k] / scale;
                  ai[k] = x;
                  s += x * x;
               }
               f = ai[l];
               g = - dsign ( sqrt ( s ), f );
               h = f * g - s;
               ai[l] = f - g;
               for ( k = l; k < n; k++ ) {
                  work[k] = ai[k] / h;
               }
               if ( i != m-1 ) {
                  for ( j = l,  aj = a + l*np; j < m; j++,  aj += np ) {
                     s = 0.0;
                     for ( k = l; k < n; k++ ) {
                        s += aj[k] * ai[k];
                     }
                     for ( k = l; k < n; k++ ) {
                        aj[k] += s * work[k];
                     }
                  }
               }
               for ( k = l; k < n; k++ ) {
                  ai[k] *= scale;
               }
            }
         }

      /* Overestimate of largest column norm for convergence test */
         cn = fabs ( w[i] ) + fabs ( work[i] );
         an = gmax ( an, cn );
      }

   /* Accumulation of right-hand transformations */
      for ( i = n - 1, ai = a + ( n - 1 ) * np, vi = v + ( n - 1 ) * np;
            i >= 0;
            i--, ai -= np, vi -= np ) {
         if ( i != n - 1 ) {
            if ( g != 0.0 ) {
               for ( j = l, vj = v + l * np; j < n; j++, vj += np ) {
                  vj[i] = ( ai[j] / ai[l] ) / g;
               }
               for ( j = l; j < n; j++ ) {
                  s = 0.0;
                  for ( k = l, vk = v + l*np; k < n; k++, vk += np ) {
                     s += ai[k] * vk[j];
                  }
                  for ( k = l, vk = v + l*np; k < n; k++, vk += np ) {
                     vk[j] += s * vk[i];
                  }
               }
            }
            for ( j = l, vj = v + l*np; j < n; j++, vj += np ) {
               vi[j] = 0.0;
               vj[i] = 0.0;
            }
         }
         vi[i] = 1.0;
         g = work[i];
         l = i;
      }

   /* Accumulation of left-hand transformations */
      for ( i = n - 1, ai = a + i*np; i >= 0; i--, ai -= np ) {
         l = i + 1;
         g = w[i];
         if ( i != n - 1 ) {
            for ( j = l; j < n; j++ ) {
               ai[j] = 0.0;
            }
         }
         if ( g != 0.0 ) {
            if ( i != n - 1 ) {
               for ( j = l; j < n; j++ ) {
                  s = 0.0;
                  for ( k = l, ak = a + l * np; k < m; k++, ak += np ) {
                     s += ak[i] * ak[j];
                  }
                  f = ( s / ai[i] ) / g;
                  for ( k = i, ak = a + i * np; k < m; k++, ak += np ) {
                     ak[j] += f * ak[i];
                  }
               }
            }
            for ( j = i, aj = ai; j < m; j++, aj += np ) {
               aj[i] /= g;
            }
         } else {
            for ( j = i, aj = ai; j < m; j++, aj += np ) {
               aj[i] = 0.0;
            }
         }
         ai[i] += 1.0;
      }

   /* Diagonalization of the bidiagonal form */
      for ( k = n - 1; k >= 0; k-- ) {
         k1 = k - 1;

      /* Iterate until converged */
         for ( its = 1; its <= ITMAX; its++ ) {

         /* Test for splitting into submatrices */
            cancel = TRUE;
            for ( l = k; l >= 0; l-- ) {
               l1 = l - 1;
               if ( an + fabs ( work[l] ) == an ) {
                  cancel = FALSE;
                  break;
               }
            /* (Following never attempted for l=0 because work[0] is zero) */
               if ( an + fabs ( w[l1] ) == an ) {
                  break;
               }
            }

         /* Cancellation of work[l] if l>0 */
            if ( cancel ) {
               c = 0.0;
               s = 1.0;
               for ( i = l; i <= k; i++ ) {
                  f = s * work[i];
                  if ( an + fabs ( f ) == an ) {
                     break;
                  }
                  g = w[i];
                  h = rms ( f, g );
                  w[i] = h;
                  c = g / h;
                  s = - f / h;
                  for ( j = 0, aj = a; j < m; j++, aj += np ) {
                     y = aj[l1];
                     z = aj[i];
                     aj[l1] = y * c + z * s;
                     aj[i] = - y * s + z * c;
                  }
               }
            }

         /* Converged? */
            z = w[k];
            if ( l == k ) {

            /* Yes: ensure singular values non-negative */
               if ( z < 0.0 ) {
                  w[k] = -z;
                  for ( j = 0, vj = v; j < n; j++, vj += np ) {
                     vj[k] = -vj[k];
                  }
               }

            /* Stop iterating */
               break;

            } else {

            /* Not converged yet: set status if iteration limit reached */
               if ( its >= ITMAX ) {
                  *jstat = k + 1;
               }

            /* Shift from bottom 2 x 2 minor */
               x = w[l];
               y = w[k1];
               g = work[k1];
               h = work[k];
               f = ( ( y - z ) * ( y + z )
                   + ( g - h ) * ( g + h ) ) / ( 2.0 * h * y );
               g = ( fabs ( f ) <= 1e15 ) ? rms ( f, 1.0 ) : fabs ( f );
               f = ( ( x - z ) * ( x + z )
                       + h * ( y / ( f + dsign ( g, f ) ) - h ) ) / x;

            /* Next QR transformation */
               c = 1.0;
               s = 1.0;
               for ( i1 = l; i1 <= k1; i1++ ) {
                  i = i1 + 1;
                  g = work[i];
                  y = w[i];
                  h = s * g;
                  g = c * g;
                  z = rms ( f, h );
                  work[i1] = z;
                  if ( z != 0.0 ) {
                     c = f / z;
                     s = h / z;
                  } else {
                     c = 1.0;
                     s = 0.0;
                  }
                  f = x * c + g * s;
                  g = - x * s + g * c;
                  h = y * s;
                  y = y * c;
                  for ( j = 0, vj = v; j < n; j++, vj += np ) {
                     x = vj[i1];
                     z = vj[i];
                     vj[i1] = x * c + z * s;
                     vj[i]  = - x * s + z * c;
                  }
                  z = rms ( f, h );
                  w[i1] = z;
                  if ( z != 0.0 ) {
                     c = f / z;
                     s = h / z;
                  }
                  f = c * g + s * y;
                  x = - s * g + c * y;
                  for ( j = 0, aj = a; j < m; j++, aj += np ) {
                     y = aj[i1];
                     z = aj[i];
                     aj[i1] = y * c + z * s;
                     aj[i] = - y * s + z * c;
                  }
               }
               work[l] = 0.0;
               work[k] = f;
               w[k] = x;
            }
         }
      }
   }
}

double rms ( double a, double b )

/* sqrt(a*a+b*b) with protection against under/overflow */

{
   double wa, wb, w;

   wa = fabs ( a );
   wb = fabs ( b );

   if ( wa > wb ) {
      w = wa;
      wa = wb;
      wb = w;
   }

   if ( wb == 0.0 ) {
      return 0.0;
   } else {
      w = wa / wb;
      return wb * sqrt ( 1.0 + w * w );
   }
}
