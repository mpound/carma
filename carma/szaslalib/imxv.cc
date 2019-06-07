#include "slalib.h"
#include "slamac.h"
void slaImxv ( float rm[3][3], float va[3], float vb[3] )
/*
**  - - - - - - - -
**   s l a I m x v
**  - - - - - - - -
**
**  Performs the 3-d backward unitary transformation:
**
**     vector vb = (inverse of matrix rm) * vector va
**
**  (single precision)
**
**  n.b.  The matrix must be unitary, as this routine assumes that
**        the inverse and transpose are identical.
**
**  Given:
**     rm       float[3][3]    matrix
**     va       float[3]       vector
**
**  Returned:
**     vb       float[3]       result vector
**
**  The same vector can be specified for both va and vb.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i, j;
   float w, vw[3];

/* Inverse of matrix rm * vector va -> vector vw */
   for ( j = 0; j < 3; j++ ) {
      w = 0.0f;
      for ( i = 0; i < 3; i++ ) {
         w += rm[i][j] * va[i];
      }
      vw[j] = w;
   }

/* Vector vw -> vector vb */
   for ( j = 0; j < 3; j++ ) {
      vb[j] = vw[j];
   }
}
