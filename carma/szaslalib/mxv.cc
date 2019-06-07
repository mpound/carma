#include "slalib.h"
#include "slamac.h"
void slaMxv ( float rm[3][3], float va[3], float vb[3] )
/*
**  - - - - - - -
**   s l a M x v
**  - - - - - - -
**
**  Perform the 3-d forward unitary transformation:
**
**     vector vb = matrix rm * vector va
**
**  (single precision)
**
**  Given:
**     rm       float[3][3]   matrix
**     va       float[3]      vector
**
**  Returned:
**     vb       float[3]      result vector
**
**  Last revision:   30 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i, j;
   float w, vw[3];

/* Matrix rm * vector va -> vector vw */
   for ( j = 0; j < 3; j++ ) {
      w = 0.0f;
      for ( i = 0; i < 3; i++ ) {
         w += rm[j][i] * va[i];
      }
      vw[j] = w;
   }

/* Vector vw -> vector vb */
   for ( j = 0; j < 3; j++ ) {
      vb[j] = vw[j];
   }
}
