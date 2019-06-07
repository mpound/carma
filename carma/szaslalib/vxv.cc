#include "slalib.h"
#include "slamac.h"
void slaVxv ( float va[3], float vb[3], float vc[3] )
/*
**  - - - - - - -
**   s l a V x v
**  - - - - - - -
**
**  Vector product of two 3-vectors.
**
**  (single precision)
**
**  Given:
**      va      float[3]     first vector
**      vb      float[3]     second vector
**
**  Returned:
**      vc      float[3]     vector result
**
**  Last revision:   22 February 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   float vw[3];
   int i;

/* Form the vector product va cross vb */
   vw[0] = va[1] * vb[2] - va[2] * vb[1];
   vw[1] = va[2] * vb[0] - va[0] * vb[2];
   vw[2] = va[0] * vb[1] - va[1] * vb[0];

/* Return the result */
   for ( i = 0; i < 3; i++ ) vc[i] = vw[i];
}
