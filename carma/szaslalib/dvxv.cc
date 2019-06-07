#include "slalib.h"
#include "slamac.h"
void slaDvxv ( double va[3], double vb[3], double vc[3] )
/*
**  - - - - - - - -
**   s l a D v x v
**  - - - - - - - -
**
**  Vector product of two 3-vectors.
**
**  (double precision)
**
**  Given:
**      va      double[3]     first vector
**      vb      double[3]     second vector
**
**  Returned:
**      vc      double[3]     vector result
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double vw[3];
   int i;

/* Form the vector product va cross vb */
   vw[0] = va[1] * vb[2] - va[2] * vb[1];
   vw[1] = va[2] * vb[0] - va[0] * vb[2];
   vw[2] = va[0] * vb[1] - va[1] * vb[0];

/* Return the result */
   for ( i = 0; i < 3; i++ ) {
      vc[i] = vw[i];
   }
}
