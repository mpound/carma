#include "slalib.h"
#include "slamac.h"
float slaVdv ( float va[3], float vb[3] )
/*
**  - - - - - - -
**   s l a V d v
**  - - - - - - -
**
**  Scalar product of two 3-vectors.
**
**  (single precision)
**
**  Given:
**      va      float[3]     first vector
**      vb      float[3]     second vector
**
**  The result is the scalar product va.vb  (single precision).
**
**  Last revision:   15 July 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   return va[0] * vb[0] + va[1] * vb[1] + va[2] * vb[2];
}
