#include "slalib.h"
#include "slamac.h"
void slaVn ( float v[3], float uv[3], float *vm )
/*
**  - - - - - -
**   s l a V n
**  - - - - - -
**
**  Normalizes a 3-vector also giving the modulus.
**
**  (single precision)
**
**  Given:
**     v       float[3]      vector
**
**  Returned:
**     uv      float[3]      unit vector in direction of v
**     *vm     float         modulus of v
**
**  If the modulus of v is zero, uv is set to zero as well.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i;
   float w1, w2;

/* Modulus */
   w1 = 0.0f;
   for ( i = 0; i < 3; i++ ) {
      w2 = v[i];
      w1 = w1 + w2 * w2;
   }
   w1 = (float) sqrt ( w1 );
   *vm = w1;

/* Normalize the vector */
   if ( w1 <= 0.0f ) {
      w1 = 1.0f;
   }
   for ( i = 0; i < 3; i++ ) {
      uv[i] = v[i] / w1;
   }
}
