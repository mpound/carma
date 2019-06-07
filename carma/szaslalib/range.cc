#include "slalib.h"
#include "slamac.h"
float slaRange ( float angle )
/*
**  - - - - - - - - -
**   s l a R a n g e
**  - - - - - - - - -
**
**  Normalize angle into range +/- pi.
**
**  (single precision)
**
**  Given:
**     angle     float      the angle in radians
**
**  The result is angle expressed in the +/- pi (single precision).
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  return (float) slaDrange ( (double) angle );
}
