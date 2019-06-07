#include "slalib.h"
#include "slamac.h"
float slaRanorm ( float angle )
/*
**  - - - - - - - - - -
**   s l a R a n o r m
**  - - - - - - - - - -
**
**  Normalize angle into range 0-2 pi.
**
**  (single precision)
**
**  Given:
**     angle     double      the angle in radians
**
**  The result is angle expressed in the range 0-2 pi (single
**  precision).
**
**  Last revision:   15 July 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  return (float) slaDranrm ( (double) angle );
}
