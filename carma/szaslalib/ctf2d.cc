#include "slalib.h"
#include "slamac.h"
void slaCtf2d ( int ihour, int imin, float sec, float *days, int *j )
/*
**  - - - - - - - - -
**   s l a C t f 2 d
**  - - - - - - - - -
**
**  Convert hours, minutes, seconds to days.
**
**  (single precision)
**
**  Given:
**     ihour       int       hours
**     imin        int       minutes
**     sec         float     seconds
**
**  Returned:
**     *days       float     interval in days
**     *j          int       status:  0 = OK
**                                    1 = ihour outside range 0-23
**                                    2 = imin outside range 0-59
**                                    3 = sec outside range 0-59.999...
**
**  Notes:
**
**     1)  The result is computed even if any of the range checks fail.
**
**     2)  The sign must be dealt with outside this routine.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

#define D2S 86400.0    /* Seconds per day */

{
/* Preset status */
   *j = 0;

/* Validate sec, min, hour */
   if ( ( sec < 0.0f ) || ( sec >= 60.0f ) ) {
      *j = 3;
      return;
   }
   if ( ( imin < 0 ) || ( imin > 59 ) ) {
      *j = 2;
      return;
   }
   if ( ( ihour < 0 ) || ( ihour > 23 ) ) {
      *j = 1;
      return;
   }

/* Compute interval */
   *days = ( 60.0f * ( 60.0f * (float) ihour + (float) imin ) + sec) / D2S;
}
