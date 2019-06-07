#include "slalib.h"
#include "slamac.h"
void slaDtf2r ( int ihour, int imin, double sec, double *rad, int *j )
/*
**  - - - - - - - - -
**   s l a D t f 2 r
**  - - - - - - - - -
**
**  Convert hours, minutes, seconds to radians.
**
**  (double precision)
**
**  Given:
**     ihour       int           hours
**     imin        int           minutes
**     sec         double        seconds
**
**  Returned:
**     *rad        double        angle in radians
**     *j          int           status:  0 = OK
**                                        1 = ihour outside range 0-23
**                                        2 = imin outside range 0-59
**                                        3 = sec outside range 0-59.999...
**
**  Called:
**     slaDtf2d
**
**  Notes:
**
**     1)  The result is computed even if any of the range checks fail.
**
**     2)  The sign must be dealt with outside this routine.
**
**  Defined in slamac.h:  D2PI
**
**  Last revision:   30 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double turns;

/* Convert to turns */
   slaDtf2d ( ihour, imin, sec, &turns, j );

/* To radians */
   *rad = D2PI * turns;
}
