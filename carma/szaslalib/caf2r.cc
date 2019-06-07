#include "slalib.h"
#include "slamac.h"
void slaCaf2r ( int ideg, int iamin, float asec, float *rad, int *j )
/*
**  - - - - - - - - -
**   s l a C a f 2 r
**  - - - - - - - - -
**
**  Convert degrees, arcminutes, arcseconds to radians.
**
**  (single precision)
**
**  Given:
**     ideg        int       degrees
**     iamin       int       arcminutes
**     asec        float     arcseconds
**
**  Returned:
**     *rad        float     angle in radians
**     *j          int       status:  0 = ok
**                                    1 = ideg outside range 0-359
**                                    2 = iamin outside range 0-59
**                                    3 = asec outside range 0-59.999...
**
**  Notes:
**     1)  The result is computed even if any of the range checks fail.
**
**     2)  The sign must be dealt with outside this routine.
**
**  Called:  slaDaf2r
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double w;

/* Call double precision version */
   slaDaf2r ( ideg, iamin, (double) asec, &w, j );
   *rad = (float) w;
}
