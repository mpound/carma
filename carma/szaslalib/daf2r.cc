#include "slalib.h"
#include "slamac.h"
void slaDaf2r ( int ideg, int iamin, double asec, double *rad, int *j )
/*
**  - - - - - - - - -
**   s l a D a f 2 r
**  - - - - - - - - -
**
**  Convert degrees, arcminutes, arcseconds to radians.
**
**  (double precision)
**
**  Given:
**     ideg        int       degrees
**     iamin       int       arcminutes
**     asec        double    arcseconds
**
**  Returned:
**     *rad        double    angle in radians
**     *j          int       status:  0 = OK
**                                    1 = ideg outside range 0-359
**                                    2 = iamin outside range 0-59
**                                    3 = asec outside range 0-59.999...
**
**  Notes:
**     1)  The result is computed even if any of the range checks fail.
**
**     2)  The sign must be dealt with outside this routine.
**
**  Defined in slamac.h:  DAS2R
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
/* Preset status */
   *j = 0;

/* Validate arcsec, arcmin, deg */
   if ( ( asec < 0.0 ) || ( asec >= 60.0 ) ) {
      *j = 3;
      return;
   }
   if ( ( iamin < 0 ) || ( iamin > 59 ) ) {
      *j = 2;
      return;
   }
   if ( ( ideg < 0 ) || ( ideg > 359 ) ) {
      *j = 1;
      return;
   }

/* Compute angle */
   *rad = DAS2R * ( 60.0 * ( 60.0 * (double) ideg
                                  + (double) iamin )
                                           + asec );
}
