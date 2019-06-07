#include "slalib.h"
#include "slamac.h"
void slaCaldj ( int iy, int im, int id, double *djm, int *j )
/*
**  - - - - - - - - -
**   s l a C a l d j
**  - - - - - - - - -
**
**  Gregorian calendar to Modified Julian Date.
**
**  (Includes century default feature:  use slaCldj for years
**   before 100AD.)
**
**  Given:
**     iy,im,id   int      year, month, day in Gregorian calendar
**
**  Returned:
**     *djm       double   Modified Julian Date (JD-2400000.5) for 0 hrs
**     *j         int      status:
**                           0 = ok
**                           1 = bad year   (MJD not computed)
**                           2 = bad month  (MJD not computed)
**                           3 = bad day    (MJD computed)
**
**  Acceptable years are 00-49, interpreted as 2000-2049,
**                       50-99,     "       "  1950-1999,
**                       100 upwards, interpreted literally.
**
**  Called:  slaCldj
**
**  Last revision:   21 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int ny;

/* Default century if appropriate */
   if ( ( iy >= 0 ) && ( iy <= 49 ) )
      ny = iy + 2000;
   else if ( ( iy >= 50 ) && ( iy <= 99 ) )
      ny = iy + 1900;
   else
      ny = iy;

/* Modified Julian Date */
   slaCldj ( ny, im, id, djm, j );
}
