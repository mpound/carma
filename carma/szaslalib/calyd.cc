#include "slalib.h"
#include "slamac.h"
void slaCalyd ( int iy, int im, int id, int *ny, int *nd, int *j )
/*
**  - - - - - - - - -
**   s l a C a l y d
**  - - - - - - - - -
**
**  Gregorian calendar date to year and day in year (in a Julian
**  calendar aligned to the 20th/21st century Gregorian calendar).
**
**  (Includes century default feature:  use slaClyd for years
**   before 100AD.)
**
**  Given:
**     iy,im,id   int    year, month, day in Gregorian calendar
**                       (year may optionally omit the century)
**  Returned:
**     *ny        int    year (re-aligned Julian calendar)
**     *nd        int    day in year (1 = January 1st)
**     *j         int    status:
**                         0 = OK
**                         1 = bad year (before -4711)
**                         2 = bad month
**                         3 = bad day (but conversion performed)
**
**  Notes:
**
**  1  This routine exists to support the low-precision routines
**     slaEarth, slaMoon and slaEcor.
**
**  2  Between 1900 March 1 and 2100 February 28 it returns answers
**     which are consistent with the ordinary Gregorian calendar.
**     Outside this range there will be a discrepancy which increases
**     by one day for every non-leap century year.
**
**  3  Years in the range 50-99 are interpreted as 1950-1999, and
**     years in the range 00-49 are interpreted as 2000-2049.
**
**  Called:  slaClyd
**
**  Last revision:   22 September 1995
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i;

/* Default century if appropriate */
   if ( ( iy >= 0 ) && ( iy <= 49 ) )
      i = iy + 2000;
   else if ( ( iy >= 50 ) && ( iy <= 99 ) )
      i = iy + 1900;
   else
      i = iy;

/* Perform the conversion */
   slaClyd ( i, im, id, ny, nd, j );
}
