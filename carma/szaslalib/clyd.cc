#include "slalib.h"
#include "slamac.h"
void slaClyd ( int iy, int im, int id, int *ny, int *nd, int *jstat )
/*
**  - - - - - - - -
**   s l a C l y d
**  - - - - - - - -
**
**  Gregorian calendar to year and day in year (in a Julian calendar
**  aligned to the 20th/21st century Gregorian calendar).
**
**  Given:
**     iy,im,id     int    year, month, day in Gregorian calendar
**
**  Returned:
**     ny          int    year (re-aligned Julian calendar)
**     nd          int    day in year (1 = January 1st)
**     jstat       int    status:
**                          0 = OK
**                          1 = bad year (before -4711)
**                          2 = bad month
**                          3 = bad day (but conversion performed)
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
**  3  The essence of the algorithm is first to express the Gregorian
**     date as a Julian Day Number and then to convert this back to
**     a Julian calendar date, with day-in-year instead of month and
**     day.  See 12.92-1 and 12.95-1 in the reference.
**
**  Reference:  Explanatory Supplement to the Astronomical Almanac,
**              ed P.K.Seidelmann, University Science Books (1992),
**              p604-606.
**
**  Last revision:   26 November 1994
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   long i, j, k, l, n, iyL, imL;

/* Month lengths in days */
   static int mtab[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };



/* Validate year */
   if ( iy < -4711 ) { *jstat = 1; return; }

/* Validate month */
   if ( ( im < 1 ) || ( im > 12 ) ) { *jstat = 2; return; }

/* Allow for (Gregorian) leap year */
   mtab[1] = ( ( ( iy % 4 ) == 0 ) &&
             ( ( ( iy % 100 ) != 0 ) || ( ( iy % 400 ) == 0 ) ) ) ?
             29 : 28;

/* Validate day */
   *jstat = ( id < 1 || id > mtab[im-1] ) ? 3 : 0;

/* Perform the conversion */
   iyL = (long) iy;
   imL = (long) im;
   i = ( 14 - imL ) /12L;
   k = iyL - i;
   j = ( 1461L * ( k + 4800L ) ) / 4L
     + ( 367L * ( imL - 2L + 12L * i ) ) / 12L
     - ( 3L * ( ( k + 4900L ) / 100L ) ) / 4L + (long) id - 30660L;
   k = ( j - 1L ) / 1461L;
   l = j - 1461L * k;
   n = ( l - 1L ) / 365L - l / 1461L;
   j = ( ( 80L * ( l - 365L * n + 30L ) ) / 2447L ) / 11L;
   i = n + j;
   *nd = 59 + (int) ( l -365L * i + ( ( 4L - n ) / 4L ) * ( 1L - j ) );
   *ny = (int) ( 4L * k + i ) - 4716;
}
