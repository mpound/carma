#include "slalib.h"
#include "slamac.h"
void slaEcmat ( double date, double rmat[3][3] )
/*
**  - - - - - - - - -
**   s l a E c m a t
**  - - - - - - - - -
**
**  Form the equatorial to ecliptic rotation matrix (IAU 1980 theory).
**
**  (double precision)
**
**  Given:
**     date     double         TDB (loosely ET) as Modified Julian Date
**                                            (JD-2400000.5)
**  Returned:
**     rmat     double[3][3]   matrix
**
**  References:
**     Murray, C.A., Vectorial Astrometry, section 4.3.
**
**  Note:
**    The matrix is in the sense   v[ecl]  =  rmat * v[equ];  the
**    equator, equinox and ecliptic are mean of date.
**
**  Called:  slaDeuler
**
**  Defined in slamac.h:  DAS2R
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   double t, eps0;

/* Interval between basic epoch J2000.0 and current epoch (JC) */
   t = ( date - 51544.5 ) / 36525.0;

/* Mean obliquity */
   eps0 = DAS2R *
        ( 84381.448 + ( -46.8150 + ( -0.00059 + 0.001813 * t ) * t ) * t );

/* Matrix */
   slaDeuler ( "X", eps0, 0.0, 0.0, rmat );
}
