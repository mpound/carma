#include "slalib.h"
#include "slamac.h"
double slaEpb ( double date )
/*
**  - - - - - - -
**   s l a E p b
**  - - - - - - -
**
**  Conversion of Modified Julian Date to Besselian epoch.
**
**  (double precision)
**
**  Given:
**     date     double      Modified Julian Date (JD - 2400000.5)
**
**  The result is the Besselian epoch.
**
**  Reference:
**     Lieske,J.H., 1979. Astron. Astrophys.,73,282.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   return 1900.0 + ( date - 15019.81352 ) / 365.242198781;
}
