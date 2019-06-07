#include "slalib.h"
#include "slamac.h"
double slaEpj2d ( double epj )
/*
**  - - - - - - - - -
**   s l a E p j 2 d
**  - - - - - - - - -
**
**  Conversion of Julian epoch to Modified Julian Date.
**
**  (double precision)
**
**  Given:
**     epj      double       Julian epoch
**
**  The result is the Modified Julian Date (JD - 2400000.5).
**
**  Reference:
**     Lieske,J.H., 1979. Astron. Astrophys.,73,282.
**
**  Last revision:   31 October 1993
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
  return 51544.5 + ( epj - 2000.0 ) * 365.25;
}
